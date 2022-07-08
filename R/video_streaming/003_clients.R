library(simmer)
library(tidyverse)
library(futile.logger)

# Client States
# 1 - browsing
# 2 - streaming_start
# 3 - playout_start
# 4 - finished

# 6 - rejected during browsing
# 7 - left due to excessive waiting time

client_arrival_rate <- 20 / 1000
sim_duration <- 60 * 60 * 1000

get_client_interarrival_time <- \() rgeom(1, prob = client_arrival_rate) + 1
get_client_bandwidth <- \() sample(c(16000, 30000, 50000, 500000), 1)

get_video_bitrate <- \() round(runif(1, 500, 3000))
get_video_duration <- \() round(runif(1, 2, 4) * 60 * 1000)
get_chunk_size <- \() round(runif(1, 1, 3)*1000)

get_download_time <- \() round(((get_attribute(env, "chunk_size")/1000) * get_attribute(env, "video_bitrate") / get_attribute(env, "client_bandwidth"))*1000)

get_frontend_service_time <- \() round(runif(1, 0, 3) * 60 * 1000)
get_backend_service_time <- \() rgeom(1, prob = 1 / 1000) + 1 
get_server_preparation_time <- \() rgeom(1, prob = 20 / 1000) + 1
get_thinking_time <- \() round(runif(1, 1, 10) * 1000)

trj_browsing <- trajectory() %>% 
  seize("frontend", reject = trajectory() %>% set_attribute("state", 6) %>% set_global("active_clients", -1, "+"), continue = F) %>% 
  timeout(get_frontend_service_time) %>% 
  
  seize("backend", reject = trajectory() %>% set_attribute("state", 6) %>% set_global("active_clients", -1, "+") %>% release("frontend"), continue = F) %>% 
  timeout(get_backend_service_time) %>% 
  release("backend") %>% 
  
  timeout(get_frontend_service_time) %>% 
  release("frontend") %>%
  
  timeout(get_thinking_time) %>% 
  
  branch(option = \() ifelse(runif(1, 0, 1) >= 0.8, 0, 1), continue = T,
         trajectory() %>% 
           set_attribute("state", 2))

trj_waiting <- trajectory() %>%
  renege_in(t = 10 * 1000, out = trajectory() %>% set_attribute("state", 7) %>% set_global("active_clients", -1, "+")) %>%
  seize("server", amount = \() get_attribute(env, "video_bitrate")) %>%
  renege_abort() %>%
  timeout(get_server_preparation_time) %>% 
  release("server", amount = \() get_attribute(env, "video_bitrate")) %>%
  set_attribute("state", 3) %>%
  set_attribute("playout_start", \() now(env))

trj_streaming <- trajectory() %>% 
  set_attribute("chunk_size", get_chunk_size) %>%
  renege_in(t = 10 * 1000, out = trajectory() %>% set_attribute("state", 7) %>% set_global("active_clients", -1, "+")) %>%
  seize("server", amount = \() get_attribute(env, "video_bitrate")) %>% 
  renege_abort() %>% 
  set_attribute("download_time", get_download_time) %>% 
  timeout(\() get_attribute(env, "download_time")) %>% 
  release("server", amount = \() get_attribute(env, "video_bitrate")) %>%
  set_attribute("buffer_level", \() get_attribute(env, "chunk_size"), "+") %>% 
  timeout(\() get_attribute(env, "chunk_size")-500) %>% 
  set_attribute("buffer_level", 500) %>% 
  leave(prob = \() ifelse(now(env) - get_attribute(env, "playout_start") >= get_attribute(env, "video_duration"), 1, 0), out = trajectory() %>% set_attribute("buffer_level", 0) %>% set_attribute("state", 4) %>% set_global("active_clients", -1, "+"))
  
trj_client <- trajectory() %>% 
  set_attribute("state", 1) %>% 
  set_attribute("buffer_level", 0) %>% 
  set_attribute("video_bitrate", get_video_bitrate) %>% 
  set_attribute("client_bandwidth", get_client_bandwidth) %>% 
  set_attribute("video_duration", get_video_duration) %>% 
  set_global("active_clients", 1, "+", init = 0) %>% 
  
  branch(option = \() get_attribute(env, "state"), continue = T,
         trj_browsing,
         trj_waiting,
         trj_streaming) %>% 
  rollback(amount = 1)

env <- simmer() %>% 
  add_generator(name_prefix = "client", trajectory = trj_client, distribution = get_client_interarrival_time, mon = 2) %>% 
  #add_generator(name_prefix = "client", trajectory = trj_client, distribution = at(1), mon = 2) %>% 
  add_resource(name = "frontend", capacity = 200, queue_size = 200) %>% 
  add_resource(name = "backend", capacity = 10, queue_size = 10) %>% 
  add_resource(name = "server", capacity = 10 * 1000 * 1000, queue_size = Inf)

env %>% 
  run(until = sim_duration, progress = flog.info)

arrivals <- get_mon_arrivals(env, per_resource = T, ongoing = T)
resources <- get_mon_resources(env) 
attributes <- get_mon_attributes(env)

states <- attributes %>%
  filter(key == "state") %>% 
  mutate(state_name = case_when(
    value == 1 ~ "browsing",
    value == 2 ~ "waiting",
    value == 3 ~ "streaming",
    value == 4 ~ "finished",
    value == 5 ~ "left",
    value == 6 ~ "rejected_browsing",
    value == 7 ~ "left_waiting",
  )) %>% 
  group_by(name) %>% 
  arrange(time) %>% 
  mutate(duration = lead(time) - time)

example_client <- "client10"

df_example_client <- states %>% 
  filter(name == example_client & key == "state")

df_example_client_details <- attributes %>% 
  filter(name == example_client)

p_example_state_diagram <- ggplot() +
  geom_crossbar(data = df_example_client, aes(x = value, ymin = time, y = time, ymax = time + duration, fill = state_name), size = 0) +
  coord_flip() + 
  labs(x = "Time [ms]", y = "State", color = "State")

p_example_state_diagram

# ggplotly(p_example_state_diagram)

clients <- attributes %>% filter(key == "active_clients") %>%
  mutate(bin = floor(time / 1000)) %>%
  group_by(bin) %>%
  summarize(mn = mean(value))

p_active_clients <- ggplot(data = clients, aes(x = bin, y = mn)) +
  geom_line() + 
  labs(x = "Time [ms]", y = "No. of active clients")
p_active_clients

df_rejects <- attributes %>% filter(key == "state" & value %in% c(6, 7))

