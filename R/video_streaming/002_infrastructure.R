library(simmer)
library(tidyverse)
library(futile.logger)

client_arrival_rate <- 1 / 1000
sim_duration <- 20 * 60 * 1000

get_client_interarrival_time <- \() rgeom(1, prob = client_arrival_rate) + 1
get_video_bitrate <- \() round(runif(1, 500, 3000))
get_video_duration <- \() round(runif(1, 2, 4) * 60 * 1000)

get_frontend_service_time <- \() rgeom(1, prob = 1 / 1000) + 1
get_backend_service_time <- \() rgeom(1, prob = 1 / 1000) + 1

trj_client <- trajectory() %>%
  set_attribute("video_bitrate", get_video_bitrate) %>% 
  set_attribute("video_duration", get_video_duration) %>% 
  
  seize("frontend") %>% 
  timeout(get_frontend_service_time) %>%
  
  seize("backend") %>% 
  timeout(get_backend_service_time) %>% 
  release("backend") %>%
  
  timeout(get_frontend_service_time) %>% 
  release("frontend") %>% 
  
  seize("server", amount = \() get_attribute(env, "video_bitrate")) %>% 
  timeout(\() get_attribute(env, "video_duration")) %>%
  release("server", amount = \() get_attribute(env, "video_bitrate"))

env <- simmer() %>% 
  add_generator(name_prefix = "client", trajectory = trj_client, distribution = get_client_interarrival_time, mon = 2) %>% 
  add_resource(name = "frontend", capacity = 200, queue_size = 1000) %>% 
  add_resource(name = "backend", capacity = 100, queue_size = 1000) %>% 
  add_resource(name = "server", capacity = 10 * 1000 * 1000, queue_size = 0)

env %>% 
  run(until = sim_duration, progress = flog.info)

arrivals <- get_mon_arrivals(env, per_resource = T, ongoing = T)
resources <- get_mon_resources(env)
attributes <- get_mon_attributes(env)

p_occupied_resources <- ggplot(data = resources, aes(x = time, y = system, color = resource)) +
  geom_line() + 
  facet_grid(resource ~ ., scales = "free") + 
  labs(x = "Time [ms]", y = "Occupied resources")
p_occupied_resources
