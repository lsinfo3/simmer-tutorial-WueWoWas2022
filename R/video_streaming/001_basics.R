library(simmer)
library(tidyverse)
library(futile.logger)
# library(zoo)

client_arrival_rate <- 1 / 1000
sim_duration <- 20 * 60 * 1000

get_client_interarrival_time <- \() rgeom(1, prob = client_arrival_rate) + 1 
get_video_bitrate <- \() round(runif(1, 500, 3000))
get_video_duration <- \() round(runif(1, 2, 4) * 60 * 1000)


trj_client <- trajectory() %>% 
  set_attribute("video_bitrate", get_video_bitrate) %>% 
  seize("server", amount = \() get_attribute(env, "video_bitrate")) %>% 
  timeout(get_video_duration) %>% 
  release("server", amount = \() get_attribute(env, "video_bitrate"))

env <- simmer() %>% 
  add_generator(name_prefix = "client", trajectory = trj_client, distribution = get_client_interarrival_time, mon = 2) %>% 
  add_resource(name = "server", capacity = 10 * 1000 * 1000, queue_size = 0)

env %>% 
  run(until = sim_duration, progress = flog.info)

arrivals <- get_mon_arrivals(env)
resources <- get_mon_resources(env)
attributes <- get_mon_attributes(env)

p_used_bandwidth <- ggplot(data = resources, aes(x = time, y = system/1000)) +
  geom_line() + 
  # geom_line(aes(y=rollmean(system/1000, 1000, na.pad=TRUE)), color = "red") + 
  labs(x = "Time [ms]", y = "Used bandwidth [Mbps]")

p_used_bandwidth

p_video_bitrate_cdf <- ggplot(data = attributes %>% filter(key == "video_bitrate"), aes(x = value)) +
  stat_ecdf()

p_video_bitrate_cdf


df_reject_rate <- arrivals %>%
  group_by(finished) %>%
  summarize(n = n()) %>%
  mutate(prob = n/sum(n))
