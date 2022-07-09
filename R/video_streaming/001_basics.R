# Non-optional libraries
library(simmer)
library(tidyverse)

# Required for progress logging
library(futile.logger)


# Prepare functions and config variables ----------------------------------

client_arrival_rate <- 1 / 1000
sim_duration <- 20 * 60 * 1000

get_client_interarrival_time <- \() rgeom(1, prob = client_arrival_rate) + 1 
get_video_bitrate <- \() round(runif(1, 500, 3000))
get_video_duration <- \() round(runif(1, 2, 4) * 60 * 1000)


# Refine client trajectory ------------------------------------------------

trj_client <- trajectory() %>% 
  set_attribute("video_bitrate", get_video_bitrate) %>% 
  seize("server", amount = \() get_attribute(env, "video_bitrate")) %>% 
  timeout(get_video_duration) %>% 
  release("server", amount = \() get_attribute(env, "video_bitrate"))


# Define simmer environment, generators and resources ---------------------

env <- simmer() %>% 
  add_generator(name_prefix = "client", trajectory = trj_client, distribution = get_client_interarrival_time, mon = 2) %>% 
  add_resource(name = "server", capacity = 10 * 1000 * 1000, queue_size = 0)

# Execute simulation ------------------------------------------------------

# Use this if you have the futile.logger library
# env %>% run(until = sim_duration, progress = flog.info)

# or this if you don't
env %>% run(until = sim_duration)


# Get results -------------------------------------------------------------

arrivals <- get_mon_arrivals(env)
resources <- get_mon_resources(env)
attributes <- get_mon_attributes(env)

# Evaluate results --------------------------------------------------------

p_used_bandwidth <- ggplot(data = resources, aes(x = time, y = system/1000)) +
  geom_line() + 
  labs(x = "Time [ms]", y = "Used bandwidth [Mbps]")

p_used_bandwidth

p_video_bitrate_cdf <- ggplot(data = attributes %>% filter(key == "video_bitrate"), aes(x = value)) +
  stat_ecdf()

p_video_bitrate_cdf

df_reject_rate <- arrivals %>%
  group_by(finished) %>%
  summarize(n = n()) %>%
  mutate(prob = n/sum(n))
