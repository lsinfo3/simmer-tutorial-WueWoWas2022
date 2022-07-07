library(simmer)
library(tidyverse)

# Prepare distribution and functions for arrival process A ------------------------------------------------------------------------------------------------

# Define a base vector for the arrival process A
a.base_vector <- c(20, rep(4, 4), rep(8, 5))

# Sample from the base vector
a.sample <- sample(a.base_vector, 1e6, replace = T)

# Validate target distribution
a.dist <- data.frame(sample = a.sample) %>% 
  group_by(sample) %>% 
  summarize(n = n()) %>% 
  mutate(prob = n/sum(n))

# Define sampling function to use during the simulation
a.func <- function() {
  return(sample(a.base_vector, 1))
}

# Prepare distribution and functions for service process B ------------------------------------------------------------------------------------------------

b.base_vector <- c(rep(4, 2), 5, rep(6, 7))
b.sample <- sample(b.base_vector, 1e6, replace = T)
b.dist <- data.frame(sample = b.sample) %>% 
  group_by(sample) %>% 
  summarize(n = n()) %>% 
  mutate(prob = n/sum(n))

b.func <- function() {
  return(sample(b.base_vector, 1))
}

# Define trajectories for arrivals ------------------------------------------------------------------------------------------------------------------------

### TODO
# Define a trajectory that claims one unit of an available resource and takes time B to process before releasing the resource

# Define resources and generators -------------------------------------------------------------------------------------------------------------------------

### TODO
# Define simulation environment
# Define a resource with a single processing unit and an infinitely large queue
# Define a generator that produces events with interarrival times A

# Execute simulation --------------------------------------------------------------------------------------------------------------------------------------

### TODO
# Execute the simulation for different periods of time
# What can you see regarding the final results?
# What other way is there to determine when to end the simulation?

# Pull and evaluate results  ------------------------------------------------------------------------------------------------------------------------------

# Obtain the simulation data
df_arrivals <- get_mon_arrivals(env)
df_resources <- get_mon_resources(env)

df_arrivals <- df_arrivals %>%
  mutate(waiting_time = end_time - start_time - activity_time)

# dta_result <- \() {
#   reticulate::source_python(file = "./ex_1_dta.py", envir =  parent.frame())
#   return(data.frame(x = Wn1$xk, y = Wn1$pk))
# }

# Plot the waiting time distribution (blue: model, red: simulation)
p_wt <- ggplot() + 
  stat_ecdf(data = df_arrivals, mapping = aes(x = waiting_time, color = "Simulation")) +
  # geom_step(data = dta_result(), mapping = aes(x = x, y = cumsum(y), color = "Model")) + 
  coord_cartesian(xlim = c(0, 20)) + 
  labs(x = "Waiting Time", y = "ECDF", color = "Datasource")
print(p_wt)

p_qs <- ggplot(df_resources %>% filter(time <= 1000), aes(x = time, y = system)) +
  geom_step()
print(p_qs)
