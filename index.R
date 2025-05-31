# Import simmer package
library(simmer)
library(simmer.bricks)
library(simmer.plot)
library(parallel)

# Triage trajectory for patients that come into a Role 1
casualty <- trajectory("casualty handling") %>%
  ########
  # TRIAGE
  ########
  # triage the patient
  select(c("medic_1", "medic_2", "nurse", "doctor"), policy = "shortest-queue") %>%
  seize_selected() %>%
  # Casualty triage takes 15 min, normally distributed with std dev of 1 min
  timeout(function() rnorm(1, 15)) %>%
  release_selected() # %>%
  # Branch for KIA
  #branch(casualty, function())


# Run 100 parallel simulations
# envs <- lapply(1:100, function(i) {
#   simmer("bch") %>%
#     add_resource("medic_1", 1) %>%
#     add_resource("medic_2", 1) %>%
#     add_resource("nurse", 1) %>%
#     add_resource("doctor",1) %>%
#     add_generator("wia", casualty, function() exp(6.86)/(24*60)) %>%
#     add_generator("kia", casualty, function() exp(1.63)/(24*60)) %>%
#     # Run simulation for 30 days with one minute intervals
#     run(30*24*60) %>%
#     wrap()
# })

  env <- simmer("bch") %>%
    add_resource("medic_1", 1) %>%
    add_resource("medic_2", 1) %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor",1) %>%
    add_generator("wia", casualty, function() exp(6.86)) %>%
    add_generator("kia", casualty, function() exp(1.63)) %>%
    # Run simulation for 30 days with one minute intervals
    run(30) %>%
    wrap()

# envs[[1]] %>% get_n_generated("wia")
# envs[[1]] %>% get_n_generated("kia")
# envs[[1]] %>% get_queue_count("medic_1")
# envs[[1]] %>% get_queue_size("medic_1")

env %>% 
  get_mon_resources() %>%
  head()

env %>%
  get_mon_arrivals() %>%
  head()

# Plot the results
resources <- get_mon_resources(env)
plot(resources, metric = "utilization")
plot(resources, metric = "usage", c("medic_2", "medic_1", "nurse", "doctor"), items = "server")
# plot(get_mon_resources(envs[[1]]), metric = "usage", "medic_1", items = "server", steps = TRUE)
# arrivals <- get_mon_arrivals(envs)
# plot(arrivals, metric = "flow_time")
# plot(arrivals, metric = "activity_time")
# plot(arrivals, metric = "waiting_time")

# Plot the trajectory
get_palette <- scales::brewer_pal(type = "qual", palette = 1)
plot(casualty, fill = get_palette)