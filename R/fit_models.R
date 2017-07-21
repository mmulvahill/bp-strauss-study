

print(sim_study)

devtools::install_github("hadley/multidplyr", ref = "0085ded")
library(multidplyr)

cluster <- create_cluster(7)
set_default_cluster(cluster)
cluster_library(cluster, c("pulsatile", "purrr"))
get_default_cluster()

# THIS WORKS
test_fits <- 
  sim_study %>% 
  rownames_to_column(var = "id") %>% 
  filter(sim_num %in% 1:2) %>%
  #filter(case == "reference" & sim_num %in% 1:5 & prior_scenario %in% c("orderstat", "hardcore40")) %>%
  mutate(this_spec = map(pulse_spec_args, ~ do.call(pulse_spec, .x))) %>%
  partition %>%
#   rowwise %>%
  mutate(fits = map2(simulation, this_spec,
                     ~ fit_pulse(.data = .x$data, 
                                 spec = .y, 
                                 thin = 50, iters = 250000, use_tibble = TRUE)))

collected_fits <- collect(test_fits)
object_size(collected_fits)


