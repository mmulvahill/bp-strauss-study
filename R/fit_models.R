

print(sim_study)

devtools::install_github("hadley/multidplyr", ref = "0085ded")
library(multidplyr)

cluster <- create_cluster(5)
set_default_cluster(cluster)
cluster_library(cluster, c("pulsatile", "purrr"))

test_sim_study <- sim_study %>% 
  filter(case == "reference" & sim_num %in% 1:5 & prior_scenario %in% c("orderstat", "hardcore40")) %>%
  partition(sim_num)

test_sim_study %>% 
  mutate(this_spec = map(pulse_spec_args, ~ do.call(pulse_spec, .x))) %>%
  mutate(fits = map2(simulation, this_spec, 
                     ~ fit_pulse(.data = .x$simulation,
                                 spec = .y,
                                 iters = 250000,
                                 thin = 50,
                                 use_tibble = TRUE)))

collect(cluster)

test_sim_study %>%
  map(~ fit_pulse(.data = .x$simulation, 
                  spec = do.call(pulse_spec, .x$pulse_spec_args), 
                  thin = 50, iters = 250000, use_tibble = TRUE))
fit_pulse()



sim_study %>% .[1:3, ]
