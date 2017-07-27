



if (TEST & RUN) {
  cluster <- create_cluster(11)
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
                                   thin = 50, burnin = 50000, iters = 250000, 
                                   use_tibble = TRUE)))

    collected_fits <- collect(test_fits)
#     collected_fits_dedup <- collected_fits %>% select(-pulse_spec_args, -this_spec)
#     object_size(collected_fits)
#     object_size(collected_fits_dedup)
    saveRDS(collected_fits, 
            file = "remote-storage/fifth_run_mdplyr_42fits_larger_small_mass_meansd_wburnin.Rds")
    print_diag_PDF(collected_fits)

} else if (RUN) {

  cluster <- create_cluster(32)
  set_default_cluster(cluster)
  cluster_library(cluster, c("pulsatile", "purrr"))
  get_default_cluster()

  # THIS WORKS
  test_fits <- 
    sim_study %>% 
    rownames_to_column(var = "id") %>% 
    filter(case == "reference" & 
           prior_scenario %in% c("orderstat", "hardcore40", "strauss40_010")) %>%
    mutate(this_spec = map(pulse_spec_args, ~ do.call(pulse_spec, .x))) %>%
    partition %>%
    mutate(fits = map2(simulation, this_spec,
                       ~ fit_pulse(.data = .x$data, 
                                   spec = .y, 
                                   thin = 50, burnin = 50000, iters = 250000, 
                                   use_tibble = TRUE)))

    collected_fits <- collect(test_fits)
    saveRDS(collected_fits, file = "output_xl/primary_analysis_reference_fits.Rds")
    print_diag_PDF(collected_fits)

} else {

  sim_study <-
    readRDS("../remote-storage/fifth_run_mdplyr_42fits_larger_small_mass_meansd_wburnin.Rds")


}

