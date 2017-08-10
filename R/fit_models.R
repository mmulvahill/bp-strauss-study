



if (TEST & RUN) {
  cluster <- create_cluster(11)
  set_default_cluster(cluster)
  cluster_library(cluster, c("pulsatile", "purrr"))
  get_default_cluster()

  # THIS WORKS
  test_fits <- 
    sim_study %>% 
    filter(sim_num %in% 1:2) %>%
    partition %>%
    #   rowwise %>%
    mutate(fits = map2(simulation, this_spec,
                       ~ fit_pulse(.data = .x$data, 
                                   spec = .y, 
                                   thin = 50, burnin = 50000, iters = 250000, 
                                   use_tibble = TRUE)))

    collected_fits <- collect(test_fits)
    saveRDS(collected_fits, 
            file = "output_xl/fifth_run_mdplyr_42fits_larger_small_mass_meansd_wburnin.Rds")
    print_diag_PDF(collected_fits)

} else if (RUN & REFERENCE) {

  library(parallel)

  # TODO: Output too large for keeping in RAM
  sim_study_test <- 
    sim_study %>% 
    mutate(seed = 1:nrow(.)) %>%
    filter(case == "reference" & 
           prior_scenario %in% c("orderstat", "hardcore40", "strauss40_010")) %>%
    filter(prior_scenario == "orderstat") %>%
    filter(sim_num %in% 1:50) 

  test_fits <- 
    pblapply(1:nrow(sim_study_test), cl = 11, 
             function(x) {
               fit_pulse(.data = sim_study_test$simulation[[x]]$data, 
                         spec  = sim_study_test$this_spec[[x]], 
                         thin  = 50, burnin = 50000, iters = 250000, 
                         use_tibble = TRUE)
             })

  sim_study_test %>% mutate(fits =  test_fits) %>% 
    print_diag_PDF(out_file = "output/diagnostics/diag_test_ref_os_1-50_min0.1_lrgwidth.pdf")
  sim_study_test %>% mutate(fits =  test_fits) %>% 
    saveRDS(file = "output_xl/test_ref_os_1-50_min0.1_lrgwidth.Rds")

}

