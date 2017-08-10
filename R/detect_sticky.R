
sticky_loop <- function(.data, .spec, run_length = 1000, n_rollavg = 50) {
  has_sticky <- FALSE
  seed <- 0

  while (!has_sticky) {

    seed <- seed + 1
    set.seed(seed)
    print(seed)
    fit <- fit_pulse(.data = .data, spec  = .spec, thin  = 1, burnin = 0,
                         iters = 250000, use_tibble = TRUE)
    print(seed)

    has_sticky <- common_chain(fit) %>% detect_sticky(run_length = run_length,
                                                      n_rollavg  = n_rollavg)

  }

  return(list(fit = fit, seed = seed))

}


# Function for identfying fits that have serious sticky point problems in the
# sd_width posteriors 
detect_sticky <- function(commonchain, run_length = 1000, n_rollavg = 50) {

  runlen <- commonchain$sd_widths %>% zoo::rollmean(k = n_rollavg) %>% round %>% rle 
  maxrun <- runlen %>% .$lengths %>% max
  maxrunval <- which(runlen$lengths == maxrun) %>% runlen$values[.] %>% min

  ifelse(maxrun > (run_length/n_rollavg) & maxrunval < 1, TRUE, FALSE)

}


