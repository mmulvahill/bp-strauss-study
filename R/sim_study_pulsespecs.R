#-------------------------------------------------------------------------------
# Argument sets 
#   Order statistic, Hardcore (R=40, gamma = 0.0), Strauss (R=40, gamma=0.1)
#
# Date: 2017-07-13
# Notes:
#   This file replaces: - R/args-reference.R
#                       - R/args-lowsnr.R
#                       - R/args-hcsensitivity.R
# 
# List of TODO: 
#   - Try doing this in a tidy way...
#   - Double check defaults with list at bottom of script)
#-------------------------------------------------------------------------------


#---------------------------------------
# Set up the 3 primary prior scenarios 
#---------------------------------------
priors <- data_frame(case                 = "reference",
                     prior_scenario       = "orderstat",
                     location_prior_type  = "order-statistic",
                     prior_location_gamma = NA,
                     prior_location_range = NA)
priors <- bind_rows(priors,
                    priors %>% mutate(prior_scenario       = "hardcore40",
                                      location_prior_type  = "strauss",
                                      prior_location_gamma = 0,
                                      prior_location_range = 40),
                    priors %>% mutate(prior_scenario       = "strauss40_010",
                                      location_prior_type  = "strauss",
                                      prior_location_gamma = 0.1,
                                      prior_location_range = 40))

#---------------------------------------
# Customize for each simulation case
#---------------------------------------
priors <- 
  bind_rows(priors %>% mutate(case = "reference"),
            ## Low mass - same as above, but prior.mass.mean ~= exp(0.30)
            priors %>% mutate(case = "low-mass", prior_mass_mean = 1.4),
            ## High error
            priors %>% mutate(case = "high-error"))

#---------------------------------------
# Add in sensitivity scenarios
#   (Strauss and Hardcore w/ range of 20 and 60)
#---------------------------------------
priors <- 
  bind_rows(priors,
            priors %>%
              filter(location_prior_type == "strauss") %>%
              mutate(prior_location_range = 20,
                     prior_scenario = str_replace_all(prior_scenario, "40", "20")),
            priors %>% 
              filter(location_prior_type == "strauss") %>%
              mutate(prior_location_range = 60,
                     prior_scenario = str_replace_all(prior_scenario, "40", "60")))

priors <- priors %>% nest(-case, -prior_scenario, .key = "pulse_spec_args")

sim_study <- full_join(sim_study, priors)

rm(priors)


#-----------------------------------------------------------
# Old priors -- w/ lognormal prior on mean mass/width
# Using default values for these args, which currently are:
#-----------------------------------------------------------
#  prior.mass.mean       = 1.50,    
#  prior.mass.var        = 10,
#  prior.width.mean      = 3.5,
#  prior.width.var       = 10,
#  prior.baseline.mean   = 2.6,
#  prior.baseline.var    = 100,
#  prior.halflife.mean   = 45,
#  prior.halflife.var    = 100,
#  prior.error.alpha     = 1e-04,
#  prior.error.beta      = 1e-04,
#  max.sd.mass           = 10,
#  max.sd.width          = 10,
#  prior.mean.num.pulses = 12,
#  sv.mass.mean          = 1.5,
#  sv.width.mean         = 3.5,
#  sv.baseline.mean      = 2.6,
#  sv.halflife.mean      = 45,
#  sv.error.var          = 0.25,
#  sv.mass.sd            = 1,
#  sv.width.sd           = 1,
#  pv.baseline           = 0.5,
#  pv.halflife           = 45,
#  pv.mass               = 2,
#  pv.width              = 2,
#  pv.re.mass            = 2,
#  pv.re.width           = 50,
#  pv.pulselocations     = 10)


#-------------------------------------------------------------------------------
# End of file
#-------------------------------------------------------------------------------
