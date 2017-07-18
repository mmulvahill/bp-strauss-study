#-------------------------------------------------------------------------------
# deprlp-createdata.R
#
# Description: 
#   For luteinizing phase, control patients from the depression study, create
#   datasets from CSV, generate argument files, and create a PDF of
#   concentration time series.
# 
# Date: 12/19/2015
# Author: MJM
# Add'l Notes:
#
#
#-------------------------------------------------------------------------------


#-----------------------------------------------------------
# File paths
#-----------------------------------------------------------
# Deidentified dataset, containing only luteal phase, healthy controls
deident_data_file <- file.path("./data/deprstudy-lhcontrols-deident.Rds")
deident_arg_file  <- file.path("./data/deprstudy-lhcontrols-argtemplate.Rds")


#-----------------------------------------------------------
# Load data
#-----------------------------------------------------------
# Load datasets and template arg file
lh_data <- readRDS(deident_data_file)
lh_arg  <- readRDS(deident_arg_file)


#-----------------------------------------------------------
# Prepare dataset
#-----------------------------------------------------------
# Minor cleanup, variable formatting, and nest concentration data
lh_data <- 
  lh_data %>% 
  mutate(dataset_char = sprintf("%03g", dataset)) %>%
  rename(obs = OBS, time = MIN, concentration = CONC, 
         group = patient, phase = pair) %>%
  mutate(group = factor(group, levels = c("c", "p"), 
                        labels = c("Control", "Patient")),
         phase = factor(phase, levels = c("l", "f"),
                        labels = c("Luteal", "Follicular"))) %>%
  select(dataset_char, dataset, group, phase, obs, time, concentration) %>%
  nest(-dataset_char, -dataset, -group, -phase)

# Cleaned
# saveRDS(lh_data, file = "./remote-output/Rds/data_pulse_depression-luteal.Rds")
# rm(idkey, depr.df, labels.df, )


#---------------------------------------
# Plot concentrations
#---------------------------------------

# Plot and save to PDF
{
  pdf(file = file.path(remote_dir, "depression/luteal/concentration-figures.pdf"), 
      paper = "USr",
      width = 0,
      height = 7.5, 
      onefile = TRUE)

  all.figs <- 
    lh_data %>% unnest %>%
      ggplot() +
        geom_path(aes(y = concentration, x = time), size = 0.5) +
        geom_point(aes(y = concentration, x = time), size = 0.5) +
        facet_wrap( ~ dataset_char, nrow = 5, scales = 'free_y') +
        xlab("Time (min)") +
        ylab("Concentration") +
        ggtitle(paste("All luteal, control patients")) 
  
  print(all.figs)
  
  all.figs.same.axes <- 
    lh_data %>% unnest %>%
      ggplot() +
        geom_path(aes(y = concentration, x = time), size = 0.5) +
        geom_point(aes(y = concentration, x = time), size = 0.5) +
        facet_wrap( ~ dataset_char, nrow = 5) + 
        xlab("Time (min)") +
        ylab("Concentration") +
        ggtitle(paste("All luteal, control patients (same axes)")) 
  
  print(all.figs.same.axes)
  
  placeholder <- 
    lapply(unique(lh_data$dataset_char), function(x) {
  
             name <- 
               filter(lh_data, dataset_char == x) %>% select(dataset_char) %>% unique 
  
             fig <- 
               lh_data %>% unnest %>%
                 filter(dataset_char == x) %>%
                 ggplot() +
                   geom_path(aes(y = concentration, x = time)) +
                   geom_point(aes(y = concentration, x = time)) + 
                   xlab("Time (min)") +
                   ylab("Concentration") +
                   ggtitle(paste("Patient:", name))
             print(fig)

                })
  dev.off()
}




#-----------------------------------------------------------
# Create arguments dataset 
#   Based on Karen's arguments for jo75/dataset=9
#-----------------------------------------------------------

# Adjust priors and other args
lh_arg <- 
  lh_arg %>% 
		select(-starts_with("pv"), -starts_with("sv")) %>%
    mutate_all(funs(as.numeric(as.character(.)))) %>%
    mutate(iterations = 500000) %>%
    mutate(prior_mass_mean       = 3.5,
           prior_mass_var        = 100,
           prior_width_mean      = 20,
           prior_width_var       = 1000,
           prior_baseline_mean   = 4.5,
           prior_baseline_var    = 100,
           prior_halflife_mean   = 45,
           prior_halflife_var    = 100,
           prior_mean_num_pulses = 9,
           max_sd_mass           = 10,
           max_sd_width          = 150)

# Refine each datasets priors
lh_arg <- 
	bind_rows(#lh_arg,  #template
						lh_arg %>% mutate(dataset = 6,
															# additionally, range=20 for strauss/hc
															prior_mass_mean       = 1.5,
															prior_baseline_mean   = 3.75,
															prior_mean_num_pulses = 15),
						lh_arg %>% mutate(dataset = 7,
															prior_baseline_mean   = 1,
															prior_mean_num_pulses = 6),
						lh_arg %>% mutate(dataset = 9),
						lh_arg %>% mutate(dataset = 10,
															prior_baseline_mean   = 0.5,
															prior_mean_num_pulses = 6),
						#max.sd.mass = 2,
						#max.sd.width = 2),
						lh_arg %>% mutate(dataset = 16,
															prior_baseline_mean   = 0.5,
															prior_mean_num_pulses = 5),
						#max.sd.mass = 2,
						#max.sd.width = 2),
						lh_arg %>% mutate(dataset = 19,
															prior_baseline_mean   = 0.5,
															prior_mean_num_pulses = 5),
						#max.sd.mass = 2,
						#max.sd.width = 2),
						lh_arg %>% mutate(dataset = 25,
															prior_baseline_mean = 0.5,
															prior_mean_num_pulses = 3),
						#max.sd.mass = 5,
						#max.sd.width = 1),
						lh_arg %>% mutate(dataset = 31,
															prior_width_mean = 55,
															prior_baseline_mean = 0.5,
															prior_mean_num_pulses = 4),
						#max.sd.mass = 2,
						#max.sd.width = 2),
						lh_arg %>% mutate(dataset = 34,
															prior_baseline_mean = 0.5,
															prior_mean_num_pulses = 7),
						#max.sd.mass = 5,
						#max.sd.width = 1), # Trying smaller, most mass was placed at limit of 2
						lh_arg %>% mutate(dataset = 35,
															prior_baseline_mean = 3,
															prior_mean_num_pulses = 8),
						#max.sd.mass = 2,
						#max.sd.width = 2),
						lh_arg %>% mutate(dataset = 46,
															prior_baseline_mean = 1,
															prior_mean_num_pulses = 4),
						#max.sd.mass = 2,
						#max.sd.width = 2),
						lh_arg %>% mutate(dataset = 47,
															prior_baseline_mean = 1,
															prior_mean_num_pulses = 7),
						#max.sd.mass = 2,
						#max.sd.width = 2),
						lh_arg %>% mutate(dataset = 49,
															prior_baseline_mean = 0.5,
															prior_mean_num_pulses = 8),
						#max.sd.mass = 2,
						#max.sd.width = 2)
						) %>%
	mutate(case = "depr_lh_luteal")




priors <- data_frame(case                 = "depr_lh_luteal",
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
                                      prior_location_range = 40),
                    priors %>% mutate(prior_scenario       = "poisson",
                                      location_prior_type  = "strauss",
                                      prior_location_gamma = 1,
                                      prior_location_range = 20))

lh_data <- 
	full_join(priors, lh_arg) %>% 
	mutate(prior_location_range = if_else(dataset == 6 & prior_scenario != "orderstat", 
																				20, prior_location_range)) %>%
	nest(-case, -prior_scenario, -dataset ,.key = "pulse_spec_args") %>%
	full_join(lh_data) %>%
	select(dataset_char, dataset, case, prior_scenario, group, phase, pulse_spec_args, data)

saveRDS(lh_data, "./data/deprstudy-lcontrols-cleaned.Rds")

pryr::object_size(lh_data)


#-------------------------------------------------------------------------------
# End of file
#-------------------------------------------------------------------------------

# new.os        <- create_argrds(args.lp, "orderstat", 121015, data.dir = "depression/luteal")
# #new.str20.010 <- create_argrds(args.lp, "strauss20-010", 121115, data.dir = "depression/luteal")
# #new.hc20      <- create_argrds(args.lp, "hardcore20", 121215, data.dir = "depression/luteal")
# new.pois      <- create_argrds(args.lp, "poisson", 121315, data.dir = "depression/luteal")
# new.str40.010 <- create_argrds(args.lp, "strauss40-010", 121415, data.dir = "depression/luteal")
# new.hc40      <- create_argrds(args.lp, "hardcore40", 121515, data.dir = "depression/luteal")

#-----------------------------------------------------------
# Functions 
#-----------------------------------------------------------

# # Function for creating args Rds files for each mcmc run
# create_argrds <- 
#   function(arg.df, mcmc.name, seed, data.dir = "depr") { 
#     # only 5 valid mcmcnames as of now
#     if (!(mcmc.name %in% c("orderstat", "strauss20-010", "strauss40-010",
#                            "hardcore40", "hardcore20", "hardcore60",
#                            "hardcore10", "poisson"))) {
#       warning("invalid mcmc.name")
#     }
# 
#     # data nickname
#     data.nickname <- str_replace_all(data.dir, "/", "-")
# 
#     # Set strauss prior values
#     pr.gamma <- switch(mcmc.name,
#                        orderstat = -1,
#                        'strauss40-010' = 0.10,
#                        'strauss20-010' = 0.10,
#                        hardcore40 = 0,
#                        hardcore20 = 0,
#                        hardcore60 = 0, 
#                        hardcore10 = 0,
#                        poisson    = 1)
#     pr.range <- switch(mcmc.name,
#                        orderstat = -1,
#                        'strauss40-010' = 40,
#                        'strauss20-010' = 20,
#                        hardcore40 = 40,
#                        hardcore20 = 20,
#                        hardcore60 = 60,
#                        hardcore10 = 10,
#                        poisson    = 20)
# 
#     # Set seed
#     set.seed(seed)
# 
#     # Create new dataframes and sort/keep in correct column order
#     arg.df <- 
#       arg.df %>%
#         mutate(dataset = sprintf("%03d", dataset)) %>%
#         mutate(data.store = "./remote-data/",
#                data.name = data.nickname,
#                mcmc.run.name = mcmc.name,
#                arg.path = paste0("./remote-data/", data.dir, "/", mcmc.name,
#                                  "/args/arg_pulse_", data.nickname, "_", dataset,
#                                  "_", mcmc.name, ".dat"),
#                data.path = paste0("./remote-data/", data.dir, "/data/pulse_",
#                                   data.nickname, "_", dataset, ".dat"),
#                common.path = paste0("./remote-data/", data.dir, "/", mcmc.name,
#                                     "/chains/c_pulse_", data.nickname, "_", dataset,
#                                     "_", mcmc.name, ".dat"),
#                pulse.path = paste0("./remote-data/", data.dir, "/", mcmc.name,
#                                    "/chains/p_pulse_", data.nickname, "_", dataset,
#                                    "_", mcmc.name, ".dat"),
#                seed1 = runif(length(dataset), exp(19), exp(23)),
#                seed2 = runif(length(dataset), exp(19), exp(23)),
#                seed3 = runif(length(dataset), exp(19), exp(23)),
#                prior.location.gamma = pr.gamma,
#                prior.location.range = pr.range) %>%
#         select(dataset, data.store, data.name, mcmc.run.name, arg.path,
#                data.path, common.path, pulse.path, seed1, seed2, seed3,
#                iterations:prior.error.beta, prior.location.gamma,
#                prior.location.range, max.sd.mass:pv.pulselocations) 
#     
#     # Custom range for patient 6 of depression (luteal)
#     arg.df %<>% mutate(prior.location.range = ifelse(dataset == "006", 20, 
#                                                      prior.location.range))
# 
#     saveRDS(arg.df, file = paste0("local-data/arg-", data.nickname, "-", 
#                                   mcmc.name, ".rds")) 
# 
#     return(arg.df)
#   }
# 
