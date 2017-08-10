#--------------------------------------------------------------------------------
# sim_study.R
#   Simulation study to assess effects of varying parameters 
#
# Updated: 2017-06-23
#
# Notes:
#   - This file replaces: - R/sim-parms.R
#                         - R/sim-study.R
#   - ggplot2 options were set by library(pulser) in thesis, look there for
#     more detail
#   - Parameters sets notes -- Converted these parameter values roughly to
#     natural scale now that we're using a truncated-t instead of log-normal
#     prior.  Originals were:
#       mass_mean  = 1.25,
#       mass_sd    = 0.50,
#       width_mean = 3.5,
#       width_sd   = 0.50
#   - Unused cases
#       High pulse mass: mass_mean <- 6.5; mass_sd <- 5
#       Low error: error_var <- 0.005
#--------------------------------------------------------------------------------


#---------------------------------------
# Set up parameters
#---------------------------------------
# Initial/Reference case
sim_parms <- 
  data_frame(case       = "reference", 
             num_obs    = 144,  
             interval   = 10,  
             error_var  = 0.005, # Error variance OR coefficient of variation (percent of mean)
             ipi_mean   = 12, 
             ipi_var    = 40, 
             ipi_min    = 4,  
             mass_mean  = 4, 
             mass_sd    = 2, # should range from exp(0.75) to exp(6.5)
             width_mean = 5,
             width_sd   = 1,
             constant_baseline = 2.6,
             constant_halflife = 45)

# Create and combine cases
sim_parms <- bind_rows(# Reference case #
                       sim_parms,
                       # Low pulse mass case #
                       sim_parms %>% mutate(case = "low-mass",
                                            mass_mean = 2,
                                            mass_sd   = 1),
                       # High Error Case #
                       sim_parms %>% mutate(case = "high-error",
                                            error_var = 0.02))


#---------------------------------------
# Generate simulations 
#   Note: sims based on CHG 2013
#---------------------------------------
set.seed(2017-06-23)
number_of_datasets <- 200

# This is a beautifull thing... 
# Create all simulations
sim_study <- 
  sim_parms %>% 
    slice(rep(1:n(), each = number_of_datasets)) %>%
    group_by(case) %>% 
    mutate(sim_num = 1:n()) %>%
    ungroup %>% 
    nest(-case, -sim_num, .key = "sim_parms") %>%
    mutate(simulation = map(sim_parms, ~ do.call(simulate_pulse, .x)))

# sim_study$simulation[1:5] %>% map(plot)


#---------------------------------------
# Save parameters as a dataframe for 
#   later reference and create pdf for Nichole
#---------------------------------------

# Sim parameters table
caption <- 
  "**Note that IPI is on the sampling units scale, but pulse
                    width and half-life are on a minutes scale." %>%
  textGrob(gp = gpar(fontsize = 11))

parmstable <- sim_parms %>% t %>% cbind(rownames(.), .) %>%
  tableGrob(rows = NULL) %>% 
  gtable_add_rows(., heights = grobHeight(caption) + unit(0.5, "line")) %>%
  gtable_add_grob(., list(caption), t = nrow(.), l = 1, r = ncol(.))

# Sim plots
sim_figs <- 
  sim_study %>% group_by(case) %>% 
    filter(sim_num %in% 1:20) %>% 
    mutate(time_series = map(simulation, ~ .x$data)) %>%
    select(-sim_parms, -simulation) %>% 
    unnest %>% split(.$case) %>%
    map(function(x) {
           ggplot(data = x, aes(x = time, y = concentration)) +
             geom_point(size = 0.5) +
             geom_path() +
             ylim(0, max(x$concentration)) + 
             facet_wrap(~ sim_num, nrow = 5, ncol = 4) +
             ggtitle(paste("First 20 Simulated Concentrations",
                           unique(x$case))) 
                      })

# Create PDF 
pdf(file = "output/sim_study_summary.pdf", paper = "USr", width = 0, height = 0)
grid.newpage()
grid.draw(parmstable)
sim_figs
dev.off()


#-----------------------------------------------------------
# Clean up workspace
#-----------------------------------------------------------
rm(sim_parms, number_of_datasets, sim_figs, caption, parmstable)




# ---------------------------------------------------------------------------- #
# EOF # EOF # EOF # EOF # EOF # EOF # EOF # EOF # EOF # EOF # EOF # EOF # EOF  #  
# ---------------------------------------------------------------------------- #
