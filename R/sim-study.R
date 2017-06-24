#--------------------------------------------------------------------------------
# sim_study.R
#   Simulation study to assess effects of varying parameters 
#
# Updated: 2017-06-23
#
# Notes:
#   - ggplot2 options were set by library(pulser) in thesis, look there for
#     more detail
#   - Parameters sets notes -- Converted these parameter values roughly to
#     natural scale now that we're using a truncated-t instead of log-normal
#     prior.  Originals were:
#       mass_mean  = 1.25,
#       mass_sd    = 0.50,
#       width_mean = 3.5,
#       width_sd   = 0.50
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
             mass_mean  = 3.5, 
             mass_sd    = 1.65, # should range from exp(0.75) to exp(6.5)
             width_mean = 35,   
             width_sd   = 1.65,
             constant_baseline = 2.6,
             constant_halflife = 45)

# Create and combine cases
sim_parms <- bind_rows(# Reference case #
                       sim_parms,
                       # Low pulse mass case #
                       sim_parms %>% mutate(case = "low-mass",
                                            mass_mean = 0.25,
                                            mass_sd   = 0.50),
                       # High Error Case #
                       sim_parms %>% mutate(case = "high-error",
                                            error_var = 0.02))

#---------------------------------------
# Unused cases
#---------------------------------------
# # High pulse mass
# high_mass           <- reference
# high_mass[["mua"]]  <- 6.5
# high_mass[["sda"]] <- 5
# # Low Error
# low_error           <- reference
# low_error[["vare"]] <- 0.005



#-----------------------------------------------------------
# Generate simulations 
#   Note: sims based on CHG 2013
#-----------------------------------------------------------
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




sim_study$simulation[[1]] %>% .$data %>% plot
sim_study$simulation[[1]] %>% plot

# NOTE: what else does writePulse do other than generate and save the simulated
#       pulse data
# writePulse(parameter.list      = parameter.list,
#            parameter.set.name  = parameter.set.name,
#            number.of.datasets  = number.of.datasets,
#            write.plots.to.file = TRUE)
# TODO: Create plots and save plot pdf


################################################################################
# Old stuff:
################################################################################


#-----------------------------------------------------------
# Run post-processing script -- i.e. combine each case into 
# a single RDS file
# NOTE: Moved to pulser::writePulse() function
#-----------------------------------------------------------
#source("R/sim-postprocess.R")


#-----------------------------------------------------------
# Save parameters as a dataframe for later reference
# and create pdf for Nichole
#-----------------------------------------------------------

# PDF start
pdf(file = "output/pdf/sim-summary.pdf", 
    paper = "USr",
    width = 0,
    height = 0) #width = 11, height = 8.5)


# Sim parameters table
parmstable <- 
  sim.parms %>% 
  t %>% 
  cbind(c("Case", "Seed", "Num of Samples", "Sampling Frequency", 
          "Error Variance", "IPI Mean", "IPI Var", "IPI Min", "Mass Mean",
          "Mass Var", "Width Mean", "Width Var", "Half-life Mean", 
          "Half-life Var", "Baseline Mean", "Baseline Var", 
          "Baseline (Constant)", "Half-life (Constant)"),
        .) %>%
  tableGrob(rows = NULL)

caption <- textGrob("**Note that IPI is on the sampling units scale, but pulse
                    width and half-life are on a minutes scale.",
                    gp = gpar(fontsize = 11))
parmstable %<>% 
  gtable_add_rows(., heights = grobHeight(caption) + unit(0.5, "line"))
parmstable %<>% 
  gtable_add_grob(., 
                  list(caption),
                  t = nrow(.), 
                  l = 1,
                  r = ncol(.))
grid.newpage()
grid.draw(parmstable)
rm(caption, parmstable)

# Sim plots
# Reference
refsims <- 
  readRDS("./output/Rds/sims_pulse_reference.Rds") %>%
  filter(dataset %in% 1:20)

reffigs <- 
  ggplot(data = refsims, aes(x = time, y = concentration)) +
    geom_path() +
    ggtitle("First 20 Simulated Concentrations: Reference Case") +
    ylim(0, max(refsims$concentration)) + 
    facet_wrap(~dataset, nrow = 5, ncol = 4)
print(reffigs)

# Low-mass
lowmasssims <- 
  readRDS("./output/Rds/sims_pulse_low-mass.Rds") %>%
  filter(dataset %in% 1:20)

lowmassfigs <- 
  ggplot(data = lowmasssims, aes(x = time, y = concentration)) +
    geom_path() +
    ggtitle("First 20 Simulated Concentrations: Low Pulse Mass Case") +
    #ylim(0, max(refsims$CONC)) + 
    ylim(0, max(lowmasssims$concentration)) + 
    facet_wrap(~dataset, nrow = 5, ncol = 4)
print(lowmassfigs)

# High-error
higherrorsims <- 
  readRDS("./output/Rds/sims_pulse_high-error.Rds") %>%
  filter(dataset %in% 1:20)

higherrfigs <-
  ggplot(data = higherrorsims, aes(x = time, y = concentration)) +
    geom_path() +
    ggtitle("First 20 Simulated Concentrations: High Error Case") +
    #ylim(0, max(refsims$CONC)) + 
    ylim(0, max(higherrorsims$concentration)) + 
    facet_wrap(~dataset, nrow = 5, ncol = 4)
print(higherrfigs)

dev.off()











# ---------------------------------------------------------------------------- #
# EOF # EOF # EOF # EOF # EOF # EOF # EOF # EOF # EOF # EOF # EOF # EOF # EOF  #  
# ---------------------------------------------------------------------------- #
