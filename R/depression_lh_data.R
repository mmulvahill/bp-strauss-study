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


# Setup
# options("stringsAsFactors" = FALSE)
# library(pulser)
# library(readr)
# library(stringr)
# library(ggthemes)
# theme_set(theme_tufte())




#-----------------------------------------------------------
# Functions 
#-----------------------------------------------------------

# Function for creating args Rds files for each mcmc run
create_argrds <- 
  function(arg.df, mcmc.name, seed, data.dir = "depr") { 
    # only 5 valid mcmcnames as of now
    if (!(mcmc.name %in% c("orderstat", "strauss20-010", "strauss40-010",
                           "hardcore40", "hardcore20", "hardcore60",
                           "hardcore10", "poisson"))) {
      warning("invalid mcmc.name")
    }

    # data nickname
    data.nickname <- str_replace_all(data.dir, "/", "-")

    # Set strauss prior values
    pr.gamma <- switch(mcmc.name,
                       orderstat = -1,
                       'strauss40-010' = 0.10,
                       'strauss20-010' = 0.10,
                       hardcore40 = 0,
                       hardcore20 = 0,
                       hardcore60 = 0, 
                       hardcore10 = 0,
                       poisson    = 1)
    pr.range <- switch(mcmc.name,
                       orderstat = -1,
                       'strauss40-010' = 40,
                       'strauss20-010' = 20,
                       hardcore40 = 40,
                       hardcore20 = 20,
                       hardcore60 = 60,
                       hardcore10 = 10,
                       poisson    = 20)

    # Set seed
    set.seed(seed)

    # Create new dataframes and sort/keep in correct column order
    arg.df <- 
      arg.df %>%
        mutate(dataset = sprintf("%03d", dataset)) %>%
        mutate(data.store = "./remote-data/",
               data.name = data.nickname,
               mcmc.run.name = mcmc.name,
               arg.path = paste0("./remote-data/", data.dir, "/", mcmc.name,
                                 "/args/arg_pulse_", data.nickname, "_", dataset,
                                 "_", mcmc.name, ".dat"),
               data.path = paste0("./remote-data/", data.dir, "/data/pulse_",
                                  data.nickname, "_", dataset, ".dat"),
               common.path = paste0("./remote-data/", data.dir, "/", mcmc.name,
                                    "/chains/c_pulse_", data.nickname, "_", dataset,
                                    "_", mcmc.name, ".dat"),
               pulse.path = paste0("./remote-data/", data.dir, "/", mcmc.name,
                                   "/chains/p_pulse_", data.nickname, "_", dataset,
                                   "_", mcmc.name, ".dat"),
               seed1 = runif(length(dataset), exp(19), exp(23)),
               seed2 = runif(length(dataset), exp(19), exp(23)),
               seed3 = runif(length(dataset), exp(19), exp(23)),
               prior.location.gamma = pr.gamma,
               prior.location.range = pr.range) %>%
        select(dataset, data.store, data.name, mcmc.run.name, arg.path,
               data.path, common.path, pulse.path, seed1, seed2, seed3,
               iterations:prior.error.beta, prior.location.gamma,
               prior.location.range, max.sd.mass:pv.pulselocations) 
    
    # Custom range for patient 6 of depression (luteal)
    arg.df %<>% mutate(prior.location.range = ifelse(dataset == "006", 20, 
                                                     prior.location.range))

    saveRDS(arg.df, file = paste0("local-data/arg-", data.nickname, "-", 
                                  mcmc.name, ".rds")) 

    return(arg.df)
  }



#---------------------------------------
# Read in file containing IDs, labels and groups 
#---------------------------------------
file.name <- "../remote-data/depression/luteal/Grouplabels.txt"
labels.colnames <- read_lines(file.name, skip = 7)[1] %>% 
  str_split(., pattern = " ") %>% unlist %>% .[. != ""] %>% .[c(1,2,8)]

labels.df <- read_fwf(file.name, skip = 8, 
                      fwf_positions(c(1, 9, 57),
                                    c(1, 12,57),
                                    col_names = labels.colnames)) %>%
             arrange(patient, pair) 

rm(labels.colnames, file.name)

#-----------------------------------------------------------
#  Read in data, combine with labels/groups, save as a combined dataframe, and
#  write out as separate space-delim files
#-----------------------------------------------------------
# Read NC's combined dataset
depr.df <- read_csv("../remote-data/depression/luteal/LH_all.csv") %>%
  mutate(sw51 = lead(sw51, n = 9)) 


# Clean/combine
# Change to long format and keep luteinizing phase, controls
long <- depr.df %>% 
  gather(key = idno, value = CONC, ru08:ac06) %>%
  mutate(idno = as.character(idno)) %>%
  left_join(labels.df, by = "idno")  %>%
  filter(!is.na(CONC)) 

# Create my own ids (idno is not deidentified, and don't have the full set of
# matching idno/karen's id number). Actually, it looks like this is the same way
# karen/nichole created ids, just based on jo75=9.
idkey <- long %>% select(idno) %>% unique %>% mutate(dataset = 1:n())
saveRDS(idkey, "../remote-data/depression/luteal/idkey.Rds")


# Filter to only control patients in luteal phase
long <- long %>% 
  left_join(., idkey, by = "idno") %>% filter(pair == "l" & patient == "c") %>%
  select(-idno)


#---------------------------------------
# Save 
#---------------------------------------

# NOT NEEDED 
# Save as separate space delimited files in data directory
# dir.create("../remote-data/depression/luteal/data/")
# lapply(unique(long$dataset), function(x) {
#          dat <- filter(long, dataset == x)
#          num <- x %>% sprintf("%03d", .)
#          dat %>%
#            select(OBS, MIN, CONC) %>%
#            write.table(., sep = " ", quote = FALSE, row.names = FALSE,
#                        file = paste0("remote-data/depression/luteal/data/pulse_depression-luteal_",
#                                      num, ".dat"))
#                       })

# Save Rds version for later use
long <-
  long %>%
    select(-TIME) %>% 
    rename(obs = OBS,
           time = MIN,
           concentration = CONC) %>%
    mutate(patient = factor(patient, levels = c("c", "p"), 
                            labels = c("Control", "Patient")),
           pair = factor(pair, levels = c("l", "f"),
                         labels = c("Luteal phase", "Follicular phase")))

# Full data
saveRDS(long, file = "remote-output/Rds/data_pulse_depression-luteal.Rds")




#---------------------------------------
# PLOT
#---------------------------------------

# Add combined idno/subjnum variables
long %<>% 
  mutate(dataset.char = sprintf("%03g", dataset))

# Plot and save to PDF
{
  pdf(file = "remote-data/depression/luteal/concentration-figures.pdf", 
      paper = "USr",
      width = 0,
      height = 7.5, 
      onefile = TRUE)

  all.figs <- 
    long %>%
      ggplot() +
        geom_path(aes(y = concentration, x = time), 
                  size = 0.5) +
        geom_point(aes(y = concentration, x = time), 
                  size = 0.5) +
        facet_wrap( ~ dataset.char, nrow = 5, scales = 'free_y') +
        xlab("Time (min)") +
        ylab("Concentration") +
        ggtitle(paste("All luteal, control patients")) 
  
  print(all.figs)
  
  all.figs.same.axes <- 
    long %>%
      ggplot() +
        geom_path(aes(y = concentration, x = time), 
                  size = 0.5) +
        geom_point(aes(y = concentration, x = time), 
                  size = 0.5) +
        facet_wrap( ~ dataset.char, nrow = 5) + 
        xlab("Time (min)") +
        ylab("Concentration") +
        ggtitle(paste("All luteal, control patients (same axes)")) 
  
  print(all.figs.same.axes)
  
  placeholder <- 
    lapply(unique(long$dataset.char), function(x) {
  
             name <- 
               filter(long, dataset.char == x) %>% select(dataset.char) %>% unique 
  
             fig <- 
               long %>%
                 filter(dataset.char == x) %>%
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


# Read in argument file, split, and convert to dataframe
arg.dataset9 <- 
  readLines("remote-data/depression/luteal/input9.txt") %>%
  str_split(., " ") %>% do.call(c, .) %>%
  as.matrix(byrow=TRUE) %>% t %>%
  as.data.frame %>% 
    tbl_df 

# Label variables
colnames(arg.dataset9) <- c("indata",
                            "out.common",
                            "out.pulse",
                            "iterations",
                            "prior.mass.mean",
                            "prior.mass.var",
                            "prior.width.mean",
                            "prior.width.var",
                            "prior.baseline.mean",
                            "prior.baseline.var",
                            "prior.halflife.mean",
                            "prior.halflife.var",
                            "prior.error.alpha",
                            "prior.error.beta",
                            "max.sd.mass",
                            "max.sd.width",
                            "prior.mean.num.pulses",
                            "sv.mass.mean",
                            "sv.width.mean",
                            "sv.baseline.mean",
                            "sv.halflife.mean",
                            "sv.error.var",
                            "sv.mass.sd",
                            "sv.width.sd",
                            "pv.baseline",
                            "pv.halflife",
                            "pv.mass",
                            "pv.width",
                            "pv.re.mass",
                            "pv.re.width",
                            "pv.pulselocations")

# Adjust priors and other args
arg.dataset9 <- 
  arg.dataset9 %>% 
    mutate_each(funs(as.numeric(as.character(.))), 
                iterations:pv.pulselocations) %>%
    mutate(iterations = 500000) %>%
    mutate(prior.mass.mean       = 1.25,
           prior.mass.var        = 10,
           prior.width.mean      = 3,
           prior.width.var       = 10,
           prior.baseline.mean   = 4.5,
           prior.baseline.var    = 100,
           prior.halflife.mean   = 45,
           prior.halflife.var    = 100,
           prior.mean.num.pulses = 9,
           max.sd.width          = 1,
           sv.mass.mean          = 1,
           sv.width.mean         = 3,
           sv.mass.sd            = 0.5)

# Set up for all 13 patients, based on dataset 9
args.lp <- long %>% select(dataset) %>% unique %>% cbind(., arg.dataset9)
rm(arg.dataset9)

# Refine each datasets priors
{
  # Patient 6
  #   additionally, range=20 for strauss/hc
  args.lp[args.lp$dataset == 6, "prior.mass.mean"]       <- 0.25
  args.lp[args.lp$dataset == 6, "prior.mass.var"]        <- 10
  args.lp[args.lp$dataset == 6, "prior.width.mean"]      <- 3
  args.lp[args.lp$dataset == 6, "prior.width.var"]       <- 10
  args.lp[args.lp$dataset == 6, "prior.baseline.mean"]   <- 3.75
  args.lp[args.lp$dataset == 6, "prior.baseline.var"]    <- 100
  args.lp[args.lp$dataset == 6, "prior.halflife.mean"]   <- 45
  args.lp[args.lp$dataset == 6, "prior.halflife.var"]    <- 100
  args.lp[args.lp$dataset == 6, "prior.mean.num.pulses"] <- 15
  args.lp[args.lp$dataset == 6, "max.sd.mass"]           <- 2
  args.lp[args.lp$dataset == 6, "max.sd.width"]          <- 1
  args.lp[args.lp$dataset == 6, "sv.mass.mean"]          <- 0.25
  args.lp[args.lp$dataset == 6, "sv.width.mean"]         <- 3
  args.lp[args.lp$dataset == 6, "sv.mass.sd"]            <- 0.5
  # Patient 7
  args.lp[args.lp$dataset == 7, "prior.mass.mean"]       <- 1.25
  args.lp[args.lp$dataset == 7, "prior.mass.var"]        <- 10
  args.lp[args.lp$dataset == 7, "prior.width.mean"]      <- 3
  args.lp[args.lp$dataset == 7, "prior.width.var"]       <- 10
  args.lp[args.lp$dataset == 7, "prior.baseline.mean"]   <- 1
  args.lp[args.lp$dataset == 7, "prior.baseline.var"]    <- 100
  args.lp[args.lp$dataset == 7, "prior.halflife.mean"]   <- 45
  args.lp[args.lp$dataset == 7, "prior.halflife.var"]    <- 100
  args.lp[args.lp$dataset == 7, "prior.mean.num.pulses"] <- 6
  args.lp[args.lp$dataset == 7, "max.sd.mass"]           <- 5
  args.lp[args.lp$dataset == 7, "max.sd.width"]          <- 1
  args.lp[args.lp$dataset == 7, "sv.mass.mean"]          <- 1.25
  args.lp[args.lp$dataset == 7, "sv.width.mean"]         <- 3
  args.lp[args.lp$dataset == 7, "sv.mass.sd"]            <- 0.5
  # Patient 9
  args.lp[args.lp$dataset == 9, "prior.mass.mean"]       <- 1.25
  args.lp[args.lp$dataset == 9, "prior.mass.var"]        <- 10
  args.lp[args.lp$dataset == 9, "prior.width.mean"]      <- 3
  args.lp[args.lp$dataset == 9, "prior.width.var"]       <- 10
  args.lp[args.lp$dataset == 9, "prior.baseline.mean"]   <- 4.5
  args.lp[args.lp$dataset == 9, "prior.baseline.var"]    <- 100
  args.lp[args.lp$dataset == 9, "prior.halflife.mean"]   <- 45
  args.lp[args.lp$dataset == 9, "prior.halflife.var"]    <- 100
  args.lp[args.lp$dataset == 9, "prior.mean.num.pulses"] <- 9
  args.lp[args.lp$dataset == 9, "max.sd.mass"]           <- 1
  args.lp[args.lp$dataset == 9, "max.sd.width"]          <- 1
  args.lp[args.lp$dataset == 9, "sv.mass.mean"]          <- 1
  args.lp[args.lp$dataset == 9, "sv.width.mean"]         <- 3
  args.lp[args.lp$dataset == 9, "sv.mass.sd"]            <- 0.5
  # Patient 10args.lp$
  args.lp[args.lp$dataset == 10, "prior.mass.mean"]       <- 1.25
  args.lp[args.lp$dataset == 10, "prior.mass.var"]        <- 10
  args.lp[args.lp$dataset == 10, "prior.width.mean"]      <- 3
  args.lp[args.lp$dataset == 10, "prior.width.var"]       <- 10
  args.lp[args.lp$dataset == 10, "prior.baseline.mean"]   <- 0.5
  args.lp[args.lp$dataset == 10, "prior.baseline.var"]    <- 100
  args.lp[args.lp$dataset == 10, "prior.halflife.mean"]   <- 45
  args.lp[args.lp$dataset == 10, "prior.halflife.var"]    <- 100
  args.lp[args.lp$dataset == 10, "prior.mean.num.pulses"] <- 6
  args.lp[args.lp$dataset == 10, "max.sd.mass"]           <- 2
  args.lp[args.lp$dataset == 10, "max.sd.width"]          <- 2
  args.lp[args.lp$dataset == 10, "sv.mass.mean"]          <- 1.25
  args.lp[args.lp$dataset == 10, "sv.width.mean"]         <- 3
  args.lp[args.lp$dataset == 10, "sv.mass.sd"]            <- 0.5
  # Patient 16args.lp$
  args.lp[args.lp$dataset == 16, "prior.mass.mean"]       <- 1.25
  args.lp[args.lp$dataset == 16, "prior.mass.var"]        <- 10
  args.lp[args.lp$dataset == 16, "prior.width.mean"]      <- 3
  args.lp[args.lp$dataset == 16, "prior.width.var"]       <- 10
  args.lp[args.lp$dataset == 16, "prior.baseline.mean"]   <- 0.5
  args.lp[args.lp$dataset == 16, "prior.baseline.var"]    <- 100
  args.lp[args.lp$dataset == 16, "prior.halflife.mean"]   <- 45
  args.lp[args.lp$dataset == 16, "prior.halflife.var"]    <- 100
  args.lp[args.lp$dataset == 16, "prior.mean.num.pulses"] <- 5
  args.lp[args.lp$dataset == 16, "max.sd.mass"]           <- 2
  args.lp[args.lp$dataset == 16, "max.sd.width"]          <- 2
  args.lp[args.lp$dataset == 16, "sv.mass.mean"]          <- 1.25
  args.lp[args.lp$dataset == 16, "sv.width.mean"]         <- 3
  args.lp[args.lp$dataset == 16, "sv.mass.sd"]            <- 0.5
  # Patient 19args.lp$
  args.lp[args.lp$dataset == 19, "prior.mass.mean"]       <- 1.25
  args.lp[args.lp$dataset == 19, "prior.mass.var"]        <- 10
  args.lp[args.lp$dataset == 19, "prior.width.mean"]      <- 3
  args.lp[args.lp$dataset == 19, "prior.width.var"]       <- 10
  args.lp[args.lp$dataset == 19, "prior.baseline.mean"]   <- 0.5
  args.lp[args.lp$dataset == 19, "prior.baseline.var"]    <- 100
  args.lp[args.lp$dataset == 19, "prior.halflife.mean"]   <- 45
  args.lp[args.lp$dataset == 19, "prior.halflife.var"]    <- 100
  args.lp[args.lp$dataset == 19, "prior.mean.num.pulses"] <- 5
  args.lp[args.lp$dataset == 19, "max.sd.mass"]           <- 2
  args.lp[args.lp$dataset == 19, "max.sd.width"]          <- 2
  args.lp[args.lp$dataset == 19, "sv.mass.mean"]          <- 1.25
  args.lp[args.lp$dataset == 19, "sv.width.mean"]         <- 3
  args.lp[args.lp$dataset == 19, "sv.mass.sd"]            <- 0.5
  # Patient 25args.lp$
  args.lp[args.lp$dataset == 25, "prior.mass.mean"]       <- 1.25
  args.lp[args.lp$dataset == 25, "prior.mass.var"]        <- 10
  args.lp[args.lp$dataset == 25, "prior.width.mean"]      <- 3
  args.lp[args.lp$dataset == 25, "prior.width.var"]       <- 10
  args.lp[args.lp$dataset == 25, "prior.baseline.mean"]   <- 0.5
  args.lp[args.lp$dataset == 25, "prior.baseline.var"]    <- 100
  args.lp[args.lp$dataset == 25, "prior.halflife.mean"]   <- 45
  args.lp[args.lp$dataset == 25, "prior.halflife.var"]    <- 100
  args.lp[args.lp$dataset == 25, "prior.mean.num.pulses"] <- 3
  args.lp[args.lp$dataset == 25, "max.sd.mass"]           <- 5
  args.lp[args.lp$dataset == 25, "max.sd.width"]          <- 1
  args.lp[args.lp$dataset == 25, "sv.mass.mean"]          <- 1.25
  args.lp[args.lp$dataset == 25, "sv.width.mean"]         <- 3
  args.lp[args.lp$dataset == 25, "sv.mass.sd"]            <- 0.5
  # Patient 31args.lp$
  args.lp[args.lp$dataset == 31, "prior.mass.mean"]       <- 1.25
  args.lp[args.lp$dataset == 31, "prior.mass.var"]        <- 10
  args.lp[args.lp$dataset == 31, "prior.width.mean"]      <- 4
  args.lp[args.lp$dataset == 31, "prior.width.var"]       <- 10
  args.lp[args.lp$dataset == 31, "prior.baseline.mean"]   <- 0.5
  args.lp[args.lp$dataset == 31, "prior.baseline.var"]    <- 100
  args.lp[args.lp$dataset == 31, "prior.halflife.mean"]   <- 45
  args.lp[args.lp$dataset == 31, "prior.halflife.var"]    <- 100
  args.lp[args.lp$dataset == 31, "prior.mean.num.pulses"] <- 4
  args.lp[args.lp$dataset == 31, "max.sd.mass"]           <- 2
  args.lp[args.lp$dataset == 31, "max.sd.width"]          <- 2
  args.lp[args.lp$dataset == 31, "sv.mass.mean"]          <- 1.25
  args.lp[args.lp$dataset == 31, "sv.width.mean"]         <- 3
  args.lp[args.lp$dataset == 31, "sv.mass.sd"]            <- 0.5
  # Patient 34args.lp$
  args.lp[args.lp$dataset == 34, "prior.mass.mean"]       <- 1.25
  args.lp[args.lp$dataset == 34, "prior.mass.var"]        <- 10
  args.lp[args.lp$dataset == 34, "prior.width.mean"]      <- 3
  args.lp[args.lp$dataset == 34, "prior.width.var"]       <- 10
  args.lp[args.lp$dataset == 34, "prior.baseline.mean"]   <- 0.5
  args.lp[args.lp$dataset == 34, "prior.baseline.var"]    <- 100
  args.lp[args.lp$dataset == 34, "prior.halflife.mean"]   <- 45
  args.lp[args.lp$dataset == 34, "prior.halflife.var"]    <- 100
  args.lp[args.lp$dataset == 34, "prior.mean.num.pulses"] <- 7
  args.lp[args.lp$dataset == 34, "max.sd.mass"]           <- 5 
  args.lp[args.lp$dataset == 34, "max.sd.width"]          <- 1 # Trying smaller, most mass was placed at limit of 2
  args.lp[args.lp$dataset == 34, "sv.mass.mean"]          <- 1.25
  args.lp[args.lp$dataset == 34, "sv.width.mean"]         <- 3
  args.lp[args.lp$dataset == 34, "sv.mass.sd"]            <- 0.5
  # Patient 35args.lp$
  args.lp[args.lp$dataset == 35, "prior.mass.mean"]       <- 1.25
  args.lp[args.lp$dataset == 35, "prior.mass.var"]        <- 10
  args.lp[args.lp$dataset == 35, "prior.width.mean"]      <- 3
  args.lp[args.lp$dataset == 35, "prior.width.var"]       <- 10
  args.lp[args.lp$dataset == 35, "prior.baseline.mean"]   <- 3
  args.lp[args.lp$dataset == 35, "prior.baseline.var"]    <- 100
  args.lp[args.lp$dataset == 35, "prior.halflife.mean"]   <- 45
  args.lp[args.lp$dataset == 35, "prior.halflife.var"]    <- 100
  args.lp[args.lp$dataset == 35, "prior.mean.num.pulses"] <- 8
  args.lp[args.lp$dataset == 35, "max.sd.mass"]           <- 2
  args.lp[args.lp$dataset == 35, "max.sd.width"]          <- 2
  args.lp[args.lp$dataset == 35, "sv.mass.mean"]          <- 1.25
  args.lp[args.lp$dataset == 35, "sv.width.mean"]         <- 3
  args.lp[args.lp$dataset == 35, "sv.mass.sd"]            <- 0.5
  # Patient 46args.lp$
  args.lp[args.lp$dataset == 46, "prior.mass.mean"]       <- 1.25
  args.lp[args.lp$dataset == 46, "prior.mass.var"]        <- 10
  args.lp[args.lp$dataset == 46, "prior.width.mean"]      <- 3
  args.lp[args.lp$dataset == 46, "prior.width.var"]       <- 10
  args.lp[args.lp$dataset == 46, "prior.baseline.mean"]   <- 1
  args.lp[args.lp$dataset == 46, "prior.baseline.var"]    <- 100
  args.lp[args.lp$dataset == 46, "prior.halflife.mean"]   <- 45
  args.lp[args.lp$dataset == 46, "prior.halflife.var"]    <- 100
  args.lp[args.lp$dataset == 46, "prior.mean.num.pulses"] <- 4
  args.lp[args.lp$dataset == 46, "max.sd.mass"]           <- 2
  args.lp[args.lp$dataset == 46, "max.sd.width"]          <- 2
  args.lp[args.lp$dataset == 46, "sv.mass.mean"]          <- 1.25
  args.lp[args.lp$dataset == 46, "sv.width.mean"]         <- 3
  args.lp[args.lp$dataset == 46, "sv.mass.sd"]            <- 0.5
  # Patient 47args.lp$
  args.lp[args.lp$dataset == 47, "prior.mass.mean"]       <- 1.25
  args.lp[args.lp$dataset == 47, "prior.mass.var"]        <- 10
  args.lp[args.lp$dataset == 47, "prior.width.mean"]      <- 3
  args.lp[args.lp$dataset == 47, "prior.width.var"]       <- 10
  args.lp[args.lp$dataset == 47, "prior.baseline.mean"]   <- 1
  args.lp[args.lp$dataset == 47, "prior.baseline.var"]    <- 100
  args.lp[args.lp$dataset == 47, "prior.halflife.mean"]   <- 45
  args.lp[args.lp$dataset == 47, "prior.halflife.var"]    <- 100
  args.lp[args.lp$dataset == 47, "prior.mean.num.pulses"] <- 7
  args.lp[args.lp$dataset == 47, "max.sd.mass"]           <- 2
  args.lp[args.lp$dataset == 47, "max.sd.width"]          <- 2
  args.lp[args.lp$dataset == 47, "sv.mass.mean"]          <- 1.25
  args.lp[args.lp$dataset == 47, "sv.width.mean"]         <- 3
  args.lp[args.lp$dataset == 47, "sv.mass.sd"]            <- 0.5
  # Patient 49args.lp$
  args.lp[args.lp$dataset == 49, "prior.mass.mean"]       <- 1.25
  args.lp[args.lp$dataset == 49, "prior.mass.var"]        <- 10
  args.lp[args.lp$dataset == 49, "prior.width.mean"]      <- 3
  args.lp[args.lp$dataset == 49, "prior.width.var"]       <- 10
  args.lp[args.lp$dataset == 49, "prior.baseline.mean"]   <- 0.5
  args.lp[args.lp$dataset == 49, "prior.baseline.var"]    <- 100
  args.lp[args.lp$dataset == 49, "prior.halflife.mean"]   <- 45
  args.lp[args.lp$dataset == 49, "prior.halflife.var"]    <- 100
  args.lp[args.lp$dataset == 49, "prior.mean.num.pulses"] <- 8
  args.lp[args.lp$dataset == 49, "max.sd.mass"]           <- 2
  args.lp[args.lp$dataset == 49, "max.sd.width"]          <- 2
  args.lp[args.lp$dataset == 49, "sv.mass.mean"]          <- 1.25
  args.lp[args.lp$dataset == 49, "sv.width.mean"]         <- 3
  args.lp[args.lp$dataset == 49, "sv.mass.sd"]            <- 0.5
}
         

new.os        <- create_argrds(args.lp, "orderstat", 121015, data.dir = "depression/luteal")
#new.str20.010 <- create_argrds(args.lp, "strauss20-010", 121115, data.dir = "depression/luteal")
#new.hc20      <- create_argrds(args.lp, "hardcore20", 121215, data.dir = "depression/luteal")
new.pois      <- create_argrds(args.lp, "poisson", 121315, data.dir = "depression/luteal")
new.str40.010 <- create_argrds(args.lp, "strauss40-010", 121415, data.dir = "depression/luteal")
new.hc40      <- create_argrds(args.lp, "hardcore40", 121515, data.dir = "depression/luteal")




#-------------------------------------------------------------------------------
# End of file
#-------------------------------------------------------------------------------
