#-------------------------------------------------------------------------------
# **NEW** Build script for entire thesis-analysis repo
#
# Date: 6/23/2017 #-------------------------------------------------------------------------------

#---------------------------------------------------------------------
# Options and packages
#---------------------------------------------------------------------

# Is this a test (smaller sample) run?
TEST <- TRUE
RUN  <- FALSE


library(knitr)
library(devtools)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(stringr)
library(pryr) # consider removing after development
library(assertr) 
library(readr)
library(ggthemes)

devtools::install_github("hadley/multidplyr", ref = "0085ded")
library(multidplyr)

theme_set(theme_tufte())
        
setwd("~/Projects/BayesPulse/bp-strauss-study/")

# Number of cores to use - This would need to be incorporated into other scripts
#n.cores <- 4
# options("stringsAsFactors" = FALSE)
#options("verbose" = TRUE)
#remote_dir <- "./remote-storage/"
# switch(Sys.info()[["sysname"]],
       #Linux = 
file.remove("remote-storage")
file.symlink("~/Projects/BayesPulse/remote-storage", "remote-storage")
remote_dir <- "./remote-storage"

#---------------------------------------------------------------------
# Install pulsatile package
#---------------------------------------------------------------------

devtools::install("lib/pulsatile") #, ref = "tag:biometrics")
library(pulsatile)


#---------------------------------------------------------------------
# A) Create simulated data
#---------------------------------------------------------------------
# Generate simulated data 
source("R/sim_study_data.R")
# Create pulse_spec objects, and join with sim_study object
source("R/sim_study_pulsespecs.R")
# sim_study object (data_frame w/ list-cols) now contains all info by row
# needed to run mcmc


#---------------------------------------------------------------------
# B) Prepare healthy patient data from LH Depression Study
#---------------------------------------------------------------------
###source("R/depression_lh_deident.R") # Run once from a PHI-safe computer
# source("R/depression_lh_data.R") # temporarily commented out


#---------------------------------------------------------------------
# 1) Run MCMC analyses
#---------------------------------------------------------------------
source("R/bulk_diagnostics_pdf.R")
source("R/fit_models.R")

# apply pulsespec()
# run fit_pulse()
# Post-process -- think this is just creating diagnostics now


