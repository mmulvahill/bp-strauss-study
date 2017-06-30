#-------------------------------------------------------------------------------
# **NEW** Build script for entire thesis-analysis repo
#
# Date: 6/23/2017
#-------------------------------------------------------------------------------

#---------------------------------------------------------------------
# Options and packages
#---------------------------------------------------------------------
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
library(pryr)
        
#options("verbose" = TRUE)
setwd("~/Projects/Thesis/thesis-analysis/new_dir_str/")

# Number of cores to use - This would need to be incorporated into other scripts
#n.cores <- 4

#---------------------------------------------------------------------
# Install pulsatile package
#---------------------------------------------------------------------

# install_github("BayesPulse/pulsatile") #, ref = "tag:biometrics")
library(pulsatile)


#---------------------------------------------------------------------
# A) Create simulated data
#---------------------------------------------------------------------

# Generate simulated data 
source("R/sim_study_data.R")
source("R/sim_study_pulsespecs.R")

# Create pulse_spec objects



