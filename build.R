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
options("verbose" = TRUE)
setwd("~/Projects/Thesis/thesis-analysis/new_dir_str/")

# Number of cores to use - This would need to be incorporated into other scripts
#n.cores <- 4

#---------------------------------------------------------------------
# Install pulsatile package
#---------------------------------------------------------------------

install_github("BayesPulse/pulsatile") #, ref = "tag:biometrics")
library(pulsatile)


#---------------------------------------------------------------------
# A) Create simulated data
#---------------------------------------------------------------------

# Generate simulated data 
source("R/sim-study.R")

# Create pulse_spec objects



