################################################################################
# massively.R
#
# 	massively parallel processing with Google Compute Engine and the 
# 	googleComputeEngineR package
#
################################################################################

#---------------------------------------
# Set up workspace
#---------------------------------------

if (!require(googleComputeEngineR)) devtools::install_github("cloudyr/googleComputeEngineR")
if (!require(pulsatile)) devtools::install_github("BayesPulse/pulsatile")
library(googleComputeEngineR)
library(future)
library(pulsatile)
library(tidyverse)


#---------------------------------------
# Set up GCE virtual machines
#---------------------------------------

# Docker container to load on each child vm
my_container <- gce_tag_container("pulsatile-container", project = "thesis-run")
# Auto auth to GCE via environment file arguments (see: https://cloudyr.github.io/googleComputeEngineR/)
# Create 8 CPUs names
vm_names <- paste0("persist", 1:6)
# Specify the cheapest VMs that may get turned off (80% cheaper, but what happens to running code?)
preemptible = list(preemptible = FALSE)

# Want to use gce_vm_container, so we dont' have to wait for each VM to boot to
# start on the next one , but this requires a cloud_init file instead of just
# specifying dockerfile w/ dynamic_image
template 				<- "r-base"
cloud_init      <- get_template_file(template)
cloud_init_file <- readChar(cloud_init, nchars = 32768)
upload_meta     <- list(template = template)
image           <- googleComputeEngineR:::get_image("rocker/r-base", dynamic_image = my_container)
cloud_init_file <- sprintf(cloud_init_file, image)

# Start up 8 VMs with R base on them (can also customise via Dockerfiles using gce_vm_template instead)
# fiftyvms <- 
# 	purrr::map(vm_names, 
# 						 ~ gce_vm_container(name = .,
# 																predefined_type = "n1-standard-4",
# 																scheduling      = preemptible,
# 																cloud_init      = cloud_init_file,
# 																metadata 			  = upload_meta))
# 

fiftyvms <- map(vm_names, gce_vm) # use if lose pointer to vms 
fiftyvms <- purrr::map(fiftyvms, gce_get_op)

## add any ssh details, username etc.
fiftyvms <- purrr::map(fiftyvms, gce_ssh_setup)

## once all launched, add to cluster
plan(cluster, workers = as.cluster(fiftyvms))

## the action you want to perform via cluster
library(pulsatile)
library(tidyverse)

simstudy_test <- 
	readRDS("~/Projects/BayesPulse/bp-strauss-study/output/sim_study.Rds") %>%
	filter(case == "reference" & 
           prior_scenario %in% c("orderstat", "hardcore40", "strauss40_010")) %>%
 	filter(prior_scenario == "orderstat")  %>% 
	filter(sim_num %in% 1:48) %>%
  mutate(seed = 1:nrow(.))

my_big_function <- function(x) {
	library(parallel)
	mclapply(x, function(y) {
             set.seed(simstudy_test$seed)
						 .data <- simstudy_test$simulation[[y]]
						 .spec <- simstudy_test$this_spec[[y]]
						 fit_pulse(.data = .data, spec = .spec, iters = 250000, burnin = 0, thin = 1)
					 })
}

## use future::future_lapply to send each call to the cluster
assignments <- 1:nrow(simstudy_test)
assignments <- split(assignments, ceiling(seq_along(assignments)/6))

all_results <- future_lapply(assignments, my_big_function, future.seed = TRUE)

resolved(all_results)
saveRDS(all_results, file = "output_xl/all_google_results.Rds")

## tidy up
lapply(fiftyvms, FUN = gce_vm_stop)
lapply(fiftyvms, FUN = gce_vm_delete)


all_results <- all_results %>% purrr::flatten(.) 
source("R/bulk_diagnostics.R")

print_diag_PDF(

