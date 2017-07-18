#-------------------------------------------------------------------------------
# depression_lh_deident.R
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
# Provided
lh_labels_file <- file.path(remote_dir, "depression/luteal/Grouplabels.txt")
lh_data_file   <- file.path(remote_dir, "depression/luteal/LH_all.csv")
lh_arg_file    <- file.path(remote_dir, "depression/luteal/input9.txt")

# Created
idkey_file     <- file.path(remote_dir, "depression/luteal/idkey.Rds")
lh_deident_data_file <- file.path("./data/deprstudy-lhcontrols-deident.Rds")
lh_deident_arg_file  <- file.path("./data/deprstudy-lhcontrols-argtemplate.Rds")


#-----------------------------------------------------------
# Read in file containing IDs, labels and patient's study group
#-----------------------------------------------------------
labels_colnames <- read_lines(lh_labels_file, skip = 7)[1] %>% 
  str_split(., pattern = " ") %>% unlist %>% .[. != ""] %>% .[c(1,2,8)]

labels_df <- read_fwf(lh_labels_file, skip = 8, 
                      fwf_positions(c(1, 9, 57), c(1, 12,57),
																		col_names = labels_colnames)) %>%
             arrange(patient, pair) 

rm(labels_colnames, lh_labels_file)


#-----------------------------------------------------------
#  Read in data, join with labels/groups data, save as a 
#  combined dataframe (orig dataset is NC's combined dataset)
#-----------------------------------------------------------
# Change to long format 
lh_data <- 
	read_csv(lh_data_file) %>%
	gather(key = idno, value = CONC, -OBS, -MIN, -TIME) %>%
	mutate(idno = as.character(idno)) %>%
	left_join(labels_df, by = "idno")  %>%
	filter(!is.na(CONC)) 

# Create my own ids (idno is not deidentified, and don't have the full set of
# matching idno/karen's id number). 
idkey <- lh_data %>% select(idno) %>% unique %>% mutate(dataset = 1:n())
saveRDS(idkey, file = idkey_file)


# Filter to luteinizing phase, healthy controls and deidentify
lh_data <- 
	lh_data %>% 
  left_join(., idkey, by = "idno") %>% 
	# Filter to only control patients in luteal phase
  filter(pair == "l" & patient == "c") %>%
  # Remove identifiable info
  select(-idno) %>%
  select(-TIME) 


#-----------------------------------------------------------
# Wrap up data deidentification
#-----------------------------------------------------------
# Use assertr package to verify some dataset assumptions
lh_data %>% 
	group_by(dataset) %>% 
	mutate(sampling_period = MIN - lag(MIN)) %>% filter(OBS != 0) %>%
	verify(sampling_period == 10 ) #| is.na(sampling_period))

lh_data %>% group_by(dataset) %>% filter(!duplicated(dataset)) %>%
	verify(MIN == 10)

# Save deidentified dataset
saveRDS(lh_data, file = lh_deident_data_file)
   
# Clean up workspace
rm(idkey, labels_df, idkey_file, lh_data_file, lh_deident_data_file, lh_data)




#-----------------------------------------------------------
# Deidentify template arg (pulsespec) file
#-----------------------------------------------------------
# Read in argument file, split, and convert to dataframe
arg_dataset9 <- 
  readLines(lh_arg_file) %>% str_split(., " ") %>% 
  do.call(c, .) %>% as.matrix(byrow = TRUE) %>% t %>%
  as.data.frame %>% tbl_df 

# Label variables
colnames(arg_dataset9) <- 
	c("indata", "out_common", "out_pulse", "iterations", "prior_mass_mean",
		"prior_mass_var", "prior_width_mean", "prior_width_var",
		"prior_baseline_mean", "prior_baseline_var", "prior_halflife_mean",
		"prior_halflife_var", "prior_error_alpha", "prior_error_beta",
		"max_sd_mass", "max_sd_width", "prior_mean_num_pulses", "sv_mass_mean",
		"sv_width_mean", "sv_baseline_mean", "sv_halflife_mean", "sv_error_var",
		"sv_mass_sd", "sv_width_sd", "pv_baseline", "pv_halflife", "pv_mass",
		"pv_width", "pv_re_mass", "pv_re_width", "pv_pulselocations")

arg_dataset9 <- arg_dataset9 %>% mutate(dataset = 9) %>% 
	select(dataset, iterations:pv_pulselocations)

# Save deidentified arg file
saveRDS(arg_dataset9, file = lh_deident_arg_file)

rm(lh_arg_file, lh_deident_arg_file, arg_dataset9)

