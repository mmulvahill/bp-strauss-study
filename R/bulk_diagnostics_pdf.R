#-------------------------------------------------------------------------------
# bulk_diagnostics_pdf.R
#   Function for creating pdf of diagnostics for all fits in a tidy dataset
#-------------------------------------------------------------------------------

########################################
# print_diag_PDF()
#   TODO: list and check for needed columns in tidy_pulse_df
########################################
print_diag_PDF <- function(.data_list, fits, .spec, file, data_name, prior_name, dataset_num) {

  require(stringr)
  require(ggplot2)
  require(grid)
  require(gridExtra)
  require(gtable)

  # Name identifiers for use in filename
#   data_nickname <- unique(tidy_pulse_df$case)
#   mcmc_name     <- unique(tidy_pulse_df$prior_scenario)
  stopifnot(length(data_name) == 1, length(prior_name) == 1) 

  # Arg parameters table ---------------
  default.scipen <- getOption("scipen")
  options("scipen" = 99)

  this_args <- 
    list(
         data_frame(arg_type = "location_prior_type", 
                    values = .spec$strauss_location_prior), 
         data_frame(arg_type = "priors", 
                    arg = list(.spec$priors %>% map(stack) %>% do.call(rbind, .) %>%
                               rownames_to_column)) %>% 
           unnest %>% mutate(ind = as.character(ind)),
         data_frame(arg_type = "starting_values", 
                    arg = list(.spec$starting_values %>% map(stack) %>%
                               do.call(rbind, .) %>% rownames_to_column)) %>% 
           unnest %>% mutate(ind = as.character(ind)),
         data_frame(arg_type = "proposal_variances", 
                    arg = list(.spec$proposal_variances %>% stack)) %>% 
           unnest %>% mutate(ind = as.character(ind)) %>% rename(rowname = ind)
         ) %>%
    reduce(full_join) %>%
    mutate(rowname = str_replace_all(rowname, "\\.[0-9]", "")) %>%
    rename(hyperparameter = ind,
           prior = rowname)



  location_type <- this_args %>% filter(arg_type == "location_prior_type") %>%
    .$values %>% as.character
  location_type <- switch(location_type, "0" = "Order Statistic", "1" = "Strauss")
  title <- paste("MCMC Prior parameters; With", location_type, "prior",
                 "on location") %>% textGrob(.,  gp = gpar(fontsize = 12))

  priortable <- 
    this_args %>% 
    filter(arg_type == "priors") %>% 
    select(-arg_type) %>%
    tableGrob(rows = NULL) %>%
    gtable_add_rows(., heights = grobHeight(title) + unit(1, "line"), pos = 0) %>%
    gtable_add_grob(., title, t = 1, l = 1, r = ncol(.))

  title <- textGrob("Starting values", gp = gpar(fontsize = 12))
  svtable <- 
    this_args %>% 
    filter(arg_type == "starting_values") %>% 
    select(-arg_type) %>%
    tableGrob(rows = NULL) %>%
    gtable_add_rows(., heights = grobHeight(title) + unit(1, "line"), pos = 0) %>%
    gtable_add_grob(., title, t = 1, l = 1, r = ncol(.))

  title <- textGrob("Proposal SD", gp = gpar(fontsize = 12))
  proposaltable <- 
    this_args %>% 
    filter(arg_type == "proposal_variances") %>% 
    select(-arg_type, -hyperparameter) %>%
    tableGrob(rows = NULL) %>%
    gtable_add_rows(., heights = grobHeight(title) + unit(1, "line"), pos = 0) %>%
    gtable_add_grob(., title, t = 1, l = 1, r = ncol(.))


  # create diagnostic plots ------------
  plot_df <- 
    tibble(dataset_num = dataset_num, fits = fits, datasets = .data_list) %>% 
    mutate(trace_plot      = map(fits, bp_trace)) %>%
    mutate(poster_plot     = map(fits, bp_posteriors, type = "histogram")) %>%
    mutate(poster_location = map(fits, bp_location_posterior)) %>%
    mutate(max_time        = map(datasets, function(x) max(x$time)) %>%
                             unlist) 
  plot_df <-
    plot_df %>%
    mutate(time_series     = 
           map(datasets, 
               ~ ggplot(.x) +
                 aes_string(x = "time", y = "concentration") + 
                 geom_path() +
                 theme(panel.grid.minor = element_line(colour = "lightgrey",
                                                       size = 0.5)) + 
                 theme(panel.grid.major = element_line(colour = "lightgrey",
                                                       size = 0.5)) + 
                 scale_x_continuous(breaks = seq(-50, unique(plot_df$max_time) + 50, 50),
                                    minor_breaks = seq(-50, unique(plot_df$max_time) + 50, 10),
                                    limits = c(-50, unique(plot_df$max_time) + 50))))
  

  # Print arg table and diagnostic plots to pdf ------------
  pdf(file    = file,
      paper   = "USr",
      width   = 0,
      height  = 8,
      onefile = TRUE)

  # Print arguments table
  grid.newpage()
  grid.draw(priortable)
  grid.newpage()
  grid.draw(svtable)
  grid.newpage()
  grid.draw(proposaltable)
  options("scipen" = default.scipen)
  rm(title, default.scipen)

  for (i in 1:nrow(plot_df)) {
    grid.newpage() # Open a new page on grid device
    pushViewport(viewport(layout = 
                          grid.layout(6, 2, heights = unit(c(0.75, rep(5, 5)), "null"))))
    print(plot_df$time_series[[i]], vp = viewport(layout.pos.row = 6, layout.pos.col = 1:2))
    print(plot_df$poster_location[[i]], vp = viewport(layout.pos.row = 5, layout.pos.col = 1:2))
    print(plot_df$trace_plot[[i]], vp = viewport(layout.pos.row = 2:4, layout.pos.col = 2:2))
    print(plot_df$poster_plot[[i]], vp = viewport(layout.pos.row = 2:4, layout.pos.col = 1)) 
    grid.text(paste("Dataset", plot_df$dataset_num[[i]]), 
              vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))

  }

  dev.off()

  # return nothing
  return(NULL)

}

