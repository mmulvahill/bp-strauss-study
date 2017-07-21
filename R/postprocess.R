#-------------------------------------------------------------------------------
# postprocess.R
#   Diagnostics and post-processing script 
#
# Postprocessing consists of:
#   1. Load args, concentraction data for diagnostics PDF
#   2. Remove burn-in
#   3. Thin
#   4. Create diagnostics PDF
#-------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(pulsatile)
tmpfits <- readRDS("~/Projects/Thesis/thesis-analysis/first_run_mdplyr_fits.Rds")
tmpfits$fits[[4]]$common %>%
  gather(key = key, value = value, num_pulses:sd_widths) %>%
  ggplot(., aes(x = iteration, y = value)) +
  geom_path(size = 0.10) +
  facet_wrap( ~ key, ncol = 2, nrow = 4, scales = "free") 

################# ORIGINAL CODE ###################################
ppOne <- function(dataset.nums, burnin, thin){# {{{
  #library(parallel)
  #library(pulser)
  #library(tidyr)

  
  # Set pulser options as constants
  data.store <- getOption("pulser.data.store")
  data.name  <- getOption("pulser.data.name")
  data.dir   <- getOption("pulser.data.dir")
  mcmc.dir   <- getOption("pulser.mcmc.dir")
  mcmc.name  <- getOption("pulser.mcmc.name")
  log.dir    <- getOption("pulser.log.dir")
  output.dir <- getOption("pulser.output.dir")
  rds.dir    <- getOption("pulser.rds.dir")
  fig.dir    <- getOption("pulser.figures")
  tab.dir    <- getOption("pulser.tables")

  data.nickname <- str_replace_all(data.name, "/", "-")
    
  
  # Data stuff -------------------------
  # Load argument dataset
  arg <- readRDS(paste0("./local-data/arg-", data.nickname, 
                        "-", mcmc.name, ".rds")) %>% tbl_df
  # Load simulation data for plotting
  if (data.name %in% c("realdata", "realhighpulsecount")) {
    sim <- readRDS(paste0(output.dir, "/Rds/data_pulse_", data.nickname, ".Rds")) 
  } else if (data.name %in% c("depr", "deprall", "deprlowmass", "depr-jo75", 
                              "depression/luteal")) {
    sim <- readRDS(paste0(output.dir, "/Rds/data_pulse_", data.nickname, ".Rds")) 
  } else {
    sim <- readRDS(paste0(output.dir, "/Rds/sims_pulse_", data.nickname, ".Rds")) 
  }

  # Load data (and remove burnin and combine) (he = high-error) 
  common <- postProcessDir(chain.type = "common", burnin = burnin,
                           num.datasets.to.combine = dataset.nums)
  pulse  <- postProcessDir(chain.type = "pulse",  burnin = burnin,
                           num.datasets.to.combine = dataset.nums)

  if (data.name %in% c("realdata", "realhighpulsecount")) {
    common <- sim %>% 
      select(dataset, orig.dataset) %>% 
        filter(!duplicated(.)) %>%
          left_join(common, ., by = "dataset")
    pulse  <- sim %>% 
      select(dataset, orig.dataset) %>% 
        filter(!duplicated(.)) %>%
          left_join(pulse, ., by = "dataset")
  }
  if (data.name %in% c("depr", "deprall", "deprlowmass", "depr-jo75")) {
    common <- sim %>% 
      select(dataset, idno) %>% 
        filter(!duplicated(.)) %>%
          left_join(common, ., by = "dataset")
    pulse  <- sim %>% 
      select(dataset, idno) %>% 
        filter(!duplicated(.)) %>%
          left_join(pulse, ., by = "dataset")
  }
  if (data.name %in% c("depression/luteal")) {
    common <- sim %>% 
      select(dataset, idno) %>% 
        filter(!duplicated(.)) %>%
          left_join(common, ., by = "dataset")
    pulse  <- sim %>% 
      select(dataset, idno) %>% 
        filter(!duplicated(.)) %>%
          left_join(pulse, ., by = "dataset")
  }

  
  # Thin chains
  #if (thin != 1) { warning("thinning not done in correct order") }
  keep.iters <- common$iteration %>% unique %>% .[(. %% thin) == 0]
  common %<>% filter(iteration %in% keep.iters) %>% mutate(mcmc.case = mcmc.name, sim.case = data.name)
  pulse  %<>% filter(iteration %in% keep.iters) %>% mutate(mcmc.case = mcmc.name, sim.case = data.name)


  # Save file processed files as single RDS for each chain type
  saveRDS(common, 
          file = paste(output.dir, "/", rds.dir, "/", 
                       "common_", data.nickname, "_", mcmc.name, ".Rds", sep = ""))
  saveRDS(pulse, 
          file = paste(output.dir, "/", rds.dir, "/", 
                       "pulse_", data.nickname, "_", mcmc.name, ".Rds", sep = ""))
  
  # Arg parameters table ---------------
  default.scipen <- getOption("scipen")
  options("scipen" = 99)

  argstable <- 
    arg %>% 
      mutate(date = date()) %>%
      select(data.name, mcmc.run.name, date, iterations:pv.pulselocations) %>%
      group_by(data.name, mcmc.run.name) %>%
      filter(!duplicated(data.name) & !duplicated(mcmc.run.name)) %>%
      t %>% 
      cbind(rownames(.), .) %>%
      rbind(., c("", "")) 

  argstable <- 
    cbind(argstable[1:17, ], argstable[18:34, ]) %>% tableGrob(rows = NULL)
  
  title <- textGrob("MCMC Arguments", gp = gpar(fontsize = 25))
  argstable %<>% 
    gtable_add_rows(., heights = grobHeight(title) + unit(1, "line"), pos = 0)

  if (mcmc.name %in% c("orderstat-testnat", 
                       "orderstat-testlog", 
                       "orderstat-testoldnat")) {
    caption <- textGrob("*Simulated data's pulse masses and widths were drawn from a t-distribution ", 
                        x = 0, hjust = 0,
                        gp = gpar(fontface = "italic"))
    argstable %<>% 
      gtable_add_rows(., heights = grobHeight(caption) + unit(0.5, "line"))
    argstable %<>% 
      gtable_add_grob(., list(title, caption), 
                      t = c(1, nrow(.)), l = c(1, 1), r = ncol(.))
  } else {
    argstable %<>% 
      gtable_add_grob(., title, t = 1, l = 1, r = ncol(.))
  }

  # create diagnostic plots ------------
  # clean up -Inf's
  common %<>% mutate_each(funs(ifelse(is.infinite(.) | is.nan(.), -300, .)))
  pulse %<>% mutate_each(funs(ifelse(is.infinite(.) | is.nan(.), -300, .)))

  max.time <- max(sim$time)

  temp <- 
    common %>%
      gather(key = key, value = value, num.pulses:sd.widths) %>%
      filter(dataset %in% dataset.nums) %>%
      group_by(dataset) %>%
      do( 
        # Trace plots
        trace.figs = 
        {
          trace.fig <- 
            ggplot(., aes(x = iteration, y = value)) +
              geom_path(size = 0.10) +
              facet_wrap( ~ key, ncol = 2, nrow = 4, scales = "free") +
              ggtitle(paste("Dataset", ifelse(is.null(.$orig.dataset), 
                                              unique(.$dataset),
                                              unique(.$orig.dataset))))
          #print(trace.fig)
        },
        # Posterior densities
        post.figs = 
        {
          post.fig <- 
            ggplot(., aes(x = value)) +
              geom_histogram(aes(y = ..density..), size = 0.15) +
              #geom_density(alpha=.2, fill="#FF6666") +
              facet_wrap( ~ key, ncol = 2, nrow = 4, scales = "free") +
              ggtitle(paste("Dataset", ifelse(is.null(.$orig.dataset), 
                                              unique(.$dataset),
                                              unique(.$orig.dataset))))
          #suppressMessages(print(post.fig))
        }
      )

  location.figs.lst <- 
    pulse %>%
      filter(dataset %in% dataset.nums) %>%
      group_by(dataset) %>%
      do(
        # Location histograms
        location.figs = 
        {
          location.fig <- 
            ggplot(., aes(x = location)) +
              geom_histogram(binwidth = 5) +
              theme(panel.grid.minor = element_line(colour="lightgrey", size=0.5)) + 
              theme(panel.grid.major = element_line(colour="lightgrey", size=0.5)) + 
              scale_x_continuous(breaks = seq(-50, max.time+50, 50),
                                 minor_breaks = seq(-50, max.time+50, 10),
                                 limits = c(-50, max.time+50)) 
        }
      )

  sim.figs.lst <-
    sim %>%
    filter(dataset %in% dataset.nums) %>%
    group_by(dataset) %>%
    do(
       sim.figs = 
       {
         sim.fig <-
           ggplot(., aes(x = time, y = concentration)) +
             geom_path() +
             geom_point() + 
             theme(panel.grid.minor = element_line(colour="lightgrey", size=0.5)) + 
             theme(panel.grid.major = element_line(colour="lightgrey", size=0.5)) + 
             scale_x_continuous(breaks = seq(-50, max.time+50, 50),
                                minor_breaks = seq(-50, max.time+50, 10),
                                limits = c(-50, max.time+50)) 
       }
    )

  temp <- bind_cols(temp, location.figs.lst[, 2], sim.figs.lst[, 2])  
  # Print arg table and diagnostic plots to pdf ------------
  pdf(file = paste0(output.dir, "/diagnostics/", 
                   "diag_", data.nickname, "_", mcmc.name, ".pdf"),
      paper = "USr",
      width = 0,
      height = 8, 
      onefile = TRUE)

  # Print arguments table
  grid.newpage()
  grid.draw(argstable)
  options("scipen" = default.scipen)
  rm(title, argstable, default.scipen)

  invisible(apply(temp, 1, function(x){
        grid.newpage() # Open a new page on grid device
        pushViewport(viewport(layout = grid.layout(5, 2, heights = unit(rep(0.2, 5), "npc"))))
        print(x$sim.figs, vp = viewport(layout.pos.row = 5, layout.pos.col = 1:2))
        print(x$location.figs, vp = viewport(layout.pos.row = 4, layout.pos.col = 1:2))
        print(x$trace.figs, vp = viewport(layout.pos.row = 1:3, layout.pos.col = 2:2))
        print(x$post.figs, vp = viewport(layout.pos.row = 1:3, layout.pos.col = 1)) 

        # old version
        #grid.newpage() # Open a new page on grid device
        #pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.375, 0.375, 0.25), "npc"))))
        #print(x$location.figs, vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2))
        #print(x$trace.figs, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 2:2))
        #print(x$post.figs, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1)) 





                   }))
  dev.off()

  # return nothing
  return(NULL)
}# }}}




#======================================= 
# Clean up constants
#======================================= 
#rm(data.store, data.name, data.dir, mcmc.dir, mcmc.name, log.dir,
#   output.dir, rds.dir, figures, tables)    
