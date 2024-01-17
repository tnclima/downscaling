# ensemble rcm changes -> kkz hull selection

library(data.table)
setDTthreads(4)
library(magrittr)
library(forcats)
library(foreach)

# library(ggplot2)
# library(ggh4x)
# library(ggrepel)
# library(plotly)
# library(flextable)

# code by Alex Cannon, PCIC, Canada
# source: https://pacificclimate.org/~tmurdock/ecdownscaling/CMIP5/code/scenario_selection/KKZ/
# date: 2023-09-26
source("R/functions/KKZ.R")

# data rcm --------------------------------------------------------------------

# copy from projects: elevdep
dat_season <- readRDS("data/future-trends/tn-crop-seasonal-mean.rds")

# TN average
dat_season2 <- dat_season[,
                          .(value = mean(value),
                            value_ref = mean(value_ref)),
                          .(season, period, variable, 
                            gcm, institute_rcm, experiment, 
                            ensemble, downscale_realisation, ind)]

# abs changes for tas and and rel for pr
dat_season2[variable == "tas", value_cast := value - value_ref]
dat_season2[variable == "pr", value_cast := (value - value_ref)/value_ref]


# # only most extreme period and rcp?
# dat_season3 <- dat_season2[period == "2071-2100" & experiment == "rcp85"]
# dat_season3 <- dat_season2[period == "2071-2100" & experiment == "rcp45"]

dat_season2_cast <- dat_season2 %>% 
  dcast(season + period + gcm + institute_rcm + experiment +
          ensemble + downscale_realisation + ind ~ variable, 
        value.var = "value_cast")

dat_rcm <- dat_season2_cast[ind == "mean"]



# different model per rcp --------------------------------------------------------------------


dat_sub <- dat_rcm[season %in% c("DJF", "JJA")]
dat_sub[, pr_sc := scale(pr), .(season, period, experiment)]
dat_sub[, tas_sc := scale(tas), .(season, period, experiment)]
dat_clust <- dat_sub %>% 
  dcast(gcm + institute_rcm + period + experiment ~ season, value.var = c("pr_sc", "tas_sc"))
l_split <- split(dat_clust, by = c("period", "experiment"))



dat_kkz <- foreach(
  i = 2:10,
  .final = rbindlist
) %do% {
  
  foreach(
    dat_clust = l_split,
    .final = rbindlist
  ) %do% {
    
    mat_clust <- as.matrix(dat_clust[, pr_sc_DJF:tas_sc_JJA])
    kkz_i <- subset.kkz(mat_clust, i)
    dat_out <- cbind(dat_clust,
                     n_points = i,
                     clust = as.vector(kkz_i$clusters),
                     centers = NA_integer_)
    dat_out[as.integer(rownames(kkz_i$cases)), centers := 1:i]
    dat_out
    
  }
  
}

dat_plot_free <- dat_sub %>% 
  merge(dat_kkz[, .(gcm, institute_rcm, period, experiment, n_points, clust, centers)],
        by = c("gcm", "institute_rcm", "period", "experiment"),
        allow.cartesian = T)


# spread covered ----------------------------------------------------------


# only period 2071-2100 models

dat_spread_free_71 <- dat_plot_free[period == "2071-2100",
                                    .(pr_spread = diff(range(pr[!is.na(centers)])) / diff(range(pr)),
                                      tas_spread = diff(range(tas[!is.na(centers)])) / diff(range(tas))),
                                    .(experiment, n_points, season)] %>% 
  melt(measure.vars = c("pr_spread", "tas_spread"))



dat_spread_free_71_mean <- dat_spread_free_71[, 
                                              .(value = mean(value)),
                                              .(experiment, n_points)] 


# table of changes
# 
# dat_table <- dat_plot_free[period == "2071-2100" & !is.na(centers) & 
#                              {(n_points == 5 & experiment == "rcp26") | 
#                                  (n_points == 6 & experiment != "rcp26") }] %>% 
#   dcast(experiment + centers ~ season, value.var = c("tas", "pr"))
# 
# dat_table[, pr_DJF := 100*pr_DJF]
# dat_table[, pr_JJA := 100*pr_JJA]



# summary output ----------------------------------------------------------

dat_selected_models <- dat_plot_free[
  period == "2071-2100" & !is.na(centers) & 
    {(n_points == 5 & experiment == "rcp26") | 
        (n_points == 6 & experiment != "rcp26") },
  .(gcm, institute_rcm, experiment, ensemble, downscale_realisation, centers)
] %>% unique


# table of changes
dat_table <- dat_sub %>% 
  merge(dat_selected_models, 
        by = c("gcm", "institute_rcm", "experiment", "ensemble", "downscale_realisation")) %>% 
  dcast(period + experiment + centers ~ season, value.var = c("tas", "pr"))

dat_table[, pr_DJF := 100*pr_DJF]
dat_table[, pr_JJA := 100*pr_JJA]


saveRDS(dat_selected_models, "data/sub-ensemble-kkz-01-selected-models.rds")
saveRDS(dat_table, "data/sub-ensemble-kkz-02-change-factors.rds")

