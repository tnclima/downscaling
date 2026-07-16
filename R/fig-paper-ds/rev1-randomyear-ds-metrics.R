#

library(data.table)
setDTthreads(4)
library(ggplot2)
library(magrittr)
library(fs)
library(stringr)
library(purrr)
library(patchwork)
library(ggh4x)
library(forcats)



# data --------------------------------------------------------------------

dat_raw <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/metrics/raw"),
  \(x){
    rcm <- x %>% path_file %>% path_ext_remove
    readRDS(x) %>% 
      cbind(institute_rcm = rcm)
  }
) %>% rbindlist()



dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/metrics/", glob = "*/ba*"),
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)

# random years
dat_ba_randyear <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v6/tnaa/metrics/", glob = "*/ba*"),
  \(path_ba){
    map(dir_ls(path_ba), 
        \(path_ba_randyear){
          map(dir_ls(path_ba_randyear), 
              \(x){
                rcm <- x %>% path_file %>% path_ext_remove
                readRDS(x) %>% 
                  cbind(institute_rcm = rcm)
              }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba), 
                                                   randyear = path_file(path_ba_randyear))
        }) |> rbindlist(fill = T)
  }
) %>% rbindlist(fill = T)

dat_ba_randyear <- dat_ba_randyear[bads %in% c(
  "ba-qdm-ds-pcalm", "ba-qdm-ds-qdm2", "bads-qdm"
)]

dat_plot <- dat_ba[bads %in% dat_ba_randyear$bads] |> 
  cbind(randyear = "0") |> 
  rbind(dat_ba_randyear, fill = T) %>% 
  melt(measure.vars = c("mae", "bias", "bias_rel", "corr"), variable.name = "metric")

dat_plot[, bads_multi := str_detect(bads, "mbcn")]
dat_plot[, bads_ds := bads %>% forcats::fct_recode(
  "ds-pcalm" = "ba-qdm-ds-pcalm",
  # "ds-pcalm" = "ba-mbcn-ds-pcalm",
  # "ds-qdm" = "ba-qdm-ds-qdm",
  # "ds-qdm" = "ba-mbcn-ds-qdm",
  "ds-qdm2" = "ba-qdm-ds-qdm2",
  # "ds-qdm2" = "ba-mbcn-ds-qdm2",
  # "ds-gam" = "ba-qdm-ds-gam",
  # "ds-gam" = "ba-mbcn-ds-gam",
  # "ds-lr" = "ba-qdm-ds-lr",
  # "ds-lr" = "ba-mbcn-ds-lr",
  "bads" = "bads-qdm"
  # "bads" = "bads-mbcn"   
)]

dat_plot[, bads_ds := factor(bads_ds, levels = c(
  "bads", "ds-qdm", "ds-qdm2", "ds-pcalm", "ds-lr", "ds-gam"
))]

ylabs <- setNames(
  c("MAE", "Bias", "Bias", "Correlation"),
  c("mae", "bias", "bias_rel", "corr")  
)

dat_plot[, metric2 := ylabs[metric]]
dat_plot[, metric2_fct := factor(metric2)]

dat_plot[, bads_multi_fct := ifelse(bads_multi, "multivariate", "univariate")]

# dat_plot[, randyear_fct := fct_relevel(randyear, c("initial", str_c("rep", 1:20)))]
# dat_plot[, randyear_fct := fct_relevel(randyear, c("initial", 1:20))]
# dat_plot[, randyear2 := ifelse(randyear == "initial", "initial sampling", "20 replications")]
dat_plot[, randyear_fct := fct_relevel(randyear, str_c(0:20))]
dat_plot[, randyear2 := ifelse(randyear == "0", "initial sampling", "20 replications")]
dat_plot[, randyear2_fct := fct_relevel(randyear2, "initial sampling")]

# fun ------------------------------------------------------------------

f_gg <- function(i_var, i_bads){
  
  if(i_var != "pr"){
    gg <-
      dat_plot[variable == i_var & metric != "bias_rel" & bads == i_bads] %>% 
      ggplot(aes(randyear_fct, value, fill = randyear2_fct))+
      geom_hline(data = data.frame(yy = 0, metric2_fct = "Bias"),
                 aes(yintercept = yy), linetype = "dashed")+
      geom_boxplot()+
      facet_grid(metric2_fct ~ season, scales = "free_y")+
      scale_fill_brewer(NULL, type = "qual")+
      # scale_y_facet(metric2_fct == "bias", labels = scales::label_percent())+
      theme_bw()+
      xlab(NULL)+
      ylab(NULL)+
      ggtitle(i_bads)
  } else {
    gg <-
      dat_plot[variable == i_var & metric != "bias" & bads == i_bads] %>% 
      ggplot(aes(randyear_fct, value, fill = randyear2_fct))+
      geom_hline(data = data.frame(yy = 0, metric2_fct = "Bias"),
                 aes(yintercept = yy), linetype = "dashed")+
      geom_boxplot()+
      facet_grid(metric2_fct ~ season, scales = "free_y")+
      scale_fill_brewer(NULL, type = "qual")+
      scale_y_facet(metric2_fct == "Bias", labels = scales::label_percent())+
      theme_bw()+
      xlab(NULL)+
      ylab(NULL)+
      ggtitle(i_bads)
  }
  
  
  ggsave(str_c("fig/paper-ds-rev1/randyear_ds-metrics_", i_var, "_", i_bads, ".png"),
         gg, width = 20, height = 10)
}


# loop
for(i_var in c("tasmax", "tasmin", "pr")){
  for(i_bads in unique(dat_plot$bads)){
    f_gg(i_var, i_bads)
  }
}


# one plot for paper ------------------------------------------------------

# only pr MAM


gg <-
  dat_plot[variable == "pr" & metric != "bias" & season == "MAM"] %>% 
  ggplot(aes(randyear_fct, value, fill = randyear2_fct))+
  geom_hline(data = data.frame(yy = 0, metric2_fct = "Bias"),
             aes(yintercept = yy), linetype = "dashed")+
  geom_boxplot()+
  facet_grid(metric2_fct ~ bads, scales = "free_y")+
  scale_fill_brewer(NULL, type = "qual")+
  scale_y_facet(metric2_fct == "Bias", labels = scales::label_percent())+
  theme_bw()+
  theme(legend.position = "bottom")+
  xlab("Replication")+
  ylab(NULL)


ggsave("fig/paper-ds-pdf/randyear-metrics_pr_MAM.pdf",
       gg, width = 12, height = 8)
