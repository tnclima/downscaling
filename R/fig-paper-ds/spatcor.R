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
library(scico)





# data --------------------------------------------------------------------

dat_ba <- map(
  dir_ls("/home/climatedata/downscaling/validation-cv-reanalysis/rdata-summary-v2/tnaa/spatcor/", glob = "*/ba*"),
  \(path_ba){
    map(dir_ls(path_ba),
        \(x){
          rcm <- x %>% path_file %>% path_ext_remove
          readRDS(x) %>% 
            cbind(institute_rcm = rcm)
        }) %>% rbindlist(fill = T) %>% cbind(bads = path_file(path_ba))
  }
) %>% rbindlist(fill = T)



dat_plot <- dat_ba[!bads %in% c("ba-qdm", "ba-mbcn")] %>% 
  .[, bads_multi := str_detect(bads, "mbcn")] %>% 
  .[, bads_ds := bads %>% forcats::fct_recode(
    "ds-pcalm" = "ba-qdm-ds-pcalm",
    "ds-pcalm" = "ba-mbcn-ds-pcalm",
    "ds-qdm" = "ba-qdm-ds-qdm",
    "ds-qdm" = "ba-mbcn-ds-qdm",
    "ds-qdm2" = "ba-qdm-ds-qdm2",
    "ds-qdm2" = "ba-mbcn-ds-qdm2",
    "ds-gam" = "ba-qdm-ds-gam",
    "ds-gam" = "ba-mbcn-ds-gam",
    "ds-lr" = "ba-qdm-ds-lr",
    "ds-lr" = "ba-mbcn-ds-lr",
    "bads" = "bads-qdm",
    "bads" = "bads-mbcn"   
  )]


dat_plot[, bads_ds := factor(bads_ds, levels = c(
  "bads", "ds-qdm", "ds-qdm2", "ds-pcalm", "ds-lr", "ds-gam"
))]

dat_plot[, bads_multi_fct := ifelse(bads_multi, "multivariate", "univariate")]




# plot --------------------------------------------------------------------


gg <- dat_plot[variable != "hn"] %>% 
  ggplot(aes(bads_ds, spatcor, fill = bads_multi_fct))+
  geom_boxplot()+
  facet_grid(variable ~ season, scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "grey40"),
        # legend.spacing = unit(0, "pt"),
        legend.position = c(0.65, 0.45))+
  xlab(NULL)+
  ylab("Average correlation across space wrt observations")

ggsave("fig/paper-ds/spatcor.png",
       gg, width = 10, height = 6)



# numbers -----------------------------------------------------------------


dat_plot[season == "DJF" & bads_multi == F,
         mean(spatcor),
         .(variable, bads_ds)] %>% 
  dcast(bads_ds ~ variable)


