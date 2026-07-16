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
library(forcats)




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

# dat_plot[, bads2 := fct_(bads, levels = c(
#   "bads-qdm", "bads-mbcn", "ba-qdm-ds-qdm2", "ba-mbcn-ds-qdm2", 
#   "ba-qdm-ds-pcalm", "ba-mbcn-ds-pcalm"
# ))]


# plot --------------------------------------------------------------------

dat_plot_spatcor <- dat_plot[!bads_ds %in% c("ds-qdm", "ds-lr", "ds-gam") &
                               variable != "hn"] 

dat_plot_spatcor[, bads2 := factor(bads, levels = c(
  "bads-qdm", "bads-mbcn", "ba-qdm-ds-qdm2", "ba-mbcn-ds-qdm2", 
  "ba-qdm-ds-pcalm", "ba-mbcn-ds-pcalm"
))]

gg <-
dat_plot_spatcor %>% 
  ggplot(aes(spatcor, fct_rev(bads2), fill = bads_multi_fct))+
  geom_boxplot()+
  scale_fill_brewer(NULL, palette = "Pastel1")+
  facet_grid(season ~ variable, scales = "free_x", space = "free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "grey40"),
        # legend.spacing = unit(0, "pt"),
        legend.position = "bottom")+
  ylab(NULL)+
  xlab("Average correlation across space wrt observations")

ggsave("fig/paper-ds-pdf/spatcor.pdf",
       gg, width = 10, height = 6)



# numbers -----------------------------------------------------------------


dat_plot[season == "DJF" & bads_multi == F,
         median(spatcor),
         .(variable, bads)] %>% 
  dcast(bads ~ variable)


