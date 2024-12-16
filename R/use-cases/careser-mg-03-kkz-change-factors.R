# plot change factors with other models


library(data.table)
setDTthreads(4)
library(magrittr)
library(forcats)
library(foreach)

library(ggplot2)
library(ggh4x)
library(ggrepel)
library(patchwork)
library(scico)

library(flextable)
library(kableExtra)
options(knitr.kable.NA = '')


# copied from projects/downscaling
dat_kkz <- readRDS("data/sub-ensemble-kkz-01-selected-models.rds")
dat_kkz_cf <- readRDS("data/sub-ensemble-kkz-02-change-factors.rds")



# table models --------------------------------------------------------
# 
# dat_table_kkz <- copy(dat_kkz)
# 
# setcolorder(dat_table_kkz, c("experiment", "centers"))
# setorder(dat_table_kkz, experiment, centers)
# setnames(dat_table_kkz,
#          c("centers", "downscale_realisation"),
#          c("model_number", "ds"))
# 
# # dat_kkz %>% 
# #   as_grouped_data(groups = c("experiment")) %>%
# #   as_flextable(hide_grouplabel = T) %>% 
# #   bold(i = ~ !is.na(experiment), j = 1) %>%
# #   autofit()
# 
# dat_table_kkz %>% 
#   flextable() %>% 
#   merge_v(j = "experiment") %>% 
#   valign(valign = "top") %>% 
#   fix_border_issues() %>% 
#   autofit() %>%
#   save_as_image(path = "tables/kkz-selected-models.png", webshot = "webshot2")
# 
# 
# dat_table_kkz %>% 
#   kbl("latex", booktabs = T, linesep = "") %>% 
#   collapse_rows(columns = 1, valign = "top", latex_hline = "major") %>% 
#   gsub("\\raggedright\\arraybackslash ", "", ., fixed = T) %>% 
#   cat(file = "tables/kkz-selected-models.tex")
# 
# 


# extended table/fig changes -------------------------------------------------------

# dat_table <- dat_kkz_cf[period == "2071-2100", -c("period")]
dat_table <- dat_kkz_cf[period == "2011-2040" & experiment == "rcp45", -c("period")]

# simple table
# 
# ft <- dat_table %>% 
#   # as_grouped_data(groups = c("period", "experiment")) %>%
#   as_grouped_data(groups = c("experiment")) %>%
#   as_flextable(hide_grouplabel = T) %>% 
#   # bold(i = ~ !is.na(period), j = 1) %>%
#   bold(i = ~ !is.na(experiment), j = 1) %>%
#   set_header_labels(centers = "model_number") %>%
#   colformat_double(j = c("tas_DJF", "tas_JJA"), digits = 1) %>% 
#   colformat_double(j = c("pr_DJF", "pr_JJA"), digits = 1, suffix = "%")
# 
# ft %>% 
#   save_as_image("fig/climate-services/kkz_EN/kkz-change-factors-table-simple-2071-2100.png",
#                 webshot = "webshot2")



# extended table

tas_JJA_max <- max(round(dat_table$tas_JJA, 1))
tas_DJF_max <- max(round(dat_table$tas_DJF, 1))

# cols_JJA <- scales::

gg <- dat_table %>% 
  ggplot(aes(centers, tas_JJA, fill = tas_JJA))+
  geom_col()+
  facet_wrap(~experiment)+
  scale_fill_scico(palette = "lajolla")
cols_tas_JJA <- layer_data(gg)$fill

gg <- dat_table %>% 
  ggplot(aes(centers, tas_DJF, fill = tas_DJF))+
  geom_col()+
  facet_wrap(~experiment)+
  scale_fill_scico(palette = "lajolla")
cols_tas_DJF <- layer_data(gg)$fill

dat_gg_pr_DJF <- dat_table[, list(list(
  ggplot(.SD)+
    geom_rect(aes(xmin = 0, xmax = pr_DJF, ymin = -1, ymax = 1, fill = pr_DJF))+
    xlim(c(-60,60))+
    scale_fill_scico(palette = "roma", limits = c(-60, 60))+
    theme_void()+
    theme(legend.position = "none")
)), by = .I]

dat_gg_pr_JJA <- dat_table[, list(list(
  ggplot(.SD)+
    geom_rect(aes(xmin = 0, xmax = pr_JJA, ymin = -1, ymax = 1, fill = pr_JJA))+
    xlim(c(-60,60))+
    scale_fill_scico(palette = "roma", limits = c(-60, 60))+
    theme_void()+
    theme(legend.position = "none")
)), by = .I]

# 
# ft %>% 
#   append_chunks(j = "tas_DJF",
#                 value = as_paragraph(
#                   minibar(value = round(tas_DJF, 1), max = tas_DJF_max)
#                 )) %>% 
#   append_chunks(j = "tas_JJA",
#                 value = as_paragraph(
#                   minibar(value = round(tas_JJA, 1), max = tas_JJA_max, barcol = cols_JJA)
#                 ))


ft_complex <- dat_table %>% 
  flextable() %>% 
  set_header_labels(centers = "model_number") %>%
  colformat_double(j = c("tas_DJF", "tas_JJA"), digits = 1) %>% 
  colformat_double(j = c("pr_DJF", "pr_JJA"), digits = 1, suffix = "%") %>% 
  
  align(align = "right", part = "all") %>% 
  merge_v(j = "experiment") %>% 
  valign(valign = "top") %>% 
  fix_border_issues() %>% 
  
  append_chunks(j = "tas_DJF",
                value = as_paragraph(
                  minibar(value = round(tas_DJF, 1), max = tas_DJF_max, barcol = cols_tas_DJF)
                )) %>% 
  append_chunks(j = "tas_JJA",
                value = as_paragraph(
                  minibar(value = round(tas_JJA, 1), max = tas_JJA_max, barcol = cols_tas_JJA)
                )) %>% 
  
  append_chunks(j = "pr_DJF",
                value = as_paragraph(gg_chunk(value = dat_gg_pr_DJF$V1))) %>% 
  append_chunks(j = "pr_JJA",
                value = as_paragraph(gg_chunk(value = dat_gg_pr_JJA$V1))) %>% 
  autofit()


ft_complex %>% 
  save_as_image("fig/use-cases/careser-mg/kkz-change-factors-rcp45-2011-2040.png",
                webshot = "webshot2")
