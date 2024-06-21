#

# library(eurocordexr)
library(lubridate)
library(data.table)
setDTthreads(4)
library(magrittr)
library(ggplot2)
library(fs)
library(stringr)
library(purrr)
library(foreach)
library(patchwork)
library(forcats)
library(scico)


# aux data ----------------------------------------------------------------

dat_icell <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary-v1/eval-08-aux-icell-1km-011deg.rds")
dat_aux <- dat_icell[!is.na(orog), .(icell = icell_1km, x, y, orog)]


# crespi
dat_crespi_month <- readRDS("/home/climatedata/downscaling/validation-cv/rdata-summary-v3/eval-crespi-month.rds")
setnames(dat_crespi_month, "variable", "vv")
# dat_crespi_month[, period20 := ifelse(period20 == "odd", "period1", "period2")]

dat_crespi_avg <- dat_crespi_month[month %in% c(11:12, 1:5) & vv == "hn",
                                   .(value = mean(mean_value)),
                                   .(icell, month, vv)]


# data 81-00, 01-20 -------------------------------------------------------


path_in <- "/home/climatedata/downscaling/validation-cv/rdata-summary-v2/eval-bads/"

dat_files <- data.table(filename = dir_ls(path_in))
dat_files[, c("period2", "ba", "centers", "freq") := tstrsplit(path_file(filename) %>% path_ext_remove(), "_")]
dat_files

dat_files_allmod <- dat_files[freq == "month" & ba %in% c("mbcn", "qdm")]

dat_allmod_fs <- lapply(dat_files_allmod$filename, \(x){
  dat <- readRDS(x)
  dat[month %in% c(11:12, 1:5) & variable == "hn", ]
}) %>% rbindlist
setnames(dat_allmod_fs, "variable", "vv")
# dat_allmod_fs[, period20 := ifelse(period20 == "odd", "period1", "period2")]

dat_allmod_fs2 <- dat_allmod_fs[,
                                .(value = mean(mean_value)), 
                                .(icell, month, vv = variable, centers, ba)]

# data odd-even -----------------------------------------------------------

path_in <- "/home/climatedata/downscaling/validation-cv/rdata-summary-v3/eval-bads/"

dat_files <- data.table(filename = dir_ls(path_in))
dat_files[, c("ba", "centers", "freq") := tstrsplit(path_file(filename) %>% path_ext_remove(), "_")]
dat_files

dat_files_allmod <- dat_files[freq == "month" & ba %in% c("mbcn", "qdm")]

dat_allmod_oe <- lapply(dat_files_allmod$filename, \(x){
  dat <- readRDS(x)
  dat[month %in% c(11:12, 1:5) & variable == "hn",
      .(value = mean(mean_value)), 
      .(icell, month, vv = variable, centers, ba)]
}) %>% rbindlist
# setnames(dat_allmod, "variable", "vv")
# dat_allmod[, period20 := ifelse(period20 == "odd", "period1", "period2")]


# data plot ---------------------------------------------------------------

dat_crespi_avg_rep <- lapply(1:6, \(x) cbind(dat_crespi_avg, centers = x)) %>% rbindlist()

f_data <- function(dat){
  
  dat_plot1 <- dat %>% 
    rbind(cbind(dat_crespi_avg_rep, ba = "crespi")) %>% 
    merge(dat_aux)
  dat_plot1[, month_fct := mitmatmisc::month_fct(month, 10)]
  
  dat_plot2 <- dat_plot1 %>% 
    dcast(... ~ ba, value.var = "value")
  # abs
  # dat_plot2[, qdm_crespi := qdm - crespi]
  # dat_plot2[, mbcn_crespi := mbcn - crespi]
  # dat_plot2[, mbcn_qdm := mbcn - qdm]
  # rel
  dat_plot2[, qdm_crespi := (qdm - crespi)/crespi]
  dat_plot2[, mbcn_crespi := (mbcn - crespi)/crespi]
  dat_plot2[, mbcn_qdm := (mbcn - qdm)/qdm]
  
  dat_plot3 <- melt(dat_plot2, 
                    id.vars = c("icell", "month", "vv", "centers", "x", "y", "orog", "month_fct"),
                    measure.vars = c("qdm_crespi", "mbcn_crespi", "mbcn_qdm"),
                    variable.name = "ff_diff")
  
  dat_plot3
  
}

dat_plot3 <- rbind(
  f_data(dat_allmod_oe) %>% cbind(train_period = "odd-even"),  
  f_data(dat_allmod_fs2) %>% cbind(train_period = "first-second-half")  
)

# summary -----------------------------------------------------------------
# 
# dat_summ1 <- dat_plot1[, .(value = mean(value)), .(month, month_fct, variable, centers, ba)]
# dat_summ1 %>% 
#   ggplot(aes(month_fct, value, colour = ba))+
#   geom_line(aes(group = paste0(ba, centers)))+
#   facet_grid(variable ~ centers, scale = "free_y")+
#   theme_bw()+
#   xlab(NULL)+ylab("mean daily snowfall [mm SWE]")
# 
# ggsave("fig/validation-eval/multivar-summary/hn-month-raw_oddeven.png",
#        width = 16, height = 8)



dat_summ3 <- dat_plot3[, .(value = mean(value)), .(month, month_fct, centers, ff_diff, train_period)]
dat_summ3[ff_diff != "mbcn_qdm"] %>%
  ggplot(aes(month_fct, value, colour = ff_diff))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_line(aes(group = paste0(ff_diff, centers)))+
  scale_y_continuous(labels = scales::label_percent())+
  facet_grid(train_period ~ centers)+
  theme_bw()+
  xlab(NULL)+ylab("Difference")

ggsave("fig/validation-eval/multivar-summary/hn-month-diff_trainperiod.png",
       width = 16, height = 6)



dat_summ3b <- dat_plot3[, 
                        .(value = mean(value)), 
                        .(month, month_fct, centers, ff_diff, train_period,
                          elev_fct = cut(orog, seq(0, 3500, by = 500), dig.lab = 5))]
dat_summ3b[ff_diff != "mbcn_qdm"] %>%
  ggplot(aes(month_fct, value, colour = ff_diff, linetype = train_period))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_line(aes(group = paste0(ff_diff, centers, train_period)))+
  scale_y_continuous(labels = scales::label_percent())+
  facet_grid(fct_rev(elev_fct) ~ centers, scales = "free_y")+
  theme_bw()+
  xlab(NULL)+ylab("Difference")

ggsave("fig/validation-eval/multivar-summary/hn-month-diff_trainperiod_elev.png",
       width = 14, height = 9)

