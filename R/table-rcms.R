# overview tables

# library(eurocordexr)
library(data.table)
library(magrittr)
library(fs)
library(stringr)

library(flextable)
# library(officer)
library(kableExtra)
options(knitr.kable.NA = '')

source("R/functions/inv_sub_reanalysis.R")



# eval --------------------------------------------------------------------


dat_inv <- inv_sub_reanalysis()
dat_inv <- dat_inv[variable == "tasmin"]

dat_inv[, rcm_short := shortnames_rcm[institute_rcm]]
dat_inv[, inst_grp := str_remove(institute_rcm, fixed(rcm_short)) %>% 
          str_remove("-$")]


dat_table <- dat_inv[, .(rcm_short, inst_grp)] 
setorder(dat_table, rcm_short)

setnames(dat_table, c("RCM model", "Institute/Group"))

dat_table %>%
  flextable() %>% 
  # set_header_labels(rcm_short = "RCM model", inst_grp = "Institute/Group") %>% 
  autofit() -> ft

# save_as_docx(ft, path = "tables/rcm-data.docx")
save_as_image(ft, path = "tables/rcm-data.png", webshot = "webshot2")


dat_table %>% 
  kbl("latex", booktabs = T, linesep = "") %>% 
  # collapse_rows(columns = 1, valign = "top", latex_hline = "major") %>% 
  # gsub("\\raggedright\\arraybackslash ", "", ., fixed = T) %>% 
  cat(file = "tables/rcm-data.tex")

