# select random years (reanalysis)

set.seed(1234)
years <- 1989:2008
years_random <- sample(years)
l_years_train_period <- list(sort(years_random[1:10]), 
                             sort(years_random[11:20]))
saveRDS(l_years_train_period, "data/random-years-reanalysis.rds")
                             