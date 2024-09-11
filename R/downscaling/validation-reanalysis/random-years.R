# select random years (reanalysis)

set.seed(1234)
years <- 1989:2008
years_random <- sample(years)
l_years_train_period <- list(sort(years_random[1:10]), 
                             sort(years_random[11:20]))
saveRDS(l_years_train_period, "data/random-years-reanalysis.rds")

# split each in 2
i1 <- sample(1:10, 5)
i2 <- sample(1:10, 5)
l_years_train_period2 <- list(
  sort(c(l_years_train_period[[1]][i1], l_years_train_period[[2]][i2])),
  sort(c(l_years_train_period[[1]][-i1], l_years_train_period[[2]][-i2]))
)
saveRDS(l_years_train_period2, "data/random-years-reanalysis2.rds")
                             