# select random years (reanalysis)

n_rep <- 20

set.seed(1234)
years <- 1989:2008

l_years_train_period <- lapply(1:n_rep, \(x){
  
  years1 <- sort(sample(years, 10))
  years2 <- years[!years %in% years1]
  list(years1, years2)
  
}) 

saveRDS(l_years_train_period, "data/random-years-reanalysis-extended.rds")

# split each in 2
l_years_train_period2 <- lapply(l_years_train_period, \(x){
  
  i1 <- sample(1:10, 5)
  i2 <- sample(1:10, 5)
  list(
    sort(c(x[[1]][i1], x[[2]][i2])),
    sort(c(x[[1]][-i1], x[[2]][-i2]))
  )
  
})

saveRDS(l_years_train_period2, "data/random-years-reanalysis2-extended.rds")
                             