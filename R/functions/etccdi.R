# manual implementation of etccdi
# assuming daily input

tr <- \(tasmin) sum(tasmin > 20)
su <- \(tasmax) sum(tasmax > 25)
id <- \(tasmax) sum(tasmax < 0)
fd <- \(tasmin) sum(tasmin < 0)

rx1day <- \(pr) max(pr)
r20mm <- \(pr) sum(pr >= 20)
wd <- \(pr) sum(pr >= 1)
sdii <- \(pr) {
  wd <- pr >= 1
  sum(pr[wd]) / sum(wd)
}
cdd <- \(pr) {
  rle_pr <- rle(pr < 1)
  if(any(rle_pr$values)){
    max(rle_pr$lengths[rle_pr$values])  
  } else 0
}

