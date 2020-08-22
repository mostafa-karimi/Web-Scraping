rm(list=ls())
source("Digikala Product - Functions.R")
###----------------------------------- Web Pages (from:to)
page <- 1:5
###----------------------------------- URLs List
url0 <- c()
for(l in page) {
  url0[l] <- paste("https://www.digikala.com/search/category-men-sport-shoes-/", "?pageno=", l, "&sortby=4", sep = "")
}
###----------------------------------- Data Collection
t0 <- Sys.time()
dt <- c()
for(n in 1:length(url0)) {
  url <- get_url(url0[n])
  results <- c()
  for(k in 1:length(url)) {
    results <- rbind(results, t(read_page(url[k])))
    rownames(results) <- NULL
    print(paste("product", k, "of page", n))
  }
  dt <- rbind(dt, results)
  print(Sys.time() - t0)
}
tt <- Sys.time() - t0
tt
###----------------------------------- Data Analysis
View(dt)