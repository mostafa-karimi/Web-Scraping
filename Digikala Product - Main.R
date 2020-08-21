rm(list=ls())
source("Digikala Product - Functions.R")
###----------------------------------- Number of Pages
page <- 1
###----------------------------------- URLs List
url0 <- c()
for(l in 1:page) {
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
  }
  dt <- rbind(dt, results)
}
t <- Sys.time() - t0
t
###----------------------------------- Data Analysis
View(dt)
p.price <- as.numeric(dt[, 3])