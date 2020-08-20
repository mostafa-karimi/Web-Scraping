rm(list=ls())
library(xml2)
library(stringr)
url <- "https://en.wikipedia.org/wiki/List_of_largest_companies_by_revenue"
rh <- read_html(url)
tb <- xml_find_all(rh, ".//table")
tb1 <- xml_children(tb[1])[2]
tb1r <- xml_find_all(tb1, ".//tr")
tb1h <- str_remove_all(xml_text(xml_find_all(tb1r[1], ".//th")), "\n")
tb1c1 <- c()
tb1cs <- c()
for(i in 2:51) {
  tb1c1[i-1] <- str_remove_all(xml_text(xml_find_all(tb1r[i], ".//th")), "\n")
  tb1cs <- rbind(tb1cs, str_remove_all(xml_text(xml_find_all(tb1r[i], ".//td")), "\n"))
}
tbl <- as.data.frame(cbind(tb1c1, tb1cs[, -7]))
colnames(tbl) <- tb1h[-8]
rownames(tbl) <- NULL
View(tbl)
