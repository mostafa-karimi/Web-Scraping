rm(list=ls())
library(xml2)
library(stringr)
library(rlist)
###----------------------------------- Read Directroy
rh <- read_html("https://www.digikala.com/")
adc <- xml_attr(xml_find_all(rh, ".//div"), "class")
nlo <- xml_find_all(rh, ".//div")[which(adc == "c-navi-new-list__options-container")]
chm <- xml_children(nlo[1])
categories.main <- c()
categories.sub <- c()
categories <- list(NULL)
for(i in 1:length(chm)) {
  ctg <- xml_children(xml_children(chm[i])[1])
  link.m <- as.character(ctg)
  link.m <- unlist(str_split(unlist(str_split(unlist(str_split(link.m, "href"))[2], "class"))[1], ""))
  link.m <- paste(link.m[-c(1, 2, length(link.m), length(link.m) - 1)], collapse = "")
  categories.main[i] <- paste("https://www.digikala.com", link.m, sep = "")
  ctgop <- xml_children(xml_children(chm[i])[2])
  for(j in 1:length(ctgop)) {
    link.s <- as.character(xml_children(ctgop[j])[1])
    link.s <- unlist(str_split(unlist(str_split(unlist(str_split(link.s, "href"))[2], "class"))[1], '"'))[2]
    if(grepl("?", link.s, fixed = TRUE)) {
      link.s <- unlist(str_split(link.s, "\\?"))[1]
    }
    # link.s <- paste(link.s[-c(1, 2, length(link.s), length(link.s) - 1)], collapse = "")
    categories.sub[j] <- paste("https://www.digikala.com", link.s, sep = "")
  }
  categories[[i]] <- categories.sub
}
###----------------------------------- Function: get_item() 
get_item <- function(item) {
  url.s <- list.search(categories, .[grepl(item,.)])
  url.s <- unlist(url.s)
  url.s
}