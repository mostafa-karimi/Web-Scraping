rm(list=ls())
library(XML)
library(xml2)
library(stringr)
###-----------------------------------
url0 <- "http://www.r-datacollection.com/materials/html/fortunes.html"
# url0 <- "https://rmarkdown.rstudio.com/lesson-12.html"
###----------------------------------- xml2
rh <- read_html(url0)
###----------------------------------- XML
hp <- htmlParse(url0)
xmlRoot(hp)[[2]][[4]][[4]]
xmlSize(xmlRoot(hp)[[2]][[4]][[4]])
htmlTreeParse(url0, asTree = TRUE)
htmlTreeParse(url0)$children
getHTMLLinks(url0)
xmlSize(xmlChildren(hp)[[2]][[2]])
length(xmlChildren(hp)[[2]])
getNodeSet(hp, "//a")
xmlAttrs(xmlRoot(hp)[[2]][[6]][[1]])
