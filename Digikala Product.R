rm(list=ls())
library(xml2)
library(stringr)
###----------------------------------- Persian to English 
pe <- "\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669\u06F0\u06F1\u06F2\u06F3\u06F4\u06F5\u06F6\u06F7\u06F8\u06F9"
en <- "01234567890123456789"
petoen <- function(s) as.numeric(chartr(pe, en, s))
###
url <- "https://www.digikala.com/product/dkp-676525/%DA%A9%D9%81%D8%B4-%D9%88%D8%B1%D8%B2%D8%B4%DB%8C-%D9%85%D8%B1%D8%AF%D8%A7%D9%86%D9%87-%D8%B3%D9%87-%D8%AE%D8%B7-%D9%85%D8%B4%DA%A9%DB%8C-%D9%85%D8%AF%D9%84-rzai#/tab-params"
rh <- read_html(url)
# xml_children(rh)
# xml_length(xml_children(rh)[2])
# xml_children(rh)[2]
###----------------------------------- Attribute "class"
ac <- xml_attr(xml_find_all(rh, ".//div"), "class")
###----------------------------------- Price
psp <- xml_text(xml_find_all(rh, ".//div")[which(ac == "c-product__seller-price-raw js-price-value")])
psp <- unlist(str_split(str_replace_all(psp, " ", ""), "\n"))[2]
psp <- paste(unlist(str_split(psp, ""))[-which(unlist(str_split(psp, "")) == ",")], collapse = "")
price <- petoen(psp)
###----------------------------------- Rating
per <- xml_text(xml_find_all(rh, ".//div")[which(ac == "c-product__engagement-rating")])
per <- unlist(str_split(str_replace_all(per, " ", ""), "\n"))
per1 <- per[2]
per2 <- paste(unlist(str_split(per[4], ""))[-c(1, length(unlist(str_split(per[4], ""))))], collapse = "")
rating.num <- petoen(per1)
rating.user <- petoen(per2)
###---------------------------------- Review and Q&A
pel <- xml_text(xml_find_all(rh, ".//div")[which(ac == "c-product__engagement-link")])
pel <- unlist(str_split(str_replace_all(pel, " ", ""), "\n"))
pel1 <- pel[2]
pel2 <- pel[6]
user.review <- petoen(pel1)
user.qa <- petoen(pel2)
###---------------------------------- Recommendation
pg <- xml_text(xml_find_all(rh, ".//div")[which(ac == "c-product__guaranteed")])
pg <- unlist(str_split(str_replace_all(pg, " ", ""), "\n"))
pg <- sort(unlist(str_split(pg, "")))
pg <- paste(sort(pg[-c(1:42)], T), collapse = "")
recommend <- petoen(pg)
###---------------------------------- Results
df <- data.frame(cbind(c("Price", "Rating", "Rating User", "Review", "Q&A", "Recommendation"), 
                       c(price, rating.num, rating.user, user.review, user.qa, recommend)
))
colnames(df) <- c("item", "value")
df
