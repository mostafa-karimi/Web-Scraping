rm(list=ls())
library(xml2)
library(stringr)
###----------------------------------- Persian to English Function
pe <- "\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669\u06F0\u06F1\u06F2\u06F3\u06F4\u06F5\u06F6\u06F7\u06F8\u06F9"
en <- "01234567890123456789"
petoen <- function(s) as.numeric(chartr(pe, en, s))
###----------------------------------- Read HTML
url <- "https://www.digikala.com/product/dkp-676525/%DA%A9%D9%81%D8%B4-%D9%88%D8%B1%D8%B2%D8%B4%DB%8C-%D9%85%D8%B1%D8%AF%D8%A7%D9%86%D9%87-%D8%B3%D9%87-%D8%AE%D8%B7-%D9%85%D8%B4%DA%A9%DB%8C-%D9%85%D8%AF%D9%84-rzai#/tab-params"
rh <- read_html(url)
# xml_children(rh)
# xml_length(xml_children(rh)[2])
# xml_children(rh)[2]
###----------------------------------- Attribute "class"
adc <- xml_attr(xml_find_all(rh, ".//div"), "class")
ahc <- xml_attr(xml_find_all(rh, ".//h1"), "class")
asc <- xml_attr(xml_find_all(rh, ".//span"), "class")
asp <- xml_attr(xml_find_all(rh, ".//span"), "property")
# aud <- xml_attr(xml_find_all(rh, ".//ul"), "data-title")
###----------------------------------- Product title
pt <- xml_text(xml_find_all(rh, ".//h1")[which(ahc == "c-product__title")])
pt <- unlist(str_split(str_replace_all(pt, "  ", ""), "\n"))
product.title <- pt[2]
fake.badge <- pt[3]
###----------------------------------- Product Price
psp <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product__seller-price-raw js-price-value")])
psp <- unlist(str_split(str_replace_all(psp, " ", ""), "\n"))[2]
psp <- paste(unlist(str_split(psp, ""))[-which(unlist(str_split(psp, "")) == ",")], collapse = "")
product.price <- petoen(psp)
###----------------------------------- Product Rating
per <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product__engagement-rating")])
per <- unlist(str_split(str_replace_all(per, " ", ""), "\n"))
per1 <- per[2]
per2 <- paste(unlist(str_split(per[4], ""))[-c(1, length(unlist(str_split(per[4], ""))))], collapse = "")
rating.star <- petoen(per1)
rating.user <- petoen(per2)
###----------------------------------- Product Review and Q&A
pel <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product__engagement-link")])
pel <- unlist(str_split(str_replace_all(pel, " ", ""), "\n"))
pel1 <- pel[2]
pel2 <- pel[6]
user.review <- petoen(pel1)
user.qa <- petoen(pel2)
###----------------------------------- Product Recommendation
pg <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product__guaranteed")])
pg <- unlist(str_split(str_replace_all(pg, " ", ""), "\n"))
pg <- sort(unlist(str_split(pg, "")))
pg <- paste(sort(pg[-c(1:42)], T), collapse = "")
recommended <- petoen(pg)
###----------------------------------- Product Seller
psn <- xml_text(xml_find_all(rh, ".//span")[which(asc == "c-product__seller-name js-seller-name")])
psn <- unlist(str_split(str_replace_all(psn, "  ", ""), "\n"))[2]
psb <- xml_text(xml_find_all(rh, ".//span")[which(asc == "c-badge-seller c-badge-seller--prominent")])
psp <- unlist(str_split(str_replace_all(psn, "  ", ""), "\n"))[1]
pso <- xml_text(xml_find_all(rh, ".//span")[which(asc == "c-badge-seller c-badge-seller--official")])
pso <- unlist(str_split(str_replace_all(pso, "  ", ""), "\n"))[1]
pss <- xml_text(xml_find_all(rh, ".//span")[which(asc == "u-text-bold js-seller-final-score")])
pss <- unlist(str_split(str_replace_all(pss, "  ", ""), "\n"))[2]
psr <- xml_text(xml_find_all(rh, ".//span")[which(asc == "u-text-bold js-seller-rate")])
psr <- unlist(str_split(str_replace_all(psr, "  ", ""), "\n"))[2]
psr <- paste(unlist(str_split(psr, ""))[-length(unlist(str_split(psr, "")))], collapse = "")
product.seller.name <- psn
product.seller.badge <- psb[1]
product.seller.official <- pso
product.seller.performance <- petoen(pss)
product.seller.satisfaction <- petoen(psr)
###----------------------------------- Product Specifications
pbt <- xml_text(xml_find_all(rh, ".//span")[which(asc == "product-brand-title")])
pbt <- unlist(str_split(str_replace_all(pbt, "  ", ""), "\n"))
# ppa <- unlist(xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product__params js-is-expandable")]))
# unlist(str_split(str_replace_all(ppa, "  ", ""), " "))[1:4]
product.specification <- unlist(xml_text(xml_find_all(rh, ".//span")[which(asp == "name")]))
product.brand.title <- pbt
###----------------------------------- Results
df <- data.frame(cbind(c(product.title, 
                         fake.badge, 
                         product.price, 
                         rating.star, 
                         rating.user, 
                         user.review, 
                         user.qa, 
                         recommended, 
                         product.seller.name, 
                         product.seller.badge, 
                         product.seller.official,
                         product.seller.performance, 
                         product.seller.satisfaction, 
                         product.brand.title)
))
rownames(df) <- c("Product Title",
                  "Fake Badge",
                  "Product Price",
                  "Rating Star",
                  "Rating User",
                  "Review",
                  "Q&A",
                  "Recommended",
                  "Seller Name",
                  "Seller Badge",
                  "Seller Official",
                  "Seller Performance",
                  "Seller Satisfaction",
                  "Product Brand")
colnames(df) <- "value"
df0 <- data.frame(cbind(product.specification[-c(1, length(product.specification))]))
cname <- c()
for(j in 1:nrow(df0)) {
  cname[j] <- paste("Product Category", j)
}
rownames(df0) <- cname
colnames(df0) <- "value"
df1 <- rbind(df, df0)
View(df1)
