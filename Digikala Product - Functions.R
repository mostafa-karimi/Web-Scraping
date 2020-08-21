rm(list=ls())
library(xml2)
library(stringr)
###----------------------------------- Function: get_url()
get_url <- function(url0) {
  rh0 <- read_html(url0)
  aac <- xml_attr(xml_find_all(rh0, ".//a"), "class")
  urlst <- xml_find_all(rh0, ".//a")[which(aac == "js-product-url")]
  url <- c()
  for(i in 1:length(urlst)) {
    url2 <- unlist(str_split(unlist(str_split(as.character(urlst)[i], " "))[3], '"'))[2]
    url[i] <- paste("https://www.digikala.com", url2, sep = "")
  }
  url
}
###----------------------------------- Function: read_page()
read_page <- function(url) {
  ###----------------------------------- Function: petoen()
  petoen <- function(p) {
    pe <- "\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669\u06F0\u06F1\u06F2\u06F3\u06F4\u06F5\u06F6\u06F7\u06F8\u06F9"
    en <- "01234567890123456789"
    e <- as.numeric(chartr(pe, en, p))
    e
  } 
  ###----------------------------------- Read HTML
  rh <- read_html(url)
  ###----------------------------------- Attribute "class"
  adc <- xml_attr(xml_find_all(rh, ".//div"), "class")
  ahc <- xml_attr(xml_find_all(rh, ".//h1"), "class")
  asc <- xml_attr(xml_find_all(rh, ".//span"), "class")
  asp <- xml_attr(xml_find_all(rh, ".//span"), "property")
  ###----------------------------------- Product title
  pt <- xml_text(xml_find_all(rh, ".//h1")[which(ahc == "c-product__title")])
  pt <- unlist(str_split(str_replace_all(pt, "  ", ""), "\n"))
  product.title <- pt[2]
  fake.badge <- ifelse(pt[3] != "", pt[3], NA)
  ###----------------------------------- Product Price
  psp <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product__seller-price-raw js-price-value")])
  psp <- unlist(str_split(str_replace_all(psp, " ", ""), "\n"))[2]
  psp <- paste(unlist(str_split(psp, ""))[-which(unlist(str_split(psp, "")) == ",")], collapse = "")
  product.price <- ifelse(is.double(petoen(psp)), petoen(psp), NA)
  ###----------------------------------- Product Rating
  per <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product__engagement-rating")])
  per <- unlist(str_split(str_replace_all(per, " ", ""), "\n"))
  per1 <- per[2]
  per2 <- paste(unlist(str_split(per[4], ""))[-c(1, length(unlist(str_split(per[4], ""))))], collapse = "")
  rating.star <- ifelse(is.double(petoen(per1)), petoen(per1), NA)
  rating.user <- ifelse(is.double(petoen(per2)), petoen(per2), NA)
  ###----------------------------------- Product Review and Q&A
  pel <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product__engagement-link")])
  pel <- unlist(str_split(str_replace_all(pel, " ", ""), "\n"))
  pel1 <- pel[2]
  pel2 <- pel[6]
  user.review <- ifelse(is.double(petoen(pel1)), petoen(pel1), NA)
  user.qa <- ifelse(is.double(petoen(pel2)), petoen(pel2), NA)
  ###----------------------------------- Product Recommendation
  pg <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product__guaranteed")])
  pg <- unlist(str_split(str_replace_all(pg, " ", ""), "\n"))
  pg <- sort(unlist(str_split(pg, "")))
  pg <- paste(sort(pg[-c(1:42)], T), collapse = "")
  recommended <- ifelse(is.double(petoen(pg)), petoen(pg), NA)
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
  product.seller.performance <- ifelse(is.double(petoen(pss)), petoen(pss), NA)
  product.seller.satisfaction <- ifelse(is.double(petoen(psr)), petoen(psr), NA)
  ###----------------------------------- Product Specifications
  pbt <- xml_text(xml_find_all(rh, ".//span")[which(asc == "product-brand-title")])
  pbt <- unlist(str_split(str_replace_all(pbt, "  ", ""), "\n"))
  # ppa <- unlist(xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product__params js-is-expandable")]))
  # unlist(str_split(str_replace_all(ppa, "  ", ""), " "))[1:4]
  product.specification <- unlist(xml_text(xml_find_all(rh, ".//span")[which(asp == "name")]))
  product.brand.title <- ifelse(is.character(pbt), pbt, NA)
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
  # df0 <- data.frame(cbind(product.specification[-c(1, length(product.specification))]))
  # cname <- c()
  # for(j in 1:nrow(df0)) {
  #   cname[j] <- paste("Product Category", j)
  # }
  # rownames(df0) <- cname
  # colnames(df0) <- "value"
  # df1 <- rbind(df, df0)
  # View(df)
  df
}
