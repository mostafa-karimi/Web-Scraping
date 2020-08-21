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
# url <- "https://www.digikala.com/product/dkp-1419760/%DA%A9%D9%81%D8%B4-%D9%85%D8%AE%D8%B5%D9%88%D8%B5-%D9%BE%DB%8C%D8%A7%D8%AF%D9%87-%D8%B1%D9%88%DB%8C-%D9%85%D8%B1%D8%AF%D8%A7%D9%86%D9%87-%D9%85%D8%AF%D9%84-dharma"
# url <- "https://www.digikala.com/product/dkp-676487/%DA%A9%D9%81%D8%B4-%D9%88%D8%B1%D8%B2%D8%B4%DB%8C-%D9%85%D8%B1%D8%AF%D8%A7%D9%86%D9%87-%D9%85%D8%AF%D9%84-rzai"
# url <- "https://www.digikala.com/product/dkp-1696173/%DA%A9%D9%81%D8%B4-%D9%85%D8%AE%D8%B5%D9%88%D8%B5-%D9%BE%DB%8C%D8%A7%D8%AF%D9%87-%D8%B1%D9%88%DB%8C-%D9%85%D8%B1%D8%AF%D8%A7%D9%86%D9%87-%D9%85%D8%AF%D9%84-adi-super-%D8%B1%D9%86%DA%AF-%D8%B7%D9%88%D8%B3%DB%8C"
# url <- "https://www.digikala.com/product/dkp-2437788/%DA%A9%D9%81%D8%B4-%D8%B1%D8%A7%D8%AD%D8%AA%DB%8C-%D9%85%D8%B1%D8%AF%D8%A7%D9%86%D9%87-%D9%85%D8%AF%D9%84-part-so"
# url <- "https://www.digikala.com/product/dkp-3126378/%DA%A9%D9%81%D8%B4-%D9%85%D8%AE%D8%B5%D9%88%D8%B5-%D9%BE%DB%8C%D8%A7%D9%87-%D8%B1%D9%88%DB%8C-%D8%B2%D9%86%D8%A7%D9%86%D9%87-%D9%85%D8%AF%D9%84-bk33"
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
  if(!is.na(psp) && !is.null(psp)) {
    if(sum(unlist(str_split(psp, "")) == ",") > 0) {
      psp <- paste(unlist(str_split(psp, ""))[-which(unlist(str_split(psp, "")) == ",")], collapse = "")
    }
  }
  pspp0 <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product__seller-price-info")])
  pspp <- unlist(str_split(str_replace_all(pspp0, " ", ""), "\n"))[2]
  if(!is.na(pspp) && !is.null(pspp)) {
    if(sum(unlist(str_split(pspp, "")) == ",") > 0) {
      pspp <- paste(unlist(str_split(pspp, ""))[-which(unlist(str_split(pspp, "")) == ",")], collapse = "")
    }
  }
  pspd <- unlist(str_split(str_replace_all(pspp0, " ", ""), "\n"))[4]
  pspd <- paste(unlist(str_split(pspd, ""))[-length(unlist(str_split(pspd, "")))], collapse = "")
  product.price <- ifelse(is.double(petoen(psp)), petoen(psp), NA)
  product.price.previous <- ifelse(is.double(petoen(pspp)), petoen(pspp), NA)
  product.price.discount <- ifelse(is.double(petoen(pspd)), petoen(pspd), NA)
  ###----------------------------------- Product Rating
  per <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product__engagement-rating")])
  per <- unlist(str_split(str_replace_all(per, " ", ""), "\n"))
  per1 <- per[2]
  per2 <- paste(unlist(str_split(per[4], ""))[-c(1, length(unlist(str_split(per[4], ""))))], collapse = "")
  if(!is.na(per2) && !is.null(per2)) {
    if(sum(unlist(str_split(per2, "")) == ",") > 0) {
      per2 <- paste(unlist(str_split(per2, ""))[-which(unlist(str_split(per2, "")) == ",")], collapse = "")
    }
  }
  rating.star <- ifelse(is.double(petoen(per1)), petoen(per1), NA)
  rating.star.user <- ifelse(is.double(petoen(per2)), petoen(per2), NA)
  ###----------------------------------- Product Review and Q&A
  pel <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product__engagement-link")])
  pel <- unlist(str_split(str_replace_all(pel, " ", ""), "\n"))
  pel1 <- pel[2]
  if(!is.na(pel1) && !is.null(pel1)) {
    if(sum(unlist(str_split(pel1, "")) == ",") > 0) {
      pel1 <- paste(unlist(str_split(pel1, ""))[-which(unlist(str_split(pel1, "")) == ",")], collapse = "")
    }
  }
  pel2 <- pel[6]
  if(!is.na(pel2) && !is.null(pel2)) {
    if(sum(unlist(str_split(pel2, "")) == ",") > 0 & !is.na(pel2)) {
      pel2 <- paste(unlist(str_split(pel2, ""))[-which(unlist(str_split(pel2, "")) == ",")], collapse = "")
    }
  }
  user.review <- ifelse(is.double(petoen(pel1)), petoen(pel1), NA)
  user.qa <- ifelse(is.double(petoen(pel2)), petoen(pel2), NA)
  ###----------------------------------- Product Recommendation
  pg <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product__guaranteed")])
  pg <- unlist(str_split(unlist(str_split(pg, "\n"))[2], " "))[19]
  if(!is.na(pg) && !is.null(pg)) {
    if(sum(unlist(str_split(pg, "")) == ",") > 0) {
      pg <- paste(unlist(str_split(pg, ""))[-which(unlist(str_split(pg, "")) == ",")], collapse = "")
    }
  }
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
  psf <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product-feedback")])
  psf1 <- unlist(str_split(str_replace_all(psf, "  ", ""), "\n"))[2]
  psf1 <- paste(unlist(str_split(psf1, ""))[-length(unlist(str_split(psf1, "")))], collapse = "")
  psf2 <- unlist(str_split(str_replace_all(psf, "  ", ""), "\n"))[7]
  psf2 <- paste(unlist(str_split(psf2, ""))[-length(unlist(str_split(psf2, "")))], collapse = "")
  psf3 <- unlist(str_split(str_replace_all(psf, "  ", ""), "\n"))[12]
  psf3 <- paste(unlist(str_split(psf3, ""))[-length(unlist(str_split(psf3, "")))], collapse = "")
  psr <- xml_text(xml_find_all(rh, ".//span")[which(asc == "u-text-bold js-seller-rate")])
  psr <- unlist(str_split(str_replace_all(psr, "  ", ""), "\n"))[2]
  psr <- paste(unlist(str_split(psr, ""))[-length(unlist(str_split(psr, "")))], collapse = "")
  psrc <- xml_text(xml_find_all(rh, ".//span")[which(asc == "js-seller-rate-count")])
  if(!is.na(psrc) && !is.null(psrc)) {
    if(sum(unlist(str_split(psrc, "")) == ",") > 0) {
      psrc <- paste(unlist(str_split(psrc, ""))[-which(unlist(str_split(psrc, "")) == ",")], collapse = "")
    }
  }
  psv1 <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product-v-feedback__percent js-seller-value-totally_satisfied")])
  psv1 <- unlist(str_split(str_replace_all(psv1, "  ", ""), "\n"))[2]
  psv1 <- paste(unlist(str_split(psv1, ""))[-length(unlist(str_split(psv1, "")))], collapse = "")
  psv2 <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product-v-feedback__percent js-seller-value-satisfied")])
  psv2 <- unlist(str_split(str_replace_all(psv2, "  ", ""), "\n"))[2]
  psv2 <- paste(unlist(str_split(psv2, ""))[-length(unlist(str_split(psv2, "")))], collapse = "")
  psv3 <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product-v-feedback__percent js-seller-value-neutral")])
  psv3 <- unlist(str_split(str_replace_all(psv3, "  ", ""), "\n"))[2]
  psv3 <- paste(unlist(str_split(psv3, ""))[-length(unlist(str_split(psv3, "")))], collapse = "")
  psv4 <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product-v-feedback__percent js-seller-value-dissatisfied")])
  psv4 <- unlist(str_split(str_replace_all(psv4, "  ", ""), "\n"))[2]
  psv4 <- paste(unlist(str_split(psv4, ""))[-length(unlist(str_split(psv4, "")))], collapse = "")
  psv5 <- xml_text(xml_find_all(rh, ".//div")[which(adc == "c-product-v-feedback__percent js-seller-value-totally_dissatisfied")])
  psv5 <- unlist(str_split(str_replace_all(psv5, "  ", ""), "\n"))[2]
  psv5 <- paste(unlist(str_split(psv5, ""))[-length(unlist(str_split(psv5, "")))], collapse = "")
  seller.name <- psn
  seller.badge <- psb[1]
  seller.official <- pso
  seller.performance <- ifelse(is.double(petoen(pss)), petoen(pss), NA)
  seller.ship.ontime <- ifelse(is.double(petoen(psf1)), petoen(psf1), NA)
  seller.no.cancellation <- ifelse(is.double(petoen(psf2)), petoen(psf2), NA)
  seller.no.return <- ifelse(is.double(petoen(psf3)), petoen(psf3), NA)
  seller.satisfaction.product <- ifelse(is.double(petoen(psr)), petoen(psr), NA)
  seller.satisfaction.product.user <- ifelse(is.double(petoen(psrc)), petoen(psrc), NA)
  seller.satisfaction.product.totally.satisfied <- ifelse(is.double(petoen(psv1)), petoen(psv1), NA)
  seller.satisfaction.product.satisfied <- ifelse(is.double(petoen(psv2)), petoen(psv2), NA)
  seller.satisfaction.product.neutral <- ifelse(is.double(petoen(psv3)), petoen(psv3), NA)
  seller.satisfaction.product.dissatisfied <- ifelse(is.double(petoen(psv4)), petoen(psv4), NA)
  seller.satisfaction.product.totally.dissatisfied <- ifelse(is.double(petoen(psv5)), petoen(psv5), NA)
  ###----------------------------------- Seller Page
  urlseller <- as.character(xml_find_all(rh, ".//div")[which(adc == "c-product-info-box__row u-text-bold")])
  urlseller <- unlist(str_split(unlist(str_split(urlseller, "="))[5], ""))[-1]
  urlseller <- paste(urlseller[-c((length(urlseller)-6):length(urlseller))], collapse = "")
  urlseller <- paste("https://www.digikala.com", urlseller, sep = "")
  rhseller <- read_html(urlseller)
  adcseller <- xml_attr(xml_find_all(rhseller, ".//div"), "class")
  spsu <- xml_text(xml_find_all(rhseller, ".//div")[which(adcseller == "c-profile-box-seller__details")])
  spsu <- unlist(str_split(str_replace_all(spsu, "  ", ""), "\n"))[c(1, 5, 9, 13, 17, 21, 22)]
  spsu1 <- paste(unlist(str_split(spsu[1], ""))[-length(unlist(str_split(spsu[1], "")))], collapse = "")
  spsu2 <- paste(unlist(str_split(spsu[2], ""))[-length(unlist(str_split(spsu[2], "")))], collapse = "")
  spsu3 <- paste(unlist(str_split(spsu[3], ""))[-length(unlist(str_split(spsu[3], "")))], collapse = "")
  spsu4 <- paste(unlist(str_split(spsu[4], ""))[-length(unlist(str_split(spsu[4], "")))], collapse = "")
  spsu5 <- paste(unlist(str_split(spsu[5], ""))[-length(unlist(str_split(spsu[5], "")))], collapse = "")
  spsu6 <- paste(unlist(str_split(spsu[6], ""))[-length(unlist(str_split(spsu[6], "")))], collapse = "")
  spsu7 <- paste(unlist(str_split(unlist(str_split(spsu[7], " "))[1], ""))[-1], collapse = "")
  if(!is.na(spsu7) && !is.null(spsu7)) {
    if(sum(unlist(str_split(spsu7, "")) == ",") > 0) {
      spsu7 <- paste(unlist(str_split(spsu7, ""))[-which(unlist(str_split(spsu7, "")) == ",")], collapse = "")
    }
  }
  seller.satisfaction.overall <- ifelse(is.double(petoen(spsu1)), petoen(spsu1), NA)
  seller.satisfaction.overall.user <- ifelse(is.double(petoen(spsu7)), petoen(spsu7), NA)
  seller.satisfaction.overall.totally.satisfied <- ifelse(is.double(petoen(spsu2)), petoen(spsu2), NA)
  seller.satisfaction.overall.satisfied <- ifelse(is.double(petoen(spsu3)), petoen(spsu3), NA)
  seller.satisfaction.overall.neutral <- ifelse(is.double(petoen(spsu4)), petoen(spsu4), NA)
  seller.satisfaction.overall.dissatisfied <- ifelse(is.double(petoen(spsu5)), petoen(spsu5), NA)
  seller.satisfaction.overall.totally.dissatisfied <- ifelse(is.double(petoen(spsu6)), petoen(spsu6), NA)
  ###----------------------------------- Product Specifications
  pbt <- xml_text(xml_find_all(rh, ".//span")[which(asc == "product-brand-title")])
  pbt <- unlist(str_split(str_replace_all(pbt, "  ", ""), "\n"))
  product.specification <- unlist(xml_text(xml_find_all(rh, ".//span")[which(asp == "name")]))
  product.brand.title <- ifelse(is.character(pbt), pbt, NA)
  ###----------------------------------- Results
  df <- data.frame(cbind(c(product.title, 
                           fake.badge, 
                           product.price, 
                           product.price.previous,
                           product.price.discount,
                           rating.star, 
                           rating.star.user, 
                           user.review, 
                           user.qa, 
                           recommended, 
                           seller.name, 
                           seller.badge, 
                           seller.official,
                           seller.performance, 
                           seller.ship.ontime, 
                           seller.no.cancellation,
                           seller.no.return,
                           seller.satisfaction.product, 
                           seller.satisfaction.product.user, 
                           seller.satisfaction.product.totally.satisfied,
                           seller.satisfaction.product.satisfied,
                           seller.satisfaction.product.neutral,
                           seller.satisfaction.product.dissatisfied,
                           seller.satisfaction.product.totally.dissatisfied, 
                           seller.satisfaction.overall,
                           seller.satisfaction.overall.user,
                           seller.satisfaction.overall.totally.satisfied,
                           seller.satisfaction.overall.satisfied,
                           seller.satisfaction.overall.neutral,
                           seller.satisfaction.overall.dissatisfied,
                           seller.satisfaction.overall.totally.dissatisfied,
                           product.brand.title)
  ))
  rownames(df) <- c("Product Title",
                    "Fake Badge",
                    "Product Price", 
                    "Product Price Previous", 
                    "Product Price Discount (%)",
                    "Rating Star (out of 5)",
                    "Rating Star - User",
                    "Review",
                    "Q&A",
                    "Recommended",
                    "Seller Name",
                    "Seller Badge",
                    "Seller Official",
                    "Seller Performance (out of 5)", 
                    "Seller Shipping On-time (%)", 
                    "Seller No Cancellation (%)",
                    "Seller No Return (%)",
                    "Seller Satisfaction Product (%)", 
                    "Seller Satisfaction Product - User", 
                    "seller Satisfaction Product (%) - Totally Satisfied",
                    "seller Satisfaction Product (%) - Satisfied",
                    "seller Satisfaction Product (%) - Neutral",
                    "seller Satisfaction Product (%) - Dissatisfied",
                    "seller Satisfaction Product (%) - Totally Dissatisfied",
                    "seller Satisfaction Overall (%)",
                    "seller Satisfaction Overall - User",
                    "seller Satisfaction Overall (%) - Totally Satisfied",
                    "seller Satisfaction Overall (%) - Satisfied",
                    "seller Satisfaction Overall (%) - Neutral",
                    "seller Satisfaction Overall (%) - Dissatisfied",
                    "seller Satisfaction Overall (%) - Totally Dissatisfied",
                    "Product Brand")
  colnames(df) <- "value"
  df0 <- data.frame(cbind(product.specification[length(product.specification)-1]))
  rownames(df0) <- "Product Category"
  colnames(df0) <- "value"
  df <- rbind(df, df0)
  df
}
# View(df)