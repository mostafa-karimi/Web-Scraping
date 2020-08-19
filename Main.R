library(xml2)
url <- "https://en.wikipedia.org/wiki/Money_(disambiguation)"
rl <- readLines(con = url)
hp <- read_html(url)
l <- as_list(hp)
xml_children(xml_children(xml_children(hp)))
xml_children(xml_children(hp)[1])[24]
xml_children(hp)[1]
xml_length(xml_children(hp)[2])
