# resource: https://nceas.github.io/oss-lessons/data-liberation/intro-webscraping.html
# BlueLetterBible SearchCriteria <- "forgive"
library(rvest)
library(stringr)
library(dplyr)

Strongs <- c("H3722","H5375","H5545","H5546","H5547","G859","G863","G5483")

URL <- "https://www.blueletterbible.org/lang/lexicon/lexicon.cfm?strongs=H3722"

URL %>% read_html() %>% html_nodes(".lexTitleHb") %>% html_text()
URL %>% read_html() %>% html_nodes("#lexTrans") %>% html_text()
URL %>% read_html() %>% html_nodes(".lexStrongsDef") %>% html_text()
URL %>% read_html() %>% html_nodes("#lexCount") %>% html_text()
URL %>% read_html() %>% html_nodes("#concordanceResults") %>% html_text()

URL %>% read_html(options = ) %>% html_nodes("#bibleTable") %>% html_text()

test1 <- c("a", "b", "c", "d")
for (i in test1) {
  print(paste0("https:/---",i,"---/.com"))
}