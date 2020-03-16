

util_scrape_table <- function(link, table_no = 1){

  require(xml2)
  require(rvest)
  require(janitor)
  require(tidyverse)

  raw_webpage <- read_html(link)

  html_table(raw_webpage, fill = TRUE)[[table_no]]

}




