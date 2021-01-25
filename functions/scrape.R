#library(robotstxt)
#library(purrr)
#library(lubridate)
#library(ggplot2)
#library(tidyverse) 
#library(rvest)
#library(janitor)
#library(xtable)
#library(tinytex)
#pacman::p_load(DT)

install.packages("devtools")  # if required
devtools::install_github("juba/rwos")
library(rwos)

# Authentication to get a session id
sid <- wos_authenticate()

# Use session id to run a query
search_query <- paste("TI='", org, "' OR AB='", org, "' OR AK='", org, "'", sep = "")
res <- wos_search(sid, search_query)

# Number of results
pubs <- wos_retrieve_all(res)



# Create a search element
#TI='silene  latifolia'  OR  AB='silene  latifolia'  OR  AK='silene  latifolia'
search_for <- paste("TI='", org, "' OR AB='", org, "' OR AK='", org, "'", sep = "")

# Search for the element



# loop over pages
getcomments <- function(url){
  doc <- read_html(url)
  comments <- doc %>%
    html_nodes(".review-text") %>%
    html_text() 
  commented<- data.frame(comments, stringsAsFactors = F) 
  return(commented)
}

scrapingamazon <- function(url){
  doc <- read_html(url)
  # Parse relevant elements from HTML
  author <- doc %>%
    html_nodes(".a-profile-name") %>%
    html_text()
  date <- doc %>%
    html_nodes(".review-date") %>%
    html_text() %>% 
    gsub(".*on ", "", .)
  reviews<- data.frame(author, date, stringsAsFactors = F) 
  return(reviews)
}


#https://www.amazon.com/Reeses-Pinworm-Medicine-Prescription-Strength
pages <- 39
reviews_all <- NULL
for(page_num in 1:pages){
  url <- paste0("https://www.amazon.com/Reeses-Pinworm-Medicine-Prescription-Strength/product-reviews/B002LF0J1M/?reviewerType=all_reviews&pageNumber=", page_num)
  reviews<-scrapingamazon(url)
  reviews_all <- rbind(reviews_all, reviews)
}
pdata<-reviews_all
