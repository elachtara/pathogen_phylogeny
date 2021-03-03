library(wosr)
library(dplyr)

# https://api.clarivate.com/swagger-ui/?url=https%3A%2F%2Fdeveloper.clarivate.com%2Fapis%2Fwoslite%2Fswagger%3FforUser%3D5171c957941c8d11ba1656f0bc4986906b2c5276
# https://developer.clarivate.com/help/api


# for a small example
data = as.data.frame(sharing)
samp <- c('Silene baccifera' , 'Silene balansae', 'Silene behen', 'Silene bernardina', 'Silene brahuica', 'Silene bungei', 'Silene bupleuroides')
hostdata = data %>% filter(org1 %in% samp) %>% filter(org2 %in% samp)
org1 =  unique(hostdata$org1)
citations = c(0, 0, 4, 2, 10, 3)
citations = cbind(org1, citations) %>% as.data.frame()

hostdata = full_join(hostdata, citations, by = "org1")

save(hostdata, file = "data/example.rda")


# Authentication to get a session id - need user
# https://cran.r-project.org/web/packages/wosr/wosr.pdf
Sys.setenv(WOS_USERNAME = NULL, WOS_PASSWORD = NULL)
sid <- auth(username = NULL,
            password = NULL)


for(org in hostnames){
# Use session id to run a query
search_query <- paste("TI='", org, "' OR AB='", org, "' OR AK='", org, "'", sep = "")
#res <- wos_search(sid, search_query)
print(search_query)
}




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
