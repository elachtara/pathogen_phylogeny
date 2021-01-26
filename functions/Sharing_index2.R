load("data/clean_pathogen.rda")
data = clean_pathogen %>% distinct()
scinames <- unique(data$sci_name)
hostnames <- unique(data$host)

# Initiate for storage
sharing <- matrix(NA, nrow = length(scinames), 
                  ncol = length(hostnames))
colnames(sharing) <- hostnames
rownames(sharing) <- scinames

# Populate the count matrix 
for(c in 1:ncol(sharing)){
  for(r in 1:nrow(sharing)){
    sharing[r,c] =  data %>% 
      filter(host == colnames(sharing)[c]) %>%
      filter(sci_name == rownames(sharing)[r]) %>%
      nrow()
  }
}

save(sharing, file = "data/counts.rda")

