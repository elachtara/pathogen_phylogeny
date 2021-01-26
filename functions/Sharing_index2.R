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


# All Hostnames 
hostnames <- unique(data$host)

# Function to parse through and create pathogen sharing index
for(org1 in hostnames){
  
  # Pull the other organisms to compare
  others <- hostnames[-1]

  # Loop through the other organisms
  for(org2 in others){
    
    # Get total pathogens between them
    total <- data %>% filter(host == c(org1, org2)) %>% 
      select(sci_name, host) %>% 
      group_by(sci_name, host) %>%
      unique() %>% nrow()
    
    # Get the number of shared pathogens
    shared <- data %>% 
      filter(host == c(org1, org2)) %>% 
      select(sci_name, host) %>% 
      group_by(sci_name, host) %>%
      unique() %>% 
      group_by(sci_name) %>%
      count() %>% 
      filter(n > 1) %>%
      nrow()
    
    if(shared == 0){
      # Dont add if nothing shared
      tmp <- matrix(NA, nrow = 0, ncol = 3)
      colnames(tmp) <- c('org1', 'org2', 'percent')
    }else{
    # Get the percent shared between them
    percent <- as.numeric(round((shared/total), 3))
    
    # Bind together this comparison with all comparisons
    temp <- as.matrix(cbind(org1, org2, percent))
    }
    
    sharing <- rbind(sharing, temp)
  }
  
  # Update hostnames, remove the one you just searched
  hostnames <- others
  
  # Calculate total runs left 
  left <- length(hostnames)
  
  print(paste("only", left, "to go!", sep= " "))
}

save(sharing, file = path_to_save)
#}
