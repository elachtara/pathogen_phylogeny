# Load in data

# Function to get Jaccard index
get_sharing <- function(data, path_to_save){
  
  # Initiate for storage
  hostnames <- unique(data$host)
  sharing <- matrix(NA, nrow = 0, ncol = 5)
  colnames(sharing) <- c('org1', 'org2', 'org1.count', 'org2.count', 'shared')
  

# Function to parse through and create pathogen sharing index
for(org1 in hostnames){
  
  # Pull the other organisms to compare
  others <- hostnames[-1]
  
  # How many for org1
  org1.sci <- data %>% filter(host == org1) %>% 
    select(sci_name, host) %>% 
    group_by(sci_name) %>%
    unique() 
  org1.count <- nrow(org1.sci)
  
  # Loop through the other organisms
  for(org2 in others){
    
    # How many for org2
    org2.sci <- data %>% filter(host == org2) %>% 
      select(sci_name, host) %>% 
      group_by(sci_name) %>%
      unique()
    org2.count <- nrow(org2.sci)
    
    # Get the number of shared pathogens
    shared <- length(Reduce(intersect, list(org1.sci$sci_name, org2.sci$sci_name)))
   
    #if(shared == NA){
      # Dont add if nothing shared
      #tmp <- matrix(NA, nrow = 0, ncol = 5)
      #colnames(tmp) <- c('org1', 'org2', 'org1.count', 'org2.count', 'shared')
    #}else{
      
    # Get the percent shared between them
    #percent <- as.numeric(round((shared/total), 3))
    
    # Bind together this comparison with all comparisons
    temp <- as.matrix(cbind(org1, org2, org1.count, org2.count, shared))
    #}
    
    sharing <- rbind(sharing, temp)
  }
  
  # Update hostnames, remove the one you just searched
  hostnames <- others
  
  # Calculate total runs left 
  left <- length(hostnames)
  
  print(paste("only", left, "to go!", sep= " "))
}

  # Save data
  save(sharing, file = path_to_save)
  
}


