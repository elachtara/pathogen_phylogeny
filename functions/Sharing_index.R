
# Function to compute sharing index across species
get_sharing <- function(data, path_to_save){
  
# Initiate for storage
sharing <- matrix(NA, nrow = 0, ncol = 3) %>% as.data.frame()
colnames(sharing) <- c('org1', 'org2', 'percent')


# Function to parse through and create pathogen sharing index
for(i in 1:length(unique(data$host))){

  # We don't want to see the warnings
  options(warn=-1)
  
  # Hostnames 
  hostnames <- unique(data$host)

  # Pull the organism of comparison
  org1 <- hostnames[i]
  
  # Pull the other organisms to compare
  others <- c(1:length(hostnames))
  others <- others[-i]

  # Loop through the other organisms
  for(j in others){
    
    # We don't want to see the warnings
    options(warn=-1)
    
    # Organism to compare
    org2 <- hostnames[j]
    
    # Get total pathogens between them
    total <- data %>% filter(host == c(org1, org2)) %>% select(sci_name, host) %>% group_by(sci_name, host) %>% unique()
    
    # Get the number of shared pathogens
    shared <- total %>% group_by(sci_name) %>% count() %>% filter(n > 1) 
    
    # Get the percent shared between them
    percent <- round(nrow(shared)/nrow(total), 3)
    
    # Bind together this comparison with all comparisons
    temp <- cbind(org1, org2, percent)
    sharing <- rbind(sharing, temp) %>% mutate(percent = as.numeric(percent))
  }
  
  # Print a progress report
  percent_done <- round(i/length(hostnames), 3)
  print(paste(percent_done, "complete", sep =" "))
}
save(sharing, file = path_to_save)
}
