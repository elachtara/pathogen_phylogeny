load("data/clean_pathogen.rda")
data = clean_pathogen
path_to_save = "data/sharing.rda"

# Function to compute sharing index across species
#get_sharing <- function(data, path_to_save){

# Initiate for storage
sharing <- matrix(NA, nrow = 0, ncol = 3) %>% as.data.frame()
colnames(sharing) <- c('org1', 'org2', 'percent')

# All Hostnames 
hostnames <- unique(data$host)

# Function to parse through and create pathogen sharing index
for(org1 in hostnames){
  
  # Pull the other organisms to compare
  others <- hostnames[-1]

  # Loop through the other organisms
  for(org2 in others){
    
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
  
  # Update hostnames, remove the one you just searched
  hostnames <- others
  
  # Calculate total runs left 
  left <- length(hostnames)
  
  print(paste("only", new, "to go!", sep= " "))
}

save(sharing, file = path_to_save)
#}
