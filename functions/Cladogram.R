
# Function to get Jaccard index
get_cladogram <- function(data, filename, type){
  
  # Initiate for storage
  if(type == "gspec"){ # if we are looking at species
  species <- unique(data$gspec)}
  
  species <- unique(data$genus) 
  
  distmat <- matrix(NA, nrow = 0, ncol = 3)
  colnames(distmat) <- c('gspec1', 'gpsec2', 'distance')
  
  # Function to parse through and create pathogen sharing index
  for(gspec1 in species){
    
    # Pull the other organisms to compare
    others <- species[-1]

    if(type == "gspec"){
    org1 <- data %>% filter(gspec == gspec1)
    }
    org1 <- data %>% filter(genus == gspec1)
    
    # Loop through the other organisms
    for(gspec2 in others){
 
      # create tmp array
      tmp.arr <- c(rep(0, 4))
      
      # pull data for pairwise comparison
      if(type == "gspec"){
      org2 <- data %>% filter(gspec == gspec2)
      }
      org2 <- data %>% filter(genus == gspec2)
      
      
      # how closely related?
      if(org1$genus == org2$genus){ 
        tmp.arr[1] = TRUE
      }
      if(org1$tribe == org2$tribe){ 
        tmp.arr[2] = TRUE
      }
      if(org1$subfamily == org2$subfamily){ 
        tmp.arr[3] = TRUE
      }
      if(org1$family == org2$family){
        tmp.arr[4] = TRUE
      }
        tmp.arr[5] = TRUE
    
        # Find location of closest branch
        distance <- min(which(tmp.arr == TRUE))
  
      # Bind together this comparison with all comparisons
      tmp <- as.matrix(cbind(gspec1, gspec2, distance))

      distmat <- rbind(distmat, tmp)
    }
    
    # Update hostnames, remove the one you just searched
    species <- others
    
    # Calculate total runs left 
    left <- length(species)
    
    print(paste("only", left, "to go!", sep= " "))
  }
  
  # Just some name things
  colnames(distmat) <- c("org1", "org2", "distance")
  distmat <- as.data.frame(distmat)
  
  # Save data
  save(distmat, file = filename)
  
}





