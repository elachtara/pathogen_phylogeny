library(ape)
library(dplyr)
library(geiger)
library(caper)
library(phytools)
library(adephylo)


# Read in Data
primate_data <- read.csv("data/phylo/Guide/full_primate_dataset_v3_2.17.2021.csv", header=T)

#10kTrees phylogeny
tree <- read.nexus("data/phylo/Guide/TenkTrees_v3_tree.nex")
  
# Plot tree
plot(tree, cex=0.4)


# Identify polytomy (when nodes < tips - 1; our case 128 < 130-1)
is.binary(tree) 
zoom(tree, list(grep("Macaca", tree$tip.label)))

# Resolve the polytomy by randomly assigning a branch to have length of zero
# transforms all multichotomies into a series of dichotomies with 1+ brances of length 0
tree <-  multi2di(tree)

# Check that names between phylogeny and dataset match
name.check(tree, primate_data, data.names = primate_data$TenkTr_species)


# Calculate phylo distance between all pairs
disMat <- adephylo::distTips(tree, tips = "all", method = "nNodes") %>% as.matrix()












# Sub analysis - clean for NA 
primate_data_sub1 = primate_data[,c("Total_PSR", "Female_kg", "WoS_citations", "TenkTr_species"),]
primate_data_sub = primate_data_sub1[-which(is.na(primate_data_sub1$Total_PSR)),]
rownames(primate_data_sub) = primate_data_sub$TenkTr_species

# Prune the phylogeny to match the subset
nameck = name.check(tree, primate_data_sub, data.names=primate_data_sub$TenkTr_species)
nameck
tree_sub = drop.tip(tree, nameck$tree_not_data)

#preliminary plots just to look at the data
par(mfrow=c(1,1))
plot(log10(Total_PSR)~log10(WoS_citations), primate_data_sub)
plot(log10(Total_PSR)~log10(Female_kg), primate_data_sub)
plot(log10(WoS_citations)~log10(Female_kg), primate_data_sub)


#ordinary regression
ordinary_ls = lm(log10(Total_PSR) ~ log10(Female_kg) + log10(WoS_citations), primate_data_sub)
summary(ordinary_ls)




