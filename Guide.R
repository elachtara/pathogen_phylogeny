library(ape)
library(geiger)
library(caper)
library(phytools)

#set working directory (where you put the data and nexus files in your computer)
setwd("~/your_path/")  

#dataset
primate_data = read.csv("full_primate_dataset_v3_2.17.2021.csv", header=T)
head(primate_data)
length(primate_data$TenkTr_species) #130 species in dataset

#10kTrees phylogeny (not an authority but matches our species dataset exactly, so we retain sample size)
tree = read.nexus("TenkTrees_v3_tree.nex") #read in the tree file
plot(tree, cex=0.4) #visualize the tree file
tree #130 tips, 128 internal nodes (# internal nodes should be # tips - 1, so this tree has a polytomy)
is.binary(tree) #and this confirms that the tree indeed is not binary, i.e. has a polytomy

#where is the polytomy? 
#on close examination, I found the polytomy in genus Macaca
zoom(tree, list(grep("Macaca", tree$tip.label)))

#resolve the polytomy by randomly assigning a branch to have length of zero
tree = multi2di(tree)
tree #now has 130 tips and 129 internal nodes,
is.binary(tree) #and now the tree is binary
zoom(tree, list(grep("Macaca", tree$tip.label))) #but should still look the same because branch length=0

#check that names between phylogeny and dataset match (I did this step already when I created the phylogeny file)
name.check(tree, primate_data, data.names=primate_data$TenkTr_species)

#now create a subset of data with just the traits we are interested in (removing NAs)
#and a subset of phylogeny to match

#select three columns to focus our analysis on, plus species names
primate_data_sub1 = primate_data[,c("Total_PSR", "Female_kg", "WoS_citations", "TenkTr_species"),]
summary(primate_data_sub1) #NAs in Total_PSR column
primate_data_sub = primate_data_sub1[-which(is.na(primate_data_sub1$Total_PSR)),] #drop NAs
summary(primate_data_sub) #no NAs
#set rownames as the species names (for some analyses this is useful)
rownames(primate_data_sub)=primate_data_sub$TenkTr_species

#now prune the phylogeny to match the subset
nameck = name.check(tree, primate_data_sub, data.names=primate_data_sub$TenkTr_species)
nameck
tree_sub=drop.tip(tree, nameck$tree_not_data) #drop the species from the tree not in our data

#preliminary plots just to look at the data
par(mfrow=c(1,1))
plot(log10(Total_PSR)~log10(WoS_citations), primate_data_sub)
plot(log10(Total_PSR)~log10(Female_kg), primate_data_sub)
plot(log10(WoS_citations)~log10(Female_kg), primate_data_sub)
#all variables should be log transformed 

#ordinary regression
ordinary_ls = lm(log10(Total_PSR)~log10(Female_kg)+log10(WoS_citations), primate_data_sub)
summary(ordinary_ls)

#pick up tutorials here using
#primate_data_sub columns Total_PSR, Female_kg, and WoS_citations (take log10)
#and tree_sub





