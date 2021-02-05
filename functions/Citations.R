library(ggplot2)
load("data/example.rda")

hostnames = unique(hostdata$org1)
for(org in hostnames){
tmp <- c(org, org, 0, 0, 0, 0)
hostdata <- rbind(hostdata, tmp)
}

# 
p <- ggplot(hostdata, aes(x = org1, y = reorder(org2, desc(org2)), fill = as.numeric(shared)))+
  geom_tile(colour = "grey50")+
  geom_text(aes(label = citations), col ="white") +
  scale_fill_viridis_c(option = "C")+
  theme_minimal()+
  scale_x_discrete(position = "top")+
  labs(fill= "Pathogens shared")+
  theme(plot.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom")

ggsave("figures/citation.pdf",
       plot = p,
       units = "in",
       width = 8,
       height = 8,
       useDingbats = FALSE)


# Dis
p <- ggplot(hostdata, aes(x = as.numeric(citations)))+
  geom_density()+
  theme_minimal()+
  labs(x = "Citations")+
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom")

ggsave("figures/citations.pdf",
       plot = p,
       units = "in",
       width = 8,
       height = 8,
       useDingbats = FALSE)
