library(intergraph)
library(igraph)
library(netrankr)
library(tidyverse)
source("Rscripts/functions.R")
ergm_mods <- readRDS("data/ergm_models.RDS")
graphs <- readRDS("data/graphs.RDS")[c(1,3:9)]
names(graphs) <- names(ergm_mods)[4:11]

ergm_disc <- map_dfr(ergm_mods[4:11], function(x){
  ergm_sim <- simulate(x, nsim = 100)  
  gList <- lapply(ergm_sim,asIgraph)
  disc_simList <- map_dfr(gList,cent_disc)  
},.id = "network")

graph_disc <- map_dfr(graphs,function(x){
  cent_disc(x)
},.id="network")

ggplot()+
  geom_boxplot(data = ergm_disc,aes(x=combo,y=disc))+
  geom_point(data = graph_disc,aes(x=combo,y=disc),col="red",size=2)+
  facet_wrap(network~.)+theme(axis.text.x = element_text(angle=90))

ggsave("figures/ergm_sims.png",width = 20,height = 14)

ggplot()+
  geom_density(data = disc_simList,aes(x=disc))+
  geom_vline(data = disc_orig,aes(xintercept=disc),col="red")+
  facet_wrap(combo~.,scales = "free_y")
