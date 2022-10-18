library(igraph)
library(networkdata)
library(netrankr)
library(tidyverse)
source("Rscripts/functions.R")
g <- karate
g <- netUtils::biggest_component(g)
deg <- degree(g)

map_dfr(1:500,function(x){
  g1 <- sample_degseq(deg,method = "simple.no.multiple")  
  cent_disc_no_sc(g1)
},.id="run") -> res_degs
res_degs$type <- "deg"
ggplot()+
  geom_boxplot(data = res_degs,aes(x=combo,y=disc))+
  geom_point(data = cent_disc_no_sc(g),aes(combo,disc),color="red")


kcore <- coreness(g)
map_dfr(1:500,function(x){
  g1 <- netUtils::sample_coreseq(kcore)  
  cent_disc_no_sc(g1)
},.id="run") -> res_cores
res_cores$type <- "core"
ggplot()+
  geom_boxplot(data = res_cores,aes(x=combo,y=disc))+
  geom_point(data = cent_disc_no_sc(g),aes(combo,disc),color="red")


map_dfr(1:500,function(x){
  g1 <- rewire(g,each_edge(1))  
  cent_disc_no_sc(g1)
},.id="run") -> res_dens
res_dens$type <- "dens"
ggplot()+
  geom_boxplot(data = res_dens,aes(x=combo,y=disc))+
  geom_point(data = cent_disc_no_sc(g),aes(combo,disc),color="red")


ggplot(bind_rows(res_degs,res_cores,res_dens))+
  geom_boxplot(aes(x=combo,y=disc,col=type))
