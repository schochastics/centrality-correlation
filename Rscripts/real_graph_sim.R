library(networkdata)
library(igraph)
library(netrankr)
library(tidyverse)
source("Rscripts/functions.R")

d <- data(package = "networkdata")
nm <- d$results[, "Item"]
data(list = nm, package = "networkdata")

for(i in 1:length(nm)){
  gList <- mget(nm[i])[[1]]
  cat(i,"\r")
  if(class(gList)=="igraph"){
    g <- igraph::simplify(as.undirected(gList))
    if(vcount(g)<20 | !is.connected(g) | vcount(g)>2500){
      next()
    }
    if("weight"%in%edge_attr_names(g)){
      g <- delete_edge_attr(g,"weight")
    }
    res <- as_tibble(cent_disc_no_sc(g))
    res$id <- paste0(nm[i])
    res$n <- vcount(g)
    res$dens <- graph.density(g)
    res$diameter <- diameter(g)
    evs <- largest_ev(g)
    res$ev1 <- evs[1]
    res$ev2 <- evs[2]
    res$spec_gap <- 1-evs[2]/evs[1]
    res$mgap <- majorization_gap(g)
    res$modularity <- modularity(g,membership(cluster_louvain(g)))
    res$core <- ifelse(graph.density(g)==1,1,netUtils::core_periphery(g)$corr)
    res$assort <- assortativity_degree(g,directed = FALSE)
    res$ccoef <- transitivity(g,"undirected")
    res$glob_eff <- average_local_efficiency(g)
    
    readr::write_csv(res,"data/real_sim.csv",append = file.exists("data/real_sim.csv"),progress = FALSE)
  } else{
    for(j in 1:length(gList)){
      g <- igraph::simplify(as.undirected(gList[[j]]))
      if(vcount(g)<20 | !is.connected(g) | vcount(g)>2500){
        next
      }
      if("weight"%in%edge_attr_names(g)){
        g <- delete_edge_attr(g,"weight")
      }
      res <- as_tibble(cent_disc_no_sc(g))
      res$id <- paste0(nm[i],"-",j)
      res$n <- vcount(g)
      res$dens <- graph.density(g)
      res$diameter <- diameter(g)
      evs <- largest_ev(g)
      res$ev1 <- evs[1]
      res$ev2 <- evs[2]
      res$spec_gap <- 1-evs[2]/evs[1]
      res$mgap <- majorization_gap(g)
      res$modularity <- modularity(g,membership(cluster_louvain(g)))
      res$core <- ifelse(graph.density(g)==1,1,netUtils::core_periphery(g)$corr)
      res$assort <- assortativity_degree(g,directed = FALSE)
      res$ccoef <- transitivity(g,"undirected")
      res$glob_eff <- average_local_efficiency(g)
      readr::write_csv(res,"data/real_sim.csv",append = file.exists("data/real_sim.csv"),progress = FALSE)
  }
}

}
