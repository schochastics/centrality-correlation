library(igraph)
library(netrankr)
library(tidyverse)
source("Rscripts/functions.R")

pin_seq <- 0.4#seq(0.4,0.99,length.out=15)
pout_fac <- seq(0.05,0.99,length.out=50)
n <- 1000
k <- 5
res <- tibble()
for(i in seq_along(pin_seq)){
  for(j in seq_along(pout_fac)){
    pin <- pin_seq[i]
    pout <- pin_seq[i]*pout_fac[j]
    for(rep in 1:10){
      cat(i,"-",j,"-",rep,"\r")
      pmat <- matrix(pout,k,k)
      diag(pmat) <- pin
      g <- sample_sbm(n,pmat,block.sizes = rep(n/k,k))
      while(!is.connected(g)){
        print("resample")
        g <- sample_sbm(n,pmat,block.sizes = rep(n/k,k))
      }
      tmp <- cent_disc_no_sc(g)
      
      # tmp$mod <- modularity(g,rep(c(1,2,3),each=100))
      tmp$mod <- modularity(g,membership(cluster_louvain(g)))
      # tmp$core <- netUtils::core_periphery(g)$corr
      evs <- largest_ev(g)
      tmp$ev1 <- evs[1]
      tmp$ev2 <- evs[2]
      tmp$mgap <- majorization_gap(g)
      tmp$pin <- pin
      tmp$pout <- pout
      res <- bind_rows(res,tmp)
    }
  }
}
write_csv(res,"data/cluster_sim_sbm.csv")

# lfr benchmark graph generation ----
setwd("data/lfr")
mu <- seq(0.1,1,by = 0.05)
res <- tibble()
for(m in mu){
  for(i in 1:10){
    system(paste0("./benchmark -N 1000 -k 15 -maxk 50 -mu ",m," -minc 20 -maxc 50"))
    system("rm statistics_.txt time_seed.dat")
    outg <- paste0("graph_",m,"-",i,".txt")
    outm <- paste0("membership_",m,"-",i,".txt")
    system(paste0("mv graph_.txt ",outg))
    system(paste0("mv membership_.txt ",outm))
  }
}
setwd("../..")

fl <- list.files("data/lfr",pattern = "txt",full.names = TRUE)
df <- tibble(graph=fl[str_detect(fl,"graph")],
       membership=fl[str_detect(fl,"memb")],
       mu = str_extract(graph,"_[0-9]+.*\\-") |> parse_number(),
       id = str_extract(graph,"[0-9]+\\.txt") |> parse_number()) |> 
  arrange(mu,id)

res <- tibble()
for(i in 1:nrow(df)){
  cat(i,"\n")
  g <- readr::read_tsv(df$graph[i],col_names = FALSE,show_col_types = FALSE) |> 
    graph_from_data_frame(directed = FALSE) |> 
    igraph::simplify()
  mem <- readr::read_tsv(df$membership[i],col_names = FALSE,show_col_types = FALSE)
  V(g)$memb <- mem$X2
  tmp <- cent_disc_no_sc(g)
  tmp$mod <- modularity(g,membership(cluster_louvain(g)))
  evs <- largest_ev(g)
  tmp$ev1 <- evs[1]
  tmp$ev2 <- evs[2]
  tmp$mgap <- majorization_gap(g)
  tmp$dens <- graph.density(g)
  tmp$mu <- df$mu[i]
  res <- bind_rows(res,tmp)
}

write_csv(res,"data/cluster_sim_lfr.csv")
