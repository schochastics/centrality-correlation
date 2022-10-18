suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(netrankr))
source("Rscripts/functions.R")
set.seed(123)

cat("-n=100,p=0.1\n")
tg <- threshold_graph(n = 100,p = 0.1)
tbl <- tibble()
for(i in 1:100){
  cat(i,"\r")
  for(j in 1:10){
    g <- rewire(tg,each_edge(prob = i/100))
    while(!is.connected(g)){
      g <- rewire(tg,each_edge(prob = i/100))
    }
    res <- as_tibble(cent_disc_no_sc(g))
    res$n <- vcount(g)
    res$dens <- graph.density(g)
    res$rewire <- i/100
    res$diameter <- diameter(g)
    evs <- largest_ev(g)
    res$ev1 <- evs[1]
    res$ev2 <- evs[2]
    #res$spec_gap <- spectral_gap(g)
    res$mgap <- majorization_gap(g)
    res$cpcor1 <- netUtils::core_periphery(g)$corr
    tbl <- bind_rows(tbl,res)
  }
}
saveRDS(tbl,"data/rewire_sim_n100_p01.RDS")


cat("-n=250,p=0.1\n")
tg <- threshold_graph(n = 250,p = 0.1)
tbl <- tibble()
for(i in 1:100){
  cat(i,"\r")
  for(j in 1:10){
    g <- rewire(tg,each_edge(prob = i/100))
    while(!is.connected(g)){
      g <- rewire(tg,each_edge(prob = i/100))
    }
    res <- as_tibble(cent_disc_no_sc(g))
    res$n <- vcount(g)
    res$dens <- graph.density(g)
    res$rewire <- i/100
    res$diameter <- diameter(g)
    evs <- largest_ev(g)
    res$ev1 <- evs[1]
    res$ev2 <- evs[2]
    # res$spec_gap <- spectral_gap(g)
    res$mgap <- majorization_gap(g)
    res$cpcor1 <- netUtils::core_periphery(g)$corr
    tbl <- bind_rows(tbl,res)
  }
}
saveRDS(tbl,"data/rewire_sim_n250_p01.RDS")

cat("-n=500,p=0.05\n")
tg <- threshold_graph(n = 500,p = 0.05)
tbl <- tibble()
for(i in 1:100){
  cat(i,"\r")
  for(j in 1:10){
    g <- rewire(tg,each_edge(prob = i/100))
    while(!is.connected(g)){
      g <- rewire(tg,each_edge(prob = i/100))
    }
    res <- as_tibble(cent_disc_no_sc(g))
    res$n <- vcount(g)
    res$dens <- graph.density(g)
    res$rewire <- i/100
    res$diameter <- diameter(g)
    evs <- largest_ev(g)
    res$ev1 <- evs[1]
    res$ev2 <- evs[2]
    # res$spec_gap <- spectral_gap(g)
    res$mgap <- majorization_gap(g)
    res$cpcor1 <- netUtils::core_periphery(g)$corr
    tbl <- bind_rows(tbl,res)
  }
}
saveRDS(tbl,"data/rewire_sim_n500_p005.RDS")

cat("-n=1000,p=0.025\n")
tg <- threshold_graph(n = 1000,p = 0.025)
tbl <- tibble()
for(i in 1:100){
  cat(i,"\r")
  for(j in 1:10){
    g <- rewire(tg,each_edge(prob = i/100))
    while(!is.connected(g)){
      g <- rewire(tg,each_edge(prob = i/100))
    }
    res <- as_tibble(cent_disc_no_sc(g))
    res$n <- vcount(g)
    res$dens <- graph.density(g)
    res$rewire <- i/100
    res$diameter <- diameter(g)
    evs <- largest_ev(g)
    res$ev1 <- evs[1]
    res$ev2 <- evs[2]
    # res$spec_gap <- spectral_gap(g)
    res$mgap <- majorization_gap(g)
    res$cpcor1 <- netUtils::core_periphery(g)$corr
    tbl <- bind_rows(tbl,res)
  }
}
saveRDS(tbl,"data/rewire_sim_n1000_p0025.RDS")

cat("-n=2500,p=0.005\n")
tg <- threshold_graph(n = 2500,p = 0.005)
tbl <- tibble()
for(i in 1:100){
  cat(i,"\r")
  for(j in 1:10){
    g <- rewire(tg,each_edge(prob = i/100))
    while(!is.connected(g)){
      g <- rewire(tg,each_edge(prob = i/100))
    }
    res <- as_tibble(cent_disc_no_sc(g))
    res$n <- vcount(g)
    res$dens <- graph.density(g)
    res$rewire <- i/100
    res$diameter <- diameter(g)
    evs <- largest_ev(g)
    res$ev1 <- evs[1]
    res$ev2 <- evs[2]
    # res$spec_gap <- spectral_gap(g)
    res$mgap <- majorization_gap(g)
    res$cpcor1 <- netUtils::core_periphery(g)$corr
    tbl <- bind_rows(tbl,res)
  }
}
saveRDS(tbl,"data/rewire_sim_n2500_p0005.RDS")


cat("-n=5000,p=0.0025\n")
tg <- threshold_graph(n = 5000,p = 0.0025)
tbl <- tibble()
for(i in 1:100){
  cat(i,"\r")
  for(j in 1:10){
    g <- rewire(tg,each_edge(prob = i/100))
    while(!is.connected(g)){
      g <- rewire(tg,each_edge(prob = i/100))
    }
    res <- as_tibble(cent_disc_no_sc(g))
    res$n <- vcount(g)
    res$dens <- graph.density(g)
    res$rewire <- i/100
    res$diameter <- diameter(g)
    evs <- largest_ev(g)
    res$ev1 <- evs[1]
    res$ev2 <- evs[2]
    # res$spec_gap <- spectral_gap(g)
    res$mgap <- majorization_gap(g)
    res$cpcor1 <- netUtils::core_periphery(g)$corr
    tbl <- bind_rows(tbl,res)
  }
}
saveRDS(tbl,"data/rewire_sim_n5000_p00025.RDS")

cat("-n=10000,p=0.001\n")
tg <- threshold_graph(n = 10000,p = 0.001)
tbl <- tibble()
for(i in 1:100){
  cat(i,"\r")
  for(j in 1:10){
    g <- rewire(tg,each_edge(prob = i/100))
    while(!is.connected(g)){
      g <- rewire(tg,each_edge(prob = i/100))
    }
    res <- as_tibble(cent_disc_no_sc(g))
    res$n <- vcount(g)
    res$dens <- graph.density(g)
    res$rewire <- i/100
    res$diameter <- diameter(g)
    evs <- largest_ev(g)
    res$ev1 <- evs[1]
    res$ev2 <- evs[2]
    # res$spec_gap <- spectral_gap(g)
    res$mgap <- majorization_gap(g)
    res$cpcor1 <- netUtils::core_periphery(g)$corr
    tbl <- bind_rows(tbl,res)  
  }
}
saveRDS(tbl,"data/rewire_sim_n10000_p0001.RDS")
