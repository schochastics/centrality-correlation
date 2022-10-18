suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(netrankr))
suppressPackageStartupMessages(library(tidyverse))
source("Rscripts/functions.R")

# if(!file.exists("data/random_er_sim.csv")){
#   ns <- c(100,250,500,1000,1500)
#   ps <- seq(0.02,0.1,length.out=10)
#   tbl <- tibble()
#   for(i in 1:50000){
#     cat(i,"\r")
#     m <- sample(ps,1)
#     g <- sample_gnp(sample(ns,1),m)
#     k <- 0
#     while(!is.connected(g) & k<5){
#       m <- sample(ps,1)
#       g <- sample_gnp(sample(ns,1),m)
#       k <- k+1
#     }
#     res <- as_tibble(cent_disc_no_sc(g))
#     res$id <- i
#     res$n <- vcount(g)
#     res$dens <- graph.density(g)
#     res$diameter <- diameter(g)
#     evs <- largest_ev(g)
#     res$ev1 <- evs[1]
#     res$ev2 <- evs[2]
#     res$mgap <- majorization_gap(g)
#     res$param <- m
#     write_csv(res,"data/random_er_sim.csv",append = file.exists("data/random_er_sim.csv"))
#   }
# }
cat("ER\n")
if(!file.exists("data/random_er_sim.csv")){
  ns <- c(100,250,500,1000,2500,5000,10000)
  ps <- c(0.1,0.1,0.05,0.025,0.005,0.0025,0.001)
  tbl <- tibble()
  for(i in seq_along(ns)){
    for(j in 1:100){
      cat(i,"-",j,"\r")
      for(d in c(0.5,1,2)){
        m <- ps[i]
        g <- sample_gnp(ns[i],m*d)
        k <- 0
        while(!is.connected(g) & k<10){
          m <- ps[i]
          g <- sample_gnp(ns[i],m*d)
          k <- k+1
        }
        res <- as_tibble(cent_disc_no_sc(g))
        res$id <- i
        res$n <- vcount(g)
        res$dens <- graph.density(g)
        res$diameter <- diameter(g)
        evs <- largest_ev(g)
        res$ev1 <- evs[1]
        res$ev2 <- evs[2]
        res$mgap <- majorization_gap(g)
        res$param <- m*d
        write_csv(res,"data/random_er_sim.csv",append = file.exists("data/random_er_sim.csv"))
      }
    }
  }
}

cat("PA\n")
if(!file.exists("data/random_pa_sim.csv")){
  ns  <- c(100,250,500,1000,2500,5000,10000)
  ps  <- c(5,14,13,13,7,6,5)
  tbl <- tibble()
  for(i in seq_along(ns)){
    for(j in 1:100){
      cat(i,"-",j,"\r")
      for(d in c(0.5,1,2)){
        m <- ps[i]
        g <- sample_pa(ns[i],power = 2,m = m*d,directed = FALSE)
        while(!is.connected(g)){
          g <- sample_pa(ns[i],power = 2,m = m*d,directed = FALSE)
        }
        res <- as_tibble(cent_disc_no_sc(g))
        res$id <- i
        res$n <- vcount(g)
        res$dens <- graph.density(g)
        res$diameter <- diameter(g)
        evs <- largest_ev(g)
        res$ev1 <- evs[1]
        res$ev2 <- evs[2]
        res$mgap <- majorization_gap(g)
        res$param <- m*d
        write_csv(res,"data/random_pa_sim.csv",append = file.exists("data/random_pa_sim.csv"))
      }
    }
  }
}
