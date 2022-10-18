library(igraph)
library(netrankr)
library(tidyverse)
source("Rscripts/functions.R")

pin_seq <- 0.3#seq(0.4,0.99,length.out=15)
pout_fac <- seq(0.05,0.99,length.out=50)
n <- 500
k <- 5
res <- tibble()
for(i in seq_along(pin_seq)){
  for(j in seq_along(pout_fac)){
    cat(i,"-",j,"\r")
    pin <- pin_seq[i]
    pout <- pin_seq[i]*pout_fac[j]
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
res |> 
  ggplot(aes(x=mod,y=disc/choose(n,2)))+
  geom_point(aes(col=pout/pin))+facet_wrap(~combo)+
  scale_x_continuous(limits=c(-0.1,1))

res |> 
  ggplot(aes(x=ev2/ev1,y=disc/choose(n,2)))+
  geom_point(aes(col=pout/pin))+facet_wrap(~combo)+
  scale_x_continuous(limits=c(-0.1,1))

cor(res$mod,res$ev2/res$ev1)
