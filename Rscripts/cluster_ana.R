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
    cat(i,"-",j,"\r")
    pin <- pin_seq[i]
    pout <- pin_seq[i]*pout_fac[j]
    for(rep in 1:10){
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

tbl <- res |>  
  group_by(combo,pin,pout) |> 
  dplyr::summarise(frac=mean(disc)/mean(choose(n,2)),
                   spec_gap=mean((ev2/ev1)),
                   modularity=mean(mod),
                   mgap=mean(mgap),
                   .groups = "drop") |> 
  dplyr::filter(!str_detect(combo,"subgraph")) |> 
  separate(combo,into = c("a","b"),sep = "-")

tbl1 <- tbl |> rename(b=1,a=2)  |> bind_rows(tbl)
tbl1 <- tbl1 |> mutate(a=factor(a,levels=c("degree","betweenness","closeness","eigenvector")),
                       b=factor(b,levels=c("degree","betweenness","closeness","eigenvector")))
ggplot(tbl1)+
  geom_point(aes(x=modularity,y=frac,col=pin-pout))+
  facet_grid(a~b)+
  scale_color_gradient(low="#d9d9d9",high="#252525",name=expression(p[`in`]-p[out]))+
  # scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1),labels = c("0","0.25","0.5","0.75","1"))+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=14,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=12,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(x="modularity",y="fraction of discordant pairs")+
  guides(color=guide_colorbar())

ggsave("figures/cluster_sim_mgap.pdf",width = 11,height=6)
system("cp figures/cluster_sim_mgap.pdf ~/Dropbox/schofie/centrality_correlation/figures/")


ggplot(tbl1)+
  geom_point(aes(x=spec_gap,y=frac,col=pin-pout))+
  facet_grid(a~b)+
  scale_color_gradient(low="#d9d9d9",high="#252525",name=expression(p[`in`]-p[out]))+
  # scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1),labels = c("0","0.25","0.5","0.75","1"))+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=14,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=12,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(x="modularity",y="fraction of discordant pairs")+
  guides(color=guide_colorbar())

ggsave("figures/cluster_sim_spec.pdf",width = 11,height=6)
system("cp figures/cluster_sim_spec.pdf ~/Dropbox/schofie/centrality_correlation/figures/")
