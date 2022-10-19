library(igraph)
library(netrankr)
library(tidyverse)
source("Rscripts/functions.R")

n <- 1000
res <- read_csv("data/cluster_sim.csv")
tbl <- res |>  
  group_by(combo,pin,pout) |> 
  dplyr::summarise(frac=mean(disc)/mean(choose(n,2)),
                   spec_gap=mean((ev2/ev1)),
                   modularity=mean(mod),
                   mgap=mean(mgap),
                   .groups = "drop") |> 
  separate(combo,into = c("a","b"),sep = "-")

tbl1 <- tbl |> rename(b=1,a=2)  |> bind_rows(tbl)
tbl1 <- tbl1 |> mutate(a=factor(a,levels=c("degree","betweenness","closeness","eigenvector")),
                       b=factor(b,levels=c("degree","betweenness","closeness","eigenvector")))
ggplot(tbl1)+
  geom_point(aes(x=modularity,y=frac,col=pout))+
  facet_grid(a~b)+
  scale_color_gradient(low="#d9d9d9",high="#252525",name=expression(p[out]))+
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
  guides(color=guide_colorbar(barwidth = 10,barheight = 0.5))

ggsave("figures/cluster_sim_mgap.pdf",width = 11,height=6)
system("cp figures/cluster_sim_mgap.pdf ~/Dropbox/schofie/centrality_correlation/figures/")

cor(res$mod,res$ev2/res$ev1)
cor(res$mod,res$mgap)

ggplot(tbl1)+
  geom_point(aes(x=mgap,y=frac,col=pin-pout))+
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
