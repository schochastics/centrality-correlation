library(tidyverse)
library(igraph)
library(patchwork)

fl <- list.files("data",pattern="rewire",full.names = TRUE)
df <- map_dfr(fl,readRDS)
tbl <- df |>  
  group_by(combo,rewire,n) |> 
  dplyr::summarise(frac=mean(disc)/mean(choose(n,2)),
                   diameter=mean(diameter),
                   density=mean(dens),
                   # spec_gap=mean((ev1-ev2)/n),
                   spec_gap=mean((ev2/ev1)),
                   mgap=mean(mgap),
                   cpcor=mean(cpcor1),
                   .groups = "drop") |> 
  dplyr::filter(!str_detect(combo,"subgraph")) |> 
  separate(combo,into = c("a","b"),sep = "-")

tbl1 <- tbl |> rename(b=1,a=2)  |> bind_rows(tbl)
tbl1 <- tbl1 |> mutate(a=factor(a,levels=c("degree","betweenness","closeness","eigenvector")),
                        b=factor(b,levels=c("degree","betweenness","closeness","eigenvector")))
ggplot(tbl1)+
  geom_line(aes(x=rewire,y=frac,color=as.factor(n)))+
  facet_grid(a~b)+
  scale_color_manual(values=c("#A65628","#FFCE4E","#F9791E","#C6242D","#9F248F","#017A4A","#244579"),
                     name="number of nodes")+
  scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1),labels = c("0","0.25","0.5","0.75","1"))+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=14,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=12,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(x="rewiring probability",y="fraction of discordant pairs")+
  guides(color=guide_legend(nrow=1,override.aes = list(size=2)))

ggsave("figures/rewire_sim.pdf",width = 11,height=6)
system("cp figures/rewire_sim.pdf ~/Dropbox/schofie/centrality_correlation/figures/")
  
p1 <- tbl |> 
  distinct(rewire,n,.keep_all = TRUE) |> 
  ggplot(aes(x=rewire,y=mgap,col=as.factor(n)))+geom_line()+
  scale_color_manual(values=c("#A65628","#FFCE4E","#F9791E","#C6242D","#9F248F","#017A4A","#244579"),
                     name="number of nodes")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18,family="serif"),
        axis.text = element_text(size=14,family="serif"),
        legend.title = element_text(size=18,family="serif"),
        legend.text = element_text(size=14,family="serif"),
        strip.text =  element_text(size=14,family="serif"))+
  guides(color=guide_legend(nrow=2,byrow = TRUE,override.aes = list(size=2)))+
  labs(x="rewiring probability",y = "majorization gap")

p2 <- tbl |> 
  distinct(rewire,n,.keep_all = TRUE) |> 
  ggplot(aes(x=rewire,y=spec_gap,col=as.factor(n)))+geom_line()+
  scale_color_manual(values=c("#A65628","#FFCE4E","#F9791E","#C6242D","#9F248F","#017A4A","#244579"),
                     name="number of nodes")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18,family="serif"),
        axis.text = element_text(size=14,family="serif"),
        legend.title = element_text(size=18,family="serif"),
        legend.text = element_text(size=14,family="serif"),
        strip.text =  element_text(size=14,family="serif"))+
  guides(color=guide_legend(nrow=2,byrow = TRUE,override.aes = list(size=2)))+
  labs(x="rewiring probability",y = "spectral gap")

p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave("figures/rewire_mgap.pdf",p1,width = 8,height=8)
system("cp figures/rewire_mgap.pdf ~/Dropbox/schofie/centrality_correlation/figures/")

tbl |> 
  mutate(mgapf=cut(mgap,breaks=50)) |>
  mutate(mgapf=as.numeric(mgapf)/50) |> 
  group_by(mgapf) |> 
  summarize(mdisc=mean(frac),vardisc=sd(frac)) |> 
  ggplot(aes(mgapf,mdisc))+geom_smooth(col="black")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18,family="serif"),
        axis.text = element_text(size=14,family="serif"),
        legend.title = element_text(size=18,family="serif"),
        legend.text = element_text(size=14,family="serif"),
        strip.text =  element_text(size=14,family="serif"))+
  guides(color=guide_legend(nrow=1,override.aes = list(size=2)))+
  labs(x="majorization gap",y = "average fraction of discordant pairs")

ggsave("figures/rewire_mgap_mean.pdf",width = 11,height=6)
system("cp figures/rewire_mgap_mean.pdf ~/Dropbox/schofie/centrality_correlation/figures/")

df %>%
  mutate(frac = disc/choose(n,2),mgap2 = mgap^2) %>% 
  split(.$combo) %>%
  map(~ lm(frac ~ mgap, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")  %>% 
  mean()


df %>%
  mutate(frac = disc/choose(n,2),mgap2 = mgap^2) %>% 
  split(.$combo) %>%
  map(~ lm(frac ~ mgap+mgap2, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")  %>% 
  mean()

df %>%
  mutate(frac = disc/choose(n,2),spec_gap=ev2/ev1) %>% 
  split(.$combo) %>%
  map(~ lm(frac ~ spec_gap, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")  %>% 
  mean()
