library(netrankr)
library(igraph)
library(ggraph)
library(tidyverse)
library(patchwork)
bseq <- c(0, 0, 1, 1, 0, 1, 0, 0, 0, 1)
tg <- threshold_graph(bseq = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1))
V(tg)$name <- LETTERS[1:10]

p1 <- ggraph(tg,"stress") + 
  geom_edge_link0(edge_width = 0.8) +
  geom_node_point(shape = 21,fill = "grey25",size = 10) + 
  geom_node_text(aes(label = name), color = "white",size = 6, family = "serif") + 
  theme_graph() +
  coord_fixed()
  
p2 <- tibble(
  label = V(tg)$name,
  name = label,
  betweenness = rank(betweenness(tg)),
  closeness = rank(closeness(tg))) |> 
  pivot_longer(betweenness:closeness,names_to = "type",values_to = "rank") |>
  mutate(label = ifelse(name=="A" & type == "betweenness","A,B,C,E,G,I",label)) |> 
  mutate(label = ifelse(name%in%c("B","C","E","G","I") & type == "betweenness","",label)) |>
  mutate(label = ifelse(name=="A" & type == "closeness","A,B",label)) |> 
  mutate(label = ifelse(name%in%c("B") & type == "closeness","",label)) |>
  mutate(label = ifelse(type == "closeness",paste0("  ",label),paste0(label,"  "))) |>
  ggplot() +
  geom_line(aes(x=type,y=rank,group = name))+
  geom_point(aes(x=type,y=rank),size=2)+
  geom_text(aes(x=type,y=rank,label=label,hjust=ifelse(type=="betweenness",1,0)),family="serif",size=6)+
  theme_minimal()+
  scale_x_discrete(position = "top")+
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=16,family="serif"),
        axis.title = element_blank(),
        plot.title = element_text(hjust=0.5))+
  coord_cartesian(clip = "off")

p1 + p2
ggsave("figures/rightties_example.pdf",width = 11,height=6)
system("cp figures/rightties_example.pdf ~/Dropbox/schofie/centrality_correlation/figures/")
