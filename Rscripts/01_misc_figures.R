library(netrankr)
library(igraph)
library(ggraph)
library(tidyverse)
library(patchwork)

# Figure 2 ----
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
  scale_y_continuous(position = "right",labels = c("lowest","highest"),breaks = c(1,10))+
  theme(panel.grid = element_blank(),
        axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),type = "closed")),
        axis.text.x = element_text(size=16,family="serif"),
        axis.title = element_blank(),
        plot.title = element_text(hjust=0.5))+
  coord_cartesian(clip = "off")

p1 + p2
ggsave("figures/rightties_example.pdf",width = 11,height=6)

# Figure 1 ----
core_graph <- threshold_graph(20,0.3)
B <- as_adj(core_graph,sparse=FALSE)
deg <- rowSums(B)
B <- B[order(deg,decreasing = TRUE),order(deg,decreasing = TRUE)]
core <- sum(core_graph$sequence)+1
B[1:core,1:core] <- B[1:core,1:core]*2
seriation::ggpimage(B)+
  scale_fill_gradient(high="grey25",low="white")+
  theme(legend.position="none",plot.background = element_rect(fill="white",color="black"))

ggsave("figures/nested_mat.pdf",width=6,height=6)


