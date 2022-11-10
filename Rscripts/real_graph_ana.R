library(patchwork)
library(tidyverse)
library(netropy)
library(igraph)
library(ggraph)
df <- read_csv("data/real_sim.csv",show_col_types = FALSE)
df <- df |> dplyr::filter(dens<1)

# plot all correlations with topological features ----
p1 <- df |> 
  ggplot(aes(x=dens,y=disc/choose(n,2),group=combo,col=combo))+
  geom_point(alpha=0.1)+
  ggpubr::stat_cor(aes(label = ..r.label..),p.accuracy = 0.001, r.accuracy = 0.01)+
  geom_smooth(aes(col=combo),method="lm",se=FALSE)+
  scale_color_manual(values=c("#9F248FFF", "#FFCE4EFF", "#017A4AFF", "#F9791EFF", 
                              "#244579FF", "#C6242DFF"),name="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=16,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=16,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(x="density",y="fraction of discordant pairs")

p2 <- df |> 
  ggplot(aes(x=diameter,y=disc/choose(n,2),group=combo,col=combo))+
  geom_point(alpha=0.1)+
  ggpubr::stat_cor(aes(label = ..r.label..),p.accuracy = 0.001, r.accuracy = 0.01)+
  geom_smooth(aes(col=combo),method="lm",se=FALSE)+
  scale_color_manual(values=c("#9F248FFF", "#FFCE4EFF", "#017A4AFF", "#F9791EFF", 
                              "#244579FF", "#C6242DFF"),name="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=16,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=16,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(x="diameter",y="")

p3 <- df |> 
  ggplot(aes(x=spec_gap,y=disc/choose(n,2),group=combo,col=combo))+
  geom_point(alpha=0.1)+
  ggpubr::stat_cor(aes(label = ..r.label..),p.accuracy = 0.001, r.accuracy = 0.01)+
  geom_smooth(aes(col=combo),method="lm",se=FALSE)+
  scale_color_manual(values=c("#9F248FFF", "#FFCE4EFF", "#017A4AFF", "#F9791EFF", 
                              "#244579FF", "#C6242DFF"),name="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=16,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=16,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(x="spectral gap",y="")

p4 <- df |> 
  ggplot(aes(x=mgap,y=disc/choose(n,2),group=combo,col=combo))+
  geom_point(alpha=0.1)+
  ggpubr::stat_cor(aes(label = ..r.label..),p.accuracy = 0.001, r.accuracy = 0.01)+
  geom_smooth(aes(col=combo),method="lm",se=FALSE)+
  scale_color_manual(values=c("#9F248FFF", "#FFCE4EFF", "#017A4AFF", "#F9791EFF", 
                              "#244579FF", "#C6242DFF"),name="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=16,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=16,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(x="majorization gap",y="fraction of discordant pairs")

p5 <- df |> 
  ggplot(aes(x=modularity,y=disc/choose(n,2),group=combo,col=combo))+
  geom_point(alpha=0.1)+
  ggpubr::stat_cor(aes(label = ..r.label..),p.accuracy = 0.001, r.accuracy = 0.01)+
  geom_smooth(aes(col=combo),method="lm",se=FALSE)+
  scale_color_manual(values=c("#9F248FFF", "#FFCE4EFF", "#017A4AFF", "#F9791EFF", 
                              "#244579FF", "#C6242DFF"),name="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=16,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=16,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(x="modularity",y="")

p6 <- df |> 
  ggplot(aes(x=core,y=disc/choose(n,2),group=combo,col=combo))+
  geom_point(alpha=0.1)+
  ggpubr::stat_cor(aes(label = ..r.label..),p.accuracy = 0.001, r.accuracy = 0.01)+
  geom_smooth(aes(col=combo),method="lm",se=FALSE)+
  scale_color_manual(values=c("#9F248FFF", "#FFCE4EFF", "#017A4AFF", "#F9791EFF", 
                              "#244579FF", "#C6242DFF"),name="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=16,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=16,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(x="core-peripheriness",y="")

p7 <- df |> 
  ggplot(aes(x=assort,y=disc/choose(n,2),group=combo,col=combo))+
  geom_point(alpha=0.1)+
  ggpubr::stat_cor(aes(label = ..r.label..),p.accuracy = 0.001, r.accuracy = 0.01)+
  geom_smooth(aes(col=combo),method="lm",se=FALSE)+
  scale_color_manual(values=c("#9F248FFF", "#FFCE4EFF", "#017A4AFF", "#F9791EFF", 
                              "#244579FF", "#C6242DFF"),name="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=16,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=16,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(x="degree assortativity",y="fraction of discordant pairs")

p8 <- df |> 
  ggplot(aes(x=ccoef,y=disc/choose(n,2),group=combo,col=combo))+
  geom_point(alpha=0.1)+
  ggpubr::stat_cor(aes(label = ..r.label..),p.accuracy = 0.001, r.accuracy = 0.01)+
  geom_smooth(aes(col=combo),method="lm",se=FALSE)+
  scale_color_manual(values=c("#9F248FFF", "#FFCE4EFF", "#017A4AFF", "#F9791EFF", 
                              "#244579FF", "#C6242DFF"),name="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=16,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=16,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(x="clustering coefficient",y="")

p9 <- df |> 
  ggplot(aes(x=glob_eff,y=disc/choose(n,2),group=combo,col=combo))+
  geom_point(alpha=0.1)+
  ggpubr::stat_cor(aes(label = ..r.label..),p.accuracy = 0.001, r.accuracy = 0.01)+
  geom_smooth(aes(col=combo),method="lm",se=FALSE)+
  scale_color_manual(values=c("#9F248FFF", "#FFCE4EFF", "#017A4AFF", "#F9791EFF", 
                              "#244579FF", "#C6242DFF"),name="")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=16,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=16,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(x="global efficiency",y="")

# combined plot ----
p <- p1+p2+p3+
  p4+p5+p6+
  p7+p8+p9+
  plot_annotation(tag_levels = "A",tag_suffix = ")")+
  plot_layout(nrow=3,byrow = TRUE,guides = "collect")  & theme(legend.position = 'bottom')

ggsave("figures/real_topo.pdf",p,width = 15,height = 15)
system("cp figures/real_topo.pdf ~/Dropbox/schofie/centrality_correlation/figures/")

#histogram of all variables ----
cent <- df |> 
  mutate(frac=disc/choose(n,2)) |> 
  select(id,combo,frac) |> 
  pivot_wider(names_from = combo,values_from = frac)

cor_data <- df |>
  select(id,dens:glob_eff,-ev1,-ev2) |> 
  rename(density=dens,`spectral gap`=spec_gap,`majorization gap` = mgap,
         `core-peripheriness`=core,`degree assortativity`=assort,`clustering coefficient`=ccoef,
         `global efficiency` = glob_eff) |> 
  distinct() |> 
  left_join(cent,by="id")
cor_data |>   
  pivot_longer(cols = density:`closeness-eigenvector`) |> 
  mutate(name=factor(name,levels = c("density", "diameter", "spectral gap", "majorization gap", 
                                     "modularity", "core-peripheriness", "degree assortativity", "clustering coefficient", 
                                     "global efficiency", "degree-betweenness", "degree-closeness", 
                                     "degree-eigenvector", "betweenness-closeness", "betweenness-eigenvector", 
                                     "closeness-eigenvector"))) |> 
  group_by(name) |> 
  mutate(med=median(value)) |> 
  ungroup() |> 
  ggplot(aes(x=value))+
  geom_histogram(bins = 30)+
  geom_vline(aes(xintercept = med),col="#cc0000")+
  facet_wrap(name~.,scales = "free",nrow = 3)+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=16,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=16,family="serif"),
        strip.text =  element_text(size=16,family="serif"))+
  labs(x="",y="")

ggsave("figures/real_topo_dens.pdf",width = 15,height = 10)
system("cp figures/real_topo_dens.pdf ~/Dropbox/schofie/centrality_correlation/figures/")

# median dichotomization ----
cor_data_bin <- cor_data |> 
  select(-id) |> 
  mutate(across(.fns = function(x) (x>median(x))+0)) |> 
  as.data.frame()

# redundancy
redundancy(cor_data_bin, 2)

# joint entropy ----
J <- joint_entropy(cor_data_bin, 3)
cent_names <- c("degree-betweenness", "degree-closeness", 
"degree-eigenvector", "betweenness-closeness", "betweenness-eigenvector", 
"closeness-eigenvector")
t(J$matrix)[cent_names,!colnames(J$matrix)%in%cent_names] |> 
  knitr::kable(format="latex",booktabs=TRUE,digits=3)

# association graph ----
J <- joint_entropy(cor_data_bin, 2)
adj <- J$matrix
diag(adj) <- 0
adj[adj < 0.26] <- 0
ag <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
V(ag)$name <- str_replace_all(V(ag)$name,"-","\n-\n")
V(ag)$name <- str_replace_all(V(ag)$name," ","\n")
ggraph(ag, layout = "stress",bbox=10) + 
  geom_edge_link0(edge_colour = "grey40",aes(edge_width = weight),show.legend = FALSE) + 
  geom_node_point(shape = 21, size = 20, fill = "white", stroke = 1,color="grey66") + 
  geom_node_text(aes(label = V(ag)$name),size = 4.5,lineheight=0.65) + 
  scale_edge_width(range = c(0.5,2))+
  theme_graph() + 
  coord_cartesian(clip = "off")

ggsave("figures/real_depend_graph.pdf",width = 10,height = 6)
system("cp figures/real_depend_graph.pdf ~/Dropbox/schofie/centrality_correlation/figures/")

# heatmaps predict ----
pp_degree.betweenness <- prediction_power('degree-betweenness', cor_data_bin)    
pp_degree.closeness <- prediction_power('degree-closeness', cor_data_bin)   
pp_degree.eigenvector  <- prediction_power('degree-eigenvector', cor_data_bin)
pp_betweenness.closeness  <- prediction_power('betweenness-closeness', cor_data_bin)
pp_betweenness.eigenvector  <- prediction_power('betweenness-eigenvector', cor_data_bin)
pp_closeness.eigenvector  <- prediction_power('closeness-eigenvector', cor_data_bin)

pp_lst <- list(
  "degree-betweenness" = pp_degree.betweenness,
  "degree-closeness" = pp_degree.closeness,
  "degree-eigenvector" = pp_degree.eigenvector,
  "betweenness-closeness" = pp_betweenness.closeness,
  "betweenness-eigenvector" = pp_betweenness.eigenvector,
  "closeness-eigenvector" = pp_closeness.eigenvector
)

pp_lst <- map(pp_lst,function(x){
  x[is.na(x)] <- 0
  x <- x+t(x)
  x[x==0] <- NA
  diag(x) <- NA #TODO: OK?
  x
})

mcol <- max(unlist(pp_lst),na.rm=TRUE)

keep <- c("density", "diameter", "spectral gap", "majorization gap", 
          "modularity", "core-peripheriness", "degree assortativity", "clustering coefficient", 
          "global efficiency")
pp_lst <- map(pp_lst,function(x) x[rownames(x)%in%keep,colnames(x)%in%keep])

p <- map(seq_along(pp_lst),function(i){
  df <- pp_lst[[i]]
  df <- df |> 
    as_tibble(rownames = "namex") |> 
    pivot_longer(cols = density:`global efficiency`,names_to = "namey") |> 
    mutate(namex=factor(namex,levels=c("density", "diameter", "spectral gap", "majorization gap", 
                                       "modularity", "core-peripheriness", "degree assortativity", "clustering coefficient", 
                                       "global efficiency"))) |> 
    mutate(namey=factor(namey,levels=c("density", "diameter", "spectral gap", "majorization gap", 
                                       "modularity", "core-peripheriness", "degree assortativity", "clustering coefficient", 
                                       "global efficiency")))
  ggplot(df,aes(namex,namey,fill=value))+
    geom_tile()+
    geom_text(aes(label=ifelse(is.na(value),"X","")),size=5)+
    scale_fill_gradient(limits = c(0,mcol),low = "black",
                        high = "white",na.value = "white",name="EH")+
    
    # scico::scale_fill_scico(palette = 'roma',na.value="grey66",limits=c(0,2),
    #                         name="expected\nconditional entropy")+ 
    coord_fixed()+
    theme_void()+
    theme(axis.text.x=element_text(size=12, angle=45, vjust=0.95, hjust=1,family="serif"),
          axis.text.y=element_text(size=12, hjust=0.95,family="serif"),
          legend.title = element_text(size=14,family="serif"),
          text = element_text(family="serif"),
          legend.position = "bottom")+
    labs(title = names(pp_lst)[i])+
    guides(fill=guide_colorbar(barwidth = 10,barheight = 0.5))
})

wrap_plots(p)+
  plot_layout(nrow=2,byrow = TRUE,guides = "collect")  & theme(legend.position = 'bottom')

ggsave("figures/real_exp_con_ent.pdf",width=15,height=10)
system("cp figures/real_exp_con_ent.pdf ~/Dropbox/schofie/centrality_correlation/figures/")
