library(igraph)
library(netrankr)
library(ggraph)
library(tidyverse)

fl <- list.files("data",pattern = "uncor_sim",full.names = TRUE)
fl <- fl[!str_detect(fl,"sc")]
tbl <- map_dfr(fl,function(x){
  df <- readRDS(x)
  df$type <- str_extract(x,"(?<=_)(.*?)(?=\\.)") 
  df$frac <- df$disc/choose(c(10,15,25,50,100),2)
  df
})

# ggplot(tbl,aes(x=N,y=frac,color=type))+geom_point()+facet_wrap(~type)
# tbl %>% group_by(type) %>% summarise(m=mean(frac)) %>% arrange(-m)
# tbl %>% arrange(-frac) %>% print(n=40)

tbl_plot <- tbl %>% 
  dplyr::filter(N==50) %>% 
  mutate(a=str_remove(type,"sim_") %>% str_sub(1,2),
         b=str_remove(type,"sim_") %>% str_sub(3,4)) %>% 
  mutate(a=factor(a,levels=c("dc","bc","cc","ec")),
         b=factor(b,levels=c("dc","bc","cc","ec"))) %>% 
  arrange(a,b)

tbl_plot$plot <- lapply(tbl_plot$graph,function(x){
  ggraph(x,"stress")+
    geom_edge_link0(edge_color="grey66",edge_width=0.2)+
    geom_node_point(shape=21,fill="grey25",size=2)+
    theme_void()
})

annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

annot_disc <- vector("list",10)
for(i in 1:nrow(tbl_plot)){
  annot_disc[[i]] <- annotation_custom2(grob=ggplotGrob(
    ggplot()+
    theme_void()+
    annotate("text",x=0,y=0,
             label=round(tbl_plot$frac[i],2),size=12,family="serif")), 
                     data = data.frame(b=tbl_plot$a[i],a=tbl_plot$b[i]),
                     ymin = -1.6, ymax=1.6, xmin=-1.6, xmax=1.6)
  
}

annot_graph <- vector("list",10)
for(i in 1:nrow(tbl_plot)){
  annot_graph[[i]] <- annotation_custom2(grob=ggplotGrob(tbl_plot$plot[[i]]), 
                                       data = data.frame(a=tbl_plot$a[i],b=tbl_plot$b[i]),
                                       ymin = -1.4, ymax=1.4, xmin=-1.4, xmax=1.4)
  
}


indices <- c(
  dc = "degree",
  bc = "betweenness",
  cc = "closeness",
  ec = "eigenvector")

ggplot(tbl_plot)+
  scale_x_continuous(limits = c(-1.6,1.6))+
  scale_y_continuous(limits = c(-1.6,1.6))+
  facet_grid(a~b,drop = FALSE,labeller = labeller(a=indices,b=indices))+
  annot_graph+
  annot_disc+
  theme_void()+
  theme(panel.background = element_rect(fill=NA,colour="black"),
        strip.text.y = element_text(angle = -90,vjust=1),
        strip.text.x = element_text(vjust=1),
        strip.text =  element_text(size=12,family="serif"))

ggsave("figures/uncor_sim.pdf",width = 11,height=6)
system("cp figures/uncor_sim.pdf ~/Dropbox/schofie/centrality_correlation/figures/")  


sort(sapply(tbl_plot$graph,graph.density))


