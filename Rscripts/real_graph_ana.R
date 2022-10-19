library(patchwork)
library(tidyverse)
df <- read_csv("data/real_sim.csv",show_col_types = FALSE)
df <- df |> dplyr::filter(dens<1)
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
  ggplot(aes(x=1-spec_gap,y=disc/choose(n,2),group=combo,col=combo))+
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

p <- p1+p2+p3+
  p4+p5+p6+
  p7+p8+p9+
  plot_annotation(tag_levels = "A",tag_suffix = ")")+
  plot_layout(nrow=3,byrow = TRUE,guides = "collect")  & theme(legend.position = 'bottom')

ggsave("figures/real_topo.pdf",p,width = 15,height = 15)
system("cp figures/real_topo.pdf ~/Dropbox/schofie/centrality_correlation/figures/")
