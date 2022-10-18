library(tidyverse)
df <- read_csv("data/random_er_sim.csv",show_col_types = FALSE)

df |>  
  mutate(frac=disc/choose(n,2)) |> 
  mutate(combo=str_replace_all(combo,"-","\n-\n")) |> 
  mutate(combo=factor(combo,
                      levels=c("degree\n-\nbetweenness", "degree\n-\ncloseness", "degree\n-\neigenvector", 
                               "betweenness\n-\ncloseness", "betweenness\n-\neigenvector", "closeness\n-\neigenvector"
                      ))) |> 
  ggplot(aes(x=combo,y=frac,col=as.factor(n)))+
  ggbeeswarm::geom_quasirandom(size=0.5,alpha=0.8)+
  scale_color_manual(values=c("#A65628","#FFCE4E","#F9791E","#C6242D","#9F248F","#017A4A","#244579"),
                     name="number of nodes")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=14,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=12,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(x="",y="fraction of discordant pairs")+
  guides(color=guide_legend(nrow=2,byrow = TRUE,override.aes = list(size=2)))

ggsave("figures/random_sim.pdf",width = 9,height=6)
system("cp figures/random_sim.pdf ~/Dropbox/schofie/centrality_correlation/figures/")

tbl <- df |> 
  separate(combo,into = c("a","b"),sep = "-") 
  
tbl1 <- tbl |> rename(b=1,a=2)  |> bind_rows(tbl)
tbl1 <- tbl1 |> mutate(a=factor(a,levels=c("degree","betweenness","closeness","eigenvector")),
                       b=factor(b,levels=c("degree","betweenness","closeness","eigenvector")))

ggplot(tbl1,aes(x=disc/choose(n,2)))+
  stat_density(geom="line",aes(col=factor(n)))+
  # geom_density(aes(col=factor(n)))+
  facet_grid(a~b,scales = "free_y")+
  scale_x_continuous(limits=c(0,1))+
  scale_color_manual(values=c("#A65628","#FFCE4E","#F9791E","#C6242D","#9F248F","#017A4A","#244579"),
                     name="number of nodes")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=14,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=12,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(y="density",x="fraction of discordant pairs")+
  guides(color=guide_legend(nrow=1,override.aes = list(size=2)))

tbl1 |> group_by(n) |> 
  mutate(param=dense_rank(param)) |> 
  ungroup() |> 
  mutate(nx=case_when(n==100~"100",n==250~"250",n==500~"500",n==1000~"1k",n==2500~"2.5k",n==5000~"5k",n==10000~"10k")) |> 
  mutate(nx=factor(nx,levels = c("100","250","500","1k","2.5k","5k","10k"))) |> 
ggplot(aes(x=nx,y=disc/choose(n,2),col=factor(param)))+
  geom_boxplot(size=0.5)+
  # geom_density(aes(col=factor(n)))+
  facet_grid(a~b)+
  scale_color_manual(values = c("#0000CC","black","#CC0000"),name="density factor",
                     labels=expression(p[1],p[2],p[3]))+
  scale_y_continuous(limits = c(0,0.3))+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=14,family="serif"),
        axis.text = element_text(size=12,family="serif"),
        legend.title = element_text(size=14,family="serif"),
        legend.text = element_text(size=12,family="serif"),
        strip.text =  element_text(size=12,family="serif"))+
  labs(x="number of nodes",y="fraction of discordant pairs")

ggsave("figures/random_sim.pdf",width = 11,height=6)
system("cp figures/random_sim.pdf ~/Dropbox/schofie/centrality_correlation/figures/")



tbl1 |> group_by(n) |> mutate(param=dense_rank(param)) |> ungroup() |> 
  group_by(a,b,n,param) |> 
  summarise(val=ifelse(as.numeric(b)>as.numeric(a),mean(disc/choose(n,2)),NA),
            sd = ifelse(as.numeric(b)<as.numeric(a),sd(disc/choose(n,2)),NA)) |> 
  ggplot(aes(x=factor(n),y=param,fill=val)) +
  geom_tile()+ 
  geom_text(aes(label=round(sd,2)))+
  scale_fill_gradient(low="white",high="red",limits=c(0,0.5),na.value = "transparent")+
  facet_grid(a~b)



df %>% 
  # dplyr::filter(dens<0.05) |> 
  ggplot(aes(x=mgap,y=disc/choose(n,2)))+
  geom_point(size=0.2,alpha=0.7,aes(col=n))+
  scale_x_continuous(limits=c(0,1))+
  facet_wrap(~combo)

df %>% 
  # dplyr::filter(dens<0.1) |> 
  ggplot(aes(x=mgap*dens*choose(n,2)/n,y=disc/choose(n,2)))+
  geom_point(size=0.2,alpha=0.7,col="grey25")+
  # scale_x_continuous(limits=c(0,1))+
  facet_wrap(~combo)


df %>% 
  ggplot(aes(x=(ev1-ev2),y=disc/choose(n,2)))+
  geom_point(size=0.2,alpha=0.7,col="grey25",show.legend = F)+
  scale_color_viridis_c()+
  # scale_x_continuous(limits=c(0,1))+
  facet_wrap(~combo)

df |> 
  ggplot(aes(x=mgap*dens*choose(n,2)/n,y=ev1-ev2))+geom_point()

df %>% 
  ggplot(aes(x=(ev2)/ev1,y=disc/choose(n,2)))+
  geom_point(size=0.2,alpha=0.7)+
  # scale_x_continuous(limits=c(0,1))+
  facet_wrap(~combo)



df %>% 
  ggplot(aes(x=mgap,y=diameter))+
  geom_point(size=0.2,alpha=0.7,)

df %>% 
  ggplot(aes(x=dens,y=disc/choose(n,2)))+
  geom_point(size=0.2,alpha=0.7)+
  facet_wrap(~combo)

df %>% 
  ggplot(aes(y=ev1-ev2,x=mgap))+
  geom_point(size=0.2,alpha=0.7)


df %>% 
  group_by(id,type) %>% 
  summarise(spec_gap=mean(spec_gap),mgap=mean(mgap)) %>% 
  ggplot(aes(x=spec_gap,y=mgap))+geom_point(aes(col=type))+
  scale_x_continuous(limits=c(0,1))+
  scale_y_continuous(limits=c(0,1))

df %>%
  mutate(frac = disc/choose(n,2),mgap2 = mgap^2) %>% 
  split(.$combo) %>%
  map(~ lm(frac ~ mgap+mgap2, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")  %>% 
  mean()

df %>%
  mutate(frac = disc/choose(n,2)) %>% 
  split(.$combo) %>%
  map(~ lm(frac ~ dens, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")  %>% 
  mean()


# df %>% 
#   separate(combo,into = c("a","b"),sep = "-") %>% 
#   mutate(a=factor(a,levels=c("degree","betweenness","closeness","eigenvector")),
#          b=factor(b,levels=c("degree","betweenness","closeness","eigenvector"))) %>% 
#   mutate(frac=disc/choose(n,2)) %>% 
#   ggplot(aes(x=ev1-ev2,y=frac))+
#   geom_point(size=0.2,alpha=0.7)+
#   geom_smooth(method = "lm",se=FALSE,color="red",size=0.4)+
#   theme_minimal()+
#   theme(legend.position = "bottom",
#         panel.grid.minor = element_blank(),
#         axis.title = element_text(size=14,family="serif"),
#         axis.text = element_text(size=12,family="serif"),
#         legend.title = element_text(size=14,family="serif"),
#         legend.text = element_text(size=12,family="serif"),
#         strip.text =  element_text(size=12,family="serif"))+
#   labs(x="density",y="fraction of discordant pairs")+
#   facet_grid(a~b,drop = FALSE)