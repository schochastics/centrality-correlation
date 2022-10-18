library(igraph)
library(netrankr)
library(ggraph)

source("Rscripts/functions.R")



A <- as_adj(sample_gnp(10,0.75),sparse = FALSE)
n <- nrow(A)
c1 <- betweenness
c2 <- eigen_centrality2
# c3 <- eigen_centrality2
tst <- optim(par = c(A), fn = max_disc, n=n,c1=c1,c2=c2,gr = gennet,method = "SANN",
                control = list(trace = TRUE))#maxit = iter, temp = temp, tmax = tmax, 


Am <- matrix(tst$par,n,n)
g <- graph_from_adjacency_matrix(Am,"undirected")
g
plot(g,vertex.size=0.5,vertex.label=NA)
cor(c1(g),c2(g),method = "kendall")
plot(c1(g),c2(g))

ggraph(g,"stress")+
  geom_edge_link0(edge_width=0.2)+
  geom_node_point(shape=21,size=4,fill="grey25")+
  theme_graph()+
  coord_equal()




all <- all_indices(g)
