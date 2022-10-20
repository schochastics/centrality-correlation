suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(netrankr))
suppressPackageStartupMessages(library(tibble))
source("Rscripts/functions.R")
N <- c(10,15,25,50,100)
# p <- c(0.3,0.2,0.15)
#DC<->BC
cat("-DC vs BC\n")
df <- tibble(N,disc=NA,graph=NA)
for(n in N){
  cat(n,"\r")
  cmin <- 10
  for(r in 1:20){
    A <- as_adj(sample_gnp(n,runif(1,0.02,0.2)),sparse = FALSE)
    c1 <- degree
    c2 <- betweenness
    res <- optim(par = c(A), fn = max_disc, n=n,c1=c1,c2=c2,gr = gennet,method = "SANN",
                 control = list(trace = FALSE))
    if(res$value<cmin){
      cmin <- res$value
      Am <- matrix(res$par,n,n)
      g <- graph_from_adjacency_matrix(Am,"undirected")
      gmin <- g
    }
  }
  df$disc[df$N==n] <- abs(cmin)
  df$graph[df$N==n] <- list(gmin)
}
saveRDS(df,"data/uncor_sim_dcbc.RDS")

cat("-DC vs CC\n")
df <- tibble(N,disc=NA,graph=NA)
for(n in N){
  cat(n,"\r")
  cmin <- 10
  for(r in 1:20){
    A <- as_adj(sample_gnp(n,runif(1,0.1,0.5)),sparse = FALSE)
    c1 <- degree
    c2 <- closeness2
    res <- optim(par = c(A), fn = max_disc, n=n,c1=c1,c2=c2,gr = gennet,method = "SANN",
                 control = list(trace = FALSE))
    if(res$value<cmin){
      cmin <- res$value
      Am <- matrix(res$par,n,n)
      g <- graph_from_adjacency_matrix(Am,"undirected")
      gmin <- g
    }
  }
  df$disc[df$N==n] <- abs(cmin)
  df$graph[df$N==n] <- list(gmin)
}
saveRDS(df,"data/uncor_sim_dccc.RDS")

# DC<->EC
cat("-DC vs EC\n")
df <- tibble(N,disc=NA,graph=NA)
for(n in N){
  cat(n,"\r")
  cmin <- 10
  for(r in 1:20){
    A <- as_adj(sample_gnp(n,runif(1,0.02,0.2)),sparse = FALSE)
    c1 <- degree
    c2 <- eigen_centrality2
    res <- optim(par = c(A), fn = max_disc, n=n,c1=c1,c2=c2,gr = gennet,method = "SANN",
                 control = list(trace = FALSE))
    if(res$value<cmin){
      cmin <- res$value
      Am <- matrix(res$par,n,n)
      g <- graph_from_adjacency_matrix(Am,"undirected")
      gmin <- g
    }
  }
  df$disc[df$N==n] <- abs(cmin)
  df$graph[df$N==n] <- list(gmin)
}
saveRDS(df,"data/uncor_sim_dcec.RDS")

#DC<->BC
cat("-BC vs CC\n")
df <- tibble(N,disc=NA,graph=NA)
for(n in N){
  cat(n,"\r")
  cmin <- 10
  for(r in 1:20){
    A <- as_adj(sample_gnp(n,runif(1,0.02,0.2)),sparse = FALSE)
    c1 <- betweenness
    c2 <- closeness2
    res <- optim(par = c(A), fn = max_disc, n=n,c1=c1,c2=c2,gr = gennet,method = "SANN",
                 control = list(trace = FALSE))
    if(res$value<cmin){
      cmin <- res$value
      Am <- matrix(res$par,n,n)
      g <- graph_from_adjacency_matrix(Am,"undirected")
      gmin <- g
    }
  }
  df$disc[df$N==n] <- abs(cmin)
  df$graph[df$N==n] <- list(gmin)
}
saveRDS(df,"data/uncor_sim_bccc.RDS")

#DC<->BC
cat("-BC vs EC\n")
df <- tibble(N,disc=NA,graph=NA)
for(n in N){
  cat(n,"\r")
  cmin <- 10
  for(r in 1:20){
    A <- as_adj(sample_gnp(n,runif(1,0.02,0.2)),sparse = FALSE)
    c1 <- betweenness
    c2 <- eigen_centrality2
    res <- optim(par = c(A), fn = max_disc, n=n,c1=c1,c2=c2,gr = gennet,method = "SANN",
                 control = list(trace = FALSE))
    if(res$value<cmin){
      cmin <- res$value
      Am <- matrix(res$par,n,n)
      g <- graph_from_adjacency_matrix(Am,"undirected")
      gmin <- g
    }
  }
  df$disc[df$N==n] <- abs(cmin)
  df$graph[df$N==n] <- list(gmin)
}
saveRDS(df,"data/uncor_sim_bcec.RDS")

#CC<->BC
cat("-CC vs EC\n")
df <- tibble(N,disc=NA,graph=NA)
for(n in N){
  cat(n,"\r")
  cmin <- 10
  for(r in 1:20){
    A <- as_adj(sample_gnp(n,runif(1,0.02,0.2)),sparse = FALSE)
    c1 <- closeness2
    c2 <- eigen_centrality2
    res <- optim(par = c(A), fn = max_disc, n=n,c1=c1,c2=c2,gr = gennet,method = "SANN",
                 control = list(trace = FALSE))
    if(res$value<cmin){
      cmin <- res$value
      Am <- matrix(res$par,n,n)
      g <- graph_from_adjacency_matrix(Am,"undirected")
      gmin <- g
    }
  }
  df$disc[df$N==n] <- abs(cmin)
  df$graph[df$N==n] <- list(gmin)
}
saveRDS(df,"data/uncor_sim_ccec.RDS")

# cat("-DC vs SC\n")
# df <- tibble(N,disc=NA,graph=NA)
# for(n in N){
#   cat(n,"\r")
#   cmin <- 10
#   for(r in 1:20){
#     A <- as_adj(sample_gnp(n,runif(1,0.02,0.2)),sparse = FALSE)
#     c1 <- degree
#     c2 <- subgraph_centrality
#     res <- optim(par = c(A), fn = max_disc, n=n,c1=c1,c2=c2,gr = gennet,method = "SANN",
#                  control = list(trace = FALSE))
#     if(res$value<cmin){
#       cmin <- res$value
#       Am <- matrix(res$par,n,n)
#       g <- graph_from_adjacency_matrix(Am,"undirected")
#       gmin <- g
#     }
#   }
#   df$disc[df$N==n] <- abs(cmin)
#   df$graph[df$N==n] <- list(gmin)
# }
# saveRDS(df,"data/uncor_sim_dcsc.RDS")
# 
# #DC<->BC
# cat("-BC vs SC\n")
# df <- tibble(N,disc=NA,graph=NA)
# for(n in N){
#   cat(n,"\r")
#   cmin <- 10
#   for(r in 1:20){
#     A <- as_adj(sample_gnp(n,runif(1,0.02,0.2)),sparse = FALSE)
#     c1 <- betweenness
#     c2 <- subgraph_centrality
#     res <- optim(par = c(A), fn = max_disc, n=n,c1=c1,c2=c2,gr = gennet,method = "SANN",
#                  control = list(trace = FALSE))
#     if(res$value<cmin){
#       cmin <- res$value
#       Am <- matrix(res$par,n,n)
#       g <- graph_from_adjacency_matrix(Am,"undirected")
#       gmin <- g
#     }
#   }
#   df$disc[df$N==n] <- abs(cmin)
#   df$graph[df$N==n] <- list(gmin)
# }
# saveRDS(df,"data/uncor_sim_bcsc.RDS")
# 
# #DC<->BC
# cat("-CC vs SC\n")
# df <- tibble(N,disc=NA,graph=NA)
# for(n in N){
#   cat(n,"\r")
#   cmin <- 10
#   for(r in 1:20){
#     A <- as_adj(sample_gnp(n,runif(1,0.02,0.2)),sparse = FALSE)
#     c1 <- closeness
#     c2 <- subgraph_centrality2
#     res <- optim(par = c(A), fn = max_disc, n=n,c1=c1,c2=c2,gr = gennet,method = "SANN",
#                  control = list(trace = FALSE))  
#     if(res$value<cmin){
#       cmin <- res$value
#       Am <- matrix(res$par,n,n)
#       g <- graph_from_adjacency_matrix(Am,"undirected")
#       gmin <- g
#     }
#   }
#   df$disc[df$N==n] <- abs(cmin)
#   df$graph[df$N==n] <- list(gmin)
# }
# saveRDS(df,"data/uncor_sim_ccsc.RDS")
# 
# #CC<->BC
# cat("-SC vs EC\n")
# df <- tibble(N,disc=NA,graph=NA)
# for(n in N){
#   cat(n,"\r")
#   cmin <- 10
#   for(r in 1:20){
#     A <- as_adj(sample_gnp(n,runif(1,0.02,0.2)),sparse = FALSE)
#     c1 <- subgraph_centrality2
#     c2 <- eigen_centrality2
#     res <- optim(par = c(A), fn = max_disc, n=n,c1=c1,c2=c2,gr = gennet,method = "SANN",
#                  control = list(trace = FALSE))  
#     if(res$value<cmin){
#       cmin <- res$value
#       Am <- matrix(res$par,n,n)
#       g <- graph_from_adjacency_matrix(Am,"undirected")
#       gmin <- g
#     }
#   }
#   df$disc[df$N==n] <- abs(cmin)
#   df$graph[df$N==n] <- list(gmin)
# }
# saveRDS(df,"data/uncor_sim_scec.RDS")