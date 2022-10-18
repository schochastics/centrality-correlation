rand_mst <- function(n){
  g <- igraph::graph.empty(n,directed = FALSE)
  discovered <- rep(FALSE,n)
  idx <- sample(seq_len(n),1)
  discovered[idx] <- TRUE
  while(igraph::ecount(g)!=(n-1)){
    # cat(ecount(g),"\r")
    target <- sample(seq_len(n),1)
    if(discovered[target]){
      idx <- target
    } else{
      g <- add.edges(g,c(idx,target))
      discovered[target] <- TRUE
      idx <- target
    }
  }
  g
}

sample_gnp2 <- function(n,p){
  g1 <- rand_mst(n)
  g2 <- sample_gnp(n,p,directed = FALSE,loops = FALSE)
  del <- sample(1:ecount(g2),n-1)
  g2 <- delete.edges(g2,del)
  g1%u%g2
}

eigen_centrality2 <- function(g){
  tryCatch(eigen_centrality(g,weights = NA)$vector,error=function(e) rep(0,vcount(g)))
}

subgraph_centrality2 <- function(g){
  tryCatch(subgraph_centrality(g),error=function(e) rep(0,vcount(g)))
}

closeness2 <- function(g){
  g |> 
    indirect_relations(type="dist_sp",FUN=dist_inv) |>
    aggregate_positions(type="sum")
}

communicability <- function(g){
  g |> 
    indirect_relations(type="walks",FUN=walks_exp) |>
    aggregate_positions(type="sum")
}

rwcc <- function(g){
  c <- tryCatch(g |> indirect_relations(type="dist_rwalk") |> aggregate_positions(),error=function(e) rep(1,vcount(g)))
  -1*c
}

ic <- function(g){
  tryCatch(sna::infocent(get.adjacency(g,sparse=F)),error=function(e) rep(1,vcount(g)))
}

katz <- function(g){
  centiserve::katzcent(g)
}

max_disc <- function(A,n,c1,c2){
  Am <- matrix(A,n,n)
  g <- graph_from_adjacency_matrix(Am,"undirected")
  -compare_ranks(round(c1(g),8),round(c2(g),8))$discordant
}
# 
# max_disc2 <- function(A,n,c1,c2,c3){
#   Am <- matrix(A,n,n)
#   g <- graph_from_adjacency_matrix(Am,"undirected")
#   -(compare_ranks(round(c1(g),8),round(c2(g),8))$discordant * 
#     compare_ranks(round(c1(g),8),round(c3(g),8))$discordant * 
#     compare_ranks(round(c3(g),8),round(c2(g),8))$discordant)
# }

gennet <- function(A,n,c1,c2){
  Am <- matrix(A,n,n)
  p <- runif(1)
  if(p<(1/3)){
    e <- which(Am==1,arr.ind = TRUE)
    if(length(e)<2){
      return(c(Am))
    }
    del <- e[sample(1:nrow(e),1),]
    Am[del[1],del[2]] <- Am[del[2],del[1]] <- 0
  } else if(p<(2/3)){
    A1 <- 1-Am-diag(1,nrow(Am))
    e <- which(A1==1,arr.ind = TRUE)
    if(length(e)<2){
      return(c(Am))
    }
    add <- e[sample(1:nrow(e),1),]
    Am[add[1],add[2]] <- Am[add[2],add[1]] <- 1
  } else{
    e <- which(Am==1,arr.ind = TRUE)
    if(length(e)<2){
      return(c(Am))
    }
    del <- e[sample(1:nrow(e),1),]
    
    A1 <- 1-Am-diag(1,nrow(Am))
    e <- which(A1==1,arr.ind = TRUE)
    if(length(e)<2){
      return(c(Am))
    }
    add <- e[sample(1:nrow(e),1),]
    
    Am[del[1],del[2]] <- Am[del[2],del[1]] <- 0
    Am[add[1],add[2]] <- Am[add[2],add[1]] <- 1
  }
  c(Am)
}

cent_df <- function(g){
  data.frame(
    degree = degree(g),
    betweenness = betweenness(g,weights = NA),
    closeness = closeness(g,weights = NA),
    eigenvector = round(eigen_centrality2(g),8),
    subgraph = round(subgraph_centrality(g),8)#,
    # communicability = round(communicability(g),8)
  )
}

cent_df_no_sc <- function(g){
  data.frame(
    degree = degree(g),
    betweenness = betweenness(g),
    closeness = closeness(g),
    eigenvector = round(eigen_centrality2(g),8)
  )
}

cent_disc <- function(g){
  df <- cent_df(g)
  res <- data.frame(combo=NA,disc=rep(NA,choose(ncol(df),2)))
  k <- 0
  for(i in 1:(ncol(df)-1)){
    for(j in (i+1):ncol(df)){
      k <- k+1
      res$combo[k] <- paste0(names(df)[i],"-",names(df)[j])
      res$disc[k] <- compare_ranks(df[[i]],df[[j]])$discordant
    }
  }
  res
}

cent_disc_no_sc <- function(g){
  df <- cent_df_no_sc(g)
  res <- data.frame(combo=NA,disc=rep(NA,choose(ncol(df),2)))
  k <- 0
  for(i in 1:(ncol(df)-1)){
    for(j in (i+1):ncol(df)){
      k <- k+1
      res$combo[k] <- paste0(names(df)[i],"-",names(df)[j])
      res$disc[k] <- compare_ranks(df[[i]],df[[j]])$discordant
    }
  }
  res
}


spectral_gap <- function(g){
  M <- as_adj(g, sparse = TRUE)
  f <- function(x,extra = NULL){
    as.vector(M%*%x)
  }
  fvec <- arpack(f,sym = TRUE,options=list(n = vcount(g),nev = 2,ncv = 8, 
                                           which = "LA",maxiter = 2000))
  
  (fvec$values[1]-fvec$values[2])/fvec$values[1]
}

largest_ev <- function(g){
  M <- as_adj(g, sparse = TRUE)
  f <- function(x,extra = NULL){
    as.vector(M%*%x)
  }
  fvec <- arpack(f,sym = TRUE,options=list(n = vcount(g),nev = 2,ncv = 8, 
                                           which = "LA",maxiter = 2000))
  
  c(fvec$values[1],fvec$values[2])
}

all_indices <- function(g){
  res <- matrix(0,vcount(g),35)
  res[,1] <- igraph::degree(g)
  res[,2] <- igraph::betweenness(g)
  res[,3] <- igraph::closeness(g)
  res[,4] <- igraph::eigen_centrality(g)$vector
  res[,5] <- 1/igraph::eccentricity(g)
  res[,6] <- igraph::subgraph_centrality(g)
  
  A <- get.adjacency(g,sparse=F)
  res[,7] <- sna::flowbet(A)
  res[,8] <- sna::loadcent(A)
  res[,9] <- sna::gilschmidt(A)
  res[,10] <- sna::infocent(A)
  res[,11] <- sna::stresscent(A)
  
  res[,12] <- 1/centiserve::averagedis(g)
  res[,13] <- centiserve::barycenter(g)
  res[,14] <- centiserve::closeness.currentflow(g)
  res[,15] <- centiserve::closeness.latora(g)
  res[,16] <- centiserve::closeness.residual(g)
  res[,17] <- centiserve::communibet(g)
  res[,18] <- centiserve::crossclique(g)
  res[,19] <- centiserve::decay(g)
  res[,20] <- centiserve::diffusion.degree(g)     
  res[,21] <- 1/centiserve::entropy(g)
  res[,22] <- centiserve::geokpath(g)
  res[,23] <- centiserve::katzcent(g)             
  res[,24] <- centiserve::laplacian(g)
  res[,25] <- centiserve::leverage(g)             
  res[,26] <- centiserve::lincent(g)
  res[,27] <- centiserve::lobby(g)
  res[,28] <- centiserve::markovcent(g)           
  res[,29] <- centiserve::mnc(g)
  res[,30] <- centiserve::radiality(g)            
  res[,31] <- centiserve::semilocal(g)
  res[,32] <- 1/centiserve::topocoefficient(g) 
  
  res[,33] <- CINNA::dangalchev_closeness_centrality(g)
  res[,34] <- CINNA::harmonic_centrality(g)
  res[,35] <- 1/CINNA::local_bridging_centrality(g)
  apply(res,2,function(x) round(x,8))
}


weighted.tau = function(v0,v1){
  require(rJava)
  # start the virtual machine
  .jinit()
  .jclassLoader()$setDebug(1L)
  # add all the jar files
  .jaddClassPath(list.files("/home/david/Documents/projects/centrality/correlation/law-2.3", full.names=T))#/LawLibrary/libs
  .jaddClassPath("/home/david/Documents/projects/centrality/correlation/law-2.3/law-2.1.jar")
  #LawLibrary/libs
  
  # create a class
  tau <- .jnew("it/unimi/dsi/law/stat/WeightedTau",check=FALSE)
  
  # call a method on it, needs exact signature
  #v0 <- c(1,2,2) # double[]
  #v1 <- c(1,2,3) # double[]
  rank <- as.integer(seq(1,length(v0))) 
  r <- .jcall(tau, "D", "compute",v0, v1, rank)
  return(r)
}
