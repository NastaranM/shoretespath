rm(list= ls())
dijkstra <- function(graph, init_node){
  #variable initialization
  v1<- graph[,1]
  v2<-graph[,2]
  w<- graph[,3]
  nodes<-unique(v1)
  stopifnot(init_node %in% nodes)
  stopifnot(is.numeric(init_node))
  stopifnot(is.data.frame(graph))
  stopifnot(length(graph) == 3)
  
  
  dist <- matrix(data = Inf, nrow = 1, ncol = length(unique(v1)))
  dist[init_node] <- 0
  unchecked <- setdiff(nodes,init_node) 
  checked <- init_node
  while(any(is.infinite(dist))){
    selected <- graph[v1 %in% checked & v2 %in% unchecked,] #checking the neighbours of the node that we are checking
    minWeight <- which.min(selected$w) #the index
    minselected <- selected[minWeight,3] # cost
    minNode <- selected[minWeight,2] #selected node with minimum distance
    
    checked <- c(checked, minNode)
    unchecked <- setdiff(unchecked, minNode)
    dist[minNode] <- minselected
    graph$w[which(v1==minNode)] <- graph$w[which(v1== minNode)] + minselected
  }
  return(dist)}