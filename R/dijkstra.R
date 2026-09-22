#'
#' dijkstra
#' 
#' @param graph A data frame with 3 varibles.
#' @param init_node A numeric value.
#'
#' @return The shortest path from inital node to every other node in the graph.
#'
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm}
#' @export 
dijkstra <- function(graph, init_node){
  if(!is.data.frame(graph) || ncol(graph) != 3){
    stop("The graph argument is formulated incorrectly!")
  }
  if(!is.numeric(init_node) || !(init_node %in% graph$v1)){
    stop("The init_node is formulated incorrectly!")
  }
  
  nodes <- unique(graph$v1)
  number_nodes <- length(unique(graph$v1))
  
  distance <- rep(Inf, number_nodes)
  distance[init_node] <- 0
  
  #previous <- rep(NA, number_nodes)
  
  unvisited <- rep(TRUE, number_nodes)
  
  for (x in 1:number_nodes){
    u <- which(unvisited)[which.min(distance[unvisited])]
    unvisited[u] <- FALSE
    
    edges_u <- graph[graph$v1 == u , ]
    
    if (nrow(edges_u) > 0) {
      for (j in 1:nrow(edges_u)) {
        edge <- edges_u[j, ]
        v <- edge$v2 
        
        alt <- distance[u] + edge$w
        if (alt < distance[v]) {
          distance[v] <- alt
          #previous[v] <- u
        }
      }
    }
  }
  return (distance)
}




wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
           v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
           w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki_graph, 1)
dijkstra(wiki_graph, 3)










