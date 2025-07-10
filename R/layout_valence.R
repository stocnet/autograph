#' Valence-based layout
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C", "D"),
#'   to   = c("B", "C", "D", "A"),
#'   weight = c(2, 3, 1, 4),
#'   sign = c(1, -1, 1, -1)  # 1 = positive, -1 = negative
#'   )
#'  plot(as_igraph(edges), layout= layout_valence(edges))
#' @export
layout_valence <- function(.data, iter = 500, repulsion_coef = 1, attraction_coef = 0.05) {
  graph <- as_tidygraph(.data)
  n <- net_nodes(graph)
  
  coords <- matrix(runif(n * 2, min = -1, max = 1), ncol = 2)
  
  for (i in 1:iter) {
    delta <- matrix(0, nrow = n, ncol = 2)
    
    # Global repulsion (Coulomb-style)
    for (j in 1:(n-1)) {
      for (k in (j+1):n) {
        vec <- coords[k, ] - coords[j, ]
        dist <- sqrt(sum(vec^2)) + 1e-4
        dir <- vec / dist
        force <- repulsion_coef / dist^2
        
        delta[j, ] <- delta[j, ] - force * dir
        delta[k, ] <- delta[k, ] + force * dir
      }
    }
    
    # Edge-based signed attraction/repulsion
    for (e in igraph::E(graph)) {
      s <- igraph::ends(graph, e)[1]
      t <- igraph::ends(graph, e)[2]
      s_id <- which(igraph::V(graph)$name == s)
      t_id <- which(igraph::V(graph)$name == t)
      
      vec <- coords[t_id, ] - coords[s_id, ]
      dist <- sqrt(sum(vec^2)) + 1e-4
      dir <- vec / dist
      force <- attraction_coef * igraph::E(graph)$weight[e] * igraph::E(graph)$sign[e]
      
      delta[s_id, ] <- delta[s_id, ] + force * dir
      delta[t_id, ] <- delta[t_id, ] - force * dir
    }
    
    # Position update with damping
    coords <- coords + 0.1 * delta
  }
  
  return(coords)
}