#' Valence-based layout
#' @inheritParams layout_layered
#' @param repulsion_coef Coefficient for global repulsion force.
#'   Default is 1.
#' @param attraction_coef Coefficient for edge-based attraction/repulsion force.
#'   Default is 0.05.
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C", "D"),
#'   to   = c("B", "C", "D", "A"),
#'   weight = c(2, 3, 1, 4),
#'   sign = c(1, -1, 1, -1)  # 1 = positive, -1 = negative
#'   )
#' graphr(as_igraph(edges), layout="valence")
#' @export
layout_valence <- function(.data, times = 500, center = NULL, circular = FALSE, 
                           repulsion_coef = 1, attraction_coef = 0.05) {
  
  graph <- manynet::as_tidygraph(.data)
  n <- manynet::net_nodes(graph)
  
  coords <- matrix(stats::runif(n * 2, min = -1, max = 1), ncol = 2)
  
  for (i in 1:times) {
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
  coords <- as.data.frame(coords)
  names(coords) <- c("x", "y")
  return(coords)
}

#' @rdname layout_valence
#' @export
layout_tbl_graph_valence <- layout_valence