#' Valence layout
#'
#' @description
#'   The "valence" layout places the nodes of a signed network so that
#'   positively tied nodes are drawn together and negatively tied nodes apart.
#' @name layout_valence
#' @template param_ggraphlayouts
#' @param center Required for `{ggraph}` compatibility, and not used here.
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
#' @family mapping
#' @export
layout_valence <- function(.data, times = 500, center = NULL, circular = FALSE, 
                           repulsion_coef = 1, attraction_coef = 0.05) {
  
  graph <- manynet::as_tidygraph(.data)
  n <- manynet::net_nodes(graph)
  # A sign is read through manynet rather than from a "sign" tie attribute,
  # since manynet 2.3.0 records the sign of a tie in its weight instead. A tie
  # with no sign attracts as a positive tie does, and a network with no weights
  # weighs every tie the same.
  signs <- if (manynet::is_signed(graph))
    as.numeric(manynet::tie_signs(graph)) else
      rep(1, manynet::net_ties(graph))
  signs[is.na(signs)] <- 1
  # The magnitude of the weight, since manynet 2.3.0 carries the sign in the
  # weight itself; multiplying a negative weight by a negative sign would make
  # a negative tie attract.
  weights <- if (manynet::is_weighted(graph))
    abs(as.numeric(manynet::tie_attribute(graph, "weight"))) else
      rep(1, manynet::net_ties(graph))
  weights[is.na(weights)] <- 1
  
  # The nodes start on a circle rather than at random points, so that the
  # layout is stable from one run to the next. A random start can place two
  # nodes on top of each other, which the repulsion force then throws apart.
  # A small jitter breaks the symmetry of the circle.
  angle <- 2 * pi * seq_len(n) / n
  coords <- cbind(cos(angle), sin(angle)) +
    matrix(stats::runif(n * 2, min = -0.01, max = 0.01), ncol = 2)
  
  for (i in 1:times) {
    delta <- matrix(0, nrow = n, ncol = 2)
    
    # Global repulsion (Coulomb-style)
    for (j in 1:(n-1)) {
      for (k in (j+1):n) {
        vec <- coords[k, ] - coords[j, ]
        dist <- sqrt(sum(vec^2)) + 1e-4
        dir <- vec / dist
        # The distance is floored, since two nodes that start close together
        # would otherwise repel each other with a force large enough to throw
        # the whole layout apart, which it never recovers from.
        force <- repulsion_coef / max(dist, 0.1)^2
        
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
      force <- attraction_coef * weights[e] * signs[e]
      
      delta[s_id, ] <- delta[s_id, ] + force * dir
      delta[t_id, ] <- delta[t_id, ] - force * dir
    }
    
    # Position update with damping, a capped step, and cooling.
    # The cap keeps one large force from moving a node further than the layout
    # is wide, and the cooling lets the layout settle over the iterations.
    step <- sqrt(rowSums(delta^2))
    limit <- ifelse(step > 1, 1 / step, 1)
    temp <- 1 - (i - 1) / times
    coords <- coords + 0.1 * temp * delta * limit
  }
  coords <- as.data.frame(coords)
  names(coords) <- c("x", "y")
  return(coords)
}

#' @rdname layout_valence
#' @export
layout_tbl_graph_valence <- layout_valence