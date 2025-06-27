#' Layered layout
#' @inheritParams layout_tbl_graph_matching
#' @param center,circular Extra parameters required for `{tidygraph}`
#'   compatibility.
#' @param times Integer of sweeps that the algorithm will pass through.
#'   By default 4.
#' @returns Returns a table of coordinates.
#' @examples
#' ties <- data.frame(
#'   from = c("A", "A", "B", "C", "D", "F", "F", "E"),
#'   to   = c("B", "C", "D", "E", "E", "E", "G", "G"),
#'   stringsAsFactors = FALSE)
#' 
#' coords <- layout_tbl_graph_layered(ties, times = 6)
#' coords
#' @export
layout_tbl_graph_layered <- function(.data,
                                     center = NULL,
                                     circular = FALSE,
                                     times = 4) {
  ties <- manynet::as_edgelist(.data)
  nodes <- unique(c(ties$from, ties$to))
  node_idx <- setNames(seq_along(nodes), nodes)
  
  # Adjacency and reverse adjacency
  adj <- lapply(nodes, function(x) character(0))
  radj <- lapply(nodes, function(x) character(0))
  names(adj) <- names(radj) <- nodes
  
  for (i in seq_len(nrow(ties))) {
    from <- ties$from[i]
    to <- ties$to[i]
    adj[[from]] <- c(adj[[from]], to)
    radj[[to]] <- c(radj[[to]], from)
  }
  
  # Topological sort for layer assignment
  in_deg <- sapply(radj, length)
  queue <- names(in_deg[in_deg == 0])
  layer <- setNames(rep(NA, length(nodes)), nodes)
  current_layer <- 0
  
  while (length(queue) > 0) {
    next_queue <- character(0)
    for (v in queue) {
      layer[v] <- current_layer
      for (w in adj[[v]]) {
        in_deg[w] <- in_deg[w] - 1
        if (in_deg[w] == 0) {
          next_queue <- c(next_queue, w)
        }
      }
    }
    queue <- next_queue
    current_layer <- current_layer + 1
  }
  
  coords <- data.frame(name = names(layer), layer = layer, stringsAsFactors = FALSE)
  layer_map <- split(coords$name, coords$layer)
  
  # Initialize x positions
  x_pos <- lapply(layer_map, function(n) setNames(seq_along(n), n))
  
  # Sweep function
  barycenter_sort <- function(layer_nodes, neighbors_pos) {
    bc <- sapply(layer_nodes, function(n) {
      neighbors <- neighbors_pos[[n]]
      if (length(neighbors) == 0) return(Inf)
      mean(unlist(neighbors))
    })
    sorted <- layer_nodes[order(bc)]
    setNames(seq_along(sorted), sorted)
  }
  
  for (s in seq_len(times)) {
    # Forward sweep (top-down)
    for (l in 2:length(layer_map)) {
      prev <- x_pos[[l - 1]]
      cur <- layer_map[[as.character(l - 1 + 1)]]
      rev_adj <- lapply(cur, function(n) radj[[n]][radj[[n]] %in% names(prev)])
      x_pos[[l]] <- barycenter_sort(cur, setNames(rev_adj, cur))
    }
    # Backward sweep (bottom-up)
    for (l in (length(layer_map) - 1):1) {
      next_ <- x_pos[[l + 1]]
      cur <- layer_map[[as.character(l - 1 + 1)]]
      fwd_adj <- lapply(cur, function(n) adj[[n]][adj[[n]] %in% names(next_)])
      x_pos[[l]] <- barycenter_sort(cur, setNames(fwd_adj, cur))
    }
  }
  
  # Convert x_pos list into flat x-coordinates
  coords$x <- unlist(unname(x_pos))[coords$name]
  coords$y <- max(coords$layer) - coords$layer
  rownames(coords) <- coords$name
  coords[ , c("x", "y")]
}