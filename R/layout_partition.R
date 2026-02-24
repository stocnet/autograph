#' Layout algorithms based on bi- or other partitions
#' 
#' @description
#'   These algorithms layout networks based on two or more partitions,
#'   and are recommended for use with `graphr()` or `{ggraph}`.
#'
#'   The "hierarchy" layout layers the first node set along the bottom,
#'   and the second node set along the top, 
#'   sequenced and spaced as necessary to minimise edge overlap.
#'   The "alluvial" layout is similar to "hierarchy", 
#'   but places successive layers horizontally rather than vertically.
#'   The "railway" layout is similar to "hierarchy",
#'   but nodes are aligned across the layers.
#'   The "ladder" layout is similar to "railway",
#'   but places successive layers horizontally rather than vertically.
#'   The "concentric" layout places a "hierarchy" layout
#'   around a circle, with successive layers appearing as concentric circles.
#'   The "multilevel" layout places successive layers as multiple levels.
#'   The "lineage" layout ranks nodes in Y axis according to values.
#' @name layout_partition
#' @inheritParams layout_layered
#' @param circular Should the layout be transformed into a radial representation. 
#' Only possible for some layouts. Defaults to FALSE.
#' @param times Maximum number of iterations, where appropriate
#' @param radius A vector of radii at which the concentric circles
#'   should be located for "concentric" layout.
#'   By default this is equal placement around an empty centre, 
#'   unless one (the core) is a single node,
#'   in which case this node occupies the centre of the graph.
#' @param order.by An attribute label indicating the (decreasing) order
#'   for the nodes around the circles for "concentric" layout.
#'   By default ordering is given by a bipartite placement that reduces
#'   the number of edge crossings.
#' @param membership A node attribute or a vector to draw concentric circles
#'   for "concentric" layout.
#' @param center Further split "hierarchical" layouts by
#'   declaring the "center" argument as the "events", "actors",
#'   or by declaring a node name in hierarchy layout. 
#'   Defaults to NULL.
#' @param level A node attribute or a vector to hierarchically order levels for
#'   "multilevel" layout.
#' @param rank A numerical node attribute to place nodes in Y axis
#'   according to values for "lineage" layout.
#' @family mapping
#' @source
#'   Diego Diez, Andrew P. Hutchins and Diego Miranda-Saavedra. 2014.
#'   "Systematic identification of transcriptional regulatory modules from
#'   protein-protein interaction networks". 
#'   _Nucleic Acids Research_, 42 (1) e6.
NULL

#' @rdname layout_partition
#' @examples
#' #graphr(ison_southern_women, layout = "concentric", membership = "type",
#' #           node_color = "type", node_size = 3)
#' @export
layout_concentric <- function(.data, membership,
                                        radius = NULL, 
                                        order.by = NULL, 
                                        circular = FALSE, times = 1000) {
  if (any(igraph::vertex_attr(.data, "name") == "")) {
    ll <- unlist(lapply(seq_len(length(.data)), function(x) {
      ifelse(igraph::vertex_attr(.data, "name")[x] == "",
             paste0("ramdom", x), igraph::vertex_attr(.data, "name")[x])
    }))
    .data <- igraph::set_vertex_attr(.data, "name", value = ll)
  }
  if (missing(membership)) { 
    if (manynet::is_twomode(.data)) membership <- manynet::node_is_mode(.data) else 
      manynet::snet_abort("Please pass the function a `membership` node attribute or a vector.")
  } else {
    if (length(membership) > 1 & length(membership) != length(.data)) {
      manynet::snet_abort("Please pass the function a `membership` node attribute or a vector.")
    } else if (length(membership) != length(.data)) {
      membership <- manynet::node_attribute(.data, membership)
    }
  }
  names(membership) <- manynet::node_names(.data)
  membership <- to_list(membership)
  all_c  <- unlist(membership, use.names = FALSE)
  if (any(table(all_c) > 1)) manynet::snet_abort("Duplicated nodes in layers!")
  if (manynet::is_labelled(.data)) all_n <- manynet::node_names(.data) else 
    all_n <- 1:manynet::net_nodes(.data)
  sel_other  <- all_n[!all_n %in% all_c]
  if (length(sel_other) > 0) membership[[length(membership) + 1]] <- sel_other
  if (is.null(radius)) {
    radius <- seq(0, 1, 1/(length(membership)))
    if (length(membership[[1]]) == 1) 
      radius <- radius[-length(radius)] else radius <- radius[-1]
  }
  if (!is.null(order.by)) {
    order.values <- lapply(order.by, 
                           function(b) manynet::node_attribute(.data, b))
  } else {
    if (manynet::is_twomode(.data) & length(membership) == 2) {
      xnet <- manynet::as_matrix(manynet::to_multilevel(.data))[membership[[2-1]], 
                                              membership[[2]]]
      lo <- layout_tbl_graph_hierarchy(manynet::as_igraph(xnet, twomode = TRUE))
      lo$names <- manynet::node_names(.data)
      if (ncol(lo) == 2) lo[,1] <- seq_len(dim(lo)[1])
      order.values <- lapply(1:0, function(x)
        if(ncol(lo) >= 3) sort(lo[lo[,2] == x,])[,3] 
        else sort(lo[lo[,2] == x,1])) 
    } else order.values <- membership[order(sapply(membership, length))]
    # order.values <- getNNvec(.data, members)
  }
  res <- matrix(NA, nrow = length(all_n), ncol = 2)
  for (k in seq_along(membership)) {
    r <- radius[k]
    l <- order.values[[k]]
    if(manynet::is_labelled(.data))
      l <- match(l, manynet::node_names(.data))
    res[l, ] <- getCoordinates(l, r)
  }
  .to_lo(res)
}

#' @rdname layout_partition
#' @export
layout_tbl_graph_concentric <- layout_concentric

#' @rdname layout_partition
#' @examples
#' #graphr(ison_lotr, layout = "multilevel",
#' #           node_color = "Race", level = "Race", node_size = 3)
#' @export
layout_multilevel <- function(.data, level, circular = FALSE) {
  if (missing(level)) {
    if (any(grepl("lvl", names(manynet::node_attribute(.data))))) {
      manynet::snet_info("Level attribute 'lvl' found in data.")
    } else {
      manynet::snet_abort("Please pass the function a `level` node attribute or a vector.")
    }
  } else {
    if (length(level) > 1 & length(level) != length(.data)) {
      manynet::snet_abort("Please pass the function a `level` node attribute or a vector.")
    } else if (length(level) != length(.data)) {
      level <- as.factor(manynet::node_attribute(.data, level))
    }
  }
  out <- igraph::set_vertex_attr(.data, "lvl", value = level)
  thisRequires("graphlayouts")
  out <- graphlayouts::layout_as_multilevel(out, alpha = 25)
  .to_lo(out)
}

#' @rdname layout_partition
#' @export
layout_tbl_graph_multilevel <- layout_multilevel

#' @rdname layout_partition
#' @examples
#' # ison_adolescents %>%
#' #   mutate(year = rep(c(1985, 1990, 1995, 2000), times = 2),
#' #          cut = node_is_cutpoint(ison_adolescents)) %>%
#' #   graphr(layout = "lineage", rank = "year", node_color = "cut",
#' #              node_size = migraph::node_degree(ison_adolescents)*10)
#' @export
layout_lineage <- function(.data, rank, circular = FALSE) {
  if (length(rank) > 1 & length(rank) != length(.data)) {
    manynet::snet_abort("Please pass the function a `rank` node attribute or a vector.")
  } else if (length(rank) != length(.data)) {
    rank <- as.numeric(manynet::node_attribute(.data, rank))
  }
  out <- layout_tbl_graph_alluvial(
    manynet::as_igraph(mutate(.data, type = ifelse(
      rank > mean(rank), TRUE, FALSE)), twomode = TRUE))
  out$x <- .rescale(rank)
  .check_dup(out)
}

#' @rdname layout_partition
#' @export
layout_tbl_graph_lineage <- layout_lineage

.rescale <- function(vector){
  (vector - min(vector)) / (max(vector) - min(vector))
}

# Sugiyama-style layout with dummy nodes and barycenter heuristic
# for better edge crossing minimization
.sugiyama_layout <- function(g, layers = NULL, times = 100) {
  n <- igraph::vcount(g)
  el <- igraph::as_edgelist(g, names = FALSE)
  # Layer assignment
  if (is.null(layers)) {
    lo <- igraph::layout_with_sugiyama(g, maxiter = times)
    node_layer <- lo$layout[, 2]
  } else {
    node_layer <- layers
  }
  layer_vals <- sort(unique(node_layer))
  n_layers <- length(layer_vals)
  if (n_layers < 2) {
    return(cbind(seq_len(n), node_layer))
  }
  # Map layers to 0-based indices
  layer_idx <- match(node_layer, layer_vals) - 1L
  # Build adjacency between original nodes
  adj <- vector("list", n)
  radj <- vector("list", n)
  for (i in seq_len(n)) { adj[[i]] <- integer(0); radj[[i]] <- integer(0) }
  if (nrow(el) > 0) {
    for (i in seq_len(nrow(el))) {
      u <- el[i, 1]; v <- el[i, 2]
      adj[[u]] <- c(adj[[u]], v)
      radj[[v]] <- c(radj[[v]], u)
    }
  }
  # Insert dummy nodes for edges spanning multiple layers
  dummy_id <- n
  # For barycenter, we need per-layer node lists and inter-layer edges
  all_layer <- layer_idx  # will grow with dummies
  # Build inter-layer edges (only between adjacent layers)
  inter_edges <- list()
  if (nrow(el) > 0) {
    for (i in seq_len(nrow(el))) {
      u <- el[i, 1]; v <- el[i, 2]
      lu <- layer_idx[u]; lv <- layer_idx[v]
      if (lu == lv) next
      # Ensure direction goes from lower layer to higher
      if (lu > lv) { tmp <- u; u <- v; v <- tmp; tmp <- lu; lu <- lv; lv <- tmp }
      if (lv - lu == 1) {
        inter_edges[[length(inter_edges) + 1]] <- c(u, v)
      } else {
        # Insert dummy nodes
        prev <- u
        for (k in (lu + 1):(lv - 1)) {
          dummy_id <- dummy_id + 1
          all_layer <- c(all_layer, k)
          inter_edges[[length(inter_edges) + 1]] <- c(prev, dummy_id)
          prev <- dummy_id
        }
        inter_edges[[length(inter_edges) + 1]] <- c(prev, v)
      }
    }
  }
  total_nodes <- length(all_layer)
  if (length(inter_edges) == 0) {
    return(cbind(seq_len(n), node_layer))
  }
  inter_edges_mat <- do.call(rbind, inter_edges)
  # Build per-layer node lists
  layer_nodes <- lapply(0:(n_layers - 1), function(k) which(all_layer == k))
  # Initialize x positions: sequential within each layer
  x_pos <- rep(0, total_nodes)
  for (k in seq_along(layer_nodes)) {
    nodes_in_layer <- layer_nodes[[k]]
    x_pos[nodes_in_layer] <- seq_along(nodes_in_layer)
  }
  # Build forward/backward adjacency for the expanded graph
  fwd_adj <- vector("list", total_nodes)
  bwd_adj <- vector("list", total_nodes)
  for (i in seq_len(total_nodes)) { fwd_adj[[i]] <- integer(0); bwd_adj[[i]] <- integer(0) }
  if (!is.null(inter_edges_mat) && nrow(inter_edges_mat) > 0) {
    for (i in seq_len(nrow(inter_edges_mat))) {
      u <- inter_edges_mat[i, 1]; v <- inter_edges_mat[i, 2]
      fwd_adj[[u]] <- c(fwd_adj[[u]], v)
      bwd_adj[[v]] <- c(bwd_adj[[v]], u)
    }
  }
  # Barycenter crossing minimization sweeps
  for (iter in seq_len(times)) {
    # Forward sweep: layer 1 to n_layers-1
    for (k in 2:n_layers) {
      nodes_k <- layer_nodes[[k]]
      if (length(nodes_k) <= 1) next
      bc <- sapply(nodes_k, function(nd) {
        neighbors <- bwd_adj[[nd]]
        if (length(neighbors) == 0) return(x_pos[nd])
        mean(x_pos[neighbors])
      })
      ord <- order(bc)
      x_pos[nodes_k[ord]] <- seq_along(nodes_k)
    }
    # Backward sweep: layer n_layers-2 to 0
    for (k in (n_layers - 1):1) {
      nodes_k <- layer_nodes[[k]]
      if (length(nodes_k) <= 1) next
      bc <- sapply(nodes_k, function(nd) {
        neighbors <- fwd_adj[[nd]]
        if (length(neighbors) == 0) return(x_pos[nd])
        mean(x_pos[neighbors])
      })
      ord <- order(bc)
      x_pos[nodes_k[ord]] <- seq_along(nodes_k)
    }
  }
  # Extract coordinates for original nodes only
  cbind(x_pos[seq_len(n)], node_layer)
}

#' @rdname layout_partition
#' @examples
#' #graphr(ison_southern_women, layout = "hierarchy", center = "events",
#' #           node_color = "type", node_size = 3)
#' @export
layout_hierarchy <- function(.data, center = NULL,
                                       circular = FALSE, times = 1000) {
  if (is.null(center)) {
    g <- manynet::as_igraph(.data)
    if (manynet::is_twomode(.data)) {
      layers <- ifelse(igraph::V(g)$type, 2, 1)
    } else {
      layers <- NULL
    }
    lo <- .sugiyama_layout(g, layers = layers, times = times)
    nodeX <- lo[, 1]
    nodeY <- lo[, 2]
    if (length(unique(nodeX)) > 1) nodeX <- .rescale(nodeX)
    if (length(unique(nodeY)) > 1) nodeY <- .rescale(nodeY)
    if (manynet::is_twomode(.data) & "name" %in% igraph::vertex_attr_names(.data)) {
      names <- igraph::vertex_attr(.data, "name")
      names(nodeX) <- igraph::vertex_attr(g, "name")
      names(nodeY) <- igraph::vertex_attr(g, "name")
      nodeX <- nodeX[order(match(names(nodeX), names))]
      nodeY <- nodeY[order(match(names(nodeY), names))]
    }
    out <- .to_lo(cbind(nodeX, nodeY))
  } else {
    if (!manynet::is_twomode(.data)) manynet::snet_abort("Please declare a two-mode network.")
    net <- manynet::as_matrix(.data)
    nn <- dim(net)[1]
    mm <- dim(net)[2]
    if (center == "actors") {
      Act <- cbind(rep(1, nrow(net)), nrm(rng(nn)))
      Evt1 <- cbind(rep(0, ceiling(ncol(net)/2)), nrm(rng(ceiling(mm/2))))
      Evt2 <- cbind(rep(2, floor(ncol(net)/2)), nrm(rng(floor(mm/2))))
      crd <- rbind(Act, Evt1, Evt2)
      crd[which(is.nan(crd))] <- 0.5
      rownames(crd) <- c(dimnames(net)[[1]], dimnames(net)[[2]])
    } else if (center == "events") {
      Act1 <- cbind(rep(0, ceiling(nrow(net)/2)), nrm(rng(ceiling(nn/2))))
      Act2 <- cbind(rep(2, floor(nrow(net)/2)), nrm(rng(floor(nn/2))))
      Evt <- cbind(rep(1, ncol(net)), nrm(rng(mm)))
      crd <- rbind(Act1, Act2, Evt)
      crd[which(is.nan(crd))] <- 0.5
      rownames(crd) <- c(dimnames(net)[[1]], dimnames(net)[[2]])
    } else {
      if (center %in% manynet::node_names(.data)) {
        side1 <- suppressWarnings(cbind(rep(0, nrow(net)), nrm(rng(nn))))
        side2 <- suppressWarnings(cbind(rep(2, ncol(net)), nrm(rng(mm))))
        if (any(rownames(net) == center)) {
          side1[,1] <- ifelse(rownames(net) == center, 1, side1[,1])
          side1[,2] <- ifelse(rownames(net) == center, 0.5, side1[,2])
        } else {
          side2[,1] <- ifelse(rownames(net) == center, 1, side2[,1])
          side2[,2] <- ifelse(rownames(net) == center, 0.5, side2[,2])
        }
        crd <- rbind(side1, side2)
        crd[which(is.nan(crd))] <- 0.5
        rownames(crd) <- c(dimnames(net)[[1]], dimnames(net)[[2]])
      } else manynet::snet_abort("Please declare actors, events, or a node name as center.")
    }
    out <- .to_lo(crd)
  }
  out
}

#' @rdname layout_partition
#' @export
layout_tbl_graph_hierarchy <- layout_hierarchy

#' @rdname layout_partition
#' @examples
#' #graphr(ison_southern_women, layout = "alluvial")
#' @export
layout_alluvial <- function(.data,
                                      circular = FALSE, times = 1000){
  g <- manynet::as_igraph(.data)
  if (manynet::is_twomode(.data)) {
    layers <- ifelse(igraph::V(g)$type, 2, 1)
  } else {
    layers <- NULL
  }
  lo <- .sugiyama_layout(g, layers = layers, times = times)
  nodeX <- lo[, 1]
  nodeY <- lo[, 2]
  # Swap x and y for left-to-right layout (alluvial)
  if (length(unique(nodeY)) > 1) nodeY <- .rescale(nodeY)
  if (length(unique(nodeX)) > 1) nodeX <- .rescale(nodeX)
  .to_lo(cbind(nodeY, nodeX))
}

#' @rdname layout_partition
#' @export
layout_tbl_graph_alluvial <- layout_alluvial

#' @rdname layout_partition
#' @export
layout_railway <- function(.data,
                                     circular = FALSE, times = 1000) {
  res <- layout_tbl_graph_hierarchy(manynet::as_igraph(.data))
  res$x <- c(match(res[res[,2]==0,1], sort(res[res[,2]==0,1])),
             match(res[res[,2]==1,1], sort(res[res[,2]==1,1])))
  res
}

#' @rdname layout_partition
#' @export
layout_tbl_graph_railway <- layout_railway

#' @rdname layout_partition
#' @export
layout_ladder <- function(.data,
                          circular = FALSE, times = 1000){
  res <- layout_tbl_graph_alluvial(manynet::as_igraph(.data))
  res$y <- c(match(res[res[,2]==1,1], sort(res[res[,2]==1,1])),
             match(res[res[,2]==0,1], sort(res[res[,2]==0,1])))
  res
}

#' @rdname layout_partition
#' @export
layout_tbl_graph_ladder <- layout_ladder

.to_lo <- function(mat) {
  res <- as.data.frame(mat)
  names(res) <- c("x","y")
  res
}

to_list <- function(members) {
  out <- lapply(sort(unique(members)), function(x){
    y <- which(members==x)
    if(!is.null(names(y))) names(y) else y
  })
  names(out) <- unique(members)
  out
}

.check_dup <- function(mat) {
  mat$y <- ifelse(duplicated(mat[c('x','y')]), mat$y*0.95, mat$y)
  mat
}

#' @importFrom igraph degree
getNNvec <- function(.data, members){
  lapply(members, function(circle){
    diss <- 1 - stats::cor(manynet::to_multilevel(manynet::as_matrix(.data))[, circle])
    diag(diss) <- NA
    if(manynet::is_labelled(.data))
      starts <- names(sort(igraph::degree(.data)[circle], decreasing = TRUE)[1])
    else starts <- paste0("V",1:manynet::net_nodes(.data))[sort(igraph::degree(.data)[circle], 
                                                       decreasing = TRUE)[1]]
    if(length(circle)>1)
      starts <- c(starts, names(which.min(diss[starts,])))
    out <- starts
    if(length(circle)>2){
      for(i in 1:(length(circle)-2)){
        diss <- diss[,!colnames(diss) %in% starts]
        if(is.matrix(diss)){
          side <- names(which.min(apply(diss[starts,], 1, min, na.rm = TRUE)))
          new <- names(which.min(diss[side,]))
        } else {
          side <- names(which.min(diss[starts]))
          new <- setdiff(circle,out)
        }
        if(side == out[1]){
          out <- c(new, out)
          starts <- c(new, starts[2])
        } else {
          out <- c(out, new)
          starts <- c(starts[1], new)
        }
      }
    }
    out
  })
}

getCoordinates <- function(x, r) {
  l <- length(x)
  d <- 360/l
  c1 <- seq(0, 360, d)
  c1 <- c1[1:(length(c1) - 1)]
  tmp <- t(vapply(c1, 
                  function(cc) c(cos(cc * pi/180) * 
                                   r, sin(cc *
                                            pi/180) * r),
                  FUN.VALUE = numeric(2)))
  rownames(tmp) <- x
  tmp
}

rng <- function(r) {
  if (r == 1L) return(0)
  if (r > 1L) {
    x <- vector()
    x <- append(x, (-1))
    for (i in 1:(r - 1)) x <- append(x, ((-1) + (2L/(r - 1L)) * i))
    return(x * (r/50L))
  } else manynet::snet_abort("no negative values")
}

nrm <- function(x, digits = 3) {
  if (isTRUE(length(x) == 1L) == TRUE) return(x)
  if (is.array(x) == TRUE) {
    xnorm <- (x[, 1] - min(x[, 1]))/(max(x[, 1]) - min(x[, 1]))
    rat <- (max(x[, 1]) - min(x[, 1]))/(max(x[, 2]) - min(x[, 2]))
    ynorm <- ((x[, 2] - min(x[, 2]))/(max(x[, 2]) - min(x[, 2]))) * (rat)
    ifelse(isTRUE(rat > 0) == FALSE,
           ynorm <- ((x[, 2] - min(x[, 2]))/(max(x[, 2]) -
                                               min(x[, 2]))) * (1L/rat), NA)
    return(round(data.frame(X = xnorm, Y = ynorm), digits))
  }
  else if (is.vector(x) == TRUE) {
    return(round((x - min(x))/(max(x) - min(x)), digits))
  }
}
