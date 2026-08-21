graph_layout <- function(g, layout, labels, node_group, snap, ...) {
  name <- NULL
  dots <- list(...)
  if ("x" %in% names(dots) & "y" %in% names(dots)) {
    lo <- ggraph::create_layout(g, layout = "manual",
                                x = dots[["x"]], y = dots[["y"]])
  } else lo <- suppressWarnings(ggraph::create_layout(g, layout, ...))
  if ("graph" %in% names(attributes(lo))) {
    if (!setequal(names(as.data.frame(attr(lo, "graph"))), names(lo))) {
      for (n in setdiff(names(as.data.frame(attr(lo, "graph"))), names(lo))) {
        lo[n] <- igraph::vertex_attr(g, n)
      }
    }
  }
  p <- ggraph::ggraph(lo) + ag_theme_void()
  if (!is.null(node_group)) {
    # thisRequires("ggforce")
    # A membership matrix repeats a node's coordinates once for each group it
    # belongs to, so a node in several groups is inside several hulls and the
    # hulls overlap. One long data frame draws them all: ggforce draws one hull
    # for each level of the fill.
    if (is.matrix(node_group)) {
      idx <- which(node_group, arr.ind = TRUE)
      hulls <- data.frame(
        x = lo[["x"]][idx[, 1]], y = lo[["y"]][idx[, 1]],
        node_group = factor(colnames(node_group)[idx[, 2]],
                            levels = colnames(node_group)))
      ngroups <- ncol(node_group)
    } else {
      hulls <- lo
      ngroups <- length(unique(p$data[[node_group]]))
    }
    p <- p + 
      ggforce::geom_mark_hull(ggplot2::aes(x, y, fill = node_group,
                                           label = node_group), data = hulls) +
      ggplot2::scale_fill_manual(values = ag_qualitative(ngroups),
                                 guide = ggplot2::guide_legend("Group"))
  }
  if(snap){
    # Layered layouts already encode meaning in their coordinates -- rank,
    # mode, or generation along one axis -- which square-grid snapping would
    # collapse. Skip snapping for those and keep the layout as computed.
    layered_layouts <- c("hierarchy", "railway", "ladder", "alluvial",
                         "multilevel", "lineage", "layered")
    is_layered <- is.character(layout) && length(layout) == 1L &&
      layout %in% layered_layouts
    if (is_layered) {
      manynet::snet_info(paste0("Skipping snapping: the '", layout,
                                "' layout is layered, so its coordinates ",
                                "are kept as computed."))
    } else {
    manynet::snet_info("Snapping layout coordinates to grid.")
    if(grepl("lattice", manynet::net_name(g), ignore.case = TRUE)){
      
      angles <- seq(0, pi/2, length.out = 180)
      scores <- sapply(angles, function(a) {
        lay2 <- .rotate_layout(lo, a)
        .edge_angle_deviation(lay2, g)
      })
      
      best_angle <- angles[which.min(scores)]
      rotated_coords <- .rotate_layout(lo, best_angle)
      # Make sure that the coordinates, if rounded to integers, are still unique
      p$data[,c("x","y")] <- round(rotated_coords[,c("x","y")])
    } else p$data[,c("x","y")] <- depth_first_recursive_search(p)
    }
  }
  p
}

# Helper functions ----

.rotate_layout <- function(layout, angle) {
  rot <- matrix(c(cos(angle), -sin(angle),
                  sin(angle),  cos(angle)), ncol = 2)
  coords <- as.matrix(layout[, c("x", "y")])
  newcoords <- coords %*% rot
  layout$x <- newcoords[,1]
  layout$y <- newcoords[,2]
  layout
}

.edge_angle_deviation <- function(layout, graph) {
  ed <- igraph::as_edgelist(graph)
  dx <- layout$x[ed[,2]] - layout$x[ed[,1]]
  dy <- layout$y[ed[,2]] - layout$y[ed[,1]]
  ang <- atan2(dy, dx)
  
  # deviation from nearest multiple of 90°
  dev <- abs((ang %% (pi/2)) - pi/4)
  mean(dev)
}




