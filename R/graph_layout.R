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
  p <- ggraph::ggraph(lo) + ggplot2::theme_void()
  if (!is.null(node_group)) {
    # thisRequires("ggforce")
    p <- p + 
      ggforce::geom_mark_hull(ggplot2::aes(x, y, fill = node_group,
                                           label = node_group), data = lo) +
      ggplot2::scale_fill_manual(values = ag_qualitative(length(unique(p$data[[node_group]]))),
                                 guide = ggplot2::guide_legend("Group"))
  }
  if(snap){
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
  # Add background ----
  if(getOption("snet_background", default = "#FFFFFF")!="#FFFFFF")
    p <- p + ggplot2::theme(panel.background = ggplot2::element_rect(fill = getOption("snet_background", 
                                                                                      default = "#FFFFFF")))
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




