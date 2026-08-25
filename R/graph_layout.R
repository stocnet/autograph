# The name of a drawn dimension. A layout that reports how much of the
# network's inertia the dimension holds says so here, since an axis is where
# a reader looks for the scale it is reading against.
.dim_label <- function(k, fit) {
  base <- paste("Dimension", k)
  share <- fit[["inertia"]][k]
  if (is.null(share) || !is.finite(share)) return(base)
  paste0(base, " (", round(share * 100), "% of inertia)")
}

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
  # A graph has no use for axes, save where its coordinates can be read. The
  # "scaling" layout draws distances that mean something, so it keeps its axes,
  # and keeps them on one scale: a distance read off two axes of different
  # scales is not the distance the layout placed there.
  # The "correspondence" layout draws distances that can be read in the same
  # way, and names the share of inertia each of its dimensions holds.
  if (is.character(layout) && length(layout) == 1L &&
      layout %in% c("scaling", "correspondence")) {
    fit <- attr(lo, "fit")
    p <- p + ag_theme_minimal() +
      ggplot2::labs(x = .dim_label(1, fit), y = .dim_label(2, fit))
    # ggraph has already set a coordinate system, and ggplot2 announces the
    # replacement, which is not news to anyone here.
    p <- suppressMessages(p + ggplot2::coord_fixed())
  }
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
    # Some layouts already encode meaning in their coordinates -- a layer, a
    # mode, a generation, or a date along one axis, a scaled distance along
    # both -- which square-grid snapping would collapse. Skip snapping for
    # those and keep the layout as computed.
    is_fixed <- is.character(layout) && length(layout) == 1L &&
      layout %in% .fixed_layouts()
    if (is_fixed) {
      manynet::snet_info(paste0("Skipping snapping: the '", layout,
                                "' layout carries meaning in its coordinates, ",
                                "so they are kept as computed."))
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
