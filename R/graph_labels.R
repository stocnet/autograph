graph_labels <- function(p, g, layout, label_dist = NULL, label_repel = TRUE,
                         node_size = NULL) {
  # `point.size` tells ggrepel the actual rendered diameter (in points) of
  # each node, so the repel algorithm keeps labels clear of the node's true
  # border rather than just its (x, y) centre -- ggrepel otherwise assumes a
  # token 1pt point. `label_dist` (default 5pt) is the *extra* gap beyond
  # that border, i.e. ggrepel's `point.padding`, mirroring igraph's
  # `vertex.label.dist`.
  point_size_pt <- if (!is.null(node_size)) node_size * ggplot2::.pt else 1
  gap_pt <- if (!is.null(label_dist)) label_dist else 5
  padding <- ggplot2::unit(gap_pt, "pt")
  # When `label_repel = FALSE` there is no repel algorithm to keep labels off
  # of nodes, so we approximate the same clearance (node radius + gap) as a
  # fixed radial/directional nudge instead. Converted from points to
  # data-space units with a factor calibrated against this function's
  # previous hardcoded nudges (e.g. 0.05 alongside a fixed 5pt default gap).
  radius_pt <- if (!is.null(node_size)) max(node_size, na.rm = TRUE) / 2 * ggplot2::.pt else 0
  nudge_unit <- (radius_pt + gap_pt) * 0.01
  # `point.size` is only a recognised aesthetic on the ggrepel geoms (used
  # when `repel = TRUE`); plain `GeomText`/`GeomLabel` (`repel = FALSE`) don't
  # know it and would warn about an unknown aesthetic.
  label_aes <- if (label_repel) {
    ggplot2::aes(label = name, point.size = point_size_pt)
  } else {
    ggplot2::aes(label = name)
  }

  if (layout == "circle" | layout == "concentric") {
    angles <- as.data.frame(.cart2pol(as.matrix(p[["data"]][,1:2])))
    angles$degree <- angles$phi * 180/pi
    # Extract x and y as vectors for case_when
    x_coord <- p[["data"]][[1]]
    y_coord <- p[["data"]][[2]]
    angles_deg <- dplyr::case_when(y_coord == 0 & x_coord == 0 ~ 0.1,
                               y_coord >= 0 & x_coord > 0 ~ angles$degree,
                               y_coord < 0 & x_coord > 0 ~ angles$degree,
                               x_coord == 1 ~ angles$degree,
                               TRUE ~ angles$degree - 180)
    if (manynet::net_nodes(g) < 10) {
      hj <- ifelse(x_coord >= 0, -0.8, 1.8)
    } else if (manynet::net_nodes(g) < 20) {
      hj <- ifelse(x_coord >= 0, -0.4, 1.4)
    } else {
      hj <- ifelse(x_coord >= 0, -0.2, 1.2)
    }
    args <- list(mapping = label_aes,
                 repel = label_repel,
                 family = ag_font(), size = 3, hjust = hj, angle = angles_deg)
    if (label_repel) {
      args$point.padding <- padding
    } else {
      angles_rad <- angles_deg * pi / 180
      args$nudge_x <- nudge_unit * cos(angles_rad)
      args$nudge_y <- nudge_unit * sin(angles_rad)
    }
    p <- p + do.call(ggraph::geom_node_text, args) +
      ggplot2::coord_cartesian(xlim=c(-1.3,1.3), ylim=c(-1.3,1.3))
  } else if (layout %in% c("bipartite", "railway") | layout == "hierarchy" &
             length(unique(p[["data"]][["y"]])) <= 2) {
    args <- list(mapping = label_aes,
                 angle = 90,
                 family = ag_font(), size = 3, hjust = "outward",
                 repel = label_repel,
                 nudge_y = ifelse(p[["data"]][,2] == 1,
                                  nudge_unit, -nudge_unit))
    if (label_repel) args$point.padding <- padding
    p <- p + do.call(ggraph::geom_node_text, args) +
      ggplot2::coord_cartesian(ylim=c(-0.2, 1.2))
  } else if (layout == "hierarchy" & length(unique(p[["data"]][["y"]])) > 2) {
    args <- list(mapping = label_aes,
                 family = ag_font(),
                 size = 3, hjust = "inward", repel = label_repel)
    if (label_repel) {
      args$point.padding <- padding
    } else {
      args$nudge_y <- -nudge_unit
    }
    p <- p + do.call(ggraph::geom_node_text, args)
  } else if (layout %in% c("alluvial", "lineage")) {
    # `fill = "white"` matches ggrepel's own hardcoded label background
    # (`GeomLabelRepel$default_aes$fill`); without it, plain `GeomLabel`
    # resolves fill via the active theme and renders fully transparent here,
    # making labels invisible wherever they sit over a node.
    args <- list(mapping = label_aes,
                 size = 3, fill = "white",
                 family = ag_font(), repel = label_repel,
                 nudge_x = ifelse(p[["data"]][,1] == 1,
                                  nudge_unit, -nudge_unit))
    if (label_repel) args$point.padding <- padding
    p <- p + do.call(ggraph::geom_node_label, args)
  } else {
    args <- list(mapping = label_aes,
                 family = ag_font(), fill = "white",
                 repel = label_repel, size = 3)
    if (label_repel) {
      args$point.padding <- padding
      args$seed <- 1234
    } else {
      args$nudge_x <- nudge_unit
      args$nudge_y <- nudge_unit
    }
    p <- p + do.call(ggraph::geom_node_label, args)
  }
  p
}

# Helper functions for .graph_labels()

.cart2pol <- function(xyz){
  stopifnot(is.numeric(xyz))
  if (is.vector(xyz) && (length(xyz) == 2 || length(xyz) == 
                         3)) {
    x <- xyz[1]
    y <- xyz[2]
    m <- 1
    n <- length(xyz)
  }
  else if (is.matrix(xyz) && (ncol(xyz) == 2 || ncol(xyz) == 
                              3)) {
    x <- xyz[, 1]
    y <- xyz[, 2]
    m <- nrow(xyz)
    n <- ncol(xyz)
  }
  else manynet::snet_abort("Input must be a vector of length 3 or a matrix with 3 columns.")
  phi <- atan2(y, x)
  r <- .hypot(x, y)
  if (n == 2) {
    if (m == 1) 
      prz <- c(phi, r)
    else prz <- cbind(phi, r)
  }
  else {
    if (m == 1) {
      z <- xyz[3]
      prz <- c(phi, r, z)
    }
    else {
      z <- xyz[, 3]
      prz <- cbind(phi, r, z)
    }
  }
  return(prz)
}

.hypot <- function (x, y) {
  if ((length(x) == 0 && is.numeric(y) && length(y) <= 1) || 
      (length(y) == 0 && is.numeric(x) && length(x) <= 1)) 
    return(vector())
  if (!is.numeric(x) && !is.complex(x) || !is.numeric(y) && 
      !is.complex(y)) 
    manynet::snet_abort("Arguments 'x' and 'y' must be numeric or complex.")
  if (length(x) == 1 && length(y) > 1) {
    x <- rep(x, length(y))
    dim(x) <- dim(y)
  }
  else if (length(x) > 1 && length(y) == 1) {
    y <- rep(y, length(x))
    dim(y) <- dim(x)
  }
  if ((is.vector(x) && is.vector(y) && length(x) != length(y)) || 
      (is.matrix(x) && is.matrix(y) && dim(x) != dim(y)) || 
      (is.vector(x) && is.matrix(y)) || is.matrix(x) && is.vector(y)) 
    manynet::snet_abort("Arguments 'x' and 'y' must be of the same size.")
  x <- abs(x)
  y <- abs(y)
  m <- pmin(x, y)
  M <- pmax(x, y)
  ifelse(M == 0, 0, M * sqrt(1 + (m/M)^2))
}

