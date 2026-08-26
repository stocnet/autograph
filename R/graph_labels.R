graph_labels <- function(p, g, layout, label_dist = NULL, label_repel = TRUE,
                         node_size = NULL, labels = TRUE) {
  # Labelling every node of a dense network hides the network behind its own
  # labels, so `labels` can also select which nodes to label. The selection is
  # resolved once here and the chosen rows handed to the geoms as their `data`,
  # rather than blanking the others' labels, so that no space is reserved for
  # labels that are not drawn (and, with `label_repel`, nothing is repelled
  # away from them either).
  sel <- .infer_labels(g, labels)
  if (!any(sel)) return(p)
  # These layouts put every node in a place that means something: a layer, a
  # ring, a rank. A repelled label leaves that place, and the reader has to
  # work out which node it belongs to. Each label is offset by a fixed amount
  # instead, so that where a label sits says which node it labels.
  if (.is_structured(layout)) label_repel <- FALSE
  ldata <- p[["data"]][sel, , drop = FALSE]
  # `node_size` arrives with one value per node when it was mapped from an
  # attribute, and has to be cut down to the labelled nodes alongside the data.
  if (length(node_size) > 1) node_size <- node_size[sel]
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

  # `layout` may be something other than a single name (a matrix of coordinates,
  # say), which would make the comparison below error rather than simply not
  # match, so check its shape first as graph_layout() does.
  is_radial <- is.character(layout) && length(layout) == 1L &&
    layout %in% c("circle", "concentric")
  if (is_radial) {
    angles <- as.data.frame(.cart2pol(as.matrix(ldata[,1:2])))
    angles$degree <- angles$phi * 180/pi
    # Extract x and y as vectors for case_when
    x_coord <- ldata[[1]]
    y_coord <- ldata[[2]]
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
    args <- list(mapping = label_aes, data = ldata,
                 repel = label_repel,
                 family = ag_font(), size = ag_text_size(3), hjust = hj, angle = angles_deg)
    if (label_repel) {
      args$point.padding <- padding
    } else {
      angles_rad <- angles_deg * pi / 180
      args$nudge_x <- nudge_unit * cos(angles_rad)
      args$nudge_y <- nudge_unit * sin(angles_rad)
    }
    p <- p + do.call(ggraph::geom_node_text, args) +
      ggplot2::coord_cartesian(xlim=c(-1.3,1.3), ylim=c(-1.3,1.3))
  } else if (layout %in% c("bipartite", "railway") | layout == "layered" &
             length(unique(p[["data"]][["y"]])) <= 2) {
    args <- list(mapping = label_aes, data = ldata,
                 angle = 90,
                 family = ag_font(), size = ag_text_size(3), hjust = "outward",
                 repel = label_repel,
                 nudge_y = ifelse(ldata[,2] == 1,
                                  nudge_unit, -nudge_unit))
    if (label_repel) args$point.padding <- padding
    p <- p + do.call(ggraph::geom_node_text, args) +
      ggplot2::coord_cartesian(ylim=c(-0.2, 1.2))
  } else if (layout == "layered" & length(unique(p[["data"]][["y"]])) > 2) {
    # As for "lineage" below: the label goes immediately to the right of its
    # own node, rather than anywhere the layers leave room.
    args <- list(mapping = label_aes, data = ldata,
                 family = ag_font(), size = ag_text_size(3),
                 repel = label_repel,
                 hjust = 0, nudge_x = .axis_nudge(radius_pt + gap_pt,
                                                  p[["data"]][["x"]]))
    if (label_repel) args$point.padding <- padding
    p <- p + do.call(ggraph::geom_node_text, args) +
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0.05, 0.25)))
  } else if (layout == "levels") {
    # `geom_node_label()`, used below, boxes each label in white, which at the
    # density these networks tend to have would paper over the plot entirely.
    # Plain text instead, nudged away from the plane the node sits in: down
    # from the lower level and up from the upper, so that labels fall into the
    # empty space beyond each plane rather than over the ties between them.
    # Which plane a node sits in is a property of the whole layout, so the
    # median is taken over every node, not just the labelled ones.
    midline <- stats::median(p[["data"]][["y"]])
    y_coord <- ldata[["y"]]
    args <- list(mapping = label_aes, data = ldata,
                 family = ag_font(), size = ag_text_size(2), colour = ag_ink(),
                 repel = label_repel,
                 nudge_y = ifelse(y_coord <= midline,
                                  -nudge_unit, nudge_unit))
    if (label_repel) {
      args$point.padding <- padding
      args$seed <- 1234
      # These layouts leave a lot of empty space above and below each plane
      # for ggrepel to push labels into, far enough that which node a label
      # belongs to stops being obvious. Pull each label back hard towards its
      # own node, and let labels sit closer to each other so that there is
      # less pushing to begin with.
      args$force_pull <- 4
      args$box.padding <- 0.1
      # Wherever a label still ends up away from its node, draw a leader line
      # to it however short the move, rather than only beyond ggrepel's
      # default half a line of text.
      args$min.segment.length <- 0
      args$segment.size <- 0.2
      args$segment.colour <- "grey70"
    }
    p <- p + do.call(ggraph::geom_node_text, args)
  } else if (layout %in% c("lineage", "ladder")) {
    # An opaque fill matches ggrepel's own hardcoded label background
    # (`GeomLabelRepel$default_aes$fill`); without it, plain `GeomLabel`
    # resolves fill via the active theme and renders fully transparent here,
    # making labels invisible wherever they sit over a node. The fill is the
    # theme's own ground rather than white, so that a dark theme does not
    # scatter white cards over its graph.
    # The layers run left to right, so every label goes immediately to the
    # right of its own node, where it reads as a name follows a thing named.
    args <- list(mapping = label_aes, data = ldata,
                 size = ag_text_size(3), fill = ag_ground_fill(), colour = ag_ink(),
                 family = ag_font(), repel = label_repel,
                 hjust = 0, nudge_x = .axis_nudge(radius_pt + gap_pt,
                                                  p[["data"]][["x"]]))
    if (label_repel) args$point.padding <- padding
    p <- p + do.call(ggraph::geom_node_label, args) +
      # Room on the right for the labels of the last layer.
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0.05, 0.25)))
  } else {
    args <- list(mapping = label_aes, data = ldata,
                 family = ag_font(), fill = ag_ground_fill(),
                 colour = ag_ink(), repel = label_repel, size = ag_text_size(3))
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

# The layered family places each node in a layer, which is where a reader looks
# for it, so a label that moves is a label that misleads. `layout` may be a
# matrix of coordinates rather than a name, which would make the comparison
# error rather than simply not match.
.is_structured <- function(layout) {
  is.character(layout) && length(layout) == 1L &&
    layout %in% c("layered", "lineage", "railway", "ladder")
}

# A nudge given in points, as `label_dist` and the node sizes are, has to reach
# the geoms as a distance along an axis. The panel is not measured until the
# plot is drawn, so the conversion takes a nominal panel of 500pt, about a
# seven inch plot, and scales the offset with the span the axis covers.
.axis_nudge <- function(pt, values) {
  span <- diff(range(values, na.rm = TRUE))
  if (!is.finite(span) || span == 0) span <- 1
  pt / 500 * span
}

# Label selection ----

# Turns the value normalised by .check_labels() into one logical per node, in
# the network's node order, which is the order of ggraph's layout data.
.infer_labels <- function(g, labels) {
  n <- as.numeric(manynet::net_nodes(g))
  if (isFALSE(labels)) return(rep(FALSE, n))
  if (isTRUE(labels)) return(rep(TRUE, n))
  if (is.character(labels)) return(manynet::node_names(g) %in% labels)
  .select_labels(g, as.integer(labels), attr(labels, "criterion"),
                 automatic = isTRUE(attr(labels, "automatic")))
}

# Which nodes a measure singles out. `ranks` is a depth rather than a headcount:
# every node within the top `ranks` scores is labelled, so nodes tied at the cut
# are kept together instead of being separated arbitrarily. That is the rule
# netrics::node_is_max() applies, and for two-mode networks it applies it within
# each mode, so both modes are labelled rather than only the denser one.
.select_labels <- function(g, ranks, criterion, automatic = FALSE) {
  n <- as.numeric(manynet::net_nodes(g))
  # Ranking nodes needs {netrics}, which is only suggested. thisRequires() asks
  # to install it when interactive but does nothing otherwise, and labelling is
  # too incidental to a plot to stop it: an automatic selection falls back to
  # the random sample, which needs nothing, while a selection the user asked
  # for by name says what is missing.
  if (criterion != "random" && !.has_netrics()) {
    if (!automatic)
      manynet::snet_abort(
        "The {.pkg netrics} package is needed to rank nodes by",
        "{.val {criterion}}. Please install it from CRAN, or choose which",
        "nodes to label directly, as in {.code labels = c(\"Alice\", \"Bob\")}",
        "or {.code labels = \"random\"}.")
    manynet::snet_info(
      "Labelling a random selection of nodes, since the {.pkg netrics}",
      "package is not installed to rank them by centrality.",
      "Please install it from CRAN to label the most central nodes instead.")
    criterion <- "random"
    ranks <- min(10L, as.integer(n))
  }
  if (criterion == "random") return(.sample_labels(g, ranks))
  # A mark rather than a ranking: label every node it flags, however many.
  if (criterion == "cutpoints") return(as.logical(netrics::node_is_cutpoint(g)))
  measure <- switch(criterion,
                    degree = netrics::node_by_degree(g, normalized = FALSE),
                    betweenness = netrics::node_by_betweenness(g))
  strata <- .label_strata(g)
  if (is.null(strata))
    return(as.logical(netrics::node_is_max(measure, ranks = ranks)))
  out <- rep(FALSE, n)
  for (lvl in unique(strata)) {
    at <- which(strata == lvl)
    out[at] <- .top_ranks(as.numeric(measure)[at], ranks)
  }
  out
}

# Its own function so that the fallback below can be tested with the package
# installed, which .libPaths() makes it awkward to arrange any other way.
.has_netrics <- function() requireNamespace("netrics", quietly = TRUE)

# node_is_max()'s own rule for a single mode, reused so the two cannot drift.
.top_ranks <- function(x, ranks) {
  x %in% x[order(x, decreasing = TRUE)[seq_len(min(ranks, length(x)))]]
}

# netrics::node_is_max() splits two-mode networks by mode itself. Multilevel
# networks that are not two-mode record their levels in the `lvl` attribute
# instead (see layout_levels()), which it knows nothing about, so
# those are the only strata worth handling here.
.label_strata <- function(g) {
  if (manynet::is_twomode(g)) return(NULL)
  if (!"lvl" %in% igraph::vertex_attr_names(g)) return(NULL)
  as.character(manynet::node_attribute(g, "lvl"))
}

# A plot should look the same when drawn twice, so the sample is taken under a
# fixed seed and the session's RNG left as it was found -- the same reason a
# fixed `seed` is passed to ggrepel above.
.sample_labels <- function(g, size) {
  n <- as.numeric(manynet::net_nodes(g))
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  }
  set.seed(1234)
  strata <- .label_strata(g)
  if (is.null(strata) && manynet::is_twomode(g))
    strata <- as.character(igraph::V(g)$type)
  out <- rep(FALSE, n)
  if (is.null(strata)) {
    out[sample.int(n, min(size, n))] <- TRUE
  } else {
    for (lvl in unique(strata)) {
      at <- which(strata == lvl)
      out[at[sample.int(length(at), min(size, length(at)))]] <- TRUE
    }
  }
  out
}

# Helper functions for .graph_labels()

.cart2pol <- function(xyz){
  if (!is.numeric(xyz))
    manynet::snet_abort(
      "Coordinates should be numeric to be converted to polar coordinates,",
      "but a value of class {.cls {class(xyz)}} was given.")
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

