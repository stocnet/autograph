# Aesthetic-resolution helpers shared by graphr() (via graph_nodes()/graph_edges())
# and grapht(). Each resolves an argument that may be a literal value (e.g.
# node_size = 6) or the name of a node/tie attribute into the vector or scalar
# actually mapped in the plot.

# Node aesthetics ----

.infer_nsize <- function(g, node_size) {
  if (!is.null(node_size)) {
    if (is.character(node_size)) {
      out <- manynet::node_attribute(g, node_size)
    } else out <- node_size
    if (length(node_size > 1) & all(out <= 1 & out >= 0)) out <- out * 10
  } else {
    out <- min(20, (250 / manynet::net_nodes(g)) / 2)
  }
  as.numeric(out)
}

.infer_nshape <- function(g, node_shape) {
  if (!is.null(node_shape)) {
    if (node_shape %in% names(manynet::node_attribute(g))) {
      out <- as.factor(as.character(manynet::node_attribute(g, node_shape)))
    } else out <- node_shape
  } else if (is_twomode(g) & is.null(node_shape)) {
    # igraph convention: type FALSE is the first mode, TRUE the second.
    # "One" sorts before "Two", so the first mode takes the first shape in
    # the scale (a circle) and the second mode the second (a square).
    out <- ifelse(igraph::V(g)$type, "Two", "One")
  } else {
    out <- 21  # Use fillable circle shape (was "circle")
  }
  out
}

.infer_ncolor <- function(g, node_color) {
  if (!is.null(node_color)) {
    if (node_color %in% names(manynet::node_attribute(g))) {
      if ("node_mark" %in% class(manynet::node_attribute(g, node_color))) {
        out <- factor(as.character(manynet::node_attribute(g, node_color)),
                      levels = c("FALSE", "TRUE"))
      } else out <- as.factor(as.character(manynet::node_attribute(g, node_color)))
      if (length(unique(out)) == 1) {
        out <- rep("black", manynet::net_nodes(g))
        manynet::snet_info("Please indicate a variable with more than one value or level when mapping node colors.")
      }
    } else out <- node_color
  } else {
    out <- "black"
  }
  out
}

.check_node_variables <- function(g, node_color, node_size) {
  if (!is.null(node_color)) {
    if (any(!tolower(node_color) %in% tolower(igraph::vertex_attr_names(g))) &
        any(!node_color %in% grDevices::colors())) {
      manynet::snet_info("Please make sure you spelled `node_color` variable correctly.")
    }
  }
  if (!is.null(node_size)) {
    if (!is.numeric(node_size) & any(!tolower(node_size) %in% tolower(igraph::vertex_attr_names(g)))) {
      manynet::snet_info("Please make sure you spelled `node_size` variable correctly.")
    }
  }
}

# Edge aesthetics ----

.infer_ecolor <- function(g, edge_color){
  if (!is.null(edge_color)) {
    if (edge_color %in% names(manynet::tie_attribute(g))) {
      if ("tie_mark" %in% class(manynet::tie_attribute(g, edge_color))) {
        out <- factor(as.character(manynet::tie_attribute(g, edge_color)),
                      levels = c("FALSE", "TRUE"))
      } else out <- as.factor(as.character(manynet::tie_attribute(g, edge_color)))
      if (length(unique(out)) == 1) {
        out <- rep("black", manynet::net_ties(g))
        manynet::snet_info("Please indicate a variable with more than one value or level when mapping edge colors.")
      }
    } else {
      out <- edge_color
    }
  } else if (is.null(edge_color) & manynet::is_signed(g)) {
    # Multiplex/complex signed networks carry a sign only on the signed layer;
    # ties on other layers have `NA` sign. Treat those (and any NA) as positive
    # so the resulting factor never contains NA, which grid rejects at draw time.
    signs <- igraph::E(g)$sign
    out <- factor(ifelse(!is.na(signs) & signs >= 0, "Positive", "Negative"),
                  levels = c("Positive", "Negative"))
    if (length(unique(out)) == 1) {
      out <- "black"
    }
  } else {
    out <- "black"
  }
  out
}

.infer_esize <- function(g, edge_size){
  if (!is.null(edge_size)) {
    if (any(edge_size %in% names(manynet::tie_attribute(g)))) {
      # strip measure classes (e.g. tie_measure) so scales can rescale
      out <- as.numeric(manynet::tie_attribute(g, edge_size))
    } else {
      out <- edge_size
    }
  } else if (is.null(edge_size) & manynet::is_weighted(g)) {
    out <- as.numeric(manynet::tie_attribute(g, "weight"))
  } else {
    out <- 0.5
  }
  out
}

.infer_arrow <- function(esize) {
  # `arrow=` is a fixed layer parameter, not a mappable aesthetic, so a
  # per-edge width vector (`esize` mapped from an attribute) is summarised by
  # its mean to pick one arrowhead size for the whole layer.
  repr <- if (length(esize) > 1) mean(esize, na.rm = TRUE) else esize
  if (length(repr) == 0 || is.na(repr) || repr <= 0) return(NULL)
  # 2mm at the default edge width (0.5), scaled proportionally and capped so
  # heavily-weighted edges don't get oversized arrowheads.
  len_mm <- min(repr / 0.5 * 2, 4)
  ggplot2::arrow(angle = 15, type = "closed", length = ggplot2::unit(len_mm, 'mm'))
}

.infer_line_type <- function(g) {
  if (manynet::is_signed(g)) {
    signs <- as.numeric(manynet::tie_signs(g))
    # Ties without a sign (e.g. non-signed layers of a multiplex network) come
    # back as NA; draw them solid rather than passing NA (an invalid linetype)
    # through to grid. Only genuinely negative ties are dashed.
    out <- ifelse(!is.na(signs) & signs < 0, "dashed", "solid")
    # Collapse to a scalar when every tie is the same so it is treated as a
    # constant layer parameter rather than a per-tie aesthetic.
    if (length(unique(out)) == 1) out <- unique(out)
  } else out <- "solid"
  out
}

.check_edge_variables <- function(g, edge_color, edge_size) {
  if (!is.null(edge_color)) {
    if (any(!tolower(edge_color) %in% tolower(igraph::edge_attr_names(g))) &
        any(!edge_color %in% grDevices::colors())) {
      manynet::snet_info("Please make sure you spelled `edge_color` variable correctly.")
    }
  }
  if (!is.null(edge_size)) {
    if (!is.numeric(edge_size) & any(!tolower(edge_size) %in% tolower(igraph::edge_attr_names(g)))) {
      manynet::snet_info("Please make sure you spelled `edge_size` variable correctly.")
    }
  }
}
