# Aesthetic-resolution helpers shared by graphr() (via graph_nodes()/graph_edges())
# and grapht(). Each resolves an argument that may be a literal value (e.g.
# node_size = 6) or the name of a node/tie attribute into the vector or scalar
# actually mapped in the plot.

# An attribute that takes the same value everywhere cannot distinguish anything,
# so it is dropped in favour of plain black. Reported the same way wherever it
# happens, in both graphr() and grapht().
.inform_constant_color <- function(arg, attribute, what) {
  manynet::snet_info(
    "Drawing every {what} black, because {.arg {arg}} was mapped to",
    "{.val {attribute}}, which holds the same value for every {what}.",
    "To colour {what}s differently, map {.arg {arg}} to an attribute that",
    "varies between them.")
}

# Node aesthetics ----

.infer_nsize <- function(g, node_size) {
  if (!is.null(node_size)) {
    if (is.character(node_size)) {
      out <- manynet::node_attribute(g, node_size)
    } else out <- node_size
    out <- .check_aes_length(out, g, "node_size", manynet::net_nodes(g), "node")
    # A vector of proportions (a centrality score, say) would be invisible at
    # face value, so rescale it into a usable range. A single number is taken
    # at face value: `node_size = 0.5` means 0.5.
    if (length(out) > 1 && all(out <= 1 & out >= 0, na.rm = TRUE)) out <- out * 10
  } else {
    out <- min(20, (250 / manynet::net_nodes(g)) / 2)
  }
  as.numeric(out)
}

# A value mapped to an aesthetic has to be either a single value or one per
# node/tie; ggplot2 would otherwise report the mismatch in terms of its own
# internal data frame ("Aesthetics must be either length 1 or the same as the
# data (8)"), which does not say which argument was wrong.
.check_aes_length <- function(out, g, arg, n, what) {
  len <- length(out)
  if (len == 1L || len == n) return(out)
  manynet::snet_abort(
    "{.arg {arg}} should be a single value or one value for each of the",
    "{n} {what}s in the network, but {len} value{?s} {?was/were} given.")
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
        .inform_constant_color("node_color", node_color, "node")
      }
    } else out <- node_color
  } else {
    out <- "black"
  }
  out
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
        .inform_constant_color("edge_color", edge_color, "tie")
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
