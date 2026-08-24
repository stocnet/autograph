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

.infer_nsize <- function(g, node_size, layout = NULL) {
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
    out <- .default_nsize(manynet::net_nodes(g))
    # The default size shrinks with how crowded the plot is, but a multilevel
    # layout draws each level in a plane of its own, so each is only as crowded
    # as itself. Sizing them from the whole network instead would draw the
    # smaller level -- 53 of `fict_marvel`'s 194 nodes -- as if it held all of
    # them, which is where the one-mode structure of such networks is.
    lvl <- .node_level(g, layout)
    if (!is.null(lvl))
      out <- vapply(lvl, function(l) .default_nsize(sum(lvl == l)), numeric(1))
  }
  as.numeric(out)
}

.default_nsize <- function(n) min(20, (250 / n) / 2)

# The level each node is drawn at by a multilevel layout, or NULL where the
# network is not being drawn that way. Only how the nodes are grouped matters
# here, not which group ends up at which level, so unlike `.infer_level()` in
# R/layout_levels.R there is no need to work out which mode holds the ties
# within itself.
.node_level <- function(g, layout) {
  if (!identical(layout, "levels")) return(NULL)
  if ("lvl" %in% igraph::vertex_attr_names(g))
    return(as.integer(as.factor(igraph::vertex_attr(g, "lvl"))))
  if (!manynet::is_twomode(g)) return(NULL)
  as.integer(manynet::node_is_mode(g)) + 1L
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
    if (node_shape %in% manynet::net_node_attributes(g)) {
      out <- as.factor(as.character(manynet::node_attribute(g, node_shape)))
    } else out <- node_shape
  } else if (is_twomode(g) & is.null(node_shape)) {
    # igraph convention: type FALSE is the first mode, TRUE the second.
    # A factor rather than a character vector, so that the first mode takes
    # the first shape in the scale (a circle) and the second mode the second
    # (a square). Relying on the labels sorting into that order only held
    # while they were "One" and "Two": mode names need not be alphabetical,
    # and `ison_southern_women`'s "social events" sort before its "women".
    modes <- .mode_labels(g)
    out <- factor(ifelse(igraph::V(g)$type, modes[2], modes[1]), levels = modes)
  } else {
    out <- 21  # Use fillable circle shape (was "circle")
  }
  out
}

# What to call each of a two-mode network's modes in the shape legend. Where
# the network records them, "characters" and "teams" say far more than "One"
# and "Two"; `mode_names()` returns NULL where it does not, and a single name
# for a one-mode network, so both modes have to be there to be used.
.mode_labels <- function(g) {
  modes <- tryCatch(manynet::mode_names(g), error = function(e) NULL)
  if (length(modes) != 2 || anyNA(modes) || any(!nzchar(modes)))
    return(c("One", "Two"))
  as.character(modes)
}

.infer_ncolor <- function(g, node_color) {
  if (!is.null(node_color)) {
    if (node_color %in% manynet::net_node_attributes(g)) {
      if ("node_mark" %in% class(manynet::node_attribute(g, node_color))) {
        out <- factor(as.character(manynet::node_attribute(g, node_color)),
                      levels = c("FALSE", "TRUE"))
      } else out <- as.factor(as.character(manynet::node_attribute(g, node_color)))
      if (length(unique(out)) == 1) {
        out <- rep(ag_ink(), manynet::net_nodes(g))
        .inform_constant_color("node_color", node_color, "node")
      }
    } else out <- node_color
  } else {
    out <- ag_ink()
  }
  out
}

# Edge aesthetics ----

.infer_ecolor <- function(g, edge_color){
  if (!is.null(edge_color)) {
    if (edge_color %in% manynet::net_tie_attributes(g)) {
      if ("tie_mark" %in% class(manynet::tie_attribute(g, edge_color))) {
        out <- factor(as.character(manynet::tie_attribute(g, edge_color)),
                      levels = c("FALSE", "TRUE"))
      } else out <- as.factor(as.character(manynet::tie_attribute(g, edge_color)))
      if (length(unique(out)) == 1) {
        out <- rep(ag_ink(), manynet::net_ties(g))
        .inform_constant_color("edge_color", edge_color, "tie")
      }
    } else {
      out <- edge_color
    }
  } else if (is.null(edge_color) & .has_layers(g)) {
    # Which layer a tie belongs to says more about a multiplex network than
    # its sign does, and only some of its ties have a sign to show: a sign is
    # still drawn, as the linetype, but every tie belongs to a layer.
    # Ordered alphabetically, as every other attribute mapped to a colour is.
    # Taking the order from `manynet::layer_names()` instead was tried and
    # dropped: with the two-value highlight palette it decides which layer is
    # drawn in the emphasis colour, and `fict_marvel` names its layers in an
    # order that greys out the very layer the plot is about.
    layers <- manynet::tie_attribute(g, .layer_attribute(g))
    out <- as.factor(as.character(layers))
    if (length(unique(out)) == 1) out <- ag_ink()
  } else if (is.null(edge_color) & manynet::is_signed(g)) {
    # Signed networks that are not layered can still carry a sign on only
    # some of their ties. Treat those (and any NA) as positive, as the
    # linetype does, so that the factor never contains NA, which grid rejects
    # at draw time, and so that colour and linetype agree about which ties
    # are negative.
    signs <- as.numeric(manynet::tie_signs(g))
    out <- factor(ifelse(is.na(signs) | signs >= 0, "Positive", "Negative"),
                  levels = c("Positive", "Negative"))
    if (length(unique(out)) == 1) {
      out <- ag_ink()
    }
  } else {
    out <- ag_ink()
  }
  out
}

# Which tie attribute records the layer each tie belongs to, or NA where none
# does. manynet spells this attribute "type" through 2.2.3 and "layer" from
# 2.3.0, and both spellings appear in the networks 2.3.0 ships, so the
# attribute the network carries decides rather than the manynet version.
# `manynet::net_layers()` reads only the "type" spelling from an igraph, and
# so counts one layer for a network whose ties record a "layer", which is why
# the layers are counted here instead.
.layer_attribute <- function(g) {
  atts <- manynet::net_tie_attributes(g)
  out <- intersect(c("type", "layer"), atts)
  if (length(out) == 0) NA_character_ else out[1]
}

# Whether the ties are divided between layers to tell apart. This is not
# `is_multiplex()`, which is TRUE for any network carrying a non-reserved tie
# attribute or parallel ties, whether or not those distinguish layers. A
# single test covers both that the layers are recorded per tie -- which naming
# them, as `layer_names()` does, need not imply -- and that there are at least
# two of them to tell apart.
.has_layers <- function(g) {
  att <- .layer_attribute(g)
  if (is.na(att)) return(FALSE)
  length(unique(manynet::tie_attribute(g, att))) > 1
}

# What the edge colour legend is titled. `edge_color` names the attribute when
# the user gave one; otherwise the colour carries whatever the default chose,
# so this is decided here alongside `.infer_ecolor()` rather than separately
# by each caller, which is how the legend came to say "Sign" over colours that
# were showing layers.
.infer_ecolor_title <- function(g, edge_color) {
  if (!is.null(edge_color)) return(edge_color)
  if (.has_layers(g)) return("Layer")
  if (manynet::is_signed(g)) return("Sign")
  "Color"
}

.infer_esize <- function(g, edge_size){
  if (!is.null(edge_size)) {
    if (any(edge_size %in% manynet::net_tie_attributes(g))) {
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
