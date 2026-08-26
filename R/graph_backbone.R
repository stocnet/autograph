# The backbone behind `graphr(backbone = )`, which answers the hairball: a
# network dense enough that every tie covers another one, and dense enough that
# a force layout has no room to pull its groups apart.
#
# {manynet} 2.3.0 marks the ties a local null model keeps -- the ties that carry
# more weight, or sit in more triangles, than chance alone would put there.
# Those ties are what this uses, in two places at once. The layout is computed
# from them, so the groups they hold together separate. Every tie is still
# drawn, but the ties the filter does not keep are faded well back, so that the
# reader can see both the shape of the network and what made it.
#
# `{graphlayouts}` draws a backbone layout of its own, but its Simmelian
# counts need `{oaqc}`, and it replaces the layout rather than informing it.
# manynet's filters need no further package, and leave the choice of layout
# where the user made it.

# The filters manynet offers. Named here so that a wrong name is caught with a
# suggestion before manynet sees it, and so that the names can be offered as
# completions (see R/graph_completion.R).
.backbone_filters <- function() {
  c("disparity", "lans", "noise", "mlf", "simmelian")
}

# Resolves the `backbone` argument to one of three things: NULL where it is
# switched off, the string "auto" where the decision is left to the network
# itself, or a list naming the filter and the threshold to run.
.check_backbone <- function(backbone) {
  if (is.null(backbone)) return("auto")
  if (isFALSE(backbone)) return(NULL)
  if (isTRUE(backbone)) return(list(filter = NULL, threshold = NULL))
  if (is.character(backbone) && length(backbone) == 1L)
    return(list(filter = .check_choice(backbone, .backbone_filters(),
                                       "backbone"),
                threshold = NULL))
  # A number is read as a threshold, since that is the only number the filters
  # take: the significance level under which a tie is kept.
  if (is.numeric(backbone) && length(backbone) == 1L && !is.na(backbone) &&
      backbone > 0 && backbone <= 1)
    return(list(filter = NULL, threshold = backbone))
  # The vector is named here rather than inside the message, since cli reads a
  # brace expression that starts with a dot as one of its own styles.
  filters <- .backbone_filters()
  manynet::snet_abort(
    "{.arg backbone} should be {.code TRUE} or {.code FALSE}, one of",
    "{.or {.val {filters}}}, or a threshold between 0 and 1.")
}

# A network dense enough to draw as a hairball. Read as a mean degree of at
# least eight across at least fifty nodes: below either of those a force layout
# still has the room to separate what there is to separate, and fading half the
# ties of a network the reader can already follow only takes it away from them.
.is_hairball <- function(g) {
  n <- as.numeric(manynet::net_nodes(g))
  m <- as.numeric(manynet::net_ties(g))
  n >= 50 && m >= 4 * n
}

# Which of the filters manynet runs where it is given none. Resolved here,
# rather than left to manynet, so that the message below can name the filter
# the reader is looking at. Kept in step with `manynet:::.backbone_spec()`.
.backbone_filter <- function(g, filter) {
  if (!is.null(filter)) return(filter)
  if (manynet::is_weighted(g)) "lans" else "simmelian"
}

# One logical for each tie of `g`, TRUE where the filter keeps it, or NULL
# where no filter applies. NULL is the answer wherever the drawing would not
# change: a network with no ties, a filter that keeps every tie or none, an
# older manynet, or a signed network, whose negative weights have no place in
# these null models.
.infer_backbone <- function(g, spec, layout = NULL, edge_bundle = FALSE,
                            manual = FALSE) {
  if (is.null(spec)) return(NULL)
  auto <- identical(spec, "auto")
  if (auto) {
    if (!.is_hairball(g)) return(NULL)
    spec <- list(filter = NULL, threshold = NULL)
  }
  if (!.ag_has_manynet("tie_is_backbone")) {
    if (!auto) manynet::snet_info(
      "Drawing every tie alike: {.arg backbone} needs {.pkg manynet} 2.3.0.")
    return(NULL)
  }
  if (manynet::net_ties(g) == 0) return(NULL)
  if (manynet::is_signed(g)) {
    if (!auto) manynet::snet_info(
      "Drawing every tie alike: a signed network has no backbone,",
      "since a negative weight has no place in these null models.")
    return(NULL)
  }
  filter <- .backbone_filter(g, spec[["filter"]])
  mark <- .backbone_mark(g, filter, spec[["threshold"]], auto)
  if (is.null(mark)) return(NULL)
  # A filter that keeps everything says nothing, and one that keeps nothing
  # leaves a drawing of nothing but faded ties.
  if (all(mark) || !any(mark)) {
    if (!auto) manynet::snet_info(
      "Drawing every tie alike: the {.val {filter}} filter keeps",
      "{ifelse(all(mark), 'every tie', 'no tie')} of this network.")
    return(NULL)
  }
  # A layout given coordinates of its own, as each panel of a `graphs()` set
  # is, reads no tie lengths either, whatever layout it is named after.
  .note_backbone(mark, filter, auto,
                 !manual && .backbone_moves_layout(layout), edge_bundle)
  mark
}

# manynet reports the filter and the threshold it settled on, which is said
# again below alongside what it did to the drawing, so its own note is stilled
# here. A filter the user named is left to fail in manynet's own words, since
# that is where the reason is known; one chosen automatically is stepped over
# instead, so that a filter that cannot run never stops a plot.
.backbone_mark <- function(g, filter, threshold, auto) {
  call_it <- function() suppressMessages(
    getExportedValue("manynet", "tie_is_backbone")(g, filter = filter,
                                                   threshold = threshold))
  mark <- if (auto) tryCatch(call_it(), error = function(e) NULL) else call_it()
  if (is.null(mark)) return(NULL)
  # `tie_is_backbone()` returns a named 'tie_mark', and a name or a class of
  # its own would travel into the plot's data as one.
  unname(as.logical(mark))
}

.note_backbone <- function(mark, filter, auto, moves, edge_bundle) {
  kept <- sum(mark)
  total <- length(mark)
  share <- round(100 * kept / total)
  # A layout that keeps its own coordinates, or that has no room for a tie
  # length, fades its ties and no more.
  what <- if (moves)
    paste("Drawing the {kept} of {total} ties ({share}%) that the",
          "{.val {filter}} filter keeps as the shortest, and fading the rest.")
  else
    paste("Fading every tie but the {kept} of {total} ({share}%) that the",
          "{.val {filter}} filter keeps.")
  if (auto) {
    manynet::snet_info(
      what, "Use {.code backbone = FALSE} to draw every tie alike.")
  } else manynet::snet_info(what)
  if (!isFALSE(edge_bundle) && !is.null(edge_bundle))
    manynet::snet_info(
      "Bundled ties are drawn alike: bundling merges ties into shared paths,",
      "which cannot each carry a fading of their own.")
}

# Whether a layout already carries meaning in its coordinates, in which case
# neither snapping (see R/graph_snap.R) nor a backbone may move them.
.is_fixed_layout <- function(layout) {
  is.character(layout) && length(layout) == 1L && layout %in% .fixed_layouts()
}

# The tie lengths the layout is given: a tie the filter keeps is drawn short,
# and a tie it does not is left long, so that the groups the backbone holds
# together are what the algorithm draws together. Every tie is still there, so
# the network is laid out as whole as it was. This is what a backbone layout
# does, and it does more for a hairball than deleting the other ties does: a
# filter is severe enough on an unweighted network to leave dozens of loose
# fragments, which a layout then packs side by side rather than reading.
#
# Which way a weight points is the layout's own business, and the two families
# point opposite ways. A larger weight draws two nodes together in "stress",
# "fr" and "drl" -- ggraph inverts the weights it hands to the first of those
# -- and holds them apart in "kk". Every other layout either takes no weights
# or does nothing with them, and is left as it is.
.backbone_pulls <- function() c("stress", "fr", "drl")
.backbone_pushes <- function() c("kk")

# Whether a layout reads tie lengths at all, which is what decides between
# laying the network out from the backbone and only fading the rest.
.backbone_moves_layout <- function(layout) {
  if (.is_fixed_layout(layout)) return(FALSE)
  is.character(layout) && length(layout) == 1L &&
    layout %in% c(.backbone_pulls(), .backbone_pushes())
}

# How much shorter a kept tie is drawn. Four holds the groups of a modular
# network apart without drawing the ties the filter dropped so long that they
# push the network into a spindle.
.backbone_ratio <- 4

.backbone_layout_weights <- function(g, layout, mark) {
  if (!.backbone_moves_layout(layout)) return(NULL)
  short <- .backbone_anchored(g, mark)
  if (layout %in% .backbone_pulls())
    ifelse(short, .backbone_ratio, 1) else ifelse(short, 1, .backbone_ratio)
}

# A node that the filter left no tie of has nothing drawing it in, and a layout
# then throws it clear of the network and squeezes everything else into the
# corner it leaves. So the strongest tie of such a node is drawn short as well,
# which holds the node beside its neighbour without saying that the filter kept
# the tie: the fading still reports what the filter did.
.backbone_anchored <- function(g, mark) {
  gi <- manynet::as_igraph(g)
  el <- igraph::as_edgelist(gi, names = FALSE)
  held <- tabulate(as.vector(el[mark, , drop = FALSE]),
                   nbins = igraph::vcount(gi))
  loose <- which(held == 0)
  if (!length(loose)) return(mark)
  weight <- if (manynet::is_weighted(gi)) igraph::edge_attr(gi, "weight") else
    rep(1, nrow(el))
  for (v in loose) {
    inc <- which(el[, 1] == v | el[, 2] == v)
    if (length(inc)) mark[inc[which.max(weight[inc])]] <- TRUE
  }
  mark
}
