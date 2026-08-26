#' Easily graph networks with sensible defaults
#' 
#' @description 
#'   This function provides users with an easy way to graph
#'   (m)any network data for exploration, investigation, inspiration, 
#'   and communication.
#'   
#'   `graphr()` builds upon `{ggplot2}` and `{ggraph}` to offer
#'   pretty, easy, and extensible graphing solutions.
#'   Just passing the function some network data
#'   will often be sufficient to return a reasonable-looking graph.
#'   
#'   The function also makes it easy to modify many of the most
#'   commonly adapted aspects of a graph, 
#'   including node and edge size, colour, and shape,
#'   as arguments rather than additional functions that you need to remember.
#'   These can be defined outright, e.g. `node_size = 8`, or
#'   in reference to an attribute of the network, e.g. `node_size = "wealth"`.
#'   
#'   Lastly, `graphr()` uses `{ggplot2}`-related theme information, so
#'   it is easy to make colour palette and fonts institution-specific and consistent.
#'   See e.g. `theme_iheid()` for more.
#'   
#'   To learn more about what can be done visually,
#'   try `run_tute("Visualisation")`.
#' @name plot_graphr
#' @family mapping
#' @param .data A manynet-consistent object.
#' @param layout An igraph, ggraph, or manynet layout algorithm.
#'   If not declared, defaults to "configuration" for networks of up to
#'   six nodes, "levels" for connected multilevel networks,
#'   "layered" for other two mode networks,
#'   and "stress" for all other networks.
#'   For "layered" layout, one can further split graph by
#'   declaring the "center" argument as the "events", "actors",
#'   or by declaring a node name.
#'   For "concentric" layout algorithm please declare the "membership" as an 
#'   extra argument.
#'   The "membership" argument expects either a quoted node attribute present
#'   in data or vector with the same length as nodes to draw concentric circles.
#'   For "levels" layout algorithm one may declare the "level"
#'   as extra argument.
#'   The "level" argument expects either a quoted node attribute present
#'   in data or vector with the same length as nodes to hierarchically
#'   order categories.
#'   If "level" is missing, the levels are taken from a 'lvl' node attribute
#'   where there is one, or else from the two modes of a two mode network.
#'   The layered layouts ("layered", "lineage", "railway" and "ladder")
#'   accept a "ranks" argument, which takes either one of the methods named
#'   at `?layout_layered` or a numeric node attribute to lay the layers out by,
#'   as a quoted attribute name or a vector with one value for each node.
#'   The "scaling" layout places the nodes by multidimensional scaling,
#'   so that the distance between two nodes approximates the number of steps
#'   between them. Since those coordinates can be read, this layout is drawn
#'   with labelled axes on one scale, and captioned with how well two
#'   dimensions hold the distances; see `?layout_scaling` and `check_stress()`.
#'   Note that those axes carry distances rather than named dimensions:
#'   the drawing can be turned or mirrored without fitting the network
#'   any better or any worse.
#'   The "correspondence" layout places the nodes by correspondence analysis,
#'   so that two nodes with similar ties are drawn together,
#'   whether or not they are tied to each other.
#'   It is the usual way to draw a two mode network, since it places both
#'   modes against the same pair of axes, and it accepts a "direction"
#'   argument for a directed network and a "double" argument for a signed
#'   one; see `?layout_correspondence`.
#'   Each axis names the share of the network's inertia that it holds.
#' @param labels Which nodes to label, if the network is labelled.
#'   `TRUE` (the default) labels every node and `FALSE` none of them,
#'   but a label for every node of a large network hides the network behind
#'   them, so a *selection* of the nodes can be given instead:
#'
#'   - a number, e.g. `labels = 5`, labels the nodes within the top five ranks
#'     by degree. Note that this is a depth of ranks rather than a count of
#'     nodes: nodes tied at the cut are labelled together, so more than five
#'     labels may appear.
#'   - a measure to rank by, e.g. `labels = "betweenness"`, labels just the
#'     node or nodes that measure singles out. `"degree"`, `"betweenness"`,
#'     `"cutpoints"` (every node the mark flags) and `"random"`
#'     (a small random sample) are available.
#'     The two can be combined by naming the number,
#'     as in `labels = c(betweenness = 5)`.
#'   - the name of a logical node attribute, e.g. `labels = "is_broker"`,
#'     labels the nodes it marks.
#'   - a logical vector, one value per node, e.g.
#'     `labels = netrics::node_is_cutpoint(net)`;
#'     or the names or positions of the nodes to label,
#'     e.g. `labels = c("Alice", "Betty")`.
#'
#'   Where a length-one string could mean more than one of these,
#'   a node attribute is preferred to a measure, and a measure to a node name.
#'   A single number is always read as a depth of ranks rather than as one
#'   node's position, so a lone node is best named, as in `labels = "Alice"`.
#'   For networks of more than 30 nodes, `labels` defaults to a selection
#'   rather than to every node; pass `labels = TRUE` for all of them.
#'   Ranking nodes uses the `{netrics}` package, which is suggested rather than
#'   required: without it installed, an automatic selection falls back to a
#'   random sample.
#'   Two-mode and multilevel networks are ranked within each mode or level,
#'   so that every level is labelled and not just the densest.
#' @param node_shape Node variable to be used for shaping the nodes.
#'   It is easiest if this is added as a node attribute to
#'   the graph before plotting.
#'   Nodes can also be shaped by declaring a shape instead.
#' @param node_size Node variable to be used for sizing the nodes.
#'   This can be any continuous variable on the nodes of the network.
#'   Since this function expects this to be an existing variable,
#'   it is recommended to calculate all node-related statistics prior
#'   to using this function.
#'   Nodes can also be sized by declaring a numeric size or vector instead.
#' @param node_color,node_colour Node variable to be used for coloring the nodes.
#'   It is easiest if this is added as a node attribute to
#'   the graph before plotting.
#'   Nodes can also be colored by declaring a color instead.
#' @param node_group Node variable to be used for grouping the nodes.
#'   It is easiest if this is added as a hull over
#'   groups before plotting.
#'   Group variables should have a minimum of 3 nodes,
#'   if less, number groups will be reduced by
#'   merging categories with lower counts into one called "other".
#'   A membership vector can also be given here.
#'   Where nodes belong to several groups at once, as they can to several
#'   cliques, give a membership matrix instead: one row for each node,
#'   one column for each group, and a one wherever the node belongs to
#'   the group. One hull is then drawn for each column, and the hulls
#'   overlap where the groups do.
#'   A measure that returns such a matrix, such as
#'   `netrics::node_x_clique()`, can be named without its network,
#'   which is taken to be the network being drawn.
#' @param edge_color,edge_colour Tie variable to be used for coloring the nodes.
#'   It is easiest if this is added as an edge or tie attribute 
#'   to the graph before plotting.
#'   Edges can also be colored by declaring a color instead.
#' @param edge_size Tie variable to be used for sizing the edges.
#'   This can be any continuous variable on the nodes of the network.
#'   Since this function expects this to be an existing variable,
#'   it is recommended to calculate all edge-related statistics prior
#'   to using this function.
#'   Edges can also be sized by declaring a numeric size or vector instead.
#' @param isolates Character scalar, how to treat isolates.
#'   "keep"  will keep isolates in the graph as they are.
#'   "legend" (default) will remove isolates from the graph but note them in the legend.
#'   "caption" will remove isolates from the graph but note them in the caption.
#'   If there are no isolates, this argument will be ignored.
#'   If the default layout ("stress") is used, 
#'   we recommend that the "legend" option is used to avoid isolates crowding
#'   out the giant component.
#' @param label_dist Numeric scalar, in points (pt), controlling the extra
#'   gap left between labels and node borders -- similar to `igraph`'s
#'   `vertex.label.dist`. Node size is always accounted for automatically
#'   (larger nodes push labels further away without any extra configuration);
#'   `label_dist` adds further spacing on top of that, and defaults to a
#'   small gap (5pt). Set to `0` for labels right at the node border,
#'   or to a larger value (e.g. `15`) for more spacing.
#'   Only used when `labels = TRUE` and `label_repel = TRUE`
#'   (as the padding passed to the repel algorithm) or `label_repel = FALSE`
#'   (as a fixed nudge away from the node, in the layouts where this makes
#'   sense, e.g. "circle"/"concentric", "railway", "lineage").
#' @param label_repel Logical scalar, whether labels should be repelled away
#'   from each other and from nodes using `ggrepel`
#'   (via `ggraph`'s `repel` argument). Defaults to `TRUE`.
#'   Set to `FALSE` to place labels at a fixed offset (see `label_dist`)
#'   without the (sometimes slow, and non-deterministic between runs for
#'   some layouts) repelling algorithm.
#'   The layered layouts ("layered", "lineage", "railway" and "ladder")
#'   place each node in a layer, which is where the reader looks for it,
#'   so a repelled label there would say less about which node it labels
#'   than a fixed offset does. They ignore this argument and always offset.
#' @param snap Logical scalar, whether the layout should be snapped to a grid.
#'   Where the network repeats a structure, as a lattice does, the two steps it
#'   repeats are mapped onto the axes, which draws it as a rectangle of rows and
#'   columns. Where it does not, each node moves to the nearest vacant grid
#'   point. Layouts that already carry meaning in their coordinates, such as
#'   "layered" or "scaling", are left as they are.
#' @param edge_bundle Edge bundling, off by default (`FALSE`). When `TRUE` (or
#'   equivalently `"force"`), edges are bundled together using ggraph's
#'   force-directed edge bundling (`geom_edge_bundle_force()`), which pulls
#'   nearby edges into shared paths to reduce visual clutter in dense networks.
#'   Alternative non-hierarchical algorithms can be selected by name:
#'   `"path"` (`geom_edge_bundle_path()`) or `"minimal"`
#'   (`geom_edge_bundle_minimal()`). Bundling only makes a visible difference
#'   when a network has enough edges; for directed networks arrowheads are
#'   retained, but the slight reciprocal-tie curvature used for unbundled edges
#'   does not apply.
#' @param backbone How to treat the network's backbone: the ties that a local
#'   null model keeps, because they carry more weight, or sit in more
#'   triangles, than chance alone would put there.
#'   Where a backbone is used, those ties are drawn as the shortest, so that
#'   the layout pulls apart the groups they hold together, and every tie is
#'   still drawn, with the ties the filter does not keep faded well back.
#'   This is what to reach for when a network is dense enough to draw as a
#'   hairball.
#'   By default (`NULL`) this is decided by the network: a network of at least
#'   50 nodes and a mean degree of at least 8 is drawn this way, and reported.
#'   `FALSE` draws every tie alike, and `TRUE` asks for a backbone whatever the
#'   network's size.
#'   One of `manynet`'s filters can be named instead: "disparity", "lans",
#'   "noise", "mlf", or "simmelian". Where none is named, `manynet` uses "lans"
#'   for a weighted network and "simmelian" for an unweighted one.
#'   A number between 0 and 1 sets the threshold instead of the filter:
#'   a smaller number keeps fewer ties.
#'   Only the layouts that read tie lengths -- "stress" (the default), "fr",
#'   "drl" and "kk" -- are laid out this way. Every other layout, including
#'   those that already carry meaning in their coordinates such as "layered"
#'   or "scaling", keeps its coordinates and only fades its ties.
#'   Requires `manynet` 2.3.0 or later, and does not apply to signed networks.
#' @param .shared Internal. A list of the aesthetic ranges and categories found
#'   across a list of networks, which `graphs()` uses to draw and label each of
#'   its panels against the same scales. Not intended to be set by hand.
#' @param ... Extra arguments to pass on to the layout algorithm, if necessary.
#' @return A `ggplot2::ggplot()` object.
#'   The last plot can be saved to the file system using `ggplot2::ggsave()`.
#' @importFrom ggraph geom_edge_link geom_node_text
#' @importFrom ggraph geom_edge_bundle_force geom_edge_bundle_path 
#' @importFrom ggraph geom_edge_bundle_minimal geom_node_label
#' @importFrom ggraph geom_node_point scale_edge_width_continuous 
#' @importFrom ggplot2 aes arrow unit scale_color_brewer scale_fill_brewer
#' @examples
#' graphr(ison_adolescents)
#' ison_adolescents |>
#'   mutate(color = rep(c("introvert","extrovert"), times = 4),
#'          size = ifelse(netrics::node_is_cutpoint(ison_adolescents), 6, 3)) |>
#'   mutate_ties(ecolor = rep(c("friends", "acquaintances"), times = 5)) |>
#'   graphr(node_color = "color", node_size = "size",
#'          edge_size = 1.5, edge_color = "ecolor")
#' graphr(ison_southern_women, labels = TRUE, label_dist = 10)
#' graphr(ison_southern_women, labels = TRUE, label_repel = FALSE)
#' # Label a selection of the nodes rather than all of them
#' graphr(ison_southern_women, labels = 2)
#' graphr(ison_southern_women, labels = "betweenness")
#' graphr(ison_adolescents, labels = c("Alice", "Betty"))
#' graphr(manynet::generate_random(40, 0.1), edge_bundle = TRUE)
#' graphr(manynet::generate_random(80, 0.2), backbone = TRUE)
#' @export
graphr <- function(.data, layout = NULL, labels = TRUE,
                   node_color, node_shape, node_size, node_group,
                   edge_color, edge_size,
                   isolates = c("legend","caption","keep"), snap = FALSE,
                   label_dist = NULL, label_repel = TRUE, edge_bundle = FALSE,
                   backbone = NULL, .shared = NULL, ...,
                   node_colour, edge_colour) {
  # A list of networks is handed to graphs(). The call is forwarded as written,
  # rather than argument by argument, because the aesthetic arguments have no
  # defaults: naming them here would force promises that are still missing.
  if(manynet::is_list(.data)) {
    cl <- match.call()
    # The function itself rather than its name, since the call is evaluated in
    # the caller's environment, where `graphs` is only visible if the package
    # happens to be attached (it is not for `autograph::graphr(mylist)`).
    cl[[1L]] <- graphs
    names(cl)[names(cl) == ".data"] <- "netlist"
    return(eval(cl, parent.frame()))
  }
  labels_missing <- missing(labels)
  g <- .check_network(.data)
  # Checked here, before isolates are dropped below, so that a vector selecting
  # which nodes to label is measured against the network as the user gave it.
  # It comes back as node names, which survive that change of node positions.
  labels <- .check_labels(g, labels)

  # Separate isolates ----
  # `isolates` is checked on its own line rather than inside .infer_isolates(),
  # which does not always force its argument: an unrecognised `isolates` would
  # otherwise be caught or ignored depending on whether the network happens to
  # have isolates.
  isolates <- .check_choice(isolates, c("legend", "caption", "keep"), "isolates")
  isolates <- .infer_isolates(g, isolates)
  if(isolates != "keep"){
    if(manynet::is_labelled(g)){
      isos <- manynet::node_names(g)[.node_is_isolate(g)]
    } else {
      isos <- which(.node_is_isolate(g))
    }
    g <- .ag_delete_isolates(g)
  }
  # A label for every node of a large network hides the network behind them,
  # so unless labelling was asked for outright, fall back to labelling the
  # nodes that stand out. Decided here rather than above so that the count
  # reflects the nodes actually drawn, once any isolates have been dropped.
  n <- as.numeric(manynet::net_nodes(g))
  if (labels_missing && isTRUE(labels) && n > 30) {
    labels <- structure(5L, criterion = "degree", automatic = TRUE)
    n_lab <- sum(.infer_labels(g, labels))
    manynet::snet_info(
      "Labelling the {n_lab} most central of {n} nodes.",
      "Use {.code labels = TRUE} to label all of them,",
      "{.code labels = 25} to label more, or {.code labels = FALSE} for none.")
  }

  layout <- .infer_layout(g, .check_layout(layout))
  # Substituted here rather than in graph_layout(), since `layout` is also
  # passed to graph_edges(), graph_nodes() and graph_labels(), which would
  # otherwise style the plot for a layout that was not the one drawn.
  layout <- .check_layout_applies(g, layout, ...)
  if (missing(node_color) && missing(node_colour)) {
    node_color <- NULL
  } else if (missing(node_color)) {
    node_color <- .check_node_color(g, as.character(substitute(node_colour)),
                                    "node_colour")
  } else {
    node_color <- .check_node_color(g, as.character(substitute(node_color)))
  }
  if (missing(node_shape)) node_shape <- NULL else
    node_shape <- .check_node_shape(g, as.character(substitute(node_shape)))
  if (missing(node_size)) node_size <- NULL else if (!is.numeric(node_size)) {
    node_size <- .check_node_size(g, as.character(substitute(node_size)))
  }
  if (missing(node_group)) node_group <- NULL else {
    node_group <- .infer_node_group(g, substitute(node_group), parent.frame())
    if (!is.matrix(node_group)) {
      if (is.character(node_group) && length(node_group) == 1L) {
        node_group <- .check_node_group(g, node_group)
      } else {
        # A membership vector is held on the network, so that it is treated as
        # any other node attribute from here on.
        g <- manynet::mutate_nodes(g, .group = node_group)
        node_group <- ".group"
      }
      g <- manynet::mutate_nodes(g,
                                 node_group = .reduce_categories(g, node_group))
    }
  }
  if (missing(edge_color) && missing(edge_colour)) {
    edge_color <- NULL
  } else if (missing(edge_color)) {
    edge_color <- .check_edge_color(g, as.character(substitute(edge_colour)),
                                    "edge_colour")
  } else {
    edge_color <- .check_edge_color(g, as.character(substitute(edge_color)))
  }
  if (missing(edge_size)) edge_size <- NULL else if (!is.numeric(edge_size)) {
    edge_size <- .check_edge_size(g, as.character(substitute(edge_size)))
  }
  # Find the backbone ----
  # After the layout is settled, since a layout that carries meaning in its
  # coordinates keeps them and fades its ties only, and after the isolates are
  # dropped, so that the filter reads the network that is drawn.
  backbone <- .infer_backbone(g, .check_backbone(backbone), layout, edge_bundle,
                              manual = all(c("x", "y") %in% names(list(...))))
  # Add layout ----
  p <- graph_layout(g, layout, labels, node_group, snap, backbone, ...)
  # Read where the layout left it, since the later steps have no use for it
  # and no reason to carry it. See `layout_scaling()`.
  fit <- attr(p[["data"]], "fit")
  # Add edges ----
  p <- graph_edges(p, g, edge_color, edge_size, node_size, edge_bundle, layout,
                   .shared, backbone)
  # Add nodes ----
  p <- graph_nodes(p, g, node_color, node_shape, node_size, layout, .shared)
  # Add labels ----
  if (!isFALSE(labels) && manynet::is_labelled(g)) {
    p <- graph_labels(p, g, layout, label_dist, label_repel,
                      node_size = .infer_nsize(g, node_size, layout),
                      labels = labels)
  }
  
  # Give the edge nodes room ----
  # After the labels, since a layered or lineage layout sets its own expansion
  # there and this widens that rather than replacing it.
  p <- .pad_for_nodes(p, .infer_nsize(g, node_size, layout))

  # Note isolates ----
  if(isolates == "legend"){
    if (length(isos) > 3) label_text <- paste(c(utils::head(isos, 3),"..."), collapse = "\n") else 
      label_text <- paste(isos, collapse = "\n")
    p <- p + ggplot2::geom_point(aes(x=rep(0, manynet::net_nodes(g)), y=0, 
                                     alpha = "Isolates"), 
                                 size = 0) +
      ggplot2::scale_alpha_manual(name = "+ Isolates", 
                                  values = c("Isolates" = 0.5), 
                                  labels = label_text)
  } else if(isolates == "caption"){
    p <- .add_caption(p, paste("Isolates:", paste(isos, collapse = ", ")))
  }

  # Note the fit ----
  # A scaled layout draws distances that can be read, so how well two
  # dimensions hold those distances is part of the drawing rather than an
  # aside. See `check_stress()` for how to read the score.
  p <- .note_fit(p, fit)
  
  # Add legends ----
  p <- graph_legends(p, g, 
                     node_color, node_shape, node_size,
                     edge_color, edge_size)
  
  # assign("last.warning", NULL, envir = baseenv()) # to avoid persistent ggrepel
  p
}

# A scaled layout draws distances that can be read, so how well two dimensions
# hold what the layout scaled is part of the drawing rather than an aside.
# Each layout that reports a fit says so in its own terms. See
# `layout_scaling()` and `layout_correspondence()`.
.note_fit <- function(p, fit) {
  if (is.null(fit)) return(p)
  switch(fit[["type"]] %||% "scaling",
         scaling = .note_scaling_fit(p, fit),
         correspondence = .note_corresp_fit(p, fit),
         p)
}

.note_scaling_fit <- function(p, fit) {
  if (!is.finite(fit[["stress"]])) return(p)
  txt <- paste0("Stress: ", round(fit[["stress"]] * 100), "%.")
  if (!is.na(fit[["variance"]])) txt <- paste0(
    txt, " Two dimensions hold ", round(fit[["variance"]] * 100),
    "% of the distance variance.")
  p <- .add_caption(p, txt)
  # Kruskal read a stress of 20% as poor, but that figure was set for
  # psychometric data: most pairs of nodes in a network sit two or three
  # steps apart, which no plane holds well, so most networks would be
  # reported on at 20% and the message would say nothing.
  if (fit[["stress"]] > 0.3) manynet::snet_info(
    "Two dimensions hold these path distances poorly",
    "(stress: {round(fit[['stress']] * 100)}%),",
    "so read the clusters rather than the distances.",
    "See {.fn check_stress}.")
  p
}

# The share of inertia each dimension holds is already named on its axis, so
# nothing is added to the caption here. What the axes cannot say is which
# nodes those two dimensions place badly, and those are exactly the nodes a
# reader would otherwise read too much into.
.note_corresp_fit <- function(p, fit) {
  .note_corresp_inertia(fit)
  .note_corresp_cos2(fit)
  p
}

# The axes name the share of inertia, but a share means nothing without the
# number of dimensions it was won from, and an axis has no room for that. Two
# dimensions of a small table hold a good deal whatever the network, so the
# share is checked against what the same two dimensions would hold if the
# inertia were divided at random. See `.broken_stick()`.
.note_corresp_inertia <- function(fit) {
  drawn <- sum(fit[["inertia"]])
  k <- length(fit[["scree"]])
  if (!is.finite(drawn) || k < 3L) return(invisible(NULL))
  if (drawn > .broken_stick(k)) return(invisible(NULL))
  manynet::snet_info(
    "These two dimensions hold no more of the inertia than dividing it at",
    "random would give them ({round(drawn * 100)}% of {k} dimensions),",
    "so read the clusters rather than the positions.")
}

# The broken stick model: the share the first two of `k` dimensions would hold
# if the inertia were broken at random into `k` pieces. A far harder baseline
# than an even share, since inertia is never spread evenly, and the one worth
# warning against (Jackson 1993, \doi{10.2307/1939574}).
.broken_stick <- function(k) sum(vapply(1:2, function(i) mean(1 / (i:k)), 1))

.note_corresp_cos2 <- function(fit) {
  cos2 <- fit[["cos2"]]
  poor <- names(cos2)[!is.na(cos2) & cos2 < 0.3]
  # Only where more than one node is placed badly: a single one is as likely
  # to be a node with few ties as a sign that the drawing is not to be read.
  if (length(poor) < 2L) return(invisible(NULL))
  shown <- if (length(poor) > 5) c(utils::head(poor, 5), "...") else poor
  manynet::snet_info(
    "{length(poor)} nodes sit far off the plane drawn",
    "({.val {shown}}), so read their positions with care.")
}

# Helper functions for graphr() ----
.node_is_isolate <- function(g) {
  if (manynet::is_directed(g)) {
    in_degree <- igraph::degree(g, mode = "in")
    out_degree <- igraph::degree(g, mode = "out")
    isolates <- (in_degree == 0) & (out_degree == 0)
  } else {
    degree <- igraph::degree(g)
    isolates <- degree == 0
  }
  isolates
}

.infer_isolates <- function(g, isolates){
  # Keep isolates when there are none to remove, or when removing them
  # would empty the graph (e.g. tie-less motif networks)
  if(!any(.node_is_isolate(g)) || all(.node_is_isolate(g))) isolates <- "keep"
  isolates
}

.infer_layout <- function(g, layout) {
  if (is.null(layout)) {
    if(manynet::is_list(g))
      g <- g[[1]]
    if (manynet::net_nodes(g) <= 6) {
      layout <- "configuration"
    } else if (.ag_is_multilevel(g) && manynet::is_connected(g)) {
      # Checked before `is_twomode()`, which is also TRUE for these networks.
      # A "layered" layout would place each level along a single row, which
      # collapses the within-level ties that make the network multilevel.
      # Only where the network is connected, since the levels layout
      # orients its levels by the distances between them and so cannot place
      # components that have no distance to each other.
      layout <- "levels"
    } else if (manynet::is_twomode(g)) {
      layout <- "layered"
    } else if (manynet::is_directed(g) && manynet::is_acyclic(g)) {
      # A directed acyclic network ranks its nodes: every tie points from an
      # earlier layer to a later one. A force-directed layout throws that
      # away, so a parent can be drawn below its own child.
      layout <- "layered"
    } else layout <- "stress"
  }
  layout
}

# `node_group` names one node attribute, which can put each node in one group
# only. A node can belong to several groups at once, though, as it can to
# several cliques, and a single attribute cannot record that. A membership
# matrix can: one row for each node, one column for each group, and a one
# wherever the node belongs to the group. `netrics::node_x_clique()` returns
# such a matrix, and graph_layout() draws one hull for each of its columns,
# so that the hulls overlap where the groups do.

# Resolves what the user gave to `node_group` into either the name of a node
# attribute, as before, or a membership matrix. A call such as
# `node_x_clique()` is evaluated on the network being drawn, so that the user
# does not need to name the network twice.
.infer_node_group <- function(g, expr, env) {
  # A name or a string is a node attribute, as it has always been. Only where
  # it names no attribute is it evaluated, which is how a matrix held in a
  # variable reaches the branch below.
  if (is.character(expr) || is.name(expr)) {
    value <- as.character(expr)
    if (length(value) != 1L || value %in% igraph::vertex_attr_names(g))
      return(value)
    out <- tryCatch(eval(expr, env), error = function(e) NULL)
    # Nothing of that name to evaluate, so the mismatch is reported against
    # the node attributes. A single string is a node attribute name too,
    # whether it was written out or held in a variable.
    if (is.null(out)) return(value)
    if (is.character(out) && length(out) == 1L) return(out)
  } else out <- eval(.add_data_arg(expr, g, env), env)
  if (is.matrix(out) || is.array(out) || inherits(out, "data.frame"))
    return(.as_group_matrix(g, out))
  # A vector of memberships is returned as it is, for graphr() to hold on the
  # network and treat as any other node attribute.
  if (length(out) == as.numeric(manynet::net_nodes(g))) return(out)
  manynet::snet_abort(
    "{.arg node_group} should name a node attribute, or give a membership",
    "vector or matrix with one row for each of the {manynet::net_nodes(g)} nodes.")
}

# Adds the network as the `.data` argument of a call that does not give one,
# e.g. `node_x_clique()` or `node_x_clique(min_clique_size = 4)`. A call that
# names its own network, e.g. `node_x_clique(ison_adolescents)`, is left alone.
.add_data_arg <- function(expr, g, env) {
  if (!is.call(expr)) return(expr)
  fun <- tryCatch(eval(expr[[1L]], env), error = function(e) NULL)
  if (!is.function(fun) || !".data" %in% names(formals(fun))) return(expr)
  args <- as.list(expr)[-1]
  given <- names(args)
  if (".data" %in% given) return(expr)
  # An unnamed argument would be matched to `.data` positionally.
  if (length(args) && (is.null(given) || !all(nzchar(given)))) return(expr)
  expr[[".data"]] <- g
  expr
}

# Normalises a membership matrix onto the nodes of `g`: one row for each node,
# in the order the network holds them, and one named column for each group.
.as_group_matrix <- function(g, value) {
  if (inherits(value, "data.frame")) {
    ischr <- vapply(value, function(x) is.character(x) || is.factor(x),
                    logical(1))
    labels <- if (any(ischr)) as.character(value[[which(ischr)[1]]]) else NULL
    value <- as.matrix(value[!ischr])
    if (!is.null(labels)) rownames(value) <- labels
  } else value <- as.matrix(unclass(value))
  n <- as.numeric(manynet::net_nodes(g))
  # Isolates are dropped before this point, so a matrix calculated on the
  # network as the user holds it can have more rows than there are nodes to
  # draw. Node names say which rows those are.
  if (!is.null(rownames(value)) && manynet::is_labelled(g)) {
    nms <- manynet::node_names(g)
    if (all(nms %in% rownames(value))) value <- value[nms, , drop = FALSE]
  }
  if (nrow(value) != n)
    manynet::snet_abort(
      "{.arg node_group} was given a membership matrix with {nrow(value)} rows,",
      "but the network has {n} nodes.")
  if (is.null(colnames(value)))
    colnames(value) <- paste0("G", seq_len(ncol(value)))
  value <- value > 0
  # A group no node belongs to has no hull to draw.
  value <- value[, colSums(value) > 0, drop = FALSE]
  if (ncol(value) == 0)
    manynet::snet_abort("{.arg node_group} was given no groups to draw.")
  if (any(colSums(value) <= 2))
    manynet::snet_info(
      "Groups of two nodes or fewer can be difficult to draw a hull around,",
      "so this plot may look uneven.")
  value
}

.reduce_categories <- function(g, node_group) {
  limit <- toCondense <- NULL
  if (sum(table(manynet::node_attribute(g, node_group)) <= 2) > 2 &
      length(unique(manynet::node_attribute(g, node_group))) > 2) {
    toCondense <- names(which(table(manynet::node_attribute(g, node_group)) <= 2))
    out <- ifelse(manynet::node_attribute(g, node_group) %in% toCondense,
                  "Other", manynet::node_attribute(g, node_group))
    .inform_groups_reduced(toCondense)
  } else if (sum(table(manynet::node_attribute(g, node_group)) <= 2) == 2 &
             length(unique(manynet::node_attribute(g, node_group))) > 2) {
    limit <- stats::reorder(manynet::node_attribute(g, node_group),
                            manynet::node_attribute(g, node_group),
                            FUN = length, decreasing = TRUE)
    if (sum(utils::tail(attr(limit, "scores"), 2))) {
      toCondense <- utils::tail(levels(limit), 3)
    } else {
      toCondense <- utils::tail(levels(limit), 2)
    }
    out <- ifelse(manynet::node_attribute(g, node_group) %in% toCondense, "Other",
                  manynet::node_attribute(g, node_group))
    .inform_groups_reduced(toCondense)
  } else if (sum(table(manynet::node_attribute(g, node_group)) <= 2) == 1 &
             length(unique(manynet::node_attribute(g, node_group))) > 2) {
    limit <- stats::reorder(manynet::node_attribute(g, node_group),
                            manynet::node_attribute(g, node_group),
                            FUN = length, decreasing = TRUE)
    toCondense <- utils::tail(levels(limit), 2)
    out <- ifelse(manynet::node_attribute(g, node_group) %in% toCondense, "Other",
                  manynet::node_attribute(g, node_group))
    .inform_groups_reduced(toCondense)
  } else if (sum(table(manynet::node_attribute(g, node_group)) <= 2) == 1 &
             length(unique(manynet::node_attribute(g, node_group))) == 2) {
    out <- as.factor(manynet::node_attribute(g, node_group))
    manynet::snet_info(
      "Groups of two nodes or fewer can be difficult to draw a hull around,",
      "so this plot may look uneven.")
  } else out <- as.factor(manynet::node_attribute(g, node_group))
  out
}

# Groups too small to draw a hull around are folded into an "Other" group.
# Said in one place so the three branches above cannot drift apart.
.inform_groups_reduced <- function(condensed) {
  manynet::snet_info(
    "Grouped {.val {condensed}} together as {.val Other},",
    "since {?it holds/they hold} two nodes or fewer,",
    "which is too few to draw a group around.")
}



