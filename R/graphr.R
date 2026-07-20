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
#'   If not declared, defaults to "triad" for networks with 3 nodes,
#'   "quad" for networks with 4 nodes,
#'   "stress" for all other one mode networks,
#'   or "hierarchy" for two mode networks.
#'   For "hierarchy" layout, one can further split graph by
#'   declaring the "center" argument as the "events", "actors",
#'   or by declaring a node name.
#'   For "concentric" layout algorithm please declare the "membership" as an 
#'   extra argument.
#'   The "membership" argument expects either a quoted node attribute present
#'   in data or vector with the same length as nodes to draw concentric circles.
#'   For "multilevel" layout algorithm please declare the "level"
#'   as extra argument.
#'   The "level" argument expects either a quoted node attribute present
#'   in data or vector with the same length as nodes to hierarchically
#'   order categories.
#'   If "level" is missing, function will look for 'lvl' node attribute in data.
#'   The "lineage" layout ranks nodes in Y axis according to values.
#'   For "lineage" layout algorithm please declare the "rank"
#'   as extra argument.
#'   The "rank" argument expects either a quoted node attribute present
#'   in data or vector with the same length as nodes.
#' @param labels Logical, whether to print node names
#'   as labels if present.
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
#'   sense, e.g. "circle"/"concentric", "bipartite"/"railway", "alluvial").
#' @param label_repel Logical scalar, whether labels should be repelled away
#'   from each other and from nodes using `ggrepel`
#'   (via `ggraph`'s `repel` argument). Defaults to `TRUE`.
#'   Set to `FALSE` to place labels at a fixed offset (see `label_dist`)
#'   without the (sometimes slow, and non-deterministic between runs for
#'   some layouts) repelling algorithm.
#' @param snap Logical scalar, whether the layout should be snapped to a grid.
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
#' @param ... Extra arguments to pass on to the layout algorithm, if necessary.
#' @return A `ggplot2::ggplot()` object.
#'   The last plot can be saved to the file system using `ggplot2::ggsave()`.
#' @importFrom ggraph geom_edge_link geom_node_text
#' @importFrom ggraph geom_edge_bundle_force geom_edge_bundle_path 
#' @importFrom ggraph geom_edge_bundle_minimal geom_node_label
#' @importFrom ggraph geom_node_point scale_edge_width_continuous 
#' @importFrom ggplot2 aes arrow unit scale_color_brewer scale_fill_brewer
#' @importFrom tidygraph activate
#' @examples
#' graphr(ison_adolescents)
#' ison_adolescents %>%
#'   mutate(color = rep(c("introvert","extrovert"), times = 4),
#'          size = ifelse(netrics::node_is_cutpoint(ison_adolescents), 6, 3)) %>%
#'   mutate_ties(ecolor = rep(c("friends", "acquaintances"), times = 5)) %>%
#'   graphr(node_color = "color", node_size = "size",
#'          edge_size = 1.5, edge_color = "ecolor")
#' graphr(ison_southern_women, labels = TRUE, label_dist = 10)
#' graphr(ison_southern_women, labels = TRUE, label_repel = FALSE)
#' graphr(manynet::generate_random(40, 0.1), edge_bundle = TRUE)
#' @export
graphr <- function(.data, layout = NULL, labels = TRUE,
                   node_color, node_shape, node_size, node_group,
                   edge_color, edge_size,
                   isolates = c("legend","caption","keep"), snap = FALSE,
                   label_dist = NULL, label_repel = TRUE, edge_bundle = FALSE,
                   ..., node_colour, edge_colour) {
  if(manynet::is_list(.data)) return(graphs(.data, layout = layout, labels = labels,
                             node_color = node_color, node_shape = node_shape, node_size = node_size, node_group = node_group,
                             edge_color = edge_color, edge_size = edge_size,
                             isolates = isolates, snap = snap,
                             edge_bundle = edge_bundle, ...,
                             node_colour = node_colour, edge_colour = edge_colour))
  g <- manynet::as_tidygraph(.data)
  
  # Separate isolates ----
  isolates <- .infer_isolates(g, match.arg(isolates))
  if(isolates != "keep"){
    if(manynet::is_labelled(g)){
      isos <- manynet::node_names(g)[.node_is_isolate(g)]
    } else {
      isos <- which(.node_is_isolate(g))
    }
    g <- manynet::to_no_isolates(g)
  } 
  
  layout <- .infer_layout(g, layout)
  if (missing(node_color) && missing(node_colour)) {
    node_color <- NULL
  } else if (missing(node_color)) {
    node_color <- as.character(substitute(node_colour))
  } else {
    node_color <- as.character(substitute(node_color))
  }
  if (missing(node_shape)) node_shape <- NULL else
    node_shape <- as.character(substitute(node_shape))
  if (missing(node_size)) node_size <- NULL else if (!is.numeric(node_size)) {
    node_size <- as.character(substitute(node_size))
  }
  if (missing(node_group)) node_group <- NULL else {
    node_group <- as.character(substitute(node_group))
    g <- manynet::mutate_nodes(g, 
                               node_group = .reduce_categories(g, node_group))
  }
  if (missing(edge_color) && missing(edge_colour)) {
    edge_color <- NULL
  } else if (missing(edge_color)) {
    edge_color <- as.character(substitute(edge_colour))
  } else {
    edge_color <- as.character(substitute(edge_color))
  }
  if (missing(edge_size)) edge_size <- NULL else if (!is.numeric(edge_size)) {
    edge_size <- as.character(substitute(edge_size))
  }
  # Add layout ----
  p <- graph_layout(g, layout, labels, node_group, snap, ...)
  # Add edges ----
  p <- graph_edges(p, g, edge_color, edge_size, node_size, edge_bundle)
  # Add nodes ----
  p <- graph_nodes(p, g, node_color, node_shape, node_size)
  # Add labels ----
  if (isTRUE(labels) & manynet::is_labelled(g)) {
    p <- graph_labels(p, g, layout, label_dist, label_repel,
                      node_size = .infer_nsize(g, node_size))
  }
  
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
    p <- p + ggplot2::labs(caption = paste("Isolates:", paste(isos, collapse = ", ")))
  }
  
  # Add legends ----
  p <- graph_legends(p, g, 
                     node_color, node_shape, node_size,
                     edge_color, edge_size)
  
  # assign("last.warning", NULL, envir = baseenv()) # to avoid persistent ggrepel
  p
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
  if(!any(.node_is_isolate(g))) isolates <- "keep"
  isolates
}

.infer_layout <- function(g, layout) {
  if (is.null(layout)) {
    if(manynet::is_list(g))
      g <- g[[1]]
    if (manynet::net_nodes(g) <= 6) {
      layout <- "configuration"
    } else if (manynet::is_twomode(g)) {
      layout <- "hierarchy"
    } else layout <- "stress"
  }
  layout
}

.reduce_categories <- function(g, node_group) {
  limit <- toCondense <- NULL
  if (sum(table(manynet::node_attribute(g, node_group)) <= 2) > 2 &
      length(unique(manynet::node_attribute(g, node_group))) > 2) {
    toCondense <- names(which(table(manynet::node_attribute(g, node_group)) <= 2))
    out <- ifelse(manynet::node_attribute(g, node_group) %in% toCondense,
                  "Other", manynet::node_attribute(g, node_group))
    manynet::snet_info("The number of groups was reduced since there were groups with less than 2 nodes.")
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
    manynet::snet_info("The number of groups was reduced since there were groups with less than 2 nodes.")
  } else if (sum(table(manynet::node_attribute(g, node_group)) <= 2) == 1 &
             length(unique(manynet::node_attribute(g, node_group))) > 2) {
    limit <- stats::reorder(manynet::node_attribute(g, node_group),
                            manynet::node_attribute(g, node_group),
                            FUN = length, decreasing = TRUE)
    toCondense <- utils::tail(levels(limit), 2)
    out <- ifelse(manynet::node_attribute(g, node_group) %in% toCondense, "Other",
                  manynet::node_attribute(g, node_group))
    manynet::snet_info("The number of groups was reduced since there were groups with less than 2 nodes.")
  } else if (sum(table(manynet::node_attribute(g, node_group)) <= 2) == 1 &
             length(unique(manynet::node_attribute(g, node_group))) == 2) {
    out <- as.factor(manynet::node_attribute(g, node_group))
    manynet::snet_info("Node groups with 2 nodes or less can be cause issues for plotting ...")
  } else out <- as.factor(manynet::node_attribute(g, node_group))
  out
}



