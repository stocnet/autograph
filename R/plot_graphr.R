#' Easily graph networks with sensible defaults
#' 
#' @description 
#'   This function provides users with an easy way to graph
#'   (m)any network data for exploration, investigation, inspiration, 
#'   and communication.
#'   
#'   It builds upon `{ggplot2}` and `{ggraph}` to offer
#'   pretty and extensible graphing solutions.
#'   However, compared to those solutions, 
#'   `graphr()` contains various algorithms to provide better looking
#'   graphs by default.
#'   This means that just passing the function some network data
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
#' @param snap Logical scalar, whether the layout should be snapped to a grid.
#' @param ... Extra arguments to pass on to the layout algorithm, if necessary.
#' @return A `ggplot2::ggplot()` object.
#'   The last plot can be saved to the file system using `ggplot2::ggsave()`.
#' @importFrom ggraph geom_edge_link geom_node_text geom_conn_bundle
#'   get_con geom_node_point scale_edge_width_continuous geom_node_label
#' @importFrom ggplot2 aes arrow unit scale_color_brewer scale_fill_brewer
#' @importFrom tidygraph mutate activate
#' @examples
#' graphr(ison_adolescents)
#' ison_adolescents %>%
#'   mutate(color = rep(c("introvert","extrovert"), times = 4),
#'          size = ifelse(node_is_cutpoint(ison_adolescents), 6, 3)) %>%
#'   mutate_ties(ecolor = rep(c("friends", "acquaintances"), times = 5)) %>%
#'   graphr(node_color = "color", node_size = "size",
#'          edge_size = 1.5, edge_color = "ecolor")
#' @export
graphr <- function(.data, layout, labels = TRUE,
                   node_color, node_shape, node_size, node_group,
                   edge_color, edge_size, snap = FALSE, ...,
                   node_colour, edge_colour) {
  g <- manynet::as_tidygraph(.data)
  if (missing(layout)) {
    if (manynet::net_nodes(g) <= 6) {
      layout <- "configuration"
    } else if (manynet::is_twomode(g)) {
      layout <- "hierarchy"
    } else layout <- "stress"
  }
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
    g <- manynet::mutate_nodes(g, node_group = reduce_categories(g, node_group))
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
  p <- .graph_layout(g, layout, labels, node_group, snap, ...)
  # Add background ----
  if(getOption("snet_background", default = "#FFFFFF")!="#FFFFFF")
    p <- p + ggplot2::theme(panel.background = ggplot2::element_rect(fill = getOption("snet_background", default = "#FFFFFF")))
  # Add edges ----
  p <- .graph_edges(p, g, edge_color, edge_size, node_size)
  # Add nodes ----
  p <- .graph_nodes(p, g, node_color, node_shape, node_size)
  # Add labels ----
  if (isTRUE(labels) & manynet::is_labelled(g)) {
    p <- .graph_labels(p, g, layout)
  }
  # assign("last.warning", NULL, envir = baseenv()) # to avoid persistent ggrepel
  p
}

.graph_layout <- function(g, layout, labels, node_group, snap, ...) {
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
    x <- y <- NULL
    # thisRequires("ggforce")
    p <- p + 
      ggforce::geom_mark_hull(ggplot2::aes(x, y, fill = node_group,
                                           label = node_group), data = lo) +
      ggplot2::scale_fill_manual(values = ag_qualitative(length(unique(p$data[[node_group]]))),
                                 guide = ggplot2::guide_legend("Group"))
  }
  if(snap){
    manynet::snet_info("Snapping layout coordinates to grid.")
    if(grepl("lattice", 
             igraph::graph_attr(attr(p$data, "graph"), "grand")$name, 
             ignore.case = TRUE))
      p$data[,c("x","y")] <- round(p$data[,c("x","y")])
    else p$data[,c("x","y")] <- depth_first_recursive_search(p)
  }
  p
}

.graph_edges <- function(p, g, edge_color, edge_size, node_size) {
  if (manynet::is_directed(g)) {
    out <- .infer_directed_edge_mapping(g, edge_color, edge_size, node_size)
    p <- map_directed_edges(p, g, out)
  } else {
    out <- .infer_edge_mapping(g, edge_color, edge_size)
    p <- map_edges(p, g, out)
  }
  if (manynet::is_complex(g)) {
    p <- p + ggraph::geom_edge_loop0(edge_alpha = 0.4)
  }
  # Check legends
  if (length(unique(out[["esize"]])) == 1) {
    p <- p + ggplot2::guides(edge_width = "none")
  } else p <- p + ggraph::scale_edge_width_continuous(range = c(0.3, 3),
                                                      guide = ggplot2::guide_legend(
                                                        ifelse(is.null(edge_size) &
                                                                 manynet::is_weighted(g),
                                                               "Edge Weight", "Edge Size")))
  if (length(unique(out[["ecolor"]])) == 1) {
    p <- p + ggplot2::guides(edge_colour = "none")
  } else if (length(unique(out[["ecolor"]])) == 2){
    p <- p + ggraph::scale_edge_colour_manual(values = getOption("snet_highlight", default = c("grey","black")),
                                                   guide = ggplot2::guide_legend(
                                                     ifelse(is.null(edge_color) &
                                                              manynet::is_signed(g),
                                                            "Edge Sign", "Edge Color")))
    } else p <- p + ggraph::scale_edge_colour_manual(values = ag_qualitative(length(unique(out[["ecolor"]]))),
                                                   guide = ggplot2::guide_legend(
                                                     ifelse(is.null(edge_color) &
                                                              manynet::is_signed(g),
                                                            "Edge Sign", "Edge Color")))
  p
}

.graph_nodes <- function(p, g, node_color, node_shape, node_size) {
  out <- .infer_node_mapping(g, node_color, node_size, node_shape)
  if (is.null(node_color) & "Infected" %in% names(manynet::node_attribute(g))) {
    p <- map_infected_nodes(p, g, out)
  } else if (is.null(node_color) & any("diff_model" %in% names(attributes(g)))) {
    p <- map_diff_model_nodes(p, g, out)
  } else {
    p <- map_nodes(p, out)
    # Check legends
    if (length(unique(out[["nsize"]])) > 1)
      p <- p + ggplot2::guides(size = ggplot2::guide_legend(title = "Node Size"))
    if (length(unique(out[["nshape"]])) > 1) 
      p <- p + ggplot2::guides(shape = ggplot2::guide_legend(
        title = ifelse(manynet::is_twomode(g) & is.null(node_shape), "Node Mode", "Node Shape")))
    if (length(unique(out[["ncolor"]])) > 1){
      if(length(unique(out[["ncolor"]])) == 2){
        p <- p + ggplot2::scale_colour_manual(values = getOption("snet_highlight", default = c("grey","black")),
                                              guide = ggplot2::guide_legend("Node Color"))
      } else {
        p <- p + ggplot2::scale_colour_manual(values = ag_qualitative(length(unique(out[["ncolor"]]))),
                                              guide = ggplot2::guide_legend("Node Color"))
      }
    }
  }
  # Consider rescaling nodes
  p <- p + ggplot2::scale_size(range = c(1/manynet::net_nodes(g)*50, 1/manynet::net_nodes(g)*100))
  p
}

.graph_labels <- function(p, g, layout) {
  if (layout == "circle" | layout == "concentric") {
    angles <- as.data.frame(cart2pol(as.matrix(p[["data"]][,1:2])))
    angles$degree <- angles$phi * 180/pi
    angles <- dplyr::case_when(p[["data"]][,2] == 0 & p[["data"]][,1] == 0 ~ 0.1,
                               p[["data"]][,2] >= 0 & p[["data"]][,1] > 0 ~ angles$degree,
                               p[["data"]][,2] < 0 & p[["data"]][,1] > 0 ~ angles$degree,
                               p[["data"]][,1] == 1 ~ angles$degree,
                               TRUE ~ angles$degree - 180)
    if (manynet::net_nodes(g) < 10) {
      hj <- ifelse(p[["data"]][,1] >= 0, -0.8, 1.8)
    } else if (manynet::net_nodes(g) < 20) {
      hj <- ifelse(p[["data"]][,1] >= 0, -0.4, 1.4)
    } else {
      hj <- ifelse(p[["data"]][,1] >= 0, -0.2, 1.2)
    }
    p <- p + ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE,
                                    family = ag_font(),
                                    size = 3, hjust = hj, angle = angles) +
      ggplot2::coord_cartesian(xlim=c(-1.2,1.2), ylim=c(-1.2,1.2))
  } else if (layout %in% c("bipartite", "railway") | layout == "hierarchy" &
             length(unique(p[["data"]][["y"]])) <= 2) {
    p <- p + ggraph::geom_node_text(ggplot2::aes(label = name), angle = 90,
                                    family = ag_font(),
                                    size = 3, hjust = "outward", repel = TRUE,
                                    nudge_y = ifelse(p[["data"]][,2] == 1,
                                                     0.05, -0.05)) +
      ggplot2::coord_cartesian(ylim=c(-0.2, 1.2))
  } else if (layout == "hierarchy" & length(unique(p[["data"]][["y"]])) > 2) {
    p <- p + ggraph::geom_node_text(ggplot2::aes(label = name),
                                    family = ag_font(),
                                    size = 3, hjust = "inward", repel = TRUE)
  } else if (layout %in% c("alluvial", "lineage")) {
    p <- p + ggraph::geom_node_label(ggplot2::aes(label = name), size = 3,
                                     family = ag_font(),
                                     repel = TRUE, nudge_x = ifelse(p[["data"]][,1] == 1, 
                                                                    0.02, -0.02))
  } else {
    p <- p + ggraph::geom_node_label(ggplot2::aes(label = name),
                                     family = ag_font(),
                                     repel = TRUE, seed = 1234, size = 3)
  }
}

# `graphr()` helper functions
reduce_categories <- function(g, node_group) {
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

.infer_directed_edge_mapping <- function(g, edge_color, edge_size, node_size) {
  check_edge_variables(g, edge_color, edge_size)
  list("ecolor" = .infer_ecolor(g, edge_color),
       "esize" = .infer_esize(g, edge_size),
       "line_type" = .infer_line_type(g),
       "end_cap" = .infer_end_cap(g, node_size))
}

.infer_edge_mapping <- function(g, edge_color, edge_size) {
  check_edge_variables(g, edge_color, edge_size)
  list("ecolor" = .infer_ecolor(g, edge_color),
       "esize" = .infer_esize(g, edge_size),
       "line_type" = .infer_line_type(g))
}

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
    out <- factor(ifelse(igraph::E(g)$sign >= 0, "Positive", "Negative"),
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
      out <- manynet::tie_attribute(g, edge_size)
    } else {
      out <- edge_size
    }
  } else if (is.null(edge_size) & manynet::is_weighted(g)) {
    out <- manynet::tie_attribute(g, "weight")
  } else {
    out <- 0.5
  }
  out
}

.infer_end_cap <- function(g, node_size) {
  nsize <- .infer_nsize(g, node_size)/2
  # Accounts for rescaling
  if (length(unique(nsize)) == 1) {
    out <- rep(unique(nsize), manynet::net_ties(g))
  } else {
    out <- g %>%
      tidygraph::activate("edges") %>%
      data.frame() %>% 
      dplyr::left_join(data.frame(node_id = 1:length(manynet::node_names(g)),
                                  nsize = nsize),
                       by = c("to" = "node_id"))
    out <- out$nsize
    out <- ((out - min(out)) / (max(out) - min(out))) *
      ((1 / manynet::net_nodes(g) * 100) - (1 / manynet::net_nodes(g)*50)) + 
      (1 / manynet::net_nodes(g) * 50)
  }
  out
}

.infer_line_type <- function(g) {
  if (manynet::is_signed(g)) {
    out <- ifelse(as.numeric(manynet::tie_signs(g)) >= 0,
           "solid", "dashed")
    # ifelse(length(unique(out)) == 1, unique(out), out)
  } else out <- "solid"
  out
}

check_edge_variables <- function(g, edge_color, edge_size) {
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

map_directed_edges <- function(p, g, out) {
  if (length(out[["ecolor"]]) == 1 & length(out[["esize"]]) == 1) {
    p <- p + ggraph::geom_edge_arc(ggplot2::aes(end_cap = ggraph::circle(c(out[["end_cap"]]), 'mm')),
                                   edge_colour = out[["ecolor"]], edge_width = out[["esize"]],
                                   edge_linetype = out[["line_type"]],
                                   edge_alpha = 0.4, strength = ifelse(igraph::which_mutual(g), 0.2, 0),
                                   arrow = ggplot2::arrow(angle = 15, type = "closed",
                                                          length = ggplot2::unit(2, 'mm')))
  } else if (length(out[["ecolor"]]) > 1 & length(out[["esize"]]) == 1) {
    p <- p + ggraph::geom_edge_arc(ggplot2::aes(edge_colour = out[["ecolor"]],
                                                end_cap = ggraph::circle(c(out[["end_cap"]]), 'mm')),
                                   edge_width = out[["esize"]], edge_linetype = out[["line_type"]],
                                   edge_alpha = 0.4, strength = ifelse(igraph::which_mutual(g), 0.2, 0),
                                   arrow = ggplot2::arrow(angle = 15, type = "closed",
                                                          length = ggplot2::unit(2, 'mm')))
  } else if (length(out[["ecolor"]]) == 1 & length(out[["esize"]]) > 1) {
    p <- p + ggraph::geom_edge_arc(ggplot2::aes(edge_width = out[["esize"]],
                                                end_cap = ggraph::circle(c(out[["end_cap"]]), 'mm')),
                                   edge_colour = out[["ecolor"]], edge_linetype = out[["line_type"]],
                                   edge_alpha = 0.4, strength = ifelse(igraph::which_mutual(g), 0.2, 0),
                                   arrow = ggplot2::arrow(angle = 15, type = "closed",
                                                          length = ggplot2::unit(2, 'mm')))
  } else {
    p <- p + ggraph::geom_edge_arc(ggplot2::aes(edge_colour = getOption("snet_cat")[out[["ecolor"]]],
                                                edge_width = out[["esize"]],
                                                end_cap = ggraph::circle(c(out[["end_cap"]]), 'mm')),
                                   # edge_linetype = out[["line_type"]],
                                   edge_alpha = 0.4, strength = ifelse(igraph::which_mutual(g), 0.2, 0),
                                   arrow = ggplot2::arrow(angle = 15, type = "closed",
                                                          length = ggplot2::unit(2, 'mm')))
  }
  p
}

map_edges <- function(p, g, out) {
  if (length(out[["ecolor"]]) == 1 & length(out[["esize"]]) == 1) {
    p <- p + ggraph::geom_edge_link0(edge_width = out[["esize"]],
                                     edge_colour = out[["ecolor"]],
                                     edge_alpha = 0.4,
                                     edge_linetype = out[["line_type"]])
  } else if (length(out[["ecolor"]]) > 1 & length(out[["esize"]]) == 1) {
    p <- p + ggraph::geom_edge_link0(ggplot2::aes(edge_colour = out[["ecolor"]]),
                                     edge_width = out[["esize"]],
                                     edge_alpha = 0.4,
                                     edge_linetype = out[["line_type"]])
  } else if (length(out[["ecolor"]]) == 1 & length(out[["esize"]]) > 1) {
    p <- p + ggraph::geom_edge_link0(ggplot2::aes(edge_width = out[["esize"]]),
                                     edge_colour = out[["ecolor"]],
                                     edge_alpha = 0.4,
                                     edge_linetype = out[["line_type"]])
  } else {
    p <- p + ggraph::geom_edge_link0(ggplot2::aes(edge_width = out[["esize"]],
                                                  edge_colour = out[["ecolor"]]),
                                     edge_alpha = 0.4, edge_linetype = out[["line_type"]])
  }
}

.infer_node_mapping <- function(g, node_color, node_size, node_shape) {
  check_node_variables(g, node_color, node_size)
  list("nshape" = .infer_nshape(g, node_shape),
       "nsize" = .infer_nsize(g, node_size),
       "ncolor" = .infer_ncolor(g, node_color))
}

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
    out <- ifelse(igraph::V(g)$type, "One", "Two")
  } else {
    out <- "circle"
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

check_node_variables <- function(g, node_color, node_size) {
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

map_infected_nodes<- function(p, g, out) {
  node_color <- as.factor(ifelse(manynet::node_attribute(g, "Exposed"), "Exposed",
                                 ifelse(manynet::node_attribute(g, "Infected"),"Infected", 
                                        ifelse(manynet::node_attribute(g, "Recovered"), "Recovered",
                                               "Susceptible"))))
  p + ggraph::geom_node_point(ggplot2::aes(color = node_color),
                              size = out[["nsize"]], shape = out[["nshape"]]) +
    ggplot2::scale_color_manual(name = NULL, guide = ggplot2::guide_legend(""),
                                values = c("Infected" = "#d73027",
                                           "Susceptible" = "#4575b4",
                                           "Exposed" = "#E6AB02",
                                           "Recovered" = "#66A61E"))
}

map_diff_model_nodes <- function(p, g, out) {
  node_adopts <- manynet::node_adoption_time(attr(g,"diff_model"))
  nshape <- ifelse(node_adopts == min(node_adopts), "Seed(s)",
                   ifelse(node_adopts == Inf, "Non-Adopter", "Adopter"))
  node_color <- ifelse(is.infinite(node_adopts), 
                       max(node_adopts[!is.infinite(node_adopts)]) + 1, 
                       node_adopts)
  p + ggraph::geom_node_point(ggplot2::aes(shape = nshape, color = node_color),
                              size = out[["nsize"]]) +
    ggplot2::scale_color_gradient(low = "#d73027", high = "#4575b4",
                                  breaks=c(min(node_color)+1, 
                                           ifelse(any(nshape=="Non-Adopter"),
                                                  max(node_color)-1,
                                                  max(node_color))),
                                  labels=c("Early\nadoption", "Late\nadoption"),
                                  name = "Time of\nAdoption\n") +
    ggplot2::scale_shape_manual(name = "",
                                breaks = c("Seed(s)", "Adopter", "Non-Adopter"),
                                values = c("Seed(s)" = "triangle",
                                           "Adopter" = "circle",
                                           "Non-Adopter" = "square")) +
    ggplot2::guides(color = ggplot2::guide_colorbar(order = 1, reverse = TRUE),
                    shape = ggplot2::guide_legend(order = 2))
}

map_nodes <- function(p, out) {
  if (length(out[["ncolor"]]) == 1 & length(out[["nsize"]]) == 1 & length(out[["nshape"]]) == 1) {
    p <- p + ggraph::geom_node_point(colour = out[["ncolor"]], size = out[["nsize"]],
                                     shape = out[["nshape"]])
  } else if (length(out[["ncolor"]]) > 1 & length(out[["nsize"]]) == 1 & length(out[["nshape"]]) == 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(colour = out[["ncolor"]]), 
                                     size = out[["nsize"]], shape = out[["nshape"]])
  } else if (length(out[["ncolor"]]) == 1 & length(out[["nsize"]]) > 1 & length(out[["nshape"]]) == 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(size = out[["nsize"]]),
                                     colour = out[["ncolor"]], shape = out[["nshape"]])
  } else if (length(out[["ncolor"]]) == 1 & length(out[["nsize"]]) == 1 & length(out[["nshape"]]) > 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(shape = out[["nshape"]]),
                                     colour = out[["ncolor"]], size = out[["nsize"]])
  } else if (length(out[["ncolor"]]) > 1 & length(out[["nsize"]]) > 1 & length(out[["nshape"]]) == 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(colour = out[["ncolor"]], size = out[["nsize"]]),
                                     shape = out[["nshape"]])
  } else if (length(out[["ncolor"]]) > 1 & length(out[["nsize"]]) == 1 & length(out[["nshape"]]) > 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(colour = out[["ncolor"]], shape = out[["nshape"]]),
                                     size = out[["nsize"]])
  } else if (length(out[["ncolor"]]) == 1 & length(out[["nsize"]]) > 1 & length(out[["nshape"]]) > 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(size = out[["nsize"]], shape = out[["nshape"]]),
                                     colour = out[["ncolor"]])
  } else {
    p <- p + ggraph::geom_node_point(ggplot2::aes(colour = out[["ncolor"]],
                                                  shape = out[["nshape"]],
                                                  size = out[["nsize"]]))
  }
  p
}


cart2pol <- function(xyz){
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
  r <- hypot(x, y)
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

hypot <- function (x, y) {
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

