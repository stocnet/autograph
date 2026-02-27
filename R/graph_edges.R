graph_edges <- function(p, g, edge_color, edge_size, node_size) {
  if (manynet::is_directed(g)) {
    out <- .infer_directed_edge_mapping(g, edge_color, edge_size, node_size)
    p <- .map_directed_edges(p, g, out)
  } else {
    out <- .infer_edge_mapping(g, edge_color, edge_size)
    p <- .map_edges(p, g, out)
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
                                                               "Weight", "Width")))
  if (length(unique(out[["ecolor"]])) == 1) {
    p <- p + ggplot2::guides(edge_colour = "none")
  } else if (length(unique(out[["ecolor"]])) == 2){
    p <- p + ggraph::scale_edge_colour_manual(values = getOption("snet_highlight", default = c("grey","black")),
                                              guide = ggplot2::guide_legend(
                                                ifelse(is.null(edge_color) &
                                                         manynet::is_signed(g),
                                                       "Sign", edge_color)))
  } else p <- p + ggraph::scale_edge_colour_manual(values = ag_qualitative(length(unique(out[["ecolor"]]))),
                                                   guide = ggplot2::guide_legend(
                                                     ifelse(is.null(edge_color) &
                                                              manynet::is_signed(g),
                                                            "Sign", edge_color)))
  p
}

# Helper functions for .graph_edges()

.infer_directed_edge_mapping <- function(g, edge_color, edge_size, node_size) {
  .check_edge_variables(g, edge_color, edge_size)
  list("ecolor" = .infer_ecolor(g, edge_color),
       "esize" = .infer_esize(g, edge_size),
       "line_type" = .infer_line_type(g),
       "end_cap" = .infer_end_cap(g, node_size))
}

.infer_edge_mapping <- function(g, edge_color, edge_size) {
  .check_edge_variables(g, edge_color, edge_size)
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

.map_directed_edges <- function(p, g, out) {
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

.map_edges <- function(p, g, out) {
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
