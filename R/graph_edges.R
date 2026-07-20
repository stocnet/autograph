graph_edges <- function(p, g, edge_color, edge_size, node_size,
                        edge_bundle = FALSE) {
  bundle_geom <- .infer_bundle_geom(edge_bundle)
  if (manynet::is_directed(g)) {
    out <- .infer_directed_edge_mapping(g, edge_color, edge_size, node_size)
    if (is.null(bundle_geom)) {
      p <- .map_directed_edges(p, g, out)
    } else {
      p <- .map_bundled_edges(p, g, out, bundle_geom, directed = TRUE)
    }
  } else {
    out <- .infer_edge_mapping(g, edge_color, edge_size)
    if (is.null(bundle_geom)) {
      p <- .map_edges(p, g, out)
    } else {
      p <- .map_bundled_edges(p, g, out, bundle_geom, directed = FALSE)
    }
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

# .infer_ecolor/.infer_esize/.infer_arrow/.infer_line_type/.check_edge_variables
# live in R/graph_aes.R, shared with grapht().

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

.map_directed_edges <- function(p, g, out) {
  if (length(out[["ecolor"]]) == 1 & length(out[["esize"]]) == 1) {
    p <- p + ggraph::geom_edge_arc(ggplot2::aes(end_cap = ggraph::circle(c(out[["end_cap"]]), 'mm')),
                                   edge_colour = out[["ecolor"]], edge_width = out[["esize"]],
                                   edge_linetype = out[["line_type"]],
                                   edge_alpha = 0.4, strength = ifelse(igraph::which_mutual(g), 0.2, 0),
                                   arrow = .infer_arrow(out[["esize"]]))
  } else if (length(out[["ecolor"]]) > 1 & length(out[["esize"]]) == 1) {
    p <- p + ggraph::geom_edge_arc(ggplot2::aes(edge_colour = out[["ecolor"]],
                                                end_cap = ggraph::circle(c(out[["end_cap"]]), 'mm')),
                                   edge_width = out[["esize"]], edge_linetype = out[["line_type"]],
                                   edge_alpha = 0.4, strength = ifelse(igraph::which_mutual(g), 0.2, 0),
                                   arrow = .infer_arrow(out[["esize"]]))
  } else if (length(out[["ecolor"]]) == 1 & length(out[["esize"]]) > 1) {
    p <- p + ggraph::geom_edge_arc(ggplot2::aes(edge_width = out[["esize"]],
                                                end_cap = ggraph::circle(c(out[["end_cap"]]), 'mm')),
                                   edge_colour = out[["ecolor"]], edge_linetype = out[["line_type"]],
                                   edge_alpha = 0.4, strength = ifelse(igraph::which_mutual(g), 0.2, 0),
                                   arrow = .infer_arrow(out[["esize"]]))
  } else {
    p <- p + ggraph::geom_edge_arc(ggplot2::aes(edge_colour = getOption("snet_cat")[out[["ecolor"]]],
                                                edge_width = out[["esize"]],
                                                end_cap = ggraph::circle(c(out[["end_cap"]]), 'mm')),
                                   # edge_linetype = out[["line_type"]],
                                   edge_alpha = 0.4, strength = ifelse(igraph::which_mutual(g), 0.2, 0),
                                   arrow = .infer_arrow(out[["esize"]]))
  }
  p
}

.infer_bundle_geom <- function(edge_bundle) {
  # Resolves the `edge_bundle` argument to one of ggraph's edge-bundling geom
  # functions, or NULL when bundling is switched off. `TRUE` is treated as the
  # force-directed default; a string selects a specific algorithm.
  if (is.null(edge_bundle) || isFALSE(edge_bundle)) return(NULL)
  type <- if (isTRUE(edge_bundle)) "force" else tolower(as.character(edge_bundle))
  type <- match.arg(type, c("force", "path", "minimal"))
  switch(type,
         force = ggraph::geom_edge_bundle_force,
         path = ggraph::geom_edge_bundle_path,
         minimal = ggraph::geom_edge_bundle_minimal)
}

.map_bundled_edges <- function(p, g, out, bundle_geom, directed = FALSE) {
  # Edge-bundling geoms draw paths that are pulled together into bundles, so the
  # arc `strength`/`end_cap` treatment used for straight/arced edges does not
  # apply. Directed networks keep arrowheads (scaled via `.infer_arrow()`);
  # undirected networks omit them. Colour/width/linetype mapping is preserved
  # to the extent the geom's aesthetics allow.
  arrow <- if (directed) .infer_arrow(out[["esize"]]) else NULL
  if (length(out[["ecolor"]]) == 1 & length(out[["esize"]]) == 1) {
    p <- p + bundle_geom(edge_colour = out[["ecolor"]], edge_width = out[["esize"]],
                         edge_linetype = out[["line_type"]],
                         edge_alpha = 0.4, arrow = arrow)
  } else if (length(out[["ecolor"]]) > 1 & length(out[["esize"]]) == 1) {
    p <- p + bundle_geom(ggplot2::aes(edge_colour = out[["ecolor"]]),
                         edge_width = out[["esize"]], edge_linetype = out[["line_type"]],
                         edge_alpha = 0.4, arrow = arrow)
  } else if (length(out[["ecolor"]]) == 1 & length(out[["esize"]]) > 1) {
    p <- p + bundle_geom(ggplot2::aes(edge_width = out[["esize"]]),
                         edge_colour = out[["ecolor"]], edge_linetype = out[["line_type"]],
                         edge_alpha = 0.4, arrow = arrow)
  } else {
    p <- p + bundle_geom(ggplot2::aes(edge_colour = out[["ecolor"]],
                                      edge_width = out[["esize"]]),
                         edge_alpha = 0.4, arrow = arrow)
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
