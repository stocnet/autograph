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
  # When linetype varies across ties (signed networks) it is mapped through
  # aes() as literal "solid"/"dashed" strings, so an identity scale is needed to
  # use them verbatim. Sign is already labelled by the colour legend, so no
  # separate linetype legend is drawn.
  if (length(unique(out[["line_type"]])) > 1) {
    p <- p + ggraph::scale_edge_linetype_identity()
  }
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

# Route the three vectorisable edge aesthetics (colour, width, linetype) either
# through aes() -- when they vary across ties, so ggraph's edge stats expand and
# subset them alongside the geometry (point expansion in geom_edge_arc, loop
# removal, faceting) -- or as a constant layer parameter when they are a single
# value. Passing a per-tie vector as a constant parameter is what breaks signed
# multiplex/longitudinal networks: it recycles against the wrong length or feeds
# NA/malformed values straight to grid ("invalid hex digit in 'color' or 'lty'").
.split_edge_aes <- function(out) {
  # `mapping` holds unevaluated expressions (not the vectors themselves) so that
  # do.call(aes, mapping) captures them as quosures resolved lazily against
  # `out` in the caller's environment -- the same way the aesthetics were
  # written literally before -- rather than as pre-evaluated constants.
  keys <- c(ecolor = "edge_colour", esize = "edge_width", line_type = "edge_linetype")
  exprs <- list(ecolor    = quote(out[["ecolor"]]),
                esize     = quote(out[["esize"]]),
                line_type = quote(out[["line_type"]]))
  mapping <- list(); params <- list()
  for (nm in names(keys)) {
    if (length(out[[nm]]) > 1) mapping[[keys[[nm]]]] <- exprs[[nm]]
    else params[[keys[[nm]]]] <- out[[nm]]
  }
  list(mapping = mapping, params = params)
}

.map_directed_edges <- function(p, g, out) {
  parts <- .split_edge_aes(out)
  parts$mapping$end_cap <- quote(ggraph::circle(c(out[["end_cap"]]), 'mm'))
  args <- c(list(mapping = do.call(ggplot2::aes, parts$mapping),
                 edge_alpha = 0.4,
                 strength = .infer_arc_strength(g),
                 arrow = .infer_arrow(out[["esize"]])),
            parts$params)
  p + do.call(ggraph::geom_edge_arc, args)
}

.infer_arc_strength <- function(g) {
  # `geom_edge_arc()` reciprocated dyads apart (0.2) and draws single ties
  # straight (0). Its stat removes self-loops before drawing (loops are drawn
  # separately by `geom_edge_loop0()`), but `strength` is a length-preserving
  # parameter rather than an aesthetic, so it must exclude loop edges. Otherwise
  # a full-length (net_ties) vector recycles against the loop-free edge set and
  # emits "longer object length is not a multiple" warnings on complex networks.
  strength <- ifelse(igraph::which_mutual(g), 0.2, 0)
  strength[!igraph::which_loop(g)]
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
  # undirected networks omit them. Colour/width mapping is preserved to the
  # extent the geom's aesthetics allow.
  arrow <- if (directed) .infer_arrow(out[["esize"]]) else NULL
  parts <- .split_edge_aes(out)
  # Bundling merges edges into shared paths whose stat inserts NA-separated
  # break points, so a per-tie linetype cannot be represented (the NAs reach
  # grid as invalid linetypes). Drop a varying linetype and draw bundles solid;
  # a linetype shared by every tie is already in `parts$params` and is kept.
  parts$mapping[["edge_linetype"]] <- NULL
  args <- c(list(edge_alpha = 0.4, arrow = arrow), parts$params)
  if (length(parts$mapping)) args$mapping <- do.call(ggplot2::aes, parts$mapping)
  p + do.call(bundle_geom, args)
}

.map_edges <- function(p, g, out) {
  parts <- .split_edge_aes(out)
  args <- c(list(edge_alpha = 0.4), parts$params)
  if (length(parts$mapping)) args$mapping <- do.call(ggplot2::aes, parts$mapping)
  p + do.call(ggraph::geom_edge_link0, args)
}
