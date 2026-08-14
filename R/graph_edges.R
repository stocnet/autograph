graph_edges <- function(p, g, edge_color, edge_size, node_size,
                        edge_bundle = FALSE, layout = NULL) {
  bundle_geom <- .infer_bundle_geom(edge_bundle)
  if (manynet::is_directed(g)) {
    out <- .infer_directed_edge_mapping(g, edge_color, edge_size, node_size,
                                        layout)
    if (is.null(bundle_geom)) {
      p <- .map_directed_edges(p, g, out)
    } else {
      p <- .map_bundled_edges(p, g, out, bundle_geom, directed = TRUE)
    }
  } else {
    out <- .infer_edge_mapping(g, edge_color, edge_size, layout)
    if (is.null(bundle_geom)) {
      p <- .map_edges(p, g, out)
    } else {
      p <- .map_bundled_edges(p, g, out, bundle_geom, directed = FALSE)
    }
  }
  if (manynet::is_complex(g)) {
    # Resolved here rather than inside aes(), which would evaluate it lazily
    # against whatever `p` held by the time the plot was built.
    loop_strength <- .infer_loop_strength(p)
    p <- p + ggraph::geom_edge_loop0(ggplot2::aes(strength = loop_strength),
                                     edge_alpha = 0.4)
  }
  # Check legends
  if (length(unique(out[["esize"]])) == 1) {
    p <- p + ggplot2::guides(edge_width = "none")
  } else p <- p + ggraph::scale_edge_width_continuous(range = c(0.3, 3),
                                                      guide = ggplot2::guide_legend(
                                                        ifelse(is.null(edge_size) &
                                                                 manynet::is_weighted(g),
                                                               "Weight", "Width")))
  ecolor_title <- .infer_ecolor_title(g, edge_color)
  if (length(unique(out[["ecolor"]])) == 1) {
    p <- p + ggplot2::guides(edge_colour = "none")
  } else if (length(unique(out[["ecolor"]])) == 2){
    p <- p + ggraph::scale_edge_colour_manual(values = getOption("snet_highlight", default = c("grey","black")),
                                              guide = ggplot2::guide_legend(ecolor_title))
  } else p <- p + ggraph::scale_edge_colour_manual(values = ag_qualitative(length(unique(out[["ecolor"]]))),
                                                   guide = ggplot2::guide_legend(ecolor_title))
  # When linetype varies across ties (signed networks) it is mapped through
  # aes() as literal "solid"/"dashed" strings, so an identity scale is needed to
  # use them verbatim. Such a scale draws no legend by default, which was right
  # while the colours said "Sign" too, but leaves the dashes unexplained now
  # that they may be the only thing showing the sign.
  if (length(unique(out[["line_type"]])) > 1) {
    if (identical(ecolor_title, "Sign")) {
      p <- p + ggraph::scale_edge_linetype_identity()
    } else {
      p <- p + ggraph::scale_edge_linetype_identity(
        name = "Sign", guide = "legend",
        breaks = c("solid", "dashed"), labels = c("Positive", "Negative"))
    }
  }
  p
}

# Helper functions for .graph_edges()

.infer_directed_edge_mapping <- function(g, edge_color, edge_size, node_size,
                                         layout = NULL) {
  list("ecolor" = .infer_ecolor(g, edge_color),
       "esize" = .infer_esize(g, edge_size),
       "line_type" = .infer_line_type(g),
       "ealpha" = .infer_ealpha(g, layout),
       "end_cap" = .infer_end_cap(g, node_size, layout))
}

.infer_edge_mapping <- function(g, edge_color, edge_size, layout = NULL) {
  list("ecolor" = .infer_ecolor(g, edge_color),
       "esize" = .infer_esize(g, edge_size),
       "line_type" = .infer_line_type(g),
       "ealpha" = .infer_ealpha(g, layout))
}

# .infer_ecolor/.infer_esize/.infer_arrow/.infer_line_type live in
# R/graph_aes.R, shared with grapht(). These arguments have already been checked
# against the network's attributes by graphr()/grapht() (see R/graph_checks.R).

# A self-loop's `strength` is its diameter, measured in the same units as the
# layout's coordinates, and `geom_edge_loop0()` defaults it to 1. Since layouts
# differ by orders of magnitude in how far their coordinates spread, that one
# number draws a loop that is either invisible or, as for the "multilevel"
# layout whose coordinates span about one unit in each direction, a circle
# wider than the network it belongs to -- which then stretches the panel to
# fit, leaving the plot squeezed against its legend. Sized as a fraction of
# the layout instead, a loop reads as a loop whatever the layout.
#
# Note that `strength` is an aesthetic of the loop geoms rather than a layer
# parameter: passed as a parameter it is silently dropped ("Ignoring unknown
# parameters"), so it has to be mapped through `aes()`.
.infer_loop_strength <- function(p) {
  spread <- max(diff(range(p[["data"]][["x"]], na.rm = TRUE)),
                diff(range(p[["data"]][["y"]], na.rm = TRUE)))
  # A network drawn at a single point has no spread to take a fraction of.
  if (!is.finite(spread) || spread <= 0) return(1)
  spread * 0.06
}

# A multilevel layout draws each level as a plane, and in an interlocking
# network the ties running between those planes typically outnumber the ties
# within them: `fict_marvel` has 683 against 558. Drawn at the same strength
# they curtain over both planes, so they are faded well back, and the ties
# within each level brought forward, so that the structure of each level and
# the shape of the interlock can both be seen.
.infer_ealpha <- function(g, layout = NULL) {
  if (identical(layout, "multilevel") && manynet::is_twomode(g) &&
      manynet::net_ties(g) > 0)
    ifelse(manynet::tie_is_twomode(g), 0.08, 0.5) else 0.4
}

.infer_end_cap <- function(g, node_size, layout = NULL) {
  nsize <- .infer_nsize(g, node_size, layout)/2
  # Accounts for rescaling
  if (length(unique(nsize)) == 1) {
    out <- rep(unique(nsize), manynet::net_ties(g))
  } else {
    # Each tie's end cap is sized from the node it points to, so index the node
    # sizes by the edgelist's target column (kept as node indices, not names).
    out <- nsize[igraph::as_edgelist(manynet::as_igraph(g), names = FALSE)[, 2]]
    out <- ((out - min(out)) / (max(out) - min(out))) *
      ((1 / manynet::net_nodes(g) * 100) - (1 / manynet::net_nodes(g)*50)) + 
      (1 / manynet::net_nodes(g) * 50)
  }
  out
}

# Route the four vectorisable edge aesthetics (colour, width, linetype, alpha)
# either through aes() -- when they vary across ties, so ggraph's edge stats
# expand and subset them alongside the geometry (point expansion in
# geom_edge_arc, loop removal, faceting) -- or as a constant layer parameter
# when they are a single value. Passing a per-tie vector as a constant parameter
# is what breaks signed multiplex/longitudinal networks: it recycles against the
# wrong length or feeds NA/malformed values straight to grid ("invalid hex digit
# in 'color' or 'lty'").
.split_edge_aes <- function(out) {
  # `mapping` holds unevaluated expressions (not the vectors themselves) so that
  # do.call(aes, mapping) captures them as quosures resolved lazily against
  # `out` in the caller's environment -- the same way the aesthetics were
  # written literally before -- rather than as pre-evaluated constants.
  keys <- c(ecolor = "edge_colour", esize = "edge_width",
            line_type = "edge_linetype", ealpha = "edge_alpha")
  exprs <- list(ecolor    = quote(out[["ecolor"]]),
                esize     = quote(out[["esize"]]),
                line_type = quote(out[["line_type"]]),
                ealpha    = quote(out[["ealpha"]]))
  mapping <- list(); params <- list()
  for (nm in names(keys)) {
    if (length(out[[nm]]) > 1) mapping[[keys[[nm]]]] <- exprs[[nm]]
    else params[[keys[[nm]]]] <- out[[nm]]
  }
  list(mapping = mapping, params = params)
}

# A varying alpha is mapped through aes() as the literal values themselves, so
# an identity scale is needed to use them verbatim. They distinguish the levels
# of a multilevel layout, which the layout already makes plain, so no alpha
# legend is drawn.
.scale_edge_aes <- function(p, parts) {
  if ("edge_alpha" %in% names(parts$mapping))
    p <- p + ggraph::scale_edge_alpha_identity()
  p
}

.map_directed_edges <- function(p, g, out) {
  parts <- .split_edge_aes(out)
  parts$mapping$end_cap <- quote(ggraph::circle(c(out[["end_cap"]]), 'mm'))
  args <- c(list(mapping = do.call(ggplot2::aes, parts$mapping),
                 strength = .infer_arc_strength(g),
                 arrow = .infer_arrow(out[["esize"]])),
            parts$params)
  .scale_edge_aes(p + do.call(ggraph::geom_edge_arc, args), parts)
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
  type <- .check_choice(type, c("force", "path", "minimal"), "edge_bundle")
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
  # A per-tie alpha cannot survive that merging either, so bundles are drawn at
  # the usual constant instead.
  parts$mapping[["edge_linetype"]] <- NULL
  if ("edge_alpha" %in% names(parts$mapping)) {
    parts$mapping[["edge_alpha"]] <- NULL
    parts$params[["edge_alpha"]] <- 0.4
  }
  args <- c(list(arrow = arrow), parts$params)
  if (length(parts$mapping)) args$mapping <- do.call(ggplot2::aes, parts$mapping)
  p + do.call(bundle_geom, args)
}

.map_edges <- function(p, g, out) {
  parts <- .split_edge_aes(out)
  args <- parts$params
  if (length(parts$mapping)) args$mapping <- do.call(ggplot2::aes, parts$mapping)
  .scale_edge_aes(p + do.call(ggraph::geom_edge_link0, args), parts)
}
