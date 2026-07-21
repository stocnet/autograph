# Dynamic networks ####

#' Easily animate dynamic networks with sensible defaults
#' @description
#'   This function provides users with an easy way to graph
#'   dynamic network data for exploration and presentation.
#'
#'   It builds upon this package's `graphr()` function,
#'   and inherits all the same features and arguments.
#'   See `graphr()` for more.
#'   However, it uses the `{gganimate}` package to animate the changes
#'   between successive iterations of a network.
#'   This is useful for networks in which the ties and/or the node or tie
#'   attributes are changing, including networks whose node composition
#'   changes over time: every node that ever appears is assigned a stable
#'   position, and nodes fade in and out in place as they enter and exit
#'   the network.
#'
#'   By default node positions transition smoothly between waves using
#'   the dynamic stress layout from `{graphlayouts}`
#'   (`graphlayouts::layout_as_dynamic()`), which anchors each wave's
#'   layout to a reference layout of the aggregate network.
#'   The `alpha` argument controls this trade-off:
#'   0 lets each wave's layout follow that wave's structure freely,
#'   while 1 freezes every node at its aggregate position.
#'   When another `layout` is requested,
#'   a single static layout is computed on the aggregate
#'   (union of waves) network instead, so that positions remain constant.
#'   Unlike `graphr()`, `grapht()` uses this dynamic stress layout by default
#'   even for two-mode networks (rather than a hierarchy layout, which would
#'   collapse many nodes onto a line); the two modes remain distinguishable
#'   by node shape.
#'   For networks with more than 30 nodes, node labels are suppressed by
#'   default to keep frames legible; pass `labels = TRUE` to force them.
#'
#'   `grapht()` returns a `{ggplot2}`-compatible object that can be
#'   extended with additional layers such as `ggplot2::labs()`,
#'   `ggplot2::theme()`, scale functions, and others,
#'   just like plots produced by `graphr()` and `graphs()`.
#'   The animation is rendered when the object is printed or displayed.
#'   Users who want more control over animation parameters can call
#'   `gganimate::animate()` directly on the returned object.
#'
#'   The visual appearance is consistent with `graphr()`:
#'   nodes use fillable shapes with the fill aesthetic,
#'   the same colour palettes are applied,
#'   directed networks receive arrowheads,
#'   signed networks distinguish positive from negative ties by linetype,
#'   and labels use the current theme font.
#'   Legends transition along with the mapped aesthetics.
#'
#'   A progress bar is shown if it takes some time to encode all the
#'   .png files into a .gif.
#' @name plot_grapht
#' @family mapping
#' @param tlist A manynet-compatible network listed according to
#'   a time attribute, waves, or slices.
#'   This can also be a single manynet network object that encodes time,
#'   which will be split automatically:
#'   longitudinal or changing networks are split into waves via
#'   `manynet::to_waves()`; dynamic (time-stamped, event-based) networks
#'   such as `manynet::irps_nuclear` into cumulative time slices via
#'   `manynet::to_slices()`; and interval (spell) networks that record tie
#'   `begin`/`end` lifespans, such as `manynet::irps_wwi`, into one snapshot
#'   per change point showing the ties active in that spell.
#'   It can also be a diffusion model result from e.g.
#'   `manynet::play_diffusion()`.
#' @param isolates One of `"keep"` (the default) or `"fade"`.
#'   `"keep"` retains isolated nodes at their layout positions in every
#'   wave in which they are present.
#'   `"fade"` fades nodes out during waves in which they are isolates,
#'   and fades them back in when they regain ties.
#'   Nodes that are absent from a wave altogether
#'   (composition change) always fade out.
#' @param alpha A number between 0 and 1 controlling the stability of
#'   node positions across waves when the default dynamic (stress) layout
#'   is used. 0 computes each wave's layout freely,
#'   1 fixes all nodes at their aggregate-network positions.
#'   By default 0.5. Passed to `graphlayouts::layout_as_dynamic()`.
#' @param keep_isolates Deprecated. Use `isolates = "keep"` or
#'   `isolates = "fade"` instead.
#' @details
#'   Unlike `graphr()`, `grapht()` does not use `ggrepel`-based label
#'   repelling (there is no straightforward way to repel labels consistently
#'   across animation frames), so `label_repel` here instead toggles a fixed
#'   offset nudging labels away from their nodes, and `label_dist` scales the
#'   size of that nudge rather than being used as repel padding.
#'
#'   Some further `graphr()` features are not available in animations:
#'   `node_group` hulls, edge bundling, curved arcs for reciprocated ties,
#'   and self-loops (loops are not drawn; a note is printed if present).
#' @inheritParams plot_graphr
#' @importFrom igraph as_data_frame vcount add_vertices permute
#' @importFrom igraph vertex_attr_names delete_vertex_attr delete_edge_attr 
#' @importFrom igraph set_vertex_attr graph_from_data_frame delete_graph_attr
#' @importFrom ggplot2 ggplot geom_segment geom_point geom_text coord_fixed .data
#' @importFrom ggplot2 scale_alpha_identity scale_linetype_identity theme_void 
#' @importFrom dplyr mutate select distinct left_join %>%
#' @source https://blog.schochastics.net/posts/2021-09-15_animating-network-evolutions-with-gganimate/
#' @return A `{ggplot2}`-compatible object with `{gganimate}` animation layers.
#'   This object can be extended with additional `{ggplot2}` layers
#'   (e.g. `+ labs(subtitle = "My subtitle")`).
#'   When printed or displayed, the animation is rendered as a .gif.
#'   For more control over animation parameters,
#'   pass the result to `gganimate::animate()` directly.
#' @examples
#' # A dynamic signed network of shifting European alliances 1872-1918,
#' # split automatically into snapshots of the ties active in each spell.
#' # Wrapped in \donttest{} because rendering the animation to a .gif is
#' # slow, not because the code is unsafe to run.
#' \donttest{
#' grapht(irps_wwi)
#' }
#' @export
grapht <- function(tlist, layout = NULL, labels = TRUE,
                   node_color, node_shape, node_size,
                   edge_color, edge_size,
                   isolates = c("keep", "fade"),
                   alpha = 0.5,
                   label_dist = NULL, label_repel = TRUE,
                   keep_isolates = NULL, ...,
                   node_colour, edge_colour) {
  thisRequires("gganimate")
  thisRequires("gifski")
  # Check arguments ####
  labels_missing <- missing(labels)
  isolates_missing <- missing(isolates)
  isolates <- match.arg(isolates)
  if (!is.null(keep_isolates)) {
    warning("The `keep_isolates` argument of `grapht()` is deprecated; ",
            "please use `isolates = \"keep\"` or `isolates = \"fade\"` instead.",
            call. = FALSE)
    if (isolates_missing) isolates <- ifelse(isTRUE(keep_isolates), "keep", "fade")
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

  # Normalise input into harmonised waves over the union of nodes ####
  wl <- .grapht_waves(tlist)
  if (!wl$has_names) labels <- FALSE
  waves <- wl$waves
  frames <- wl$frames
  g_ref <- waves[[1]]
  # Node labels become illegible once there are many nodes, so suppress them
  # by default for large networks (the user can still force `labels = TRUE`).
  if (labels_missing && isTRUE(labels) && igraph::vcount(g_ref) > 30) {
    labels <- FALSE
    manynet::snet_info("Suppressing node labels for a network with more than 30 nodes; set `labels = TRUE` to show them.")
  }
  .check_node_variables(g_ref, node_color, node_size)
  .check_edge_variables(g_ref, edge_color, edge_size)
  if (manynet::is_complex(manynet::as_igraph(g_ref)))
    manynet::snet_info("Self-loops are not drawn in animations.")

  # Layout ####
  # Default to the smooth dynamic stress layout regardless of mode or size:
  # grapht()'s purpose is seamless transitions, so unlike graphr() it does
  # not fall back to a static hierarchy for two-mode networks (which collapses
  # many nodes onto a line). The two modes remain distinguishable by shape.
  # An explicitly requested layout is still honoured (via a static fallback).
  if (is.null(layout)) layout <- "stress"
  layouts <- .grapht_layout(waves, layout, alpha, ...)

  # Frame data ####
  nodes_out <- .grapht_node_frames(waves, layouts, frames, wl$present,
                                   wl$isolated, node_color, node_shape,
                                   node_size, isolates)
  edges_out <- .grapht_edge_frames(waves, layouts, frames,
                                   edge_color, edge_size)
  if (isolates == "keep" &&
      igraph::vcount(g_ref) > 30 && any(wl$isolated & wl$present, na.rm = TRUE)) {
    manynet::snet_info("Please consider fading isolates (`isolates = \"fade\"`) to improve visualisation.")
  }

  # Plot with ggplot2 and animate with gganimate ####
  frame <- NULL
  p <- .grapht_build(nodes_out, edges_out, g_ref, labels,
                     node_color, node_shape, node_size,
                     edge_color, edge_size,
                     label_dist, label_repel) +
    gganimate::transition_states(states = frame, transition_length = 5,
                                 state_length = 10, wrap = FALSE) +
    gganimate::enter_fade() + gganimate::enter_grow() +
    gganimate::exit_fade() + gganimate::exit_shrink() +
    ggplot2::labs(title = "{closest_state}")
  # Return extensible ggplot+gganimate object ####
  attr(p, "nwaves") <- length(waves)
  class(p) <- c("grapht", class(p))
  p
}

# nocov start
#' @rdname plot_grapht
#' @param x A grapht object to print.
#' @export
print.grapht <- function(x, ...) {
  nwaves <- if (!is.null(attr(x, "nwaves"))) attr(x, "nwaves") else 5
  anim <- gganimate::animate(x, duration = 2 * nwaves,
                              start_pause = 5, end_pause = 10,
                              renderer = gganimate::gifski_renderer())
  print(anim)
}
# nocov end

# Helper functions for grapht() ----

# Coerces the input into a list of tidygraph waves that all share the same
# vertex set (the union of nodes ever observed) in the same order, recording
# which nodes were actually present, and which were isolates, in each wave.
.grapht_waves <- function(tlist) {
  # A single manynet object is split into a list of snapshots depending on how
  # it encodes time: diffusion results and changing or longitudinal networks
  # split into waves; dynamic (time-stamped, event-based) networks such as
  # `irps_nuclear` split into cumulative time slices; and interval/spell
  # networks that record tie `begin`/`end` spells (e.g. `irps_wwi`) split into
  # per-period snapshots of the ties active in each spell.
  if (!manynet::is_list(tlist) && manynet::is_manynet(tlist)) {
    if (inherits(tlist, "diff_model") ||
        manynet::is_changing(tlist) || manynet::is_longitudinal(tlist)) {
      tlist <- manynet::to_waves(tlist)
    } else if (.grapht_is_spell(tlist)) {
      tlist <- .grapht_spell_slices(tlist)
    } else if (manynet::is_dynamic(tlist)) {
      tlist <- manynet::to_slices(tlist)
    }
  } else if (inherits(tlist, "diff_model")) {
    tlist <- manynet::to_waves(tlist)
  }
  # If the input is still a single network at this point, none of the
  # splitting strategies above applied; iterating over it would decompose
  # the graph object itself and crash much later with a cryptic error.
  if (!manynet::is_list(tlist)) {
    manynet::snet_abort(paste(
      "Please declare a manynet-compatible network listed according",
      "to a time attribute, waves, or slices.",
      "If you split the network yourself, e.g. with `to_waves()`,",
      "check that it returned a list of networks",
      "(and that the named attribute exists on the ties)."))
  }
  tlist <- lapply(tlist, manynet::as_tidygraph)
  frames <- if (is.null(names(tlist))) as.character(seq_along(tlist)) else names(tlist)
  # Ensure nodes are named so they can be matched across waves
  has_names <- "name" %in% names(manynet::node_attribute(tlist[[1]]))
  if (!has_names) {
    for (i in seq_along(tlist)) {
      tlist[[i]] <- manynet::add_node_attribute(tlist[[i]], "name",
                                                as.character(seq_len(igraph::vcount(tlist[[i]]))))
    }
  }
  all_names <- unique(unlist(lapply(tlist, function(x) igraph::V(x)$name)))
  # Presence and isolation per original wave
  present <- vapply(tlist, function(x) all_names %in% igraph::V(x)$name,
                    logical(length(all_names)))
  present <- matrix(present, nrow = length(all_names),
                    dimnames = list(all_names, frames))
  isolated <- matrix(FALSE, nrow = length(all_names), ncol = length(tlist),
                     dimnames = list(all_names, frames))
  for (i in seq_along(tlist)) {
    iso <- .node_is_isolate(tlist[[i]])
    isolated[igraph::V(tlist[[i]])$name, i] <- iso
  }
  # Harmonise every wave to the union vertex set, in identical order
  waves <- lapply(tlist, function(x) {
    ig <- manynet::as_igraph(x)
    miss <- setdiff(all_names, igraph::V(ig)$name)
    if (length(miss) > 0) ig <- igraph::add_vertices(ig, length(miss), name = miss)
    ig <- igraph::permute(ig, match(igraph::V(ig)$name, all_names))
    ig
  })
  waves <- .grapht_fill_attrs(waves, all_names)
  waves <- lapply(waves, manynet::as_tidygraph)
  list(waves = waves, frames = frames, present = present,
       isolated = isolated, has_names = has_names)
}

# A spell (interval) network records each tie's lifespan as `begin`/`end` tie
# attributes rather than as discrete time-stamped events (a `time` attribute).
# `manynet::is_dynamic()` is TRUE for both, but only the event form can be split
# by `to_slices()`, so spell networks are detected and sliced separately here.
.grapht_is_spell <- function(net) {
  atts <- names(manynet::tie_attribute(net))
  "begin" %in% atts && "end" %in% atts && !("time" %in% atts)
}

# Splits a spell network into one snapshot per change point (each moment at
# which some tie begins or ends), keeping the ties active during that spell
# (begin <= t < end). Unlike the cumulative slices of an event network, these
# show the network as it stood at each moment, so ties that dissolve disappear
# again. `manynet::to_time()` gained this behaviour in manynet 2.2.2, so it is
# used when available and reimplemented equivalently for older manynet. The
# version guard is paired with a check that to_time() actually accepts a missing
# `time` (its 2.2.2 signature), because a pre-release 2.2.2 dev build can carry
# the version string without yet exposing the feature; the fallback is
# behaviourally identical either way.
.grapht_spell_slices <- function(net) {
  if (utils::packageVersion("manynet") >= "2.2.2" &&
      !identical(formals(manynet::to_time)[["time"]], quote(expr = ))) {
    out <- manynet::to_time(net)
    # to_time() returns a single network when there is only one change point,
    # but grapht() always needs a list of snapshots to iterate over.
    if (!manynet::is_list(out)) out <- list(out)
    return(out)
  }
  begin <- end <- NULL # for R CMD check (used inside filter_ties' data mask)
  moments <- sort(unique(stats::na.omit(c(manynet::tie_attribute(net, "begin"),
                                          manynet::tie_attribute(net, "end")))))
  out <- lapply(moments, function(t)
    manynet::filter_ties(net, begin <= t & (is.na(end) | end > t)))
  names(out) <- as.character(moments)
  out
}

# Nodes added to a wave during harmonisation carry NA attributes; fill them
# from the nearest wave in which the node was present so that attribute-mapped
# aesthetics do not flicker. Genuinely time-varying attributes (e.g. diffusion
# status) are only filled where missing, so their per-wave values persist.
.grapht_fill_attrs <- function(waves, all_names) {
  if (length(waves) < 2) return(waves)
  attrs <- unique(unlist(lapply(waves, igraph::vertex_attr_names)))
  attrs <- setdiff(attrs, "name")
  for (a in attrs) {
    vals <- lapply(waves, function(w) {
      v <- igraph::vertex_attr(w, a)
      if (is.null(v)) rep(NA, length(all_names)) else v
    })
    vals <- do.call(cbind, vals)
    filled <- t(apply(vals, 1, .fill_series))
    for (i in seq_along(waves))
      waves[[i]] <- igraph::set_vertex_attr(waves[[i]], a, value = filled[, i])
  }
  waves
}

# Forward-fills NAs in a vector from the last non-NA value,
# then backward-fills any leading NAs.
.fill_series <- function(x) {
  if (!anyNA(x) || all(is.na(x))) return(x)
  last <- NA
  for (j in seq_along(x)) {
    if (!is.na(x[j])) last <- x[j] else x[j] <- last
  }
  nxt <- NA
  for (j in rev(seq_along(x))) {
    if (!is.na(x[j])) nxt <- x[j] else x[j] <- nxt
  }
  x
}

# Aggregate (union of ties across waves) network on the harmonised vertex set,
# used for static fallback layouts.
.grapht_union <- function(waves) {
  nodes <- igraph::as_data_frame(manynet::as_igraph(waves[[1]]), "vertices")
  if (!"name" %in% names(nodes)) nodes <- cbind(name = rownames(nodes), nodes)
  nodes <- nodes[, c("name", setdiff(names(nodes), "name")), drop = FALSE]
  edges <- do.call(rbind, lapply(waves, function(w)
    igraph::as_data_frame(manynet::as_igraph(w), "edges")))
  if (nrow(edges) > 0)
    edges <- edges[!duplicated(edges[, c("from", "to")]), , drop = FALSE]
  ig <- igraph::graph_from_data_frame(edges,
                                      directed = manynet::is_directed(waves[[1]]),
                                      vertices = nodes)
  manynet::as_tidygraph(ig)
}

# Per-frame node coordinates. The default stress layout transitions smoothly
# via graphlayouts::layout_as_dynamic(); any other layout is computed once on
# the aggregate network and held fixed across frames.
.grapht_layout <- function(waves, layout, alpha, ...) {
  all_names <- igraph::V(manynet::as_igraph(waves[[1]]))$name
  if (identical(layout, "stress") &&
      requireNamespace("graphlayouts", quietly = TRUE)) {
    gl <- lapply(waves, function(w) {
      ig <- manynet::as_igraph(w)
      for (a in setdiff(igraph::vertex_attr_names(ig), "name"))
        ig <- igraph::delete_vertex_attr(ig, a)
      for (a in igraph::edge_attr_names(ig))
        ig <- igraph::delete_edge_attr(ig, a)
      for (a in igraph::graph_attr_names(ig))
        ig <- igraph::delete_graph_attr(ig, a)
      ig
    })
    xy <- graphlayouts::layout_as_dynamic(gl, alpha = alpha)
    lapply(xy, function(m)
      data.frame(name = all_names, x = m[, 1], y = m[, 2]))
  } else {
    if (identical(layout, "stress"))
      thisRequires("graphlayouts")
    manynet::snet_info("Using a static ", layout,
                       " layout computed on the aggregate network.")
    lo <- suppressWarnings(ggraph::create_layout(.grapht_union(waves), layout, ...))
    lapply(seq_along(waves), function(i)
      data.frame(name = all_names, x = lo$x, y = lo$y))
  }
}

# Resolves node colour across all waves at once, so that a variable that is
# constant within one wave but varies over time is still mapped, with factor
# levels consistent across frames.
.grapht_ncolor <- function(waves, node_color) {
  diffusion <- is.null(node_color) &&
    "diffusion" %in% names(manynet::node_attribute(waves[[1]]))
  if (diffusion) {
    vals <- vapply(waves, function(w)
      dplyr::recode_values(as.character(manynet::node_attribute(w, "diffusion")),
                           "E" ~ "Exposed", "I" ~ "Infected",
                           "R" ~ "Recovered", "S" ~ "Susceptible"),
      character(igraph::vcount(waves[[1]])))
    return(list(mapped = TRUE, diffusion = TRUE, values = vals))
  }
  if (!is.null(node_color) &&
      node_color %in% names(manynet::node_attribute(waves[[1]]))) {
    vals <- vapply(waves, function(w)
      as.character(manynet::node_attribute(w, node_color)),
      character(igraph::vcount(waves[[1]])))
    if (length(unique(stats::na.omit(as.vector(vals)))) == 1) {
      manynet::snet_info("Please indicate a variable with more than one value or level when mapping node colors.")
      return(list(mapped = FALSE, diffusion = FALSE, literal = "black"))
    }
    return(list(mapped = TRUE, diffusion = FALSE, values = vals))
  }
  list(mapped = FALSE, diffusion = FALSE,
       literal = if (!is.null(node_color)) node_color else "black")
}

# One row per union node per frame, with stable coordinates, presence and
# isolation flags, and aesthetic columns shared with graphr()'s inference.
.grapht_node_frames <- function(waves, layouts, frames, present, isolated,
                                node_color, node_shape, node_size, isolates) {
  all_names <- layouts[[1]]$name
  n <- length(all_names)
  ncol_res <- .grapht_ncolor(waves, node_color)
  out <- do.call(rbind, lapply(seq_along(waves), function(i) {
    w <- waves[[i]]
    nshape <- .infer_nshape(w, node_shape)
    data.frame(name = all_names,
               frame = frames[i],
               x = layouts[[i]]$x, y = layouts[[i]]$y,
               present = present[, i],
               isolated = isolated[, i],
               nsize = rep_len(.infer_nsize(w, node_size), n),
               nshape = rep_len(as.character(nshape), n),
               ncolor = rep_len(
                 if (ncol_res$mapped) ncol_res$values[, i] else ncol_res$literal, n),
               stringsAsFactors = FALSE)
  }))
  out$frame <- factor(out$frame, levels = frames)
  out$nalpha <- ifelse(out$present &
                         !(isolates == "fade" & out$isolated), 1, 0)
  attr(out, "diffusion") <- ncol_res$diffusion
  out
}

# Resolves edge colour across all waves at once (see .grapht_ncolor).
# `raw` carries one colour vector per wave, aligned with that wave's ties.
.grapht_ecolor <- function(waves, edge_color) {
  g1 <- waves[[1]]
  attr_mapped <- !is.null(edge_color) &&
    edge_color %in% names(manynet::tie_attribute(g1))
  signed <- is.null(edge_color) && manynet::is_signed(g1)
  if (attr_mapped) {
    raw <- lapply(waves, function(w)
      as.character(manynet::tie_attribute(w, edge_color)))
  } else if (signed) {
    raw <- lapply(waves, function(w)
      ifelse(as.numeric(manynet::tie_signs(w)) >= 0, "Positive", "Negative"))
  } else {
    return(list(mapped = FALSE,
                literal = if (!is.null(edge_color)) edge_color else "black"))
  }
  if (length(unique(stats::na.omit(unlist(raw)))) <= 1) {
    if (attr_mapped)
      manynet::snet_info("Please indicate a variable with more than one value or level when mapping edge colors.")
    return(list(mapped = FALSE, literal = "black"))
  }
  list(mapped = TRUE, signed = signed, raw = raw)
}

# One row per union edge per frame. Absent edges keep rows (with alpha 0 and
# aesthetics carried over from waves where present) so that gganimate can
# tween them in and out while their endpoints move.
.grapht_edge_frames <- function(waves, layouts, frames, edge_color, edge_size) {
  directed <- manynet::is_directed(waves[[1]])
  all_names <- layouts[[1]]$name
  ecol_res <- .grapht_ecolor(waves, edge_color)
  # Per-wave edge tables with aesthetics resolved via shared helpers
  ew <- lapply(seq_along(waves), function(i) {
    w <- waves[[i]]
    ed <- igraph::as_data_frame(manynet::as_igraph(w), "edges")
    nties <- nrow(ed)
    df <- data.frame(from = ed[["from"]], to = ed[["to"]],
                     esize = rep_len(.infer_esize(w, edge_size), nties),
                     linetype = rep_len(.infer_line_type(w), nties),
                     ecolor = rep_len(
                       if (ecol_res$mapped) ecol_res$raw[[i]] else ecol_res$literal,
                       nties),
                     stringsAsFactors = FALSE)
    df <- df[df$from != df$to, , drop = FALSE]  # loops are not drawn
    if (!directed) {
      # Canonicalise undirected endpoints by their position in the union node
      # order rather than by string comparison, which returns NA for names
      # with non-ASCII characters under some locales.
      swap <- match(df$from, all_names) > match(df$to, all_names)
      swap[is.na(swap)] <- FALSE
      tmp <- df$from[swap]
      df$from[swap] <- df$to[swap]
      df$to[swap] <- tmp
    }
    df$id <- paste(df$from, df$to, sep = "->")
    df[!duplicated(df$id), , drop = FALSE]
  })
  all_ids <- unique(unlist(lapply(ew, function(d) d$id)))
  ends <- do.call(rbind, ew)[, c("id", "from", "to")]
  ends <- ends[!duplicated(ends$id), , drop = FALSE]
  # Fade present edges more in dense frames so heavily overlapping ties read as
  # a density gradient rather than a solid black mass. Sparse networks keep the
  # standard 0.4 (matching graphr()); the alpha only drops past ~100 ties.
  max_ties <- max(vapply(ew, nrow, integer(1)), 0L)
  present_alpha <- max(0.08, min(0.4, 40 / max(max_ties, 1)))
  out <- do.call(rbind, lapply(seq_along(waves), function(i) {
    d <- data.frame(id = all_ids, frame = frames[i], stringsAsFactors = FALSE)
    d <- dplyr::left_join(d, ends, by = "id")
    d$status <- d$id %in% ew[[i]]$id
    d <- dplyr::left_join(d, ew[[i]][, c("id", "esize", "linetype", "ecolor")],
                          by = "id")
    lay <- layouts[[i]]
    d$x <- lay$x[match(d$from, lay$name)]
    d$y <- lay$y[match(d$from, lay$name)]
    d$xend <- lay$x[match(d$to, lay$name)]
    d$yend <- lay$y[match(d$to, lay$name)]
    d$ealpha <- ifelse(d$status, present_alpha, 0)
    d
  }))
  if (nrow(out) > 0) {
    # Carry aesthetics of absent edges over from waves where they exist
    out <- out[order(match(out$id, all_ids), match(out$frame, frames)), ]
    for (col in c("esize", "linetype", "ecolor")) {
      out[[col]] <- unlist(lapply(
        split(out[[col]], factor(out$id, levels = all_ids)), .fill_series),
        use.names = FALSE)
    }
  }
  out$frame <- factor(out$frame, levels = frames)
  attr(out, "ecolor_signed") <- isTRUE(ecol_res$signed)
  out
}

# Shortens directed segments at the target end so arrowheads sit at the node
# boundary rather than its centre. An approximation of ggraph's `end_cap`:
# the trim scales with the target's point size as a fraction of the layout
# span (visually calibrated at the default render size), capped at 40% of
# the segment length.
.shorten_segments <- function(edges_out, nodes_out) {
  span <- max(diff(range(nodes_out$x)), diff(range(nodes_out$y)))
  if (!is.finite(span) || span == 0) return(edges_out)
  nsize_t <- nodes_out$nsize[match(paste(edges_out$to, edges_out$frame),
                                   paste(nodes_out$name, nodes_out$frame))]
  dx <- edges_out$xend - edges_out$x
  dy <- edges_out$yend - edges_out$y
  len <- sqrt(dx^2 + dy^2)
  cap <- pmin(0.25 * nsize_t / 100 * span, 0.4 * len)
  keep <- is.finite(len) & len > 0
  edges_out$xend[keep] <- edges_out$xend[keep] -
    dx[keep] / len[keep] * cap[keep]
  edges_out$yend[keep] <- edges_out$yend[keep] -
    dy[keep] / len[keep] * cap[keep]
  edges_out
}

# Builds the (frame-stacked) ggplot that gganimate animates. Aesthetics whose
# resolved column varies are mapped through ordinary scales (so legends appear
# and transition); constant columns become literal layer parameters, mirroring
# graphr()'s behaviour.
.grapht_build <- function(nodes_out, edges_out, g_ref, labels,
                          node_color, node_shape, node_size,
                          edge_color, edge_size,
                          label_dist, label_repel) {
  directed <- manynet::is_directed(g_ref)
  n_union <- length(unique(nodes_out$name))
  diffusion <- isTRUE(attr(nodes_out, "diffusion"))
  p <- ggplot2::ggplot()

  # --- Edges ----
  if (nrow(edges_out) > 0) {
    ecolor_signed <- isTRUE(attr(edges_out, "ecolor_signed"))
    if (directed) edges_out <- .shorten_segments(edges_out, nodes_out)
    if (ecolor_signed)
      edges_out$ecolor <- factor(edges_out$ecolor,
                                 levels = c("Positive", "Negative"))
    ecolor_mapped <- length(unique(edges_out$ecolor)) > 1
    esize_mapped <- length(unique(edges_out$esize)) > 1
    ltype_mapped <- length(unique(edges_out$linetype)) > 1
    emap <- ggplot2::aes(x = .data$x, y = .data$y,
                         xend = .data$xend, yend = .data$yend,
                         group = .data$id, alpha = .data$ealpha)
    eparams <- list()
    if (ecolor_mapped) {
      emap <- utils::modifyList(emap, ggplot2::aes(colour = .data$ecolor))
    } else eparams$colour <- unique(edges_out$ecolor)
    if (esize_mapped) {
      emap <- utils::modifyList(emap, ggplot2::aes(linewidth = .data$esize))
    } else eparams$linewidth <- unique(edges_out$esize)
    if (ltype_mapped) {
      emap <- utils::modifyList(emap, ggplot2::aes(linetype = .data$linetype))
    } else eparams$linetype <- unique(edges_out$linetype)
    if (directed) eparams$arrow <- .infer_arrow(edges_out$esize[edges_out$status])
    p <- p + do.call(ggplot2::geom_segment,
                     c(list(mapping = emap, data = edges_out), eparams))
    if (ecolor_mapped) {
      ecolor_name <- ifelse(is.null(edge_color) & manynet::is_signed(g_ref),
                            "Sign", edge_color)
      nlevels <- length(unique(edges_out$ecolor))
      if (nlevels == 2) {
        p <- p + ggplot2::scale_colour_manual(
          name = ecolor_name,
          values = getOption("snet_highlight", default = c("grey", "black")))
      } else {
        p <- p + ggplot2::scale_colour_manual(
          name = ecolor_name, values = ag_qualitative(nlevels))
      }
    }
    if (esize_mapped)
      p <- p + ggplot2::scale_linewidth_continuous(
        name = ifelse(is.null(edge_size) & manynet::is_weighted(g_ref),
                      "Weight", "Width"),
        range = c(0.3, 3))
    if (ltype_mapped)
      p <- p + ggplot2::scale_linetype_identity(guide = "none")
  }

  # --- Nodes ----
  ncolor_mapped <- diffusion || length(unique(nodes_out$ncolor)) > 1
  nshape_mapped <- length(unique(nodes_out$nshape)) > 1
  nsize_mapped <- length(unique(nodes_out$nsize)) > 1
  nmap <- ggplot2::aes(x = .data$x, y = .data$y,
                       group = .data$name, alpha = .data$nalpha)
  nparams <- list()
  if (ncolor_mapped) {
    nmap <- utils::modifyList(nmap, ggplot2::aes(fill = .data$ncolor))
  } else {
    lit <- unique(nodes_out$ncolor)
    nparams$fill <- lit
  }
  if (nshape_mapped) {
    nmap <- utils::modifyList(nmap, ggplot2::aes(shape = .data$nshape))
  } else {
    lit <- unique(nodes_out$nshape)
    nparams$shape <- if (!is.na(suppressWarnings(as.numeric(lit))))
      as.numeric(lit) else lit
  }
  if (nsize_mapped) {
    nmap <- utils::modifyList(nmap, ggplot2::aes(size = .data$nsize))
  } else nparams$size <- unique(nodes_out$nsize)
  p <- p + do.call(ggplot2::geom_point,
                   c(list(mapping = nmap, data = nodes_out), nparams))
  if (diffusion) {
    cols <- match_color(c("#d73027", "#4575b4", "#E6AB02", "#66A61E"))
    p <- p + ggplot2::scale_fill_manual(
      name = NULL,
      values = c("Infected" = cols[1], "Susceptible" = cols[2],
                 "Exposed" = cols[3], "Recovered" = cols[4]))
  } else if (ncolor_mapped) {
    nlevels <- length(unique(nodes_out$ncolor))
    if (nlevels == 2) {
      p <- p + ggplot2::scale_fill_manual(
        values = getOption("snet_highlight", default = c("grey", "black")))
    } else {
      p <- p + ggplot2::scale_fill_manual(values = ag_qualitative(nlevels))
    }
  }
  if (nshape_mapped)
    p <- p + ggplot2::scale_shape_manual(values = c(21, 22, 24, 23, 25,
                                                    3, 4, 8,
                                                    10, 12, 9,
                                                    13, 7, 11, 14))
  if (nsize_mapped)
    p <- p + ggplot2::scale_size(range = c(1 / n_union * 50,
                                           1 / n_union * 100))

  # --- Labels (drawn above nodes, as in graphr) ----
  if (isTRUE(labels)) {
    # No ggrepel-based repelling here (see @details in grapht()'s docs);
    # `label_repel` toggles a fixed offset instead, scaled by `label_dist`
    # when supplied (calibrated so graphr()'s default `label_dist` of 10
    # corresponds to the default offset). The offset is applied in data
    # units, sized to clear the node radius so labels sit above nodes.
    label_offset <- if (isTRUE(label_repel)) {
      if (!is.null(label_dist)) label_dist / 50 else 0.2
    } else 0
    span <- max(diff(range(nodes_out$x)), diff(range(nodes_out$y)))
    nudge <- if (label_offset > 0 && is.finite(span))
      (0.3 * max(nodes_out$nsize) / 100 + 0.1 * label_offset) * span else 0
    p <- p + ggplot2::geom_text(
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$name,
                   group = .data$name, alpha = .data$nalpha),
      data = nodes_out, colour = ag_base(), family = ag_font(),
      nudge_y = nudge, show.legend = FALSE)
  }

  p <- p + ggplot2::scale_alpha_identity(guide = "none")

  # --- Legends and theme (consistent with graphr) ----
  p <- graph_legends(p, g_ref, node_color, node_shape, node_size,
                     edge_color, edge_size)
  p <- p + ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom")
  if (directed) p <- p + ggplot2::coord_fixed()
  if (getOption("snet_background", default = "#FFFFFF") != "#FFFFFF")
    p <- p + ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = getOption("snet_background", default = "#FFFFFF")))
  p
}
