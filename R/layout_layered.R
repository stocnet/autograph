#' Layered layouts
#'
#' @description
#'   These algorithms assign each node to a layer, which becomes one axis,
#'   and a position within that layer, which becomes the other.
#'   They are recommended for use with `graphr()` or `{ggraph}`,
#'   and suit two-mode networks and directed acyclic networks.
#'
#'   The four layouts are one engine drawn four ways,
#'   and differ only in which axis carries the layers
#'   and in how each layer is spread out:
#'
#'   |                        | Layers stacked flat | Layers standing up |
#'   |------------------------|---------------------|--------------------|
#'   | `alignment = "straight"` | "layered"         | "lineage"          |
#'   | `alignment = "rungs"`    | "railway"         | "ladder"           |
#'
#'   That is, the "layered" layout places the first node set along the bottom
#'   and the second node set along the top,
#'   sequenced and spaced as necessary to minimise tie overlap.
#'   The "lineage" layout is the same layout with the axes exchanged,
#'   so that successive layers run left to right rather than bottom to top.
#'   The "railway" and "ladder" layouts are "layered" and "lineage"
#'   with every layer given the same spacing,
#'   so that the nodes line up across the layers
#'   like the rails and rungs the names describe.
#' @name layout_layered
#' @template param_ggraphlayouts
#' @param ranks How the layers are assigned:
#'   "tight" (the default) chooses the layers that make the total tie length
#'   as short as possible, while still pointing every tie down at least one
#'   layer;
#'   "generation" ranks each node by its distance from a root, so that a layer
#'   is a generation, at the cost of some longer ties;
#'   "compact" asks `igraph::layout_with_sugiyama()` for the layers.
#'   The first two need an acyclic network, and fall back to "compact" where
#'   the network is not.
#'   Ignored for a two-mode network, whose layers are its modes.
#'
#'   A node attribute can be given here instead, either as the name of a
#'   numeric node attribute or as a numeric vector as long as the network has
#'   nodes. Then the layers are those values, and nodes are placed along that
#'   axis in proportion to them rather than at even steps, so that a network
#'   of dated nodes is drawn as a timeline.
#'   The values run in the same direction as the layers the engine works out:
#'   down the page in a "layered" or "railway" layout, and left to right in a
#'   "lineage" or "ladder" layout, so that the smallest value comes first.
#'   A "lineage" given values lays out all of its nodes together, rather than
#'   one component at a time, so that an isolate or a small component sits in
#'   the column of its own value. `graphr()` draws it as a timeline.
#'   A node that a tie would run straight through, other than the two nodes
#'   the tie joins, is moved off that tie, so that several works citing one
#'   earlier work can be told from a chain of works each citing the one
#'   before. Two tied nodes with the same value are put next to each other,
#'   and nodes are moved within their layer where that leaves fewer ties
#'   crossing. That last step is left out of larger networks, where the
#'   number of nodes times the number of ties is more than 5000,
#'   to save time.
#' @param alignment How each layer is spread out:
#'   "straight" (the default) draws the ties as close to straight as the
#'   ordering allows, which groups the nodes that belong together;
#'   "rungs" gives every layer the same integer spacing, so that the nodes
#'   line up across the layers.
#' @param center Further split a "layered" layout by
#'   declaring the "center" argument as the "events", "actors",
#'   or by declaring a node name.
#'   Defaults to NULL.
#' @param rank Deprecated. Use `ranks` instead, which now takes a node
#'   attribute as well as a method.
#' @family mapping
NULL

#' @rdname layout_layered
#' @examples
#' #graphr(ison_southern_women, layout = "layered", center = "events",
#' #           node_color = "type", node_size = 3)
#' @export
layout_layered <- function(.data, center = NULL,
                           ranks = c("tight", "generation", "compact"),
                           alignment = c("straight", "rungs"),
                           circular = FALSE, times = 1000) {
  if (is.null(center)) {
    out <- .to_lo(.layer_axes(.data, ranks = ranks, alignment = alignment,
                              times = times))
  } else {
    if (!manynet::is_twomode(.data)) manynet::snet_abort(
      "The {.val layered} layout can only centre on a mode of a two-mode",
      "network, but a one-mode network was given.",
      "Either drop the {.arg center} argument, or use a two-mode network.")
    net <- manynet::as_matrix(.data)
    nn <- dim(net)[1]
    mm <- dim(net)[2]
    if (center == "actors") {
      Act <- cbind(rep(1, nrow(net)), .nrm(.rng(nn)))
      Evt1 <- cbind(rep(0, ceiling(ncol(net)/2)), .nrm(.rng(ceiling(mm/2))))
      Evt2 <- cbind(rep(2, floor(ncol(net)/2)), .nrm(.rng(floor(mm/2))))
      crd <- rbind(Act, Evt1, Evt2)
      crd[which(is.nan(crd))] <- 0.5
      rownames(crd) <- c(dimnames(net)[[1]], dimnames(net)[[2]])
    } else if (center == "events") {
      Act1 <- cbind(rep(0, ceiling(nrow(net)/2)), .nrm(.rng(ceiling(nn/2))))
      Act2 <- cbind(rep(2, floor(nrow(net)/2)), .nrm(.rng(floor(nn/2))))
      Evt <- cbind(rep(1, ncol(net)), .nrm(.rng(mm)))
      crd <- rbind(Act1, Act2, Evt)
      crd[which(is.nan(crd))] <- 0.5
      rownames(crd) <- c(dimnames(net)[[1]], dimnames(net)[[2]])
    } else {
      if (center %in% manynet::node_names(.data)) {
        side1 <- suppressWarnings(cbind(rep(0, nrow(net)), .nrm(.rng(nn))))
        side2 <- suppressWarnings(cbind(rep(2, ncol(net)), .nrm(.rng(mm))))
        if (any(rownames(net) == center)) {
          side1[,1] <- ifelse(rownames(net) == center, 1, side1[,1])
          side1[,2] <- ifelse(rownames(net) == center, 0.5, side1[,2])
        } else {
          # The centred node is in the second mode, which `net` holds in its
          # columns: comparing the row names here would test the wrong mode
          # and recycle a vector of the wrong length into `side2`.
          side2[,1] <- ifelse(colnames(net) == center, 1, side2[,1])
          side2[,2] <- ifelse(colnames(net) == center, 0.5, side2[,2])
        }
        crd <- rbind(side1, side2)
        crd[which(is.nan(crd))] <- 0.5
        rownames(crd) <- c(dimnames(net)[[1]], dimnames(net)[[2]])
      } else .abort_no_match(center, manynet::node_names(.data), "center",
                             what = "node name",
                             extra_desc = paste("{.val actors} or {.val events}",
                                                "can also be given here,",
                                                "to centre on a whole mode."))
    }
    out <- .to_lo(crd)
  }
  out
}

#' @rdname layout_layered
#' @export
layout_tbl_graph_layered <- layout_layered

#' @rdname layout_layered
#' @examples
#' #graphr(ison_southern_women, layout = "lineage")
#' # ison_adolescents |>
#' #   mutate_nodes(year = rep(c(1985, 1990, 1995, 2000), times = 2)) |>
#' #   graphr(layout = "lineage", ranks = "year")
#' @export
layout_lineage <- function(.data,
                           ranks = c("tight", "generation", "compact"),
                           alignment = c("straight", "rungs"),
                           circular = FALSE, times = 1000, rank = NULL){
  ranks <- .absorb_rank(ranks, rank)
  # The same coordinates as "layered", with the axes exchanged, so that the
  # layers run left to right rather than bottom to top.
  # Ranks given as values make a timeline, where a work of 2011 is looked for
  # in the column of 2011 whatever it cites. Packing the components side by
  # side would stack the small ones and the isolates far off along each year,
  # so a timeline lays its nodes out in one pass instead.
  # A timeline also moves a node off any tie that would otherwise run
  # straight through it. See `.clear_ties()`.
  timeline <- .ranks_given(ranks)
  lo <- .layer_axes(.data, ranks = ranks, alignment = alignment, times = times,
                    pack = !timeline, clear = timeline)
  # The layer axis is negated so that the layers run left to right, as they run
  # top to bottom in "layered".
  out <- .to_lo(cbind(-lo[, 2], lo[, 1]))
  # Nodes the caller gave the same value land on the same coordinate, so they
  # need nudging apart.
  if (.ranks_given(ranks)) .check_dup(out) else out
}

#' @rdname layout_layered
#' @export
layout_tbl_graph_lineage <- layout_lineage

#' @rdname layout_layered
#' @export
layout_railway <- function(.data,
                           ranks = c("tight", "generation", "compact"),
                           circular = FALSE, times = 1000) {
  # "railway" is "layered" with every layer given the same integer spacing,
  # so that the nodes line up across the layers like the rungs of a ladder.
  layout_layered(.data, ranks = ranks, alignment = "rungs", times = times)
}

#' @rdname layout_layered
#' @export
layout_tbl_graph_railway <- layout_railway

#' @rdname layout_layered
#' @export
layout_ladder <- function(.data,
                          ranks = c("tight", "generation", "compact"),
                          circular = FALSE, times = 1000){
  layout_lineage(.data, ranks = ranks, alignment = "rungs", times = times)
}

#' @rdname layout_layered
#' @export
layout_tbl_graph_ladder <- layout_ladder

# Drawing a lineage ---------------------------------------------------------

# A "lineage" gives every node its own layer, so an isolate has a place in the
# layout as much as any other node. See `graphr()`.
.keeps_isolates <- function(layout) identical(layout, "lineage")

# A "lineage" whose ranks are values, such as years, places its layers in
# proportion to those values, so the x axis can be read as a time axis.
.is_timeline <- function(layout, dots) {
  identical(layout, "lineage") &&
    .ranks_given(dots[["ranks"]] %||% dots[["rank"]])
}

# Breaks the x axis at the values the layers were ranked by, with a light
# gridline at each, and keeps the rest of the theme empty. The layout rescales
# those values, so the breaks are put where the layout placed them. More than
# 40 distinct values would crowd the axis, so those get pretty breaks instead.
.add_time_axis <- function(p, g, ranks) {
  if (is.character(ranks) && length(ranks) == 1L) {
    nm <- .match_name(ranks, igraph::vertex_attr_names(manynet::as_igraph(g)),
                      "ranks", what = "node attribute")
    raw <- manynet::node_attribute(g, nm)
  } else raw <- ranks
  vals <- as.numeric(raw)
  xs <- p[["data"]][["x"]]
  if (length(vals) != length(xs) || length(unique(vals)) < 2L) return(p)
  at <- sort(unique(vals))
  if (length(at) > 40L) {
    at <- pretty(vals, n = 10)
    at <- at[at >= min(vals) & at <= max(vals)]
  }
  # The layout placed the values on a line, so two points fix where any value
  # falls along it.
  lo <- which.min(vals)
  hi <- which.max(vals)
  breaks <- xs[lo] + (at - vals[lo]) / (vals[hi] - vals[lo]) * (xs[hi] - xs[lo])
  labels <- if (inherits(raw, "Date")) format(as.Date(at)) else
    if (inherits(raw, "POSIXct")) format(as.POSIXct(at)) else
      format(at, trim = TRUE)
  sc <- p[["scales"]]$get_scales("x")
  if (is.null(sc)) {
    p <- p + ggplot2::scale_x_continuous(breaks = breaks, labels = labels)
  } else {
    sc[["breaks"]] <- breaks
    sc[["labels"]] <- labels
  }
  p + ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1,
                                        colour = ag_ink(), size = ggplot2::rel(0.8)),
    panel.grid.major.x = ggplot2::element_line(
      colour = ggplot2::alpha(ag_ink(), 0.12), linewidth = 0.3))
}

# Axes --------------------------------------------------------------------

# The three ways the engine can work the layers out for itself. Anything else
# given to `ranks` is a node attribute holding the layers already.
.rank_methods <- function() c("tight", "generation", "compact")

# Has the caller given values rather than named a method? The default is the
# full vector of methods, which `match.arg()` would take the first of, so a
# character vector every element of which is a method is a method.
.ranks_given <- function(ranks) {
  if (is.null(ranks)) return(FALSE)
  !(is.character(ranks) && all(ranks %in% .rank_methods()))
}

# `rank` named the attribute before `ranks` could take one. Accept it for a
# release, so that a call written against the older argument still draws.
.absorb_rank <- function(ranks, rank) {
  if (is.null(rank)) return(ranks)
  manynet::snet_warn(
    "The {.arg rank} argument is deprecated.",
    "Please use {.code ranks} instead, which takes a node attribute",
    "as well as one of {.val tight}, {.val generation} or {.val compact}.")
  rank
}

# Turn `ranks` into a method the engine understands and, where the caller gave
# values instead, the numeric layer of each node in the order `.data` holds
# them.
.resolve_ranks <- function(.data, ranks) {
  if (!.ranks_given(ranks))
    return(list(method = if (is.null(ranks)) "tight" else ranks[1],
                values = NULL))
  n <- as.integer(manynet::net_nodes(.data))
  if (is.character(ranks) && length(ranks) == 1L) {
    nm <- .match_name(ranks, igraph::vertex_attr_names(manynet::as_igraph(.data)),
                      "ranks", what = "node attribute")
    values <- as.numeric(manynet::node_attribute(.data, nm))
  } else if (is.numeric(ranks) && length(ranks) == n) {
    values <- as.numeric(ranks)
  } else .abort_layout_arg("ranks", "lineage", n)
  if (anyNA(values)) manynet::snet_abort(
    "The {.arg ranks} attribute must be numeric and complete,",
    "but it holds missing values.")
  # The engine still orders the nodes within each layer, so it is given the
  # layers these values imply; the values themselves become the axis below.
  list(method = "compact", values = values)
}

# Map the engine's layer and position onto a pair of axes. A two-mode network
# has no direction to its layers, and its first mode has always been drawn
# along the bottom, so its layers ascend with y. A one-mode network's layers do
# have a direction -- a tie points from the earlier layer to the later -- so
# they descend with y, putting parents above their children.
.layer_axes <- function(.data, ranks, alignment, times, pack = TRUE,
                        clear = FALSE) {
  g <- manynet::as_igraph(.data)
  twomode <- manynet::is_twomode(.data)
  spec <- .resolve_ranks(.data, ranks)
  layers <- if (!is.null(spec$values)) .compact_ranks(spec$values) + 1L else
    if (twomode) ifelse(igraph::V(g)$type, 2, 1) else NULL
  lo <- .layer_coords(g, layers = layers, ranks = spec$method,
                      alignment = alignment, times = times,
                      pack = pack && !twomode)
  x <- lo$pos
  y <- if (twomode) lo$rank else -lo$rank
  # `as_igraph()` can reorder the nodes of a two-mode network, so the
  # coordinates are put back into the order the caller's network holds them in.
  # The names are read through 'manynet', since igraph reads only an igraph,
  # and a two-mode network can arrive as a 'stocnet'.
  if (twomode && manynet::is_labelled(.data)) {
    ord <- order(match(igraph::vertex_attr(g, "name"),
                       manynet::node_names(.data)))
    x <- x[ord]
    y <- y[ord]
  }
  # Values the caller gave are already in that order, and are placed in
  # proportion to themselves rather than at even steps. They descend the page
  # like the layers the engine works out, so that the smallest value -- the
  # earliest date, the first generation -- is at the top.
  if (!is.null(spec$values)) {
    if (clear && !twomode) {
      x <- .clear_ties(g, spec$values, x)
      x <- .uncross_ties(g, spec$values, x)
    }
    y <- -spec$values
  }
  if (length(unique(x)) > 1) x <- .rescale(x)
  if (length(unique(y)) > 1) y <- .rescale(y)
  cbind(x, y)
}

# Where the values place a layer, a tie can run from one layer to one several
# layers away, and `.straighten()` pulls the nodes that cite one work onto the
# row of that work. A tie from the last of them then runs straight through all
# the others, and a reader cannot tell whether each cites the work or each
# cites the one before it. So a node that a tie runs through, other than the
# two it joins, is moved off that tie, to whichever side leaves the fewest
# such nodes, and its layer is spaced out again. A chain, in which each node
# cites only the one before it, has no tie that runs through a node, and so
# stays in its row.
#
# A tie between two works of one year runs along their column, through every
# work placed between them, and moving those works up or down the column
# cannot take them off it. So the column is put in another order instead:
# one end of the tie is moved next to the other.
#
# `at` is where each node is along the layers (its value) and `pos` where it is
# within its layer, one apart at the closest. Each pass looks at every tie
# against every node, so a network of more than 500 nodes is left as it is,
# and the passes stop at 200.
.clear_ties <- function(g, at, pos, sep = 1, passes = 200) {
  el <- igraph::as_edgelist(g, names = FALSE)
  n <- length(pos)
  if (!nrow(el) || n > 500L || length(unique(at)) < 3L) return(pos)
  el <- el[el[, 1] != el[, 2], , drop = FALSE]
  passed <- function(pos) .ties_through(el, at, pos, sep)
  respace <- function(pos, layer) .respace_layer(at, pos, layer, sep)
  # Of the positions tried, the one that leaves the fewest nodes on ties. It
  # is kept only where it leaves fewer than before, so the passes cannot go
  # round in a circle, and the passes stop where nothing can be moved to that
  # end.
  best <- function(tries, hit) {
    left <- vapply(tries, function(p) length(passed(p)), integer(1))
    if (min(left) < length(hit)) tries[[which.min(left)]]
  }
  # Half a step either way clears a tie across the layers; a whole step is
  # tried as well, for where half a step lands on the next tie.
  shift <- function(pos, w, hit) {
    best(lapply(c(0.5, -0.5, 1, -1) * sep, function(d) {
      new <- pos
      new[w] <- new[w] + d
      respace(new, at[w])
    }), hit)
  }
  # Either end of a tie within a layer, put on either side of the other end.
  regroup <- function(pos, hit) {
    within <- el[at[el[, 1]] == at[el[, 2]], , drop = FALSE]
    for (k in seq_len(nrow(within))) {
      u <- within[k, 1]; v <- within[k, 2]
      if (abs(pos[u] - pos[v]) <= sep * 1.01) next
      tries <- list()
      for (e in list(c(u, v), c(v, u))) for (d in c(0.5, -0.5) * sep) {
        new <- pos
        new[e[1]] <- new[e[2]] + d
        tries[[length(tries) + 1L]] <- respace(new, at[u])
      }
      if (!is.null(out <- best(tries, hit))) return(out)
    }
    NULL
  }
  for (i in seq_len(passes)) {
    hit <- passed(pos)
    if (!length(hit)) break
    new <- NULL
    for (w in hit) if (!is.null(new <- shift(pos, w, hit))) break
    if (is.null(new)) new <- regroup(pos, hit)
    if (is.null(new)) break
    pos <- new
  }
  pos
}

# The nodes that some tie runs through, other than the two it joins. A tie
# across the layers is taken as straight, and a node within a quarter step of
# it is on it. A tie within a layer runs through every node of that layer
# between its two ends.
.ties_through <- function(el, at, pos, sep = 1) {
  n <- length(pos)
  tol <- sep / 4
  hit <- logical(n)
  for (k in seq_len(nrow(el))) {
    u <- el[k, 1]; v <- el[k, 2]
    if (at[u] == at[v]) {
      hit[at == at[u] & pos > min(pos[u], pos[v]) &
            pos < max(pos[u], pos[v])] <- TRUE
      next
    }
    w <- which(at > min(at[u], at[v]) & at < max(at[u], at[v]))
    if (!length(w)) next
    line <- pos[u] + (pos[v] - pos[u]) * (at[w] - at[u]) / (at[v] - at[u])
    hit[w[abs(pos[w] - line) < tol]] <- TRUE
  }
  which(hit)
}

# Put the nodes of one layer back at least one step apart, in the order their
# positions now give them.
.respace_layer <- function(at, pos, layer, sep = 1) {
  idx <- which(at == layer)
  idx <- idx[order(pos[idx])]
  pos[idx] <- .place_layer(pos[idx], sep)
  pos
}

# How many pairs of ties cross, taking each tie as straight, between the ties
# `a` and the ties `b` (rows of `el`). Two ties that share a node are not
# counted, since they meet there rather than cross. Pairs that are in both `a`
# and `b` are counted twice.
.cross_pairs <- function(el, at, pos, a = seq_len(nrow(el)), b = a) {
  if (!length(a) || !length(b)) return(0L)
  p1 <- el[a, 1]; p2 <- el[a, 2]; q1 <- el[b, 1]; q2 <- el[b, 2]
  # The side of the line through p and q that r falls on, for every tie of
  # `a` (rows) against every node r of `b` (columns).
  side <- function(p, q, r) {
    dx <- at[q] - at[p]; dy <- pos[q] - pos[p]
    val <- dx * outer(pos[p], pos[r], function(y0, y) y - y0) -
      dy * outer(at[p], at[r], function(x0, x) x - x0)
    # Points on the line itself would otherwise fall to one side or the other
    # by rounding alone.
    sign(val) * (abs(val) > 1e-9)
  }
  sideT <- function(p, q, r) t(side(p, q, r))
  x <- side(p1, p2, q1) * side(p1, p2, q2) < 0 &
    sideT(q1, q2, p1) * sideT(q1, q2, p2) < 0
  meet <- outer(p1, q1, "==") | outer(p1, q2, "==") |
    outer(p2, q1, "==") | outer(p2, q2, "==")
  as.integer(sum(x & !meet))
}

.count_crossings <- function(el, at, pos) .cross_pairs(el, at, pos) %/% 2L

# How many times a node lies on a tie that does not join it, between the ties
# `a` and the nodes `w`. As in `.ties_through()`, a tie across the layers is
# taken as straight, and a tie within a layer runs through the nodes of that
# layer between its ends.
.through_pairs <- function(el, at, pos, a, w, sep = 1) {
  if (!length(a) || !length(w)) return(0L)
  u <- el[a, 1]; v <- el[a, 2]
  lo <- pmin(at[u], at[v]); hi <- pmax(at[u], at[v])
  flat <- lo == hi
  ends <- outer(u, w, "==") | outer(v, w, "==")
  across <- outer(lo, at[w], "<") & outer(hi, at[w], ">")
  frac <- outer(-at[u], at[w], "+") / ifelse(flat, 1, at[v] - at[u])
  line <- pos[u] + (pos[v] - pos[u]) * frac
  near <- abs(line - rep(pos[w], each = length(a))) < sep / 4
  inlayer <- flat & outer(lo, at[w], "==") &
    outer(pmin(pos[u], pos[v]), pos[w], "<") &
    outer(pmax(pos[u], pos[v]), pos[w], ">")
  as.integer(sum(!ends & ((!flat & across & near) | inlayer)))
}

# The sweeps that ordered the layers saw only the layers next to each other,
# and a timeline's ties often span many layers, so some of its crossings are
# ones a better order would undo. Each node in turn is tried in every gap of
# its layer and a step up or down it, and moved to wherever leaves the fewest
# crossings without putting a node on a tie it does not join (see
# `.clear_ties()`). This repeats until no move helps. Only the ties of the
# nodes a move changes are counted again, so a move costs the number of
# those ties times the number of ties. Even so, the time grows with the
# number of nodes times the number of ties: 50 nodes and 67 ties take under a
# second, but 100 nodes and 149 ties take more than five. So a network where
# that product is more than 5000 is left as it is, and the passes stop at 10.
# A historiograph is usually drawn from 20 to 50 works.
.uncross_ties <- function(g, at, pos, sep = 1, passes = 10) {
  el <- igraph::as_edgelist(g, names = FALSE)
  el <- el[el[, 1] != el[, 2], , drop = FALSE]
  n <- length(pos)
  m <- nrow(el)
  if (m < 2L || n * m > 5000) return(pos)
  all_ties <- seq_len(m)
  all_nodes <- seq_len(n)
  ties_of <- lapply(all_nodes, function(v) which(el[, 1] == v | el[, 2] == v))
  # The part of the score that the nodes `moved` take part in.
  part <- function(p, moved) {
    a <- unique(unlist(ties_of[moved]))
    b <- setdiff(all_ties, a)
    c(.through_pairs(el, at, p, a, all_nodes, sep) +
        .through_pairs(el, at, p, b, moved, sep),
      .cross_pairs(el, at, p, a, b) + .cross_pairs(el, at, p, a, a) %/% 2L)
  }
  for (i in seq_len(passes)) {
    moved_any <- FALSE
    for (w in all_nodes) {
      layer <- which(at == at[w])
      if (!length(unlist(ties_of[layer]))) next
      others <- sort(pos[setdiff(layer, w)])
      # A step either way, every gap in the layer, and the row of each node
      # it is tied to, which lets a node that is alone in its layer move too.
      nbrs <- setdiff(unique(c(el[ties_of[[w]], ])), w)
      spots <- c(pos[w] + c(-1, -0.5, 0.5, 1) * sep, pos[nbrs],
                 if (length(others)) c(others[1] - sep,
                                       (others[-1] + others[-length(others)]) / 2,
                                       others[length(others)] + sep))
      base <- NULL
      best <- NULL
      gain <- c(0, 0)
      for (y in spots) {
        new <- pos
        new[w] <- y
        new <- .respace_layer(at, new, at[w], sep)
        moved <- layer[new[layer] != pos[layer]]
        if (!length(moved)) next
        after <- part(new, moved)
        before <- part(pos, moved)
        d <- after - before
        if (d[1] < gain[1] || (d[1] == gain[1] && d[1] <= 0 && d[2] < gain[2])) {
          gain <- d
          best <- new
        }
      }
      if (!is.null(best)) {
        pos <- best
        moved_any <- TRUE
      }
    }
    if (!moved_any) break
  }
  pos
}

# Nodes sharing a layer value land on the same coordinate, so nudge them apart.
.check_dup <- function(mat) {
  mat$y <- ifelse(duplicated(mat[c('x','y')]), mat$y*0.95, mat$y)
  mat
}

.rng <- function(r) {
  if (r == 1L) return(0)
  if (r > 1L) {
    x <- vector()
    x <- append(x, (-1))
    for (i in 1:(r - 1)) x <- append(x, ((-1) + (2L/(r - 1L)) * i))
    return(x * (r/50L))
  } else manynet::snet_abort(
    "A layout cannot be built for a negative number of nodes, but {r} was given.")
}

.nrm <- function(x, digits = 3) {
  if (isTRUE(length(x) == 1L) == TRUE) return(x)
  if (is.array(x) == TRUE) {
    xnorm <- (x[, 1] - min(x[, 1]))/(max(x[, 1]) - min(x[, 1]))
    rat <- (max(x[, 1]) - min(x[, 1]))/(max(x[, 2]) - min(x[, 2]))
    ynorm <- ((x[, 2] - min(x[, 2]))/(max(x[, 2]) - min(x[, 2]))) * (rat)
    ifelse(isTRUE(rat > 0) == FALSE,
           ynorm <- ((x[, 2] - min(x[, 2]))/(max(x[, 2]) -
                                               min(x[, 2]))) * (1L/rat), NA)
    return(round(data.frame(X = xnorm, Y = ynorm), digits))
  }
  else if (is.vector(x) == TRUE) {
    return(round((x - min(x))/(max(x) - min(x)), digits))
  }
}

# Engine ------------------------------------------------------------------
#
# "layered", "lineage", "railway" and "ladder" are one layout drawn four
# ways. Each node gets a rank, which becomes one axis, and a
# position within its rank, which becomes the other. The members differ only
# in which axis carries the rank and in how a rank is aligned, so the work
# lives here once and each layout is a wrapper over it.
#
# Two costs are minimised, and they are separate problems.
# `.tighten_layers()` shortens the ties by choosing the ranks;
# `.straighten()` shortens them sideways by choosing the positions.

# Rank each node by its distance from a root, so that a rank is a generation.
# A node is ranked only once every node pointing at it has been, which is what
# makes each tie point down at least one rank.
.rank_layers <- function(g) {
  n <- igraph::vcount(g)
  rank <- rep(NA_integer_, n)
  indeg <- igraph::degree(g, mode = "in")
  adj <- lapply(igraph::adjacent_vertices(g, igraph::V(g), mode = "out"),
                as.integer)
  queue <- which(indeg == 0)
  current <- 0L
  while (length(queue)) {
    rank[queue] <- current
    nxt <- integer(0)
    for (v in queue) for (w in adj[[v]]) {
      indeg[w] <- indeg[w] - 1L
      if (indeg[w] == 0L) nxt <- c(nxt, w)
    }
    queue <- nxt
    current <- current + 1L
  }
  # A node caught in a cycle never has its in-degree fall to zero, so it keeps
  # the NA it started with. Give those a rank of their own below the rest,
  # rather than letting an NA coordinate reach the drawing.
  if (anyNA(rank)) rank[is.na(rank)] <- current
  rank
}

# Shorten the ties by moving nodes down the ranks. `.rank_layers()` puts every
# node as high as it can go, which pins a parent whose only child is several
# generations down to the top rank and manufactures a long tie to reach it.
# Each node moves instead to the median of its neighbours' ranks, clamped to
# the range its own ties leave it, until nothing moves. The clamp keeps every
# intermediate state feasible, so the loop can stop at any point.
.tighten_layers <- function(g, rank = NULL, times = 50) {
  if (is.null(rank)) rank <- .rank_layers(g)
  n <- igraph::vcount(g)
  if (n == 0L) return(rank)
  parents <- lapply(igraph::adjacent_vertices(g, igraph::V(g), mode = "in"),
                    as.integer)
  children <- lapply(igraph::adjacent_vertices(g, igraph::V(g), mode = "out"),
                     as.integer)
  for (i in seq_len(times)) {
    moved <- FALSE
    for (v in seq_len(n)) {
      up <- parents[[v]]
      down <- children[[v]]
      if (!length(up) && !length(down)) next
      lower <- if (length(up)) max(rank[up]) + 1L else -Inf
      upper <- if (length(down)) min(rank[down]) - 1L else Inf
      if (lower > upper) next
      want <- stats::median(rank[c(up, down)])
      new <- round(min(max(want, lower), upper))
      if (is.finite(new) && new != rank[v]) {
        rank[v] <- as.integer(new)
        moved <- TRUE
      }
    }
    if (!moved) break
  }
  .compact_ranks(rank)
}

# `igraph::layout_with_sugiyama()` numbers its layers the other way up from
# `.rank_layers()`, giving a source the highest layer rather than the lowest.
# Turn the ranks over where the ties mostly point up them, so that whichever
# rule assigned them, rank 0 is where the ties start.
.orient_ranks <- function(g, rank) {
  el <- igraph::as_edgelist(g, names = FALSE)
  if (!nrow(el)) return(rank)
  if (mean(rank[el[, 2]] - rank[el[, 1]]) < 0) rank <- max(rank) - rank
  rank
}

# Ranks are used as indices further on, so close any gaps the tightening left.
.compact_ranks <- function(rank) as.integer(match(rank, sort(unique(rank))) - 1L)

# Place one rank as close as it can get to where its nodes want to be, given
# the order they are already in and a minimum separation between neighbours.
# Subtracting `k * sep` turns the separation constraints into a plain
# monotonicity constraint, and isotonic regression solves that exactly, so
# there is no iteration to tune and no drift towards either side.
.place_layer <- function(want, sep = 1) {
  if (length(want) < 2L) return(want)
  k <- seq_along(want)
  stats::isoreg(k, want - k * sep)$yf + k * sep
}

# Straighten the ties. Each node is pulled towards the median position of its
# neighbours in the rank above or below, alternately, and each rank is then
# placed by `.place_layer()`, which keeps the order the crossing-minimisation
# sweeps found. A node with no neighbours in the direction being swept stays
# where it is, so it is not dragged out of its family.
.straighten <- function(g, pos, rank, sweeps = 20, sep = 1) {
  ranks <- sort(unique(rank))
  if (length(ranks) < 2L) return(pos)
  parents <- lapply(igraph::adjacent_vertices(g, igraph::V(g), mode = "in"),
                    as.integer)
  children <- lapply(igraph::adjacent_vertices(g, igraph::V(g), mode = "out"),
                     as.integer)
  for (s in seq_len(sweeps)) {
    downwards <- s %% 2L == 1L
    order_r <- if (downwards) ranks[-1] else rev(ranks)[-1]
    look <- if (downwards) parents else children
    for (r in order_r) {
      idx <- which(rank == r)
      if (!length(idx)) next
      idx <- idx[order(pos[idx])]
      want <- vapply(idx, function(v) {
        nb <- look[[v]]
        if (!length(nb)) pos[v] else stats::median(pos[nb])
      }, numeric(1))
      pos[idx] <- .place_layer(want, sep)
    }
  }
  pos
}

# Give every rank the same integer spacing, so that the ranks line up like the
# rungs of a ladder. This is what "railway" and "ladder" ask for, and it is not
# what `layout_snap = TRUE` does: that snaps the whole plot to a square grid.
.align_rungs <- function(pos, rank) {
  for (r in unique(rank)) {
    idx <- which(rank == r)
    pos[idx] <- rank(pos[idx], ties.method = "first")
  }
  pos
}

# Lay each weakly connected component out on its own and pack the results side
# by side, largest first. Ranks stay on one shared scale, so the components
# share their rows rather than floating; only the positions are offset. This is
# what keeps one family from being drawn through another.
.pack_components <- function(g, FUN, gap = 0.05) {
  memb <- igraph::components(g, mode = "weak")$membership
  if (length(unique(memb)) < 2L) return(FUN(g, seq_len(igraph::vcount(g))))
  n <- igraph::vcount(g)
  out <- data.frame(pos = numeric(n), rank = numeric(n))
  parts <- list()
  total <- 0
  for (cc in names(sort(table(memb), decreasing = TRUE))) {
    idx <- which(memb == as.integer(cc))
    co <- FUN(igraph::induced_subgraph(g, idx), idx)
    parts[[cc]] <- list(idx = idx, co = co)
    total <- total + diff(range(co$pos)) + 1
  }
  offset <- 0
  for (part in parts) {
    out$pos[part$idx] <- part$co$pos - min(part$co$pos) + offset
    out$rank[part$idx] <- part$co$rank
    offset <- offset + diff(range(part$co$pos)) + 1 + gap * total
  }
  out
}

# Rank and position every node, in node order. `ranks` chooses how the ranks
# are assigned and `alignment` how a rank is spread out; the caller decides
# which axis each becomes.
.layer_coords <- function(.data, layers = NULL,
                          ranks = c("tight", "generation", "compact"),
                          alignment = c("straight", "rungs"),
                          times = 1000, sweeps = 20, pack = TRUE, ...) {
  ranks <- match.arg(ranks)
  alignment <- match.arg(alignment)
  g <- manynet::as_igraph(.data)
  if (is.null(layers) && ranks != "compact" && !manynet::is_acyclic(g)) {
    manynet::snet_info(
      "The {.val {ranks}} ranks need an acyclic network,",
      "so {.val compact} ranks are used instead.", ...)
    ranks <- "compact"
  }
  one <- function(sub, idx) {
    # `layers`, where given, is indexed over the whole network, so it is cut
    # down to the component being laid out.
    lo <- .sugiyama_layout(sub, layers = .layers_for(sub, layers[idx], ranks),
                           times = times)
    rank <- .compact_ranks(lo[, 2])
    # Layers the caller gave are used as they are; ones we derived are turned
    # the right way up first.
    if (is.null(layers)) rank <- .orient_ranks(sub, rank)
    coords <- data.frame(pos = as.numeric(lo[, 1]), rank = rank)
    coords$pos <- if (alignment == "rungs") {
      .align_rungs(coords$pos, coords$rank)
    } else .straighten(sub, coords$pos, coords$rank, sweeps = sweeps)
    # Centre the component as a whole. Centring each rank separately would
    # shift the ranks against each other and undo the straightening.
    coords$pos <- coords$pos - mean(range(coords$pos))
    coords
  }
  out <- if (pack) .pack_components(g, one) else
    one(g, seq_len(igraph::vcount(g)))
  out$pos <- out$pos - mean(range(out$pos))
  out
}

# Where the layers are given -- the two modes of a two-mode network, say --
# they are used as they are, and the ranking rules do not apply.
.layers_for <- function(g, layers, ranks) {
  if (!is.null(layers)) return(layers)
  switch(ranks,
         tight = .tighten_layers(g),
         generation = .rank_layers(g),
         compact = NULL)
}

# Sugiyama-style layout with dummy nodes and barycenter heuristic
# for better edge crossing minimization
.sugiyama_layout <- function(g, layers = NULL, times = 100) {
  n <- igraph::vcount(g)
  el <- igraph::as_edgelist(g, names = FALSE)
  # Layer assignment
  if (is.null(layers)) {
    lo <- igraph::layout_with_sugiyama(g, maxiter = times)
    node_layer <- lo$layout[, 2]
  } else {
    node_layer <- layers
  }
  layer_vals <- sort(unique(node_layer))
  n_layers <- length(layer_vals)
  if (n_layers < 2) {
    return(cbind(seq_len(n), node_layer))
  }
  # Map layers to 0-based indices (used as list keys offset by 1)
  layer_idx <- match(node_layer, layer_vals) - 1L
  # Insert dummy nodes for edges spanning multiple layers
  dummy_id <- n
  # For barycenter, we need per-layer node lists and inter-layer edges
  all_layer <- layer_idx  # will grow with dummies
  # Build inter-layer edges (only between adjacent layers)
  inter_edges <- list()
  if (nrow(el) > 0) {
    for (i in seq_len(nrow(el))) {
      u <- el[i, 1]; v <- el[i, 2]
      lu <- layer_idx[u]; lv <- layer_idx[v]
      if (lu == lv) next
      # Ensure direction goes from lower layer to higher
      if (lu > lv) { tmp <- u; u <- v; v <- tmp; tmp <- lu; lu <- lv; lv <- tmp }
      if (lv - lu == 1) {
        inter_edges[[length(inter_edges) + 1]] <- c(u, v)
      } else {
        # Insert dummy nodes
        prev <- u
        for (k in (lu + 1):(lv - 1)) {
          dummy_id <- dummy_id + 1
          all_layer <- c(all_layer, k)
          inter_edges[[length(inter_edges) + 1]] <- c(prev, dummy_id)
          prev <- dummy_id
        }
        inter_edges[[length(inter_edges) + 1]] <- c(prev, v)
      }
    }
  }
  total_nodes <- length(all_layer)
  if (length(inter_edges) == 0) {
    return(cbind(seq_len(n), node_layer))
  }
  inter_edges_mat <- do.call(rbind, inter_edges)
  # Build per-layer node lists
  layer_nodes <- lapply(0:(n_layers - 1), function(k) which(all_layer == k))
  # Initialize x positions: sequential within each layer
  x_pos <- rep(0, total_nodes)
  for (k in seq_along(layer_nodes)) {
    nodes_in_layer <- layer_nodes[[k]]
    x_pos[nodes_in_layer] <- seq_along(nodes_in_layer)
  }
  # Build forward/backward adjacency for the expanded graph. `split()` keeps
  # each node's neighbours in the order of the ties, as appending them did.
  all_ids <- factor(inter_edges_mat[, 1], levels = seq_len(total_nodes))
  fwd_adj <- unname(split(inter_edges_mat[, 2], all_ids))
  bwd_adj <- unname(split(inter_edges_mat[, 1],
                          factor(inter_edges_mat[, 2],
                                 levels = seq_len(total_nodes))))
  # Which neighbours each layer averages over is the same in every sweep, so
  # it is worked out once here rather than node by node in each sweep.
  fwd_index <- lapply(layer_nodes, .barycentre_index, nbrs = fwd_adj)
  bwd_index <- lapply(layer_nodes, .barycentre_index, nbrs = bwd_adj)
  # Barycenter crossing minimization sweeps. The sweeps are deterministic, so
  # once an ordering comes round again the sweeps only repeat themselves: a
  # fixed point stays put, and a cycle goes round. The ordering that the
  # remaining sweeps would end on is then known without running them.
  seen <- list(x_pos)
  keys <- paste(x_pos, collapse = ",")
  for (iter in seq_len(times)) {
    # Forward sweep: layer 1 to n_layers-1
    for (k in 2:n_layers) {
      nodes_k <- layer_nodes[[k]]
      if (length(nodes_k) <= 1) next
      ord <- order(.barycentres(bwd_index[[k]], x_pos))
      x_pos[nodes_k[ord]] <- seq_along(nodes_k)
    }
    # Backward sweep: layer n_layers-2 to 0
    for (k in (n_layers - 1):1) {
      nodes_k <- layer_nodes[[k]]
      if (length(nodes_k) <= 1) next
      ord <- order(.barycentres(fwd_index[[k]], x_pos))
      x_pos[nodes_k[ord]] <- seq_along(nodes_k)
    }
    key <- paste(x_pos, collapse = ",")
    again <- match(key, keys)
    if (!is.na(again)) {
      # `seen[[j]]` is the ordering after sweep j - 1, and the cycle from it
      # takes `period` sweeps to come round.
      period <- iter - (again - 1L)
      x_pos <- seen[[again + (times - (again - 1L)) %% period]]
      break
    }
    seen[[iter + 1L]] <- x_pos
    keys[iter + 1L] <- key
  }
  # Extract coordinates for original nodes only
  cbind(x_pos[seq_len(n)], node_layer)
}

# The nodes of one layer, with the neighbours each averages over laid end to
# end, so that `.barycentres()` can sum them all at once.
.barycentre_index <- function(nodes, nbrs) {
  nb <- nbrs[nodes]
  deg <- lengths(nb)
  end <- cumsum(deg)
  list(nodes = nodes, src = unlist(nb, use.names = FALSE),
       deg = deg, start = end - deg, end = end)
}

# The mean position of each node's neighbours, or the node's own position
# where it has none. Positions are whole numbers, so the sums taken from the
# running total are exact, and the means are those `mean()` would give.
.barycentres <- function(index, x_pos) {
  total <- c(0, cumsum(x_pos[index$src]))
  bc <- (total[index$end + 1L] - total[index$start + 1L]) / index$deg
  none <- index$deg == 0
  bc[none] <- x_pos[index$nodes[none]]
  bc
}
