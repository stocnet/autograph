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
#' #   mutate(year = rep(c(1985, 1990, 1995, 2000), times = 2)) |>
#' #   graphr(layout = "lineage", ranks = "year")
#' @export
layout_lineage <- function(.data,
                           ranks = c("tight", "generation", "compact"),
                           alignment = c("straight", "rungs"),
                           circular = FALSE, times = 1000, rank = NULL){
  ranks <- .absorb_rank(ranks, rank)
  # The same coordinates as "layered", with the axes exchanged, so that the
  # layers run left to right rather than bottom to top.
  lo <- .layer_axes(.data, ranks = ranks, alignment = alignment, times = times)
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
.layer_axes <- function(.data, ranks, alignment, times) {
  g <- manynet::as_igraph(.data)
  twomode <- manynet::is_twomode(.data)
  spec <- .resolve_ranks(.data, ranks)
  layers <- if (!is.null(spec$values)) .compact_ranks(spec$values) + 1L else
    if (twomode) ifelse(igraph::V(g)$type, 2, 1) else NULL
  lo <- .layer_coords(g, layers = layers, ranks = spec$method,
                      alignment = alignment, times = times,
                      pack = !twomode)
  x <- lo$pos
  y <- if (twomode) lo$rank else -lo$rank
  # `as_igraph()` can reorder the nodes of a two-mode network, so the
  # coordinates are put back into the order the caller's network holds them in.
  if (twomode && "name" %in% igraph::vertex_attr_names(.data)) {
    ord <- order(match(igraph::vertex_attr(g, "name"),
                       igraph::vertex_attr(.data, "name")))
    x <- x[ord]
    y <- y[ord]
  }
  # Values the caller gave are already in that order, and are placed in
  # proportion to themselves rather than at even steps. They descend the page
  # like the layers the engine works out, so that the smallest value -- the
  # earliest date, the first generation -- is at the top.
  if (!is.null(spec$values)) y <- -spec$values
  if (length(unique(x)) > 1) x <- .rescale(x)
  if (length(unique(y)) > 1) y <- .rescale(y)
  cbind(x, y)
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
# what `snap = TRUE` does: that snaps the whole plot to a square grid.
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
  # Build adjacency between original nodes
  adj <- vector("list", n)
  radj <- vector("list", n)
  for (i in seq_len(n)) { adj[[i]] <- integer(0); radj[[i]] <- integer(0) }
  if (nrow(el) > 0) {
    for (i in seq_len(nrow(el))) {
      u <- el[i, 1]; v <- el[i, 2]
      adj[[u]] <- c(adj[[u]], v)
      radj[[v]] <- c(radj[[v]], u)
    }
  }
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
  # Build forward/backward adjacency for the expanded graph
  fwd_adj <- vector("list", total_nodes)
  bwd_adj <- vector("list", total_nodes)
  for (i in seq_len(total_nodes)) { fwd_adj[[i]] <- integer(0); bwd_adj[[i]] <- integer(0) }
  if (!is.null(inter_edges_mat) && nrow(inter_edges_mat) > 0) {
    for (i in seq_len(nrow(inter_edges_mat))) {
      u <- inter_edges_mat[i, 1]; v <- inter_edges_mat[i, 2]
      fwd_adj[[u]] <- c(fwd_adj[[u]], v)
      bwd_adj[[v]] <- c(bwd_adj[[v]], u)
    }
  }
  # Barycenter crossing minimization sweeps
  for (iter in seq_len(times)) {
    # Forward sweep: layer 1 to n_layers-1
    for (k in 2:n_layers) {
      nodes_k <- layer_nodes[[k]]
      if (length(nodes_k) <= 1) next
      bc <- sapply(nodes_k, function(nd) {
        neighbors <- bwd_adj[[nd]]
        if (length(neighbors) == 0) return(x_pos[nd])
        mean(x_pos[neighbors])
      })
      ord <- order(bc)
      x_pos[nodes_k[ord]] <- seq_along(nodes_k)
    }
    # Backward sweep: layer n_layers-2 to 0
    for (k in (n_layers - 1):1) {
      nodes_k <- layer_nodes[[k]]
      if (length(nodes_k) <= 1) next
      bc <- sapply(nodes_k, function(nd) {
        neighbors <- fwd_adj[[nd]]
        if (length(neighbors) == 0) return(x_pos[nd])
        mean(x_pos[neighbors])
      })
      ord <- order(bc)
      x_pos[nodes_k[ord]] <- seq_along(nodes_k)
    }
  }
  # Extract coordinates for original nodes only
  cbind(x_pos[seq_len(n)], node_layer)
}
