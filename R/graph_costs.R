#' Checking how well a layout draws its ties
#' @description
#'   These functions score a drawing rather than the network it draws,
#'   so that a layout can be compared with another on the same network.
#'
#'   `check_span()` reports how many rows of nodes each tie crosses.
#'   A layered layout should send most ties to the next row down,
#'   and a long tie is one that skips rows to get where it is going.
#'
#'   `check_offset()` reports how far each tie travels sideways,
#'   as a share of the width of the whole drawing.
#'   A tie that drops straight down scores zero.
#'
#'   `check_stress()` reports how far the distances drawn
#'   depart from the distances through the network.
#'   A layout that draws two nodes twice as far apart as two others
#'   should be drawing a path twice as long.
#' @details
#'   `check_span()` and `check_offset()` answer different questions,
#'   and a layered layout needs both answered.
#'   `check_span()` asks whether the rows were well chosen,
#'   and `check_offset()` asks whether the nodes were well placed within them.
#'   The "layered" layout minimises each in turn, and its `ranks` and
#'   `alignment` arguments choose how.
#'
#'   Which axis holds the rows is read from the plot,
#'   as the axis on which the nodes take fewer distinct positions.
#'   This is the y axis for "layered" and the x axis for "lineage",
#'   so the same score can be compared across the two.
#'   For a layout with no rows at all, such as "stress",
#'   `check_span()` reports the distance in that axis' ranks,
#'   which is not meaningful; the function is for layered layouts.
#'
#'   `check_stress()` applies to any layout, since every layout draws its
#'   nodes some distance apart, and the score is the share of the path
#'   distances that the drawn distances get wrong.
#'   It is Kruskal's stress-1, so 0 is a perfect drawing,
#'   and Kruskal read 20% as poor, 10% as fair, 5% as good,
#'   and 2.5% as excellent.
#'   Those figures were set for psychometric data rather than for networks,
#'   which are harder: most pairs of nodes in a small-world network sit
#'   two or three steps apart, and a plane holds few such distances at once,
#'   so a score near 30% is ordinary and one near 5% is rare.
#'   A layout that never set out to draw path distances,
#'   such as "layered", "circle" or "configuration",
#'   scores poorly by design.
#'
#'   The drawn distances are scaled to the path distances before they are
#'   compared, since a layout may place its nodes on any scale it likes,
#'   and the ties are counted unweighted, as `layout_scaling()` counts them.
#'   Where a network is disconnected, the pairs with no path between them
#'   are left out of the score.
#' @name check_layout
#' @family mapping
#' @source
#'   Kruskal, Joseph B. 1964.
#'   "Multidimensional scaling by optimizing goodness of fit to a nonmetric
#'   hypothesis", _Psychometrika_ 29(1): 1-27.
#'   \doi{10.1007/BF02289565}
#' @param x A plot, as `graphr()` returns.
#' @returns
#'   `check_span()` returns one whole number for each tie,
#'   with `total` and `mean` attributes holding the sum and the average.
#'
#'   `check_offset()` returns one number between 0 and 1 for each tie,
#'   with a `mean` attribute.
#'
#'   `check_stress()` returns a single number of 0 or more,
#'   with a `scale` attribute holding the factor the drawn distances were
#'   scaled by, and a `pairs` attribute holding how many pairs were scored.
#' @examples
#' thrones <- manynet::to_uniplex(manynet::fict_thrones, "parent")
#' # How long are the ties of the default layout?
#' attr(check_span(graphr(thrones)), "total")
#' # And of the layers igraph would have chosen?
#' attr(check_span(graphr(thrones, ranks = "compact")), "total")
#' # How straight are they?
#' attr(check_offset(graphr(thrones)), "mean")
#' # Which layout draws the path distances best?
#' check_stress(graphr(manynet::ison_southern_women, layout = "scaling"))
#' check_stress(graphr(manynet::ison_southern_women, layout = "circle"))
NULL

#' @rdname check_layout
#' @export
check_span <- function(x) {
  lo <- .plot_coords(x)
  # The rows are the axis the nodes take fewer distinct positions on, so that
  # the score reads the same whether the layout runs downwards or rightwards.
  rows <- if (length(unique(lo$y)) <= length(unique(lo$x))) lo$y else lo$x
  rank <- match(rows, sort(unique(rows)))
  el <- .plot_ties(x)
  out <- abs(rank[el[, 2]] - rank[el[, 1]])
  structure(out, total = sum(out), mean = mean(out))
}

#' @rdname check_layout
#' @export
check_offset <- function(x) {
  lo <- .plot_coords(x)
  across <- if (length(unique(lo$y)) <= length(unique(lo$x))) lo$x else lo$y
  width <- diff(range(across))
  el <- .plot_ties(x)
  out <- abs(across[el[, 2]] - across[el[, 1]])
  if (width > 0) out <- out / width
  structure(out, mean = mean(out))
}

#' @rdname check_layout
#' @export
check_stress <- function(x) {
  crd <- as.matrix(.plot_coords(x))
  g <- manynet::as_igraph(.plot_graph(x))
  src <- .stress_sources(igraph::vcount(g))
  .stress1(igraph::distances(g, v = src, weights = NA), crd, src)
}

# Kruskal's stress-1, between the path distances from a set of source nodes to
# every node, and the distances the layout draws between the same pairs.
# Shared with layout_scaling(), which reports the same number for the layout it
# has just computed.
.stress1 <- function(dis, crd, sources) {
  drawn <- vapply(sources, function(i)
    sqrt(rowSums((crd - rep(crd[i, ], each = nrow(crd)))^2)),
    numeric(nrow(crd)))
  drawn <- t(drawn)
  keep <- is.finite(dis) & dis > 0
  d <- drawn[keep]
  target <- dis[keep]
  # A layout may place its nodes on any scale, so the drawn distances are
  # scaled to the path distances before they are compared. Without this a
  # pivot scaling of ison_southern_women, whose coordinates run much larger,
  # scores 8.53 where it should score 0.32.
  if (!length(d) || sum(d^2) == 0 || sum(target^2) == 0)
    return(structure(NA_real_, scale = NA_real_, pairs = length(d)))
  b <- sum(d * target) / sum(d^2)
  structure(sqrt(sum((b * d - target)^2) / sum(target^2)),
            scale = b, pairs = length(d))
}

# The nodes the distances are measured from. Every node where the network is
# small enough, and an evenly spaced sample of them otherwise, since a full
# distance matrix holds n^2 numbers and is soon larger than the network it
# measures. The sample is taken by position rather than at random, so that the
# same drawing scores the same on every call.
.stress_sources <- function(n, max_full = 500L) {
  if (n <= max_full) return(seq_len(n))
  unique(round(seq(1, n, length.out = max_full)))
}

.plot_coords <- function(x) {
  if (!all(c("x", "y") %in% names(x[["data"]]))) manynet::snet_abort(
    "{.arg x} should be a plot with node coordinates,",
    "such as one {.fn graphr} returns.")
  x[["data"]][, c("x", "y")]
}

.plot_ties <- function(x) {
  igraph::as_edgelist(manynet::as_igraph(.plot_graph(x)), names = FALSE)
}

.plot_graph <- function(x) {
  g <- attr(x[["data"]], "graph")
  if (is.null(g)) manynet::snet_abort(
    "{.arg x} should be a plot that carries the network it draws,",
    "such as one {.fn graphr} returns.")
  g
}
