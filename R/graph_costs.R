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
#' @details
#'   The two answer different questions, and a layout needs both answered.
#'   `check_span()` asks whether the rows were well chosen,
#'   and `check_offset()` asks whether the nodes were well placed within them.
#'   The "hierarchy" layout minimises each in turn, and its `ranks` and
#'   `alignment` arguments choose how.
#'
#'   Which axis holds the rows is read from the plot,
#'   as the axis on which the nodes take fewer distinct positions.
#'   This is the y axis for "hierarchy" and the x axis for "alluvial",
#'   so the same score can be compared across the two.
#'   For a layout with no rows at all, such as "stress",
#'   `check_span()` reports the distance in that axis' ranks,
#'   which is not meaningful; the function is for layered layouts.
#' @name check_layout
#' @family mapping
#' @param x A plot, as `graphr()` returns.
#' @returns
#'   `check_span()` returns one whole number for each tie,
#'   with `total` and `mean` attributes holding the sum and the average.
#'
#'   `check_offset()` returns one number between 0 and 1 for each tie,
#'   with a `mean` attribute.
#' @examples
#' thrones <- manynet::to_layer(manynet::fict_thrones, "parent")
#' # How long are the ties of the default layout?
#' attr(check_span(graphr(thrones)), "total")
#' # And of the layers igraph would have chosen?
#' attr(check_span(graphr(thrones, ranks = "compact")), "total")
#' # How straight are they?
#' attr(check_offset(graphr(thrones)), "mean")
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

.plot_coords <- function(x) {
  if (!all(c("x", "y") %in% names(x[["data"]]))) manynet::snet_abort(
    "{.arg x} should be a plot with node coordinates,",
    "such as one {.fn graphr} returns.")
  x[["data"]][, c("x", "y")]
}

.plot_ties <- function(x) {
  g <- attr(x[["data"]], "graph")
  if (is.null(g)) manynet::snet_abort(
    "{.arg x} should be a plot that carries the network it draws,",
    "such as one {.fn graphr} returns.")
  igraph::as_edgelist(manynet::as_igraph(g), names = FALSE)
}
