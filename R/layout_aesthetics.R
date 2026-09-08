#' Checking how well a layout draws a network
#' @description
#'   These functions score a drawing rather than the network it draws,
#'   so that one layout can be compared with another on the same network.
#'   Each measures one of the qualities a layout algorithm may set out to
#'   optimise.
#'
#'   `check_crossings()` reports how many other ties each tie crosses.
#'   A drawing with no crossings at all is a planar drawing.
#'
#'   `check_slopes()` reports the slope each tie is drawn at.
#'   The number of distinct slopes is the slope number of the drawing,
#'   and a drawing of few slopes reads as an orderly one.
#'
#'   `check_lengths()` reports how long each tie is drawn,
#'   as a share of the diagonal of the drawing.
#'
#'   `check_angles()` reports the smallest angle between the ties that meet
#'   at each node.
#'   The smallest of these is the angular resolution of the drawing,
#'   and a wide angle is what keeps two ties from reading as one.
#'
#'   `check_stress()` reports how far the distances drawn
#'   depart from the distances through the network.
#'   A layout that draws two nodes twice as far apart as two others
#'   should be drawing a path twice as long.
#'
#'   `check_span()` reports how many rows of nodes each tie crosses.
#'   A layered layout should send most ties to the next row down,
#'   and a long tie is one that skips rows to get where it is going.
#'
#'   `check_offset()` reports how far each tie travels sideways,
#'   as a share of the width of the whole drawing.
#'   A tie that drops straight down scores zero.
#'
#'   `check_drawing()` runs every check above except the two for rows,
#'   and returns their headline numbers in one row.
#' @details
#' # Reading the scores
#'
#'   `check_drawing()` reports one number from each check, and the columns are
#'   not all read the same way.
#'   Every column is better small except `angle_min`, which is better large.
#'   `nodes`, `ties` and `angle_ideal` are not scores at all:
#'   the first two say what was drawn, so that two rows of one network can be
#'   told from two rows of different ones, and `angle_ideal` is the ceiling
#'   that `angle_min` is read against.
#'   No layout wins on every column.
#'   A layout is chosen by which columns the reader of the figure needs,
#'   rather than by how many of them it wins.
#'
#'   Each column is read as follows.
#'
#'   - `crossings` counts the pairs of ties that cross, so fewer is better,
#'     and zero is a planar drawing.
#'     Zero is not always available: a network of more than three times its
#'     nodes, less six, ties cannot be drawn on a plane without a crossing,
#'     and a two-mode network of more than twice its nodes, less four, ties
#'     cannot either.
#'     Where that floor is above zero, read the score against another layout
#'     of the same network rather than against zero.
#'   - `slopes` counts the directions the ties are drawn in, so fewer is
#'     better. The fewest any drawing can use is half the largest degree,
#'     rounded up, since the ties at the busiest node need that many
#'     directions to leave it by.
#'   - `length_total`, `length_max` and `length_cv` are all better small.
#'     Each length is a share of the diagonal, so it runs between 0 and 1,
#'     and 1 is a tie drawn corner to corner.
#'     A `length_cv` of 0 means every tie is drawn the same length.
#'     The total grows with the number of ties, so compare two drawings of one
#'     network by their total, and two networks by the `mean` attribute of
#'     `check_lengths()`.
#'   - `angle_min` is the angular resolution in degrees, and is better large.
#'     Read it as a share of `angle_ideal`, which is 360 degrees divided by
#'     the largest degree, and is the best any drawing of that network could
#'     do.
#'   - `stress` is Kruskal's stress-1, so 0 is a perfect drawing.
#'     Kruskal read 20% as poor, 10% as fair, 5% as good, and 2.5% as
#'     excellent.
#'     Those figures were set for psychometric data rather than for networks,
#'     which are harder: most pairs of nodes in a small-world network sit
#'     two or three steps apart, and a plane holds few such distances at once,
#'     so a score near 30% is ordinary and one near 5% is rare.
#'     A layout that never set out to draw path distances,
#'     such as "layered", "circle" or "configuration", scores poorly by design.
#'
#'   `check_span()` and `check_offset()` are left out of the row,
#'   since they measure rows of nodes and so only mean something for a layered
#'   layout.
#'   Run them beside it where the layout has rows.
#'   Both are better small.
#'   They answer different questions, and a layered layout needs both answered:
#'   `check_span()` asks whether the rows were well chosen,
#'   and `check_offset()` asks whether the nodes were well placed within them.
#'   The "layered" layout minimises each in turn, and its `ranks` and
#'   `alignment` arguments choose how.
#'
#'   The scores belong to the drawing rather than to the network,
#'   which is what separates `check_stress()` from the share of distance
#'   variance that `graphr()` reports beside it.
#'   Draw one network two ways and the stress changes, since one drawing
#'   holds its distances better than the other;
#'   the share of variance does not, since two dimensions can hold
#'   just as much of that network either way.
#'   A network whose variance is held poorly sets a floor
#'   that no layout gets under.
#'
#' # How the drawing is measured
#'
#'   Every check reads the straight line between two nodes.
#'   `graphr()` may draw a tie as an arc, a fan, or a bundle,
#'   in which case the score is an approximation of what is drawn.
#'   A loop is left out, since it joins a node to itself and so has neither
#'   direction nor length on the plane.
#'
#'   `check_crossings()` counts a proper crossing, where two ties meet away
#'   from their ends.
#'   Two ties that share a node are not counted, since they must meet there.
#'   Comparing every pair of ties costs the square of their number,
#'   so above `max_full` ties an evenly spaced sample of the ties is scored
#'   instead, and the `sampled` attribute records this.
#'   The sample is taken by position rather than at random,
#'   so that the same drawing scores the same on every call.
#'
#'   `check_slopes()` reads a slope as an angle between 0 and 180 degrees,
#'   since a tie drawn from left to right has the same slope as the same tie
#'   drawn from right to left.
#'   Two angles closer than `tolerance` are counted as one slope,
#'   because two lines that differ by a fraction of a degree read as parallel.
#'
#'   `check_lengths()` divides by the diagonal of the drawing,
#'   since a layout may place its nodes on any scale it likes,
#'   and two layouts can only be compared once both are on the same one.
#'   The spread is reported twice, since the two answer different questions:
#'   `variance` is on the scale of the drawing, and `cv` divides by the mean,
#'   so only `cv` compares a drawing of long ties with a drawing of short ones.
#'
#'   `check_angles()` compares the directions of the ties at a node,
#'   so a node with fewer than two ties scores `NA`.
#'   Two ties drawn one on top of the other count once,
#'   since a multiplex pair is drawn apart by `graphr()` rather than at the
#'   same angle.
#'
#'   `check_stress()` scales the drawn distances to the path distances before
#'   it compares them, since a layout may place its nodes on any scale,
#'   and the ties are counted unweighted, as `layout_scaling()` counts them.
#'   Where a network is disconnected, the pairs with no path between them
#'   are left out of the score.
#'
#'   `check_span()` and `check_offset()` read the rows from the plot,
#'   as the axis on which the nodes take fewer distinct positions.
#'   This is the y axis for "layered" and the x axis for "lineage",
#'   so the same score can be compared across the two.
#'   For a layout with no rows at all, such as "stress",
#'   `check_span()` reports the distance in that axis' ranks,
#'   which is not meaningful; the two are for layered layouts.
#' @name check_layout
#' @family mapping
#' @seealso [check_colors()], which scores a palette rather than a drawing.
#' @source
#'   Kruskal, Joseph B. 1964.
#'   "Multidimensional scaling by optimizing goodness of fit to a nonmetric
#'   hypothesis", _Psychometrika_ 29(1): 1-27.
#'   \doi{10.1007/BF02289565}
#' @param x A plot, as `graphr()` returns.
#' @param max_full The largest number of ties to compare in full.
#'   By default 2000.
#' @param tolerance How many degrees apart two slopes must be to count as two.
#'   By default 1.
#' @returns
#'   `check_crossings()` returns one whole number for each tie, being how many
#'   other ties it crosses, with `total` and `mean` attributes,
#'   and a `sampled` attribute saying whether the ties were sampled.
#'
#'   `check_slopes()` returns one angle in degrees for each tie,
#'   between 0 and 180, with `distinct` and `tolerance` attributes.
#'
#'   `check_lengths()` returns one number between 0 and 1 for each tie,
#'   with `total`, `max`, `mean`, `variance` and `cv` attributes.
#'
#'   `check_angles()` returns one angle in degrees for each node,
#'   with `min`, `mean` and `ideal` attributes.
#'
#'   Each of those returns `NA` for a loop, or for a node with fewer than two
#'   ties.
#'
#'   `check_stress()` returns a single number of 0 or more,
#'   with a `scale` attribute holding the factor the drawn distances were
#'   scaled by, and a `pairs` attribute holding how many pairs were scored.
#'
#'   `check_span()` returns one whole number for each tie,
#'   with `total` and `mean` attributes holding the sum and the average.
#'
#'   `check_offset()` returns one number between 0 and 1 for each tie,
#'   with a `mean` attribute.
#'
#'   `check_drawing()` returns a one row data frame of class "check_drawing",
#'   holding the headline number from each check.
#'   Printing it names the direction each column is read in.
#' @examples
#' sw <- manynet::ison_southern_women
#' # Every check at once, for two layouts side by side
#' circled <- graphr(sw, layout = "circle")
#' rbind(stress = check_drawing(graphr(sw, layout = "stress")),
#'       circle = check_drawing(circled))
#' # How many crossings does the default layout draw?
#' attr(check_crossings(graphr(sw)), "total")
#' # How many slopes does a circle draw, and how evenly does it draw its ties?
#' attr(check_slopes(circled), "distinct")
#' attr(check_lengths(circled), "cv")
#' # Is there room between the ties that meet at a node?
#' attr(check_angles(circled), "min")
#' # And where the layout has rows, how long and how straight are its ties?
#' thrones <- manynet::to_uniplex(manynet::fict_thrones, "parent")
#' drawn <- graphr(thrones)
#' attr(check_span(drawn), "total")
#' attr(check_offset(drawn), "mean")
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


#' @rdname check_layout
#' @export
check_crossings <- function(x, max_full = 2000L) {
  seg <- .plot_segments(x)
  out <- rep(NA_real_, attr(seg, "ties"))
  drawn <- attr(seg, "kept")
  el <- attr(seg, "el")
  # Comparing every pair of ties costs the square of their number, so a large
  # network is scored on an evenly spaced sample of its ties instead, as
  # .stress_sources() samples nodes for the same reason.
  sampled <- length(drawn) > max_full
  if (sampled) {
    take <- unique(round(seq(1, length(drawn), length.out = max_full)))
    seg <- seg[take, , drop = FALSE]
    el <- el[take, , drop = FALSE]
    drawn <- drawn[take]
  }
  m <- nrow(seg)
  counts <- integer(m)
  if (m > 1L) {
    pairs <- which(upper.tri(matrix(TRUE, m, m)), arr.ind = TRUE)
    i <- pairs[, 1]
    j <- pairs[, 2]
    # Two ties that share a node meet there by construction, and that meeting
    # is not a crossing.
    apart <- el[i, 1] != el[j, 1] & el[i, 1] != el[j, 2] &
      el[i, 2] != el[j, 1] & el[i, 2] != el[j, 2]
    i <- i[apart]
    j <- j[apart]
    cross <- .segments_cross(seg[i, , drop = FALSE], seg[j, , drop = FALSE])
    if (any(cross)) {
      tab <- table(c(i[cross], j[cross]))
      counts[as.integer(names(tab))] <- as.integer(tab)
    }
  }
  out[drawn] <- counts
  structure(out, total = sum(counts)/2, mean = mean(counts),
            sampled = sampled)
}

#' @rdname check_layout
#' @export
check_slopes <- function(x, tolerance = 1) {
  seg <- .plot_segments(x)
  out <- rep(NA_real_, attr(seg, "ties"))
  # A slope runs from 0 to 180 degrees, since a tie drawn one way round has the
  # same slope as the same tie drawn the other way round.
  ang <- (atan2(seg[, "y2"] - seg[, "y1"], seg[, "x2"] - seg[, "x1"]) *
            180/pi) %% 180
  out[attr(seg, "kept")] <- ang
  bins <- round(ang/tolerance) %% max(round(180/tolerance), 1)
  structure(out, distinct = length(unique(bins)), tolerance = tolerance)
}

#' @rdname check_layout
#' @export
check_lengths <- function(x) {
  seg <- .plot_segments(x)
  lo <- attr(seg, "nodes")
  out <- rep(NA_real_, attr(seg, "ties"))
  len <- sqrt((seg[, "x2"] - seg[, "x1"])^2 + (seg[, "y2"] - seg[, "y1"])^2)
  # The diagonal of the drawing puts two layouts on the same scale, since a
  # layout may place its nodes on any scale it likes.
  diagonal <- sqrt(diff(range(lo[, 1]))^2 + diff(range(lo[, 2]))^2)
  if (diagonal > 0) len <- len/diagonal
  out[attr(seg, "kept")] <- len
  mn <- if (length(len)) mean(len) else NA_real_
  structure(out, total = sum(len), max = if (length(len)) max(len) else NA_real_,
            mean = mn,
            variance = if (length(len) > 1L) stats::var(len) else NA_real_,
            cv = if (length(len) > 1L && mn > 0) stats::sd(len)/mn else NA_real_)
}

#' @rdname check_layout
#' @export
check_angles <- function(x) {
  seg <- .plot_segments(x)
  el <- attr(seg, "el")
  lo <- attr(seg, "nodes")
  out <- rep(NA_real_, nrow(lo))
  # Each tie is read from both of its ends, since the angle belongs to the node
  # the ties meet at rather than to the tie.
  at <- c(el[, 1], el[, 2])
  to <- c(el[, 2], el[, 1])
  ang <- atan2(lo[to, 2] - lo[at, 2], lo[to, 1] - lo[at, 1]) * 180/pi
  for (i in unique(at)) {
    # Two ties drawn at the same angle are one direction as far as the reader
    # is concerned, and would otherwise report a resolution of zero.
    a <- sort(unique(round(ang[at == i], 9) %% 360))
    if (length(a) < 2L) next
    gaps <- c(diff(a), 360 - (a[length(a)] - a[1]))
    out[i] <- min(gaps)
  }
  deg <- tabulate(at, nbins = nrow(lo))
  structure(out, min = if (all(is.na(out))) NA_real_ else min(out, na.rm = TRUE),
            mean = if (all(is.na(out))) NA_real_ else mean(out, na.rm = TRUE),
            ideal = if (max(deg) > 0) 360/max(deg) else NA_real_)
}

#' @rdname check_layout
#' @export
check_drawing <- function(x) {
  crossings <- check_crossings(x)
  slopes <- check_slopes(x)
  lengths <- check_lengths(x)
  angles <- check_angles(x)
  out <- data.frame(
    nodes = nrow(.plot_coords(x)),
    ties = length(crossings),
    crossings = attr(crossings, "total"),
    slopes = attr(slopes, "distinct"),
    length_total = attr(lengths, "total"),
    length_max = attr(lengths, "max"),
    length_cv = attr(lengths, "cv"),
    angle_min = attr(angles, "min"),
    # The ceiling rather than a score: the smallest angle means little without
    # the widest one the degrees of that network allow.
    angle_ideal = attr(angles, "ideal"),
    stress = as.numeric(check_stress(x)))
  class(out) <- c("check_drawing", class(out))
  out
}

#' @export
print.check_drawing <- function(x, ...) {
  out <- as.data.frame(unclass(x), row.names = rownames(x))
  num <- vapply(out, is.numeric, logical(1)) &
    !names(out) %in% c("nodes", "ties", "crossings", "slopes")
  out[num] <- lapply(out[num], round, 3)
  print(out, ...)
  # A row of nine numbers says nothing about which way each is read, and the
  # one column read the other way round is the one a reader would guess wrong.
  cat("# Lower is better, except angle_min, where higher is better.\n",
      "# nodes, ties and angle_ideal are context, not scores.\n", sep = "")
  invisible(x)
}

# The two ends of each tie, as one row for each tie the plane can draw. A loop
# joins a node to itself, so it has neither direction nor length here and is
# dropped; the attributes carry what the callers need to put their scores back
# beside every tie of the network.
.plot_segments <- function(x) {
  lo <- as.matrix(.plot_coords(x))
  el <- .plot_ties(x)
  keep <- el[, 1] != el[, 2]
  el <- el[keep, , drop = FALSE]
  structure(cbind(x1 = lo[el[, 1], 1], y1 = lo[el[, 1], 2],
                  x2 = lo[el[, 2], 1], y2 = lo[el[, 2], 2]),
            kept = which(keep), ties = length(keep), nodes = lo, el = el)
}

# Whether each pair of segments crosses away from their ends. Two segments
# cross when each straddles the line the other lies on, which is what the two
# pairs of opposite orientations say. A pair that only touch, so that an
# orientation is zero, is not a crossing.
.segments_cross <- function(a, b) {
  orient <- function(px, py, qx, qy, rx, ry)
    sign((qx - px) * (ry - py) - (qy - py) * (rx - px))
  d1 <- orient(a[, "x1"], a[, "y1"], a[, "x2"], a[, "y2"], b[, "x1"], b[, "y1"])
  d2 <- orient(a[, "x1"], a[, "y1"], a[, "x2"], a[, "y2"], b[, "x2"], b[, "y2"])
  d3 <- orient(b[, "x1"], b[, "y1"], b[, "x2"], b[, "y2"], a[, "x1"], a[, "y1"])
  d4 <- orient(b[, "x1"], b[, "y1"], b[, "x2"], b[, "y2"], a[, "x2"], a[, "y2"])
  d1 * d2 < 0 & d3 * d4 < 0
}
