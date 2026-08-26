#' Correspondence layout
#'
#' @description
#'   The "correspondence" layout places nodes by correspondence analysis,
#'   so that two nodes are drawn together where they have similar ties.
#'   Where the "scaling" layout reads the paths between nodes,
#'   this one reads the profile of each node's ties,
#'   and so two nodes with no tie between them can still be drawn together
#'   if they are tied to the same others.
#'
#'   This is the usual way to draw a two-mode network,
#'   since correspondence analysis takes a rectangular table
#'   and places its rows and its columns in one space.
#'   Both modes are therefore drawn on one pair of axes.
#'
#'   Like the "scaling" layout, the coordinates can be read,
#'   and so this layout draws labelled axes at a fixed ratio.
#'   Each axis is labelled with the share of the network's inertia
#'   that the dimension holds.
#' @name layout_correspondence
#' @template param_ggraphlayouts
#' @param direction Which ties to read for a directed network,
#'   as one of "all", "out", or "in".
#'   By default this is "all", which reads a tie in either direction,
#'   so that each node has one position.
#'   "out" places each node by the ties it sends,
#'   and "in" by the ties it receives.
#'   This is ignored where the network is undirected or two-mode.
#' @param double Whether to split each tie into a positive and a negative part,
#'   so that a signed network can be drawn.
#'   By default this is `FALSE`, and a signed network is not drawn,
#'   since correspondence analysis is not defined for a negative tie.
#' @details
#'   Correspondence analysis divides the ties of each node by how many ties
#'   that node has, and so places nodes by the shape of their ties
#'   rather than by how many they have.
#'   The distance drawn is the chi-square distance between two such profiles.
#'
#'   A two-mode network is read as its incidence matrix,
#'   one row for each node of the first mode and one column for each of the
#'   second. A one-mode network is read as its adjacency matrix instead,
#'   as is a multimodal network that has ties within its modes as well as
#'   between them, so that no tie is dropped.
#'
#'   Tie weights are read as they are, since correspondence analysis was built
#'   for counts and a weight counts in the same way.
#'   A negative weight has no such reading, which is why a signed network
#'   needs `double = TRUE`. That stacks the positive network and the negative
#'   network side by side, doubling the width of the table,
#'   so that a node is placed by both who it is tied to positively
#'   and who it is tied to negatively.
#'   A pair of nodes with no tie between them counts in neither half.
#' @section Reading the plot:
#'   Two nodes of the same mode drawn together have similar ties.
#'   A node drawn near the origin has a profile close to the average,
#'   or is held poorly by the two dimensions drawn: these are not the same
#'   thing, and `graphr()` names the nodes for which it is the second.
#'
#'   A node of one mode drawn near a node of the other mode is *not*
#'   necessarily tied to it.
#'   Only the distances within a mode can be read this way.
#'
#'   Where a network runs along one strong gradient,
#'   correspondence analysis draws it as an arch rather than as a line.
#'   This is expected of the method, and the second dimension then repeats
#'   the first rather than adding to it.
#'
#'   Where a network is disconnected, the first dimensions merely separate its
#'   components, and say little about the nodes within them.
#' @section Reading the inertia:
#'   The share of inertia a dimension holds is not a share of variance
#'   explained, and does not have a fixed ceiling to be read against.
#'   It is a share of however many dimensions the table has,
#'   which `attr(x, "fit")$scree` reports in full.
#'   Two dimensions of a table that has twelve start from a base of a sixth;
#'   two of a table that has thirty start from a base of a fifteenth.
#'   Compare the share drawn against that base rather than against 100%,
#'   and note that this can reverse the ranking the raw shares suggest.
#'   Bear in mind that an even share is a lenient base, since inertia is
#'   never spread evenly; the broken stick model asks what the dimensions
#'   would hold if the inertia were divided at random, and is the harder test.
#'   Neither is a standard statistic, and neither carries a threshold,
#'   so read them as a check on the raw share rather than as a verdict.
#'   `graphr()` says so at the console where two dimensions hold no more
#'   than a random division of the inertia would give them.
#'   To choose a number of dimensions properly, see Lorenzo-Seva (2011).
#'
#'   These shares need no correction.
#'   The Benzécri correction, and Greenacre's adjusted version of it,
#'   exist because the indicator matrix that *multiple* correspondence
#'   analysis is run on invents dimensions that deflate every share.
#'   This layout runs simple correspondence analysis on one two-way table,
#'   which invents nothing, so the shares reported are already exact.
#' @family mapping
#' @source
#'   Greenacre, Michael. 2017.
#'   _Correspondence Analysis in Practice_, 3rd ed.
#'   Boca Raton: Chapman and Hall.
#'   \doi{10.1201/9781315369983}
#'
#'   Lorenzo-Seva, Urbano. 2011.
#'   "Horn's parallel analysis for selecting the number of dimensions in
#'   correspondence analysis",
#'   _Methodology_ 7(3): 96-105.
#'   \doi{10.1027/1614-2241/a000027}
#'
#'   Constantine, A.G., and John C. Gower. 1978.
#'   "Graphical representation of asymmetric matrices",
#'   _Journal of the Royal Statistical Society C_ 27(3): 297-304.
#'   \doi{10.2307/2347234}
#' @examples
#' graphr(manynet::ison_southern_women, layout = "correspondence")
#' @export
layout_correspondence <- function(.data, direction = c("all", "out", "in"),
                                  double = FALSE, circular = FALSE, times = 1) {
  direction <- .check_choice(direction, c("all", "out", "in"), "direction")
  .data <- manynet::as_igraph(.data)
  n <- igraph::vcount(.data)
  # A network with no ties has no profiles to compare, and one with fewer than
  # three nodes has no shape a plane could hold.
  if (n < 3L || igraph::ecount(.data) == 0L) return(.to_lo(.trivial_coords(n)))
  tab <- .corresp_table(.data, direction, double)
  crd <- .corresp_coords(tab)
  res <- .to_lo(crd[["xy"]])
  # Carried on the coordinates rather than recomputed later, as the "scaling"
  # layout carries its own fit: the attribute survives create_layout(), so
  # graphr() can label the axes of the layout it actually drew.
  attr(res, "fit") <- list(
    type = "correspondence",
    inertia = crd[["inertia"]],
    total = crd[["total"]],
    # Every dimension, not only the two drawn. A share of inertia means little
    # on its own: what it is worth depends on how many dimensions it was won
    # against, and on how fast the rest fall away. See `?layout_correspondence`.
    scree = crd[["scree"]],
    cos2 = stats::setNames(crd[["cos2"]], manynet::node_names(.data)))
  res
}

#' @rdname layout_correspondence
#' @export
layout_tbl_graph_correspondence <- layout_correspondence

# The two-way table the analysis is run on, and whether its columns are nodes
# as well as its rows. Only a two-mode network read from its incidence matrix
# has nodes down both sides of the table; for every other network the columns
# are the same nodes read a second way, or the halves a doubled tie is split
# into, and only the rows are drawn.
.corresp_table <- function(g, direction, double) {
  # Only a network whose ties all run between the modes has an incidence
  # matrix that keeps every tie. A multilevel network is two-mode as well, but
  # also has ties within its modes, and manynet::as_matrix() would drop those,
  # so it is read as a square matrix like any one-mode network.
  bipartite <- manynet::is_twomode(g) && !.ag_is_multilevel(g)
  if (bipartite) {
    # manynet orders the nodes of the first mode before those of the second,
    # which is the order the rows and then the columns of the incidence matrix
    # are in, and so the order the coordinates are returned in.
    return(list(N = as.matrix(manynet::as_matrix(g)), bipartite = TRUE))
  }
  N <- as.matrix(manynet::as_matrix(g))
  if (manynet::is_directed(g)) {
    # "all" reads a tie in either direction, so that a node has one position
    # rather than the two an asymmetric table would give it. There is no
    # agreed way to place both at once. See Constantine and Gower (1978).
    N <- switch(direction, all = N + t(N), out = N, `in` = t(N))
  }
  if (double) {
    # The positive and the negative network, stacked side by side, so that the
    # table has no negative cell left in it and a node is placed by both.
    #
    # This is not Greenacre's doubling, though it is named for the doubled
    # width. Doubling maps a value on a scale to a pair that sums to a
    # constant, which here would turn every pair of nodes with no tie between
    # them into a neutral pair carrying as much mass as a real tie. A network
    # is mostly non-ties, so that would place the nodes by what they are not
    # tied to. Splitting rather than doubling leaves a non-tie counting in
    # neither half, as it should.
    N <- cbind(pmax(N, 0), pmax(-N, 0))
  }
  list(N = N, bipartite = FALSE)
}

# Correspondence analysis of a table, returning the coordinates of the nodes
# in it, the share of inertia each of the first two dimensions holds, and how
# well those two dimensions hold each node.
.corresp_coords <- function(tab) {
  N <- tab[["N"]]
  if (any(N < 0)) {
    manynet::snet_abort(
      "Correspondence analysis is not defined for a negative tie.",
      "Use {.code double = TRUE} to split the signs.")
  }
  # A node with no tie at all has no profile to place, and a mass of zero
  # would divide the analysis by zero, so it is set aside and returned to the
  # origin afterwards. graphr() drops isolates before the layout sees them, so
  # this is reached by a direct call, or by `isolates = "keep"`.
  keep_r <- rowSums(N) > 0
  keep_c <- colSums(N) > 0
  ca <- .corresp(N[keep_r, keep_c, drop = FALSE])
  rows <- .corresp_place(ca[["rows"]], keep_r)
  # Where the columns are nodes too, they follow the rows, as the second mode
  # follows the first. Where they are not, they are the doubled halves of the
  # ties, and are not drawn.
  if (tab[["bipartite"]]) {
    cols <- .corresp_place(ca[["cols"]], keep_c)
    crd <- rbind(rows[["xy"]], cols[["xy"]])
    cos2 <- c(rows[["cos2"]], cols[["cos2"]])
  } else {
    crd <- rows[["xy"]]
    cos2 <- rows[["cos2"]]
  }
  eig <- ca[["d"]]^2
  total <- sum(eig)
  # A table of I rows and J columns has min(I, J) - 1 dimensions at most, and
  # the decomposition returns a zero for each one the table does not support.
  # Those are dropped, so that the count is the number of dimensions the
  # network actually has to spread its inertia over.
  shares <- if (total > 0) eig[eig > total * 1e-12] / total else numeric()
  list(xy = crd, cos2 = cos2, total = total, scree = shares,
       inertia = if (total > 0) eig[1:2] / total else c(NA_real_, NA_real_))
}

# The standardised residuals of a table, decomposed. The principal coordinates
# put the rows and the columns on the same scale, which is what lets a
# two-mode network be drawn on one pair of axes.
.corresp <- function(N) {
  P <- N / sum(N)
  r <- rowSums(P)
  cm <- colSums(P)
  # Subtracting the outer product of the masses removes the trivial first
  # dimension, so every dimension the decomposition returns is one to read.
  S <- (P - outer(r, cm)) / outer(sqrt(r), sqrt(cm))
  sv <- svd(S)
  # The decomposition fixes the axes but not their direction, so the same
  # network could be drawn mirrored from one call to the next. Pointing each
  # dimension so that its largest coordinate is positive settles that.
  flip <- apply(sv$u, 2, function(x) if (x[which.max(abs(x))] < 0) -1 else 1)
  list(rows = .corresp_dims(sweep(sv$u, 2, sv$d * flip, "*") / sqrt(r)),
       cols = .corresp_dims(sweep(sv$v, 2, sv$d * flip, "*") / sqrt(cm)),
       d = sv$d)
}

# A table with only two columns yields a single dimension, which is still
# drawn, along a second axis of zeroes.
.corresp_dims <- function(X) {
  if (ncol(X) >= 2L) return(X)
  cbind(X, matrix(0, nrow = nrow(X), ncol = 2L - ncol(X)))
}

# Returns the first two coordinates of each node, and how much of each node's
# distance from the origin those two hold. Nodes set aside for having no ties
# are returned at the origin, with no fit to report.
.corresp_place <- function(X, keep) {
  xy <- matrix(0, nrow = length(keep), ncol = 2L)
  xy[keep, ] <- X[, 1:2, drop = FALSE]
  # The squared cosine of the angle between where a node sits in full and
  # where it is drawn: 1 where the plane holds it exactly, 0 where the node is
  # somewhere the plane cannot show.
  full <- rowSums(X^2)
  q <- rep(NA_real_, length(keep))
  q[keep] <- ifelse(full > 0, rowSums(X[, 1:2, drop = FALSE]^2) / full,
                    NA_real_)
  list(xy = xy, cos2 = q)
}
