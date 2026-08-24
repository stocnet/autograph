#' Scaling layout
#'
#' @description
#'   The "scaling" layout places nodes by multidimensional scaling,
#'   so that the distance drawn between two nodes approximates
#'   the number of steps of the shortest path between them.
#'   Unlike a force-directed layout, then, the coordinates can be read,
#'   and so this layout draws labelled axes,
#'   at a fixed ratio so that the two axes share one scale.
#'
#'   Which algorithm is used depends on the size of the network.
#'   Up to a hundred nodes, classical multidimensional scaling is used,
#'   as `igraph::layout_with_mds()` offers it.
#'   Above that, or where `pivots` is given,
#'   pivot multidimensional scaling is used instead,
#'   as `graphlayouts::layout_with_pmds()` offers it,
#'   which approximates the same solution from a sample of the nodes
#'   and is much the faster for a large network.
#'   Note that "mds" and "pmds" remain available as layouts in their own right,
#'   though "pmds" then requires its own `pivots`.
#'
#'   Two dimensions rarely hold every path distance of a network at once,
#'   so `graphr()` captions the plot with how well this one does:
#'   see `check_stress()` for how to read the score.
#' @name layout_scaling
#' @template param_ggraphlayouts
#' @param pivots The number of nodes to approximate the scaling from.
#'   By default this is `NULL`, which uses every node where the network has
#'   no more than a hundred, and samples the nodes otherwise.
#'   Giving a number selects the pivot algorithm whatever the size of network.
#' @details
#'   The distances scaled are those of the unweighted network,
#'   that is, the number of ties on the shortest path between two nodes.
#'   Tie weights are ignored, since the interpretation of a drawn distance
#'   is then the same whatever the network,
#'   and since a signed network has no shortest paths to speak of.
#'
#'   Where a network is disconnected, there is no path between its components,
#'   and so no distance to scale. Each component is laid out and the components
#'   are then placed beside one another, and the fit is reported over
#'   the pairs of nodes that a path does connect.
#' @family mapping
#' @source
#'   Kruskal, Joseph B. 1964.
#'   "Multidimensional scaling by optimizing goodness of fit to a nonmetric
#'   hypothesis", _Psychometrika_ 29(1): 1-27.
#'   \doi{10.1007/BF02289565}
#'
#'   Brandes, Ulrik, and Christian Pich. 2007.
#'   "Eigensolver methods for progressive multidimensional scaling of large
#'   data", in _Graph Drawing_, 42-53.
#'   \doi{10.1007/978-3-540-70904-6_6}
#' @examples
#' graphr(manynet::ison_southern_women, layout = "scaling")
#' @export
layout_scaling <- function(.data, pivots = NULL,
                           circular = FALSE, times = 1) {
  .data <- manynet::as_igraph(.data)
  n <- igraph::vcount(.data)
  if (n < 3L) return(.to_lo(.trivial_coords(n)))
  if (!is.null(pivots)) {
    if (!is.numeric(pivots) || length(pivots) != 1L || pivots < 2) {
      manynet::snet_abort(
        "{.arg pivots} should be a single number of at least 2,",
        "or {.val NULL} to let the number be chosen.")
    }
    pivots <- min(as.integer(pivots), n - 1L)
  }
  # `weights = NA` counts ties rather than summing their weights: a signed
  # network otherwise aborts on a negative cycle, and a weighted one would be
  # scaled in units the caption could not name.
  if (is.null(pivots) && n <= 100L) {
    # The distances are computed here rather than left to igraph, so that the
    # layout and the fit reported for it scale the same dissimilarities.
    dis <- igraph::distances(.data, weights = NA)
    crd <- igraph::layout_with_mds(.data, dist = dis)
    src <- seq_len(n)
  } else {
    if (is.null(pivots)) pivots <- min(n - 1L, max(50L, ceiling(sqrt(n))))
    crd <- .pivot_scaling(.data, pivots)
    # A network large enough for the pivot algorithm is large enough that a
    # full distance matrix is the expensive part, so the fit is measured from
    # a sample of the nodes. See `.stress_sources()`.
    src <- .stress_sources(n)
    dis <- igraph::distances(.data, v = src, weights = NA)
  }
  res <- .to_lo(crd)
  # Carried on the coordinates rather than recomputed later: the attribute
  # survives ggraph::create_layout(), so graphr() can read the fit of the
  # layout it actually drew.
  attr(res, "fit") <- .scaling_fit(dis, crd, src, pivots)
  res
}

#' @rdname layout_scaling
#' @export
layout_tbl_graph_scaling <- layout_scaling

# `graphlayouts::layout_with_pmds()` aborts where the network is disconnected,
# so each component is laid out on its own and the components are then packed
# together. A component with fewer nodes than pivots, or too few nodes to
# sample at all, is scaled in full instead, which for a component that small
# costs nothing.
.pivot_scaling <- function(g, pivots) {
  igraph::layout_components(g, layout = function(part) {
    m <- igraph::vcount(part)
    # A component of one or two nodes has no distances worth scaling, and
    # igraph refuses to scale fewer nodes than dimensions.
    if (m < 3L) return(.trivial_coords(m))
    if (m < 4L || m - 1L <= pivots) {
      igraph::layout_with_mds(part,
                              dist = igraph::distances(part, weights = NA))
    } else graphlayouts::layout_with_pmds(part, pivots = pivots, weights = NA)
  })
}

# Coordinates for a network too small to scale: a node, or two side by side.
.trivial_coords <- function(n) {
  cbind(seq_len(n) - 1, rep(0, n))
}

# How well the two dimensions drawn hold the path distances, carried on the
# layout so that graphr() can caption the plot with it rather than compute the
# scaling a second time. `pivots` is NA where every node was scaled.
.scaling_fit <- function(dis, crd, src, pivots) {
  list(stress = .stress1(dis, crd, src),
       # The decomposition the share of variance is read from is the very work
       # the pivot algorithm is used to avoid, so it is only reported where
       # every node was scaled in full.
       variance = if (is.null(pivots)) .scaling_variance(dis) else NA_real_,
       pivots = if (is.null(pivots)) NA_integer_ else as.integer(pivots))
}

# The share of the distance variance the first two dimensions hold, from the
# eigenvalues classical scaling decomposes the distances into. This is only
# defined where every pair of nodes has a distance, so a disconnected network
# has no such share and reports none.
.scaling_variance <- function(dis) {
  if (any(!is.finite(dis))) return(NA_real_)
  eig <- tryCatch(stats::cmdscale(stats::as.dist(dis), k = 2, eig = TRUE)$eig,
                  error = function(e) NULL)
  if (is.null(eig) || sum(abs(eig)) == 0) return(NA_real_)
  sum(eig[1:2]) / sum(abs(eig))
}
