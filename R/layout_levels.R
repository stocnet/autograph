#' Levels layout
#'
#' @description
#'   The "levels" layout draws each level of a multilevel network
#'   as a plane of its own, projected at an angle,
#'   with the ties within each level drawn on its plane
#'   and the ties between levels drawn between them.
#'
#'   Note that `{graphlayouts}` offers a layout of the same idea under the
#'   name "multilevel". This one is named for its `level` argument.
#' @name layout_levels
#' @template param_ggraphlayouts
#' @param level A node attribute or a vector to hierarchically order levels.
#'   By default the levels are those already recorded in a "lvl" node attribute,
#'   as `manynet::to_multilevel()` writes, or, for a two-mode network,
#'   the two modes, with whichever mode holds the ties within itself
#'   placed at the first level.
#' @param method How the levels should be laid out:
#'   "all" (the default) lays every level out at once,
#'   "separate" lays each level out independently,
#'   and "fix1" and "fix2" lay out the first or second level respectively
#'   and derive the other from it.
#'   Note that all but "all" require ties within the levels they lay out.
#' @param FUN1,FUN2 The layout functions used for the first and second levels
#'   respectively by the "separate", "fix1" and "fix2" methods.
#'   By default both are `graphlayouts::layout_with_stress()`.
#' @param alpha,beta The angles, in degrees, at which the levels
#'   are projected onto the plane.
#' @family mapping
#' @examples
#' # fict_marvel interlocks a one-mode layer of ties among its characters
#' # with a two-mode layer of their affiliations, so it is laid out this way
#' # by default; the levels need not be named.
#' graphr(manynet::fict_marvel, labels = FALSE)
#' @export
layout_levels <- function(.data, level,
                              method = c("all", "separate", "fix1", "fix2"),
                              circular = FALSE, alpha = 25, beta = 45,
                              FUN1 = graphlayouts::layout_with_stress,
                              FUN2 = graphlayouts::layout_with_stress) {
  method <- .check_choice(method, c("all", "separate", "fix1", "fix2"), "method")
  # Coerced up front, as the other layouts do, so that a network given in
  # another form -- such as the list-based class manynet 2.3.0 introduced --
  # reaches the igraph functions below as a graph, and `length()` counts its
  # nodes rather than the parts the object is built from.
  .data <- manynet::as_igraph(.data)
  if (missing(level)) {
    level <- .infer_level(.data)
  } else {
    if (length(level) > 1 & length(level) != length(.data)) {
      .abort_layout_arg("level", "levels", length(.data))
    } else if (length(level) != length(.data)) {
      level <- .match_name(level, igraph::vertex_attr_names(.data),
                           "level", what = "node attribute")
      level <- manynet::node_attribute(.data, level)
    }
  }
  level <- .as_level(level)
  # `layout_as_multilevel()` lays each of its "separate", "fix1" and "fix2"
  # variants out level by level, dropping isolates from each level's subgraph.
  # A level whose nodes are tied only to the other level therefore leaves an
  # empty subgraph, which it reports as an obscure indexing error.
  if (method != "all") .check_level_ties(.data, level, method)
  .check_level_reach(.data)
  out <- .drop_unusable_weights(.data)
  out <- igraph::set_vertex_attr(out, "lvl", value = level)
  out <- graphlayouts::layout_as_multilevel(out, type = method,
                                            FUN1 = FUN1, FUN2 = FUN2,
                                            alpha = alpha, beta = beta)
  .to_lo(out)
}

# `graphlayouts::layout_as_multilevel()` reads levels from a 'lvl' node
# attribute holding consecutive integers from 1. Anything else -- a factor, a
# character vector, the logical 'type' of a two-mode network -- has to be coded
# into one. A numeric attribute keeps its own ordering rather than being
# re-coded, so that levels given as e.g. c(3, 1, 2) are not silently reordered.
.as_level <- function(level) {
  if (is.numeric(level)) as.integer(level) else as.integer(as.factor(level))
}

# Levels for `layout_levels()` when none were given. A network already
# converted by `manynet::to_multilevel()` carries them in 'lvl'; a two-mode
# network has them implied by its modes.
.infer_level <- function(.data) {
  if ("lvl" %in% igraph::vertex_attr_names(.data)) {
    manynet::snet_info("Using the levels found in the {.val lvl} node attribute.")
    return(igraph::vertex_attr(.data, "lvl"))
  }
  if (!manynet::is_twomode(.data))
    .abort_layout_arg("level", "levels", length(.data))
  mode <- manynet::node_is_mode(.data)
  within <- !manynet::tie_is_twomode(.data)
  # The mode holding the within-mode ties is placed at the first level, so that
  # whichever level has a structure of its own is the one laid out in the plane
  # rather than fanned out from the other. Which mode that is varies by network,
  # so it is read off the ties rather than assumed to be the first.
  base <- FALSE
  if (any(within)) {
    el <- igraph::as_edgelist(.data, names = FALSE)
    base <- as.logical(names(sort(table(mode[el[within, 1]]),
                                  decreasing = TRUE))[1])
  }
  ifelse(mode == base, 1L, 2L)
}

# `layout_as_multilevel()` orients its levels by the shortest paths between
# them, and those are infinite between components, which leaves it minimising
# a stress that is never less than any other. It fails part way through, with
# R's own "missing value where TRUE/FALSE needed".
.check_level_reach <- function(.data) {
  if (manynet::is_connected(.data)) return(invisible(NULL))
  manynet::snet_abort(
    "The {.val levels} layout places the levels by the distances between",
    "them, so it can only be used on a connected network,",
    "but this network has {igraph::count_components(.data)} components.",
    "Please use {.code manynet::to_giant()} to keep only the largest,",
    "or choose another layout.")
}

# Those same shortest paths come from `igraph::distances()`, which reads any
# 'weight' tie attribute and rejects one holding negative values outright
# ("Negative cycle detected while calculating shortest paths"). Weights that
# cannot be read as distances are dropped, so that the levels are placed by
# structure alone rather than the layout failing.
.drop_unusable_weights <- function(.data) {
  if (!"weight" %in% igraph::edge_attr_names(.data)) return(.data)
  weights <- igraph::edge_attr(.data, "weight")
  if (all(weights > 0, na.rm = TRUE)) return(.data)
  manynet::snet_info(
    "Ignoring the tie weights, because the {.val levels} layout places",
    "the levels by the distances between them and some weights are not",
    "positive.")
  igraph::delete_edge_attr(.data, "weight")
}

.check_level_ties <- function(.data, level, method) {
  # "separate" lays out both levels independently and so needs ties within
  # each; "fix1" derives level 2 from level 1 and so needs only level 1's.
  needed <- switch(method, separate = c(1L, 2L), fix1 = 1L, fix2 = 2L)
  el <- igraph::as_edgelist(.data, names = FALSE)
  within <- level[el[, 1]] == level[el[, 2]]
  empty <- needed[!vapply(needed, function(l)
    any(within & level[el[, 1]] == l), logical(1))]
  if (!length(empty)) return(invisible(NULL))
  # Written out rather than pluralised, since the quantity that matters is how
  # many levels are empty while the value reported is which they are.
  which_levels <- paste(empty, collapse = " and ")
  manynet::snet_abort(
    "The {.val {method}} method for the {.val levels} layout lays out",
    "each level on its own, but there are no ties within level",
    "{which_levels} of this network to lay out.",
    "Please use {.code method = \"all\"} to lay every level out together.")
}


#' @rdname layout_levels
#' @export
layout_tbl_graph_levels <- layout_levels
