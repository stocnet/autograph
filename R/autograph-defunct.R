# Compatibility with older {manynet} ------------------------------------------

# autograph declares the oldest {manynet} it works with, which is the version
# on CRAN. Two functions it uses arrived in manynet 2.3.0, so each is reached
# through a shim that calls manynet where the function is there and does the
# same thing itself where it is not. Each shim tests for the function rather
# than for the version string, because a pre-release dev build can carry the
# version without yet exporting the function. Delete a shim once its minimum
# is raised past 2.3.0. Both branches reach the function through
# `getExportedValue()` rather than `::`, since `R CMD check` resolves a `::`
# call against the installed manynet and reports the newer name as missing
# even where the call is never reached.

# `manynet::delete_isolates()` is manynet 2.3.0's name for `to_no_isolates()`,
# which it otherwise leaves unchanged.
.ag_delete_isolates <- function(.data) {
  fn <- if (.ag_has_manynet("delete_isolates")) "delete_isolates" else
    "to_no_isolates"
  getExportedValue("manynet", fn)(.data)
}

# `manynet::is_multilevel()` marks TRUE a network whose nodes fall into two or
# more levels that are tied both within and between. The fallback repeats
# manynet's own igraph method: levels are read from the 'lvl' attribute that
# `to_multilevel()` writes, and otherwise from the modes of a two-mode
# network. A 'stocnet' is coerced to an 'igraph' first, as manynet's default
# method does, so a network of three or more levels is read as two.
.ag_is_multilevel <- function(.data) {
  if (.ag_has_manynet("is_multilevel"))
    return(getExportedValue("manynet", "is_multilevel")(.data))
  .data <- manynet::as_igraph(.data)
  if ("lvl" %in% igraph::vertex_attr_names(.data))
    return(length(unique(igraph::vertex_attr(.data, "lvl"))) > 1)
  if (!manynet::is_twomode(.data)) return(FALSE)
  # A tie-less network is neither, and is returned before `tie_is_twomode()`,
  # which cannot name an empty measure.
  if (manynet::net_ties(.data) == 0) return(FALSE)
  between <- manynet::tie_is_twomode(.data)
  any(between) && any(!between)
}

.ag_has_manynet <- function(fn) {
  fn %in% getNamespaceExports("manynet")
}

# Compatibility with older {goldfish} -----------------------------------------

# The goldfish overview draws each panel from a goldfish diagnostic, several of
# which arrived after the version on CRAN. Reaching them through
# `getExportedValue()` rather than `::` keeps `R CMD check` from resolving the
# call against whichever goldfish is installed and reporting the newer names as
# missing. Where the function really is missing the error this raises is caught
# by `gf_overview_try()`, which leaves that panel out, exactly as it does for a
# fit that stores no such primitive.
.ag_goldfish <- function(fn) {
  getExportedValue("goldfish", fn)
}

# The goldfish classes were renamed to a package prefix plus a noun, in
# camelCase (see the class naming rule in .github/CONTRIBUTING.md). Five old
# names keep an alias that forwards to the renamed method, so an object classed
# the way an earlier autograph expected still plots. `diagnose_outliers` and
# `diagnose_changepoints` are the names goldfish 1.9.21 stamps;
# `outliers.goldfish` and `changepoints.goldfish` are the two the draft methods
# were written against before that; `result.goldfish` is the fit class every
# goldfish stamps, back to the version on CRAN, so it is the alias that reaches
# the most users. Nothing is aliased for the classes only the renamed goldfish
# emits, since nothing ever stamped those.
#
# An alias restores dispatch, not the old column contract: each forwards to a
# method that reads the current columns (`.series`, and a logical `outlier` or
# `cpt`), so an object carrying the pre-1.9.21 shape still fails on its columns.
# Delete each alias once the oldest goldfish autograph works with is past the
# rename.

#' @rdname plot_adequacy
#' @details
#'   `plot.diagnose_outliers()`, `plot.outliers.goldfish()`,
#'   `plot.diagnose_changepoints()` and `plot.changepoints.goldfish()` are
#'   aliases for `plot.goldfishOutliers()` and `plot.goldfishChangepoints()`,
#'   kept so that an object carrying one of the older class names plots as
#'   before. Each reads the columns the current methods read. They will be
#'   removed.
#' @method plot diagnose_outliers
#' @export
plot.diagnose_outliers <- function(x, ...) {
  plot.goldfishOutliers(x, ...)
}

#' @rdname plot_adequacy
#' @method plot outliers.goldfish
#' @export
plot.outliers.goldfish <- function(x, ...) {
  plot.goldfishOutliers(x, ...)
}

#' @rdname plot_adequacy
#' @method plot diagnose_changepoints
#' @export
plot.diagnose_changepoints <- function(x, ...) {
  plot.goldfishChangepoints(x, ...)
}

#' @rdname plot_adequacy
#' @method plot changepoints.goldfish
#' @export
plot.changepoints.goldfish <- function(x, ...) {
  plot.goldfishChangepoints(x, ...)
}

#' @rdname plot_goldfish_fit
#' @details
#'   `plot.result.goldfish()` is an alias for `plot.goldfishFit()`, kept so that
#'   a fit from a goldfish that still stamps the old class name plots as before.
#'   It will be removed.
#' @method plot result.goldfish
#' @export
plot.result.goldfish <- function(x, ..., effects = 4) {
  plot.goldfishFit(x, ..., effects = effects)
}


# Layouts -----------------------------------------------------------------

#' Deprecated layout names
#'
#' @description
#'   Each of these draws what its replacement draws, after saying so.
#'   They are kept so that a call naming the older layout still draws,
#'   and will be removed.
#'
#'   - "hierarchy" is now "layered", which is what the layout does to a
#'     two-mode network, where the two modes are two layers and neither is
#'     above the other in any hierarchy.
#'   - "alluvial" is now "lineage". The name is held for a plot of changing
#'     membership composition over time.
#'   - "multilevel" is now "levels", which `{graphlayouts}` does not also use.
#'   - "dyad", "triad", "tetrad", "pentad" and "hexad" are now all
#'     "configuration", which already picks the one matching the number of
#'     nodes. The functions of those names are not deprecated.
#'
#'   Note that `.deprecated_layouts()` lists these, so that neither the
#'   completions nor the functional audit offers a retired name.
#' @name layout_deprecated
#' @param .data Some `{manynet}` compatible network data.
#' @param ... Arguments passed on to the replacement layout.
#' @returns Returns a table of nodes' x and y coordinates.
#' @keywords internal
NULL


#' @rdname layout_deprecated
#' @export
layout_multilevel <- function(.data, ...) {
  manynet::snet_warn(
    "The {.val multilevel} layout is deprecated.",
    "Please use {.code layout = \"levels\"} instead, which takes the same {.arg level} argument.")
  layout_levels(.data, ...)
}

#' @rdname layout_deprecated
#' @export
layout_tbl_graph_multilevel <- layout_multilevel

#' @rdname layout_deprecated
#' @export
layout_tbl_graph_dyad <- function(.data, ...) {
  manynet::snet_warn(
    "The {.val dyad} layout is deprecated.",
    "Please use {.code layout = \"configuration\"} instead,",
    "which draws whichever configuration the network has nodes for.",
    "The {.fn layout_dyad} function itself is not deprecated.")
  layout_configuration(.data, ...)
}

#' @rdname layout_deprecated
#' @export
layout_tbl_graph_triad <- function(.data, ...) {
  manynet::snet_warn(
    "The {.val triad} layout is deprecated.",
    "Please use {.code layout = \"configuration\"} instead,",
    "which draws whichever configuration the network has nodes for.",
    "The {.fn layout_triad} function itself is not deprecated.")
  layout_configuration(.data, ...)
}

#' @rdname layout_deprecated
#' @export
layout_tbl_graph_tetrad <- function(.data, ...) {
  manynet::snet_warn(
    "The {.val tetrad} layout is deprecated.",
    "Please use {.code layout = \"configuration\"} instead,",
    "which draws whichever configuration the network has nodes for.",
    "The {.fn layout_tetrad} function itself is not deprecated.")
  layout_configuration(.data, ...)
}

#' @rdname layout_deprecated
#' @export
layout_tbl_graph_pentad <- function(.data, ...) {
  manynet::snet_warn(
    "The {.val pentad} layout is deprecated.",
    "Please use {.code layout = \"configuration\"} instead,",
    "which draws whichever configuration the network has nodes for.",
    "The {.fn layout_pentad} function itself is not deprecated.")
  layout_configuration(.data, ...)
}

#' @rdname layout_deprecated
#' @export
layout_tbl_graph_hexad <- function(.data, ...) {
  manynet::snet_warn(
    "The {.val hexad} layout is deprecated.",
    "Please use {.code layout = \"configuration\"} instead,",
    "which draws whichever configuration the network has nodes for.",
    "The {.fn layout_hexad} function itself is not deprecated.")
  layout_configuration(.data, ...)
}
