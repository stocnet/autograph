#' @param .data Some `{manynet}` compatible network data.
#' @param circular Should the layout be transformed into a radial
#'   representation. Only possible for some layouts. Defaults to FALSE.
#'   Required for `{ggraph}` compatibility.
#' @param times Maximum number of iterations, where appropriate.
#'   Required for `{ggraph}` compatibility, and ignored by the layouts that
#'   do not iterate.
#' @returns Returns a table of nodes' x and y coordinates.
