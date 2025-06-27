#' Matching layout
#' @description
#'   This layout works to position nodes opposite their matching nodes.
#'   See `manynet::to_matching()` for more details on the matching procedure.
#' @param .data Some `{manynet}` compatible network data.
#' @param center,circular,times Extra parameters required for `{tidygraph}`
#'   compatibility.
#' @returns Returns a table of nodes' x and y coordinates.
#' @export
layout_tbl_graph_matching <- function(.data,
                                      center = NULL,
                                      circular = FALSE,
                                      times = 1) {
  hlay <- manynet::layout_tbl_graph_hierarchy(.data)
  matchd <- manynet::as_edgelist(manynet::to_unnamed(manynet::to_matching(.data)))
  hlay[matchd$to,"x"] <- hlay[matchd$from,"x"]
  hlay
}