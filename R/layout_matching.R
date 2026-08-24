#' Matching layout
#' @name layout_matching
#' @description
#'   This layout works to position nodes opposite their matching nodes.
#'   See `manynet::to_matching()` for more details on the matching procedure.
#' @template param_ggraphlayouts
#' @param center Required for `{ggraph}` compatibility, and not used here.
#' @family mapping
#' @export
layout_matching <- function(.data, center = NULL,
                            circular = FALSE, times = 1) {
  hlay <- layout_tbl_graph_layered(.data)
  matchd <- manynet::as_edgelist(manynet::to_unnamed(manynet::to_matching(.data)))
  hlay[matchd$to,"x"] <- hlay[matchd$from,"x"]
  hlay
}

#' @rdname layout_matching
#' @export
layout_tbl_graph_matching <- layout_matching
