#' @export
layout_tbl_graph_matching <- function(.data,
                                      center = NULL,
                                      circular = FALSE,
                                      times = 1000) {
  hlay <- manynet::layout_tbl_graph_hierarchy(.data)
  matchd <- as_edgelist(to_unnamed(to_matching(.data)))
  hlay[matchd$to,"x"] <- hlay[matchd$from,"x"]
  hlay
}