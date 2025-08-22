#' Layout algorithms based on configurational positions
#' 
#' @description
#'   Configurational layouts locate nodes at symmetric coordinates
#'   to help illustrate particular configurations.
#'   Currently "triad" and "quad" layouts are available.
#'   The "configuration" layout will choose the appropriate configurational
#'   layout automatically.
#' 
#' @name layout_configuration
#' @family mapping
#' @inheritParams layout_partition
NULL

#' @rdname layout_configuration
#' @export
layout_configuration <- function(.data,
                                           circular = FALSE, times = 1000){
  if (manynet::net_nodes(.data) == 2) {
    layout_tbl_graph_dyad(.data, circular = circular, times = times)
  } else if (manynet::net_nodes(.data) == 3) {
    layout_tbl_graph_triad(.data, circular = circular, times = times)
  } else if (manynet::net_nodes(.data) == 4) {
    layout_tbl_graph_tetrad(.data, circular = circular, times = times)
  } else if (manynet::net_nodes(.data) == 5) {
    layout_tbl_graph_pentad(.data, circular = circular, times = times)
  } else if (manynet::net_nodes(.data) == 6) {
    layout_tbl_graph_hexad(.data, circular = circular, times = times)
  }
}

#' @rdname layout_configuration
#' @export
layout_tbl_graph_configuration <- layout_configuration

#' @rdname layout_configuration
#' @export
layout_dyad <- function(.data,
                                  circular = FALSE, times = 1000){
  res <- matrix(c(0,0,
                  1,0), 2, 2, byrow = TRUE)
  .to_lo(res)  
}

#' @rdname layout_configuration
#' @export
layout_tbl_graph_dyad <- layout_dyad

#' @rdname layout_configuration
#' @export
layout_triad <- function(.data,
                                   circular = FALSE, times = 1000){
  res <- matrix(c(0,0,
                  2,3.5,
                  4,0), 3, 2, byrow = TRUE)
  .to_lo(res)  
}

#' @rdname layout_configuration
#' @export
layout_tbl_graph_triad <- layout_triad

#' @rdname layout_configuration
#' @export
layout_tetrad <- function(.data,
                                    circular = FALSE, times = 1000){
  res <- matrix(c(0,0,
                  0,1,
                  1,0,
                  1,1), 4, 2, byrow = TRUE)
  .to_lo(res)  
}

#' @rdname layout_configuration
#' @export
layout_tbl_graph_tetrad <- layout_tetrad

#' @rdname layout_configuration
#' @export
layout_pentad <- function(.data,
                                    circular = FALSE, times = 1000){
  res <- matrix(c(0,1,
                  -0.9511,0.3090,
                  -0.5878,-0.8090,
                  0.5878,-0.8090,
                  0.9511,0.3090), 5, 2, byrow = TRUE)
  .to_lo(res)  
}

#' @rdname layout_configuration
#' @export
layout_tbl_graph_pentad <- layout_pentad

#' @rdname layout_configuration
#' @export
layout_hexad <- function(.data,
                                   circular = FALSE, times = 1000){
  res <- matrix(c(1,0,
                  1/2,sqrt(3)/2,
                  -1/2,sqrt(3)/2,
                  -1,0,
                  -1/2,-sqrt(3)/2,
                  1/2,-sqrt(3)/2), 6, 2, byrow = TRUE)
  .to_lo(res)  
}

#' @rdname layout_configuration
#' @export
layout_tbl_graph_hexad <- layout_hexad
