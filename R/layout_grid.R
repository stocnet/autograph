#' Layouts for snapping layouts to a grid
#'
#' @description The function uses approximate pattern matching
#'   to redistribute coarse layouts on square grid points, while
#'   preserving the topological relationships among the nodes (see Inoue et al. 2012). 
#' @references
#' Inoue, Kentaro, Shinichi Shimozono, Hideaki Yoshida, and Hiroyuki Kurata. 2012. 
#' “Application of Approximate Pattern Matching in Two Dimensional Spaces to Grid Layout for Biochemical Network Maps” edited by J. Bourdon. 
#' _PLoS ONE_ 7(6):e37739.
#' \doi{https://doi.org/10.1371/journal.pone.0037739}.
#' @keywords internal
depth_first_recursive_search <- function(layout) {
  if("ggraph" %in% class(layout)) layout <- layout$data[,c("x","y")]
  layout <- as.data.frame(layout)
  dims <- ceiling(2 * sqrt(nrow(layout)))
  # evens <- 0:dims[0:dims %% 2 == 0]
  vacant_points <- expand.grid(seq.int(0, dims, 1), seq.int(0, dims, 1)) # create options
  vacant_points <- vacant_points - floor(dims / 2) # centre options
  names(vacant_points) <- c("x", "y")
  gridout <- layout[order(abs(layout[,1]) + abs(layout[,2])), ] # sort centroid distance
  nodes <- seq_len(nrow(gridout))
  for (i in nodes) {
    dists <- as.matrix(stats::dist(rbind(gridout[i, 1:2], vacant_points),
                            method = "manhattan"))[, 1]
    mindist <- which(dists == min(dists[2:length(dists)]))[1] - 1
    vacpoint <- vacant_points[mindist, ]
    changes <- vacpoint - gridout[i, 1:2]
    gridout[nodes >= i, 1] <- gridout[nodes >= i, 1] + 
      changes[[1]]
    gridout[nodes >= i, 2] <- gridout[nodes >= i, 2] + 
      changes[[2]]
    vacant_points <- vacant_points[-mindist, ]
  }
  gridout[order(row.names(gridout)),] # reorder from centroid
  # gridout
  # plot(gridout[order(row.names(gridout)),])
}

# localmin <- function(layout, graph) {
#   repeat {
#     f0 <- sum(cost_function(layout, graph))
#     L <- get_vacant_points(layout)
#     for (a in seq_len(nrow(layout))) {
#       out <- t(apply(L, 1, function(y) {
#         layout_new <- layout
#         layout_new[a, 1:2] <- y
#         c(a, y, sum(cost_function(layout_new, graph)))
#       }))
#     }
#     if (out[which.min(out[, 4]), 4] < f0) {
#       layout[out[which.min(out[, 4]), 1], 1:2] <- out[which.min(out[, 4]), 2:3]
#     } else{
#       break
#     }
#   }
#   layout
# }
# 
# get_vacant_points <- function(layout) {
#   all_points <- expand.grid(min(layout$x):max(layout$x),
#                             min(layout$y):max(layout$y))
#   names(all_points) <- c("x", "y")
#   vacant_points <- rbind(all_points,
#                          layout[, c("x", "y")])
#   vacant_points <- subset(vacant_points,
#                           !(duplicated(vacant_points) |
#                               duplicated(vacant_points, fromLast = TRUE)))
#   vacant_points
# }
# 
# cost_function <- function(layout, graph, max_repulse_distance = max(layout[, 1]) * .75) {
#   d <- as.matrix(dist(layout[, 1:2], method = "manhattan"))
#   a <- as_matrix(graph)
#   i <- diag(nrow(a))
#   m <- a + i
#   w <- ifelse(m > 0, 3,
#               ifelse(m == 0 & m %*% t(m) > 0, 0, -2)) # only three levels here
#   # see Li and Kurata (2005: 2037) for more granulated option
#   ifelse(w >= 0, w * d, w * min(d, max_repulse_distance))
# }
# 
# plot_gl <- function(x, tmax, tmin, rmin, fmin, ne, rc, p) {
#   l <- index <- a <- NULL # initialize variables to avoid CMD check notes
#   x <- as_tidygraph(x)
#   lo <- ggraph::create_layout(x, layout = "igraph", algorithm = "randomly")
#   lo[, 1] <- round(lo[, 1] * 1000)
#   lo[, 2] <- round(lo[, 2] * 1000)
#   dists <- as.matrix(dist(lo[, 1:2], method = "manhattan"))
#   colMax <- function(data) apply(data, MARGIN = 1, FUN = max, na.rm = TRUE)
#   diag(dists) <- NA
#   rsep <- l * sum(ifelse(colMax(a / dists - 1) > 0, colMax(a / dists - 1), 0))
#   ggraph::ggraph(x, graph = lo) +
#     ggraph::geom_edge_link(ggplot2::aes(alpha = ggplot2::stat(index)), 
#                            show.legend = FALSE) +
#     ggraph::geom_node_point()
# }

.rescale <- function(vector){
  (vector - min(vector)) / (max(vector) - min(vector))
}
