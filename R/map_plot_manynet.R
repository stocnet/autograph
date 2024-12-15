# Measures ####

#' Plotting numeric measures
#' @description
#'   These functions plot distributions for node, tie, and network measures,
#'   as defined in the `{manynet}` package.
#' @name map_measure
#' @param x An object of "node_measure", "tie_measure", 
#'   or "network_measures" class.
#' @param ... Other arguments to be passed on.
#' @param type For node and tie measures, whether the plot should be 
#'   "h" a histogram or "d" a density plot. By default "h".
#' @export
plot.node_measure <- function(x, type = c("h", "d"), ...) {
  #type <- match.arg(type)
  density <- NULL
  if (is.null(attr(x, "mode"))) attr(x, "mode") <- rep(FALSE, length(x))
  data <- data.frame(Score = x, Mode = attr(x, "mode"))
  if (length(type) == 2) {
    p <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data$Score)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                              binwidth = ifelse(max(data$Score) > 1, 1,
                                                ifelse(max(data$Score) > 
                                                         .1, .1, .01))) +
      ggplot2::geom_density(col = 2) +
      ggplot2::scale_y_continuous("Frequency", sec.axis = 
                                    ggplot2::sec_axis(~ ., breaks = c(0,1),
                                                      name = "Density"))
  } else if (length(type) == 1 & type == "h") {
    p <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data$Score)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                              binwidth = ifelse(max(data$Score) > 1, 1,
                                                ifelse(max(data$Score) >
                                                         .1, .1, .01))) +
      ggplot2::labs(x = "Density", y = "Frequency")
  } else if (length(type) == 1 & type == "d") {
    p <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data$Score)) +
      ggplot2::geom_density(col = 2) +
      ggplot2::ylab("Density")
  }
  p +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey90"))
}

#' @rdname map_measure
#' @export
plot.tie_measure <- function(x, type = c("h", "d"), ...) {
  type <- match.arg(type)
  data <- data.frame(Score = x)
  if (type == "h") {
    p <- ggplot2::ggplot(data = data) +
      ggplot2::geom_histogram(ggplot2::aes(x = .data$Score),
                              binwidth = ifelse(max(data$Score) > 1, 1,
                                                ifelse(max(data$Score) > .1,
                                                       .1,
                                                       .01))) +
      ggplot2::ylab("Frequency")
  } else {
    p <- ggplot2::ggplot(data = data) +
      ggplot2::geom_density(ggplot2::aes(x = .data$Score)) +
      ggplot2::ylab("Density")
  }
  p + ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey90"))
}

#' @rdname map_measure
#' @export
plot.network_measures <- function(x, ...) {
  ggplot2::ggplot(data = x, ggplot2::aes(x = .data$time, y = .data$value)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Value")
}

