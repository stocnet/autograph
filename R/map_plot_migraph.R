# CUG/QAP ####

#' Plotting methods for CUG and QAP tests
#' @description
#'   These plotting methods are for results obtained by testing some statistic
#'   against those produced in a reference distribution of conditional uniform
#'   graphs or as a quadratic assignment procedure.
#'   The S3 class is "network_test".
#' @param x An object obtained from a conditional uniform graph or
#'   quadratic assignment procedure test.
#'   For example, `migraph::test_permutation()`.
#' @inheritParams map_measure
#' @param threshold The empirical threshold to shade in the plot.
#' @param tails By default "two" indicating a two-tailed test,
#'   but "one" for a one-tailed test is also available.
#' @export
plot.network_test <- function(x, ...,
                              threshold = .95, 
                              tails = c("two", "one")){
  data <- data.frame(Statistic = x$testdist)
  p <- ggplot2::ggplot(data, 
                       ggplot2::aes(x = .data$Statistic)) + 
    ggplot2::geom_density()
  if(all(data$Statistic >= -1 & data$Statistic <= 1)){
    p <- p + ggplot2::expand_limits(x=0) + 
      ggplot2::geom_vline(ggplot2::aes(xintercept = 0),
                          linetype="dashed")
    if(any(data$Statistic < 0)) p <- p + ggplot2::expand_limits(x=-1)
    if(any(data$Statistic > 0)) p <- p + ggplot2::expand_limits(x=1)
  }
  d <- ggplot2::ggplot_build(p)$data[[1]]
  tails = match.arg(tails)
  if(tails == "one"){
    if(x$testval < stats::quantile(data$Statistic, .5)){
      thresh <- stats::quantile(data$Statistic, 1 - threshold)
      p <- p + ggplot2::geom_area(data = subset(d, x < thresh), 
                                  aes(x = x, y = .data$y), fill = "lightgrey")
    } else {
      thresh <- stats::quantile(data$Statistic, threshold)
      p <- p + ggplot2::geom_area(data = subset(d, x > thresh), 
                                  aes(x = x, y = .data$y), fill = "lightgrey")
    }
  } else if (tails == "two"){
    thresh <- stats::quantile(data$Statistic, 
                       c((1-threshold)/2, ((1-threshold)/2)+threshold))
    p <- p + ggplot2::geom_area(data = subset(d, x < thresh[1]), 
                                aes(x = x, y = .data$y), fill = "lightgrey") + 
      ggplot2::geom_area(data = subset(d, x > thresh[2]), 
                         aes(x = x, y = .data$y), fill = "lightgrey")
  }
  p + ggplot2::theme_classic() + ggplot2::geom_density() +
    ggplot2::geom_vline(ggplot2::aes(xintercept = x$testval),
                        color="red", linewidth=1.2) + ggplot2::ylab("Density")
}

