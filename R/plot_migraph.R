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
#' @returns A distribution of the simulated or permuted statistics,
#'   with 2.5% shaded at each end, and a line highlighting where the observed
#'   statistic lies on this distribution.
#' @examples
#' # Here's something I cooked up with migraph earlier:
#' plot(res_migraph_test)
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
                        color = utils::tail(getOption("snet_highlight", 
                                               default = "red"), n = 1), 
                        linewidth=1.2) + ggplot2::ylab("Density")
}

# MRQAP ####

#' Plotting methods for MRQAP models
#' @description
#'   These plotting methods are for results obtained by fitting an MRQAP
#'   model.
#'   The S3 classes are "netlm" or "netlogit", and so are compatible with the
#'   results from either the `{sna}` or `{migraph}` packages.
#' @name model_mrqap
#' @param x An object obtained by fitting an MRQAP model to some data.
#'   For example, `migraph::net_regression()`.
#' @param ... Further arguments to be passed on to plot.
#' @returns A plot showing the location of observed statistics compared to the
#'   distribution of statistics from permuted networks.
#' @examples
#' # Here's something I cooked up with migraph earlier:
#' plot(res_migraph_reg)
#' @export
plot.netlm <- function(x, ...){
  distrib <- x$dist
  distrib <- as.data.frame(distrib)
  names(distrib) <- x$names
  distrib$obs <- seq_len(nrow(distrib))
  distrib <- stats::reshape(data = distrib, # tidyr::pivot_longer replacement
                            direction = "long",
                            varying = colnames(distrib)[-ncol(distrib)],
                            v.names = "value",
                            times = colnames(distrib)[-ncol(distrib)],
                            timevar = "name")
  rownames(distrib) <- NULL
  distrib$id <- NULL
  distrib <- dplyr::arrange(distrib, obs)
  distrib$coef <- rep(unname(x$coefficients), nrow(x$dist))
  distrib$tstat <- rep(unname(x$tstat), nrow(x$dist))
  distrib$name <- factor(distrib$name, x$names)
  ggplot2::ggplot(distrib, ggplot2::aes(.data$value, .data$name)) + 
    ggplot2::geom_violin(draw_quantiles = c(0.025, 0.975)) + 
    ggplot2::theme_minimal() +
    ylab("") + xlab("Statistic") + 
    ggplot2::geom_point(aes(x = .data$tstat), size = 2, 
                        colour = utils::tail(getOption("snet_highlight", 
                                                default = "red"), n = 1)) +
    scale_y_discrete(limits=rev)
}

#' @rdname model_mrqap
#' @export
plot.netlogit <- function(x, ...){
  distrib <- x$dist
  distrib <- as.data.frame(distrib)
  names(distrib) <- x$names
  distrib$obs <- seq_len(nrow(distrib))
  distrib <- stats::reshape(data = distrib, # tidyr::pivot_longer replacement
                            direction = "long",
                            varying = colnames(distrib)[-ncol(distrib)],
                            v.names = "value",
                            times = colnames(distrib)[-ncol(distrib)],
                            timevar = "name")
  rownames(distrib) <- NULL
  distrib$id <- NULL
  distrib <- dplyr::arrange(distrib, obs)
  distrib$coef <- rep(unname(x$coefficients), nrow(x$dist))
  distrib$tstat <- rep(unname(x$tstat), nrow(x$dist))
  distrib$name <- factor(distrib$name, x$names)
  ggplot2::ggplot(distrib, ggplot2::aes(.data$value, .data$name)) + 
    ggplot2::geom_violin(draw_quantiles = c(0.025, 0.975)) + 
    ggplot2::theme_minimal() +
    ylab("") + xlab("Statistic") + 
    ggplot2::geom_point(aes(x = .data$tstat), size = 2, 
                        colour = utils::tail(getOption("snet_highlight", 
                                                default = "red"), n = 1)) +
    scale_y_discrete(limits=rev)
}

