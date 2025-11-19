#' Plotting adequacy diagnostics
#' @description
#'   These plotting methods are for diagnosing the adequacy of model specification, 
#'   such as those used in goldfish.
#'   These plots are useful for identifying whether there might be significant
#'   outliers affecting the results or significant time heterogeneity.
#' @name plot_adequacy
#' @importFrom patchwork plot_layout
#' @param x An object of class "outliers.goldfish" or "changepoints.goldfish".
#' @param ... Additional plotting parameters, currently unused.
#' @return The function shows a line plot tracing the statistics obtained at
#'   each simulation step, as well as a density plot showing the distribution
#'   of the statistics over the entire simulation.
NULL

#' @rdname plot_adequacy
#' @examples
#' plot(goldfish_outliers)
#' @export
plot.outliers.goldfish <- function(x, ...) {
  if (!"YES" %in% x$outlier) {
    cat("No outliers found.\n")
    return(invisible(NULL))
  }
  
  ggplot2::ggplot(x, ggplot2::aes(x = .data$time, y = .data$intervalLogL)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(color = .data$outlier)) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label),
                       angle = 300, size = 4,
                       hjust = "outward", color = ag_highlight()
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_colour_manual(
      values = c(ag_base(), ag_highlight()),
      guide = "none"
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("Interval log likelihood")
}

#' @rdname plot_adequacy
#' @examples
#' plot(goldfish_changepoints)
#' @export
plot.changepoints.goldfish <- function(x, ...) {
  data <- x$data
  cpt.pts <- x$cpt_points
  
  if (is.null(cpt.pts) || length(cpt.pts) == 0) {
    cat("No regime changes found.\n")
    return(invisible(NULL))
  }
  
  ggplot2::ggplot(data, 
                  ggplot2::aes(x = .data$time, y = .data$intervalLogL)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(
      xintercept = stats::na.exclude(data$time[cpt.pts]),
      color = ag_highlight()
    ) +
    ggplot2::theme_minimal() +
    ggplot2::xlab("") +
    ggplot2::ylab("Interval log likelihood") +
    ggplot2::scale_x_continuous(
      breaks = data$time[cpt.pts],
      labels = data$time[cpt.pts]
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}