# CUG/QAP ####

#' Plotting methods for CUG and QAP tests
#' @description
#'   These plotting methods are for results obtained by testing some statistic
#'   against those produced in a reference distribution of conditional uniform
#'   graphs or as a quadratic assignment procedure.
#'   The S3 class is "network_test".
#' @param x An object obtained from a conditional uniform graph or
#'   quadratic assignment procedure test.
#'   For example, `infernet::test_permutation()`.
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
  p + ag_theme_classic() + ggplot2::geom_density() +
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
#'   The S3 class "netreg" comes from `{infernet}`, and includes its
#'   "netreg_lm", "netreg_glm", and "netreg_css" results.
#'   The S3 classes "netlm" and "netlogit" come from the `{sna}` package
#'   or from earlier versions of `{migraph}`.
#' @name model_mrqap
#' @param x An object obtained by fitting an MRQAP model to some data.
#'   For example, `infernet::net_regression()`.
#' @param ... Further arguments to be passed on to plot.
#' @returns A plot showing the location of observed statistics compared to the
#'   distribution of statistics from permuted networks.
#'   The vertical lines mark the 2.5% and 97.5% quantiles of each distribution.
#'   A term with no permuted statistics, such as the intercept where
#'   predictors are permuted, shows only its observed statistic.
#' @examples
#' # Here's something I cooked up with infernet earlier:
#' plot(res_infernet_reg)
#' # And with an earlier version of migraph:
#' plot(res_migraph_reg)
#' @export
plot.netreg <- function(x, ...){
  plot_mrqap(x$testdist, x$obs)
}

#' @rdname model_mrqap
#' @export
plot.netlm <- function(x, ...){
  plot_mrqap(stats::setNames(as.data.frame(x$dist), x$names),
             stats::setNames(x$tstat, x$names))
}

#' @rdname model_mrqap
#' @export
plot.netlogit <- function(x, ...){
  plot_mrqap(stats::setNames(as.data.frame(x$dist), x$names),
             stats::setNames(x$tstat, x$names))
}

# Draws the permuted statistics for each term as a violin, and the observed
# statistic as a point on it. `draws` has one column for each term, named as
# `tstat` is; a term whose draws are all NA gets the point alone.
plot_mrqap <- function(draws, tstat){
  terms <- names(tstat)
  draws <- as.data.frame(draws)
  distrib <- data.frame(name = rep(terms, each = nrow(draws)),
                        value = unlist(draws[terms], use.names = FALSE))
  distrib <- distrib[!is.na(distrib$value), ]
  distrib$name <- factor(distrib$name, terms)
  observed <- data.frame(name = factor(terms, terms),
                         tstat = unname(tstat))
  ggplot2::ggplot(distrib, ggplot2::aes(.data$value, .data$name)) + 
    ggplot2::geom_violin(quantile.color = ag_base(),
                         quantile.linetype = "solid", 
                         quantiles = c(0.025, 0.975)) + 
    ag_theme_minimal() +
    ylab("") + xlab("Statistic") + 
    ggplot2::geom_point(data = observed, aes(x = .data$tstat, y = .data$name),
                        size = 2, 
                        colour = utils::tail(getOption("snet_highlight", 
                                                default = "red"), n = 1)) +
    scale_y_discrete(limits = rev, drop = FALSE)
}

# Confusion ####

#' Plotting how well a model classifies ties
#' @description
#'   This plotting method is for the result of
#'   `infernet::diagnose_confusion()`, which compares the ties that a model of
#'   a binary outcome predicts with the ties that were observed.
#'   The S3 class is "infernetConfusion".
#'
#'   - The ROC curve gives the true positive rate against the false positive
#'     rate at every threshold. The diagonal is a model that guesses, and the
#'     area under the curve (AUC) is in the title.
#'   - The confusion matrix gives the mean count of each cell over the draws
#'     of the predicted probabilities, with its 95% interval.
#'   - The metrics give the accuracy, sensitivity, specificity, precision,
#'     and F1 on one scale from 0 to 1.
#' @param x An object of class "infernetConfusion".
#'   For example, `infernet::diagnose_confusion()`.
#' @param panel By default "all", which shows the three panels together.
#'   "roc", "confusion", or "metrics" shows one panel.
#' @param ... Further arguments to be passed on to plot.
#' @returns A ggplot2 object, or a patchwork of three of them.
#' @examples
#' # Needs 'infernet' (https://github.com/stocnet/infernet)
#' \dontrun{
#'   fit <- infernet::estimate_mrqap(. ~ ego(Age) + alter(Age),
#'                                   manynet::ison_adolescents |>
#'                                     manynet::mutate_nodes(Age = 12:19),
#'                                   times = 20, control = list(seed = 1))
#'   plot(infernet::diagnose_confusion(fit, times = 100))
#' }
#' @export
plot.infernetConfusion <- function(x, panel = c("all", "roc", "confusion",
                                                 "metrics"), ...){
  panel <- match.arg(panel)
  switch(panel,
         roc = plot_confusion_roc(x),
         confusion = plot_confusion_matrix(x),
         metrics = plot_confusion_metrics(x),
         all = patchwork::wrap_plots(
           plot_confusion_roc(x),
           patchwork::wrap_plots(plot_confusion_matrix(x),
                                 plot_confusion_metrics(x),
                                 ncol = 1, heights = c(1, 1)),
           ncol = 2, widths = c(1, 1)))
}

# The ROC curve is a step function, read from the highest threshold down.
plot_confusion_roc <- function(x){
  roc <- as.data.frame(x$roc)
  ggplot2::ggplot(roc, ggplot2::aes(.data$fpr, .data$tpr)) +
    ggplot2::annotate("segment", x = 0, y = 0, xend = 1, yend = 1,
                      colour = ag_base(), linetype = "dashed") +
    ggplot2::geom_step(colour = ag_highlight(), linewidth = 1) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ag_theme_minimal() +
    ggplot2::labs(x = "False positive rate", y = "True positive rate",
                  title = paste0("ROC curve (AUC = ",
                                 formatC(x$auc, format = "f", digits = 2), ")"))
}

# Each cell is shaded by its share of the observed row, so that the
# cells of a sparse network's ties are as legible as those of its non-ties.
plot_confusion_matrix <- function(x){
  cm <- x$confusion_matrix
  cells <- expand.grid(Actual = rownames(cm), Predicted = colnames(cm),
                       stringsAsFactors = FALSE)
  cells$count <- as.vector(cm)
  cells$share <- cells$count / rowSums(cm)[cells$Actual]
  cells$label <- paste0(formatC(cells$count, format = "f", digits = 1), "\n[",
                        round(as.vector(x$confusion_matrix_ci_lower)), ", ",
                        round(as.vector(x$confusion_matrix_ci_upper)), "]")
  ggplot2::ggplot(cells, ggplot2::aes(.data$Predicted, .data$Actual)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$share), colour = ag_ink()) +
    # The label sits on the ground colour, so it stays readable on any tile,
    # including themes ("bw", "crisp") whose highlight is their ink.
    ggplot2::geom_label(ggplot2::aes(label = .data$label), colour = ag_ink(),
                        fill = ag_ground_fill(), linewidth = 0,
                        family = ag_font(), size = ag_text_size(3.5)) +
    ggplot2::scale_fill_gradient(low = ag_ground_fill(), high = ag_highlight(),
                                 limits = c(0, 1), guide = "none") +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::coord_equal() +
    ag_theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(title = "Confusion matrix")
}

# The metrics from `glance()`, as a lollipop on one scale from 0 to 1.
plot_confusion_metrics <- function(x){
  metrics <- c(Accuracy = x$accuracy, Sensitivity = x$sensitivity,
               Specificity = x$specificity, Precision = x$precision,
               F1 = x$f1)
  dat <- data.frame(name = factor(names(metrics), rev(names(metrics))),
                    value = unname(metrics))
  ggplot2::ggplot(dat, ggplot2::aes(.data$value, .data$name)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$value,
                                       yend = .data$name),
                          colour = ag_base()) +
    ggplot2::geom_point(colour = ag_highlight(), size = 3) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ag_theme_minimal() +
    ggplot2::labs(x = "", y = "", title = "Metrics")
}
