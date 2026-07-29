#' Plotting adequacy diagnostics
#' @description
#'   These plotting methods are for diagnosing the adequacy of model
#'   specification, such as those used in goldfish.
#'   These plots are useful for identifying whether there might be significant
#'   outliers affecting the results, whether there is significant time
#'   heterogeneity, and which actors' activity the model does not reproduce.
#' @details
#'   goldfish emits these objects plot-ready. Each is a tibble carrying the
#'   diagnostic metadata contract --- which function produced it, which model
#'   and sub-model it came from, and the arguments that shape how it is read
#'   --- so these methods take their series, their labels and their reference
#'   lines from the object rather than inferring them from the columns that
#'   happen to be present.
#'
#'   The `.series` column is the series the diagnostic actually analysed: the
#'   per-interval log-likelihood by default, and the selected term's own
#'   series when the diagnostic was called with `effect =`. It is `NA` on the
#'   intervals that took no part, which on a rate or REM fit are the
#'   right-censored ones.
#' @name plot_adequacy
#' @param x An object of class `diagnose_outliers`, `diagnose_changepoints`
#'   or `margin_table`, as returned by the goldfish functions of the same
#'   names.
#' @param ... Additional plotting parameters, currently unused.
#' @return A ggplot object.
NULL

# The goldfish diagnostic metadata contract: every object these methods
# receive carries `diagnostic`, `context`, `params` and `version` attributes.
gf_meta <- function(x, which) {
  out <- attr(x, which)
  if (is.null(out)) list() else out
}

# What the y axis is measuring, named by the producer rather than guessed
# here: with `effect =` the analysed series is that term's own, and the two
# diagnostics choose different ones.
gf_series_label <- function(params) {
  label <- params$series
  if (is.null(label)) "Interval log likelihood" else label
}

gf_term_subtitle <- function(params) {
  if (is.null(params$effect)) NULL else paste("Term:", params$effect)
}

#' @rdname plot_adequacy
#' @examples
#' plot(goldfish_outliers)
#' @export
plot.diagnose_outliers <- function(x, ...) {
  params <- gf_meta(x, "params")
  flagged <- !is.na(x$outlier) & x$outlier
  if (!any(flagged)) {
    cat("No outliers found.\n")
    return(invisible(NULL))
  }

  ggplot2::ggplot(x, ggplot2::aes(x = .data$time, y = .data$.series)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$outlier), na.rm = TRUE) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$label),
      angle = 300,
      size = 4,
      na.rm = TRUE,
      hjust = "outward",
      colour = ag_highlight()
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_colour_manual(
      values = c("FALSE" = ag_base(), "TRUE" = ag_highlight()),
      guide = "none"
    ) +
    ggplot2::labs(
      x = "",
      y = gf_series_label(params),
      subtitle = gf_term_subtitle(params)
    )
}

#' @rdname plot_adequacy
#' @examples
#' plot(goldfish_changepoints)
#' @export
plot.diagnose_changepoints <- function(x, ...) {
  params <- gf_meta(x, "params")
  breaks <- x$time[!is.na(x$cpt) & x$cpt]
  if (length(breaks) == 0) {
    cat("No regime changes found.\n")
    return(invisible(NULL))
  }

  p <- ggplot2::ggplot(x, ggplot2::aes(x = .data$time, y = .data$.series)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = breaks, colour = ag_highlight()) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "",
      y = gf_series_label(params),
      subtitle = gf_term_subtitle(params)
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  # Labelling the axis with the break times themselves only works where they
  # are numbers; a goldfish event stream may just as well be dated, and there
  # the default date scale already reads well beside the marked breaks.
  if (is.numeric(x$time)) {
    p <- p + ggplot2::scale_x_continuous(breaks = breaks, labels = breaks)
  }
  p
}

#' @rdname plot_adequacy
#' @details
#'   `plot.margin_table()` shows each actor's observed activity against what
#'   the model expected of them. Which comparison it draws follows the scales
#'   the fit's model class defines, which the object records: where a
#'   compensator is defined (the exact-time sub-models) the difference
#'   `observed - expected_count` is the per-actor martingale residual, read
#'   against zero; on the multinomial sub-models, which have no exposure-time
#'   term and so no compensator, the ratio `observed / expected_probability`
#'   is a calibration ratio, read against one.
#'
#'   These are descriptives rather than per-actor tests: the differences are
#'   plug-in quantities and are negatively correlated across actors. Read the
#'   plot as a map screening for unmodelled actor heterogeneity.
#'
#'   A node set large enough to make one row per actor unreadable is the
#'   ordinary case, so only the `top` actors furthest from the reference are
#'   drawn, and the subtitle says how many were left out. Actors are ranked by
#'   their largest deviation over the roles they appear in, so an actor kept
#'   for one margin keeps the other beside it. Pass `top = Inf` for all of
#'   them.
#' @param top The number of actors to draw, those furthest from the reference.
#' @examples
#' plot(goldfish_margins)
#' @export
plot.margin_table <- function(x, ..., top = 25) {
  scales <- gf_meta(x, "context")$defined_scales
  martingale <- "expected_count" %in% scales
  data <- as.data.frame(x)
  data$value <- if (martingale) {
    data$observed - data$expected_count
  } else {
    data$observed / data$expected_probability
  }
  reference <- if (martingale) 0 else 1
  data$side <- ifelse(data$value >= reference, "above", "below")

  deviation <- abs(data$value - reference)
  ranked <- names(sort(
    tapply(deviation, data$actor, max, na.rm = TRUE),
    decreasing = TRUE
  ))
  omitted <- max(0, length(ranked) - top)
  if (omitted > 0) {
    data <- data[data$actor %in% ranked[seq_len(top)], ]
  }
  data$actor <- stats::reorder(factor(data$actor), data$value)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$value, y = .data$actor)) +
    ggplot2::geom_vline(xintercept = reference, colour = ag_base()) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = reference,
        xend = .data$value,
        y = .data$actor,
        yend = .data$actor,
        colour = .data$side
      )
    ) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$side)) +
    ggplot2::scale_colour_manual(
      values = c(above = ag_positive(), below = ag_negative()),
      guide = "none"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = if (martingale) {
        "Observed minus expected events"
      } else {
        "Observed over expected events"
      },
      y = "",
      subtitle = if (omitted > 0) {
        paste(omitted, "further actors not shown")
      }
    )

  # A tie-oriented fit contributes both margins per actor, and a flavoured fit
  # arrives row-bound with the columns naming its process, so the facets are
  # whichever of those the table carries.
  facets <- intersect(c("flavor", "family", "role"), names(data))
  if (length(facets) > 0) {
    p <- p +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", paste(facets, collapse = " + "))),
        scales = "free_y"
      )
  }
  p
}
