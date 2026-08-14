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
#' @param x An object of class `diagnose_outliers`, `diagnose_changepoints`,
#'   `margin_table`, `test_gof`, `test_time` or `diagnose_onset`, as returned
#'   by the goldfish functions of the same names.
#' @param ... Additional plotting parameters, currently unused.
#' @param page Which page to draw, for the per-term figures. `NULL` (the
#'   default) draws every panel in one figure, exactly as before. A number
#'   draws that page alone; a number past the last is an error naming the
#'   count. Use [ag_pages()] to learn the count without rendering, so a loop
#'   can write every page with nobody at a screen.
#' @param nrow,ncol Panels per page when `page` is given.
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

  p <- ggplot2::ggplot(x, ggplot2::aes(x = .data$time, y = .data$.series)) +
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
  gf_facet_processes(p, x)
}

# Split a row-bound flavoured table into one panel per process.
#
# Not cosmetic on these two plots: the series is drawn with `geom_line()`, and a
# flavoured table arrives as several processes' series stacked, so without the
# split the line is drawn straight across the boundary between one process's
# last event and the next process's first. The panels are what make it a series
# per process rather than one line through all of them.
gf_facet_processes <- function(p, data) {
  facets <- intersect(c("flavor", "family"), names(data))
  if (length(facets) == 0) {
    return(p)
  }
  p +
    ggplot2::facet_wrap(
      stats::as.formula(paste("~", paste(facets, collapse = " + "))),
      scales = "free"
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

  # Carried as a data frame rather than a bare `xintercept` vector: on a
  # flavoured table the breaks belong to the process they were detected in, and
  # a plain vector would draw every process's breaks onto every panel.
  marked <- x[!is.na(x$cpt) & x$cpt, , drop = FALSE]

  p <- ggplot2::ggplot(x, ggplot2::aes(x = .data$time, y = .data$.series)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::geom_vline(
      data = marked,
      mapping = ggplot2::aes(xintercept = .data$time),
      colour = ag_highlight()
    ) +
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
  gf_facet_processes(p, x)
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
  # `margin_table(dispersion = TRUE)` carries a second reading, and where both
  # are present the informative figure is the two against each other rather
  # than either alone: level says whether an actor acted often enough, shape
  # whether its events were spaced the way the model implies, and an actor can
  # fail one while passing the other.
  if ("dispersion" %in% names(data) && !all(is.na(data$dispersion))) {
    return(gf_margin_scatter(data, martingale, top))
  }
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

#' @rdname plot_adequacy
#' @details
#'   `plot.test_gof()` draws each effect's standardized cumulative score
#'   process against the Brownian-bridge bands its p-value was read from. At
#'   the maximum the per-event scores sum to zero, so every path starts and
#'   ends at zero; under a correctly specified model it is a bridge, and a path
#'   that wanders outside the bands is an effect whose contribution is
#'   concentrated somewhere in the sequence.
#'
#'   The x axis is the object's own process-time axis, taken from its `u`
#'   column and labelled by the `clock` it records. This is not a
#'   presentational detail: the bands are valid on whichever clock produced the
#'   process, and re-deriving an event-index axis here would draw the path on
#'   one clock and the reference on another. On the information clock the
#'   spacing of the steps is itself the diagnostic --- a path that crosses most
#'   of the axis in a few steps is an effect whose information arrives late.
#' @param level The confidence level of the reference bands, defaulting to
#'   0.95. The band is the two-sided Kolmogorov quantile of the supremum of a
#'   Brownian bridge, which is the reference the event-clock p-value uses.
#' @examples
#' plot(goldfish_gof)
#' @export
plot.test_gof <- function(
  x,
  ...,
  level = 0.95,
  page = NULL,
  nrow = 2,
  ncol = 2
) {
  process <- as.data.frame(x$process)
  clock <- gf_meta(x, "params")$clock

  p <- ggplot2::ggplot(
    process,
    ggplot2::aes(x = .data$u, y = .data$process)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = ag_base()) +
    ggplot2::geom_hline(
      yintercept = c(-1, 1) * gf_bridge_quantile(level),
      colour = ag_highlight(),
      linetype = "dashed"
    ) +
    ggplot2::geom_step(na.rm = TRUE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = gf_clock_label(clock),
      y = "Standardized cumulative score",
      subtitle = paste0(
        "Brownian-bridge band at ",
        format(100 * level),
        "%"
      )
    )
  gf_facet_paged(
    p,
    gf_block_facets(process),
    page,
    nrow,
    ncol,
    ag_pages(x, nrow, ncol),
    scales = "fixed"
  )
}

#' @rdname plot_adequacy
#' @details
#'   `plot.test_time()` draws the scaled Schoenfeld residuals of each tested
#'   effect against time, with a smooth and the fitted estimate as the
#'   reference. A residual scatter is centred on the coefficient the model
#'   estimated; a smooth that drifts away from that line over the sequence is
#'   the coefficient failing to be constant, which is what the test's p-value
#'   states formally.
#'
#'   Under `method = "periods"` the intervals are coloured by their period, so
#'   the regimes the test compared are visible against the same scatter.
#' @examples
#' plot(goldfish_time)
#' @export
plot.test_time <- function(x, ..., page = NULL, nrow = 2, ncol = 2) {
  residuals <- as.data.frame(x$residuals)
  params <- gf_meta(x, "params")
  # `period` is all-NA under the trend method, which has no periods; colouring
  # by a constant would put a one-level legend on every trend plot.
  by_period <- !all(is.na(residuals$period))

  p <- ggplot2::ggplot(
    residuals,
    ggplot2::aes(x = .data$clock, y = .data$residual)
  ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = .data$reference),
      colour = ag_base()
    )
  p <- if (by_period) {
    p +
      ggplot2::geom_point(
        ggplot2::aes(colour = .data$period),
        alpha = 0.4,
        na.rm = TRUE
      )
  } else {
    p + ggplot2::geom_point(alpha = 0.4, na.rm = TRUE, colour = ag_base())
  }
  p <- p +
    ggplot2::geom_smooth(
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      colour = ag_highlight(),
      na.rm = TRUE
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Model time",
      y = "Scaled Schoenfeld residual",
      subtitle = gf_time_subtitle(params)
    )
  gf_facet_paged(
    p,
    gf_block_facets(residuals),
    page,
    nrow,
    ncol,
    ag_pages(x, nrow, ncol),
    scales = "free_y"
  )
}

# The panels a test object facets on: the term always, plus the two identity
# columns a flavoured (multi-process) result appends. Taking them from the
# table rather than from the object's class is what lets one method serve both
# shapes, as `plot.margin_table()` already does.
gf_block_facets <- function(data) {
  facets <- c("term", intersect(c("flavor", "family"), names(data)))
  stats::as.formula(paste("~", paste(facets, collapse = " + ")))
}

# The two-sided Kolmogorov quantile: the level `q` with
# P(sup|B| <= q) = level for a Brownian bridge B. Solved by bisection on the
# series 1 - 2 sum (-1)^{j-1} exp(-2 j^2 q^2), which is the same distribution
# the event-clock p-value inverts, so band and p-value cannot disagree.
gf_bridge_quantile <- function(level) {
  cdf <- function(q) {
    j <- seq_len(100)
    1 - 2 * sum((-1)^(j - 1) * exp(-2 * j^2 * q^2))
  }
  # `uniroot`'s default tolerance is about 1e-4, which is invisible in a drawn
  # band but would make the band and the p-value disagree in the last digits.
  # They invert the same distribution, so solve it to machine precision.
  stats::uniroot(
    function(q) cdf(q) - level,
    interval = c(0.1, 10),
    tol = .Machine$double.eps^0.75
  )$root
}

# The axis label names the clock the process was built on, because the two are
# different quantities: event-clock steps are equally spaced by construction,
# information-clock steps are spaced by how much each event contributed.
gf_clock_label <- function(clock) {
  if (identical(clock, "information")) {
    return("Cumulative share of information")
  }
  "Share of events"
}

gf_time_subtitle <- function(params) {
  if (identical(params$method, "periods")) {
    return("Score test of a coefficient difference across periods")
  }
  transform <- params$transform
  if (is.null(transform) || is.na(transform)) {
    transform <- "identity"
  }
  paste0("Score test of a ", transform, " time trend")
}

#' @rdname plot_adequacy
#' @details
#'   `plot.diagnose_onset()` composes two panels: each coefficient's
#'   leave-the-first-`m`-events-out path, and the share of the model's
#'   information those events delivered.
#'
#'   Both panels are **windowed on the excursion rather than the sequence**,
#'   because the full range is mostly bridge tail --- the path returns to the
#'   estimate by construction, so drawing all of it squashes the part being
#'   read into a few percent of the axis. Each coefficient gets its own window
#'   and its own x scale, since coefficients settle at very different points
#'   and a window shared across facets re-creates the squashing it exists to
#'   prevent. A coefficient whose path never left its band takes the full
#'   range, there being no excursion to window on.
#'
#'   The accrual panel is drawn full-range with the onset window shaded, and
#'   carries the proportional diagonal `y = x / n`. Without the diagonal a
#'   monotone curve from 0 to 1 says nothing: the signal is the *departure*
#'   from proportional, which is what makes an opening segment that carries
#'   little information visible.
#'
#'   Coefficients held fixed through `offset()` are not drawn. Their path is a
#'   flat line at the imposed value by construction.
#' @param view Which panels to draw: `"both"` (default), or `"path"` or
#'   `"accrual"` alone, which is the escape hatch when a model has too many
#'   coefficients for a composed figure to stay readable.
#' @param tolerance_band Whether to draw each coefficient's stabilization
#'   band, the `+/- tolerance * std_error` corridor the path had to re-enter.
#' @examples
#' plot(goldfish_onset)
#' @export
plot.diagnose_onset <- function(
  x,
  ...,
  view = c("both", "path", "accrual"),
  tolerance_band = TRUE,
  page = NULL,
  nrow = 2,
  ncol = 2
) {
  view <- match.arg(view)
  context <- gf_meta(x, "context")
  params <- gf_meta(x, "params")
  summary <- as.data.frame(x$summary)
  # An offset never moves, so its path is its imposed value repeated.
  summary <- summary[!summary$fixed, , drop = FALSE]
  if (nrow(summary) == 0) {
    cat("No estimated coefficient to trace.\n")
    return(invisible(NULL))
  }

  path <- gf_onset_path_panel(
    x,
    summary,
    params,
    tolerance_band,
    page = page,
    nrow = nrow,
    ncol = ncol,
    n_pages = ag_pages(x, nrow, ncol)
  )
  if (identical(view, "path")) {
    return(path)
  }
  accrual <- gf_onset_accrual_panel(x, summary, context)
  if (identical(view, "accrual")) {
    return(accrual)
  }
  patchwork::wrap_plots(path, accrual, ncol = 1, heights = c(2, 1))
}

# The path panel, windowed per coefficient on its own excursion. The window is
# `1.15 * stabilized_at`, floored at 10: a proportional floor squashes the
# coefficients that settle in a handful of events, and an absolute margin
# (`+ 20`) overshoots the ones whose whole excursion is shorter than that.
gf_onset_path_panel <- function(
  x,
  summary,
  params,
  tolerance_band,
  page = NULL,
  nrow = 2,
  ncol = 2,
  n_pages = 1L
) {
  path <- as.data.frame(x$path)
  path <- path[path$index %in% summary$index, , drop = FALSE]
  n_events <- max(path$dropped_events)
  windows <- stats::setNames(
    vapply(
      summary$stabilized_at,
      function(at) {
        if (at == 0) n_events else min(n_events, max(ceiling(1.15 * at), 10))
      },
      numeric(1)
    ),
    summary$term
  )
  path <- path[path$dropped_events <= windows[path$term], , drop = FALSE]
  # The marker joins `summary` onto `path` by term, which the plot-data
  # contract permits: the two tables of one object may be read together.
  markers <- summary[summary$stabilized_at > 0, , drop = FALSE]

  p <- ggplot2::ggplot(
    path,
    ggplot2::aes(x = .data$dropped_events, y = .data$estimate)
  )
  if (tolerance_band) {
    tolerance <- params$tolerance
    if (is.null(tolerance)) {
      tolerance <- 0.1
    }
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = .data$reference - tolerance * .data$std_error,
          ymax = .data$reference + tolerance * .data$std_error
        ),
        fill = ag_base(),
        alpha = 0.2
      )
  }
  p <- p +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = .data$reference),
      colour = ag_base()
    ) +
    ggplot2::geom_line(colour = ag_base(), na.rm = TRUE)
  if (nrow(markers) > 0) {
    p <- p +
      ggplot2::geom_vline(
        data = markers,
        ggplot2::aes(xintercept = .data$stabilized_at),
        colour = ag_highlight(),
        linetype = "dashed"
      )
  }
  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Initial events dropped",
      y = "Estimate",
      subtitle = "Path with the stabilization point marked"
    )
  gf_facet_paged(
    p,
    stats::as.formula("~ term"),
    page,
    nrow,
    ncol,
    n_pages,
    scales = "free"
  )
}

# The accrual panel: full range with the onset window shaded, and the
# proportional diagonal drawn. The diagonal is what makes the curve readable --
# the departure from it is the finding, not the curve's monotonicity.
gf_onset_accrual_panel <- function(x, summary, context) {
  accrual <- as.data.frame(x$accrual)
  onset <- max(summary$stabilized_at)
  n_events <- context$n_events
  if (is.null(n_events)) {
    n_events <- max(accrual$dropped_events)
  }

  p <- ggplot2::ggplot(
    accrual,
    ggplot2::aes(x = .data$dropped_events, y = .data$share)
  )
  if (onset > 0) {
    p <- p +
      ggplot2::annotate(
        "rect",
        xmin = 0,
        xmax = onset,
        ymin = 0,
        ymax = 1,
        fill = ag_highlight(),
        alpha = 0.15
      )
  }
  p +
    ggplot2::geom_abline(
      slope = 1 / n_events,
      intercept = 0,
      colour = ag_base(),
      linetype = "dashed"
    ) +
    ggplot2::geom_line(colour = ag_base(), na.rm = TRUE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Initial events dropped",
      y = "Share of information",
      subtitle = "Accrual against proportional, onset window shaded"
    )
}

#' Plotting a goldfish model fit at a glance
#'
#' @description
#'   One call, four diagnostic panels: whether any interval is badly fitted,
#'   whether any coefficient drifts, whether each effect's contribution is
#'   spread over the sequence, and whether the waiting times are what the model
#'   says they are.
#'
#' @details
#'   Everything is drawn from what the **fit already stores** --- no evaluation
#'   pass and no preprocessed statistics --- so the figure costs a plot and not
#'   a re-fit. The consequence is that a panel needing a primitive the fit did
#'   not store is **left out** rather than erroring: which panels appear is
#'   itself a readout of what was requested at estimation.
#'
#'   \describe{
#'     \item{deviance}{the per-interval log-likelihood with outlying intervals
#'       marked. Needs the `"loglik"` primitive.}
#'     \item{scaled Schoenfeld}{a smooth per effect against the fitted estimate,
#'       flat under a constant coefficient. Needs `"scores"` on a multinomial
#'       sub-model, and `"conditional_scores"` on an exact-time one, where the
#'       score carries an exposure term the Schoenfeld residual does not.}
#'     \item{cumulative score}{each effect's standardized process against its
#'       Brownian-bridge band. Needs `"scores"`.}
#'     \item{waiting times}{the Cox-Snell residuals against the unit
#'       exponential they follow under the model. Exact-time sub-models only:
#'       an ordinal likelihood conditions the timing away, so there is no
#'       waiting time to check.}
#'   }
#'
#'   The Schoenfeld panel is capped at the `effects` most worth looking at,
#'   ranked by their cumulative-score statistic, since a model with a dozen
#'   terms makes a facet grid unreadable at overview size.
#'
#' @param x A fitted model of class `result.goldfish`.
#' @param ... Additional plotting parameters, currently unused.
#' @param effects The number of effects to draw in the Schoenfeld panel.
#' @return A patchwork composition of the available panels.
#' @name plot_goldfish_fit
#' @examples
#' plot(goldfish_fit)
#' @export
plot.result.goldfish <- function(x, ..., effects = 4) {
  thisRequires("goldfish")
  panels <- list(
    gf_overview_deviance(x),
    gf_overview_schoenfeld(x, effects),
    gf_overview_gof(x),
    gf_overview_waiting(x)
  )
  panels <- Filter(Negate(is.null), panels)
  if (length(panels) == 0) {
    cat("This fit stores no diagnostic primitive to plot.\n")
    return(invisible(NULL))
  }
  patchwork::wrap_plots(panels, ncol = min(2, length(panels)))
}

# Each panel is attempted and dropped on failure rather than pre-checked
# against a primitive list: goldfish already raises a named error when a
# primitive is missing, and duplicating its availability rules here is how the
# two would drift apart.
gf_overview_try <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}

gf_overview_deviance <- function(x) {
  outliers <- gf_overview_try(.ag_goldfish("diagnose_outliers")(x))
  if (is.null(outliers)) {
    return(NULL)
  }
  data <- as.data.frame(outliers)
  # Unlike the standalone method this draws the trace even with nothing
  # flagged: in a composed figure a clean panel is a finding, and a panel that
  # vanished would read as a missing primitive instead.
  ggplot2::ggplot(data, ggplot2::aes(x = .data$time, y = .data$.series)) +
    ggplot2::geom_line(colour = ag_base(), na.rm = TRUE) +
    ggplot2::geom_point(
      data = data[!is.na(data$outlier) & data$outlier, , drop = FALSE],
      colour = ag_highlight(),
      na.rm = TRUE
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "", y = "Interval log likelihood", subtitle = "Deviance")
}

gf_overview_schoenfeld <- function(x, effects) {
  rows <- gf_overview_try(
    stats::residuals(x, type = "scaled_schoenfeld")
  )
  if (is.null(rows)) {
    return(NULL)
  }
  available <- colnames(rows)
  keep <- gf_overview_rank(x, available, effects)
  omitted <- length(available) - length(keep)
  labels <- gf_overview_labels(x, keep, available)
  long <- data.frame(
    interval = rep(seq_len(nrow(rows)), times = length(keep)),
    term = rep(labels, each = nrow(rows)),
    value = as.numeric(rows[, keep, drop = FALSE])
  )
  estimates <- stats::coef(x)[keep]
  reference <- data.frame(term = labels, estimate = as.numeric(estimates))

  ggplot2::ggplot(long, ggplot2::aes(x = .data$interval, y = .data$value)) +
    ggplot2::geom_hline(
      data = reference,
      ggplot2::aes(yintercept = .data$estimate),
      colour = ag_base()
    ) +
    ggplot2::geom_smooth(
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      colour = ag_highlight(),
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(~ .data$term, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "",
      y = "",
      # A reduced figure says so. Drawing four of fifty-six without a word is
      # the same failure as a diagnostic reporting nothing because it could not
      # see anything: the output looks like an answer about the whole model.
      subtitle = if (omitted > 0) {
        paste0(
          "Scaled Schoenfeld \u2014 ",
          length(keep),
          " of ",
          length(available),
          " terms, ranked; ",
          omitted,
          " not shown"
        )
      } else {
        "Scaled Schoenfeld"
      }
    )
}

# Which effects the Schoenfeld panel draws. Ranked by the cumulative-score
# statistic where it is available, so the panel shows what is worth looking at
# rather than whichever terms the formula happened to name first.
gf_overview_rank <- function(x, terms, effects) {
  if (length(terms) <= effects) {
    return(seq_along(terms))
  }
  gof <- gf_overview_try(.ag_goldfish("test_gof")(x))
  if (is.null(gof)) {
    return(seq_len(effects))
  }
  # Selected by COLUMN POSITION, not by name. The residual matrix is named by
  # effect (`indeg`, `outdeg`), which repeats when one effect appears over two
  # networks, while the test names coefficients (`ideg_cal`, `ideg_fri`). The
  # two vocabularies intersect only on the intercept, so matching them by name
  # silently kept one term where several were asked for -- and with duplicated
  # names, `rows[, "indeg"]` would have drawn the first of them either way.
  ranked <- gof$effects$index[order(gof$effects$statistic, decreasing = TRUE)]
  ranked <- ranked[ranked >= 1 & ranked <= length(terms)]
  if (length(ranked) == 0) {
    seq_len(effects)
  } else {
    utils::head(ranked, effects)
  }
}

# Panel labels for the selected columns. The test's own compact term strings
# where they are available, since those distinguish an effect appearing over
# two networks; otherwise the residual names made unique, which is ugly but
# never ambiguous.
gf_overview_labels <- function(x, keep, terms) {
  gof <- gf_overview_try(.ag_goldfish("test_gof")(x))
  labels <- make.unique(terms)[keep]
  if (!is.null(gof)) {
    matched <- match(keep, gof$effects$index)
    labels <- ifelse(is.na(matched), labels, gof$effects$term[matched])
  }
  labels
}

gf_overview_gof <- function(x) {
  gof <- gf_overview_try(.ag_goldfish("test_gof")(x))
  if (is.null(gof)) {
    return(NULL)
  }
  plot(gof) +
    ggplot2::labs(subtitle = "Cumulative score", x = "", y = "") +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 7))
}

gf_overview_waiting <- function(x) {
  # Exact-time only, and the error goldfish raises on an ordinal fit is what
  # decides that -- the panel does not re-derive which families have a
  # compensator.
  residuals <- gf_overview_try(stats::residuals(x, type = "cox_snell"))
  if (is.null(residuals)) {
    return(NULL)
  }
  observed <- sort(as.numeric(residuals))
  data <- data.frame(
    theoretical = stats::qexp(stats::ppoints(length(observed))),
    observed = observed
  )
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$theoretical, y = .data$observed)
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = ag_base()) +
    ggplot2::geom_point(colour = ag_highlight(), alpha = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Unit exponential",
      y = "Cox-Snell residual",
      subtitle = "Waiting times"
    )
}

# Pagination --------------------------------------------------------------

#' How many pages a paged diagnostic figure has
#'
#' @description
#' The page count of [plot()] on a per-term diagnostic, derivable **without
#' rendering** so a loop can write every page.
#'
#' A method that only discovers it is on the last page once it gets there
#' cannot be scripted, and scripting is the case this exists for: fits go to a
#' cluster, so a figure has to be producible with nobody at a screen to press
#' return.
#'
#' @param x a diagnostic object with one panel per term -- as returned by
#'   `test_gof()`, `test_time()`, `diagnose_onset()`, or a fitted goldfish
#'   model.
#' @param nrow,ncol panels per page, matching what will be passed to `plot()`.
#'
#' @return A single integer, at least 1.
#' @examples
#' ag_pages(goldfish_gof)
#' @export
ag_pages <- function(x, nrow = 2, ncol = 2) {
  panels <- gf_panel_count(x)
  if (is.na(panels) || panels < 1) {
    return(1L)
  }
  as.integer(max(1L, ceiling(panels / (nrow * ncol))))
}

# How many panels a per-term figure would draw. Read off the same component and
# the same facet columns the plot method facets by, so the two cannot disagree
# about what a page holds.
gf_panel_count <- function(x) {
  data <- gf_panel_data(x)
  if (is.null(data)) {
    return(NA_integer_)
  }
  keys <- intersect(c("term", "flavor", "family"), names(data))
  if (length(keys) == 0) {
    return(NA_integer_)
  }
  nrow(unique(data[keys]))
}

gf_panel_data <- function(x) {
  if (inherits(x, "test_gof")) {
    return(as.data.frame(x$process))
  }
  if (inherits(x, "test_time")) {
    return(as.data.frame(x$residuals))
  }
  if (inherits(x, "diagnose_onset")) {
    return(as.data.frame(x$path))
  }
  if (inherits(x, "result.goldfish")) {
    return(NULL)
  }
  NULL
}

# One page of a faceted figure, or all of it.
#
# `page = NULL` keeps the ordinary `facet_wrap()`, so nothing about the
# unpaged figure changes. A page beyond the last is an error naming the count
# rather than an empty panel, which is what a loop with an off-by-one would
# otherwise produce and not notice.
gf_facet_paged <- function(p, facets, page, nrow, ncol, n_pages, scales) {
  if (is.null(page)) {
    return(p + ggplot2::facet_wrap(facets, scales = scales))
  }
  if (!is.numeric(page) || length(page) != 1L || is.na(page) || page < 1) {
    manynet::snet_abort("{.arg page} must be a single positive number.")
  }
  page <- as.integer(page)
  if (page > n_pages) {
    manynet::snet_abort(
      "{.arg page} {.val {page}} is past the last page.",
      "This figure has {n_pages} page{?s} at",
      "{.code nrow = {nrow}, ncol = {ncol}}.",
      "{.fn ag_pages} reports the count without rendering.")
  }
  p +
    ggforce::facet_wrap_paginate(
      facets,
      nrow = nrow,
      ncol = ncol,
      page = page,
      scales = scales
    )
}

# Level against shape, one point per actor.
#
# The quadrants are the reading. An actor is calibrated on level near the
# vertical reference and on shape near a dispersion of one, so the four corners
# are four distinct misfits: too many events and bursty, too few and bursty,
# and so on. Sized by the event count because the shape reading is undefined
# below two completed spans and noisy just above it -- a large point is one
# worth believing.
gf_margin_scatter <- function(data, martingale, top) {
  data$value <- if (martingale) {
    data$observed - data$expected_count
  } else {
    data$observed / data$expected_probability
  }
  reference <- if (martingale) 0 else 1
  usable <- data[!is.na(data$dispersion), , drop = FALSE]
  omitted_shape <- nrow(data) - nrow(usable)

  deviation <- abs(usable$value - reference)
  ranked <- names(sort(
    tapply(deviation, usable$actor, max, na.rm = TRUE),
    decreasing = TRUE
  ))
  omitted_top <- max(0, length(ranked) - top)
  if (omitted_top > 0) {
    usable <- usable[usable$actor %in% ranked[seq_len(top)], ]
  }

  p <- ggplot2::ggplot(
    usable,
    ggplot2::aes(x = .data$value, y = .data$dispersion)
  ) +
    ggplot2::geom_vline(xintercept = reference, colour = ag_base()) +
    # One is the dispersion of a unit exponential, which each span is under a
    # correct model -- the same reference the level axis reads against.
    ggplot2::geom_hline(yintercept = 1, colour = ag_base()) +
    ggplot2::geom_point(
      ggplot2::aes(size = .data$observed),
      alpha = 0.6,
      colour = ag_highlight(),
      na.rm = TRUE
    ) +
    ggplot2::scale_size_continuous(name = "Events") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = if (martingale) {
        "Observed minus expected events"
      } else {
        "Observed over expected events"
      },
      y = "Dispersion of the actor's own spans",
      subtitle = gf_scatter_subtitle(omitted_shape, omitted_top)
    )
  facets <- intersect(c("flavor", "family", "role"), names(usable))
  if (length(facets) > 0) {
    p <- p +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", paste(facets, collapse = " + ")))
      )
  }
  p
}

# Both kinds of omission are named. An actor can be missing because it has too
# few events for a shape reading, or because it is not among the `top` furthest
# from the reference, and a figure that drew a subset without saying which
# would look like the whole node set.
gf_scatter_subtitle <- function(omitted_shape, omitted_top) {
  parts <- c(
    if (omitted_shape > 0) {
      paste(omitted_shape, "actors below two completed spans")
    },
    if (omitted_top > 0) paste(omitted_top, "further actors not shown")
  )
  if (length(parts) == 0) NULL else paste(parts, collapse = "; ")
}
