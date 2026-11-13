# Models ####

#' Plotting diffusion models
#' @name plot.diffusion
#' @param x A "diff_model" of "diffs_model" class of object.
#'   E.g. as a result from `manynet::play_diffusion()`.
#' @param ... Other arguments to be passed.
#' @param all_steps Whether all steps should be plotted or just those where
#'   there is change in the distributions.
#' @details
#'   `plot.diff_model()` draws one line for each value of the attribute
#'   that the diffusion changes,
#'   and bars for the nodes that become infectious at each step.
#'   A diffusion in the values S, E, I, R, and D names them
#'   "Susceptible", "Exposed", "Infected", "Recovered", and "Dead".
#'   A diffusion in values of its own,
#'   as from `manynet::play_diffusion(compartments = )`,
#'   names the lines by these values.
#'   The roles in `attr(x, "compartments")` give the colours:
#'   blue for S, yellow for E, red for I, green for R,
#'   and black for D.
#'   The bars count the new nodes in the first value of I.
#' @importFrom dplyr left_join
#' @importFrom ggplot2 geom_histogram
#' @returns `plot.diff_model()` returns a bar chart of the number of new 
#'   infected nodes at each time point, as well as an overlay line plot of the
#'   total in each value
#' @examples
#' plot(res_manynet_diff)
#' @export
plot.diff_model <- function(x, ..., all_steps = TRUE){
  value <- prop <- new <- NULL # initialize variables to avoid CMD check notes
  attribute <- attr(x, "attribute")
  if(is.null(attribute)) attribute <- "diffusion"
  if(nrow(x) <= 1){
    if(identical(attribute, "diffusion")) manynet::snet_warn(
      "No diffusion was observed, so there is nothing to plot.",
      "The diffusion ran for a single step, which happens when no node was",
      "seeded or when the seeds had no one to infect.",
      "Check the {.arg seeds} given to {.fn manynet::play_diffusion},",
      "and that the network has ties.") else manynet::snet_warn(
        "No change in {.val {attribute}} was observed, so there is nothing to plot.",
        "The diffusion ran for a single step, which happens when no node was",
        "infectious or when the infectious nodes had no one to infect.",
        "Check the start values of {.val {attribute}}",
        "and the {.arg compartments} given to {.fn manynet::play_diffusion},",
        "and that the network has ties.")
    return(invisible(NULL))
  }
  data <- as.data.frame(x)
  tcol <- if("time" %in% names(data)) "time" else "t"
  n <- if("n" %in% names(data)) data$n else
    rowSums(data[, .diffusion_values(data), drop = FALSE])
  values <- .diffusion_values(data)
  comps <- .diffusion_roles(attr(x, "compartments"), values)
  infectious <- if(length(comps$I)) as.character(comps$I[1]) else NULL
  if(!is.null(infectious) && !infectious %in% values) infectious <- NULL
  seir <- all(values %in% c("S", "E", "I", "R", "D")) &&
    all(vapply(names(comps), function(r) identical(as.character(comps[[r]]), r),
               logical(1)))
  if(!all_steps && !is.null(infectious)){
    last <- data[[infectious]][nrow(data)]
    data <- data[!(data[[infectious]] == last *
                     duplicated(data[[infectious]] == last)), , drop = FALSE]
    n <- if("n" %in% names(data)) data$n else n[seq_len(nrow(data))]
  }
  # A record in S, E, I, and R always has the four columns, but draws E and R
  # only where some node is in them, as it always has.
  drawn <- if(seir) values[values %in% c("S", "I") |
                             vapply(values, function(v) any(data[[v]] > 0),
                                    logical(1))] else values
  long <- do.call(rbind, lapply(drawn, function(v)
    data.frame(time = data[[tcol]], value = v, prop = data[[v]]/n)))
  long$value <- factor(long$value, levels = drawn)
  labs <- if(seir) c(S = "Susceptible", E = "Exposed", I = "Infected",
                     R = "Recovered", D = "Dead")[drawn] else drawn
  # The lines of S and I are drawn under the bars, and the other lines over
  # them, as they always have been.
  under <- as.character(long$value) %in% unlist(comps[c("S", "I")])
  p <- ggplot2::ggplot(long) + 
    ggplot2::geom_line(ggplot2::aes(x = time, y = prop, color = value), 
                       data = long[under, , drop = FALSE], linewidth = 1.25)
  if(!is.null(infectious))
    p <- p + ggplot2::geom_col(ggplot2::aes(x = time, y = new), 
                               data = data.frame(time = data[[tcol]], 
                                                 new = data[[paste0(infectious, "_new")]]/n),
                               alpha = 0.4)
  if(any(!under))
    p <- p + ggplot2::geom_line(ggplot2::aes(x = time, y = prop, color = value), 
                                data = long[!under, , drop = FALSE], 
                                linewidth = 1.25)
  p + ag_theme_minimal() + 
    # using coord_cartesian to avoid printing warnings
    ggplot2::coord_cartesian(ylim = c(0,1)) + 
    ggplot2::scale_x_continuous(breaks = function(x) pretty(x, n=6)) +
    ggplot2::ylab("Proportion") + ggplot2::xlab("Steps") +
    ggplot2::scale_color_manual(if(seir) "Legend" else attribute, 
                                labels = unname(labs),
                                values = .diffusion_colors(drawn, comps),
                                breaks = drawn,
                                guide = "legend")
}

# The value columns of a diffusion report: the columns that are not the time,
# the number of nodes, the number exposed, or the new nodes in each value.
.diffusion_values <- function(data){
  nm <- names(data)
  nm[!nm %in% c("time", "t", "n", "s") & !grepl("_new$", nm)]
}

# The roles of the values as a named list of character vectors. A report from
# before the compartments were kept is read as S, E, I, and R where its values
# are these, and as FALSE and TRUE where its values are logical.
.diffusion_roles <- function(comps, values){
  comps <- comps[!vapply(comps, is.null, logical(1))]
  if(is.null(names(comps)) || !length(comps)) comps <- NULL
  if(is.null(comps)){
    if(all(values %in% c("S", "E", "I", "R", "D")))
      comps <- as.list(stats::setNames(values, values)) else
        if(setequal(values, c("FALSE", "TRUE")) || identical(values, "FALSE") ||
           identical(values, "TRUE"))
          comps <- list(S = "FALSE", I = "TRUE") else comps <- list()
  }
  lapply(comps, as.character)
}

# Gives each role its usual colour: S blue, E yellow, I red, R green, and D
# black. The first value of a role takes the colour of the role; the other
# values, and values without a role, take other colours of the palette.
.diffusion_colors <- function(values, comps){
  role_cols <- c(S = "#4575b4", E = "#E6AB02", I = "#d73027", R = "#66A61E")
  base <- stats::setNames(match_color(role_cols), names(role_cols))
  # D is black in every theme, and is not matched to the palette. The darkest
  # colour of a palette can be near the colour of I, as plum is near maroon in
  # "iheid"; black is darker than all the colours of S, E, I, and R.
  base <- c(base, D = "#000000")
  pal <- ag_qualitative()
  rest <- setdiff(pal, base)
  out <- stats::setNames(rep(NA_character_, length(values)), values)
  for(r in names(comps)){
    first <- comps[[r]][1]
    if(first %in% values && r %in% names(base)) out[first] <- base[[r]]
  }
  free <- names(out)[is.na(out)]
  if(length(free)){
    extra <- if(length(free) <= length(rest)) rest[seq_along(free)] else
      ag_qualitative(length(free) + length(base))[-seq_along(base)][seq_along(free)]
    out[free] <- extra
  }
  out
}

#' @noRd
#' @export
plot.mnet <- function(x, ...){
  if(.is_mobility(x)){
    .plot_mobility_grid(x)
  } else if(.is_opinion(x)){
    .plot_opinion_network(x, ...)
  } else if(manynet::is_changing(x)){
    plot(manynet::as_diffusion(x, attribute = .diffusion_attribute(x)), ...)
  } else {
    graphr(x)
  }
}

#' @noRd
#' @export
plot.stocnet <- plot.mnet

# The attribute whose changes record a diffusion: "diffusion" where it changes,
# else the changing attribute whose values have roles in `info$compartments`,
# else a logical attribute that changes, else the first attribute that changes.
.diffusion_attribute <- function(x){
  changes <- manynet::as_changelist(x)
  vars <- if(!is.null(changes) && "var" %in% names(changes))
    unique(as.character(changes$var)) else character(0)
  # "active" records when nodes are present, not a diffusion
  vars <- setdiff(vars, "active")
  if(!length(vars) || "diffusion" %in% vars) return("diffusion")
  comps <- .net_compartments(x)
  if(!is.null(comps)){
    roled <- as.character(unlist(comps, use.names = FALSE))
    for(v in vars){
      vals <- as.character(unlist(changes$value[changes$var == v]))
      if(length(vals) && all(vals %in% roled)) return(v)
    }
  }
  # a logical attribute needs no compartments: FALSE is S and TRUE is I
  for(v in vars){
    vals <- unlist(changes$value[changes$var == v])
    if(is.logical(vals)) return(v)
  }
  vars[1]
}

# The colours of a diffusion in an attribute other than "diffusion", for
# `graphs()` and `grapht()`: the attribute, and a colour for each of its
# values, by its role in the compartments. NULL where the network is not such
# a diffusion. A diffusion in "diffusion" has colours of its own; see
# `.map_infected_nodes()` in R/graph_nodes.R.
.diffusion_fill <- function(x){
  if(manynet::is_list(x) || !manynet::is_manynet(x) ||
     !manynet::is_changing(x) || .is_mobility(x) || .is_opinion(x))
    return(NULL)
  attribute <- .diffusion_attribute(x)
  if(identical(attribute, "diffusion") ||
     !attribute %in% manynet::net_node_attributes(x)) return(NULL)
  changes <- manynet::as_changelist(x)
  changed <- unlist(changes$value[as.character(changes$var) == attribute])
  start <- manynet::node_attribute(x, attribute)
  if(is.numeric(start) && !is.logical(start) &&
     length(unique(c(start, changed))) > 10) return(NULL)
  values <- unique(as.character(c(start, changed)))
  values <- values[!is.na(values)]
  comps <- .diffusion_roles(.net_compartments(x), values)
  roled <- unlist(comps[intersect(c("S", "E", "I", "R", "D"), names(comps))],
                  use.names = FALSE)
  # A changing attribute whose values have no roles is not a diffusion
  if(!any(values %in% roled)) return(NULL)
  values <- unique(c(intersect(roled, values), values))
  list(attribute = attribute, values = .diffusion_colors(values, comps))
}

# Gives the nodes of a plot the colours of `.diffusion_fill()`, in place, and
# a key for every value, also a value that no node holds in that plot.
.recolour_diffusion <- function(p, dfill){
  fill <- p$scales$get_scales("fill")
  if (is.null(fill)) return(invisible(p))
  values <- dfill$values
  fill$palette <- function(n) values
  fill$limits <- names(values)
  fill$drop <- FALSE
  fill$name <- dfill$attribute
  for (l in p$layers)
    if ("fill" %in% names(l$mapping)) l$show.legend <- c(fill = TRUE)
  invisible(p)
}

# The compartments that a network keeps: in `info` for a stocnet, and as a
# graph attribute once it is coerced to an igraph or tidygraph.
.net_compartments <- function(x){
  if(inherits(x, "stocnet")) return(x$info$compartments)
  if(inherits(x, "igraph")) return(igraph::graph_attr(x, "compartments"))
  NULL
}

#' @rdname plot.diffusion
#' @param observed Optionally, an observed diffusion to draw over the
#'   simulated diffusions:
#'   a network with a record of a diffusion,
#'   as from `manynet::play_diffusion()`,
#'   or a 'diffusionModel', as from `manynet::as_diffusion()`.
#' @param level The share of the simulated diffusions that the band
#'   holds at each step. By default 0.95.
#' @details
#'   `plot.diffs_model()` draws the simulated diffusions,
#'   as from `infernet::play_diffusions()`.
#'   Without `observed`, it draws a smoothed line for each value.
#'   With `observed`, it draws the proportion of the nodes in the
#'   infectious value at each step:
#'   a band that holds the middle `level` of the simulated diffusions,
#'   a dashed line for their median,
#'   and the observed diffusion over them.
#'   This is the curve that `infernet::test_gof()` compares.
#'   A diffusion that ends before the others keeps its last value
#'   until the last step, as in `infernet::test_gof()`.
#' @returns `plot.diffs_model()` returns a line plot of the simulated
#'   diffusions, or, with `observed`, a band of the simulated diffusions
#'   with the observed diffusion over it.
#' @examples
#' plot(res_migraph_diff)
#' @export
plot.diffs_model <- function(x, ..., observed = NULL, level = 0.95){
  if(!is.null(observed)) return(.plot_diffs_observed(x, observed, level))
  S <- E <- I <- R <- n <- NULL # initialize variables to avoid CMD check notes
  data <- dplyr::tibble(x)
  # ggplot2::ggplot(data) + geom_smooth()
  p <- ggplot2::ggplot(data) + 
    # ggplot2::geom_point(ggplot2::aes(x = t, y = S/n))
    ggplot2::geom_smooth(ggplot2::aes(x = time, y = S/n, color = "A"), 
                         method = "loess", se=TRUE, level = .95, formula = 'y~x') +
    ggplot2::geom_smooth(ggplot2::aes(x = time, y = I/n, color = "C"), 
                         method = "loess", se=TRUE, level = .95, formula = 'y~x') +
    ag_theme_minimal() + 
    ggplot2::coord_cartesian(ylim = c(0,1)) + # using coord_cartesion to avoid printing warnings
    ggplot2::scale_x_continuous(breaks = function(x) pretty(x, n=6)) +
    ggplot2::ylab("Proportion") + ggplot2::xlab("Steps")
  labs <- c("Susceptible", "Infected")
  if(any(data$E>0)){
    p <- p +
      ggplot2::geom_smooth(ggplot2::aes(x = time, y = E/n, color = "B"), 
                           method = "loess", se=TRUE, level = .95, formula = 'y~x')
    labs <- c("Susceptible", "Exposed", "Infected")
  }
  if(any(data$R>0)){
    p <- p +
      ggplot2::geom_smooth(ggplot2::aes(x = time, y = R/n, color = "D"), 
                           method = "loess", se=TRUE, level = .95, formula = 'y~x')
    labs <- c(labs, "Recovered")
  }
  
  colvals <- structure(match_color(c("#4575b4","#E6AB02","#d73027","#66A61E")), 
            names = c("A","B","C","D"))
  p + ggplot2::scale_color_manual("Legend", 
                                  labels = labs,
                                  values = colvals,
                                  guide = "legend")
}

# The simulated diffusions as a band, with an observed diffusion over it.
.plot_diffs_observed <- function(x, observed, level = 0.95){
  time <- lower <- upper <- median <- prop <- NULL # avoid CMD check notes
  if(!is.numeric(level) || length(level) != 1 || is.na(level) ||
     level <= 0 || level >= 1)
    manynet::snet_abort(
      "{.arg level} must be one number between 0 and 1, such as 0.95,",
      "but {.val {level}} was given.")
  if(!inherits(observed, "diff_model")){
    if(inherits(observed, "diffs_model"))
      manynet::snet_abort(
        "{.arg observed} holds many diffusions, but it must hold one.",
        "To compare two models, use {.fn infernet::test_distributions}.")
    observed <- tryCatch(
      manynet::as_diffusion(observed,
                            attribute = .diffusion_attribute(observed)),
      error = function(e) manynet::snet_abort(
        "{.arg observed} must be a network with a record of a diffusion,",
        "or a {.cls diffusionModel}.",
        "Make one with {.fn manynet::play_diffusion}, as in",
        "{.code plot(diffs, observed = play_diffusion(net))}."))
  }
  obs <- as.data.frame(observed)
  sims <- as.data.frame(x)
  # The infectious value names the same column in both, as the simulations
  # come from the same model as the observed diffusion.
  comps <- .diffusion_roles(attr(observed, "compartments"),
                            .diffusion_values(obs))
  infectious <- if(length(comps$I)) comps$I[1] else "I"
  if(!infectious %in% names(obs) || !infectious %in% names(sims))
    manynet::snet_abort(
      "The diffusions have no column for the infectious value",
      "{.val {infectious}}, so there is no curve to compare.",
      "Simulate the diffusions with the same {.arg compartments}",
      "as the observed diffusion.")
  curve <- function(d){
    tcol <- if("time" %in% names(d)) "time" else "t"
    n <- if("n" %in% names(d)) d$n else
      rowSums(d[, .diffusion_values(d), drop = FALSE])
    d <- data.frame(time = as.numeric(d[[tcol]]), prop = d[[infectious]]/n)
    d[order(d$time), , drop = FALSE]
  }
  obs <- curve(obs)
  runs <- lapply(split(sims, sims$sim), curve)
  steps <- sort(unique(c(obs$time, unlist(lapply(runs, `[[`, "time")))))
  # A diffusion that ends early keeps its last value, as in test_gof().
  pad <- function(d) stats::approx(d$time, d$prop, xout = steps, method = "constant",
                                   rule = 2, f = 0)$y
  sim <- do.call(cbind, lapply(runs, pad))
  if(is.null(dim(sim))) sim <- matrix(sim, nrow = length(steps))
  probs <- c((1 - level)/2, 0.5, 1 - (1 - level)/2)
  qs <- t(apply(sim, 1, stats::quantile, probs = probs, names = FALSE))
  band <- data.frame(time = steps, lower = qs[, 1], median = qs[, 2],
                     upper = qs[, 3])
  obs <- data.frame(time = steps, prop = pad(obs))
  col <- .diffusion_colors(infectious, list(I = infectious))[[1]]
  sim_lab <- paste0("Simulated (", round(level * 100), "%)")
  ggplot2::ggplot(band, ggplot2::aes(x = time)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper,
                                      fill = sim_lab), alpha = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = median, linetype = "Simulated median"),
                       colour = ag_ink(), linewidth = 0.75) +
    ggplot2::geom_line(ggplot2::aes(y = prop, colour = "Observed"), data = obs,
                       linewidth = 1.25) +
    ggplot2::geom_point(ggplot2::aes(y = prop, colour = "Observed"), data = obs) +
    ggplot2::scale_fill_manual(NULL, values = stats::setNames(ag_missing(), sim_lab)) +
    ggplot2::scale_linetype_manual(NULL, values = c("Simulated median" = "dashed")) +
    ggplot2::scale_colour_manual(NULL, values = c("Observed" = col)) +
    ag_theme_minimal() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::scale_x_continuous(breaks = function(x) pretty(x, n = 6)) +
    ggplot2::ylab(paste("Proportion", if(identical(infectious, "I"))
      "infected" else paste("in", infectious))) +
    ggplot2::xlab("Steps")
}

#' @rdname plot.diffusion
#' @examples
#' plot(play_learning(ison_networkers, beliefs = runif(net_nodes(ison_networkers))))
#' @export
plot.learn_model <- function(x, ...){
  y <- t(x)
  colnames(y) <- paste0("t",0:(ncol(y)-1))
  y <- as.data.frame.table(y)
  y$Step <- as.numeric(gsub("t", "", y$Var2))
  ggplot2::ggplot(y, ggplot2::aes(x = Step, y = Freq, color = Var1)) + 
    ggplot2::geom_line(show.legend = FALSE) + ag_theme_minimal() +
    ggplot2::scale_color_manual(values = ag_qualitative(ncol(x))) +
    ggplot2::ylab("Belief")
}

#' @rdname plot.diffusion
#' @details
#'   `plot.diffusionModel()` draws a 'diffusionModel',
#'   as from `manynet::as_diffusion()`,
#'   as `plot.diff_model()` does.
#' @examples
#' set.seed(1)
#' plot(manynet::as_diffusion(manynet::play_diffusion(manynet::ison_adolescents,
#'   seeds = 1, transmissibility = 0.5)))
#' @export
plot.diffusionModel <- function(x, ..., all_steps = TRUE){
  plot.diff_model(x, ..., all_steps = all_steps)
}

#' Plotting the transitions of a diffusion
#' @name plot.transitions
#' @param x A 'diffusionTransitions' table, as `manynet::play_diffusion()`
#'   keeps in `info$transitions` of the network that it returns.
#' @param ... Other arguments to be passed.
#' @param compartments Optionally, the roles of the values,
#'   as a named list such as `list(S = "S", I = "I")`,
#'   as `manynet::play_diffusion()` keeps in `info$compartments`.
#'   The roles give the colours of the values.
#'   By default `NULL`, which reads the values S, E, I, R, and D
#'   as these roles,
#'   and otherwise reads the values that a contagion moves nodes from as S,
#'   and the values that it comes from as I.
#' @details
#'   `plot.diffusionTransitions()` draws the values of the attribute
#'   as circles in a row,
#'   and each transition as an arrow from one value to another,
#'   labelled with its rate.
#'   A transition to the next value in the row is straight,
#'   a transition back is curved under the row,
#'   and a transition that skips a value is curved over it.
#'   A transition by contagion gets a dashed arrow from each value
#'   that it comes from (its `source`),
#'   labelled with how a node is exposed:
#'   "threshold", "cascade", or "voter".
#'
#'   A rate that differs between nodes is labelled with its range,
#'   a rate from a node attribute with the name of the attribute,
#'   and a rate from a function with "f(x)".
#' @returns `plot.diffusionTransitions()` returns a diagram of the values
#'   and the transitions between them.
#' @examples
#' set.seed(1)
#' net <- manynet::play_diffusion(manynet::ison_adolescents, seeds = 1,
#'   transmissibility = 0.5, latency = 0.4, recovery = 0.2, waning = 0.05)
#' plot(net$info$transitions)
#' @export
plot.diffusionTransitions <- function(x, ..., compartments = NULL){
  px <- py <- group <- label <- kind <- NULL # avoid CMD check notes
  tr <- as.data.frame(x)
  if(!nrow(tr)){
    manynet::snet_warn("The table has no transitions, so there is nothing to plot.")
    return(invisible(NULL))
  }
  from <- as.character(tr$from)
  to <- as.character(tr$to)
  sources <- if("source" %in% names(tr)) .transition_sources(tr$source) else
    rep(list(character(0)), nrow(tr))
  # The values in the order of their roles, then in the order of the table
  states <- unique(c(from, to, unlist(sources)))
  if(is.null(compartments)){
    compartments <- .diffusion_roles(NULL, states)
    if(!length(compartments))
      compartments <- list(S = unique(from[lengths(sources) > 0]),
                           I = unique(unlist(sources)))
  } else compartments <- .diffusion_roles(compartments, states)
  roled <- unlist(compartments[intersect(c("S", "E", "I", "R", "D"),
                                         names(compartments))], use.names = FALSE)
  states <- unique(c(intersect(roled, states), states))
  pos <- stats::setNames(seq_along(states), states)
  fills <- .diffusion_colors(states, compartments)
  r <- 0.22
  # Points along a quadratic curve from (x1, 0) to (x2, y2) over (cx, cy),
  # without the parts inside the circles at its ends.
  bend <- function(x1, y1, x2, y2, cx, cy, trim_end = TRUE){
    t <- seq(0, 1, length.out = 60)
    bx <- (1 - t)^2 * x1 + 2 * (1 - t) * t * cx + t^2 * x2
    by <- (1 - t)^2 * y1 + 2 * (1 - t) * t * cy + t^2 * y2
    keep <- sqrt((bx - x1)^2 + (by - y1)^2) > r * 1.15
    if(trim_end) keep <- keep & sqrt((bx - x2)^2 + (by - y2)^2) > r * 1.25
    data.frame(px = bx[keep], py = by[keep])
  }
  paths <- list()
  labels <- list()
  mids <- list()
  seen <- character(0)
  for(i in seq_len(nrow(tr))){
    x1 <- pos[[from[i]]]
    x2 <- pos[[to[i]]]
    pair <- paste(from[i], to[i])
    k <- sum(seen == pair)
    seen <- c(seen, pair)
    gap <- x2 - x1
    h <- if(gap == 1) 0.35 * k else if(gap > 1) 0.3 * gap + 0.3 * k else
      -(0.3 * abs(gap) + 0.3 * k)
    if(gap == 0) h <- 0.6 + 0.3 * k
    cx <- (x1 + x2)/2 + if(gap == 0) 0.4 else 0
    path <- bend(x1, 0, x2, 0, cx, 2 * h)
    paths[[length(paths) + 1]] <- cbind(path, group = paste0("t", i),
                                        kind = "Transition")
    mid <- c((x1 + x2)/2 + if(gap == 0) 0.2 else 0, h)
    mids[[i]] <- mid
    labels[[length(labels) + 1]] <- data.frame(
      # A straight arrow has its label under it, where no contagion arrives
      px = mid[1], py = mid[2] + if(h <= 0) -0.1 else 0.1,
      label = .transition_rate(tr$rate[[i]]))
  }
  for(i in seq_len(nrow(tr))){
    for(s in sources[[i]]){
      if(!s %in% states) next
      xs <- pos[[s]]
      mid <- mids[[i]]
      top <- max(mid[2], 0) + 0.45 + 0.15 * abs(xs - mid[1])
      path <- bend(xs, 0, mid[1], mid[2] + 0.04, (xs + mid[1])/2, 2 * top,
                   trim_end = FALSE)
      path <- path[path$py > mid[2] + 0.06 | path$px == path$px[1], , drop = FALSE]
      paths[[length(paths) + 1]] <- cbind(path, group = paste0("c", i, s),
                                          kind = "Contagion")
      labels[[length(labels) + 1]] <- data.frame(
        px = (xs + mid[1])/2, py = top + 0.08,
        label = .transition_exposure(tr, i))
    }
  }
  paths <- do.call(rbind, paths)
  labels <- do.call(rbind, labels)
  angle <- seq(0, 2 * pi, length.out = 61)
  circles <- do.call(rbind, lapply(states, function(s)
    data.frame(px = pos[[s]] + r * cos(angle), py = r * sin(angle),
               group = s)))
  nodes <- data.frame(px = unname(pos), py = 0, label = states)
  inks <- ifelse(is_dark(fills[states]), "#FFFFFF", "#121212")
  ggplot2::ggplot() +
    ggplot2::geom_path(ggplot2::aes(x = px, y = py, group = group,
                                    linetype = kind),
                       data = paths, colour = ag_ink(), linewidth = 0.6,
                       arrow = ggplot2::arrow(length = ggplot2::unit(2.5, "mm"),
                                              type = "closed")) +
    ggplot2::geom_polygon(ggplot2::aes(x = px, y = py, group = group, fill = group),
                          data = circles, colour = ag_ink(), linewidth = 0.3,
                          show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(x = px, y = py, label = label),
                       data = nodes, colour = inks, family = ag_font(),
                       size = 4.5) +
    ggplot2::geom_text(ggplot2::aes(x = px, y = py, label = label),
                       data = labels, colour = ag_ink(), family = ag_font(),
                       size = 3.2) +
    ggplot2::scale_fill_manual(values = fills, breaks = states) +
    ggplot2::scale_linetype_manual(NULL, values = c(Transition = "solid",
                                                    Contagion = "dashed"),
                                   breaks = c("Transition", "Contagion")) +
    ggplot2::coord_fixed(clip = "off") +
    ag_theme_void() +
    ggplot2::theme(legend.position = "bottom")
}

# The values that each transition comes from by contagion, as a list of
# character vectors. A text source such as "E+I" names more than one.
.transition_sources <- function(source){
  if(!is.list(source)) source <- as.list(source)
  lapply(source, function(s){
    if(is.null(s) || !length(s)) return(character(0))
    s <- as.character(unlist(s))
    s <- s[!is.na(s) & nzchar(s)]
    unique(unlist(strsplit(s, "+", fixed = TRUE)))
  })
}

# The label of a rate: a number, the range of the rates of the nodes,
# the name of a node attribute, or "f(x)" for a function.
.transition_rate <- function(rate){
  if(is.list(rate) && length(rate) == 1 && !is.function(rate)) rate <- rate[[1]]
  if(is.function(rate)) return("f(x)")
  if(is.character(rate)) return(rate[1])
  rate <- as.numeric(rate)
  if(length(unique(rate)) == 1) return(format(signif(rate[1], 2)))
  paste(format(signif(min(rate), 2)), "to", format(signif(max(rate), 2)))
}

# The label of a contagion: how a node is exposed, with its threshold where
# it is not 1, and the prevalence where it counts.
.transition_exposure <- function(tr, i){
  exposure <- if("exposure" %in% names(tr)) as.character(tr$exposure[[i]]) else NA
  if(is.na(exposure)) exposure <- "threshold"
  out <- exposure
  if(identical(exposure, "threshold") && "threshold" %in% names(tr)){
    th <- tr$threshold[[i]]
    if(is.list(th)) th <- unlist(th)
    if(length(th) && !all(is.na(th)) && !identical(as.character(th[1]), "1"))
      out <- paste(out, .transition_rate(if(is.character(th)) suppressWarnings(
        if(anyNA(as.numeric(th))) th else as.numeric(th)) else th))
  }
  if("prevalence" %in% names(tr) && isTRUE(tr$prevalence[[i]] > 0))
    out <- paste(out, "+ prevalence")
  out
}

#' Plotting opinion models
#' @name plot.opinion
#' @param x An 'opinionModel', as from `manynet::as_opinion()`.
#' @param ... Other arguments to be passed.
#' @param stubbornness Optionally, the stubbornness of each node,
#'   as one number or one number for each node, from 0 to 1.
#'   The nodes with a stubbornness above 0 are drawn with a thicker line,
#'   and a square at the last step.
#'   By default `NULL`, which marks no node.
#' @details
#'   `plot.opinionModel()` draws one line for each node,
#'   from its first opinion at step 0 to its opinion at the last step.
#'   The colour of each line is the first opinion of its node,
#'   on the sequential palette of the theme,
#'   so that the colours meet where the opinions converge.
#'   Where all the nodes end at one opinion, a dashed line marks it.
#'
#'   `plot()` of a network with a record of opinions,
#'   as from `manynet::play_opinion()`,
#'   draws the same plot.
#'   It marks the stubborn nodes where the network has a node attribute
#'   named "stubbornness",
#'   as when `manynet::play_opinion(stubbornness = "stubbornness")` is used.
#' @returns `plot.opinionModel()` returns a line plot of the opinion of each
#'   node at each step.
#' @examples
#' set.seed(1)
#' net <- manynet::add_node_attribute(manynet::ison_networkers, "stubbornness",
#'   c(1, 0.5, rep(0, 30)))
#' opined <- manynet::play_opinion(net, opinions = stats::runif(32),
#'   stubbornness = "stubbornness")
#' plot(opined)
#' plot(manynet::as_opinion(opined))
#' @export
plot.opinionModel <- function(x, ..., stubbornness = NULL){
  step <- opinion <- start <- node <- stubborn <- limit <- NULL # avoid CMD check notes
  y <- as.matrix(as.data.frame(x))
  n <- ncol(y)
  if(!nrow(y) || !n){
    manynet::snet_warn("The opinion model is empty, so there is nothing to plot.")
    return(invisible(NULL))
  }
  if(is.null(stubbornness)) stubbornness <- 0
  if(!is.numeric(stubbornness) || !length(stubbornness) %in% c(1, n) ||
     anyNA(stubbornness))
    manynet::snet_abort(
      "{.arg stubbornness} must be one number or {n} numbers, one for each node,",
      "but a value of length {length(stubbornness)} was given.")
  stubbornness <- rep_len(stubbornness, n)
  nodes <- colnames(y)
  if(is.null(nodes)) nodes <- as.character(seq_len(n))
  long <- data.frame(step = rep(seq_len(nrow(y)) - 1, times = n),
                     node = factor(rep(nodes, each = nrow(y)), levels = nodes),
                     opinion = as.vector(y),
                     start = rep(y[1, ], each = nrow(y)),
                     stubborn = rep(stubbornness > 0, each = nrow(y)))
  p <- ggplot2::ggplot(long, ggplot2::aes(x = step, y = opinion, group = node,
                                          colour = start)) +
    ggplot2::geom_line(data = long[!long$stubborn, , drop = FALSE],
                       linewidth = 0.6, alpha = 0.8)
  # The last opinions, where all the nodes hold one opinion, are the limit.
  last <- y[nrow(y), ]
  spread <- diff(range(y, na.rm = TRUE))
  if(nrow(y) > 1 && all(is.finite(last)) &&
     diff(range(last)) <= 1e-3 * max(spread, 1e-8))
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept = limit,
                                              linetype = "Consensus"),
                                 data = data.frame(limit = mean(last)),
                                 colour = ag_ink(), inherit.aes = FALSE) +
      ggplot2::scale_linetype_manual(NULL, values = c("Consensus" = "dashed"))
  if(any(stubbornness > 0)){
    held <- long[long$stubborn, , drop = FALSE]
    ends <- held[held$step == max(held$step), , drop = FALSE]
    # An outline in ink sets the stubborn nodes apart from the others,
    # whatever their colour on the gradient.
    p <- p + ggplot2::geom_line(data = held, linewidth = 2.4, colour = ag_ink()) +
      ggplot2::geom_line(data = held, linewidth = 1.2) +
      ggplot2::geom_point(ggplot2::aes(shape = "Stubborn"), data = ends,
                          size = 3, colour = ag_ink()) +
      ggplot2::scale_shape_manual(NULL, values = c("Stubborn" = 15))
  }
  p + ggplot2::scale_colour_gradientn("First opinion", colours = ag_sequential(9)) +
    ag_theme_minimal() +
    ggplot2::scale_x_continuous(breaks = function(x) pretty(x, n = 6)) +
    ggplot2::ylab("Opinion") + ggplot2::xlab("Steps")
}

# A network with a record of opinions, as from manynet::play_opinion(): its
# changes are to a numeric node attribute named "opinions".
.is_opinion <- function(x){
  if(!manynet::is_manynet(x) ||
     !"opinions" %in% manynet::net_node_attributes(x)) return(FALSE)
  changes <- manynet::as_changelist(x)
  !is.null(changes) && "var" %in% names(changes) &&
    "opinions" %in% changes$var &&
    is.numeric(manynet::node_attribute(x, "opinions"))
}

.plot_opinion_network <- function(x, ...){
  dots <- list(...)
  if(is.null(dots$stubbornness) &&
     "stubbornness" %in% manynet::net_node_attributes(x))
    dots$stubbornness <- as.numeric(manynet::node_attribute(x, "stubbornness"))
  do.call(plot.opinionModel, c(list(manynet::as_opinion(x, "opinions")), dots))
}

#' Plotting mobility models
#' @name plot.mobility
#' @param x A 'mobilityModel', as from `manynet::as_mobility()`.
#' @param ... Other arguments to be passed.
#' @details
#'   `plot.mobilityModel()` draws the number of dissatisfied occupants
#'   after each move.
#'   A 'mobilityModel' alone does not hold the start,
#'   so its line begins at the first move.
#'
#'   `plot()` of a network of places with a record of moves,
#'   as from `manynet::play_mobility()`, draws more:
#'   a grid with a row for each step, from the start at step 0,
#'   and a column for each place.
#'   Each tile is filled by the attribute of its occupant,
#'   and an empty place is left blank.
#'   A cross marks a dissatisfied occupant,
#'   and an outline marks the place that an occupant moved to at that step.
#'   In the margin, a bar gives the number of dissatisfied occupants
#'   at each step.
#'
#'   To draw the places at the start, the middle, and the last move,
#'   use `graphs()` on the network of places.
#' @returns `plot.mobilityModel()` returns a line plot of the number of
#'   dissatisfied occupants at each step.
#'   `plot()` of a network of places returns a grid of the occupants of the
#'   places at each step, with a bar plot of the dissatisfied occupants.
#' @examples
#' places <- manynet::add_node_attribute(manynet::create_lattice(25), "group",
#'   rep(c(0, 1, NA, 1, 0), 5))
#' moved <- manynet::play_mobility(places, "group")
#' plot(moved)
#' plot(manynet::as_mobility(moved))
#' graphs(moved)
#' @export
plot.mobilityModel <- function(x, ...){
  time <- dissatisfied <- NULL # initialize variables to avoid CMD check notes
  data <- data.frame(time = as.numeric(x$time),
                     dissatisfied = as.numeric(x$dissatisfied))
  start <- attr(x, "start")
  if(!is.null(start))
    data <- rbind(data.frame(time = 0, dissatisfied = as.numeric(start)), data)
  if(!nrow(data) || all(is.na(data$dissatisfied))){
    if(!nrow(x)) manynet::snet_warn(
      "No occupant moved, so there is nothing to plot.",
      "Check that some places are empty and that some occupants are dissatisfied.") else
        manynet::snet_warn(
          "The moves have no record of {.var satisfied}, so there is nothing to plot.",
          "Use a network from {.fn manynet::play_mobility}.")
    return(invisible(NULL))
  }
  ggplot2::ggplot(data, ggplot2::aes(x = time, y = dissatisfied)) +
    ggplot2::geom_line(linewidth = 1.25, colour = ag_qualitative(1)) +
    ggplot2::geom_point(colour = ag_qualitative(1)) +
    ag_theme_minimal() +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(breaks = function(x) pretty(x, n = 6)) +
    ggplot2::ylab("Dissatisfied occupants") + ggplot2::xlab("Steps")
}

# A network of places with a record of moves, as from manynet::play_mobility():
# it has an `occupant` node attribute, and changes to it.
.is_mobility <- function(x){
  if(!manynet::is_manynet(x) || inherits(x, "mobilityModel") ||
     !"occupant" %in% manynet::net_node_attributes(x)) return(FALSE)
  changes <- manynet::as_changelist(x)
  !is.null(changes) && "var" %in% names(changes) &&
    "occupant" %in% changes$var
}

# The attribute of the occupants: kept in `info$mobility` of a stocnet, else
# the first attribute that moves with the occupants.
.mobility_attribute <- function(x){
  if(inherits(x, "stocnet") && !is.null(x$info$mobility$attribute))
    return(x$info$mobility$attribute)
  vars <- unique(as.character(manynet::as_changelist(x)$var))
  vars <- setdiff(vars, c("occupant", "satisfied", "active"))
  if(length(vars)) vars[1] else "occupant"
}

# The 'mobilityModel' of a network of places, with the number of dissatisfied
# occupants at the start, which the model alone does not hold.
.as_mobility <- function(x){
  out <- manynet::as_mobility(x, .mobility_attribute(x))
  satisfied <- manynet::as_stocnet(x)$nodes$satisfied
  if(!is.null(satisfied))
    attr(out, "start") <- sum(!as.logical(satisfied), na.rm = TRUE)
  out
}

# The occupants of each place at each step, as a grid, with the number of
# dissatisfied occupants at each step in the margin.
.plot_mobility_grid <- function(x){
  place <- step <- group <- dissatisfied <- NULL # avoid CMD check notes
  model <- .as_mobility(x)
  if(!nrow(model)) return(plot(model))
  attribute <- .mobility_attribute(x)
  last <- max(model$time)
  grid <- do.call(rbind, lapply(0:last, function(t){
    g <- manynet::to_time(x, t)
    satisfied <- if("satisfied" %in% manynet::net_node_attributes(g))
      as.logical(manynet::node_attribute(g, "satisfied")) else
        rep(NA, manynet::net_nodes(g))
    data.frame(step = t, place = seq_len(manynet::net_nodes(g)),
               group = manynet::node_attribute(g, attribute),
               dissatisfied = !satisfied)
  }))
  continuous <- .is_continuous(grid$group)
  if(!continuous) grid$group <- factor(grid$group)
  moves <- data.frame(step = as.numeric(model$time),
                      place = as.numeric(model$destination))
  moves <- moves[!is.na(moves$place), , drop = FALSE]
  steps <- function(x) pretty(x, n = min(last + 1, 8))
  grid_plot <- ggplot2::ggplot(grid, ggplot2::aes(x = place, y = step)) +
    ggplot2::geom_tile(ggplot2::aes(fill = group), colour = ag_ground_fill(),
                       linewidth = 0.4) +
    ggplot2::geom_point(data = grid[grid$dissatisfied %in% TRUE, , drop = FALSE],
                        shape = 4, size = 1.4, colour = ag_ink()) +
    ggplot2::geom_tile(data = moves, fill = NA, colour = ag_ink(),
                       linewidth = 0.8) +
    (if(continuous)
      ggplot2::scale_fill_gradientn(attribute, colours = ag_sequential(9),
                                    na.value = ag_ground_fill()) else
      ggplot2::scale_fill_manual(attribute,
                                 values = ag_qualitative(nlevels(grid$group)),
                                 na.value = ag_ground_fill(),
                                 na.translate = FALSE)) +
    ggplot2::scale_y_reverse(breaks = steps, expand = c(0, 0)) +
    ggplot2::scale_x_continuous(breaks = function(x) pretty(x, n = 6),
                                expand = c(0, 0)) +
    ag_theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::xlab("Places") + ggplot2::ylab("Steps")
  counts <- data.frame(step = as.numeric(model$time),
                       dissatisfied = as.numeric(model$dissatisfied))
  start <- attr(model, "start")
  if(!is.null(start))
    counts <- rbind(data.frame(step = 0, dissatisfied = as.numeric(start)), counts)
  margin_plot <- ggplot2::ggplot(counts, ggplot2::aes(x = dissatisfied, y = step)) +
    ggplot2::geom_col(orientation = "y", fill = ag_missing(), width = 0.8) +
    ggplot2::scale_y_reverse(breaks = NULL, expand = c(0, 0),
                             limits = c(last + 0.5, -0.5)) +
    ggplot2::scale_x_continuous(breaks = function(x) pretty(x, n = 3)) +
    ag_theme_minimal() +
    ggplot2::xlab("Dissatisfied") + ggplot2::ylab(NULL)
  patchwork::wrap_plots(grid_plot, margin_plot, widths = c(5, 1))
}

# The network of places at some steps, for `graphs()`. `waves` select the steps
# as they select networks in a list: the first is the start, step 0.
.graphs_mobility <- function(x, waves, based_on, ...){
  changes <- manynet::as_changelist(x)
  last <- max(c(0, changes$time[changes$var == "occupant"]))
  steps <- 0:last
  if(missing(waves)) at <- unique(c(0, round(last/2), last)) else {
    if(!is.numeric(waves) || any(is.na(waves)))
      manynet::snet_abort(
        "{.arg waves} should be the number of steps to plot, or a vector of",
        "which steps to plot, but a value of class {.cls {class(waves)}}",
        "was given.")
    if(any(waves < 1) || any(waves > length(steps)))
      manynet::snet_abort(
        "{.arg waves} should be between 1 (the start) and {length(steps)}",
        "(the last move), but {.val {waves}} was given.")
    at <- if(length(waves) == 1) steps[seq_len(waves)] else steps[waves]
  }
  nets <- lapply(at, function(t) manynet::to_time(x, t))
  names(nets) <- paste("Step", at)
  dots <- list(...)
  if(!any(c("node_color", "node_colour") %in% names(dots)))
    dots$node_color <- .mobility_attribute(x)
  p <- do.call(graphs, c(list(nets, waves = seq_along(nets),
                              based_on = based_on), dots))
  # The empty places are drawn blank
  for(i in seq_along(nets)){
    fill <- p[[i]]$scales$get_scales("fill")
    if(!is.null(fill)) fill$na.value <- "white"
  }
  p
}

#' #' Plot agreements network
#' #' 
#' #' @description Facilitates plotting of 'many' data.
#' #' @param dataset A dataset from one of the many packages
#' #' or a "consolidated" database.
#' #' @param actor An actor variable.
#' #' "stateID", by default.
#' #' @param treaty_type The type of treaties to be returned.
#' #' NULL, by default.
#' #' Other options are "bilateral" or "multilateral".
#' #' @param key An ID column to collapse by.
#' #' By default "manyID".
#' #' @param layout How do you want the plot to look like?
#' #' An `{ggraph}` layout algorithm.
#' #' If not declared, reasonable defaults are used.
#' #' @name plot_
#' NULL
#' 
#' #' @rdname plot_
#' #' @importFrom dplyr select mutate distinct rename
#' #' @return A network of agreements' relations.
#' #' @examples
#' #' \donttest{
#' #' #agreements <- dplyr::filter(manyenviron::agreements$ECOLEX,
#' #' #Begin > "2000-01-01" & Begin < "2000-12-12")
#' #' #plot_agreements(agreements)
#' #'}
#' #' @export
#' plot_agreements <- function(dataset, treaty_type = NULL, key = "manyID",
#'                             layout = "circle") {
#'   manyID <- treatyID <- name <- NULL
#'   if (key == "manyID") {
#'     out <- dplyr::select(dataset, manyID) |>
#'       dplyr::rename(key = manyID) |>
#'       dplyr::distinct()
#'   } else if (key == "treatyID") {
#'     out <- dplyr::select(dataset, treatyID) |>
#'       dplyr::rename(key == treatyID) |>
#'       dplyr::distinct()
#'   } else cli::cli_abort("Please declare either 'manyID' or 'treatyID'.")
#'   if (!is.null(treaty_type)) {
#'     if (treaty_type == "bilateral") {
#'       out <- out[grep("-", out$key),]
#'     }
#'     if (treaty_type == "multilateral") {
#'       out <- out[grep("-", out$key, invert = TRUE),]
#'     }
#'   }
#'   dplyr::mutate(out,
#'                 link = ifelse(grepl(":", key), sapply(strsplit(key, ":"),
#'                                                       "[", 2), "NA"),
#'                 key = gsub("\\:.*", "", key)) |>
#'     as_tidygraph() |>
#'     dplyr::filter(name != "NA") |>
#'     graphr(layout = layout)
#' }
#' 
#' #' @rdname plot_
#' #' @importFrom dplyr select distinct all_of rename
#' #' @return A network of agreements' memberships.
#' #' @examples
#' #' \donttest{
#' #' #memberships <- dplyr::filter(manyenviron::memberships$ECOLEX_MEM,
#' #' #Begin > "2000-01-01" & Begin < "2000-01-31")
#' #' #plot_memberships(memberships)
#' #'}
#' #' @export
#' plot_memberships <- function(dataset, actor = "stateID", treaty_type = NULL,
#'                             key = "manyID", layout = "bipartite") {
#'   manyID <- treatyID <- name <- NULL
#'   if (key == "manyID") {
#'     out <- dplyr::select(dataset, manyID, dplyr::all_of(actor)) |>
#'       dplyr::rename(key = manyID) |>
#'       dplyr::distinct()
#'   } else if (key == "treatyID") {
#'     out <- dplyr::select(dataset, treatyID, dplyr::all_of(actor)) |>
#'       dplyr::rename(key == treatyID) |>
#'       dplyr::distinct()
#'   } else cli::cli_abort("Please declare either 'manyID' or 'treatyID'.")
#'   if (!is.null(treaty_type)) {
#'     if (treaty_type == "bilateral") {
#'       out <- out[grep("-", out$key),]
#'     }
#'     if (treaty_type == "multilateral") {
#'       out <- out[grep("-", out$key, invert = TRUE),]
#'     }
#'   }
#'   stats::na.omit(out) |>
#'     as_tidygraph() |>
#'     mutate(type = ifelse(grepl("[0-9][0-9][0-9][0-9][A-Za-z]",
#'                                         name), TRUE, FALSE)) |>
#'     graphr(layout = layout)
#' }
#' 
#' #' @rdname plot_
#' #' @importFrom dplyr select mutate distinct filter rename
#' #' @return A plot of agreements' lineages.
#' #' @examples
#' #' \donttest{
#' #' #lineage <- dplyr::filter(manyenviron::agreements$HUGGO,
#' #' #Begin > "2000-01-01", Begin < "2001-12-31")
#' #' #plot_lineage(lineage)
#' #' }
#' #' @export
#' plot_lineage <- function(dataset, treaty_type = NULL, key = "manyID",
#'                          layout = "nicely") {
#'   manyID <- treatyID <- name <- NULL
#'   if (key == "manyID") {
#'     out <- dplyr::select(dataset, manyID) |>
#'       dplyr::rename(key = manyID) |>
#'       dplyr::distinct()
#'   } else if (key == "treatyID") {
#'     out <- dplyr::select(dataset, treatyID) |>
#'       dplyr::rename(key == treatyID) |>
#'       dplyr::distinct()
#'   } else cli::cli_abort("Please declare either 'manyID' or 'treatyID'.")
#'   if (!is.null(treaty_type)) {
#'     if (treaty_type == "bilateral") {
#'       out <- out[grep("-", out$key),]
#'     }
#'     if (treaty_type == "multilateral") {
#'       out <- out[grep("-", out$key, invert = TRUE),]
#'     }
#'   }
#'   out |>
#'     dplyr::filter(grepl(":", key)) |>
#'     dplyr::mutate(key1 = gsub(".*\\:", "", key),
#'                   key = gsub("\\:.*", "", key)) |>
#'     dplyr::distinct() |>
#'     as_tidygraph() |>
#'     graphr(layout = "nicely")
#' } 
#' 
#' #' @rdname plot_
#' #' @param date String date from the network snapshot.
#' #' Used by \code{{cshapes}} to plot the correct map.
#' #' By default, 2019-12-31.
#' #' Date can be between 1886-01-01 and 2019-12-31.
#' #' @param theme Theme you would like to use to plot the graph.
#' #' bey defalt, "light".
#' #' Available themes are "light", "dark", and "earth".
#' #' @details `plot_map()` creates a plot of the a unimodal geographical network 
#' #' at a single point in time.
#' #' @importFrom dplyr mutate inner_join rename filter
#' #' @return A map of a country level geographical network.
#' #' @examples
#' #' \donttest{
#' #' #memberships <- dplyr::filter(manyenviron::memberships$ECOLEX_MEM,
#' #' #Begin > "2000-01-01" & Begin < "2000-12-12")
#' #' #plot_map(memberships, actor = "stateID") +
#' #' #ggplot2::labs(title = "Bilateral International Environmental Treaties Signed in the year 2000",
#' #' #subtitle = "Ecolex data")
#' #'}
#' #' @export
#' plot_map <- function(dataset, actor = "stateID", treaty_type = NULL,
#'                      date = "2019-12-31", theme = "light") {
#'   # check packages
#'   manynet::snet_requires("cshapes")
#'   manynet::snet_requires("manydata")
#'   # Checks for correct input
#'   weight <- .data <- NULL
#'   # Step 1: get membership list
#'   dataset <- manydata::call_treaties(dataset = dataset, actor = actor,
#'                                      treaty_type = treaty_type)
#'   # Step 2: set up empty matrix
#'   actor <- unique(unlist(strsplit(dataset$Memberships, ", ")))
#'   out <- matrix(0, nrow = length(actor), ncol = length(actor))
#'   rownames(out) <- actor
#'   colnames(out) <- actor
#'   # Step 3: fill matrix with values
#'   for (k in colnames(out)) {
#'     m <- data.frame(table(unlist(strsplit(grep(k, dataset$Memberships,
#'                                                value = TRUE), ", "))))
#'     m <- m[!(m$Var1 %in% k),]
#'     out[k, ] <- ifelse(names(out[k,]) %in% m$Var1 == TRUE, m$Freq/100, out[k,])
#'   }
#'   out  <- igraph::get.data.frame(igraph::graph.adjacency(out, weighted = TRUE))
#'   # Step 4 = get theme
#'   if (theme == "dark") {
#'     maptheme <- maptheme(palette = c("#FFFAFA", "#596673"))
#'     countrycolor <- "#FFFAFA"
#'   }
#'   if (theme == "earth") {
#'     maptheme <- maptheme(palette = c("#79B52F", "#4259FD"))
#'     countrycolor <- "#79B52F"
#'   }
#'   if (theme == "light") {
#'     maptheme <- maptheme(palette = c("#596673", "#FFFAFA"))
#'     countrycolor <- "#596673"
#'   }
#'   # Step 5: import the historical shapefile data
#'   cshapes <- cshapes::cshp(as.Date(date), useGW = FALSE)
#'   coment <- vapply(countryregex[, 3], # add stateID abbreviations
#'                    function(x) grepl(x, cshapes$country_name,
#'                                      ignore.case = TRUE, perl = TRUE) * 1,
#'                    FUN.VALUE = double(length(cshapes$country_name)))
#'   colnames(coment) <- countryregex[, 1]
#'   rownames(coment) <- cshapes$country_name
#'   ab <- apply(coment, 1, function(x) paste(names(x[x == 1]),
#'                                            collapse = "_"))
#'   ab[ab == ""] <- NA
#'   cshapes <- dplyr::mutate(cshapes, stateID = unname(ab))
#'   # Step 6: create edges with from/to lat/long
#'   edges <- out |>
#'     dplyr::inner_join(cshapes, by = c("from" = "stateID")) |>
#'     dplyr::rename(x = .data$caplong, y = .data$caplat) |>
#'     dplyr::inner_join(cshapes, by = c("to" = "stateID")) |>
#'     dplyr::rename(xend = .data$caplong, yend = .data$caplat)
#'   # Step 7: Create plotted network from computed edges
#'   g <- as_tidygraph(edges)
#'   # Step 8: Get the country shapes from the edges dataframe
#'   country_shapes <- ggplot2::geom_sf(data = cshapes$geometry,
#'                                      fill = countrycolor)
#'   # Step 9: generate the point coordinates for capitals
#'   cshapes_pos <- cshapes |>
#'     dplyr::filter(.data$stateID %in% node_names(g)) |>
#'     dplyr::rename(x = .data$caplong, y = .data$caplat)
#'   # Reorder things according to nodes in plotted network g
#'   cshapes_pos <- cshapes_pos[match(node_names(g),
#'                                    cshapes_pos[["stateID"]]), ]
#'   # Step 10: generate the layout
#'   lay <- ggraph::create_layout(g, layout = cshapes_pos)
#'   edges$circular <- rep(FALSE, nrow(edges))
#'   edges$edge.id <- rep(1, nrow(edges))
#'   # Step 11: plot
#'   ggraph::ggraph(lay) +
#'     country_shapes +
#'     ggraph::geom_edge_arc(data = edges, ggplot2::aes(edge_width = weight),
#'                           strength = 0.33, alpha = 0.25) +
#'     ggraph::scale_edge_width_continuous(range = c(0.5, 2), # scales edge widths
#'                                         guide = "none") +
#'     ggraph::geom_node_point(shape = 21, # draw nodes
#'                             fill = "white", color = "black", stroke = 0.5) +
#'     ggraph::geom_node_text(ggplot2::aes(label = node_names(g)),
#'                            repel = TRUE, size = 3, color = "white",
#'                            fontface = "bold") +
#'     maptheme
#' }
#' 
#' # Helper function providing the network map function with a few map themes.
#' maptheme <- function(palette = c("#FFFAFA", "#596673")) {
#'   oceancolor <- palette[2]
#'   titlecolor <- ifelse(is_dark(palette[2]), "white", "black")
#'   # Create map theme
#'   maptheme <- ggplot2::theme(panel.grid = ggplot2::element_blank()) +
#'     ggplot2::theme(axis.text = ggplot2::element_blank()) +
#'     ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
#'     ggplot2::theme(axis.title = ggplot2::element_blank()) +
#'     ggplot2::theme(legend.position = "bottom") +
#'     ggplot2::theme(panel.grid = ggplot2::element_blank()) +
#'     ggplot2::theme(panel.background = ggplot2::element_blank()) +
#'     ggplot2::theme(plot.background = ggplot2::element_rect(fill = oceancolor)) +
#'     ggplot2::theme(plot.title = ggplot2::element_text(color = titlecolor,
#'                                                       hjust = 0.1, vjust = 0.1),
#'                    plot.subtitle = ggplot2::element_text(color = titlecolor,
#'                                                          hjust = 0.065,
#'                                                          vjust = 0.1),
#'                    plot.caption = ggplot2::element_text(color = titlecolor, hjust = 0.96)) +
#'     ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0.5, 0), "cm"))
#'   # This function returns a map theme for ggplot
#'   maptheme
#' }
#' 

