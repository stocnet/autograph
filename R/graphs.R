# Longitudinal or comparative networks ####

#' Easily graph a set of networks with sensible defaults
#' 
#' @description 
#'   This function provides users with an easy way to graph
#'   lists of network data for comparison.
#'   
#'   It builds upon this package's `graphr()` function, 
#'   and inherits all the same features and arguments.
#'   See `graphr()` for more.
#'   However, it uses the `{patchwork}` package to plot the graphs
#'   side by side and, if necessary, in successive rows.
#'   This is useful for lists of networks that represent, for example, 
#'   ego or component subgraphs of a network,
#'   or a list of a network's different types of tie or across time.
#'   By default just the first and last network will be plotted,
#'   but this can be overridden by the "waves" parameter.
#'   
#'   Where the graphs are of the same network (same nodes),
#'   the graphs may share a layout to facilitate comparison.
#'   By default, successive graphs will use the layout calculated for 
#'   the "first" network, but other options include the "last" layout,
#'   or a mix, "both", of them.
#' @family mapping
#' @param netlist A list of manynet-compatible networks.
#'   This can also be a single manynet network object that encodes time,
#'   which will be split automatically (as in `grapht()`):
#'   longitudinal or changing networks are split into waves via
#'   `manynet::to_waves()`; dynamic (time-stamped, event-based) networks
#'   such as `manynet::irps_nuclear` into cumulative time slices via
#'   `manynet::to_slices()`; and interval (spell) networks that record tie
#'   `begin`/`end` lifespans, such as `manynet::irps_wwi`, into one snapshot
#'   per change point. It can also be a diffusion model result from e.g.
#'   `manynet::play_diffusion()`.
#' @param waves Numeric, the number of plots to be displayed side-by-side.
#'   If missing, the number of plots will be reduced to the first and last
#'   when there are more than four plots.
#'   This argument can also be passed a vector selecting the waves to plot.
#' @param based_on Whether the layout of the joint plots should
#'   be based on the "first" or the "last" network, or "both".
#' @param ... Additional arguments passed to `graphr()`.
#' @return Multiple `ggplot2::ggplot()` objects displayed side-by-side.
#' @name plot_graphs
#' @examples
#' #graphs(to_egos(ison_adolescents))
#' #graphs(to_egos(ison_adolescents), waves = 8)
#' #graphs(to_egos(ison_adolescents), waves = c(2, 4, 6))
#' #graphs(play_diffusion(ison_adolescents))
#' @export
graphs <- function(netlist, waves,
                   based_on = c("first", "last", "both"), ...) {
  based_on <- .check_choice(based_on, c("first", "last", "both"), "based_on")
  # A single manynet network that encodes time is split into a list of
  # snapshots, mirroring grapht()'s handling (see .split_time_network()):
  # longitudinal/changing networks (and diffusion results) into waves,
  # spell (begin/end) networks into per-period snapshots, and other dynamic
  # (event) networks into cumulative slices. Splitting is why a bare
  # longitudinal or dynamic network can be passed directly; without it the
  # raw graph object would be iterated over and crash later.
  if (!manynet::is_list(netlist) &&
      (manynet::is_manynet(netlist) || inherits(netlist, "diff_model"))) {
    netlist <- .split_time_network(netlist)
  }
  if (missing(waves)) {
    if (length(netlist) > 4) {
      n_waves <- length(netlist)
      netlist <- netlist[c(1, n_waves)]
      manynet::snet_info(
        "Plotting the first and last of {n_waves} networks side-by-side.",
        "To choose which to plot, use the {.arg waves} argument, as in",
        "{.code waves = 4} for the first four, or {.code waves = c(1, 3, 5)}.")
    }
  } else if (!missing(waves)) {
    # Out-of-range indices would otherwise give NULL entries here and fail much
    # later, inside patchwork, with no hint that `waves` was the problem.
    if (!is.numeric(waves) || any(is.na(waves)))
      manynet::snet_abort(
        "{.arg waves} should be the number of networks to plot, or a vector of",
        "which networks to plot, but a value of class {.cls {class(waves)}}",
        "was given.")
    n_waves <- length(netlist)
    if (any(waves < 1) || any(waves > n_waves))
      manynet::snet_abort(
        "{.arg waves} should be between 1 and {n_waves}, the number of networks",
        "available, but {.val {waves}} was given.")
    if (length(waves) == 1) netlist <- netlist[c(1:waves)] else
      netlist <- netlist[waves]
  }
  if (is.null(names(netlist))) names(netlist) <- rep("", length(netlist))
  # Each panel is a plot of its own, so each would otherwise scale its
  # aesthetics against its own network alone. The ranges and categories found
  # across the whole list are worked out once here and passed down to every
  # panel, so that `patchwork` can collect the guides into one legend and the
  # same value is drawn the same way in each panel. See `.shared_aes()`.
  shared <- .shared_aes_from_dots(netlist, list(...))
  if (length(unique(lapply(netlist, length))) == 1) {
    # Sharing a layout requires every panel to draw every node, so isolates
    # are kept unless the user explicitly asks otherwise
    dots <- list(...)
    dots$.shared <- shared
    if (!"isolates" %in% names(dots)) dots$isolates <- "keep"
    # Every panel draws the same nodes, so which of them to label is settled
    # once here, against the network the layout is based on, and passed down as
    # names. Left to each panel, `graphr()` would rank the nodes of each network
    # separately and the labels would jump from panel to panel.
    ref <- manynet::as_tidygraph(
      netlist[[if (based_on == "last") length(netlist) else 1]])
    if (manynet::is_labelled(ref)) {
      labels_given <- "labels" %in% names(dots)
      lab <- .check_labels(ref, if (labels_given) dots$labels else TRUE)
      n_ref <- as.numeric(manynet::net_nodes(ref))
      if (isTRUE(lab) && !labels_given && n_ref > 30)
        lab <- structure(5L, criterion = "degree", automatic = TRUE)
      if (!isTRUE(lab) && !isFALSE(lab)) {
        dots$labels <- manynet::node_names(ref)[.infer_labels(ref, lab)]
        n_lab <- length(dots$labels)
        if (!labels_given) manynet::snet_info(
          "Labelling the {n_lab} most central of {n_ref} nodes in each panel.",
          "Use {.code labels = TRUE} to label all of them,",
          "{.code labels = 25} to label more,",
          "or {.code labels = FALSE} for none.")
      }
    }
    shared_graphr <- function(net, extra = NULL)
      do.call(graphr, c(list(net), dots, extra))
    if (based_on == "first") {
      lay <- shared_graphr(netlist[[1]])
      x <- lay$data$x
      y <- lay$data$y
    } else if (based_on == "last") {
      lay <- shared_graphr(netlist[[length(netlist)]])
      x <- lay$data$x
      y <- lay$data$y
    } else if (based_on == "both") {
      lay <- shared_graphr(netlist[[1]])
      x1 <- lay$data$x
      y1 <- lay$data$y
      lay1 <- shared_graphr(netlist[[length(netlist)]])
      x <- (lay1$data$x + x1)/2
      y <- (lay1$data$y + y1)/2
    }
    gs <- lapply(1:length(netlist), function(i)
      shared_graphr(netlist[[i]], list(x = x, y = y)) +
        ggtitle(names(netlist)[i]))
  } else {
    thisRequires("methods")
    if (!methods::hasArg("layout") & is_ego_network(netlist)) {
      gs <- lapply(1:length(netlist), function(i)
        graphr(netlist[[i]], layout = "star", center = names(netlist)[[i]],
               .shared = shared, ...) +
          ggtitle(names(netlist)[i]))
    } else {
      manynet::snet_info(
        "Giving each network its own layout, since not all nodes appear in",
        "every one of them, so a shared layout would place them differently.")
      gs <- lapply(1:length(netlist), function(i)
        graphr(netlist[[i]], .shared = shared, ...) + ggtitle(names(netlist)[i]))
    }
  }
  do.call(patchwork::wrap_plots, c(gs, list(guides = "collect")))
}

# `graphs()` helper functions

# The aesthetic arguments reach `graphs()` through `...`, where they are values
# rather than the expressions `graphr()` reads with `substitute()`, and either
# spelling of the two colour arguments may be used. Pulled out here so that
# `.shared_aes()` is given the same argument each panel will be drawn with.
.shared_aes_from_dots <- function(netlist, dots) {
  pick <- function(...) {
    nms <- c(...)
    for (nm in nms) if (nm %in% names(dots)) {
      out <- dots[[nm]]
      # A colour or a size given outright ("red", 3) maps nothing, and only an
      # attribute name can be resolved against every network in the list.
      if (is.character(out) && length(out) == 1) return(out)
      return(NULL)
    }
    NULL
  }
  tryCatch(
    .shared_aes(netlist,
                node_color = pick("node_color", "node_colour"),
                node_shape = pick("node_shape"),
                node_size = pick("node_size"),
                edge_color = pick("edge_color", "edge_colour"),
                edge_size = pick("edge_size"),
                layout = if (is.character(dots[["layout"]])) dots[["layout"]]),
    error = function(e) NULL)
}

is_ego_network <- function(nlist) {
  if (all(unique(names(nlist)) != "")) {
    all_names <- unique(unlist(unname(lapply(nlist, manynet::node_names))))
    length(names(nlist)) == length(all_names) &&
      setequal(names(nlist), all_names)
  } else FALSE
}

