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
  based_on <- match.arg(based_on)
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
      netlist <- netlist[c(1, length(netlist))]
      manynet::snet_info("Plotting first and last waves side-by-side. \nTo set the waves plotted use the 'waves = ' argument.")
    }
  } else if (!missing(waves)) {
    if (length(waves) == 1) netlist <- netlist[c(1:waves)] else 
      netlist <- netlist[waves]
  }
  if (is.null(names(netlist))) names(netlist) <- rep("", length(netlist))
  if (length(unique(lapply(netlist, length))) == 1) {
    # Sharing a layout requires every panel to draw every node, so isolates
    # are kept unless the user explicitly asks otherwise
    dots <- list(...)
    if (!"isolates" %in% names(dots)) dots$isolates <- "keep"
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
        graphr(netlist[[i]], layout = "star", center = names(netlist)[[i]], ...) + 
          ggtitle(names(netlist)[i]))
    } else {
      manynet::snet_info("Layouts were not standardised since not all nodes appear across waves.")  
      gs <- lapply(1:length(netlist), function(i)
        graphr(netlist[[i]], ...) + ggtitle(names(netlist)[i]))
    }
  }
  # if (all(c("Infected", "Exposed", "Recovered") %in% names(gs[[1]]$data))) {
  #   gs <- .collapse_guides(gs)
  # }
  do.call(patchwork::wrap_plots, c(gs, list(guides = "collect")))
}

# `graphs()` helper functions
is_ego_network <- function(nlist) {
  if (all(unique(names(nlist)) != "")) {
    all_names <- unique(unlist(unname(lapply(nlist, manynet::node_names))))
    length(names(nlist)) == length(all_names) &&
      setequal(names(nlist), all_names)
  } else FALSE
}

