graph_nodes <- function(p, g, node_color, node_shape, node_size,
                        layout = NULL, shared = NULL) {
  out <- .infer_node_mapping(g, node_color, node_size, node_shape, layout,
                             shared)
  # A changing network is only treated as a diffusion when nodes actually
  # adopt; otherwise (e.g. `fict_potter`) it is rendered as a standard
  # changing network. TODO: revisit once diffusion is reworked in manynet.
  if(is.null(node_color) && manynet::is_changing(g) &&
     any(is.finite(.node_adoption_time(g)))){
    p <- .map_diff_model_nodes(p, g, out, shared)
  } else if(is.null(node_color) &&
            "diffusion" %in% manynet::net_node_attributes(g)){
    p <- .map_infected_nodes(p, g, out, shared)
  } else {
    p <- .map_nodes(p, out, shared)
    # Check legends
    if (length(unique(out[["nsize"]])) > 1 && !out[["nsize_default"]])
      p <- p + ggplot2::guides(size = ggplot2::guide_legend(title = node_size))
    if (length(unique(out[["nshape"]])) > 1) 
      p <- p + ggplot2::guides(shape = ggplot2::guide_legend(
        title = ifelse(manynet::is_twomode(g) & is.null(node_shape), 
                       "Mode", node_shape)))
    # Named values, shared limits and `drop = FALSE` for the same reason as the
    # edge colours in R/graph_edges.R: a category keeps its colour and its key
    # in every panel of a `graphs()` plot.
    if (is.numeric(out[["ncolor"]])) {
      # A measure is drawn as a gradient from the theme's base colour to its
      # highlight, so that the order of its values can be read off the plot.
      # The limits are shared across the panels of a `graphs()` plot, so that
      # one value keeps one colour throughout.
      p <- p + ggplot2::scale_fill_gradientn(
        colours = ag_sequential(9), limits = shared[["ncolor_range"]],
        guide = ggplot2::guide_colourbar(title = node_color))
    } else {
      nlevels <- shared[["ncolor"]]
      if (is.null(nlevels)) nlevels <- unique(as.character(out[["ncolor"]]))
      if (length(nlevels) > 1){
        nvalues <- if (length(nlevels) == 2)
          getOption("snet_highlight", default = c("grey","black")) else
            ag_qualitative(length(nlevels))
        p <- p + ggplot2::scale_fill_manual(
          values = stats::setNames(nvalues, nlevels), limits = nlevels,
          drop = FALSE, guide = ggplot2::guide_legend(node_color))
      }
    }
  }
  # Consider rescaling nodes
  p <- p + ggplot2::scale_size(range = c(1/manynet::net_nodes(g)*50, 
                                         1/manynet::net_nodes(g)*100),
                               limits = shared[["nsize"]])
  p
}

# Helper functions for .graph_nodes()

.infer_node_mapping <- function(g, node_color, node_size, node_shape,
                                layout = NULL, shared = NULL) {
  list("nshape" = .infer_nshape(g, node_shape, shared[["nshape"]]),
       "nsize" = .infer_nsize(g, node_size, layout),
       # A size the user asked for is mapped through aes(), so that it is
       # rescaled and given a legend naming the attribute it came from. A
       # default size is not: it varies only with how crowded the plot is,
       # which is not something to put in a legend, and rescaling it would
       # undo the very sizing it was calculated to give.
       "nsize_default" = is.null(node_size),
       "ncolor" = .infer_ncolor(g, node_color, shared[["ncolor"]]))
}

# .infer_nsize/.infer_nshape/.infer_ncolor live in R/graph_aes.R, shared with
# grapht(). These arguments have already been checked against the network's
# attributes by graphr()/grapht() (see R/graph_checks.R), so by this point they
# are known to be either an attribute name or a usable literal.

# The four states a diffusion puts a node in, named and ordered the same way
# wherever they are drawn.
.diffusion_levels <- c("Susceptible", "Exposed", "Infected", "Recovered")

.recode_diffusion <- function(x) {
  dplyr::recode_values(x,
                       "E" ~ "Exposed",
                       "I" ~ "Infected",
                       "R" ~ "Recovered",
                       "S" ~ "Susceptible")
}

.map_infected_nodes<- function(p, g, out, shared = NULL) {
  # node_color <- as.factor(ifelse(manynet::node_attribute(g, "Exposed"), "Exposed",
  #                                ifelse(manynet::node_attribute(g, "Infected"),"Infected", 
  #                                       ifelse(manynet::node_attribute(g, "Recovered"), "Recovered",
  #                                              "Susceptible"))))
  node_color <- .recode_diffusion(manynet::node_attribute(g, "diffusion"))
  cols <- match_color(c("#d73027", "#4575b4", "#E6AB02", "#66A61E"))
  # A wave in which every node has been infected shows one state, and the wave
  # beside it two, so without shared limits the two legends differ and only one
  # of them is collected. Kept in the order the states are passed through.
  limits <- shared[["diffusion"]]
  if (!is.null(limits))
    limits <- .diffusion_levels[.diffusion_levels %in% limits]
  p + ggraph::geom_node_point(ggplot2::aes(fill = node_color),
                              size = out[["nsize"]], shape = out[["nshape"]]) +
    ggplot2::scale_fill_manual(name = NULL, guide = ggplot2::guide_legend(""),
                               limits = limits, drop = FALSE,
                                values = c("Infected" = cols[1],
                                           "Susceptible" = cols[2],
                                           "Exposed" = cols[3],
                                           "Recovered" = cols[4]))
}

.map_diff_model_nodes <- function(p, g, out, shared = NULL) {
  dm <- manynet::as_diffusion(g)
  node_adopts <- .node_adoption_time(g)
  nshape <- ifelse(node_adopts == min(node_adopts), "Seed(s)",
                   ifelse(node_adopts == Inf, "Non-Adopter", "Adopter"))
  node_color <- ifelse(is.infinite(node_adopts),
                       max(node_adopts[!is.infinite(node_adopts)]) + 1,
                       node_adopts)
  # Read from every panel beside this one where there is one, so that a time of
  # adoption is drawn in the same colour throughout.
  span <- shared[["nadopt"]]
  if (is.null(span)) span <- range(node_color[is.finite(node_color)])
  early <- span[1] + 1
  late <- if (any(nshape == "Non-Adopter")) span[2] - 1 else span[2]
  p + ggraph::geom_node_point(ggplot2::aes(shape = nshape, fill = node_color),
                              size = out[["nsize"]]) +
    ggplot2::scale_fill_gradient(low = match_color("#d73027"), high = match_color("#4575b4"),
                                  limits = range(c(span, node_color)),
                                  breaks=c(early, late),
                                  labels=c("Early\nadoption", "Late\nadoption"),
                                  name = "Time of\nAdoption\n") +
    ggplot2::scale_shape_manual(name = "",
                                breaks = c("Seed(s)", "Adopter", "Non-Adopter"),
                                values = c("Seed(s)" = 24,    # triangle
                                           "Adopter" = 21,     # circle
                                           "Non-Adopter" = 22)) +  # square
    ggplot2::guides(fill = ggplot2::guide_colorbar(order = 1, reverse = TRUE),
                    shape = ggplot2::guide_legend(order = 2))
}

# Each of the three node aesthetics is mapped through aes() when it varies
# across nodes, so that ggplot2 scales it and gives it a legend, and passed as
# a constant layer parameter when it does not. A default size is the exception:
# it varies with how crowded each part of the plot is rather than with anything
# about the nodes themselves, so it is passed as a parameter even when it
# varies, which also keeps it clear of the rescaling in graph_nodes().
.map_nodes <- function(p, out, shared = NULL) {
  # The expressions are quoted rather than evaluated so that
  # do.call(aes, mapping) captures them as quosures resolved lazily against
  # `out`, exactly as writing them literally here would.
  keys <- c(ncolor = "fill", nshape = "shape", nsize = "size")
  exprs <- list(ncolor = quote(out[["ncolor"]]),
                nshape = quote(out[["nshape"]]),
                nsize  = quote(out[["nsize"]]))
  mapping <- list(); params <- list()
  for (nm in names(keys)) {
    varies <- length(out[[nm]]) > 1 &&
      !(nm == "nsize" && isTRUE(out[["nsize_default"]]))
    if (varies) mapping[[keys[[nm]]]] <- exprs[[nm]] else
      params[[keys[[nm]]]] <- out[[nm]]
  }
  args <- params
  if (length(mapping)) args$mapping <- do.call(ggplot2::aes, mapping)
  # Naming the shapes by the categories they stand for, where `graphs()` has
  # worked out what those are across its panels, stops a panel that is missing
  # one of them from giving the rest each other's shapes.
  shapes <- c(21, 22, 24, 23, 25, 3, 4, 8, 10, 12, 9, 13, 7, 11, 14)
  slevels <- shared[["nshape"]]
  if (!is.null(slevels) && length(slevels) <= length(shapes))
    shapes <- stats::setNames(shapes[seq_along(slevels)], slevels)
  p + do.call(ggraph::geom_node_point, args) +
    ggplot2::scale_shape_manual(values = shapes, limits = slevels,
                                drop = is.null(slevels))
}

.node_adoption_time <- function(.data){
  
  if(inherits(.data, "diff_model")){
    net <- attr(.data, "network") 
    out <- summary(.data) |> dplyr::filter(event == "I") |> 
      dplyr::distinct(nodes, .keep_all = TRUE) |> 
      dplyr::select(nodes,t)
    if(!manynet::is_labelled(net))
      out <- dplyr::arrange(out, nodes) else if (is.numeric(out$nodes))
        out$nodes <- manynet::node_names(net)[out$nodes]
    out <- stats::setNames(out$t, out$nodes)
    if(length(out) != manynet::net_nodes(net)){
      full <- rep(Inf, manynet::net_nodes(net))
      names(full) <- `if`(manynet::is_labelled(net), 
                          manynet::node_names(net), 
                          as.character(seq_len(manynet::net_nodes(net))))
      full[match(names(out), names(full))] <- out
      out <- `if`(manynet::is_labelled(net), full, unname(full))
    }
  } else {
    net <- .data
    out <- manynet::as_changelist(.data) |> dplyr::filter(value == "I") |> 
      dplyr::distinct(node, .keep_all = TRUE) |> 
      dplyr::select(node,time)
    if(!manynet::is_labelled(net))
      out <- dplyr::arrange(out, node) else if (is.numeric(out$node))
        out$node <- manynet::node_names(net)[out$node]
    out <- stats::setNames(out$time, out$node)
    if(length(out) != manynet::net_nodes(net)){
      full <- rep(Inf, manynet::net_nodes(net))
      names(full) <- `if`(manynet::is_labelled(net), 
                          manynet::node_names(net), 
                          as.character(seq_len(manynet::net_nodes(net))))
      full[match(names(out), names(full))] <- out
      out <- `if`(manynet::is_labelled(net), full, unname(full))
    }
  }
  
  if(!manynet::is_labelled(net)) out <- unname(out)
  out
}