graph_nodes <- function(p, g, node_color, node_shape, node_size,
                        layout = NULL) {
  out <- .infer_node_mapping(g, node_color, node_size, node_shape, layout)
  # A changing network is only treated as a diffusion when nodes actually
  # adopt; otherwise (e.g. `fict_potter`) it is rendered as a standard
  # changing network. TODO: revisit once diffusion is reworked in manynet.
  if(is.null(node_color) && manynet::is_changing(g) &&
     any(is.finite(.node_adoption_time(g)))){
    p <- .map_diff_model_nodes(p, g, out)
  } else if(is.null(node_color) && "diffusion" %in% names(manynet::node_attribute(g))){
    p <- .map_infected_nodes(p, g, out)
  } else {
    p <- .map_nodes(p, out)
    # Check legends
    if (length(unique(out[["nsize"]])) > 1 && !out[["nsize_default"]])
      p <- p + ggplot2::guides(size = ggplot2::guide_legend(title = node_size))
    if (length(unique(out[["nshape"]])) > 1) 
      p <- p + ggplot2::guides(shape = ggplot2::guide_legend(
        title = ifelse(manynet::is_twomode(g) & is.null(node_shape), 
                       "Mode", node_shape)))
    if (length(unique(out[["ncolor"]])) > 1){
      if(length(unique(out[["ncolor"]])) == 2){
        p <- p + ggplot2::scale_fill_manual(values = getOption("snet_highlight", 
                                                               default = c("grey","black")),
                                              guide = ggplot2::guide_legend(node_color))
      } else {
        p <- p + ggplot2::scale_fill_manual(values = ag_qualitative(length(unique(out[["ncolor"]]))),
                                              guide = ggplot2::guide_legend(node_color))
      }
    }
  }
  # Consider rescaling nodes
  p <- p + ggplot2::scale_size(range = c(1/manynet::net_nodes(g)*50, 
                                         1/manynet::net_nodes(g)*100))
  p
}

# Helper functions for .graph_nodes()

.infer_node_mapping <- function(g, node_color, node_size, node_shape,
                                layout = NULL) {
  list("nshape" = .infer_nshape(g, node_shape),
       "nsize" = .infer_nsize(g, node_size, layout),
       # A size the user asked for is mapped through aes(), so that it is
       # rescaled and given a legend naming the attribute it came from. A
       # default size is not: it varies only with how crowded the plot is,
       # which is not something to put in a legend, and rescaling it would
       # undo the very sizing it was calculated to give.
       "nsize_default" = is.null(node_size),
       "ncolor" = .infer_ncolor(g, node_color))
}

# .infer_nsize/.infer_nshape/.infer_ncolor live in R/graph_aes.R, shared with
# grapht(). These arguments have already been checked against the network's
# attributes by graphr()/grapht() (see R/graph_checks.R), so by this point they
# are known to be either an attribute name or a usable literal.

.map_infected_nodes<- function(p, g, out) {
  # node_color <- as.factor(ifelse(manynet::node_attribute(g, "Exposed"), "Exposed",
  #                                ifelse(manynet::node_attribute(g, "Infected"),"Infected", 
  #                                       ifelse(manynet::node_attribute(g, "Recovered"), "Recovered",
  #                                              "Susceptible"))))
  node_color <- dplyr::recode_values(manynet::node_attribute(g, "diffusion"),
                                 "E" ~ "Exposed",
                                 "I" ~ "Infected",
                                 "R" ~ "Recovered",
                                 "S" ~ "Susceptible")
  cols <- match_color(c("#d73027", "#4575b4", "#E6AB02", "#66A61E"))
  p + ggraph::geom_node_point(ggplot2::aes(fill = node_color),
                              size = out[["nsize"]], shape = out[["nshape"]]) +
    ggplot2::scale_fill_manual(name = NULL, guide = ggplot2::guide_legend(""),
                                values = c("Infected" = cols[1],
                                           "Susceptible" = cols[2],
                                           "Exposed" = cols[3],
                                           "Recovered" = cols[4]))
}

.map_diff_model_nodes <- function(p, g, out) {
  dm <- manynet::as_diffusion(g)
  node_adopts <- .node_adoption_time(g)
  nshape <- ifelse(node_adopts == min(node_adopts), "Seed(s)",
                   ifelse(node_adopts == Inf, "Non-Adopter", "Adopter"))
  node_color <- ifelse(is.infinite(node_adopts),
                       max(node_adopts[!is.infinite(node_adopts)]) + 1,
                       node_adopts)
  p + ggraph::geom_node_point(ggplot2::aes(shape = nshape, fill = node_color),
                              size = out[["nsize"]]) +
    ggplot2::scale_fill_gradient(low = match_color("#d73027"), high = match_color("#4575b4"),
                                  breaks=c(min(node_color)+1, 
                                           ifelse(any(nshape=="Non-Adopter"),
                                                  max(node_color)-1,
                                                  max(node_color))),
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
.map_nodes <- function(p, out) {
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
  p + do.call(ggraph::geom_node_point, args) +
    ggplot2::scale_shape_manual(values = c(21, 22, 24, 23, 25,
                                           3, 4, 8,
                                           10, 12, 9,
                                           13, 7, 11, 14))
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