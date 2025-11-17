graph_nodes <- function(p, g, node_color, node_shape, node_size) {
  out <- .infer_node_mapping(g, node_color, node_size, node_shape)
  if(is.null(node_color) && manynet::is_changing(g)){
    p <- .map_diff_model_nodes(p, g, out)
  } else if(is.null(node_color) && "diffusion" %in% names(manynet::node_attribute(g))){
    p <- .map_infected_nodes(p, out)
  } else {
    p <- .map_nodes(p, out)
    # Check legends
    if (length(unique(out[["nsize"]])) > 1)
      p <- p + ggplot2::guides(size = ggplot2::guide_legend(title = node_size))
    if (length(unique(out[["nshape"]])) > 1) 
      p <- p + ggplot2::guides(shape = ggplot2::guide_legend(
        title = ifelse(manynet::is_twomode(g) & is.null(node_shape), "Node Mode", "Node Shape")))
    if (length(unique(out[["ncolor"]])) > 1){
      if(length(unique(out[["ncolor"]])) == 2){
        p <- p + ggplot2::scale_colour_manual(values = getOption("snet_highlight", default = c("grey","black")),
                                              guide = ggplot2::guide_legend(node_color))
      } else {
        p <- p + ggplot2::scale_colour_manual(values = ag_qualitative(length(unique(out[["ncolor"]]))),
                                              guide = ggplot2::guide_legend(node_color))
      }
    }
  }
  # Consider rescaling nodes
  p <- p + ggplot2::scale_size(range = c(1/manynet::net_nodes(g)*50, 1/manynet::net_nodes(g)*100))
  p
}

# Helper functions for .graph_nodes()

.infer_node_mapping <- function(g, node_color, node_size, node_shape) {
  .check_node_variables(g, node_color, node_size)
  list("nshape" = .infer_nshape(g, node_shape),
       "nsize" = .infer_nsize(g, node_size),
       "ncolor" = .infer_ncolor(g, node_color))
}

.infer_nsize <- function(g, node_size) {
  if (!is.null(node_size)) {
    if (is.character(node_size)) {
      out <- manynet::node_attribute(g, node_size)
    } else out <- node_size
    if (length(node_size > 1) & all(out <= 1 & out >= 0)) out <- out * 10
  } else {
    out <- min(20, (250 / manynet::net_nodes(g)) / 2)
  }
  as.numeric(out)
}

.infer_nshape <- function(g, node_shape) {
  if (!is.null(node_shape)) {
    if (node_shape %in% names(manynet::node_attribute(g))) {
      out <- as.factor(as.character(manynet::node_attribute(g, node_shape)))
    } else out <- node_shape
  } else if (is_twomode(g) & is.null(node_shape)) {
    out <- ifelse(igraph::V(g)$type, "One", "Two")
  } else {
    out <- "circle"
  }
  out
}

.infer_ncolor <- function(g, node_color) {
  if (!is.null(node_color)) {
    if (node_color %in% names(manynet::node_attribute(g))) {
      if ("node_mark" %in% class(manynet::node_attribute(g, node_color))) {
        out <- factor(as.character(manynet::node_attribute(g, node_color)),
                      levels = c("FALSE", "TRUE"))
      } else out <- as.factor(as.character(manynet::node_attribute(g, node_color)))
      if (length(unique(out)) == 1) {
        out <- rep("black", manynet::net_nodes(g))
        manynet::snet_info("Please indicate a variable with more than one value or level when mapping node colors.")
      }
    } else out <- node_color
  } else {
    out <- "black"
  }
  out
}

.check_node_variables <- function(g, node_color, node_size) {
  if (!is.null(node_color)) {
    if (any(!tolower(node_color) %in% tolower(igraph::vertex_attr_names(g))) &
        any(!node_color %in% grDevices::colors())) {
      manynet::snet_info("Please make sure you spelled `node_color` variable correctly.")
    } 
  }
  if (!is.null(node_size)) {
    if (!is.numeric(node_size) & any(!tolower(node_size) %in% tolower(igraph::vertex_attr_names(g)))) {
      manynet::snet_info("Please make sure you spelled `node_size` variable correctly.")
    }
  }
}

.map_infected_nodes<- function(p, g, out) {
  node_color <- as.factor(ifelse(manynet::node_attribute(g, "Exposed"), "Exposed",
                                 ifelse(manynet::node_attribute(g, "Infected"),"Infected", 
                                        ifelse(manynet::node_attribute(g, "Recovered"), "Recovered",
                                               "Susceptible"))))
  p + ggraph::geom_node_point(ggplot2::aes(color = node_color),
                              size = out[["nsize"]], shape = out[["nshape"]]) +
    ggplot2::scale_color_manual(name = NULL, guide = ggplot2::guide_legend(""),
                                values = c("Infected" = "#d73027",
                                           "Susceptible" = "#4575b4",
                                           "Exposed" = "#E6AB02",
                                           "Recovered" = "#66A61E"))
}

.map_diff_model_nodes <- function(p, g, out) {
  dm <- manynet::as_diffusion(g)
  node_adopts <- manynet::node_adoption_time(g)
  nshape <- ifelse(node_adopts == min(node_adopts), "Seed(s)",
                   ifelse(node_adopts == Inf, "Non-Adopter", "Adopter"))
  node_color <- ifelse(is.infinite(node_adopts), 
                       max(node_adopts[!is.infinite(node_adopts)]) + 1, 
                       node_adopts)
  p + ggraph::geom_node_point(ggplot2::aes(shape = nshape, color = node_color),
                              size = out[["nsize"]]) +
    ggplot2::scale_color_gradient(low = match_color("#d73027"), high = match_color("#4575b4"),
                                  breaks=c(min(node_color)+1, 
                                           ifelse(any(nshape=="Non-Adopter"),
                                                  max(node_color)-1,
                                                  max(node_color))),
                                  labels=c("Early\nadoption", "Late\nadoption"),
                                  name = "Time of\nAdoption\n") +
    ggplot2::scale_shape_manual(name = "",
                                breaks = c("Seed(s)", "Adopter", "Non-Adopter"),
                                values = c("Seed(s)" = "triangle",
                                           "Adopter" = "circle",
                                           "Non-Adopter" = "square")) +
    ggplot2::guides(color = ggplot2::guide_colorbar(order = 1, reverse = TRUE),
                    shape = ggplot2::guide_legend(order = 2))
}

.map_nodes <- function(p, out) {
  if (length(out[["ncolor"]]) == 1 & length(out[["nsize"]]) == 1 & length(out[["nshape"]]) == 1) {
    p <- p + ggraph::geom_node_point(colour = out[["ncolor"]], size = out[["nsize"]],
                                     shape = out[["nshape"]])
  } else if (length(out[["ncolor"]]) > 1 & length(out[["nsize"]]) == 1 & length(out[["nshape"]]) == 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(colour = out[["ncolor"]]), 
                                     size = out[["nsize"]], shape = out[["nshape"]])
  } else if (length(out[["ncolor"]]) == 1 & length(out[["nsize"]]) > 1 & length(out[["nshape"]]) == 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(size = out[["nsize"]]),
                                     colour = out[["ncolor"]], shape = out[["nshape"]])
  } else if (length(out[["ncolor"]]) == 1 & length(out[["nsize"]]) == 1 & length(out[["nshape"]]) > 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(shape = out[["nshape"]]),
                                     colour = out[["ncolor"]], size = out[["nsize"]])
  } else if (length(out[["ncolor"]]) > 1 & length(out[["nsize"]]) > 1 & length(out[["nshape"]]) == 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(colour = out[["ncolor"]], size = out[["nsize"]]),
                                     shape = out[["nshape"]])
  } else if (length(out[["ncolor"]]) > 1 & length(out[["nsize"]]) == 1 & length(out[["nshape"]]) > 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(colour = out[["ncolor"]], shape = out[["nshape"]]),
                                     size = out[["nsize"]])
  } else if (length(out[["ncolor"]]) == 1 & length(out[["nsize"]]) > 1 & length(out[["nshape"]]) > 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(size = out[["nsize"]], shape = out[["nshape"]]),
                                     colour = out[["ncolor"]])
  } else {
    p <- p + ggraph::geom_node_point(ggplot2::aes(colour = out[["ncolor"]],
                                                  shape = out[["nshape"]],
                                                  size = out[["nsize"]]))
  }
  p
}

