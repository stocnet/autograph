# nocov start
# Dynamic networks ####

#' Easily animate dynamic networks with sensible defaults
#' @description 
#'   This function provides users with an easy way to graph
#'   dynamic network data for exploration and presentation.
#'   
#'   It builds upon this package's `graphr()` function,
#'   and inherits all the same features and arguments.
#'   See `graphr()` for more.
#'   However, it uses the `{gganimate}` package to animate the changes
#'   between successive iterations of a network.
#'   This is useful for networks in which the ties and/or the node or tie
#'   attributes are changing.
#'   
#'   `grapht()` returns a `{ggplot2}`-compatible object that can be 
#'   extended with additional layers such as `ggplot2::labs()`, 
#'   `ggplot2::theme()`, scale functions, and others,
#'   just like plots produced by `graphr()` and `graphs()`.
#'   The animation is rendered when the object is printed or displayed.
#'   Users who want more control over animation parameters can call
#'   `gganimate::animate()` directly on the returned object.
#'   
#'   The visual appearance is consistent with `graphr()`:
#'   nodes use fillable shapes with the fill aesthetic,
#'   the same colour palettes are applied,
#'   and labels use the current theme font.
#'   
#'   A progress bar is shown if it takes some time to encode all the
#'   .png files into a .gif.
#' @name plot_grapht
#' @family mapping
#' @param tlist A manynet-compatible network listed according to
#'   a time attribute, waves, or slices.
#'   This can also be a diffusion model result from e.g.
#'   `manynet::play_diffusion()`.
#' @param keep_isolates Logical, whether to keep isolate nodes in the graph.
#'   TRUE by default.
#'   If FALSE, removes nodes from each frame they are isolated in.
#' @inheritParams plot_graphr
#' @importFrom igraph gsize as_data_frame get.edgelist vcount
#' @importFrom ggplot2 ggplot geom_segment geom_point geom_text
#'   scale_alpha_manual theme_void
#' @importFrom ggraph create_layout
#' @importFrom dplyr mutate select distinct left_join %>%
#' @source https://blog.schochastics.net/posts/2021-09-15_animating-network-evolutions-with-gganimate/
#' @return A `{ggplot2}`-compatible object with `{gganimate}` animation layers.
#'   This object can be extended with additional `{ggplot2}` layers
#'   (e.g. `+ labs(subtitle = "My subtitle")`).
#'   When printed or displayed, the animation is rendered as a .gif.
#'   For more control over animation parameters,
#'   pass the result to `gganimate::animate()` directly.
#' @examples
#' #ison_adolescents %>%
#' #  mutate_ties(year = sample(1995:1998, 10, replace = TRUE)) %>%
#' #  to_waves(attribute = "year", cumulative = TRUE) %>%
#' #  grapht()
#' #ison_adolescents %>% 
#' #  mutate(gender = rep(c("male", "female"), times = 4),
#' #         hair = rep(c("black", "brown"), times = 4),
#' #         age = sample(11:16, 8, replace = TRUE)) %>%
#' #  mutate_ties(year = sample(1995:1998, 10, replace = TRUE),
#' #              links = sample(c("friends", "not_friends"), 10, replace = TRUE),
#' #              weekly_meetings = sample(c(3, 5, 7), 10, replace = TRUE)) %>%
#' #  to_waves(attribute = "year") %>%
#' #  grapht(layout = "concentric", membership = "gender",
#' #             node_shape = "gender", node_color = "hair",
#' #             node_size =  "age", edge_color = "links",
#' #             edge_size = "weekly_meetings")
#' #grapht(play_diffusion(ison_adolescents, seeds = 5))
#' @export
grapht <- function(tlist, keep_isolates = TRUE,
                   layout = NULL, labels = TRUE,
                   node_color, node_shape, node_size,
                   edge_color, edge_size, ...,
                   node_colour, edge_colour) {
  thisRequires("gganimate")
  thisRequires("gifski")
  # Check arguments ####
  layout <- .infer_layout(tlist, layout)
  if (missing(node_color) && missing(node_colour)) {
    node_color <- NULL
  } else if (missing(node_color)) {
    node_color <- as.character(substitute(node_colour))
  } else {
    node_color <- as.character(substitute(node_color))
  }
  if (missing(node_shape)) node_shape <- NULL else
    node_shape <- as.character(substitute(node_shape))
  if (missing(node_size)) node_size <- NULL else if (!is.numeric(node_size)) {
    node_size <- as.character(substitute(node_size))
  }
  if (missing(edge_color) && missing(edge_colour)) {
    edge_color <- NULL
  } else if (missing(edge_color)) {
    edge_color <- as.character(substitute(edge_colour))
  } else {
    edge_color <- as.character(substitute(edge_color))
  }
  if (missing(edge_size)) edge_size <- NULL else if (!is.numeric(edge_size)) {
    edge_size <- as.character(substitute(edge_size))
  }
  
  # If diffusion model ####
  if (inherits(tlist, "diff_model") || manynet::is_changing(tlist)) 
    tlist <- manynet::to_waves(tlist)
  # Convert list elements to tidygraph for S4 compatibility ####
  tlist <- lapply(tlist, manynet::as_tidygraph)
  # Check if object is a list of networks ####
  if (!is.list(tlist[[1]])) {
    manynet::snet_abort("Please declare a manynet-compatible network listed according
         to a time attribute, waves, or slices.")
  }
  # Remove lists without edges ####
  tlist <- Filter(function(x) manynet::net_ties(x) > 0, tlist)
  # Check names for groups ####
  if (!"name" %in% names(manynet::node_attribute(tlist[[1]]))) {
    labels <- FALSE
    for (i in seq_len(length(tlist))) {
      tlist[[i]] <- manynet::add_node_attribute(tlist[[i]], "name",
                                                as.character(seq_len(igraph::vcount(tlist[[i]]))))
    }
  }
  
  # Create an edge list ####
  edges_lst <- lapply(1:length(tlist), function(i)
    cbind(igraph::as_data_frame(tlist[[i]], "edges"),
          frame = ifelse(is.null(names(tlist)), i, names(tlist)[i])))
  # Check if all names are present in all lists
  if (length(unique(unname(lapply(tlist, length)))) != 1) {
    if (any(c(node_shape, node_color, node_size) %in% names(manynet::node_attribute(tlist[[1]])))) {
      node_info <- dplyr::distinct(do.call(rbind, lapply(1:length(tlist), function(i)
        tlist[[i]] %>% tidygraph::activate("nodes") %>% data.frame()))) # keep node info for later
    } else node_info <- NULL
    tlist <- manynet::to_waves(manynet::as_tidygraph(do.call("rbind", edges_lst)), attribute = "frame")
    tlist <- lapply(tlist, manynet::as_tidygraph)
  } else node_info <- NULL
  
  # Add separate layouts for each time point ####
  lay <- lapply(1:length(tlist), function(i)
    ggraph::create_layout(tlist[[i]], layout, ...))
  
  # Create node/edge list for each time point ####
  nodes_lst <- lapply(1:length(tlist), function(i) {
    cbind(igraph::as_data_frame(tlist[[i]], "vertices"),
          x = lay[[i]][, 1], y = lay[[i]][, 2],
          frame = ifelse(is.null(names(tlist)), i, names(tlist)[i]))
  })
  edges_lst <- .time_edges_lst(tlist, edges_lst, nodes_lst)
  
  # Get edge IDs for all edges ####
  all_edges <- do.call("rbind", lapply(tlist, igraph::get.edgelist))
  all_edges <- all_edges[!duplicated(all_edges), ]
  all_edges <- cbind(all_edges, paste0(all_edges[, 1], "-", all_edges[, 2]))
  
  # Add edges level information for edge transitions ####
  edges_lst <- .transition_edge_lst(tlist, edges_lst, nodes_lst, all_edges)
  
  # Bind nodes and edges list ####
  edges_out <- do.call("rbind", edges_lst)
  nodes_out <- do.call("rbind", nodes_lst)
  if (!is.null(node_info)) {
    nodes_out <- dplyr::left_join(nodes_out, node_info[!duplicated(node_info$name),], by = "name")
  }
  
  # Delete nodes for each frame if isolate ####
  if (isFALSE(keep_isolates)) {
    nodes_out <- .remove_isolates(edges_out, nodes_out)
  } else {
    if (nrow(nodes_out)/length(unique(nodes_out$frame)) > 30 &
        any(unlist(lapply(tlist, manynet::node_is_isolate)) == TRUE)) {
      manynet::snet_info("Please considering deleting isolates to improve visualisation.")
    } 
    nodes_out$status <- TRUE
  }
  
  # Plot with ggplot2 and animate with gganimate ####
  p <- .map_dynamic(edges_out, nodes_out, edge_color, node_shape,
                   node_color, node_size, edge_size, labels) +
    gganimate::transition_states(states = frame, transition_length = 5,
                                 state_length = 10, wrap = FALSE) +
    gganimate::enter_fade() +
    gganimate::exit_fade() +
    ggplot2::labs(title = "{closest_state}")
  # Return extensible ggplot+gganimate object ####
  attr(p, "nwaves") <- length(tlist)
  class(p) <- c("grapht", class(p))
  p
}

#' @rdname plot_grapht
#' @param x A grapht object to print.
#' @export
print.grapht <- function(x, ...) {
  nwaves <- if (!is.null(attr(x, "nwaves"))) attr(x, "nwaves") else 5
  anim <- gganimate::animate(x, duration = 2 * nwaves,
                              start_pause = 5, end_pause = 10,
                              renderer = gganimate::gifski_renderer())
  print(anim)
  invisible(anim)
}

# Helper functions for grapht() ####

# Aesthetic mapping for dynamic networks, consistent with graphr()
.map_dynamic <- function(edges_out, nodes_out, edge_color, node_shape,
                         node_color, node_size, edge_size, labels) {
  alphad <- ifelse(nodes_out$status == TRUE, 1, 0)
  alphae <- ifelse(edges_out$status == TRUE, 1, 0)
  if (all(unique(alphae) == 1)) alphae <- 0.4
  
  # --- Plot edges ----
  ecolor_mapped <- !is.null(edge_color) && edge_color %in% names(edges_out)
  esize_mapped <- !is.null(edge_size) && edge_size %in% names(edges_out)
  if (ecolor_mapped) edges_out$ecolor_var <- as.factor(edges_out[[edge_color]])
  if (esize_mapped) {
    edges_out$esize_var <- as.numeric(edges_out[[edge_size]])
    edges_out$esize_var <- ifelse(is.na(edges_out$esize_var), 0.5,
                                  edges_out$esize_var)
  }
  seg_color <- if (!is.null(edge_color) && !ecolor_mapped) edge_color else "black"
  seg_size <- if (!is.null(edge_size) && !esize_mapped) edge_size else 0.5
  
  if (ecolor_mapped && esize_mapped) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y, yend = yend,
                                         group = id, colour = ecolor_var,
                                         linewidth = esize_var),
                            alpha = alphae, data = edges_out)
  } else if (ecolor_mapped) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y, yend = yend,
                                         group = id, colour = ecolor_var),
                            alpha = alphae, data = edges_out,
                            linewidth = seg_size)
  } else if (esize_mapped) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y, yend = yend,
                                         group = id, linewidth = esize_var),
                            alpha = alphae, data = edges_out,
                            color = seg_color)
  } else {
    p <- ggplot2::ggplot() +
      ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y, yend = yend,
                                         group = id),
                            alpha = alphae, data = edges_out,
                            color = seg_color, linewidth = seg_size)
  }
  
  # Edge scales
  if (ecolor_mapped) {
    n_ecolors <- length(unique(edges_out$ecolor_var))
    if (n_ecolors == 2) {
      p <- p + ggplot2::scale_colour_manual(
        values = getOption("snet_highlight", default = c("grey", "black")),
        guide = ggplot2::guide_legend(edge_color))
    } else if (n_ecolors > 2) {
      p <- p + ggplot2::scale_colour_manual(
        values = ag_qualitative(n_ecolors),
        guide = ggplot2::guide_legend(edge_color))
    }
  }
  
  # --- Set up node aesthetics ----
  n_per_frame <- nrow(nodes_out) / length(unique(nodes_out$frame))
  is_diffusion <- "diffusion" %in% names(nodes_out)
  
  # Node color
  ncolor_mapped <- FALSE
  if (!is.null(node_color) && node_color %in% names(nodes_out)) {
    nodes_out$ncolor <- as.factor(nodes_out[[node_color]])
    ncolor_mapped <- TRUE
  } else if (is.null(node_color) && is_diffusion) {
    nodes_out$ncolor <- dplyr::case_match(nodes_out[["diffusion"]],
                                          "E" ~ "Exposed", "I" ~ "Infected",
                                          "R" ~ "Recovered", "S" ~ "Susceptible")
    ncolor_mapped <- TRUE
  }
  nfill <- if (!ncolor_mapped) {
    if (!is.null(node_color)) node_color else "black"
  }
  
  # Node shape
  nshape_mapped <- !is.null(node_shape) && node_shape %in% names(nodes_out)
  if (nshape_mapped) {
    nodes_out$nshape <- as.factor(nodes_out[[node_shape]])
  }
  nshape <- if (!nshape_mapped) {
    if (!is.null(node_shape)) node_shape else 21
  }
  
  # Node size (default consistent with graphr)
  nsize_mapped <- !is.null(node_size) && node_size %in% names(nodes_out)
  if (nsize_mapped) {
    nodes_out$nsize <- as.numeric(nodes_out[[node_size]])
  }
  nsize <- if (!nsize_mapped) {
    if (!is.null(node_size)) node_size else min(20, (250 / n_per_frame) / 2)
  }
  
  # --- Add labels ----
  if (isTRUE(labels)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(x, y, label = name),
                                alpha = alphad, data = nodes_out,
                                color = "black", family = ag_font(),
                                hjust = -0.2, vjust = -0.2, show.legend = FALSE)
  }
  
  # --- Plot nodes (8 mapping cases, consistent with graphr) ----
  if (!ncolor_mapped && !nshape_mapped && !nsize_mapped) {
    p <- p + ggplot2::geom_point(ggplot2::aes(x, y, group = name),
                                 alpha = alphad, data = nodes_out,
                                 fill = nfill, size = nsize, shape = nshape,
                                 show.legend = FALSE)
  } else if (ncolor_mapped && !nshape_mapped && !nsize_mapped) {
    p <- p + ggplot2::geom_point(ggplot2::aes(x, y, group = name, fill = ncolor),
                                 alpha = alphad, data = nodes_out,
                                 size = nsize, shape = nshape,
                                 show.legend = TRUE)
  } else if (!ncolor_mapped && !nshape_mapped && nsize_mapped) {
    p <- p + ggplot2::geom_point(ggplot2::aes(x, y, group = name, size = nsize),
                                 alpha = alphad, data = nodes_out,
                                 fill = nfill, shape = nshape,
                                 show.legend = TRUE)
  } else if (!ncolor_mapped && nshape_mapped && !nsize_mapped) {
    p <- p + ggplot2::geom_point(ggplot2::aes(x, y, group = name, shape = nshape),
                                 alpha = alphad, data = nodes_out,
                                 fill = nfill, size = nsize,
                                 show.legend = TRUE)
  } else if (ncolor_mapped && nshape_mapped && !nsize_mapped) {
    p <- p + ggplot2::geom_point(ggplot2::aes(x, y, group = name,
                                              fill = ncolor, shape = nshape),
                                 alpha = alphad, data = nodes_out,
                                 size = nsize, show.legend = TRUE)
  } else if (ncolor_mapped && !nshape_mapped && nsize_mapped) {
    p <- p + ggplot2::geom_point(ggplot2::aes(x, y, group = name,
                                              fill = ncolor, size = nsize),
                                 alpha = alphad, data = nodes_out,
                                 shape = nshape, show.legend = TRUE)
  } else if (!ncolor_mapped && nshape_mapped && nsize_mapped) {
    p <- p + ggplot2::geom_point(ggplot2::aes(x, y, group = name,
                                              shape = nshape, size = nsize),
                                 alpha = alphad, data = nodes_out,
                                 fill = nfill, show.legend = TRUE)
  } else {
    p <- p + ggplot2::geom_point(ggplot2::aes(x, y, group = name,
                                              fill = ncolor, shape = nshape,
                                              size = nsize),
                                 alpha = alphad, data = nodes_out,
                                 show.legend = TRUE)
  }
  
  # --- Add scales (consistent with graphr) ----
  # Node color scales
  if (ncolor_mapped) {
    if (is_diffusion && is.null(node_color)) {
      cols <- match_color(c("#d73027", "#4575b4", "#E6AB02", "#66A61E"))
      p <- p + ggplot2::scale_fill_manual(name = NULL,
                                          values = c("Infected" = cols[1],
                                                     "Susceptible" = cols[2],
                                                     "Exposed" = cols[3],
                                                     "Recovered" = cols[4]))
    } else {
      n_colors <- length(unique(nodes_out$ncolor))
      if (n_colors == 2) {
        p <- p + ggplot2::scale_fill_manual(
          values = getOption("snet_highlight", default = c("grey", "black")),
          guide = ggplot2::guide_legend(node_color))
      } else if (n_colors > 2) {
        p <- p + ggplot2::scale_fill_manual(
          values = ag_qualitative(n_colors),
          guide = ggplot2::guide_legend(node_color))
      }
    }
  }
  # Node shape scale
  if (nshape_mapped) {
    p <- p + ggplot2::scale_shape_manual(values = c(21, 22, 24, 23, 25))
  }
  # Node size rescaling: range adapts to network density, matching graphr's graph_nodes()
  p <- p + ggplot2::scale_size(range = c(1/n_per_frame*50, 1/n_per_frame*100))
  
  # --- Apply theme ----
  p + ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom")
}

.time_edges_lst <- function(tlist, edges_lst, nodes_lst) {
  lapply(1:length(tlist), function(i) {
    edges_lst[[i]]$x <- nodes_lst[[i]]$x[match(edges_lst[[i]]$from,
                                                nodes_lst[[i]]$name)]
    edges_lst[[i]]$y <- nodes_lst[[i]]$y[match(edges_lst[[i]]$from,
                                                nodes_lst[[i]]$name)]
    edges_lst[[i]]$xend <- nodes_lst[[i]]$x[match(edges_lst[[i]]$to,
                                                   nodes_lst[[i]]$name)]
    edges_lst[[i]]$yend <- nodes_lst[[i]]$y[match(edges_lst[[i]]$to,
                                                   nodes_lst[[i]]$name)]
    edges_lst[[i]]$id <- paste0(edges_lst[[i]]$from, "-", edges_lst[[i]]$to)
    edges_lst[[i]]$status <- TRUE
    edges_lst[[i]]
  })
}

.transition_edge_lst <- function(tlist, edges_lst, nodes_lst, all_edges) {
  lapply(1:length(tlist), function(i) {
    idx <- which(!all_edges[, 3] %in% edges_lst[[i]]$id)
    if (length(idx) != 0) {
      tmp <- data.frame(from = all_edges[idx, 1], to = all_edges[idx, 2],
                        id = all_edges[idx, 3])
      tmp$x <- nodes_lst[[i]]$x[match(tmp$from, nodes_lst[[i]]$name)]
      tmp$y <- nodes_lst[[i]]$y[match(tmp$from, nodes_lst[[i]]$name)]
      tmp$xend <- nodes_lst[[i]]$x[match(tmp$to, nodes_lst[[i]]$name)]
      tmp$yend <- nodes_lst[[i]]$y[match(tmp$to, nodes_lst[[i]]$name)]
      tmp$frame <- ifelse(is.null(names(tlist)), i, names(tlist)[i])
      tmp$status <- FALSE
      edges_lst[[i]] <- dplyr::bind_rows(edges_lst[[i]], tmp)
    }
    edges_lst[[i]]
  })
}

.remove_isolates <- function(edges_out, nodes_out) {
  # Create node metadata for node presence in certain frame
  meta <- edges_out %>%
    dplyr::filter(status == TRUE) %>%
    dplyr::mutate(framen = match(frame, unique(frame)),
                  meta = ifelse(framen > 1, paste0(from, (framen - 1)), from)) %>%
    dplyr::select(meta, status) %>%
    dplyr::distinct()
  metab <- edges_out %>%
    dplyr::filter(status == TRUE) %>%
    dplyr::mutate(framen = match(frame, unique(frame)),
                  meta = ifelse(framen > 1, paste0(to, (framen - 1)), to)) %>%
    dplyr::select(meta, status) %>%
    rbind(meta) %>%
    dplyr::distinct()
  # Mark nodes that are isolates
  nodes_out$meta <- rownames(nodes_out)
  # Join data
  nodes_out <- dplyr::left_join(nodes_out, metab, by = "meta") %>%
    dplyr::mutate(status = ifelse(is.na(status), FALSE, TRUE)) %>%
    dplyr::distinct()
}
# nocov end
