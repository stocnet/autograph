# Dynamic networks ####

#' Easily animate dynamic networks with sensible defaults
#' 
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
#'   A progress bar is shown if it takes some time to encoding all the
#'   .png files into a .gif.
#' @name plot_grapht
#' @family mapping
#' @param tlist The same migraph-compatible network listed according to
#'   a time attribute, waves, or slices.
#' @param keep_isolates Logical, whether to keep isolate nodes in the graph.
#'   TRUE by default.
#'   If FALSE, removes nodes from each frame they are isolated in.
#' @inheritParams plot_graphr
#' @importFrom igraph gsize as_data_frame get.edgelist
#' @importFrom ggplot2 ggplot geom_segment geom_point geom_text
#' scale_alpha_manual theme_void
#' @importFrom ggraph create_layout
#' @importFrom dplyr mutate select distinct left_join %>%
#' @source https://blog.schochastics.net/posts/2021-09-15_animating-network-evolutions-with-gganimate/
#' @return Shows a .gif image. Assigning the result of the function
#'   saves the gif to a temporary folder and the object holds the path to this file.
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
                   layout, labels = TRUE,
                   node_color, node_shape, node_size,
                   edge_color, edge_size, ...,
                   node_colour, edge_colour) {
  thisRequires("gganimate")
  thisRequires("gifski")
  # thisRequires("png")
  x <- y <- name <- status <- frame <- NULL
  # Check arguments
  if (missing(layout)) {
    if (length(tlist[[1]]) == 3) {
      layout <- "triad" 
    } else if (length(tlist[[1]]) == 4) {
      layout <- "quad" 
    } else if (is_twomode(tlist[[1]])) {
      layout <- "hierarchy"
    } else layout <- "stress"
  }
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
  # Check if diffusion model
  if (inherits(tlist, "diff_model")) tlist <- manynet::to_waves(tlist)
  # Check if object is a list of lists
  if (!is.list(tlist[[1]])) {
    manynet::snet_abort("Please declare a manynet-compatible network listed according
         to a time attribute, waves, or slices.")
  }
  # Remove lists without edges
  tlist <- Filter(function(x) igraph::gsize(x) > 0, tlist)
  # Check names for groups
  if (!"name" %in% names(manynet::node_attribute(tlist[[1]]))) {
    labels <- FALSE
    for (i in seq_len(length(tlist))) {
      tlist[[i]] <- manynet::add_node_attribute(tlist[[i]], "name",
                                                as.character(seq_len(igraph::vcount(tlist[[i]]))))
    }
  }
  # Create an edge list
  edges_lst <- lapply(1:length(tlist), function(i)
    cbind(igraph::as_data_frame(tlist[[i]], "edges"),
          frame = ifelse(is.null(names(tlist)), i, names(tlist)[i])))
  # Check if all names are present in all lists
  if (length(unique(unname(lapply(tlist, length)))) != 1) {
    if (any(c(node_shape, node_color, node_size) %in% names(manynet::node_attribute(tlist[[1]])))) {
      node_info <- dplyr::distinct(do.call(rbind, lapply(1:length(tlist), function(i)
        tlist[[i]] %>% tidygraph::activate("nodes") %>% data.frame()))) # keep node info for latter
    } else node_info <- NULL
    tlist <- manynet::to_waves(manynet::as_tidygraph(do.call("rbind", edges_lst)), attribute = "frame")
  } else node_info <- NULL
  # Add separate layouts for each time point
  lay <- lapply(1:length(tlist), function(i)
    ggraph::create_layout(tlist[[i]], layout, ...))
  # Create a node list for each time point
  nodes_lst <- lapply(1:length(tlist), function(i) {
    cbind(igraph::as_data_frame(tlist[[i]], "vertices"),
          x = lay[[i]][, 1], y = lay[[i]][, 2],
          frame = ifelse(is.null(names(tlist)), i, names(tlist)[i]))
  })
  # Create an edge list for each time point
  edges_lst <- time_edges_lst(tlist, edges_lst, nodes_lst)
  # Get edge IDs for all edges
  all_edges <- do.call("rbind", lapply(tlist, igraph::get.edgelist))
  all_edges <- all_edges[!duplicated(all_edges), ]
  all_edges <- cbind(all_edges, paste0(all_edges[, 1], "-", all_edges[, 2]))
  # Add edges level information for edge transitions
  edges_lst <- transition_edge_lst(tlist, edges_lst, nodes_lst, all_edges)
  # Bind nodes and edges list
  edges_out <- do.call("rbind", edges_lst)
  nodes_out <- do.call("rbind", nodes_lst)
  if (!is.null(node_info)) {
    nodes_out <- dplyr::left_join(nodes_out, node_info[!duplicated(node_info$name),], by = "name")
  }
  # Delete nodes for each frame if isolate
  if (isFALSE(keep_isolates)) {
    nodes_out <- remove_isolates(edges_out, nodes_out)
  } else {
    if (nrow(nodes_out)/length(unique(nodes_out$frame)) > 30 &
        any(unlist(lapply(tlist, manynet::node_is_isolate)) == TRUE)) {
      manynet::snet_info("Please considering deleting isolates to improve visualisation.")
    } 
    nodes_out$status <- TRUE
  }
  # Plot with ggplot2/ggraph and animate with gganimate
  p <- map_dynamic(edges_out, nodes_out, edge_color, node_shape,
                   node_color, node_size, edge_size, labels) +
    gganimate::transition_states(states = frame, transition_length = 5,
                                 state_length = 10, wrap = FALSE) +
    gganimate::enter_fade() +
    gganimate::exit_fade() +
    ggplot2::labs(title = "{closest_state}")
  gganimate::animate(p, duration = 2*length(tlist), start_pause = 5,
                     end_pause = 10, renderer = gganimate::gifski_renderer())
}

map_dynamic <- function(edges_out, nodes_out, edge_color, node_shape,
                        node_color, node_size, edge_size, labels) {
  x <- xend <- y <- yend <- id <- status <- Infected <- name <- NULL
  alphad <- ifelse(nodes_out$status == TRUE, 1, 0)
  alphae <- ifelse(edges_out$status == TRUE, 1, 0)
  if (all(unique(alphae) == 1)) alphae <- 0.8
  # Plot edges
  if (!is.null(edge_color)) {
    # Remove NAs in edge color, if declared
    if (edge_color %in% names(edges_out)) {
      edge_color <- .check_color(edges_out[[edge_color]])
    }
  } else edge_color <- "black"
  if (!is.null(edge_size)) {
    if (edge_size %in% names(edges_out)) {
      edge_size <- as.numeric(edges_out[[edge_size]])
      edge_size <- ifelse(is.na(edge_size), 0.5, edge_size)
    }
  } else edge_size <- 0.5
  p <- ggplot2::ggplot() + 
    ggplot2::geom_segment(aes(x = x, xend = xend, y = y, yend = yend, group = id),
                          alpha = alphae, data = edges_out, color = edge_color,
                          linewidth = edge_size, show.legend = FALSE)
  # Set node shape, color, and size
  if (!is.null(node_shape)) {
    if (node_shape %in% names(nodes_out)) {
      node_shape <- as.factor(nodes_out[[node_shape]])
      if (!any(grepl("circle|square|triangle", node_shape))) {
        node_shape <- c("circle", "square", "triangle")[node_shape]
      }
    }
  } else node_shape <- "circle"
  if (!is.null(node_color)) {
    if (node_color %in% names(nodes_out)) {
      node_color <- .check_color(nodes_out[[node_color]])
    }
  } else if (is.null(node_color) & "Infected" %in% names(nodes_out)) {
    node_color <- as.factor(ifelse(nodes_out[["Exposed"]], "Exposed",
                                   ifelse(nodes_out[["Infected"]],"Infected", 
                                          ifelse(nodes_out[["Recovered"]], "Recovered",
                                                 "Susceptible"))))
  } else node_color <- "darkgray"
  if (!is.null(node_size)) {
    if (node_size %in% names(nodes_out)) {
      node_size <- nodes_out[[node_size]]
    }
  } else if (nrow(nodes_out) > 100) {
    node_size <- 3
  } else node_size <- nrow(nodes_out)/length(unique(nodes_out$frame))
  # Add labels
  if (isTRUE(labels)) {
    p <- p + ggplot2::geom_text(aes(x, y, label = name), alpha = alphad,
                                data = nodes_out, color = "black",
                                hjust = -0.2, vjust = -0.2, show.legend = FALSE)
  }
  # Plot nodes
  if ("Infected" %in% names(nodes_out)) {
    p <- p + ggplot2::geom_point(aes(x, y, group = name, color = node_color),
                                 size = node_size, shape = node_shape, data = nodes_out) +
      ggplot2::scale_color_manual(name = NULL, values = c("Infected" = "#d73027",
                                                          "Susceptible" = "#4575b4",
                                                          "Exposed" = "#E6AB02",
                                                          "Recovered" = "#66A61E")) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "bottom")
  } else {
    p <- p + ggplot2::geom_point(aes(x, y, group = name), alpha = alphad,
                                 size = node_size, data = nodes_out,
                                 color = node_color, shape = node_shape,
                                 show.legend = FALSE) +
      ggplot2::theme_void()
  }
  p
}

# `graphd()` helper functions
.check_color <- function(v) {
  color <- grDevices::colors()
  color <- color[!color %in% "black"]
  v <- ifelse(is.na(v), "black", v)
  if (!any(grepl(paste(color, collapse = "|"), v)) | any(grepl("^#", v))) {
    for(i in unique(v)) {
      if (i != "black") {
        v[v == i] <- sample(color, 1)
      }
    }
  }
  v
}

time_edges_lst <- function(tlist, edges_lst, nodes_lst, edge_color) {
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

transition_edge_lst <- function(tlist, edges_lst, nodes_lst, all_edges) {
  x <- lapply(1:length(tlist), function(i) {
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

remove_isolates <- function(edges_out, nodes_out) {
  status <- frame <- from <- to <- framen <- NULL
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
