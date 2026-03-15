# Marks ####

#' Plotting logical marks

# Measures ####

#' Plotting numeric measures
#' @description
#'   These functions plot distributions for node, tie, and network measures,
#'   as defined in the `{manynet}` package.
#' @name map_measure
#' @param x An object of "node_measure", "tie_measure", 
#'   or "network_measures" class.
#' @param ... Other arguments to be passed on.
#' @param type For node and tie measures, whether the plot should be 
#'   "h" a histogram or "d" a density plot. By default "h".
#' @returns `plot.node_measure()` and `plot.tie_measure()` returns a histogram 
#'   and/or density plot of the distribution of the measure.
#' @examples
#' plot(netrics::node_by_deg(ison_karateka))
#' @export
plot.node_measure <- function(x, type = c("h", "d"), ...) {
  #type <- match.arg(type)
  density <- NULL
  if (is.null(attr(x, "mode"))) attr(x, "mode") <- rep(FALSE, length(x))
  data <- data.frame(Score = x, Mode = attr(x, "mode"))
  if (length(type) == 2) {
    p <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data$Score)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                              binwidth = ifelse(max(data$Score) > 1, 1,
                                                ifelse(max(data$Score) > 
                                                         .1, .1, .01)),
                              fill = ag_base()) +
      ggplot2::geom_density(colour = ag_highlight(),
                            linewidth = 1.5) +
      ggplot2::scale_y_continuous("Frequency", sec.axis = 
                                    ggplot2::sec_axis(~ ., breaks = c(0,1),
                                                      name = "Density"))
  } else if (length(type) == 1 & type == "h") {
    p <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data$Score)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                              binwidth = ifelse(max(data$Score) > 1, 1,
                                                ifelse(max(data$Score) >
                                                         .1, .1, .01)),
                              fill = ag_base()) +
      ggplot2::labs(x = "Density", y = "Frequency")
  } else if (length(type) == 1 & type == "d") {
    p <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data$Score)) +
      ggplot2::geom_density(colour = ag_highlight(),
                            linewidth = 1.5) +
      ggplot2::ylab("Density")
  }
  p +
    ggplot2::theme_classic(base_family = ag_font()) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey90"))
}

#' @rdname map_measure
#' @examples
#' plot(netrics::tie_by_betweenness(ison_karateka))
#' @export
plot.tie_measure <- function(x, type = c("h", "d"), ...) {
  # type <- match.arg(type)
  data <- data.frame(Score = x)
  if (length(type) == 2) {
    p <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data$Score)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                              binwidth = ifelse(max(data$Score) > 1, 1,
                                                ifelse(max(data$Score) > 
                                                         .1, .1, .01)),
                              fill = ag_base()) +
      ggplot2::geom_density(colour = ag_highlight(),
                            linewidth = 1.5) +
      ggplot2::scale_y_continuous("Frequency", sec.axis = 
                                    ggplot2::sec_axis(~ ., breaks = c(0,1),
                                                      name = "Density"))
  } else if (length(type) == 1 & type == "h") {
    p <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data$Score)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                              binwidth = ifelse(max(data$Score) > 1, 1,
                                                ifelse(max(data$Score) >
                                                         .1, .1, .01)),
                              fill = ag_base()) +
      ggplot2::labs(x = "Density", y = "Frequency")
  } else if (length(type) == 1 & type == "d") {
    p <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data$Score)) +
      ggplot2::geom_density(colour = ag_highlight(),
                            linewidth = 1.5) +
      ggplot2::ylab("Density")
  }
  p + ggplot2::theme_classic(base_family = ag_font()) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey90"))
}

#' @rdname map_measure
#' @returns `plot.network_measures()` returns a plot of the measure traced over
#'   time.
#' @export
plot.network_measures <- function(x, ...) {
  ggplot2::ggplot(data = x, ggplot2::aes(x = .data$time, y = .data$value)) +
    ggplot2::geom_line(colour = ag_highlight()) +
    ggplot2::theme_minimal(base_family = ag_font()) +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Value")
}

# Memberships ####

#' Plotting categorical memberships
#' @description
#'   This plotting method operates on "node_member" class objects from the 
#'   `{manynet}` package, plotting the dendrogram of their membership.
#' @name map_member
#' @param x An object of "node_member" class, for example as a result of
#'   running `manynet::node_in_community()`.
#' @inheritParams map_measure
#' @returns `plot.node_member()` returns a dendrogram, with labels colored to
#'   indicate the different clusters, and with the optimal cutpoint shown by a
#'   dashed highlight line.
#' @importFrom stats cutree
#' @importFrom ggdendro ggdendrogram
#' @examples
#' plot(netrics::node_in_walktrap(ison_southern_women, "e"))
#' @export
plot.node_member <- function(x, ...) {
  hc <- attr(x, "hc")
  k <- attr(x, "k")
  memb <- x[hc$order]
  clust <- memb[!duplicated(memb)]
  colors <- ifelse(match(memb, clust) %% 2,
                   ag_positive(), ag_negative())
  ggdendro::ggdendrogram(hc, rotate = TRUE) +
    ggplot2::geom_hline(yintercept = hc$height[length(hc$order) - k],
                        linetype = 2,
                        color = ag_highlight()) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(colour = ag_base()),
                   axis.text.y = suppressWarnings(
                     ggplot2::element_text(colour = colors)))
}

# #' @export
# plot.node_members <- function(x, ...) {
#   df <- x %>% dplyr::mutate(wave = dplyr::row_number())
#   df_long <- df %>%
#     tidyr::pivot_longer(-wave, names_to = "person", values_to = "group")
# group_counts <- df_long %>%
#   dplyr::group_by(wave, group) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop")
#
# # Step 1: Reshape to wide format: one row per person, one column per wave
# df_wide <- df_long %>%
#   dplyr::mutate(wave = paste0("wave", wave)) %>%
#   tidyr::pivot_wider(names_from = wave, values_from = group)
#
# # Step 2: Create a vector of wave columns for use as axes
# wave_cols <- names(df_wide)[!names(df_wide) %in% "person"]
#
# # Create a mapping list like list(axis1 = wave1, axis2 = wave2, ...)
# aes_mapping <- list(setNames(wave_cols, paste0("axis", seq_along(wave_cols))))
#
# # Use do.call to inject into aes
# ggplot2::ggplot(df_wide) +
#   do.call(ggalluvial::geom_alluvium, c(list(aes = do.call(ggplot2::aes, aes_mapping),
#                                 fill = "steelblue"))) +
#   do.call(ggalluvial::geom_stratum,
#           list(width = 1/12, fill = "grey70", color = "black")) +
#   ggplot2::geom_text(stat = "stratum",
#                      ggplot2::aes(label = ggplot2::after_stat(stratum))) +
#   ggplot2::scale_x_discrete(labels = paste("Wave", seq_along(wave_cols))) +
#   ggplot2::theme_minimal()

#   # Step 1: Reshape to wide format with one row per person
#   df_wide <- df_long |>
#     dplyr::mutate(wave = paste0("wave", wave)) |>
#     tidyr::pivot_wider(names_from = wave, values_from = group) |>
#     dplyr::mutate(alluvium = dplyr::row_number())
# 
#   # Step 2: Identify the wave columns
#   wave_cols <- setdiff(names(df_wide), c("person", "alluvium"))
# 
#   # Step 3: Build aes mapping dynamically for wide format
#   aes_list <- rlang::set_names(rlang::syms(wave_cols), paste0("axis", seq_along(wave_cols)))
#   aes_list$alluvium <- rlang::sym("alluvium")
#   aes_mapping <- do.call(ggplot2::aes, aes_list)
# 
#   # Step 4: Construct and return the plot
#   ggplot2::ggplot(df_wide) +
#     ggalluvial::geom_alluvium(aes_mapping, stat = "alluvium", fill_color, alpha = 0.8) +
#     ggalluvial::geom_stratum(stat = "alluvium", width = 1 / 12, fill = "grey70", color = "black") +
#     ggplot2::geom_text(
#       stat = ggalluvial::StatStratum,
#       mapping = ggplot2::aes(label = ggplot2::after_stat(stratum)),
#       size = 3
#     ) +
#     ggplot2::scale_x_discrete(labels = paste("Wave", seq_along(wave_cols))) +
#     ggplot2::theme_minimal()
# 
# }

#' @rdname map_member
#' @param membership A "node_member" membership vector.
#' @importFrom ggplot2 ggplot geom_tile aes scale_fill_gradient theme_grey labs
#' @importFrom ggplot2 theme scale_x_discrete scale_y_discrete geom_vline
#' @importFrom ggplot2 geom_hline element_blank element_text
#' @importFrom manynet is_twomode
#' @returns `plot.matrix()` returns a plot of an adjacency or incidency matrix,
#'   potentially with the rows and columns reordered to illustrate an additional
#'   membership vector.
#' @examples
#' plot(as_matrix(ison_adolescents),
#'   membership = netrics::node_in_walktrap(ison_adolescents, "e"))
#' plot(as_matrix(ison_southern_women),
#'   membership = netrics::node_in_walktrap(ison_southern_women, "e"))
#' @export
plot.matrix <- function(x, ..., membership = NULL) {
  
  if (!manynet::is_twomode(x)) {
    blocked_data <- manynet::as_matrix(x)
    if (!is.null(membership)) blocked_data <- blocked_data[order(membership),
                                                           order(membership)]
  } else if (manynet::is_twomode(x) &&
             length(intersect(membership[!manynet::node_is_mode(x)], 
                              membership[!manynet::node_is_mode(x)])) > 0) {
    blocked_data <- manynet::to_multilevel(x)
    if (!is.null(membership)) blocked_data <- blocked_data[order(membership),
                                                           order(membership)]
  } else {
    blocked_data <- manynet::as_matrix(x)
  }
  
  plot_data <- manynet::as_edgelist(blocked_data)
  if(!manynet::is_labelled(x)){
    indices <- c(plot_data$from,plot_data$to)
    plot_data$from <- paste0("N", gsub("\\s", "0", 
                                       format(plot_data$from, 
                                              width=max(nchar(indices)))))
    plot_data$to <- paste0("N", gsub("\\s", "0", 
                                     format(plot_data$to, 
                                            width=max(nchar(indices)))))
  }
  plot_data$weight <- 1
  all_nodes <- expand.grid(manynet::node_names(blocked_data), 
                           manynet::node_names(blocked_data))
  all_nodes <- data.frame(from = all_nodes$Var1, to = all_nodes$Var2,
                          weight = 0)
  plot_data <- rbind(plot_data, all_nodes) %>% 
    dplyr::distinct(from, to, .keep_all = TRUE)
  g <- ggplot2::ggplot(plot_data, ggplot2::aes(to, from)) +
    ggplot2::theme_grey(base_size = 9) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(
      legend.position = "none",
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        size = 9 * 0.8,
        colour = ag_base()
      ),
      axis.text.x = ggplot2::element_text(
        size = 9 * 0.8,
        angle = 30, hjust = 0,
        colour = ag_base()
      )
    ) +
    # ggplot2::geom_tile(ggplot2::aes(fill = .data[["value"]]),
    ggplot2::geom_tile(ggplot2::aes(fill = weight),
                       colour = "white"
    )
  
  # Color for signed networks
  if (manynet::is_signed(x)) {
    g <- g +
      ggplot2::scale_fill_gradient2(high = "#003049",
                                    mid = "white",
                                    low = "#d62828")
  } else {
    g <- g +
      ggplot2::scale_fill_gradient(
        low = "white",
        high = ag_base()
      )
  }
  
  # Structure for multimodal networks
  if (!manynet::is_twomode(x)) {
    g <- g +
      ggplot2::scale_x_discrete(expand = c(0, 0), position = "top",
                                limits = colnames(blocked_data)
      ) +
      ggplot2::scale_y_discrete(expand = c(0, 0),
                                limits = rev(rownames(blocked_data))
      )
    if (!is.null(membership))
      if(!is.numeric(membership)) membership <- as.numeric(as.factor(membership))
    g <- g + ggplot2::geom_vline(
      xintercept = c(1 + which(diff(membership[order(membership)]) != 0)) - .5,
      colour = ag_highlight()
    ) +
      ggplot2::geom_hline(
        yintercept = nrow(blocked_data) -
          c(1 + which(diff(membership[order(membership)]) != 0)) + 1.5,
        colour = ag_highlight()
      )
  } else {
    group_boundaries <- diff(as.numeric(factor(membership[order(membership)])))
    g <- g +
      ggplot2::scale_y_discrete(expand = c(0, 0),
                                limits = rev(rownames(blocked_data))
      ) +
      ggplot2::scale_x_discrete(expand = c(0, 0), position = "top",
                                limits = colnames(blocked_data)
      ) +
      ggplot2::geom_vline(
        xintercept =
          c(1 + which(group_boundaries != 0)) - .5,
        colour = ag_positive()
      ) +
      ggplot2::geom_hline(
        yintercept = nrow(blocked_data) - 
          c(1 + which(group_boundaries != 0)) + 1.5,
        colour = ag_negative()
      )
  }
  g
}

elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})


# Motifs ####

#' Plotting tabular motifs
#' @description
#'   These functions will plot graphs of the motifs used in a vector of 
#'   results of e.g. a triad census.
#' @name map_motifs
#' @param x An object of "node_motif" class, e.g. resulting from a call to
#'   `manynet::node_by_triad()`.
#' @inheritParams map_measure
#' @returns `plot.node_motif()` returns a set of graphs that illustrate the
#'   motifs mentioned in the results from a node_motif function in `{manynet}`.
#' @export
plot.node_motif <- function(x, ...) {
  motifs <- dimnames(x)[[2]]
  if("X4" %in% motifs){
    graphs(manynet::create_motifs(4), waves = 1:11)
  } else if("021D" %in% motifs){
    graphs(manynet::create_motifs(3, directed = TRUE), waves = 1:16)
  } else if("102" %in% motifs){
    graphs(manynet::create_motifs(3), waves = 1:4)
  } else if("Asymmetric" %in% motifs){
    graphs(manynet::create_motifs(2, directed = TRUE), waves = 1:3)
  } else if("Mutual" %in% motifs){
    graphs(manynet::create_motifs(2), waves = 1:2)
  } else stop("Cannot plot these motifs yet, sorry.")
}

#' @rdname map_motifs
#' @returns `plot.network_motif()` returns a set of graphs that illustrate the
#'   motifs mentioned in the results from a net_motif function in `{manynet}`.
#' @export
plot.network_motif <- function(x, ...) {
  motifs <- attr(x, "names")
  if("X4" %in% motifs){
    graphs(manynet::create_motifs(4), waves = 1:11)
  } else if("021D" %in% motifs){
    graphs(manynet::create_motifs(3, directed = TRUE), waves = 1:16)
  } else if("102" %in% motifs){
    graphs(manynet::create_motifs(3), waves = 1:4)
  } else if("Asymmetric" %in% motifs){
    graphs(manynet::create_motifs(2, directed = TRUE), waves = 1:3)
  } else if("Mutual" %in% motifs){
    graphs(manynet::create_motifs(2), waves = 1:2)
  } else stop("Cannot plot these motifs yet, sorry.")
}
