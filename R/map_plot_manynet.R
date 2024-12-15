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
                                                         .1, .1, .01))) +
      ggplot2::geom_density(col = 2) +
      ggplot2::scale_y_continuous("Frequency", sec.axis = 
                                    ggplot2::sec_axis(~ ., breaks = c(0,1),
                                                      name = "Density"))
  } else if (length(type) == 1 & type == "h") {
    p <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data$Score)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                              binwidth = ifelse(max(data$Score) > 1, 1,
                                                ifelse(max(data$Score) >
                                                         .1, .1, .01))) +
      ggplot2::labs(x = "Density", y = "Frequency")
  } else if (length(type) == 1 & type == "d") {
    p <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data$Score)) +
      ggplot2::geom_density(col = 2) +
      ggplot2::ylab("Density")
  }
  p +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey90"))
}

#' @rdname map_measure
#' @export
plot.tie_measure <- function(x, type = c("h", "d"), ...) {
  type <- match.arg(type)
  data <- data.frame(Score = x)
  if (type == "h") {
    p <- ggplot2::ggplot(data = data) +
      ggplot2::geom_histogram(ggplot2::aes(x = .data$Score),
                              binwidth = ifelse(max(data$Score) > 1, 1,
                                                ifelse(max(data$Score) > .1,
                                                       .1,
                                                       .01))) +
      ggplot2::ylab("Frequency")
  } else {
    p <- ggplot2::ggplot(data = data) +
      ggplot2::geom_density(ggplot2::aes(x = .data$Score)) +
      ggplot2::ylab("Density")
  }
  p + ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey90"))
}

#' @rdname map_measure
#' @export
plot.network_measures <- function(x, ...) {
  ggplot2::ggplot(data = x, ggplot2::aes(x = .data$time, y = .data$value)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
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
#' @importFrom stats cutree
#' @importFrom ggdendro ggdendrogram
#' @export
plot.node_member <- function(x, ...) {
  hc <- attr(x, "hc")
  k <- attr(x, "k")
  memb <- x[hc$order]
  clust <- memb[!duplicated(memb)]
  colors <- ifelse(match(memb, clust) %% 2,
                   "#000000", "#E20020")
  ggdendro::ggdendrogram(hc, rotate = TRUE) +
    ggplot2::geom_hline(yintercept = hc$height[length(hc$order) - k],
                        linetype = 2,
                        color = "#E20020") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(colour = "#5c666f"),
                   axis.text.y = suppressWarnings(
                     ggplot2::element_text(colour = colors)))
}

#' @rdname map_member
#' @param membership A "node_member" membership vector.
#' @importFrom ggplot2 ggplot geom_tile aes scale_fill_gradient theme_grey labs theme scale_x_discrete scale_y_discrete geom_vline geom_hline element_blank element_text
#' @importFrom manynet is_twomode
#' @examples
#' plot(as_matrix(ison_adolescents),
#'   membership = node_in_regular(ison_adolescents, "e"))
#' plot(as_matrix(ison_southern_women),
#'   membership = node_in_regular(ison_southern_women, "e"))
#' @export
plot.matrix <- function(x, ..., membership = NULL) {
  
  if (!manynet::is_twomode(x)) {
    blocked_data <- manynet::as_matrix(x)
    if (!is.null(membership)) blocked_data <- blocked_data[order(membership),
                                                           order(membership)]
  } else if (manynet::is_twomode(x) &&
             length(intersect(membership[!manynet::node_is_mode(x)], 
                              membership[!manynet::node_is_mode(x)])) > 0) {
    blocked_data <- manynet::as_matrix(manynet::to_multilevel(x))
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
        colour = "grey50"
      ),
      axis.text.x = ggplot2::element_text(
        size = 9 * 0.8,
        angle = 30, hjust = 0,
        colour = "grey50"
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
        high = "black"
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
      xintercept = c(1 + which(diff(membership[order(membership)]) != 0))
      - .5,
      colour = "red"
    ) +
      ggplot2::geom_hline(
        yintercept = nrow(blocked_data) -
          c(1 + which(diff(membership[order(membership)]) != 0)) +
          1.5,
        colour = "red"
      )
  } else {
    g <- g +
      ggplot2::scale_y_discrete(expand = c(0, 0),
                                limits = rev(rownames(x[["blocked.data"]])[x[["order.vector"]][["nodes1"]]])
      ) +
      ggplot2::scale_x_discrete(expand = c(0, 0), position = "top",
                                limits = colnames(x[["blocked.data"]])[x[["order.vector"]][["nodes2"]]]
      ) +
      ggplot2::geom_vline(
        xintercept =
          c(1 + which(diff(x[["block.membership"]][["nodes2"]]) != 0))
        - .5,
        colour = "blue"
      ) +
      ggplot2::geom_hline(
        yintercept = nrow(x[["blocked.data"]])
        - c(1 + which(diff(x[["block.membership"]][["nodes1"]]) != 0))
        + 1.5,
        colour = "red"
      )
  }
  g
}

elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})


