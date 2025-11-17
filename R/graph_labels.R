graph_labels <- function(p, g, layout) {
  if (layout == "circle" | layout == "concentric") {
    angles <- as.data.frame(.cart2pol(as.matrix(p[["data"]][,1:2])))
    angles$degree <- angles$phi * 180/pi
    angles <- dplyr::case_when(p[["data"]][,2] == 0 & p[["data"]][,1] == 0 ~ 0.1,
                               p[["data"]][,2] >= 0 & p[["data"]][,1] > 0 ~ angles$degree,
                               p[["data"]][,2] < 0 & p[["data"]][,1] > 0 ~ angles$degree,
                               p[["data"]][,1] == 1 ~ angles$degree,
                               TRUE ~ angles$degree - 180)
    if (manynet::net_nodes(g) < 10) {
      hj <- ifelse(p[["data"]][,1] >= 0, -0.8, 1.8)
    } else if (manynet::net_nodes(g) < 20) {
      hj <- ifelse(p[["data"]][,1] >= 0, -0.4, 1.4)
    } else {
      hj <- ifelse(p[["data"]][,1] >= 0, -0.2, 1.2)
    }
    p <- p + ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE,
                                    family = ag_font(),
                                    size = 3, hjust = hj, angle = angles) +
      ggplot2::coord_cartesian(xlim=c(-1.2,1.2), ylim=c(-1.2,1.2))
  } else if (layout %in% c("bipartite", "railway") | layout == "hierarchy" &
             length(unique(p[["data"]][["y"]])) <= 2) {
    p <- p + ggraph::geom_node_text(ggplot2::aes(label = name), angle = 90,
                                    family = ag_font(),
                                    size = 3, hjust = "outward", repel = TRUE,
                                    nudge_y = ifelse(p[["data"]][,2] == 1,
                                                     0.05, -0.05)) +
      ggplot2::coord_cartesian(ylim=c(-0.2, 1.2))
  } else if (layout == "hierarchy" & length(unique(p[["data"]][["y"]])) > 2) {
    p <- p + ggraph::geom_node_text(ggplot2::aes(label = name),
                                    family = ag_font(),
                                    size = 3, hjust = "inward", repel = TRUE)
  } else if (layout %in% c("alluvial", "lineage")) {
    p <- p + ggraph::geom_node_label(ggplot2::aes(label = name), size = 3,
                                     family = ag_font(),
                                     repel = TRUE, nudge_x = ifelse(p[["data"]][,1] == 1, 
                                                                    0.02, -0.02))
  } else {
    p <- p + ggraph::geom_node_label(ggplot2::aes(label = name),
                                     family = ag_font(),
                                     repel = TRUE, seed = 1234, size = 3)
  }
}

# Helper functions for .graph_labels()

.cart2pol <- function(xyz){
  stopifnot(is.numeric(xyz))
  if (is.vector(xyz) && (length(xyz) == 2 || length(xyz) == 
                         3)) {
    x <- xyz[1]
    y <- xyz[2]
    m <- 1
    n <- length(xyz)
  }
  else if (is.matrix(xyz) && (ncol(xyz) == 2 || ncol(xyz) == 
                              3)) {
    x <- xyz[, 1]
    y <- xyz[, 2]
    m <- nrow(xyz)
    n <- ncol(xyz)
  }
  else manynet::snet_abort("Input must be a vector of length 3 or a matrix with 3 columns.")
  phi <- atan2(y, x)
  r <- .hypot(x, y)
  if (n == 2) {
    if (m == 1) 
      prz <- c(phi, r)
    else prz <- cbind(phi, r)
  }
  else {
    if (m == 1) {
      z <- xyz[3]
      prz <- c(phi, r, z)
    }
    else {
      z <- xyz[, 3]
      prz <- cbind(phi, r, z)
    }
  }
  return(prz)
}

.hypot <- function (x, y) {
  if ((length(x) == 0 && is.numeric(y) && length(y) <= 1) || 
      (length(y) == 0 && is.numeric(x) && length(x) <= 1)) 
    return(vector())
  if (!is.numeric(x) && !is.complex(x) || !is.numeric(y) && 
      !is.complex(y)) 
    manynet::snet_abort("Arguments 'x' and 'y' must be numeric or complex.")
  if (length(x) == 1 && length(y) > 1) {
    x <- rep(x, length(y))
    dim(x) <- dim(y)
  }
  else if (length(x) > 1 && length(y) == 1) {
    y <- rep(y, length(x))
    dim(y) <- dim(x)
  }
  if ((is.vector(x) && is.vector(y) && length(x) != length(y)) || 
      (is.matrix(x) && is.matrix(y) && dim(x) != dim(y)) || 
      (is.vector(x) && is.matrix(y)) || is.matrix(x) && is.vector(y)) 
    manynet::snet_abort("Arguments 'x' and 'y' must be of the same size.")
  x <- abs(x)
  y <- abs(y)
  m <- pmin(x, y)
  M <- pmax(x, y)
  ifelse(M == 0, 0, M * sqrt(1 + (m/M)^2))
}

