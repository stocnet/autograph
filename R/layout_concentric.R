#' Concentric layout
#'
#' @description
#'   The "concentric" layout places the nodes on one or more circles,
#'   with each group of nodes on a circle of its own,
#'   and the groups ordered around those circles
#'   so that adjacent nodes are drawn close together.
#'   Where one group holds a single node, that node occupies the centre.
#' @name layout_concentric
#' @template param_ggraphlayouts
#' @param membership A node attribute or a vector to draw concentric circles.
#'   By default this is the two modes of a two-mode network.
#' @param radius A vector of radii at which the concentric circles
#'   should be located.
#'   By default this is equal placement around an empty centre,
#'   unless one (the core) is a single node,
#'   in which case this node occupies the centre of the graph.
#' @param order.by An attribute label indicating the (decreasing) order
#'   for the nodes around the circles.
#'   By default ordering is given by a bipartite placement that reduces
#'   the number of edge crossings.
#' @family mapping
#' @source
#'   Diego Diez, Andrew P. Hutchins and Diego Miranda-Saavedra. 2014.
#'   "Systematic identification of transcriptional regulatory modules from
#'   protein-protein interaction networks".
#'   _Nucleic Acids Research_, 42 (1) e6.
#' @examples
#' #graphr(ison_southern_women, layout = "concentric", membership = "type",
#' #           node_color = "type", node_size = 3)
#' @export
layout_concentric <- function(.data, membership, radius = NULL,
                              order.by = NULL,
                              circular = FALSE, times = 1000) {
  .data <- manynet::as_igraph(.data)
  if (any(igraph::vertex_attr(.data, "name") == "")) {
    ll <- unlist(lapply(seq_len(length(.data)), function(x) {
      ifelse(igraph::vertex_attr(.data, "name")[x] == "",
             paste0("ramdom", x), igraph::vertex_attr(.data, "name")[x])
    }))
    .data <- igraph::set_vertex_attr(.data, "name", value = ll)
  }
  if (missing(membership)) {
    if (manynet::is_twomode(.data)) membership <- manynet::node_is_mode(.data) else
      .abort_layout_arg("membership", "concentric", length(.data))
  } else {
    if (length(membership) > 1 & length(membership) != length(.data)) {
      .abort_layout_arg("membership", "concentric", length(.data))
    } else if (length(membership) != length(.data)) {
      membership <- .match_name(membership, igraph::vertex_attr_names(.data),
                                "membership", what = "node attribute")
      membership <- manynet::node_attribute(.data, membership)
    }
  }
  names(membership) <- manynet::node_names(.data)
  membership <- .to_list(membership)
  all_c  <- unlist(membership, use.names = FALSE)
  if (any(table(all_c) > 1)) {
    duplicated_nodes <- names(which(table(all_c) > 1))
    manynet::snet_abort(
      "The {.val concentric} layout draws each node in one circle only,",
      "but {.val {duplicated_nodes}} appear{?s} in more than one.",
      "Please check that {.arg membership} gives each node a single group.")
  }
  if (manynet::is_labelled(.data)) all_n <- manynet::node_names(.data) else 
    all_n <- 1:manynet::net_nodes(.data)
  sel_other  <- all_n[!all_n %in% all_c]
  if (length(sel_other) > 0) membership[[length(membership) + 1]] <- sel_other
  if (is.null(radius)) {
    radius <- seq(0, 1, 1/(length(membership)))
    if (length(membership[[1]]) == 1) 
      radius <- radius[-length(radius)] else radius <- radius[-1]
  }
  if (!is.null(order.by)) {
    order.values <- lapply(order.by, 
                           function(b) manynet::node_attribute(.data, b))
  } else {
    if (manynet::is_twomode(.data) & length(membership) == 2) {
      xnet <- manynet::as_matrix(manynet::to_multilevel(.data))[membership[[2-1]], 
                                              membership[[2]]]
      lo <- layout_tbl_graph_layered(manynet::as_igraph(xnet, twomode = TRUE))
      lo$names <- manynet::node_names(.data)
      if (ncol(lo) == 2) lo[,1] <- seq_len(dim(lo)[1])
      order.values <- lapply(1:0, function(x)
        if(ncol(lo) >= 3) sort(lo[lo[,2] == x,])[,3] 
        else sort(lo[lo[,2] == x,1])) 
    } else order.values <- membership[order(sapply(membership, length))]
  }
  res <- matrix(NA, nrow = length(all_n), ncol = 2)
  for (k in seq_along(membership)) {
    r <- radius[k]
    l <- order.values[[k]]
    if(manynet::is_labelled(.data))
      l <- match(l, manynet::node_names(.data))
    res[l, ] <- .get_coordinates(l, r)
  }
  .to_lo(res)
}

#' @rdname layout_concentric
#' @export
layout_tbl_graph_concentric <- layout_concentric

# Helper functions --------------------------------------------------------

# Turn a vector of memberships into a list of the nodes in each group.
.to_list <- function(members) {
  out <- lapply(sort(unique(members)), function(x){
    y <- which(members==x)
    if(!is.null(names(y))) names(y) else y
  })
  names(out) <- unique(members)
  out
}

# Space the nodes `x` evenly around a circle of radius `r`.
.get_coordinates <- function(x, r) {
  l <- length(x)
  d <- 360/l
  c1 <- seq(0, 360, d)
  c1 <- c1[1:(length(c1) - 1)]
  tmp <- t(vapply(c1,
                  function(cc) c(cos(cc * pi/180) *
                                   r, sin(cc *
                                            pi/180) * r),
                  FUN.VALUE = numeric(2)))
  rownames(tmp) <- x
  tmp
}
