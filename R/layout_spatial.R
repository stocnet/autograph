#' Spatial layout
#'
#' @description
#'   The "spatial" layout places each node at its own position,
#'   as a spatial network records it (see `manynet::is_spatial()`):
#'   in 'x' and 'y' node attributes,
#'   or in 'lon' and 'lat' (or 'longitude' and 'latitude').
#'   `graphr()` uses it by default for a spatial network,
#'   such as one read by `manynet::read_geojson()`.
#'   The two axes share one scale, so that a distance drawn is the same
#'   whichever way it runs.
#'
#'   Longitudes and latitudes are drawn on an equirectangular projection
#'   centred on the network's mean latitude:
#'   each longitude is multiplied by the cosine of that latitude,
#'   so that a degree of longitude is drawn as long as it is on the ground there.
#'   This keeps the shape of a city or a region,
#'   though not of a continent.
#'
#'   A node without a position is placed at the centre of those with one.
#' @name layout_spatial
#' @template param_ggraphlayouts
#' @family mapping
#' @examples
#' net <- manynet::create_ring(4) |>
#'   manynet::mutate_nodes(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
#' graphr(net)
#' @export
layout_spatial <- function(.data, circular = FALSE, times = 1) {
  pos <- .node_positions(.data)
  if (is.null(pos)) {
    manynet::snet_abort(
      "The {.val spatial} layout needs the nodes' positions,",
      "as {.val x} and {.val y} or {.val lon} and {.val lat} node attributes.")
  }
  x <- pos$x
  y <- pos$y
  if (pos$geographic) {
    x <- x * cos(mean(y, na.rm = TRUE) * pi / 180)
  }
  missing <- is.na(x) | is.na(y)
  if (any(missing)) {
    manynet::snet_info("Placing {sum(missing)} node{?s} without a position",
                       "at the centre of the others.")
    x[missing] <- mean(x[!missing])
    y[missing] <- mean(y[!missing])
  }
  .to_lo(cbind(x, y))
}

#' @rdname layout_spatial
#' @export
layout_tbl_graph_spatial <- layout_spatial

# The positions of the nodes, or NULL where they have none: a pair of numeric
# node attributes, named as a plane names them ('x' and 'y') or as a globe does
# ('lon' and 'lat'), which `geographic` tells apart. These are the names
# `manynet::is_spatial()` reads.
.node_positions <- function(.data) {
  attrs <- manynet::net_node_attributes(.data)
  find <- function(aka) attrs[tolower(attrs) %in% aka][1]
  pairs <- list(list(x = "x", y = "y", geographic = FALSE),
                list(x = c("lon", "long", "lng", "longitude"),
                     y = c("lat", "latitude"), geographic = TRUE))
  for (pair in pairs) {
    vars <- c(find(pair$x), find(pair$y))
    if (anyNA(vars)) next
    x <- manynet::node_attribute(.data, vars[1])
    y <- manynet::node_attribute(.data, vars[2])
    if (!is.numeric(x) || !is.numeric(y)) next
    return(list(x = as.numeric(x), y = as.numeric(y),
                geographic = pair$geographic))
  }
  NULL
}

# The length of each tie, in the order igraph holds them, or NULL where the
# network records none. A length is a distance (see `manynet::tie_lengths()`),
# so a long tie is drawn long and a short one counts as a strong one.
.tie_lengths_of <- function(g) {
  len <- igraph::edge_attr(manynet::as_igraph(g), "length")
  if (is.numeric(len) && length(unique(stats::na.omit(len))) > 1)
    as.numeric(len) else NULL
}
