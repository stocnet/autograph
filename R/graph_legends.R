
graph_legends <- function(p, g, 
                          node_color = NULL, node_shape = NULL, node_size = NULL,
                          edge_color = NULL, edge_size = NULL) {
  .check_legend_size(g, node_color, node_shape, edge_color)
  p +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1,
                                                 title = ifelse(is.null(node_color), 
                                                                "Color", node_color),
                                                 override.aes = list(shape = 21)),
                    color = ggplot2::guide_legend(order = 2), 
                    shape = ggplot2::guide_legend(order = 3,
                                                  title = ifelse(is.null(node_shape),
                                                                 ifelse(manynet::is_twomode(g), "Mode", "Shape"),
                                                                 node_shape)),
                    size = ggplot2::guide_legend(order = 4,
                                                 title = ifelse(is.null(node_size),
                                                                "Size", node_size)),
                    linetype = ggplot2::guide_legend(order = 5),
                    # `.infer_ecolor_title()` decides this alongside the colours
                    # themselves in R/graph_aes.R, so that the two cannot
                    # disagree about what the colour is showing, as they did
                    # when this said "Sign" over colours that showed layers.
                    edge_colour = ggplot2::guide_legend(
                      order = 6, title = .infer_ecolor_title(g, edge_color)),
                    edge_size = ggplot2::guide_legend(order = 7,
                                                      title = ifelse(is.null(edge_size),
                                                                     ifelse(manynet::is_weighted(g), "Weight", "Size"),
                                                                     edge_size)),
                    alpha = ggplot2::guide_legend(order = 99,
                                                  override.aes = list( alpha = 0, size = 0, shape = NA )))
}

# A legend is read by matching a key against a mark, and a reader cannot hold
# many keys in mind while doing it: colours in particular are not recalled
# reliably. Beyond about seven the legend stops helping, and on a small or
# projected figure it stops fitting. Said once for the whole plot, from the
# widest of the categorical mappings, rather than once for each of them.
.legend_max_keys <- 7L

.check_legend_size <- function(g, node_color = NULL, node_shape = NULL,
                               edge_color = NULL){
  levels_of <- function(name, attrs, values){
    if(is.null(name) || length(name) != 1L || !is.character(name)) return(0L)
    if(!name %in% attrs) return(0L)
    vals <- values(g, name)
    if(is.numeric(vals) && !is.factor(vals)) return(0L)
    length(unique(vals[!is.na(vals)]))
  }
  n <- max(
    levels_of(node_color, igraph::vertex_attr_names(g), manynet::node_attribute),
    levels_of(node_shape, igraph::vertex_attr_names(g), manynet::node_attribute),
    levels_of(edge_color, igraph::edge_attr_names(g), manynet::tie_attribute))
  if(n <= .legend_max_keys) return(invisible(NULL))
  manynet::snet_info(
    "The legend will hold {n} keys, which is more than most readers can match",
    "against the graph. Consider grouping the smaller categories together,",
    "or showing them with {.arg node_group} instead.")
  invisible(NULL)
}
