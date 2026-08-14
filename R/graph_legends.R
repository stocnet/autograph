
graph_legends <- function(p, g, 
                          node_color = NULL, node_shape = NULL, node_size = NULL,
                          edge_color = NULL, edge_size = NULL) {
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