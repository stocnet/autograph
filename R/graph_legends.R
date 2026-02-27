
graph_legends <- function(p, g, 
                          node_color = NULL, node_shape = NULL, node_size = NULL,
                          edge_color = NULL, edge_size = NULL) {
  p +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1,
                                                 title = ifelse(is.null(node_color), 
                                                                "Color", node_color)),
                    color = ggplot2::guide_legend(order = 2), 
                    shape = ggplot2::guide_legend(order = 3,
                                                  title = ifelse(is.null(node_shape),
                                                                 ifelse(manynet::is_twomode(g), "Mode", "Shape"),
                                                                 node_shape)),
                    size = ggplot2::guide_legend(order = 4,
                                                 title = ifelse(is.null(node_size),
                                                                "Size", node_size)),
                    linetype = ggplot2::guide_legend(order = 5),
                    edge_colour = ggplot2::guide_legend(order = 6,
                                                        title = ifelse(is.null(edge_color),
                                                                       ifelse(manynet::is_signed(g), "Sign", "Color"),
                                                                       edge_color)),
                    edge_size = ggplot2::guide_legend(order = 7,
                                                      title = ifelse(is.null(edge_size),
                                                                     ifelse(manynet::is_weighted(g), "Weight", "Size"),
                                                                     edge_size)),
                    alpha = ggplot2::guide_legend(order = 99,
                                                  override.aes = list( alpha = 0, size = 0, shape = NA )))
}