graph_layout <- function(g, layout, labels, node_group, snap, ...) {
  name <- NULL
  dots <- list(...)
  if ("x" %in% names(dots) & "y" %in% names(dots)) {
    lo <- ggraph::create_layout(g, layout = "manual",
                                x = dots[["x"]], y = dots[["y"]])
  } else lo <- suppressWarnings(ggraph::create_layout(g, layout, ...))
  if ("graph" %in% names(attributes(lo))) {
    if (!setequal(names(as.data.frame(attr(lo, "graph"))), names(lo))) {
      for (n in setdiff(names(as.data.frame(attr(lo, "graph"))), names(lo))) {
        lo[n] <- igraph::vertex_attr(g, n)
      }
    }
  }
  p <- ggraph::ggraph(lo) + ggplot2::theme_void()
  if (!is.null(node_group)) {
    # thisRequires("ggforce")
    p <- p + 
      ggforce::geom_mark_hull(ggplot2::aes(x, y, fill = node_group,
                                           label = node_group), data = lo) +
      ggplot2::scale_fill_manual(values = ag_qualitative(length(unique(p$data[[node_group]]))),
                                 guide = ggplot2::guide_legend("Group"))
  }
  if(snap){
    manynet::snet_info("Snapping layout coordinates to grid.")
    if(grepl("lattice", manynet::net_name(g), ignore.case = TRUE))
      p$data[,c("x","y")] <- round(p$data[,c("x","y")]) else 
        p$data[,c("x","y")] <- depth_first_recursive_search(p)
  }
  # Add background ----
  if(getOption("snet_background", default = "#FFFFFF")!="#FFFFFF")
    p <- p + ggplot2::theme(panel.background = ggplot2::element_rect(fill = getOption("snet_background", 
                                                                                      default = "#FFFFFF")))
  p
}
