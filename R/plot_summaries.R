# Models ####

#' Plotting diffusion models
#' @name plot.diffusion
#' @param x A "diff_model" of "diffs_model" class of object.
#'   E.g. as a result from `manynet::play_diffusion()`.
#' @param ... Other arguments to be passed.
#' @param all_steps Whether all steps should be plotted or just those where
#'   there is change in the distributions.
#' @importFrom dplyr left_join
#' @importFrom ggplot2 geom_histogram
#' @returns `plot.diff_model()` returns a bar chart of the number of new 
#'   infected nodes at each time point, as well as an overlay line plot of the
#'   total of infected
#' @examples
#' plot(res_manynet_diff)
#' @export
plot.diff_model <- function(x, ..., all_steps = TRUE){
  # if(nrow(x)==1) snet_warn("No diffusion observed.") else {
  if(nrow(x)==1) warning("No diffusion observed.") else {
    data <- x
    if(!all_steps) data <- data %>% 
        dplyr::filter(!(data$I==data$I[length(data$I)] *
                          duplicated(data$I==data$I[length(data$I)])))
    p <- ggplot2::ggplot(data) + 
      ggplot2::geom_line(ggplot2::aes(x = time, y = S/n, color = "A"), 
                         linewidth = 1.25) +
      ggplot2::geom_line(ggplot2::aes(x = time, y = I/n, color = "C"), 
                         linewidth = 1.25) +
      ggplot2::geom_col(ggplot2::aes(x = time, y = I_new/n), 
                        alpha = 0.4) +
      ggplot2::theme_minimal() + 
      # using coord_cartesian to avoid printing warnings
      ggplot2::coord_cartesian(ylim = c(0,1)) + 
      ggplot2::scale_x_continuous(breaks = function(x) pretty(x, n=6)) +
      ggplot2::ylab("Proportion") + ggplot2::xlab("Steps")
    labs <- c("Susceptible", "Infected")
    if(any(data$E>0)){
      p <- p +
        ggplot2::geom_line(ggplot2::aes(x = time, y = E/n, color = "B"), 
                           linewidth = 1.25)
      labs <- c("Susceptible", "Exposed", "Infected")
    }
    if(any(data$R>0)){
      p <- p +
        ggplot2::geom_line(ggplot2::aes(x = time, y = R/n, color = "D"),
                           linewidth = 1.25)
      labs <- c(labs, "Recovered")
    }
    
    colval <- structure(match_color(c("#4575b4","#E6AB02","#d73027","#66A61E")), 
                         names = c("A","B","C","D"))
    p + ggplot2::scale_color_manual("Legend", 
                                    labels = labs,
                                    values = colval,
                                    guide = "legend")
  }
}

#' @noRd
#' @export
plot.mnet <- function(x, ...){
  if(manynet::is_changing(x)){
    plot(manynet::as_diffusion(x))
  } else {
    graphr(x)
  }
}

#' @rdname plot.diffusion
#' @examples
#' plot(res_migraph_diff)
#' @export
plot.diffs_model <- function(x, ...){
  S <- E <- I <- R <- n <- NULL # initialize variables to avoid CMD check notes
  data <- dplyr::tibble(x)
  # ggplot2::ggplot(data) + geom_smooth()
  p <- ggplot2::ggplot(data) + 
    # ggplot2::geom_point(ggplot2::aes(x = t, y = S/n))
    ggplot2::geom_smooth(ggplot2::aes(x = time, y = S/n, color = "A"), 
                         method = "loess", se=TRUE, level = .95, formula = 'y~x') +
    ggplot2::geom_smooth(ggplot2::aes(x = time, y = I/n, color = "C"), 
                         method = "loess", se=TRUE, level = .95, formula = 'y~x') +
    ggplot2::theme_minimal() + 
    ggplot2::coord_cartesian(ylim = c(0,1)) + # using coord_cartesion to avoid printing warnings
    ggplot2::scale_x_continuous(breaks = function(x) pretty(x, n=6)) +
    ggplot2::ylab("Proportion") + ggplot2::xlab("Steps")
  labs <- c("Susceptible", "Infected")
  if(any(data$E>0)){
    p <- p +
      ggplot2::geom_smooth(ggplot2::aes(x = time, y = E/n, color = "B"), 
                           method = "loess", se=TRUE, level = .95, formula = 'y~x')
    labs <- c("Susceptible", "Exposed", "Infected")
  }
  if(any(data$R>0)){
    p <- p +
      ggplot2::geom_smooth(ggplot2::aes(x = time, y = R/n, color = "D"), 
                           method = "loess", se=TRUE, level = .95, formula = 'y~x')
    labs <- c(labs, "Recovered")
  }
  
  colvals <- structure(match_color(c("#4575b4","#E6AB02","#d73027","#66A61E")), 
            names = c("A","B","C","D"))
  p + ggplot2::scale_color_manual("Legend", 
                                  labels = labs,
                                  values = colvals,
                                  guide = "legend")
}

#' @rdname plot.diffusion
#' @examples
#' plot(play_learning(ison_networkers, beliefs = runif(net_nodes(ison_networkers))))
#' @export
plot.learn_model <- function(x, ...){
  y <- t(x)
  colnames(y) <- paste0("t",0:(ncol(y)-1))
  y <- as.data.frame.table(y)
  y$Step <- as.numeric(gsub("t", "", y$Var2))
  ggplot2::ggplot(y, ggplot2::aes(x = Step, y = Freq, color = Var1)) + 
    ggplot2::geom_line(show.legend = FALSE) + ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = ag_qualitative(ncol(x))) +
    ggplot2::ylab("Belief")
}

#' #' Plot agreements network
#' #' 
#' #' @description Facilitates plotting of 'many' data.
#' #' @param dataset A dataset from one of the many packages
#' #' or a "consolidated" database.
#' #' @param actor An actor variable.
#' #' "stateID", by default.
#' #' @param treaty_type The type of treaties to be returned.
#' #' NULL, by default.
#' #' Other options are "bilateral" or "multilateral".
#' #' @param key An ID column to collapse by.
#' #' By default "manyID".
#' #' @param layout How do you want the plot to look like?
#' #' An `{ggraph}` layout algorithm.
#' #' If not declared, reasonable defaults are used.
#' #' @name plot_
#' NULL
#' 
#' #' @rdname plot_
#' #' @importFrom dplyr %>% select mutate distinct rename
#' #' @return A network of agreements' relations.
#' #' @examples
#' #' \donttest{
#' #' #agreements <- dplyr::filter(manyenviron::agreements$ECOLEX,
#' #' #Begin > "2000-01-01" & Begin < "2000-12-12")
#' #' #plot_agreements(agreements)
#' #'}
#' #' @export
#' plot_agreements <- function(dataset, treaty_type = NULL, key = "manyID",
#'                             layout = "circle") {
#'   manyID <- treatyID <- name <- NULL
#'   if (key == "manyID") {
#'     out <- dplyr::select(dataset, manyID) %>%
#'       dplyr::rename(key = manyID) %>%
#'       dplyr::distinct()
#'   } else if (key == "treatyID") {
#'     out <- dplyr::select(dataset, treatyID) %>%
#'       dplyr::rename(key == treatyID) %>%
#'       dplyr::distinct()
#'   } else cli::cli_abort("Please declare either 'manyID' or 'treatyID'.")
#'   if (!is.null(treaty_type)) {
#'     if (treaty_type == "bilateral") {
#'       out <- out[grep("-", out$key),]
#'     }
#'     if (treaty_type == "multilateral") {
#'       out <- out[grep("-", out$key, invert = TRUE),]
#'     }
#'   }
#'   dplyr::mutate(out,
#'                 link = ifelse(grepl(":", key), sapply(strsplit(key, ":"),
#'                                                       "[", 2), "NA"),
#'                 key = gsub("\\:.*", "", key)) %>%
#'     as_tidygraph() %>%
#'     dplyr::filter(name != "NA") %>%
#'     graphr(layout = layout)
#' }
#' 
#' #' @rdname plot_
#' #' @importFrom dplyr %>% select distinct all_of rename
#' #' @return A network of agreements' memberships.
#' #' @examples
#' #' \donttest{
#' #' #memberships <- dplyr::filter(manyenviron::memberships$ECOLEX_MEM,
#' #' #Begin > "2000-01-01" & Begin < "2000-01-31")
#' #' #plot_memberships(memberships)
#' #'}
#' #' @export
#' plot_memberships <- function(dataset, actor = "stateID", treaty_type = NULL,
#'                             key = "manyID", layout = "bipartite") {
#'   manyID <- treatyID <- name <- NULL
#'   if (key == "manyID") {
#'     out <- dplyr::select(dataset, manyID, dplyr::all_of(actor)) %>%
#'       dplyr::rename(key = manyID) %>%
#'       dplyr::distinct()
#'   } else if (key == "treatyID") {
#'     out <- dplyr::select(dataset, treatyID, dplyr::all_of(actor)) %>%
#'       dplyr::rename(key == treatyID) %>%
#'       dplyr::distinct()
#'   } else cli::cli_abort("Please declare either 'manyID' or 'treatyID'.")
#'   if (!is.null(treaty_type)) {
#'     if (treaty_type == "bilateral") {
#'       out <- out[grep("-", out$key),]
#'     }
#'     if (treaty_type == "multilateral") {
#'       out <- out[grep("-", out$key, invert = TRUE),]
#'     }
#'   }
#'   stats::na.omit(out) %>%
#'     as_tidygraph() %>%
#'     mutate(type = ifelse(grepl("[0-9][0-9][0-9][0-9][A-Za-z]",
#'                                         name), TRUE, FALSE)) %>%
#'     graphr(layout = layout)
#' }
#' 
#' #' @rdname plot_
#' #' @importFrom dplyr %>% select mutate distinct filter rename
#' #' @return A plot of agreements' lineages.
#' #' @examples
#' #' \donttest{
#' #' #lineage <- dplyr::filter(manyenviron::agreements$HUGGO,
#' #' #Begin > "2000-01-01", Begin < "2001-12-31")
#' #' #plot_lineage(lineage)
#' #' }
#' #' @export
#' plot_lineage <- function(dataset, treaty_type = NULL, key = "manyID",
#'                          layout = "nicely") {
#'   manyID <- treatyID <- name <- NULL
#'   if (key == "manyID") {
#'     out <- dplyr::select(dataset, manyID) %>%
#'       dplyr::rename(key = manyID) %>%
#'       dplyr::distinct()
#'   } else if (key == "treatyID") {
#'     out <- dplyr::select(dataset, treatyID) %>%
#'       dplyr::rename(key == treatyID) %>%
#'       dplyr::distinct()
#'   } else cli::cli_abort("Please declare either 'manyID' or 'treatyID'.")
#'   if (!is.null(treaty_type)) {
#'     if (treaty_type == "bilateral") {
#'       out <- out[grep("-", out$key),]
#'     }
#'     if (treaty_type == "multilateral") {
#'       out <- out[grep("-", out$key, invert = TRUE),]
#'     }
#'   }
#'   out %>%
#'     dplyr::filter(grepl(":", key)) %>%
#'     dplyr::mutate(key1 = gsub(".*\\:", "", key),
#'                   key = gsub("\\:.*", "", key)) %>%
#'     dplyr::distinct() %>%
#'     as_tidygraph() %>%
#'     graphr(layout = "nicely")
#' } 
#' 
#' #' @rdname plot_
#' #' @param date String date from the network snapshot.
#' #' Used by \code{{cshapes}} to plot the correct map.
#' #' By default, 2019-12-31.
#' #' Date can be between 1886-01-01 and 2019-12-31.
#' #' @param theme Theme you would like to use to plot the graph.
#' #' bey defalt, "light".
#' #' Available themes are "light", "dark", and "earth".
#' #' @details `plot_map()` creates a plot of the a unimodal geographical network 
#' #' at a single point in time.
#' #' @importFrom dplyr mutate inner_join rename filter
#' #' @return A map of a country level geographical network.
#' #' @examples
#' #' \donttest{
#' #' #memberships <- dplyr::filter(manyenviron::memberships$ECOLEX_MEM,
#' #' #Begin > "2000-01-01" & Begin < "2000-12-12")
#' #' #plot_map(memberships, actor = "stateID") +
#' #' #ggplot2::labs(title = "Bilateral International Environmental Treaties Signed in the year 2000",
#' #' #subtitle = "Ecolex data")
#' #'}
#' #' @export
#' plot_map <- function(dataset, actor = "stateID", treaty_type = NULL,
#'                      date = "2019-12-31", theme = "light") {
#'   # check packages
#'   thisRequires("cshapes")
#'   thisRequires("manydata")
#'   # Checks for correct input
#'   weight <- .data <- NULL
#'   # Step 1: get membership list
#'   dataset <- manydata::call_treaties(dataset = dataset, actor = actor,
#'                                      treaty_type = treaty_type)
#'   # Step 2: set up empty matrix
#'   actor <- unique(unlist(strsplit(dataset$Memberships, ", ")))
#'   out <- matrix(0, nrow = length(actor), ncol = length(actor))
#'   rownames(out) <- actor
#'   colnames(out) <- actor
#'   # Step 3: fill matrix with values
#'   for (k in colnames(out)) {
#'     m <- data.frame(table(unlist(strsplit(grep(k, dataset$Memberships,
#'                                                value = TRUE), ", "))))
#'     m <- m[!(m$Var1 %in% k),]
#'     out[k, ] <- ifelse(names(out[k,]) %in% m$Var1 == TRUE, m$Freq/100, out[k,])
#'   }
#'   out  <- igraph::get.data.frame(igraph::graph.adjacency(out, weighted = TRUE))
#'   # Step 4 = get theme
#'   if (theme == "dark") {
#'     maptheme <- maptheme(palette = c("#FFFAFA", "#596673"))
#'     countrycolor <- "#FFFAFA"
#'   }
#'   if (theme == "earth") {
#'     maptheme <- maptheme(palette = c("#79B52F", "#4259FD"))
#'     countrycolor <- "#79B52F"
#'   }
#'   if (theme == "light") {
#'     maptheme <- maptheme(palette = c("#596673", "#FFFAFA"))
#'     countrycolor <- "#596673"
#'   }
#'   # Step 5: import the historical shapefile data
#'   cshapes <- cshapes::cshp(as.Date(date), useGW = FALSE)
#'   coment <- vapply(countryregex[, 3], # add stateID abbreviations
#'                    function(x) grepl(x, cshapes$country_name,
#'                                      ignore.case = TRUE, perl = TRUE) * 1,
#'                    FUN.VALUE = double(length(cshapes$country_name)))
#'   colnames(coment) <- countryregex[, 1]
#'   rownames(coment) <- cshapes$country_name
#'   ab <- apply(coment, 1, function(x) paste(names(x[x == 1]),
#'                                            collapse = "_"))
#'   ab[ab == ""] <- NA
#'   cshapes <- dplyr::mutate(cshapes, stateID = unname(ab))
#'   # Step 6: create edges with from/to lat/long
#'   edges <- out %>%
#'     dplyr::inner_join(cshapes, by = c("from" = "stateID")) %>%
#'     dplyr::rename(x = .data$caplong, y = .data$caplat) %>%
#'     dplyr::inner_join(cshapes, by = c("to" = "stateID")) %>%
#'     dplyr::rename(xend = .data$caplong, yend = .data$caplat)
#'   # Step 7: Create plotted network from computed edges
#'   g <- as_tidygraph(edges)
#'   # Step 8: Get the country shapes from the edges dataframe
#'   country_shapes <- ggplot2::geom_sf(data = cshapes$geometry,
#'                                      fill = countrycolor)
#'   # Step 9: generate the point coordinates for capitals
#'   cshapes_pos <- cshapes %>%
#'     dplyr::filter(.data$stateID %in% node_names(g)) %>%
#'     dplyr::rename(x = .data$caplong, y = .data$caplat)
#'   # Reorder things according to nodes in plotted network g
#'   cshapes_pos <- cshapes_pos[match(node_names(g),
#'                                    cshapes_pos[["stateID"]]), ]
#'   # Step 10: generate the layout
#'   lay <- ggraph::create_layout(g, layout = cshapes_pos)
#'   edges$circular <- rep(FALSE, nrow(edges))
#'   edges$edge.id <- rep(1, nrow(edges))
#'   # Step 11: plot
#'   ggraph::ggraph(lay) +
#'     country_shapes +
#'     ggraph::geom_edge_arc(data = edges, ggplot2::aes(edge_width = weight),
#'                           strength = 0.33, alpha = 0.25) +
#'     ggraph::scale_edge_width_continuous(range = c(0.5, 2), # scales edge widths
#'                                         guide = "none") +
#'     ggraph::geom_node_point(shape = 21, # draw nodes
#'                             fill = "white", color = "black", stroke = 0.5) +
#'     ggraph::geom_node_text(ggplot2::aes(label = node_names(g)),
#'                            repel = TRUE, size = 3, color = "white",
#'                            fontface = "bold") +
#'     maptheme
#' }
#' 
#' # Helper function providing the network map function with a few map themes.
#' maptheme <- function(palette = c("#FFFAFA", "#596673")) {
#'   oceancolor <- palette[2]
#'   titlecolor <- ifelse(is_dark(palette[2]), "white", "black")
#'   # Create map theme
#'   maptheme <- ggplot2::theme(panel.grid = ggplot2::element_blank()) +
#'     ggplot2::theme(axis.text = ggplot2::element_blank()) +
#'     ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
#'     ggplot2::theme(axis.title = ggplot2::element_blank()) +
#'     ggplot2::theme(legend.position = "bottom") +
#'     ggplot2::theme(panel.grid = ggplot2::element_blank()) +
#'     ggplot2::theme(panel.background = ggplot2::element_blank()) +
#'     ggplot2::theme(plot.background = ggplot2::element_rect(fill = oceancolor)) +
#'     ggplot2::theme(plot.title = ggplot2::element_text(color = titlecolor,
#'                                                       hjust = 0.1, vjust = 0.1),
#'                    plot.subtitle = ggplot2::element_text(color = titlecolor,
#'                                                          hjust = 0.065,
#'                                                          vjust = 0.1),
#'                    plot.caption = ggplot2::element_text(color = titlecolor, hjust = 0.96)) +
#'     ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0.5, 0), "cm"))
#'   # This function returns a map theme for ggplot
#'   maptheme
#' }
#' 

