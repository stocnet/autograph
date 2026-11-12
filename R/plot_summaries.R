# Models ####

#' Plotting diffusion models
#' @name plot.diffusion
#' @param x A "diff_model" of "diffs_model" class of object.
#'   E.g. as a result from `manynet::play_diffusion()`.
#' @param ... Other arguments to be passed.
#' @param all_steps Whether all steps should be plotted or just those where
#'   there is change in the distributions.
#' @details
#'   `plot.diff_model()` draws one line for each value of the attribute
#'   that the diffusion changes,
#'   and bars for the nodes that become infectious at each step.
#'   A diffusion in the values S, E, I, R, and D names them
#'   "Susceptible", "Exposed", "Infected", "Recovered", and "Dead".
#'   A diffusion in values of its own,
#'   as from `manynet::play_diffusion(compartments = )`,
#'   names the lines by these values.
#'   The roles in `attr(x, "compartments")` give the colours:
#'   blue for S, yellow for E, red for I, green for R,
#'   and black for D.
#'   The bars count the new nodes in the first value of I.
#' @importFrom dplyr left_join
#' @importFrom ggplot2 geom_histogram
#' @returns `plot.diff_model()` returns a bar chart of the number of new 
#'   infected nodes at each time point, as well as an overlay line plot of the
#'   total in each value
#' @examples
#' plot(res_manynet_diff)
#' @export
plot.diff_model <- function(x, ..., all_steps = TRUE){
  value <- prop <- new <- NULL # initialize variables to avoid CMD check notes
  attribute <- attr(x, "attribute")
  if(is.null(attribute)) attribute <- "diffusion"
  if(nrow(x) <= 1){
    if(identical(attribute, "diffusion")) manynet::snet_warn(
      "No diffusion was observed, so there is nothing to plot.",
      "The diffusion ran for a single step, which happens when no node was",
      "seeded or when the seeds had no one to infect.",
      "Check the {.arg seeds} given to {.fn manynet::play_diffusion},",
      "and that the network has ties.") else manynet::snet_warn(
        "No change in {.val {attribute}} was observed, so there is nothing to plot.",
        "The diffusion ran for a single step, which happens when no node was",
        "infectious or when the infectious nodes had no one to infect.",
        "Check the start values of {.val {attribute}}",
        "and the {.arg compartments} given to {.fn manynet::play_diffusion},",
        "and that the network has ties.")
    return(invisible(NULL))
  }
  data <- as.data.frame(x)
  tcol <- if("time" %in% names(data)) "time" else "t"
  n <- if("n" %in% names(data)) data$n else
    rowSums(data[, .diffusion_values(data), drop = FALSE])
  values <- .diffusion_values(data)
  comps <- .diffusion_roles(attr(x, "compartments"), values)
  infectious <- if(length(comps$I)) as.character(comps$I[1]) else NULL
  if(!is.null(infectious) && !infectious %in% values) infectious <- NULL
  seir <- all(values %in% c("S", "E", "I", "R", "D")) &&
    all(vapply(names(comps), function(r) identical(as.character(comps[[r]]), r),
               logical(1)))
  if(!all_steps && !is.null(infectious)){
    last <- data[[infectious]][nrow(data)]
    data <- data[!(data[[infectious]] == last *
                     duplicated(data[[infectious]] == last)), , drop = FALSE]
    n <- if("n" %in% names(data)) data$n else n[seq_len(nrow(data))]
  }
  # A record in S, E, I, and R always has the four columns, but draws E and R
  # only where some node is in them, as it always has.
  drawn <- if(seir) values[values %in% c("S", "I") |
                             vapply(values, function(v) any(data[[v]] > 0),
                                    logical(1))] else values
  long <- do.call(rbind, lapply(drawn, function(v)
    data.frame(time = data[[tcol]], value = v, prop = data[[v]]/n)))
  long$value <- factor(long$value, levels = drawn)
  labs <- if(seir) c(S = "Susceptible", E = "Exposed", I = "Infected",
                     R = "Recovered", D = "Dead")[drawn] else drawn
  # The lines of S and I are drawn under the bars, and the other lines over
  # them, as they always have been.
  under <- as.character(long$value) %in% unlist(comps[c("S", "I")])
  p <- ggplot2::ggplot(long) + 
    ggplot2::geom_line(ggplot2::aes(x = time, y = prop, color = value), 
                       data = long[under, , drop = FALSE], linewidth = 1.25)
  if(!is.null(infectious))
    p <- p + ggplot2::geom_col(ggplot2::aes(x = time, y = new), 
                               data = data.frame(time = data[[tcol]], 
                                                 new = data[[paste0(infectious, "_new")]]/n),
                               alpha = 0.4)
  if(any(!under))
    p <- p + ggplot2::geom_line(ggplot2::aes(x = time, y = prop, color = value), 
                                data = long[!under, , drop = FALSE], 
                                linewidth = 1.25)
  p + ag_theme_minimal() + 
    # using coord_cartesian to avoid printing warnings
    ggplot2::coord_cartesian(ylim = c(0,1)) + 
    ggplot2::scale_x_continuous(breaks = function(x) pretty(x, n=6)) +
    ggplot2::ylab("Proportion") + ggplot2::xlab("Steps") +
    ggplot2::scale_color_manual(if(seir) "Legend" else attribute, 
                                labels = unname(labs),
                                values = .diffusion_colors(drawn, comps),
                                breaks = drawn,
                                guide = "legend")
}

# The value columns of a diffusion report: the columns that are not the time,
# the number of nodes, the number exposed, or the new nodes in each value.
.diffusion_values <- function(data){
  nm <- names(data)
  nm[!nm %in% c("time", "t", "n", "s") & !grepl("_new$", nm)]
}

# The roles of the values as a named list of character vectors. A report from
# before the compartments were kept is read as S, E, I, and R where its values
# are these, and as FALSE and TRUE where its values are logical.
.diffusion_roles <- function(comps, values){
  comps <- comps[!vapply(comps, is.null, logical(1))]
  if(is.null(names(comps)) || !length(comps)) comps <- NULL
  if(is.null(comps)){
    if(all(values %in% c("S", "E", "I", "R", "D")))
      comps <- as.list(stats::setNames(values, values)) else
        if(setequal(values, c("FALSE", "TRUE")) || identical(values, "FALSE") ||
           identical(values, "TRUE"))
          comps <- list(S = "FALSE", I = "TRUE") else comps <- list()
  }
  lapply(comps, as.character)
}

# Gives each role its usual colour: S blue, E yellow, I red, R green, and D
# black. The first value of a role takes the colour of the role; the other
# values, and values without a role, take other colours of the palette.
.diffusion_colors <- function(values, comps){
  role_cols <- c(S = "#4575b4", E = "#E6AB02", I = "#d73027", R = "#66A61E")
  base <- stats::setNames(match_color(role_cols), names(role_cols))
  # D is black in every theme, and is not matched to the palette. The darkest
  # colour of a palette can be near the colour of I, as plum is near maroon in
  # "iheid"; black is darker than all the colours of S, E, I, and R.
  base <- c(base, D = "#000000")
  pal <- ag_qualitative()
  rest <- setdiff(pal, base)
  out <- stats::setNames(rep(NA_character_, length(values)), values)
  for(r in names(comps)){
    first <- comps[[r]][1]
    if(first %in% values && r %in% names(base)) out[first] <- base[[r]]
  }
  free <- names(out)[is.na(out)]
  if(length(free)){
    extra <- if(length(free) <= length(rest)) rest[seq_along(free)] else
      ag_qualitative(length(free) + length(base))[-seq_along(base)][seq_along(free)]
    out[free] <- extra
  }
  out
}

#' @noRd
#' @export
plot.mnet <- function(x, ...){
  if(.is_mobility(x)){
    plot(.as_mobility(x), ...)
  } else if(manynet::is_changing(x)){
    plot(manynet::as_diffusion(x, attribute = .diffusion_attribute(x)), ...)
  } else {
    graphr(x)
  }
}

#' @noRd
#' @export
plot.stocnet <- plot.mnet

# The attribute whose changes record a diffusion: "diffusion" where it changes,
# else the changing attribute whose values have roles in `info$compartments`,
# else a logical attribute that changes, else the first attribute that changes.
.diffusion_attribute <- function(x){
  changes <- manynet::as_changelist(x)
  vars <- if(!is.null(changes) && "var" %in% names(changes))
    unique(as.character(changes$var)) else character(0)
  # "active" records when nodes are present, not a diffusion
  vars <- setdiff(vars, "active")
  if(!length(vars) || "diffusion" %in% vars) return("diffusion")
  comps <- .net_compartments(x)
  if(!is.null(comps)){
    roled <- as.character(unlist(comps, use.names = FALSE))
    for(v in vars){
      vals <- as.character(unlist(changes$value[changes$var == v]))
      if(length(vals) && all(vals %in% roled)) return(v)
    }
  }
  # a logical attribute needs no compartments: FALSE is S and TRUE is I
  for(v in vars){
    vals <- unlist(changes$value[changes$var == v])
    if(is.logical(vals)) return(v)
  }
  vars[1]
}

# The compartments that a network keeps: in `info` for a stocnet, and as a
# graph attribute once it is coerced to an igraph or tidygraph.
.net_compartments <- function(x){
  if(inherits(x, "stocnet")) return(x$info$compartments)
  if(inherits(x, "igraph")) return(igraph::graph_attr(x, "compartments"))
  NULL
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
    ag_theme_minimal() + 
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
    ggplot2::geom_line(show.legend = FALSE) + ag_theme_minimal() +
    ggplot2::scale_color_manual(values = ag_qualitative(ncol(x))) +
    ggplot2::ylab("Belief")
}

#' Plotting mobility models
#' @name plot.mobility
#' @param x A 'mobilityModel', as from `manynet::as_mobility()`.
#' @param ... Other arguments to be passed.
#' @details
#'   `plot.mobilityModel()` draws the number of dissatisfied occupants
#'   after each move.
#'   `plot()` of a network of places with a record of moves,
#'   as from `manynet::play_mobility()`, draws the same line,
#'   and adds the number of dissatisfied occupants at the start, step 0.
#'   A 'mobilityModel' alone does not hold the start,
#'   so its line begins at the first move.
#'
#'   To draw the places at the start, the middle, and the last move,
#'   use `graphs()` on the network of places.
#' @returns `plot.mobilityModel()` returns a line plot of the number of
#'   dissatisfied occupants at each step.
#' @examples
#' places <- manynet::add_node_attribute(manynet::create_lattice(25), "group",
#'   rep(c(0, 1, NA, 1, 0), 5))
#' moved <- manynet::play_mobility(places, "group")
#' plot(moved)
#' plot(manynet::as_mobility(moved))
#' graphs(moved)
#' @export
plot.mobilityModel <- function(x, ...){
  time <- dissatisfied <- NULL # initialize variables to avoid CMD check notes
  data <- data.frame(time = as.numeric(x$time),
                     dissatisfied = as.numeric(x$dissatisfied))
  start <- attr(x, "start")
  if(!is.null(start))
    data <- rbind(data.frame(time = 0, dissatisfied = as.numeric(start)), data)
  if(!nrow(data) || all(is.na(data$dissatisfied))){
    if(!nrow(x)) manynet::snet_warn(
      "No occupant moved, so there is nothing to plot.",
      "Check that some places are empty and that some occupants are dissatisfied.") else
        manynet::snet_warn(
          "The moves have no record of {.var satisfied}, so there is nothing to plot.",
          "Use a network from {.fn manynet::play_mobility}.")
    return(invisible(NULL))
  }
  ggplot2::ggplot(data, ggplot2::aes(x = time, y = dissatisfied)) +
    ggplot2::geom_line(linewidth = 1.25, colour = ag_qualitative(1)) +
    ggplot2::geom_point(colour = ag_qualitative(1)) +
    ag_theme_minimal() +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(breaks = function(x) pretty(x, n = 6)) +
    ggplot2::ylab("Dissatisfied occupants") + ggplot2::xlab("Steps")
}

# A network of places with a record of moves, as from manynet::play_mobility():
# it has an `occupant` node attribute, and changes to it.
.is_mobility <- function(x){
  if(!manynet::is_manynet(x) || inherits(x, "mobilityModel") ||
     !"occupant" %in% manynet::net_node_attributes(x)) return(FALSE)
  changes <- manynet::as_changelist(x)
  !is.null(changes) && "var" %in% names(changes) &&
    "occupant" %in% changes$var
}

# The attribute of the occupants: kept in `info$mobility` of a stocnet, else
# the first attribute that moves with the occupants.
.mobility_attribute <- function(x){
  if(inherits(x, "stocnet") && !is.null(x$info$mobility$attribute))
    return(x$info$mobility$attribute)
  vars <- unique(as.character(manynet::as_changelist(x)$var))
  vars <- setdiff(vars, c("occupant", "satisfied", "active"))
  if(length(vars)) vars[1] else "occupant"
}

# The 'mobilityModel' of a network of places, with the number of dissatisfied
# occupants at the start, which the model alone does not hold.
.as_mobility <- function(x){
  out <- manynet::as_mobility(x, .mobility_attribute(x))
  satisfied <- manynet::as_stocnet(x)$nodes$satisfied
  if(!is.null(satisfied))
    attr(out, "start") <- sum(!as.logical(satisfied), na.rm = TRUE)
  out
}

# The network of places at some steps, for `graphs()`. `waves` select the steps
# as they select networks in a list: the first is the start, step 0.
.graphs_mobility <- function(x, waves, based_on, ...){
  changes <- manynet::as_changelist(x)
  last <- max(c(0, changes$time[changes$var == "occupant"]))
  steps <- 0:last
  if(missing(waves)) at <- unique(c(0, round(last/2), last)) else {
    if(!is.numeric(waves) || any(is.na(waves)))
      manynet::snet_abort(
        "{.arg waves} should be the number of steps to plot, or a vector of",
        "which steps to plot, but a value of class {.cls {class(waves)}}",
        "was given.")
    if(any(waves < 1) || any(waves > length(steps)))
      manynet::snet_abort(
        "{.arg waves} should be between 1 (the start) and {length(steps)}",
        "(the last move), but {.val {waves}} was given.")
    at <- if(length(waves) == 1) steps[seq_len(waves)] else steps[waves]
  }
  nets <- lapply(at, function(t) manynet::to_time(x, t))
  names(nets) <- paste("Step", at)
  dots <- list(...)
  if(!any(c("node_color", "node_colour") %in% names(dots)))
    dots$node_color <- .mobility_attribute(x)
  p <- do.call(graphs, c(list(nets, waves = seq_along(nets),
                              based_on = based_on), dots))
  # The empty places are drawn blank
  for(i in seq_along(nets)){
    fill <- p[[i]]$scales$get_scales("fill")
    if(!is.null(fill)) fill$na.value <- "white"
  }
  p
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
#' #' @importFrom dplyr select mutate distinct rename
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
#'     out <- dplyr::select(dataset, manyID) |>
#'       dplyr::rename(key = manyID) |>
#'       dplyr::distinct()
#'   } else if (key == "treatyID") {
#'     out <- dplyr::select(dataset, treatyID) |>
#'       dplyr::rename(key == treatyID) |>
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
#'                 key = gsub("\\:.*", "", key)) |>
#'     as_tidygraph() |>
#'     dplyr::filter(name != "NA") |>
#'     graphr(layout = layout)
#' }
#' 
#' #' @rdname plot_
#' #' @importFrom dplyr select distinct all_of rename
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
#'     out <- dplyr::select(dataset, manyID, dplyr::all_of(actor)) |>
#'       dplyr::rename(key = manyID) |>
#'       dplyr::distinct()
#'   } else if (key == "treatyID") {
#'     out <- dplyr::select(dataset, treatyID, dplyr::all_of(actor)) |>
#'       dplyr::rename(key == treatyID) |>
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
#'   stats::na.omit(out) |>
#'     as_tidygraph() |>
#'     mutate(type = ifelse(grepl("[0-9][0-9][0-9][0-9][A-Za-z]",
#'                                         name), TRUE, FALSE)) |>
#'     graphr(layout = layout)
#' }
#' 
#' #' @rdname plot_
#' #' @importFrom dplyr select mutate distinct filter rename
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
#'     out <- dplyr::select(dataset, manyID) |>
#'       dplyr::rename(key = manyID) |>
#'       dplyr::distinct()
#'   } else if (key == "treatyID") {
#'     out <- dplyr::select(dataset, treatyID) |>
#'       dplyr::rename(key == treatyID) |>
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
#'   out |>
#'     dplyr::filter(grepl(":", key)) |>
#'     dplyr::mutate(key1 = gsub(".*\\:", "", key),
#'                   key = gsub("\\:.*", "", key)) |>
#'     dplyr::distinct() |>
#'     as_tidygraph() |>
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
#'   manynet::snet_requires("cshapes")
#'   manynet::snet_requires("manydata")
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
#'   edges <- out |>
#'     dplyr::inner_join(cshapes, by = c("from" = "stateID")) |>
#'     dplyr::rename(x = .data$caplong, y = .data$caplat) |>
#'     dplyr::inner_join(cshapes, by = c("to" = "stateID")) |>
#'     dplyr::rename(xend = .data$caplong, yend = .data$caplat)
#'   # Step 7: Create plotted network from computed edges
#'   g <- as_tidygraph(edges)
#'   # Step 8: Get the country shapes from the edges dataframe
#'   country_shapes <- ggplot2::geom_sf(data = cshapes$geometry,
#'                                      fill = countrycolor)
#'   # Step 9: generate the point coordinates for capitals
#'   cshapes_pos <- cshapes |>
#'     dplyr::filter(.data$stateID %in% node_names(g)) |>
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

