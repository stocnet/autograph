#' Consistent palette calls
#' @description
#'   These functions assist in calling particular parts of a theme's palette.
#'   For example, `ag_base()` will return the current theme's base or background
#'   color, and `ag_highlight()` will return the color used in that theme to
#'   highlight one or more nodes, lines, or such.
#'   
#'   Using palettes that are high contrast, aesthetically pleasing, and
#'   institutionally or thematically consistent is not without its challenges.
#' @section Colour blindness: 
#'   The default palettes are designed to be colour-blind friendly.
#'   There are different types of colour-blindness.
#'   The most common type, red-green colour-blindness,
#'   finds it difficult to distinguish between the red and green hues used
#'   in the [rainbow palette](https://colorspace.r-forge.r-project.org/articles/endrainbow.html), 
#'   for instance.
#'   Fortunately there are a range of palettes that function fairly well for
#'   those who are color-blind.
#'   These include the [viridis](https://CRAN.R-project.org/package=viridis) 
#'   palette,
#'   and the ColorBrewer palettes (included in the RColorBrewer package).
#'   The default palettes in `{autograph}` are designed to be colour-blind
#'   friendly, but users should always check that their visualisations serve
#'   their intended audience.
#' @name ag_call
#' @param number Integer of how many category colours to return.
#' @returns One or more hexcodes as strings.
#' @examples
#' # Single colours from the currently active theme
#' ag_base()
#' ag_highlight()
#' ag_positive()
#' ag_negative()
#' # Palettes of a requested length
#' ag_qualitative(3)
#' ag_sequential(5)
#' ag_divergent(5)
#' # The accessors follow whichever theme is set
#' ag_font()
#' @importFrom grDevices colorRampPalette
#' @export
ag_base <- function(){
  utils::head(getOption("snet_highlight", default = "black"), n = 1)
}

#' @rdname ag_call
#' @export
ag_highlight <- function(){
  utils::tail(getOption("snet_highlight", default = "red"), n = 1)
}

#' @rdname ag_call
#' @export
ag_positive <- function(){
  utils::tail(getOption("snet_div", default = "#4575b4"), n = 1)
}

#' @rdname ag_call
#' @export
ag_negative <- function(){
  utils::head(getOption("snet_div", default = "#d73027"), n = 1)
}

#' @rdname ag_call
#' @export
ag_qualitative <- function(number){
  snet_colors <- getOption("snet_cat", default = c("#1B9E77","#4575b4","#d73027",
                                                   "#66A61E","#E6AB02","#D95F02","#7570B3",
                                                   "#A6761D","#E7298A","#666666"))
  if(missing(number)) number <- length(snet_colors)
  colorRampPalette(snet_colors)(number)
}

#' @rdname ag_call
#' @export
ag_sequential <- function(number){
  snet_colors <- getOption("snet_highlight", default = "#d73027")
  if(length(snet_colors)==1) snet_colors <- c(ag_base(), snet_colors[1])
  colorRampPalette(snet_colors)(number)
}

#' @rdname ag_call
#' @export
ag_divergent <- function(number){
  # The default must be a real pair of colours, matching ag_negative()'s and
  # ag_positive()'s defaults (which read the head and tail of this same
  # option). It was the literal string "default", so ag_divergent() errored
  # with "invalid color name 'default'" in any session where stocnet_theme()
  # had not yet been called -- which the test suite never saw, because
  # tests/testthat.R sets the theme before running.
  snet_colors <- getOption("snet_div", default = c("#d73027", "#4575b4"))
  if(length(snet_colors)==2) 
    snet_colors <- c(snet_colors[1], "white", snet_colors[2])
  colorRampPalette(snet_colors)(number)
}

#' @rdname ag_call
#' @export
ag_font <- function(){
  getOption("snet_font", default = "sans")
}

# nocov start
# Interactive helper for displaying palettes; not called by any package code
ggpizza <- function(colors, init.angle = 105, cex = 4, labcol = NULL) {
  n <- length(colors)
  angles <- seq(0, 2*pi, length.out = n + 1) + init.angle * pi/180
  
  # Data for slices
  slices <- lapply(seq_len(n), function(i) {
    theta <- seq(angles[i], angles[i+1], length.out = 100)
    data.frame(
      x = c(0, cos(theta)),
      y = c(0, sin(theta)),
      color = colors[i],
      group = i
    )
  }) |> dplyr::bind_rows()
  
  # Label positions
  mids <- (angles[-1] + angles[-(n+1)]) / 2
  labels <- data.frame(
    x = 1.1 * cos(mids),
    y = 1.1 * sin(mids),
    label = colors
  )
  
  # Label color choice
  labels$labcol <- ag_base()
  
  ggplot2::ggplot() +
    ggplot2::geom_polygon(data = slices, aes(x, y, group = group, fill = color), 
                          color = "white") +
    ggplot2::geom_text(data = labels, aes(x, y, label = label, color = labcol), 
                       size = cex) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::coord_equal() +
    ggplot2::theme_void()
}
# nocov end

