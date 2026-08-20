#' Consistent palette calls
#' @description
#'   These functions assist in calling particular parts of a theme's palette.
#'   For example, `ag_base()` will return the current theme's base or background
#'   color, and `ag_highlight()` will return the color used in that theme to
#'   highlight one or more nodes, lines, or such.
#'   `ag_ink()` returns the darker colour that theme writes with:
#'   axis text, reference lines, and other chrome.
#'   Keeping the two apart lets the base be light enough to stand away from
#'   the highlight while the ink stays dark enough to read.
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
#'   
#'   An institutional palette is not ours to change, but its order is.
#'   Each theme's categorical palette is therefore reordered when the theme is
#'   set, so that the first colours a plot draws on are those that stay
#'   distinct under each type of colour blindness, and `ag_qualitative()`
#'   takes those colours in order rather than interpolating between them.
#'   Divergent palettes pair a warm pole with a cool one for the same reason.
#'   Use [contrast_colors()] to check how your own colours fare,
#'   and [simulate_colorblind()] to see them as a colour-blind viewer would.
#'   
#'   The "rainbow" theme is the exception, and is left in its own order.
#'   Its point is fidelity to the spectrum of an observed rainbow,
#'   which reordering would destroy,
#'   so `ag_qualitative()` samples across its whole length instead.
#'   A spectrum is not a colour-blind safe scheme:
#'   its reds and greens are exactly the pair that red-green colour blindness
#'   cannot separate.
#'   Choose it where the order of the categories is itself meaningful,
#'   and check the result with [contrast_colors()];
#'   for categories with no order, another theme serves more readers.
#' @name ag_call
#' @param number Integer of how many category colours to return.
#' @returns One or more hexcodes as strings.
#' @examples
#' # Single colours from the currently active theme
#' ag_base()
#' ag_ink()
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
ag_ink <- function(){
  getOption("snet_ink", default = "#121212")
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
  # The fallback is the default theme's palette in the order colorblind_sort() gives
  # it, so that a session that has not called stocnet_theme() yet draws the
  # same colours, in the same order, as one that has.
  snet_colors <- getOption("snet_cat", default = c("#1B9E77","#E6AB02","#7570B3",
                                                   "#d73027","#666666","#D95F02",
                                                   "#66A61E","#E7298A","#A6761D",
                                                   "#4575b4"))
  if(missing(number)) number <- length(snet_colors)
  # Take the palette's own colours while they last. Interpolating between them
  # returned mixtures that no longer belonged to the palette, and that sat much
  # closer together than the colours they were mixed from: five categories from
  # the "clay" palette used to come back only 3 apart under simulation, where
  # anything under 10 reads as the same colour. Palettes are ordered so that
  # the first `number` of them are the ones that separate best.
  if(number <= length(snet_colors) &&
     !isTRUE(getOption("snet_cat_spread", default = FALSE)))
    return(snet_colors[seq_len(number)])
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

