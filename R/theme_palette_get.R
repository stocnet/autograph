#' Consistent palette calls
#' @description
#'   These functions assist in calling particular parts of a theme's palette.
#'   For example, `ag_base()` will return the current theme's base or background
#'   color, and `ag_highlight()` will return the color used in that theme to
#'   highlight one or more nodes, lines, or such.
#'   `ag_ink()` returns the darker colour that theme writes with:
#'   axis text, reference lines, and other chrome.
#'   `ag_missing()` returns the neutral that theme sets aside for data that
#'   should recede: missing values, isolates counted out of a drawing,
#'   and any "other" remainder left when small categories are grouped down.
#'   Keeping one colour for all three means a reader learns it once.
#'   Keeping the two apart lets the base be light enough to stand away from
#'   the highlight while the ink stays dark enough to read.
#'   Where the ground changes under a theme -- the "print" medium forces
#'   white, whatever the theme prefers -- `ag_ink()` falls back to black or
#'   white rather than return an ink that cannot be read on it.
#'   See [contrast_ratio()] and [stocnet_medium()].
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
#'   Two further questions are worth asking of a palette.
#'   Whether its text can be read on what it sits on is a matter of contrast
#'   rather than of hue, and [contrast_ratio()] scores it against the
#'   thresholds of WCAG 2.1.
#'   Whether it survives print is a matter of lightness alone, since a
#'   greyscale device keeps the luminance of a colour and discards the rest;
#'   `simulate_colorblind(type = "grey")` shows that view, and
#'   [contrast_colors()] reports the greyscale distances beside its own score.
#'   Most institutional palettes separate by hue and so collapse in greyscale.
#'   Where a figure has to print in black and white, use the "bw" theme, or
#'   add a second channel such as `node_shape`.
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
#' ag_missing()
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
  ink <- getOption("snet_ink", default = "#121212")
  ground <- ag_ground_fill()
  # A theme's ink is chosen for that theme's own ground, but the ground can
  # change under it: the "print" medium forces white, and a session that
  # restores a persisted theme may not have applied the ink yet. Rather than
  # write text that cannot be read, fall back to whichever of black and white
  # reads better on whatever ground is actually there. WCAG asks 4.5 of body
  # text; see contrast_ratio().
  if(contrast_ratio(ink, ground)[1, 2] >= 4.5) return(ink)
  alts <- c("#121212", "#FFFFFF")
  ratios <- vapply(alts, function(a) contrast_ratio(a, ground)[1, 2],
                   numeric(1))
  unname(alts[which.max(ratios)])
}

#' @rdname ag_call
#' @export
ag_missing <- function(){
  getOption("snet_missing", default = "#8C8C8C")
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
  # Past the end of the palette there are only mixtures left, and they sit
  # closer together than the colours they were mixed from. Say so rather than
  # returning colours that quietly fail a check the palette itself would pass.
  # No alternative theme is suggested: an institutional palette is chosen
  # because it is that institution's, so swapping it is not an answer.
  if(number > length(snet_colors))
    snet_info("This palette holds {length(snet_colors)} colours,",
              "so the {number} asked for include mixtures of them,",
              "which sit closer together than the palette's own colours.",
              "Consider fewer categories,",
              "or choose the colours yourself with",
              "{.fn ggplot2::scale_fill_manual}.")
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
  
  # The labels sit outside the wheel, on the plot's ground, so they take the
  # colour the theme writes with rather than the colour of an unhighlighted
  # mark: ag_base() is light in several themes and vanished against white.
  labels$labcol <- ag_ink()
  
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

