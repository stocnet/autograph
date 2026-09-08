#' Checking colours for colour blindness and legibility
#' @description
#'   These functions score a set of colours rather than the plot that uses
#'   them, so that one palette can be compared with another.
#'
#'   `check_separation()` reports how far apart the colours are,
#'   taking the worst case over normal vision and each type of colour
#'   blindness, so that a palette is only credited for a difference that
#'   every viewer can see.
#'
#'   `check_contrast()` reports whether text can be read on a ground.
#' @details
#'   The two functions answer different questions,
#'   and a palette needs both answered.
#'   `check_separation()` asks whether two marks can be told apart,
#'   and `check_contrast()` asks whether text can be read on what it sits on.
#'   [simulate_colorblind()] asks a third question,
#'   which is whether either survives a photocopier.
#'
#'   Distances are Euclidean distances in CIELAB space, the same measure
#'   [match_color()] uses.
#'   As a rule of thumb, a distance below 10 means two colours are easily
#'   confused, 10 to 25 means they are separable but close,
#'   and above 25 means they are comfortably distinct.
#'   Ratios are those of WCAG 2.1, which asks for at least 4.5 for body text
#'   and at least 3 for large text and for graphical objects.
#'
#'   Colour blindness affects about 8% of men and 0.5% of women,
#'   and the worst case is taken over deuteranopia, protanopia and
#'   tritanopia as well as normal vision.
#' @name check_colors
#' @family themes
#' @seealso [check_layout()],
#'   which scores a drawing rather than a palette.
#' @param colors One or more colours, given as hexcodes or as names R knows.
#' @references
#'   World Wide Web Consortium. 2018.
#'   _Web Content Accessibility Guidelines (WCAG) 2.1_.
#'   \url{https://www.w3.org/TR/WCAG21/}
#' @returns
#'   `check_separation()` returns a square matrix of worst-case distances,
#'   with the colours as its dimnames and a missing diagonal,
#'   so that `min(x, na.rm = TRUE)` gives the closest pair.
#'   A "grey" attribute holds the same matrix as seen in greyscale.
#'
#'   `check_contrast()` returns a square matrix of WCAG contrast ratios,
#'   shaped the same way.
#' @examples
#' # How well does the current theme's palette separate five categories?
#' check_separation(ag_qualitative(5))
#' # The closest pair in it
#' min(check_separation(ag_qualitative(5)), na.rm = TRUE)
#' # And the closest pair once it is printed in greyscale
#' min(attr(check_separation(ag_qualitative(5)), "grey"), na.rm = TRUE)
#' # A red and a green that only look different to some viewers
#' check_separation(c("#B7352D", "#627313"))[1, 2]
#' # Can the current theme's ink be read on its ground?
#' check_contrast(ag_ink())[1, 2]
NULL

#' @rdname check_colors
#' @param background Optionally, a colour to include in the comparison,
#'   so that a colour too pale or too dark to be seen against it is not
#'   counted as distinct.
#'   By default the current theme's background is used.
#' @export
check_separation <- function(colors, background = NULL){
  if(!is.null(background)) colors <- c(background, colors)
  types <- names(colorblind_matrices)
  dists <- lapply(types,
                  function(ty) as.matrix(stats::dist(colorblind_lab(colors, ty))))
  # A pair is only as distinguishable as its worst view of it.
  out <- Reduce(pmin, dists)
  # The diagonal is left missing rather than zero, so that the obvious way to
  # ask how well a palette separates -- min() over the matrix -- reports the
  # closest pair of different colours, and not the zero distance from each
  # colour to itself.
  diag(out) <- NA_real_
  dimnames(out) <- list(colors, colors)
  # Greyscale is reported beside the score rather than folded into it. Two
  # colours that differ only in hue collapse in greyscale however well they
  # serve a colour-blind reader, so a worst case that included it would
  # condemn nearly every institutional palette and leave only lightness to
  # design with. Whether a figure has to survive a photocopier is the user's
  # question to answer, so the number is offered, not imposed.
  grey <- as.matrix(stats::dist(colorblind_lab(colors, "grey")))
  diag(grey) <- NA_real_
  dimnames(grey) <- dimnames(out)
  attr(out, "grey") <- grey
  class(out) <- c("check_separation", class(out))
  out
}

#' @export
print.check_separation <- function(x, ...){
  grey <- attr(x, "grey")
  out <- unclass(x)
  attr(out, "grey") <- NULL
  print(out, ...)
  # The greyscale matrix is summarised rather than printed. Its interest is
  # almost always the one number -- whether anything collapses in print --
  # and a second matrix of the same size would bury the first.
  if(!is.null(grey) && any(!is.na(grey)))
    cat("\nClosest pair in greyscale: ",
        round(min(grey, na.rm = TRUE), 1), "\n", sep = "")
  invisible(x)
}

#' @rdname check_colors
#' @export
check_contrast <- function(colors, background = NULL){
  # Unlike check_separation(), where a background is one more colour to keep
  # away from, here it is what the others are read *on*, so it belongs in the
  # comparison whether or not the user names one.
  if(is.null(background)) background <- ag_ground_fill()
  colors <- c(background, colors)
  lum <- relative_luminance(colors)
  lighter <- outer(lum, lum, pmax)
  darker <- outer(lum, lum, pmin)
  out <- (lighter + 0.05)/(darker + 0.05)
  diag(out) <- NA_real_
  dimnames(out) <- list(colors, colors)
  out
}
