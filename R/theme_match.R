#' Matching colors across palettes
#' @name theme_match
#' @description
#'   Sometimes a palette or particular colours are chosen to symbolise or 
#'   represent a particular idea,
#'   such as red for "stop" or green for "go", 
#'   or to convey some other interpretation.
#'   Yet institutional palettes do not necessarily include all colours,
#'   which can constrain how interpretable visualisations are under
#'   institutional branding requirements.
#'   `match_color()` helps to find the closest matching colours in a given palette
#'   to one or more input colours.
#'
#'   There is also a helper function, `is_dark()`, to determine whether a color
#'   is dark or light, which can be useful when deciding whether to use white or
#'   black text on top of a colored background.
#' @details
#'   This function uses the Euclidean distance of colours in CIELAB space to
#'   those of a target palette to find the closes corresponding colours.
#'   It also ensures that each input color is matched to a unique color in the
#'   palette.
#'   If there are more input colors than unique colors in the palette,
#'   an error is returned.
#'
#'   By default, the current theme's qualitative palette is used,
#'   but any vector of hexcodes can be passed to the `pal` argument.
#' @param colors One or more hexcodes to match with colors from the palette.
#' @param pal Optionally, a vector of hexcodes representing a palette in which 
#'   to find matches.
#'   By default, the current theme's qualitative palette is used.
#' @returns A vector of hexcodes the length of the first argument.
#' @examples
#' match_color("#4575b4")
#' @export
match_color <- function(colors, pal) {
  if (missing(pal)) pal <- ag_qualitative()
  if (length(colors) > length(pal)) {
    stop("Not enough unique colors in the palette for the input colors.")
  }
  
  # Force colors to be a character vector
  colors <- as.character(colors)
  
  # Convert all to Lab
  labc <- grDevices::convertColor(t(grDevices::col2rgb(colors)), 
                                  from = "sRGB", to = "Lab")
  labp <- grDevices::convertColor(t(grDevices::col2rgb(pal)), 
                                  from = "sRGB", to = "Lab")
  
  if (is.null(dim(labc))) labc <- matrix(labc, nrow = 1)
  
  # Compute the distance matrix (rows: input colors, cols: palette colors)
  dists <- as.matrix(stats::dist(rbind(labc, labp)))
  dists <- dists[seq_len(nrow(labc)), -(1:nrow(labc))]
  
  if(length(colors)==1){
    matched <- pal[which.min(dists)]
  } else {
    matched <- character(nrow(labc))
    used <- rep(FALSE, nrow(labp))
    
    for (i in seq_len(nrow(labc))) {
      # Mask already used palette entries
      dists[i, used] <- Inf
      idx <- which.min(dists[i, ])
      matched[i] <- pal[idx]
      used[idx] <- TRUE
    }
  }
  
  matched
}

#' @rdname theme_match
#' @examples
#' is_dark(c("#000","#FFF"))
#' @export
is_dark <- function(colors) {
  if (length(colors) != 1) {
    return(vapply(colors, is_dark, FUN.VALUE = logical(1)))
  }
  # Google luma formula for details.
  luma <- 0.33 * grDevices::col2rgb(colors)[[1]] +
    0.5 * grDevices::col2rgb(colors)[[2]] +
    0.16 * grDevices::col2rgb(colors)[[3]]
  isdark <- ifelse(luma < 186, TRUE, FALSE)
  isdark
}

