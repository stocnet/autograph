# Helper function to check whether a color is light or dark:
is_dark <- function(hex) {
  # Google luma formula for details.
  luma <- 0.33 * grDevices::col2rgb(hex)[[1]] +
    0.5 * grDevices::col2rgb(hex)[[2]] +
    0.16 * grDevices::col2rgb(hex)[[3]]
  isdark <- ifelse(luma < 186, TRUE, FALSE)
  isdark
}


#' Matching colors across palettes
#' @description
#'   Sometimes particular colours are coded in certain ways to facilitate
#'   interpretation.
#'   For example, perhaps primary colours or traffic light colours are used
#'   to represent some discrete options.
#'   Yet institutional palettes vary in terms of which colours they have 
#'   available.
#'   This function uses the Euclidean distance of colours in CIELAB space to
#'   those of a target palette to find the closes corresponding colours.
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

