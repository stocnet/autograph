#' Checking colours for colour blindness
#' @description
#'   These functions report how a set of colours holds up for viewers with
#'   colour vision deficiency (CVD), which affects about 8% of men and 0.5%
#'   of women.
#'   
#'   `simulate_colorblind()` returns what a set of colours looks like to a viewer with
#'   a given type of colour blindness.
#'   `contrast_colors()` scores how far apart colours are, taking the worst case
#'   over normal vision and each type of colour blindness,
#'   so that a palette is only credited for a difference that every viewer
#'   can see.
#' @details
#'   Simulation uses the matrices of Machado, Oliveira and Fernandes (2009)
#'   at full severity, applied in linear RGB.
#'   Distances are Euclidean distances in CIELAB space, the same measure
#'   [match_color()] uses.
#'   As a rule of thumb, a distance below 10 means two colours are easily
#'   confused, 10 to 25 means they are separable but close, 
#'   and above 25 means they are comfortably distinct.
#' @name theme_colorblind
#' @family themes
#' @param colors One or more colours, given as hexcodes or as names R knows.
#' @param type The type of colour blindness to simulate:
#'   "deutan" (green-blind, the most common), "protan" (red-blind),
#'   "tritan" (blue-blind), or "normal" for unaffected vision.
#' @references
#'   Machado, Gustavo M., Manuel M. Oliveira, and Leandro A. F. Fernandes. 2009.
#'   "A Physiologically-Based Model for Simulation of Color Vision Deficiency".
#'   _IEEE Transactions on Visualization and Computer Graphics_ 15(6): 1291-98.
#'   \doi{10.1109/TVCG.2009.113}
#' @returns 
#'   `simulate_colorblind()` returns a vector of hexcodes as long as `colors`.
#'   `contrast_colors()` returns a square matrix of worst-case distances,
#'   with the colours as its dimnames and a missing diagonal,
#'   so that `min(x, na.rm = TRUE)` gives the closest pair.
#' @examples
#' simulate_colorblind(c("#d73027", "#4575b4"), "deutan")
#' # How well does the current theme's palette separate five categories?
#' contrast_colors(ag_qualitative(5))
#' # The closest pair in it
#' min(contrast_colors(ag_qualitative(5)), na.rm = TRUE)
#' # A red and a green that only look different to some viewers
#' contrast_colors(c("#B7352D", "#627313"))[1, 2]
#' @export
simulate_colorblind <- function(colors,
                        type = c("deutan", "protan", "tritan", "normal")){
  type <- match.arg(type)
  rgb <- t(grDevices::col2rgb(colors))/255
  rgb[] <- srgb_to_linear(rgb)
  sim <- rgb %*% t(colorblind_matrices[[type]])
  sim[sim < 0] <- 0
  sim[sim > 1] <- 1
  sim[] <- linear_to_srgb(sim)
  grDevices::rgb(sim[,1], sim[,2], sim[,3])
}

#' @rdname theme_colorblind
#' @param background Optionally, a colour to include in the comparison,
#'   so that a colour too pale or too dark to be seen against it is not
#'   counted as distinct.
#'   By default the current theme's background is used.
#' @export
contrast_colors <- function(colors, background = NULL){
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
  out
}

# Machado, Oliveira and Fernandes (2009), severity 1.0, for linear RGB.
colorblind_matrices <- list(
  normal = diag(3),
  protan = matrix(c( 0.152286,  1.052583, -0.204868,
                     0.114503,  0.786281,  0.099216,
                    -0.003882, -0.048116,  1.051998), 3, 3, byrow = TRUE),
  deutan = matrix(c( 0.367322,  0.860646, -0.227968,
                     0.280085,  0.672501,  0.047413,
                    -0.011820,  0.042940,  0.968881), 3, 3, byrow = TRUE),
  tritan = matrix(c( 1.255528, -0.076749, -0.178779,
                    -0.078411,  0.930809,  0.147602,
                     0.004733,  0.691367,  0.303900), 3, 3, byrow = TRUE))

srgb_to_linear <- function(u){
  ifelse(u <= 0.04045, u/12.92, ((u + 0.055)/1.055)^2.4)
}

linear_to_srgb <- function(u){
  ifelse(u <= 0.0031308, u*12.92, 1.055*u^(1/2.4) - 0.055)
}

colorblind_lab <- function(colors, type){
  sim <- if(type == "normal") colors else simulate_colorblind(colors, type)
  lab <- grDevices::convertColor(t(grDevices::col2rgb(sim))/255,
                                 from = "sRGB", to = "Lab")
  if(is.null(dim(lab))) lab <- matrix(lab, nrow = 1)
  lab
}

# Reorders a palette so that, for every number of categories a user might ask
# for, the colours they get are as distinguishable as a greedy pass can make
# them. The colours themselves are left alone, since an institutional palette
# is not ours to change; only their order is chosen. The background counts as
# an already-taken colour, so a colour too faint to see against it is not
# mistaken for a distant one. The first colour kept is the first in the given
# palette that stands out from the background, which keeps a brand's primary
# colour primary.
colorblind_sort <- function(colors, background = "#FFFFFF", floor = 30){
  n <- length(colors)
  if(n < 3) return(colors)
  dists <- contrast_colors(colors, background = background)
  from_bg <- dists[1, -1]
  dists <- dists[-1, -1, drop = FALSE]
  ord <- which(from_bg >= floor)[1]
  if(is.na(ord)) ord <- which.max(from_bg)
  while(length(ord) < n){
    rest <- setdiff(seq_len(n), ord)
    gaps <- vapply(rest, function(i) min(c(dists[i, ord], from_bg[i])),
                   numeric(1))
    ord <- c(ord, rest[which.max(gaps)])
  }
  unname(colors[ord])
}
