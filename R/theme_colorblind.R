#' Simulating how colours look to a colour-blind viewer
#' @description
#'   `simulate_colorblind()` returns what a set of colours looks like to a
#'   viewer with a given type of colour vision deficiency (CVD),
#'   which affects about 8% of men and 0.5% of women,
#'   or in greyscale, as a reader sees the plot in print.
#' @details
#'   Simulation uses the matrices of Machado, Oliveira and Fernandes (2009),
#'   applied in linear RGB.
#'   Those matrices are published for each severity of colour blindness;
#'   `severity` interpolates between the identity and the full-severity matrix,
#'   which approximates the published steps closely enough for a check.
#'   Full severity is dichromacy (deuteranopia, protanopia, tritanopia);
#'   a lower severity is anomalous trichromacy (deuteranomaly, protanomaly),
#'   which is the more common condition.
#'   Greyscale conversion takes the relative luminance of the colour,
#'   the same quantity [check_contrast()] scores with.
#' @name theme_colorblind
#' @family themes
#' @seealso [check_colors()], which scores a palette rather than simulating it.
#' @param colors One or more colours, given as hexcodes or as names R knows.
#' @param type The type of colour blindness to simulate:
#'   "deutan" (green-blind, the most common), "protan" (red-blind),
#'   "tritan" (blue-blind), "grey" for greyscale, as a photocopier renders it,
#'   or "normal" for unaffected vision.
#' @param severity How severe the colour blindness is, between 0 and 1.
#'   By default 1, which is dichromacy.
#'   A value between 0 and 1 is anomalous trichromacy.
#'   Ignored for the "grey" and "normal" types.
#' @references
#'   Machado, Gustavo M., Manuel M. Oliveira, and Leandro A. F. Fernandes. 2009.
#'   "A Physiologically-Based Model for Simulation of Color Vision Deficiency".
#'   _IEEE Transactions on Visualization and Computer Graphics_ 15(6): 1291-98.
#'   \doi{10.1109/TVCG.2009.113}
#' @returns
#'   `simulate_colorblind()` returns a vector of hexcodes as long as `colors`.
#' @examples
#' simulate_colorblind(c("#d73027", "#4575b4"), "deutan")
#' # A milder deuteranomaly, and the same colours in greyscale
#' simulate_colorblind(c("#d73027", "#4575b4"), "deutan", severity = 0.5)
#' simulate_colorblind(c("#d73027", "#4575b4"), "grey")
#' @export
simulate_colorblind <- function(colors,
                        type = c("deutan", "protan", "tritan", "grey", "normal"),
                        severity = 1){
  type <- match.arg(type)
  if(!is.numeric(severity) || length(severity) != 1L ||
     is.na(severity) || severity < 0 || severity > 1)
    manynet::snet_abort(
      "{.arg severity} should be a single number between 0 and 1,",
      "but {.val {severity}} was given.")
  rgb <- t(grDevices::col2rgb(colors))/255
  rgb[] <- srgb_to_linear(rgb)
  if(type == "grey"){
    # A greyscale device keeps the luminance of a colour and discards the rest,
    # which is why two colours of the same lightness merge in print however
    # different their hues.
    lum <- as.vector(rgb %*% luminance_weights)
    sim <- cbind(lum, lum, lum)
  } else {
    sim <- rgb %*% t(colorblind_matrix(type, severity))
  }
  sim[sim < 0] <- 0
  sim[sim > 1] <- 1
  sim[] <- linear_to_srgb(sim)
  grDevices::rgb(sim[,1], sim[,2], sim[,3])
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

# The published matrices run from the identity at severity 0 to those above at
# severity 1, so a partial severity is read off the line between the two.
colorblind_matrix <- function(type, severity = 1){
  full <- colorblind_matrices[[type]]
  if(severity == 1) return(full)
  (1 - severity) * diag(3) + severity * full
}

# Rec. 709 luminance weights, which both WCAG and greyscale conversion use.
luminance_weights <- c(0.2126, 0.7152, 0.0722)

relative_luminance <- function(colors){
  rgb <- t(grDevices::col2rgb(colors))/255
  rgb[] <- srgb_to_linear(rgb)
  as.vector(rgb %*% luminance_weights)
}

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
  dists <- check_separation(colors, background = background)
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
