# The font lists that grDevices reports name only a handful of device aliases
# ("sans", "Helvetica", "Arial", and a few more), so a font that a user
# installs for a theme stays invisible to them. Ask the system font registry
# first, where {systemfonts} is installed, and fall back to the device aliases
# otherwise. Fonts registered by extrafont::loadfonts() arrive through those
# same device lists, so no second package is needed to see them.
available_fonts <- function(){
  fonts <- character(0)
  if(requireNamespace("systemfonts", quietly = TRUE))
    fonts <- c(fonts, systemfonts::system_fonts()$family)
  if(.Platform$OS.type == "windows"){
    fonts <- c(fonts, names(grDevices::windowsFonts()))
  } else {
    fonts <- c(fonts, names(grDevices::X11Fonts()))
  }
  fonts <- c(fonts, names(grDevices::postscriptFonts()))
  sort(unique(fonts))
}

set_font_theme <- function(theme){
  
  candidates <- theme_fonts(theme)
  if(is.null(candidates)){
    options(snet_font = "sans")
    return(invisible(NULL))
  }
  
  installed <- available_fonts()
  
  # Find first match
  if(any(candidates %in% installed)){
    font_match <- candidates[candidates %in% installed]
    snet_info("Setting font to {font_match[1]}.")
  } else {
    snet_info("None of the preferred fonts for theme {.emph {theme}},",
              "{candidates}, are available.",
              "See {.fn autograph::list_fonts} for the fonts R can see,",
              "and {.help autograph::theme_set} for how to install more.",
              "Using default sans-serif font instead.")
    font_match <- "sans"
  }
  options(snet_font = font_match[1])
}

theme_fonts <- function(theme){
  switch(theme,
         "iheid" = c("Helvetica", "Arial", "Verdana"),
         "ethz" = c("DIN Next","Arial"),
         "uzh" = c("Source Sans", "TheSans", "Palatino"),
         "rug" = c("Arial","Parry","Georgia","Open Sans"),
         "oxf" = c("Roboto","Noto Serif","Aktiv Grotesk"),
         "cmu" = c("Open Sans","Source Serif Pro","Helvetica","Times"),
         "iast" = c("Gogh","Monserrat","Playfair","Roboto","tse"),
         "hwu" = c("Univers LT Pro","Baskerville BT","Arial"),
         "neon" = "Comic Sans MS",
         "clay" = c("Styrene B", "Styrene A", "Tiempos Text",
                    "Copernicus", "Helvetica Neue", "Arial")
  )
}

#' Listing the fonts available to R
#' @description
#'   `list_fonts()` reports the font families that R can currently see,
#'   which is what a theme's preferred fonts are matched against.
#'   A font that is installed on the system but missing from this list is not
#'   available to R yet;
#'   see the Fonts section of [theme_set] for how to make it so.
#' @name list_fonts
#' @family themes
#' @param pattern Optionally, a string with which to filter the font families
#'   returned, matched without regard to case.
#'   For example, `list_fonts("sans")` returns every family whose name includes
#'   "sans".
#' @returns A vector of font family names.
#' @examples
#' head(list_fonts())
#' @export
list_fonts <- function(pattern = NULL){
  fonts <- available_fonts()
  if(!is.null(pattern)) fonts <- grep(pattern, fonts, ignore.case = TRUE,
                                      value = TRUE)
  fonts
}
