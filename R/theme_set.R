#' Setting a consistent theme for all plots
#' @description
#'   This function enables plots to be quickly, easily and consistently themed.
#'   This is achieved by setting a theme option, usually at the start of an R
#'   session, that enables the palette to be used for 
#'   all autograph-consistent plotting methods.
#'   This includes thematic colours for backgrounds, highlights, 
#'   sequential, divergent and categorical colour schemes.
#'   The function sets these palettes to options that are then
#'   used by the various plotting functions.
#'   
#'   If no theme is specified (i.e. the function is called without argument), 
#'   the current theme is reported.
#'   The default theme is "default".
#'   This theme uses a white background, blue and red for
#'   highlighting, and a blue-white-red divergent palette.
#'   The themes can be changed at any time by calling `stocnet_theme()`
#'   or its alias `set_stocnet_theme()` with a different theme name.
#'   
#'   Other themes include those based on the colour schemes of various 
#'   universities, including ETH Zurich, UZH, UNIBE, RUG, and Oxford.
#'   Other themes include "bw" for black and white, "crisp" for a
#'   high-contrast black and white theme, "neon" for a dark theme
#'   with neon highlights, and "rainbow" for a colourful theme.
#'   Most themes are designed to be colour-blind safe.
#'   
#' @name theme_set
#' @family themes
#' @section Fonts: 
#'   Some themes also set a preferred font for use in plots, 
#'   if available on the system (a check is performed).
#'   In some cases, this includes a vector of options to try in sequence.
#'   If none of the preferred fonts are available, a sans-serif font is used.
#'   If you receive a warning about a missing font when setting a theme, 
#'   try installing one of the preferred fonts or make sure that the font is
#'   available to R using `extrafont::font_import()` and `extrafont::loadfont()`
#' @returns This function sets the theme and palette(s) to be used across all
#'   stocnet packages. The palettes are written to options and held there.
#' @examples
#' stocnet_theme("default")
#' plot(manynet::node_degree(ison_karateka))
#' stocnet_theme("rug")
#' plot(manynet::node_degree(ison_karateka))
NULL

theme_opts <- c("default", "bw", "crisp", "neon", 
                "iheid", "ethz", "uzh", "rug", "unibe", 
                "oxf", "unige", "cmu", "iast",
                "rainbow")

#' @rdname theme_set
#' @param theme String naming a theme.
#'   By default "default".
#'   The following themes are currently available:
#'   `r autograph:::theme_opts`.
#'   This string can be capitalised or not.
#' @importFrom manynet snet_info snet_success snet_warn
#' @export
stocnet_theme <- function(theme = NULL){
  if(is.null(theme)){
    theme <- getOption("stocnet_theme", default = "default")
    snet_info("Theme is currently set to {.emph {theme}}.",
              "The following themes are available: {.emph {theme_opts}}.")
  } else {
    theme <- tolower(theme)
    if(theme %in% theme_opts){
      options(stocnet_theme = theme)
      set_highlight_theme(theme)
      set_divergent_theme(theme)
      set_background_theme(theme)
      set_categorical_theme(theme)
      set_font_theme(theme)
      snet_success("Theme set to {.emph {theme}}.")
    } else {
      snet_warn("Please choose one of the available themes: {.emph {theme_opts}}.")
    }
  }
}

#' @rdname theme_set
#' @export
set_stocnet_theme <- stocnet_theme

set_background_theme <- function(theme){
  if(theme == "neon"){
    options(snet_background = "#070f23")
  } else if(theme == "cmu"){
    options(snet_background = "#E4DAC4")
  } else {
    options(snet_background = "#FFFFFF")
  }
}

set_highlight_theme <- function(theme){
  if(theme == "iheid"){
    options(snet_highlight = c("#000010","#E20020"))
  } else if(theme == "unige"){
    options(snet_highlight = c("#A3A3A3","#CF0063"))
  } else if(theme == "rug"){
    options(snet_highlight = c("#000000", "#dc002d"))
  } else if(theme == "uzh"){
    options(snet_highlight = c("#a3adb7", "#dc6027"))
  } else if(theme == "unibe"){
    options(snet_highlight = c("#121212", "#e4003c"))
  } else if(theme == "oxf"){
    options(snet_highlight = c("#002147", "#c09725"))
  } else if(theme == "ethz"){
    options(snet_highlight = c("#6F6F6F", "#0028a5"))
  } else if(theme == "cmu"){
    options(snet_highlight = c("#6D6E71", "#C41230"))
  } else if(theme == "iast"){
    options(snet_highlight = c("#555", "#e54a37"))
  } else if(theme == "crisp"){
    options(snet_highlight = c("#FFFFFA", "#101314"))
  } else if(theme == "bw"){
    options(snet_highlight = c("#CCCCCC", "#000000"))
  } else if(theme == "neon"){
    options(snet_highlight = c("#5aeafd", "#54fe4b"))
  } else if(theme == "rainbow"){
    options(snet_highlight = c('#1965B0', '#DC050C'))
  } else {
    options(snet_highlight = c("#4576B5", "#D83127"))
  }
}

# "#E20020" - IHEID red
# "#215CAF" - ETH blue
# "#EDEDF4" - ghost white
# "#071013" - rich black
# "#EDAE49" - hunyadi yellow
# "#3C493F" - field green
# "#679289" - viridian

set_divergent_theme <- function(theme){
  if(theme == "bw"){
    options(snet_div = c("black","grey","white"))
  } else if(theme == "iheid"){
    options(snet_div = c("#820C2B","#006EAA","#006564"))
  } else if(theme == "ethz"){
    options(snet_div = c("#B7352D","#007894","#627313"))
  } else if(theme == "uzh"){
    options(snet_div = c("#FC4C02","#4AC9E3","#A4D233"))
  } else if(theme == "unibe"){
    options(snet_div = c("#8a1e22","#007ea2","#466553"))
  } else if(theme == "cmu"){
    options(snet_div = c("#941120","#BCB49E","#182C4B"))
  } else if(theme == "iast"){
    options(snet_div = c("#e62117","#999","#3b5998"))
  } else if(theme == "rainbow"){
    options(snet_div = c('#DC050C','#CAE0AB','#882E72'))
  } else {
    options(snet_div = c("#d73027","white","#4575b4"))
  }
}

set_categorical_theme <- function(theme){
  if(theme == "bw"){
    options(snet_cat = c("#CCCCCC", "#000000"))
  } else if(theme == "iheid"){
    options(snet_cat = c("#006564","#0094D8","#622550",
                         "#268D2B","#3E2682","#820C2B",
                         "#008F92","#006EAA","#A8086E"))
  } else if(theme == "unige"){
    options(snet_cat = c("#F42941","#0067C5","#96004B",
                         "#007E64","#465F7F","#F1AB00",
                         "#00B1AE","#4B0B71","#FF5C00"))
  } else if(theme == "ethz"){
    options(snet_cat = c("#215CAF","#007894","#627313",
                         "#8E6713","#B7352D","#A7117A","#6F6F6F"))
  } else if(theme == "cmu"){
    options(snet_cat = c("#EF3A47","#FDB515","#009647",
                         "#008F91","#043673","#007BC0",
                         "#1F4C4C","#719F94"))
  } else if(theme == "iast"){
    options(snet_cat = c("#fbda26","#0a0","#9c1a1a",
                         "#1b870b","#3d86d8","#50e3c2",
                         "#7ad03d","#fe0087","#e62117",
                         "#1db6d6","#3b5998","#f58b4c",
                         "#e9711c","#ff2b46","#d9372f",
                         "#2fa7d5","#f0c020","#47c965"))
  } else if(theme == "uzh"){
    options(snet_cat = c("#0028A5","#4AC9E3","#A4D233",
                         "#FFC845","#FC4C02","#BF0D3E",
                         "#BDC9E8","#DBF4F9","#ECF6D6",
                         "#FFF4DA","#FFDBCC","#FBC6D4",
                         "#7596FF","#B7E9F4","#DBEDAD",
                         "#FFE9B5","#FEB799","#F78CAA",
                         "#3062FF","#92DFEE","#C8E485",
                         "#FFDE8F","#FE9367","#F3537F",
                         "#001E7C","#1EA7C4","#7CA023",
                         "#F3AB00","#BD3902","#8F0A2E",
                         "#001452","#147082","#536B18",
                         "#A27200","#7E2601","#60061F"))
    
  } else if(theme == "unibe"){
    options(snet_cat = c("#466553","#668271","#8aa092","#afbfb5","#d6ded9",
                         "#007ea2","#5294b4","#85adc6","#b0c7d9","#d8e2ec",
                         "#203a5d","#4a5575","#757792","#a1a0b4","#d0ced9",
                         "#8a1e22","#a14540","#b86f65","#d19d93","#e8cdc6",
                         "#5a3217","#754e31","#927157","#b49b87","#d7cac0",
                         "#36b5b6","#75c4c5","#a0d3d4","#c4e3e3","#e2f1f2",
                         "#ec627d","#f08797","#f4a9b1","#f8c8cc","#fce4e7",
                         "#4767af","#6e82c0","#949fd1","#b9bee1","#dcdef1",
                         "#c2b600","#cfc43c","#dcd274","#e8e1a4","#f4f0d3",
                         "#ee7402","#f3923e","#f7af70","#fbcba1","#fde6d1"))
    
  } else if(theme == "rainbow"){
    options(snet_cat = c('#E8ECFB', '#D9CCE3', '#D1BBD7', 
                         '#CAACCB', '#BA8DB4', '#AE76A3', 
                         '#AA6F9E', '#994F88', '#882E72', 
                         '#1965B0', '#437DBF', '#5289C7', 
                         '#6195CF', '#7BAFDE', 
                         '#4EB265', '#90C987', '#CAE0AB', 
                         '#F7F056', '#F7CB45', '#F6C141', 
                         '#F4A736', '#F1932D', '#EE8026', 
                         '#E8601C', '#E65518', '#DC050C', 
                         '#A5170E', '#72190E', '#42150A'))
  } else if(theme == "oxf"){
    options(snet_cat = c("#776885", '#E08D79', '#ED9390', 
                         '#C4A29E', '#D1BDD5', '#994636', 
                         '#AA1A2D', '#7F055F', '#FE615A', 
                         '#D4CDF4', '#FB5607', '#E6007E', 
                         '#426A5A', '#789E9E', 
                         '#E2C044', '#E4F0EF', '#B9D6F2', 
                         '#A0AF84', '#15616D', '#1D42A6', 
                         '#00AAB4', '#65E5AE', '#95C11F', 
                         '#49B6FF', '#F7EF66'))
  } else {
    options(snet_cat = c("#1B9E77","#4575b4","#d73027",
                         "#66A61E","#E6AB02","#D95F02","#7570B3",
                         "#A6761D","#E7298A","#666666"))
  }
}

set_font_theme <- function(theme){
  
  # Get available fonts depending on OS
  if (.Platform$OS.type == "windows") {
    available_fonts <- c(names(grDevices::windowsFonts()),
                         names(grDevices::postscriptFonts()))
  } else {
    available_fonts <- c(names(grDevices::X11Fonts()),
                         names(grDevices::postscriptFonts()))
  }
  
  candidates <- switch(theme,
                       "iheid" = c("Helvetica", "Arial", "Verdana"),
                       "ethz" = c("DIN Next","Arial"),
                       "uzh" = c("Source Sans", "TheSans", "Palatino"),
                       "rug" = c("Arial","Parry","Georgia","Open Sans"),
                       "oxf" = c("Roboto","Noto Serif","Aktiv Grotesk"),
                       "cmu" = c("Open Sans","Source Serif Pro","Helvetica","Times"),
                       "iast" = c("Gogh","Monserrat","Playfair","Roboto","tse"),
                       "neon" = "Comic Sans MS"
  )
  
  # Find first match
  if(any(candidates %in% available_fonts)){
    font_match <- candidates[candidates %in% available_fonts]
    snet_info("Setting font to {font_match[1]}.")
  } else {
    snet_info("None of the preferred fonts for theme {.emph {theme}},",
              "{candidates}, are available.",
              "Try using {.pkg extrafont} to import and load fonts.",
              "Using default sans-serif font instead.")
    font_match <- "sans"
  }
  options(snet_font = font_match[1])
}
