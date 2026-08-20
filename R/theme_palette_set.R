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
#'   The "clay" theme follows the palette and fonts used in the slides and
#'   documents that Anthropic's Claude produces: an ivory background,
#'   a slate ink base, and a clay orange highlight.
#'   Most themes are designed to be colour-blind safe.
#'   
#' @name theme_set
#' @family themes
#' @section Fonts: 
#'   Some themes also set a preferred font for use in plots, 
#'   if available on the system (a check is performed).
#'   In some cases, this includes a vector of options to try in sequence.
#'   If none of the preferred fonts are available, a sans-serif font is used.
#'   Themes then look much more alike than they should,
#'   since the typeface carries a good deal of an institution\'s identity.
#'   Call `list_fonts()` to see which font families R can currently see,
#'   and `ag_font()` to see which one the current theme settled on.
#'   
#'   To make more fonts available, there are two steps.
#'   
#'   1. Install the font on your computer.
#'   Many of the fonts these themes prefer are free:
#'   Google Fonts (<https://fonts.google.com>) offers Roboto, Open Sans,
#'   Source Sans 3, Source Serif 4, Noto Serif, Montserrat, and Playfair
#'   Display, among others.
#'   Download the family, then install it as you would any other font:
#'   double-click the files and choose "Install" on Windows,
#'   open them in Font Book on macOS,
#'   or copy them into `~/.local/share/fonts` and run `fc-cache -f` on Linux.
#'   Some fonts are licensed and are only available to members of the
#'   institution concerned, or for purchase;
#'   the theme falls back to a near relative where it can.
#'   2. Make the font available to R.
#'   Install the `{systemfonts}` package and the fonts installed on your system
#'   are found directly, with no further step.
#'   Otherwise, use `extrafont::font_import()` once and
#'   `extrafont::loadfonts()` in each session.
#'   Restart R after installing a font, then call `list_fonts()` to check that
#'   the family is now listed, and set the theme again.
#'   
#'   Note that a font is only used where the graphics device can draw it.
#'   The `{ragg}` devices (for example `ragg::agg_png()`) and `{svglite}` are
#'   the most reliable;
#'   the default PDF device needs the font embedded,
#'   for which `extrafont::embed_fonts()` is available.
#' @section Custom: 
#'   If you have specific needs or preferences, you can
#'   set your own palettes or overwrite part of an existing one using `options()`.
#'   For example, to set a custom base color, you can use:
#'   `options(snet_highlight = c("#1b9e77", "#d95f02", "#7570b3"))`.
#'   This will set a custom highlight color palette.
#'   Similarly, you can set `snet_div` for divergent palettes
#'   and `snet_cat` for categorical palettes.
#' @returns This function sets the theme and palette(s) to be used across all
#'   stocnet packages. The palettes are written to options and held there.
#' @examples
#' stocnet_theme("default")
#' plot(netrics::node_by_degree(ison_karateka))
#' stocnet_theme("uzh")
#' plot(netrics::node_by_degree(ison_karateka))
NULL

# Themes whose palette order carries meaning of its own, and so is not
# reordered for colour blindness. See the Colour blindness section of ?ag_call.
colorblind_unsorted <- "rainbow"

theme_opts <- c("default", "bw", "crisp", "neon", "clay", 
                "iheid", "ethz", "uzh", "rug", "unibe", 
                "oxf", "unige", "cmu", "iast", "hwu",
                "rainbow")

#' @rdname theme_set
#' @param theme String naming a theme.
#'   By default "default".
#'   The following themes are currently available:
#'   `r autograph:::theme_opts`.
#'   This string can be capitalised or not.
#' @param persist Logical, by default FALSE.
#'   If TRUE, the theme is remembered across sessions,
#'   by writing it to the user's configuration directory
#'   (see `tools::R_user_dir()`).
#'   Nothing is written to disk unless this is set explicitly.
#'   Use `stocnet_theme(persist = FALSE)` when setting a theme
#'   to forget a previously persisted choice.
#' @importFrom manynet snet_info snet_success
#' @export
stocnet_theme <- function(theme = NULL, persist = FALSE){
  if(is.null(theme)){
    theme <- getOption("stocnet_theme", default = "default")
    snet_info("Theme is currently set to {.emph {theme}}.",
              "The following themes are available: {.emph {theme_opts}}.")
  } else {
    if(!is.character(theme) || length(theme) != 1L)
      manynet::snet_abort(
        "{.arg theme} should be the name of a single theme, given as a string.",
        "The themes available are {.val {theme_opts}}.")
    # An unrecognised theme used to warn and leave the theme unchanged, which
    # was easy to miss and left plots looking wrong for no visible reason.
    theme <- .match_name(tolower(theme), theme_opts, "theme", what = "theme")
    options(stocnet_theme = theme)
    set_highlight_theme(theme)
    set_ink_theme(theme)
    set_divergent_theme(theme)
    set_background_theme(theme)
    set_categorical_theme(theme)
    set_font_theme(theme)
    snet_success("Theme set to {.emph {theme}}.")
    if(persist){
      if(write_theme_pref(theme))
        snet_success("Theme will be remembered in future sessions.")
    } else forget_theme_pref()
  }
}

# The reading and writing itself is shared with any other remembered
# preference; see write_pref() in autograph_utilities.R.
theme_pref_file <- function() pref_file("theme")

write_theme_pref <- function(theme) write_pref("theme", theme)

forget_theme_pref <- function() forget_pref("theme")

read_theme_pref <- function(){
  theme <- read_pref("theme")
  # Guard against a stale file naming a theme this version no longer ships.
  if(is.null(theme) || !is.character(theme) || length(theme) != 1L ||
     !theme %in% theme_opts) return(NULL)
  theme
}

#' @rdname theme_set
#' @export
set_stocnet_theme <- stocnet_theme

set_background_theme <- function(theme){
  if(theme == "neon"){
    options(snet_background = "#070f23")
  } else if(theme == "cmu"){
    options(snet_background = "#E4DAC4")
  } else if(theme == "clay"){
    options(snet_background = "#F0EEE6")
  } else {
    options(snet_background = "#FFFFFF")
  }
}

# The ink is the dark colour a plot writes with: axis text, reference lines,
# and other chrome. It is kept apart from the base, which is the colour of an
# unhighlighted mark, because the two roles pull in opposite directions. A
# base must stand away from the highlight, which for a dark brand colour means
# a lighter grey; ink must stay legible, which means a dark one.
set_ink_theme <- function(theme){
  options(snet_ink = switch(theme,
                            "neon" = "#EDEDF4",
                            "iheid" = "#000010",
                            "oxf" = "#002147",
                            "hwu" = "#0A3E65",
                            "clay" = "#3D3D3A",
                            "crisp" = "#101314",
                            "cmu" = "#1A1A1A",
                            "bw" = "#000000",
                            "rug" = "#000000",
                            "#121212"))
}

set_highlight_theme <- function(theme){
  hl <- switch(theme,
               "iheid" = c("#000010","#E20020"),
               "unige" = c("#A3A3A3","#CF0063"),
               "rug" = c("#000000", "#dc002d"),
               "uzh" = c("#a3adb7", "#dc6027"),
               "unibe" = c("#121212", "#e4003c"),
               "oxf" = c("#002147", "#c09725"),
               # The ETH grey and blue used to sit 28 apart under
               # simulation, both being mid-dark; the lighter grey separates
               # them by lightness, which every viewer keeps.
               "ethz" = c("#919191", "#0028a5"),
               "cmu" = c("#8F9194", "#C41230"),
               "iast" = c("#555", "#e54a37"),
               "hwu" = c("#0A3E65", "#0095DB"),
               "crisp" = c("#bfbfbf", "#101314"),
               "bw" = c("#CCCCCC", "#000000"),
               # The neon cyan and green scored 12.7 apart under simulation,
               # so the highlight is now a yellow that keeps the same voltage.
               "neon" = c("#5aeafd", "#fdfd54"),
               "clay" = c("#3D3D3A", "#D97757"),
               "rainbow" = c('#1965B0', '#DC050C'),
               c("#4576B5", "#D83127"))
  options(snet_highlight = hl)
}

# "#E20020" - IHEID red
# "#215CAF" - ETH blue
# "#EDEDF4" - ghost white
# "#071013" - rich black
# "#EDAE49" - hunyadi yellow
# "#3C493F" - field green
# "#679289" - viridian

set_divergent_theme <- function(theme){
  # Each triplet runs warm pole, light middle, cool pole, so that a reader
  # meets the same convention in every theme, and so that the poles differ in
  # lightness as well as in hue. Several of these used to pair a red pole with
  # a green or teal one, drawn from the same institutional palette, which is
  # the one pairing that red-green colour blindness cannot resolve: the ETH
  # red and olive poles scored 3.8 apart under simulation, where 10 is already
  # confusable. The poles below are still each theme's own colours, chosen for
  # the widest separation the palette allows. See [contrast_colors()].
  dv <- switch(theme,
               "iheid" = c("#820C2B","white","#006EAA"),
               "unige" = c("#F42941","white","#0067C5"),
               "ethz" = c("#B7352D","white","#0028a5"),
               "uzh" = c("#FC4C02","white","#0028A5"),
               "unibe" = c("#8a1e22","white","#4767af"),
               "oxf" = c('#FB5607', 'white', '#002147'),
               "cmu" = c("#C41230","#E4DAC4","#007BC0"),
               "iast" = c("#e62117","#999","#3b5998"),
               "hwu" = c("#E38C33","white","#0A3E65"),
               "bw" = c("black","grey","white"),
               "clay" = c("#D97757","#F0EEE6","#6B5B95"),
               "rainbow" = c('#DC050C','#CAE0AB','#882E72'),
               c("#d73027","white","#4575b4"))
    options(snet_div = dv)
}

set_categorical_theme <- function(theme){
  if(theme == "bw"){
    pal <- c("#CCCCCC", "#000000")
  } else if(theme == "iheid"){
    pal <- c("#006564","#0094D8","#622550",
                         "#268D2B","#3E2682","#820C2B",
                         "#008F92","#006EAA","#A8086E")
  } else if(theme == "unige"){
    pal <- c("#F42941","#0067C5","#96004B",
                         "#007E64","#465F7F","#F1AB00",
                         "#00B1AE","#4B0B71","#FF5C00")
  } else if(theme == "ethz"){
    pal <- c("#215CAF","#007894","#627313",
                         "#8E6713","#B7352D","#A7117A","#6F6F6F")
  } else if(theme == "cmu"){
    pal <- c("#EF3A47","#FDB515","#009647",
                         "#008F91","#043673","#007BC0",
                         "#1F4C4C","#719F94")
  } else if(theme == "iast"){
    pal <- c("#fbda26","#0a0","#9c1a1a",
                         "#1b870b","#3d86d8","#50e3c2",
                         "#7ad03d","#fe0087","#e62117",
                         "#1db6d6","#3b5998","#f58b4c",
                         "#e9711c","#ff2b46","#d9372f",
                         "#2fa7d5","#f0c020","#47c965")
  } else if(theme == "hwu"){
    pal <- c("#342B20","#6A5B49","#947F68","#F7D6A8",
                         "#1A4323","#2C642C","#BBC33E","#D3E3BE",
                         "#5C1F0A","#B25A22","#E38C33","#F5D1A7",
                         "#921E3F","#D32D5C","#DD7488","#E59CBB",
                         "#490C3B","#782066","#B84E8F","#D9AACA",
                         "#031B39","#0A3E65","#0095DB","#C4CEDE")
  } else if(theme == "uzh"){
    pal <- c("#0028A5","#4AC9E3","#A4D233",
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
                         "#A27200","#7E2601","#60061F")
    
  } else if(theme == "unibe"){
    pal <- c("#466553","#668271","#8aa092","#afbfb5","#d6ded9",
                         "#007ea2","#5294b4","#85adc6","#b0c7d9","#d8e2ec",
                         "#203a5d","#4a5575","#757792","#a1a0b4","#d0ced9",
                         "#8a1e22","#a14540","#b86f65","#d19d93","#e8cdc6",
                         "#5a3217","#754e31","#927157","#b49b87","#d7cac0",
                         "#36b5b6","#75c4c5","#a0d3d4","#c4e3e3","#e2f1f2",
                         "#ec627d","#f08797","#f4a9b1","#f8c8cc","#fce4e7",
                         "#4767af","#6e82c0","#949fd1","#b9bee1","#dcdef1",
                         "#c2b600","#cfc43c","#dcd274","#e8e1a4","#f4f0d3",
                         "#ee7402","#f3923e","#f7af70","#fbcba1","#fde6d1")
    
  } else if(theme == "clay"){
    pal <- c("#D97757","#5B6E8F","#788C5D",
                         "#D4A27F","#6B5B95","#B1ADA1",
                         "#A8563C","#8CA3B0","#3D3D3A")
  } else if(theme == "rainbow"){
    pal <- c('#E8ECFB', '#D9CCE3', '#D1BBD7', 
                         '#CAACCB', '#BA8DB4', '#AE76A3', 
                         '#AA6F9E', '#994F88', '#882E72', 
                         '#1965B0', '#437DBF', '#5289C7', 
                         '#6195CF', '#7BAFDE', 
                         '#4EB265', '#90C987', '#CAE0AB', 
                         '#F7F056', '#F7CB45', '#F6C141', 
                         '#F4A736', '#F1932D', '#EE8026', 
                         '#E8601C', '#E65518', '#DC050C', 
                         '#A5170E', '#72190E', '#42150A')
  } else if(theme == "oxf"){
    pal <- c("#776885", '#E08D79', '#ED9390', 
                         '#C4A29E', '#D1BDD5', '#994636', 
                         '#AA1A2D', '#7F055F', '#FE615A', 
                         '#D4CDF4', '#FB5607', '#E6007E', 
                         '#426A5A', '#789E9E', 
                         '#E2C044', '#E4F0EF', '#B9D6F2', 
                         '#A0AF84', '#15616D', '#1D42A6', 
                         '#00AAB4', '#65E5AE', '#95C11F', 
                         '#49B6FF', '#F7EF66')
  } else {
    pal <- c("#1B9E77","#4575b4","#d73027",
                         "#66A61E","#E6AB02","#D95F02","#7570B3",
                         "#A6761D","#E7298A","#666666")
  }
  # The palettes are written above in whatever order their source gives them,
  # which is often a brand's own listing, or families of tints. Neither order
  # separates the first few colours a plot actually uses, so reorder for that.
  # The "rainbow" theme is the exception: its order is the palette, so it is
  # left as the observed spectrum runs.
  spread <- theme %in% colorblind_unsorted
  if(!spread)
    pal <- colorblind_sort(pal, getOption("snet_background", default = "#FFFFFF"))
  # A spectrum is only a spectrum if a plot draws from the whole of it, so a
  # palette left in its own order is sampled across its length instead of
  # taken from the front.
  options(snet_cat = pal, snet_cat_spread = spread)
}

# Every autograph plot is drawn on the theme's own ground, not only the graphs
# that graphr() and grapht() draw. A theme with a dark background used to give
# a dark network plot and white-backed panels for everything else, which left
# the "neon" highlights unreadable on the plots that missed out.
ag_ground <- function(base){
  bg <- getOption("snet_background", default = "#FFFFFF")
  if(bg == "#FFFFFF") return(base)
  out <- base + ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = bg, colour = NA),
    panel.background = ggplot2::element_rect(fill = bg, colour = NA),
    legend.background = ggplot2::element_rect(fill = bg, colour = NA),
    legend.key = ggplot2::element_rect(fill = bg, colour = NA))
  # Only recolour text the base theme actually draws. Handing an element_text()
  # to a theme that had blanked it puts the element back: colouring the axis
  # text of ag_theme_void() drew axis ticks and coordinates onto graphs, which
  # a graph has no use for, and which the white-backed themes never showed.
  for(part in c("text", "axis.text", "strip.text", "legend.text",
                "plot.title", "plot.subtitle", "plot.caption")){
    if(inherits(out[[part]], "element_blank")) next
    out <- out + ggplot2::theme(
      !!part := ggplot2::element_text(colour = ag_ink()))
  }
  # A theme that blanks its strips has no strip background to fill either.
  if(!inherits(out[["strip.text"]], "element_blank"))
    out <- out + ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = bg, colour = NA))
  out
}

# The colour a plot is drawn on, which is white unless the theme says otherwise.
ag_ground_fill <- function(){
  getOption("snet_background", default = "#FFFFFF")
}

ag_theme_minimal <- function(...) ag_ground(ggplot2::theme_minimal(...))

ag_theme_classic <- function(...) ag_ground(ggplot2::theme_classic(...))

ag_theme_grey <- function(...) ag_ground(ggplot2::theme_grey(...))

ag_theme_bw <- function(...) ag_ground(ggplot2::theme_bw(...))

ag_theme_void <- function(...) ag_ground(ggplot2::theme_void(...))

