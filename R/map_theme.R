#' Many themes
#' 
#' @description
#'   This function enables all plots to be 
#'   quickly, easily and consistently themed.
#'   This is achieved by setting a theme option that enables
#'   the appropriate palette to be used for all autograph-consistent
#'   plotting methods.
#'   
#'   The following themes are currently available:
#'   `r autograph:::theme_opts`.
#' @name map_themes
#' @examples
#' to_mentoring(ison_brandes) %>%
#'   mutate(color = c(rep(c(1,2,3), 3), 3)) %>%
#'   graphr(node_color = "color") +
#'   labs(title = "Who leads and who follows?") +
#'   scale_color_iheid() +
#'   theme_iheid()
NULL

#' @rdname map_themes
#' @param theme String naming a theme.
#'   By default "default".
#' @export
snet_theme <- function(theme = NULL){
  if(is.null(theme)){
    theme <- getOption("snet_theme", default = "default")
    cli::cli_alert_info("Theme is set to {.emph {theme}}.")
  } else {
    if(theme %in% theme_opts){
      options(snet_theme = theme)
      set_highlight_theme(theme)
      set_background_theme(theme)
      set_categorical_theme(theme)
      cli::cli_alert_success("Theme set to {.emph {theme}}.")
    } else {
      cli::cli_alert_warning("Please choose one of the available themes: {.emph {theme_opts}}.")
    }
  }
}

theme_opts <- c("default", "bw",
                "iheid", "ethz", "uzh", "rug", "unibe",
                "crisp", "neon", "rainbow")

set_highlight_theme <- function(theme){
  if(theme == "iheid"){
    options(snet_highlight = c("#000010","#E20020"))
  } else if(theme == "rug"){
    options(snet_highlight = c("#000000", "#dc002d"))
  } else if(theme == "uzh"){
    options(snet_highlight = c("#a3adb7", "#dc6027"))
  } else if(theme == "unibe"){
    options(snet_highlight = c("#121212", "#E3003A"))
  } else if(theme == "ethz"){
    options(snet_highlight = c("#6F6F6F", "#0028a5"))
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

set_background_theme <- function(theme){
  if(theme == "neon"){
    options(snet_background = "#070f23")
  } else {
    options(snet_background = "#FFFFFF")
  }
}

set_categorical_theme <- function(theme){
  if(theme == "bw"){
    options(snet_cat = c("#CCCCCC", "#000000"))
  } else if(theme == "iheid"){
    options(snet_cat = c("#006564","#0094D8","#622550",
                         "#268D2B","#3E2682","#820C2B",
                         "#008F92","#006EAA","#A8086E"))
  } else if(theme == "ethz"){
    options(snet_cat = c("#215CAF","#007894","#627313",
                         "#8E6713","#B7352D","#A7117A","#6F6F6F"))
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
  } else {
    options(snet_cat = c("#1B9E77","#4575b4","#d73027",
                         "#66A61E","#E6AB02","#D95F02","#7570B3",
                         "#A6761D","#E7298A","#666666"))
  }
}
