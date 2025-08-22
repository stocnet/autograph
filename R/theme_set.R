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
#' @name theme_set
#' @returns This function sets the theme and palette(s) to be used across all
#'   stocnet packages. The palettes are written to options and held there.
#' @examples
#' stocnet_theme("default")
#' plot(manynet::node_degree(ison_karateka))
#' stocnet_theme("rug")
#' plot(manynet::node_degree(ison_karateka))
NULL

#' @rdname theme_set
#' @param theme String naming a theme.
#'   By default "default".
#'   This string can be capitalised or not.
#' @export
stocnet_theme <- function(theme = NULL){
  if(is.null(theme)){
    theme <- getOption("stocnet_theme", default = "default")
    cli::cli_alert_info("Theme is set to {.emph {theme}}.")
    cli::cli_alert_info("The following themes are available: {.emph {theme_opts}}.")
  } else {
    theme <- tolower(theme)
    if(theme %in% theme_opts){
      options(stocnet_theme = theme)
      set_highlight_theme(theme)
      set_divergent_theme(theme)
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

set_background_theme <- function(theme){
  if(theme == "neon"){
    options(snet_background = "#070f23")
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

colorsafe_palette <- c("#d73027", "#4575b4", "#1B9E77","#D95F02","#7570B3",
                       "#E7298A", "#66A61E","#E6AB02","#A6761D","#666666")
