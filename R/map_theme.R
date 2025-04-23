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

