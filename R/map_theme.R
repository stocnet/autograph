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

