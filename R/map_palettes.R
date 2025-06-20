# Helper function to check whether a color is light or dark:
is_dark <- function(hex) {
  # Google luma formula for details.
  luma <- 0.33 * grDevices::col2rgb(hex)[[1]] +
    0.5 * grDevices::col2rgb(hex)[[2]] +
    0.16 * grDevices::col2rgb(hex)[[3]]
  isdark <- ifelse(luma < 186, TRUE, FALSE)
  isdark
}

#' Consistent palette calls
#' @name ag_call
#' @param number Integer of how many category colours to return.
#' @importFrom grDevices colorRampPalette
#' @export
ag_base <- function(){
  utils::head(getOption("snet_highlight", default = "black"), n = 1)
}

#' @rdname ag_call
#' @export
ag_highlight <- function(){
  utils::tail(getOption("snet_highlight", default = "red"), n = 1)
}

#' @rdname ag_call
#' @export
ag_positive <- function(){
  utils::tail(getOption("snet_div", default = "#4575b4"), n = 1)
}

#' @rdname ag_call
#' @export
ag_negative <- function(){
  utils::head(getOption("snet_div", default = "#d73027"), n = 1)
}

#' @rdname ag_call
#' @export
ag_qualitative <- function(number){
  snet_colors <- getOption("snet_cat", default = "default")
  colorRampPalette(snet_colors)(number)
}

#' @rdname ag_call
#' @export
ag_sequential <- function(number){
  snet_colors <- getOption("snet_highlight", default = "default")
  if(length(snet_colors)==1) snet_colors <- c("white", snet_colors[1])
  colorRampPalette(snet_colors)(number)
}

#' @rdname ag_call
#' @export
ag_divergent <- function(number){
  snet_colors <- getOption("snet_div", default = "default")
  if(length(snet_colors)==2) 
    snet_colors <- c(snet_colors[1], "white", snet_colors[2])
  colorRampPalette(snet_colors)(number)
}
