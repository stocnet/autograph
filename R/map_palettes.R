#' @importFrom grDevices colorRampPalette
#' @export
ag_qualitative <- function(number){
  snet_colors <- getOption("snet_cat", default = "default")
  colorRampPalette(snet_colors)(number)
}

#' @export
ag_sequential <- function(number){
  snet_colors <- getOption("snet_highlight", default = "default")
  if(length(snet_colors)==1) snet_colors <- c("white", snet_colors[1])
  colorRampPalette(snet_colors)(number)
}

#' @export
ag_divergent <- function(number){
  snet_colors <- getOption("snet_div", default = "default")
  if(length(snet_colors)==2) 
    snet_colors <- c(snet_colors[1], "white", snet_colors[2])
  colorRampPalette(snet_colors)(number)
}