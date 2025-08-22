# Helper function for checking and downloading packages
thisRequires <- function(pkgname){
  if (!requireNamespace(pkgname, quietly = TRUE) & interactive()) {
    if(utils::askYesNo(msg = paste("The", pkgname, 
                                   "package is required to run this function. Would you like to install", pkgname, "from CRAN?"))) {
      utils::install.packages(pkgname)
    } else {
      manynet::snet_abort(paste("Please install", pkgname, "from CRAN to run this function."))
    }
  }
}

thisRequiresBio <- function(pkgname) {
  if (!requireNamespace(pkgname, quietly = TRUE) & interactive()) {
    if(utils::askYesNo(msg = paste("The", pkgname, 
                                   "package is required to run this function. Would you like to install", pkgname, "from BioConductor?"))) {
      thisRequires("BiocManager")
      BiocManager::install(pkgname)
    }}
}

#' Add ggplot objects together
#' @description
#' This function allows you to add ggplot objects together using the `+` operator.
#' This is made possible by the `{patchwork}` package.
#' This is useful for combining multiple plots into a single figure.
#' @param e1,e2 ggplot objects
#' @param ... Other arguments passed to `patchwork::wrap_plots()`.
#' @export
`+.ggplot` <- function(e1, e2, ...) {
  if (inherits(e2, c("ggplot", "ggplot2::ggplot"))) {
    thisRequires("patchwork")
    patchwork::wrap_plots(e1, e2, ...)
  } else {
    NextMethod()
  }
}
