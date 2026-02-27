# nocov start
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
# nocov end

seq_nodes <- function(.data){
  seq.int(manynet::net_nodes(.data))
}

add_spaces <- function(CamelString) {
  gsub("([a-z])([A-Z])", "\\1 \\2", CamelString)
}

# Note that we use patchwork and not gridExtra, cowplot, or ggpubr
# because patchwork is the only one that doesn't mess with ggplot2 themes
# and aesthetics. It just arranges plots as they are.
# See https://patchwork.data-imaginists.com/
# and
# https://cran.r-project.org/web/packages/patchwork/vignettes/patchwork.html

