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

add_spaces <- function(CamelString) {
  gsub("([a-z])([A-Z])", "\\1 \\2", CamelString)
}

# Note that we use patchwork and not gridExtra, cowplot, or ggpubr
# because patchwork is the only one that doesn't mess with ggplot2 themes
# and aesthetics. It just arranges plots as they are.
# See https://patchwork.data-imaginists.com/
# and
# https://cran.r-project.org/web/packages/patchwork/vignettes/patchwork.html


# Remembered preferences ----

# A preference the user asked to keep, such as a theme or whether argument
# values are completed, is written to the user's configuration directory. Only
# ever called with `persist = TRUE`, i.e. at the user's explicit request.
# Failure is not worth an error: the choice still holds for this session.
pref_file <- function(name) {
  file.path(tools::R_user_dir("autograph", which = "config"),
            paste0(name, ".rds"))
}

write_pref <- function(name, value) {
  f <- pref_file(name)
  tryCatch({
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    saveRDS(value, f)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
}

read_pref <- function(name) {
  f <- pref_file(name)
  if (!file.exists(f)) return(NULL)
  tryCatch(readRDS(f), error = function(e) NULL)
}

forget_pref <- function(name) {
  f <- pref_file(name)
  if (file.exists(f)) tryCatch(unlink(f), error = function(e) NULL)
  invisible(NULL)
}
