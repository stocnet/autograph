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

# Squash a vector into the unit interval. Shared by the layouts, which place
# their coordinates there, and by the grid snapping, which reads them back.
.rescale <- function(vector){
  (vector - min(vector)) / (max(vector) - min(vector))
}

# A plot has one caption, and more than one step may have something to say in
# it: a scaled layout reports its fit, and a plot that sets its isolates aside
# names them. Each is added rather than assigned, so that the second does not
# replace the first.
.add_caption <- function(p, text) {
  old <- p[["labels"]][["caption"]]
  if (!is.null(old) && !is.na(old) && nzchar(old))
    text <- paste(old, text, sep = " | ")
  p + ggplot2::labs(caption = text)
}

# Every layout returns its coordinates as a two-column data frame named x and y.
.to_lo <- function(mat) {
  res <- as.data.frame(mat)
  names(res) <- c("x","y")
  res
}
