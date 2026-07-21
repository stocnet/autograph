# Infrastructure for test-tutorials_autograph.R, mirroring the tutorial
# functional testing in {manynet} (tests/testthat/helper-manynet.R there).
# The tutorials' code chunks are purled to a script and evaluated
# expression by expression, so that any chunk that errors or raises a
# deprecation warning fails the suite. Rendering the learnr tutorials
# themselves is deliberately not tested here.

find_pkg_tutorial_paths <- function(pkg) {
  tute_folders <- list.dirs(system.file("tutorials", package = pkg),
                            recursive = FALSE)
  tute_files <- unlist(lapply(tute_folders, function(folder) {
    list.files(folder, pattern = "*.Rmd", full.names = TRUE)
  }))
  tute_files
}

check_tute_functions <- function(path, skip = "ergm\\(|grapht\\(", quiet = TRUE){
  tmp <- tempfile(fileext = ".R")
  knitr::purl(
    input  = path,
    output = tmp,
    quiet  = quiet
  )
  exprs <- parse(tmp)
  env <- new.env(parent = globalenv())

  is_skipped_call <- function(expr) {
    any(grepl(skip, deparse(expr)))
  }

  for (i in seq_along(exprs)) {
    # Stop at the first slow call: it and any later (dependent) expressions
    # are skipped, but we return normally so the caller's loop over the
    # remaining tutorials continues. Using skip() here would unwind to the
    # enclosing test_that() and abort every subsequent tutorial too.
    if (is_skipped_call(exprs[[i]])) {
      break
    }

    w <- NULL
    e <- NULL
    m <- NULL

    not_out <- withCallingHandlers(
      tryCatch(
        eval(exprs[[i]], envir = env),
        error = function(err) {
          e <<- err
          NULL
        }
      ),
      warning = function(wrn) {
        w <<- wrn
        invokeRestart("muffleWarning")
      },
      message = function(msg) {
        m <<- c(m, conditionMessage(msg))
        invokeRestart("muffleMessage")
      }
    )

    # If there *was* a warning, check if it's a deprecated/defunct one
    if (!is.null(w)) {
      msg <- conditionMessage(w)

      # Only fail if it's a deprecated/defunct warning
      if (!grepl("deprecate|defunct|moved", msg, ignore.case = TRUE)) {
        w <- NULL
      }
    }

    # Now test what happened
    expect_null(
      e,
      info = paste0("Error in expression ", i,
                    " of ", basename(path), ": ", deparse(exprs[[i]]))
    )

    expect_null(
      w,
      info = paste("Warning in expression", i, ":", deparse(exprs[[i]]))
    )
  }
}
