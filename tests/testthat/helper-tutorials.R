# Infrastructure for test-tutorials_autograph.R, mirroring the tutorial
# functional testing in {manynet} (tests/testthat/helper-manynet.R there).
# The tutorials' code chunks are extracted to a script and evaluated
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

# Extract the R code from an R Markdown / learnr tutorial's `{r}` chunks, in
# document order, dropping any chunk marked `purl = FALSE`. This replicates the
# only part of knitr's purl() we rely on, so that {knitr} need not be a package
# dependency (it was otherwise used solely to run these tutorial tests). These
# tutorials use no child documents, chunk references, or non-R engines, so this
# simple line scanner is sufficient and matches purl()'s output expression set.
# Kept identical to the same helper in {manynet} and {netrics}.
extract_rmd_code <- function(path) {
  lines <- readLines(path, warn = FALSE)
  open_re  <- "^\\s*```+\\s*\\{[rR][\\s,}]"
  close_re <- "^\\s*```+\\s*$"
  code <- character()
  i <- 1L
  n <- length(lines)
  while (i <= n) {
    if (grepl(open_re, lines[i], perl = TRUE)) {
      keep <- !grepl("purl\\s*=\\s*(FALSE|F)\\b", lines[i])
      i <- i + 1L
      while (i <= n && !grepl(close_re, lines[i])) {
        if (keep) code <- c(code, lines[i])
        i <- i + 1L
      }
    }
    i <- i + 1L
  }
  code
}

check_tute_functions <- function(path, skip = "ergm\\(|grapht\\("){
  exprs <- parse(text = extract_rmd_code(path))
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

      # Only fail if the tutorial calls the deprecated function itself. A
      # dependency that calls a deprecated function of its own dependency
      # raises the same warning, and no edit to this tutorial can silence it.
      if (!is.null(w)) {
        code <- paste(deparse(exprs[[i]]), collapse = " ")
        # The name a deprecation warning reports, in either the base R form
        # ("'to_ties' is deprecated") or the {lifecycle} form
        # ("`to_ties()` was deprecated in manynet 2.3.0").
        hits <- regmatches(msg, gregexpr(
          "[`'\"][^`'\"]+[`'\"][^[:alpha:]]*(is|was|has been)[[:space:]]+(deprecated|defunct|moved)",
          msg))[[1]]
        named <- gsub("^[`'\"]([^`'\"]+)[`'\"].*$", "\\1", hits)
        named <- gsub("\\(\\)$", "", named)
        if (length(named) > 0 &&
            !any(vapply(named, grepl, logical(1), x = code, fixed = TRUE))) {
          w <- NULL
        }
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
