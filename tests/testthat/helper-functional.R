# Infrastructure for the test-functional_*.R files, mirroring the functional
# testing approach in {manynet} (since v2.2.0, see helper-functional.R there).
# Where manynet enumerates function families (to_*, net_*, ...) over a fixture
# grid, autograph's natural families are its layout algorithms
# (layout_tbl_graph_*), its plot.<class> S3 methods, its palette accessors
# (ag_*), and graphr()'s aesthetic arguments. Each family is enumerated
# automatically from the namespace, so new layouts/methods/palettes are picked
# up and audited without writing new tests. Non-conformant combinations are
# skipped with a structured "AUDIT [...]" message rather than failed, so test
# output can be grepped to find where implementations still need work.

# Exported functions in a family, excluding deprecated/defunct shims
ag_alive_functions <- function(pattern) {
  fns <- sort(grep(pattern, getNamespaceExports("autograph"), value = TRUE))
  keep <- vapply(fns, function(f) {
    fun <- get(f, envir = asNamespace("autograph"))
    is.function(fun) &&
      !grepl("Deprecated|Defunct",
             paste(deparse(body(fun)), collapse = " "))
  }, logical(1))
  fns[keep]
}

# plot.<class> S3 methods registered by autograph
ag_plot_classes <- function() {
  fns <- grep("^plot\\.", ls(asNamespace("autograph")), value = TRUE)
  sort(sub("^plot\\.", "", fns))
}

# Evaluate expr; on error, skip with a structured, greppable audit message.
# Under AUTOGRAPH_STRICT_AUDIT=true (set for CI, see .github/workflows/) the
# error is raised as a failure instead, so a broken layout or plot method
# cannot pass CI green. Locally the default remains skip-and-report, so the
# audits stay usable as a to-do list of implementations still needing work.
run_or_skip <- function(expr, fn, fixture) {
  tryCatch(
    expr,
    error = function(e) {
      msg <- paste0("AUDIT [", fn, " x ", fixture, "]: ", conditionMessage(e))
      if (isTRUE(as.logical(Sys.getenv("AUTOGRAPH_STRICT_AUDIT", "false")))) {
        testthat::fail(msg)
      } else testthat::skip(msg)
    }
  )
}

# A plot is only really tested once it is built: building forces layout
# computation, aesthetic mapping, and scale training, which is where most
# plotting bugs live. Accepts ggplot, patchwork, or ggraph objects.
expect_buildable <- function(p) {
  testthat::expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  testthat::expect_true(!is.null(built))
  invisible(built)
}

# Standard grid of fixture networks covering the formats autograph's
# layouts and graphr() aesthetics are expected to handle.
ag_fixtures <- local({
  set.seed(1234)
  list(
    basic      = manynet::create_ring(8),
    directed   = manynet::generate_random(8, directed = TRUE),
    labelled   = manynet::ison_adolescents,
    twomode    = manynet::ison_southern_women,
    weighted   = manynet::add_tie_attribute(manynet::create_ring(8), "weight",
                                            rep(c(1, 2), each = 4)),
    signed     = manynet::to_signed(manynet::create_ring(8)),
    attributed = manynet::add_node_attribute(manynet::create_ring(8), "group",
                                             rep(c("A", "B"), each = 4)),
    multiplex  = manynet::ison_algebra,
    tree       = manynet::create_tree(10, directed = TRUE)
  )
})

