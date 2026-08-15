# Functional audit of the layout family. Rather than pinning a fixture per
# layout, this reads the applicability contract the package itself declares in
# .layout_requirements() (R/graph_checks.R) and selects fixtures from a shared
# pool accordingly. Adding a layout therefore needs no change here: declare its
# requirement next to the others and the audit picks it up.
#
# Both sides of the contract are audited. Where a layout applies, it must draw
# a buildable plot and must NOT announce a substitution; where it does not, it
# must still draw something and must say what it needed and what it used.

# Arguments some layouts require, keyed by argument name rather than by layout,
# and derived from formals() below. A new layout taking a `membership` needs
# nothing added here.
#
# An argmaker returns NULL where the network cannot support the argument. That
# matters for concentric and multilevel, whose requirement is "two-mode OR an
# explicit partition": supplying one unconditionally would make them applicable
# to everything, and the inapplicable half of the contract would go untested.
layout_argmakers <- list(
  membership = function(net) if (manynet::is_twomode(net)) "type"
    else if (manynet::is_labelled(net))
      rep(c("a", "b"), length.out = as.integer(manynet::net_nodes(net)))
    else NULL,
  level      = function(net) if (manynet::is_twomode(net)) "type" else NULL,
  rank       = function(net) if (manynet::is_labelled(net))
    seq_len(as.integer(manynet::net_nodes(net))) else NULL
)

# `center` names one of the two modes rather than a node attribute, so it is
# the one argument that cannot be made generically.
layout_center_arg <- function(net) {
  if (!manynet::is_twomode(net)) return(NULL)
  list(center = "events")
}

# Arguments with no default, minus the ones every layout takes.
ag_required_args <- function(fn) {
  fm <- formals(get(fn, envir = asNamespace("autograph")))
  req <- names(fm)[vapply(fm, function(x) identical(x, quote(expr = )), logical(1))]
  setdiff(req, c(".data", "..."))
}

# Build the extra arguments a layout needs for a given network.
layout_extra_args <- function(lay, net) {
  fn <- paste0("layout_tbl_graph_", lay)
  if (!exists(fn, envir = asNamespace("autograph"))) return(NULL)
  args <- list()
  for (a in ag_required_args(fn)) {
    if (!is.null(layout_argmakers[[a]])) args[[a]] <- layout_argmakers[[a]](net)
  }
  if (lay == "hierarchy") args <- c(args, layout_center_arg(net))
  args
}

# Partition the pool by the package's own predicate, capped so the audit stays
# proportionate: every layout is checked on two networks it should handle and
# one it should not, which is enough to exercise both sides of the contract
# without running every layout against every fixture. Deterministic (the first
# matches in pool order), so a failure reproduces.
layout_candidates <- function(lay, n_ok = 2, n_no = 1) {
  fn <- paste0("layout_tbl_graph_", lay)
  needed <- if (exists(fn, envir = asNamespace("autograph")))
    ag_required_args(fn) else character()
  applies <- vapply(names(ag_layout_pool), function(nm) {
    net <- ag_layout_pool[[nm]]
    args <- layout_extra_args(lay, net)
    if (!all(needed %in% names(args))) {
      # A required argument this network cannot supply. Two different cases:
      # where the layout declares a requirement the network also fails
      # (concentric and multilevel need two modes *or* an explicit partition),
      # graphr() substitutes before ever calling it, so this is a genuine
      # inapplicable case. Where it declares none (lineage needs a `rank` that
      # an unlabelled network has nothing to give), the call would rightly
      # abort asking for the argument, so leave it out of the pool entirely.
      declared <- !is.null(autograph:::.layout_requirements()[[lay]])
      fails <- !isTRUE(do.call(autograph:::.layout_applies,
                               c(list(net, lay), args)))
      return(if (declared && fails) FALSE else NA)
    }
    # Judged with the same arguments the audit will pass, since for concentric
    # and multilevel an explicit membership/level is itself what makes the
    # layout applicable.
    isTRUE(do.call(autograph:::.layout_applies, c(list(net, lay), args)))
  }, logical(1))
  applies <- applies[!is.na(applies)]
  list(ok = utils::head(names(applies)[applies], n_ok),
       no = utils::head(names(applies)[!applies], n_no))
}

# Capture whether graphr() announced a layout substitution.
layout_substituted <- function(expr) {
  msgs <- character()
  p <- withCallingHandlers(expr, message = function(m) {
    msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage")
  })
  list(plot = p, substituted = any(grepl("is used instead", msgs)), msgs = msgs)
}

test_that("every layout applies where the package says it does", {
  skip_on_cran()
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  layouts <- sub("^layout_tbl_graph_", "",
                 ag_alive_functions("^layout_tbl_graph_"))
  expect_true(length(layouts) > 0)
  reqs <- autograph:::.layout_requirements()
  for (lay in layouts) {
    cand <- layout_candidates(lay)
    # Pool honesty: an audit that silently has nothing to test is worse than a
    # failing one, so say so rather than passing vacuously.
    if (length(cand$ok) == 0) {
      fail(paste0("AUDIT [layout ", lay, "]: no applicable network in the pool"))
      next
    }
    if (!is.null(reqs[[lay]]) && length(cand$no) == 0) {
      fail(paste0("AUDIT [layout ", lay,
                  "]: declares a requirement but the pool has no network failing it"))
    }
    for (fix in cand$ok) {
      net <- ag_layout_pool[[fix]]
      res <- run_or_skip(
        layout_substituted(do.call(graphr,
          c(list(net, layout = lay), layout_extra_args(lay, net)))),
        paste0("layout ", lay), fix)
      run_or_skip({
        expect_buildable(res$plot)
        # It applies, so it must be the layout actually drawn
        testthat::expect_false(res$substituted,
          info = paste0(lay, " x ", fix, ": ", paste(res$msgs, collapse = " ")))
      }, paste0("build ", lay), fix)
    }
  }
})

test_that("every layout substitutes and says so where it does not apply", {
  skip_on_cran()
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  reqs <- autograph:::.layout_requirements()
  for (lay in names(reqs)) {
    cand <- layout_candidates(lay)
    for (fix in cand$no) {
      net <- ag_layout_pool[[fix]]
      res <- run_or_skip(
        layout_substituted(do.call(graphr,
          c(list(net, layout = lay), layout_extra_args(lay, net)))),
        paste0("inapplicable ", lay), fix)
      run_or_skip({
        # Still draws something usable, and explains the swap
        expect_buildable(res$plot)
        testthat::expect_true(res$substituted,
          info = paste0(lay, " x ", fix, " should have substituted"))
        testthat::expect_match(paste(res$msgs, collapse = " "), lay, fixed = TRUE)
      }, paste0("inapplicable build ", lay), fix)
    }
  }
})

test_that("every exported layout_* alias returns usable coordinates", {
  skip_on_cran()
  # The audits above go through graphr(); the user-facing layout_* aliases can
  # also be called directly, so check they return coordinates for every node.
  aliases <- grep("^layout_tbl_graph_", ag_alive_functions("^layout_"),
                  value = TRUE, invert = TRUE)
  expect_true(length(aliases) > 0)
  for (fn in aliases) {
    lay <- sub("^layout_", "", fn)
    fix <- layout_candidates(lay, n_ok = 1)$ok
    if (length(fix) == 0) {
      fail(paste0("AUDIT [alias ", fn, "]: no applicable network in the pool"))
      next
    }
    net <- ag_layout_pool[[fix]]
    coords <- run_or_skip(
      do.call(get(fn, envir = asNamespace("autograph")),
              c(list(net), layout_extra_args(lay, net))),
      paste0("alias ", fn), fix)
    run_or_skip({
      coords <- as.data.frame(coords)
      testthat::expect_true(all(c("x", "y") %in% names(coords)))
      testthat::expect_equal(nrow(coords),
                             as.integer(manynet::net_nodes(net)))
      # No NA coordinates: they survive to draw time and fail there
      testthat::expect_true(all(is.finite(coords$x)) && all(is.finite(coords$y)))
    }, paste0("alias coords ", fn), fix)
  }
})

test_that("layered layout accepts a raw edgelist and returns coordinates", {
  ties <- data.frame(
    from = c("A", "A", "B", "C", "D", "F", "F", "E"),
    to   = c("B", "C", "D", "E", "E", "E", "G", "G"),
    stringsAsFactors = FALSE)
  coords <- layout_tbl_graph_layered(ties, times = 6)
  expect_equal(sort(rownames(coords)), sort(unique(c(ties$from, ties$to))))
  expect_true(all(c("x", "y") %in% names(coords)))
  # sources sit above sinks
  expect_true(coords["A", "y"] > coords["G", "y"])
})

test_that("matching layout aligns matched partners vertically", {
  skip_on_cran()
  coords <- layout_tbl_graph_matching(manynet::ison_southern_women)
  expect_true(all(c("x", "y") %in% names(coords)))
  expect_true(nrow(coords) ==
                manynet::net_nodes(manynet::ison_southern_women))
})

test_that("snapping a layout to the grid yields integer-ish unique positions", {
  skip_on_cran()
  p <- graphr(manynet::ison_adolescents, snap = TRUE)
  expect_buildable(p)
  # depth_first_recursive_search() assigns each node its own grid point
  expect_false(any(duplicated(p$data[, c("x", "y")])))
})

test_that("lattice networks snap by rotation to align edges to the grid", {
  skip_on_cran()
  p <- suppressMessages(graphr(manynet::create_lattice(9), snap = TRUE))
  expect_buildable(p)
  expect_true(all(p$data$x == round(p$data$x)))
})

test_that("snapping a two-mode (hierarchy) layout falls back gracefully", {
  skip_on_cran()
  # The default two-mode layout is "hierarchy", whose layered coordinates
  # would be collapsed by square-grid snapping, so snapping is skipped and
  # the original coordinates are retained (see graph_layout()).
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_message(
    graphr(manynet::ison_southern_women, snap = TRUE),
    "hierarchy")
  snapped <- suppressMessages(graphr(manynet::ison_southern_women, snap = TRUE))
  plain   <- graphr(manynet::ison_southern_women)
  expect_buildable(snapped)
  expect_equal(snapped$data[, c("x", "y")], plain$data[, c("x", "y")])
})

test_that("snapping still works on a two-mode network with a force layout", {
  skip_on_cran()
  p <- suppressMessages(
    graphr(manynet::ison_southern_women, layout = "stress", snap = TRUE))
  expect_buildable(p)
  # every node lands on its own grid point
  expect_false(any(duplicated(p$data[, c("x", "y")])))
})

test_that("snapping returns coordinates in the original node order (>= 10 nodes)", {
  skip_on_cran()
  # Regression: depth_first_recursive_search() sorts nodes by centroid distance
  # internally, then must restore the input node order before returning, because
  # graph_layout() assigns the result positionally. Ordering the row names
  # lexicographically ("1","10","11",...,"2") scrambled coordinates across nodes
  # for any network with 10+ nodes; they must be ordered numerically.
  lo <- ggraph::create_layout(manynet::as_tidygraph(manynet::fict_lotr),
                              "stress")
  expect_true(nrow(lo) >= 10)
  out <- depth_first_recursive_search(lo)
  # returned rows line up with the input nodes, not a lexicographic shuffle
  expect_identical(rownames(out), as.character(seq_len(nrow(out))))
  # snapped positions track the pre-snap layout rather than being permuted
  expect_gt(stats::cor(lo$x, out$x), 0.5)
  expect_gt(stats::cor(lo$y, out$y), 0.5)
})
