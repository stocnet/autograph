# Completion of argument values (R/graph_completion.R).
#
# The parsing and candidate functions are tested directly, since they know
# nothing about RStudio. The hook is tested against a stand-in for RStudio's
# `tools:rstudio` environment: pressing Tab itself cannot be tested here.

# Reading the line ----

test_that("the line is read for the call, argument and value being typed", {
  ctx <- autograph:::.completion_context('graphr(fict_lotr, node_color = "Ra')
  expect_equal(ctx$fun, "graphr")
  expect_equal(ctx$arg, "node_color")
  expect_equal(ctx$token, "Ra")
  expect_true(ctx$quoted)
  expect_equal(ctx$data, "fict_lotr")
})

test_that("a value without quotes, and one still empty, are both read", {
  ctx <- autograph:::.completion_context("graphr(fict_lotr, node_color = ")
  expect_equal(ctx$arg, "node_color")
  expect_equal(ctx$token, "")
  expect_false(ctx$quoted)
  ctx <- autograph:::.completion_context('graphr(fict_lotr, layout = "')
  expect_equal(ctx$arg, "layout")
  expect_equal(ctx$token, "")
})

test_that("an argument given by position is matched to the right formal", {
  # The second formal of graphr() is `layout`, and the first is taken by the
  # network, so an unnamed second value is a layout.
  ctx <- autograph:::.completion_context('graphr(fict_lotr, "st')
  expect_equal(ctx$arg, "layout")
  # A formal already given by name is not offered again for a position.
  ctx <- autograph:::.completion_context('graphr(fict_lotr, layout = "fr", "')
  expect_equal(ctx$arg, "labels")
})

test_that("a value inside c() belongs to the argument c() is given to", {
  ctx <- autograph:::.completion_context('graphr(fict_lotr, labels = c("Fro')
  expect_equal(ctx$arg, "labels")
  expect_equal(ctx$token, "Fro")
})

test_that("lines that are not one of these calls are left alone", {
  expect_null(autograph:::.completion_context("mean(x, na.rm = "))
  expect_null(autograph:::.completion_context('graphr(fict_lotr, node_size = fn("'))
  expect_null(autograph:::.completion_context("graphr(fict_lotr)"))
  expect_null(autograph:::.completion_context(""))
  expect_null(autograph:::.completion_context(NULL))
})

test_that("a nested call does not confuse the commas", {
  ctx <- autograph:::.completion_context('plot(graphr(fict_lotr, node_color = "R')
  expect_equal(ctx$fun, "graphr")
  expect_equal(ctx$arg, "node_color")
  ctx <- autograph:::.completion_context('graphr(fict_lotr[1:2, ], node_color = "')
  expect_equal(ctx$arg, "node_color")
})

# Candidate values ----

# The values are a data frame; `values()` reads just the column of values.
values <- function(...) autograph:::.completion_values(...)$value

test_that("each argument offers the values it accepts", {
  g <- manynet::as_igraph(manynet::fict_lotr)
  expect_true("Race" %in% values("node_color", g))
  expect_true("Race" %in% values("node_colour", g))
  expect_true("Race" %in% values("node_size", g))
  # Node attributes come before the literal values an argument also takes.
  shapes <- values("node_shape", g)
  expect_equal(shapes[1], "Race")
  expect_true("circle" %in% shapes)
  # Node labels are not variables to map an aesthetic to.
  expect_false("name" %in% values("node_color", g))
})

test_that("layouts, themes and labels offer their own sets", {
  g <- manynet::as_igraph(manynet::fict_lotr)
  layouts <- values("layout", g)
  expect_true(all(autograph:::.autograph_layouts() %in% layouts))
  # autograph's own layouts come first, being the ones not documented elsewhere.
  expect_equal(layouts[seq_along(autograph:::.autograph_layouts())],
               autograph:::.autograph_layouts())
  expect_equal(values("theme"), autograph:::theme_opts)
  labels <- values("labels", g)
  expect_true(all(autograph:::.label_criteria() %in% labels))
  expect_true("Frodo" %in% labels)
})

test_that("an argument whose default is a set of choices offers them", {
  expect_equal(values("isolates", NULL, "graphr"), c("legend", "caption", "keep"))
  expect_equal(values("isolates", NULL, "grapht"), c("keep", "fade"))
  expect_equal(values("based_on", NULL, "graphs"), c("first", "last", "both"))
})

test_that("an argument with no known set offers nothing", {
  g <- manynet::as_igraph(manynet::fict_lotr)
  expect_equal(nrow(autograph:::.completion_values("snap", g)), 0L)
  expect_equal(nrow(autograph:::.completion_values("", g)), 0L)
  # Without a network there are no attributes to offer.
  expect_equal(nrow(autograph:::.completion_values("node_color", NULL)), 0L)
})

# What each value is labelled with ----

test_that("a variable is labelled with its kind and a line about its values", {
  g <- manynet::as_igraph(manynet::fict_greys)
  vals <- autograph:::.completion_values("node_color", g)
  expect_equal(vals$label[vals$value == "sex"], "character")
  expect_equal(vals$label[vals$value == "birthyear"], "numeric")
  # Few enough categories to read at a glance are named outright.
  expect_equal(vals$meta[vals$value == "sex"], "F, M")
  # More than that are counted instead.
  expect_match(vals$meta[vals$value == "position"], "^[0-9]+ categories$")
  # A number runs over a range.
  expect_equal(vals$meta[vals$value == "birthyear"], "1944 to 1987")
})

test_that("values that are not variables carry a label of their own", {
  g <- manynet::as_igraph(manynet::fict_lotr)
  shapes <- autograph:::.completion_values("node_shape", g)
  expect_equal(shapes$label[shapes$value == "circle"], "shape")
  themes <- autograph:::.completion_values("theme")
  expect_true(all(themes$label == "theme"))
  labels <- autograph:::.completion_values("labels", g)
  expect_equal(labels$label[labels$value == "degree"], "measure")
  expect_equal(labels$label[labels$value == "Frodo"], "node")
  isolates <- autograph:::.completion_values("isolates", NULL, "graphr")
  expect_true(all(isolates$label == "option"))
})

test_that("a layout is labelled with the package that draws it", {
  layouts <- autograph:::.completion_values("layout")
  expect_true(all(layouts$label[layouts$value %in% autograph:::.autograph_layouts()] ==
                    "autograph"))
  expect_equal(layouts$label[layouts$value == "circle"], "igraph")
  expect_true("ggraph" %in% layouts$label)
})

test_that("a mark is labelled as one, and only marks are offered for labels", {
  g <- manynet::as_igraph(manynet::fict_lotr)
  g <- igraph::set_vertex_attr(g, "is_hobbit",
                               value = manynet::node_labels(g) %in% c("Frodo", "Sam"))
  vals <- autograph:::.completion_values("labels", g)
  expect_equal(vals$label[vals$value == "is_hobbit"], "mark")
  expect_equal(vals$meta[vals$value == "is_hobbit"],
               paste("2 of", manynet::net_nodes(g)))
  # `Race` is a variable rather than a selection of nodes, so it is not offered.
  expect_false("Race" %in% autograph:::.completion_marks(g))
  expect_true("is_hobbit" %in% autograph:::.completion_marks(g))
})

# Finding the network ----

test_that("only a symbol is looked up, and never a call", {
  net <- manynet::fict_lotr
  expect_s3_class(autograph:::.completion_object("net", environment()), "igraph")
  expect_null(autograph:::.completion_object("to_undirected(net)", environment()))
  expect_null(autograph:::.completion_object("nosuchobject", environment()))
  expect_null(autograph:::.completion_object("", environment()))
  # An object that is not a network is not one to complete from.
  notanet <- 1:5
  expect_null(autograph:::.completion_object("notanet", environment()))
})

# What is offered ----

test_that("what is offered narrows to what has been typed", {
  fict_lotr <- manynet::fict_lotr
  out <- autograph:::.completion_suggest('graphr(fict_lotr, node_color = "',
                                         environment())
  expect_equal(out$values$value, "Race")
  expect_true(out$quoted)
  out <- autograph:::.completion_suggest('graphr(fict_lotr, node_color = "Ra',
                                         environment())
  expect_equal(out$token, "Ra")
  expect_equal(out$values$value, "Race")
  # Matching ignores case, as .match_name() does when the value is given.
  out <- autograph:::.completion_suggest('graphr(fict_lotr, node_color = "ra',
                                         environment())
  expect_equal(out$values$value, "Race")
  # Nothing to offer, rather than an empty list of completions.
  expect_null(autograph:::.completion_suggest('graphr(fict_lotr, node_color = "zz',
                                              environment()))
  expect_null(autograph:::.completion_suggest("mean(x, na.rm = ", environment()))
})

# The RStudio hook ----

# A stand-in for the three things this uses from RStudio, attached under the
# name RStudio gives its own environment.
with_fake_rstudio <- function(code) {
  fake <- list(
    .rs.rpc.get_completions = function(token, contextData, line, isConsole)
      "rstudio's own",
    .rs.makeCompletions = function(token, results, packages, meta, quote, type,
                                   excludeOtherCompletions)
      list(token = token, results = results, packages = packages, meta = meta,
           quote = quote, type = type),
    .rs.acCompletionTypes = list(COLUMN = 27, STRING = 20))
  suppressWarnings(attach(fake, name = "tools:rstudio", warn.conflicts = FALSE))
  on.exit(detach("tools:rstudio"), add = TRUE)
  force(code)
}

test_that("activation replaces RStudio's function and deactivation restores it", {
  with_fake_rstudio({
    env <- as.environment("tools:rstudio")
    original <- get(".rs.rpc.get_completions", envir = env)
    expect_false(autograph:::.completion_active())
    expect_true(autograph:::.completion_activate())
    expect_true(autograph:::.completion_active())
    expect_false(identical(get(".rs.rpc.get_completions", envir = env), original))
    # Formals are taken from the version installed, so every argument RStudio
    # passes still reaches the original.
    expect_equal(names(formals(get(".rs.rpc.get_completions", envir = env))),
                 names(formals(original)))
    # Activating twice is not an error, and does not wrap the wrapper.
    expect_true(autograph:::.completion_activate())
    expect_true(autograph:::.completion_deactivate())
    expect_false(autograph:::.completion_active())
    expect_identical(get(".rs.rpc.get_completions", envir = env), original)
  })
})

test_that("a line this does not recognise is passed to RStudio untouched", {
  with_fake_rstudio({
    env <- as.environment("tools:rstudio")
    autograph:::.completion_activate()
    on.exit(autograph:::.completion_deactivate(), add = TRUE)
    completions <- get(".rs.rpc.get_completions", envir = env)
    expect_equal(completions("", list(), "mean(x, na.rm = ", TRUE),
                 "rstudio's own")
    # An argument that is missing is not forced, and a line of the wrong shape
    # is RStudio's business too.
    expect_equal(completions("", list(), character(), TRUE), "rstudio's own")
  })
})

test_that("a recognised line is answered with the values available", {
  # `fict_lotr` is found on the search path, as it would be for a user who has
  # attached manynet.
  with_fake_rstudio({
    env <- as.environment("tools:rstudio")
    autograph:::.completion_activate()
    on.exit(autograph:::.completion_deactivate(), add = TRUE)
    completions <- get(".rs.rpc.get_completions", envir = env)
    out <- completions("", list(), 'graphr(fict_lotr, node_color = "', TRUE)
    expect_equal(out$results, "Race")
    # The kind of variable is shown in brackets beside it, and its values after.
    expect_equal(out$packages, "character")
    expect_match(out$meta, "categories$")
    # Already inside quotes, so the value is inserted without adding more.
    expect_false(out$quote)
    out <- completions("", list(), "graphr(fict_lotr, node_color = ", TRUE)
    expect_true(out$quote)
  })
})

test_that("a broken RStudio function leaves completion working", {
  with_fake_rstudio({
    env <- as.environment("tools:rstudio")
    # Whatever changes in RStudio, an error here must not stop completion.
    assign(".rs.makeCompletions", function(...) stop("changed"), envir = env)
    autograph:::.completion_activate()
    on.exit(autograph:::.completion_deactivate(), add = TRUE)
    completions <- get(".rs.rpc.get_completions", envir = env)
    expect_equal(completions("", list(), 'graphr(fict_lotr, node_color = "', TRUE),
                 "rstudio's own")
  })
})

test_that("outside RStudio nothing is changed", {
  expect_null(autograph:::.completion_env())
  expect_false(autograph:::.completion_active())
  expect_false(autograph:::.completion_activate())
  expect_false(autograph:::.completion_deactivate())
  expect_false(suppressMessages(stocnet_completion(TRUE)))
  expect_false(suppressMessages(stocnet_completion()))
})

test_that("the completion preference is written and forgotten on request", {
  # R_user_dir() is redirected so the test never touches the real config.
  tmp <- tempfile("agconfig")
  dir.create(tmp)
  old <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA)
  Sys.setenv(R_USER_CONFIG_DIR = tmp)
  on.exit({
    if (is.na(old)) Sys.unsetenv("R_USER_CONFIG_DIR")
    else Sys.setenv(R_USER_CONFIG_DIR = old)
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  expect_true(autograph:::write_pref("completion", TRUE))
  expect_true(autograph:::read_pref("completion"))
  autograph:::forget_pref("completion")
  expect_null(autograph:::read_pref("completion"))
})
