# A diffusion in values of its own needs a manynet that plays one.
skip_if_no_compartments <- function() {
  testthat::skip_if_not("compartments" %in%
                          names(formals(manynet::play_diffusion)),
                        "manynet::play_diffusion() has no `compartments`")
}

line_labels <- function(p) {
  built <- ggplot2::ggplot_build(p)
  built$plot$scales$get_scales("colour")$get_labels()
}

test_that("a diffusion in S, I, and R is named and coloured by its roles", {
  skip_on_cran()
  set.seed(1)
  sir <- manynet::play_diffusion(manynet::create_ring(20), recovery = 0.3)
  report <- manynet::as_diffusion(sir)
  expect_no_warning(p <- plot(report))
  expect_buildable(p)
  expect_equal(line_labels(p), c("Susceptible", "Infected", "Recovered"))
  expect_equal(p$scales$get_scales("colour")$name, "Legend")
  # The precooked report, from before the compartments were kept
  expect_no_warning(p <- plot(autograph::res_manynet_diff))
  expect_equal(line_labels(p), c("Susceptible", "Infected"))
})

test_that("a diffusion with fatality draws the dead in black", {
  skip_on_cran()
  testthat::skip_if_not("fatality" %in% names(formals(manynet::play_diffusion)),
                        "manynet::play_diffusion() has no `fatality`")
  set.seed(1)
  sird <- manynet::play_diffusion(manynet::ison_networkers, seeds = 1:3,
                                  recovery = 0.1, fatality = 0.1, steps = 30)
  skip_if_not(max(manynet::as_diffusion(sird)$D) > 0,
              "manynet::as_diffusion() does not count the dead in D")
  expect_no_warning(p <- plot(sird))
  expect_buildable(p)
  expect_equal(line_labels(p),
               c("Susceptible", "Infected", "Recovered", "Dead"))
  cols <- ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")$palette.cache
  expect_equal(unname(cols[4]), "#000000")
})

test_that("a diffusion with a value of its own for D names it", {
  skip_on_cran()
  skip_if_no_compartments()
  testthat::skip_if_not("fatality" %in% names(formals(manynet::play_diffusion)),
                        "manynet::play_diffusion() has no `fatality`")
  set.seed(2)
  y <- manynet::play_diffusion(manynet::ison_networkers, seeds = 1:3,
                               recovery = 0.1, fatality = 0.1, steps = 30,
                               compartments = c(S = "healthy", I = "sick",
                                                R = "immune", D = "deceased"))
  skip_if_not(max(manynet::as_diffusion(y)$deceased) > 0,
              "manynet::as_diffusion() does not count the dead in D")
  expect_no_warning(p <- plot(y))
  expect_buildable(p)
  expect_true("deceased" %in% line_labels(p))
})

test_that("a diffusion in values of its own is named by its values", {
  skip_on_cran()
  skip_if_no_compartments()
  set.seed(1)
  law <- manynet::play_diffusion(manynet::ison_lawfirm, "status", steps = 3,
                                 transmissibility = 0.2,
                                 compartments = c(S = "associate",
                                                  I = "partner"))
  expect_no_warning(p <- plot(law))
  expect_buildable(p)
  expect_equal(line_labels(p), c("associate", "partner"))
  expect_equal(p$scales$get_scales("colour")$name, "status")
  # the bars count the new partners
  expect_s3_class(p$layers[[2]]$geom, "GeomCol")
  cols <- .diffusion_colors(c("associate", "partner"),
                            list(S = "associate", I = "partner"))
  expect_equal(unname(cols), unname(.diffusion_colors(c("S", "I"),
                                                      list(S = "S", I = "I"))))
})

test_that("a diffusion of a logical attribute is named FALSE and TRUE", {
  skip_on_cran()
  skip_if_no_compartments()
  logi <- manynet::mutate_nodes(manynet::create_ring(10),
                                adopted = c(TRUE, rep(FALSE, 9)))
  logi <- manynet::play_diffusion(logi, "adopted", steps = 4)
  expect_equal(.diffusion_attribute(logi), "adopted")
  expect_no_warning(p <- plot(logi))
  expect_equal(line_labels(p), c("FALSE", "TRUE"))
  times <- .node_adoption_time(logi)
  expect_equal(times[1], 0)
  expect_true(all(is.finite(times[-6])))
})

test_that("several values can share a role", {
  skip_on_cran()
  skip_if_no_compartments()
  blogs <- manynet::mutate_nodes(manynet::create_ring(6),
                                 leaning = c("Liberal", "Conservative",
                                             "Moderate", NA, "Moderate",
                                             "Conservative"))
  blogs <- manynet::play_diffusion(blogs, "leaning", steps = 3,
                                   compartments = list(
                                     S = c("Conservative", "Moderate"),
                                     I = "Liberal"))
  expect_no_warning(p <- plot(blogs))
  expect_equal(line_labels(p), c("Conservative", "Moderate", "Liberal"))
  cols <- .diffusion_colors(c("Conservative", "Moderate", "Liberal"),
                            list(S = c("Conservative", "Moderate"),
                                 I = "Liberal"))
  expect_equal(length(unique(cols)), 3)
  # the Liberal node adopts at the start
  expect_equal(.node_adoption_time(blogs), c(0, 1, 2, 3, 2, 1))
  expect_buildable(graphr(blogs))
})

test_that("a report of a single step warns and draws nothing", {
  report <- autograph::res_manynet_diff[1, ]
  expect_warning(out <- plot(report), "No diffusion was observed")
  expect_null(out)
  attr(report, "attribute") <- "status"
  expect_warning(plot(report), "No change in")
})

test_that("graphs() shows one legend for a diffusion over its waves", {
  skip_on_cran()
  set.seed(1)
  sir <- manynet::play_diffusion(manynet::create_ring(20), recovery = 0.3)
  p <- graphs(sir, node_color = "diffusion")
  built <- patchwork::patchworkGrob(p)
  guides <- grep("guide-box", vapply(built$grobs, function(g) g$name,
                                     character(1)))
  expect_length(guides, 1)
})

# Mobility ####

mobility_fixture <- function() {
  set.seed(1)
  group <- stats::rbinom(25, 1, 0.5)
  group[sample(25, 5)] <- NA
  places <- manynet::add_node_attribute(manynet::create_lattice(25),
                                        "group", group)
  manynet::play_mobility(places, "group", 0.2)
}

test_that("plot() of a mobility draws the places by step, and a margin", {
  skip_if_not(exists("as_mobility", asNamespace("manynet")),
              "manynet has no as_mobility()")
  moved <- mobility_fixture()
  model <- manynet::as_mobility(moved)
  expect_no_warning(p <- plot(moved))
  expect_s3_class(p, "patchwork")
  # The grid: a tile for each place at each step, and an outline for each move
  grid <- ggplot2::ggplot_build(p[[1]])$data
  expect_equal(nrow(grid[[1]]),
               (max(model$time) + 1) * manynet::net_nodes(moved))
  expect_equal(nrow(grid[[3]]), nrow(model))
  expect_equal(-grid[[3]]$y, model$time)
  # The margin: the dissatisfied at the start, and after each move
  bars <- ggplot2::ggplot_build(p[[2]])$data[[1]]
  expect_equal(nrow(bars), nrow(model) + 1)
  expect_equal(bars$xmax[1], sum(!moved$nodes$satisfied, na.rm = TRUE))
  expect_equal(bars$xmax[-1], model$dissatisfied)
  # The crosses are the dissatisfied at the start
  expect_equal(sum(-grid[[2]]$y == 0), sum(!moved$nodes$satisfied, na.rm = TRUE))
  # The model alone does not hold the start
  expect_equal(nrow(ggplot2::ggplot_build(plot(model))$data[[2]]), nrow(model))
})

test_that("graphs() of a mobility draws the start, middle, and last move", {
  skip_if_not(exists("as_mobility", asNamespace("manynet")),
              "manynet has no as_mobility()")
  moved <- mobility_fixture()
  last <- max(manynet::as_mobility(moved)$time)
  p <- graphs(moved)
  titles <- vapply(seq_len(length(p)), function(i) p[[i]]$labels$title,
                   character(1))
  expect_equal(titles, paste("Step", c(0, round(last/2), last)))
  # At the start, the places are coloured as the network that is returned,
  # with the empty places blank
  fill <- ggplot2::ggplot_build(p[[1]])$data[[2]]$fill
  group <- moved$nodes$group
  expect_true(all(fill[is.na(group)] == "white"))
  by_group <- tapply(fill[!is.na(group)], group[!is.na(group)], unique)
  expect_true(all(lengths(by_group) == 1))
  expect_false(any(duplicated(unlist(by_group))))
  expect_length(graphs(moved, waves = c(1, 2)), 2)
  expect_error(graphs(moved, waves = 99), "between 1")
})

# Simulated diffusions as `infernet::play_diffusions()` binds them, made here
# so that the test does not need infernet.
diffs_fixture <- function(times = 30) {
  set.seed(1)
  runs <- lapply(seq_len(times), function(j) {
    d <- as.data.frame(manynet::as_diffusion(manynet::play_diffusion(
      manynet::ison_adolescents, seeds = 1, transmissibility = 0.5)))
    names(d)[names(d) == "t"] <- "time"
    data.frame(sim = j, d[, c("time", "S", "I")])
  })
  out <- dplyr::as_tibble(do.call(rbind, runs))
  class(out) <- c("diffs_model", class(out))
  out
}

test_that("plot() of simulated diffusions draws an observed one over a band", {
  skip_on_cran()
  diffs <- diffs_fixture()
  set.seed(2)
  observed <- manynet::play_diffusion(manynet::ison_adolescents, seeds = 1,
                                      transmissibility = 0.5)
  expect_no_warning(p <- plot(diffs, observed = observed))
  built <- expect_buildable(p)
  geoms <- unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
  expect_equal(geoms, c("GeomRibbon", "GeomLine", "GeomLine", "GeomPoint"))
  band <- built$data[[1]]
  median <- built$data[[2]]
  expect_true(all(band$ymin <= median$y & median$y <= band$ymax))
  # The observed curve is the proportion infected, at every step of the band
  report <- manynet::as_diffusion(observed)
  obs <- built$data[[3]]
  expect_equal(nrow(obs), nrow(band))
  expect_equal(obs$y[seq_len(nrow(report))], report$I/8)
  # A report works as well as the network, and `level` narrows the band
  narrow <- ggplot2::ggplot_build(plot(diffs, observed = report, level = 0.5))
  expect_true(all(narrow$data[[1]]$ymax - narrow$data[[1]]$ymin <=
                    band$ymax - band$ymin + 1e-12))
  expect_error(plot(diffs, observed = observed, level = 2), "between 0 and 1")
  expect_error(plot(diffs, observed = diffs), "many diffusions")
})

test_that("a diffusionModel dispatches on its own class", {
  skip_on_cran()
  set.seed(1)
  report <- manynet::as_diffusion(manynet::play_diffusion(
    manynet::ison_adolescents, seeds = 1, transmissibility = 0.5))
  skip_if_not(inherits(report, "diffusionModel"),
              "manynet::as_diffusion() returns no diffusionModel")
  expect_true("plot.diffusionModel" %in% ls(asNamespace("autograph"), all.names = TRUE))
  class(report) <- setdiff(class(report), "diff_model")
  expect_no_warning(p <- plot(report))
  expect_buildable(p)
  expect_equal(line_labels(p), c("Susceptible", "Infected"))
})

opinion_fixture <- function(stubborn = TRUE) {
  set.seed(1)
  net <- manynet::ison_networkers
  if (stubborn) {
    net <- manynet::add_node_attribute(net, "stubbornness", c(1, 0.5, rep(0, 30)))
    manynet::play_opinion(net, opinions = stats::runif(32),
                          stubbornness = "stubbornness")
  } else manynet::play_opinion(net, opinions = stats::runif(32))
}

test_that("plot() of opinions draws a gradient line for each node", {
  skip_on_cran()
  skip_if_not(exists("play_opinion", asNamespace("manynet")),
              "manynet has no play_opinion()")
  model <- manynet::as_opinion(opinion_fixture(stubborn = FALSE))
  expect_no_warning(p <- plot(model))
  built <- expect_buildable(p)
  lines <- built$data[[1]]
  expect_equal(length(unique(lines$group)), ncol(model))
  expect_equal(nrow(lines), nrow(model) * ncol(model))
  expect_s3_class(p$scales$get_scales("colour"), "ScaleContinuous")
  # The run reaches a consensus, which a dashed line marks
  expect_equal(class(p$layers[[2]]$geom)[1], "GeomHline")
  expect_equal(built$data[[2]]$yintercept,
               mean(unlist(model[nrow(model), ])), tolerance = 1e-6)
  # The old class still has its own method, as before
  expect_buildable(plot(structure(model, class = setdiff(class(model),
                                                         "opinionModel"))))
})

test_that("plot() of opinions marks the stubborn nodes", {
  skip_on_cran()
  skip_if_not(exists("play_opinion", asNamespace("manynet")),
              "manynet has no play_opinion()")
  opined <- opinion_fixture()
  expect_no_warning(p <- plot(opined))
  built <- expect_buildable(p)
  geoms <- unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
  # No consensus with stubborn nodes, so no dashed line
  expect_false("GeomHline" %in% geoms)
  expect_equal(sum(geoms == "GeomPoint"), 1)
  expect_equal(nrow(built$data[[which(geoms == "GeomPoint")]]), 2)
  # The model alone marks nobody unless told
  model <- manynet::as_opinion(opined)
  expect_false("GeomPoint" %in% vapply(plot(model)$layers,
                                       function(l) class(l$geom)[1], ""))
  expect_buildable(plot(model, stubbornness = c(1, 0.5, rep(0, 30))))
  expect_error(plot(model, stubbornness = 1:3), "one number or 32")
})

test_that("plot() of a transitions table draws the values and transitions", {
  skip_on_cran()
  set.seed(1)
  net <- manynet::play_diffusion(manynet::ison_adolescents, seeds = 1,
                                 transmissibility = 0.5, latency = 0.4,
                                 recovery = 0.2, waning = 0.05)
  tr <- net$info$transitions
  skip_if_not(inherits(tr, "diffusionTransitions"),
              "manynet keeps no table of transitions")
  expect_no_warning(p <- plot(tr))
  built <- expect_buildable(p)
  # One circle and one label for each value
  expect_equal(length(unique(built$data[[2]]$group)), 4)
  expect_equal(built$data[[3]]$label, c("S", "E", "I", "R"))
  # Four transitions and one contagion, from I
  paths <- built$data[[1]]
  expect_equal(length(unique(paths$group)), 5)
  expect_equal(sort(built$data[[4]]$label),
               sort(c("0.69", "0.92", "0.22", "0.051", "threshold")))
  # The values have the colours of their roles
  fills <- p$scales$get_scales("fill")$palette(4)
  expect_equal(unname(fills[c("S", "I")]),
               unname(.diffusion_colors(c("S", "I"), list(S = "S", I = "I"))))
})

test_that("graphs() colours a diffusion in an attribute of its own", {
  skip_on_cran()
  skip_if_no_compartments()
  set.seed(3)
  net <- manynet::add_node_attribute(manynet::ison_adolescents, "status",
                                     c("aware", rep("unaware", 7)))
  d <- manynet::play_diffusion(net, attribute = "status",
                               compartments = list(S = "unaware", I = "aware"),
                               transmissibility = 0.5)
  p <- graphs(d)
  expect_gt(length(p), 1)
  cols <- .diffusion_colors(c("unaware", "aware"),
                            list(S = "unaware", I = "aware"))
  for (i in seq_len(length(p))) {
    panel <- p[[i]]
    expect_equal(panel$scales$get_scales("fill")$name, "status")
    fill <- ggplot2::ggplot_build(panel)$data
    fill <- unlist(lapply(fill, function(x) x$fill))
    expect_true(all(fill[!is.na(fill)] %in% c(cols, "white", "#FFFFFF")))
  }
  # A given colour, and a network that is not a diffusion, are left alone
  expect_null(.diffusion_fill(manynet::ison_adolescents))
  expect_s3_class(graphs(d, node_color = "status"), "ggplot")
})
