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

test_that("plot() of a mobility draws one point for each move and the start", {
  skip_if_not(exists("as_mobility", asNamespace("manynet")),
              "manynet has no as_mobility()")
  moved <- mobility_fixture()
  model <- manynet::as_mobility(moved)
  expect_no_warning(p <- plot(moved))
  expect_s3_class(p, "ggplot")
  points <- ggplot2::ggplot_build(p)$data[[2]]
  expect_equal(nrow(points), nrow(model) + 1)
  expect_equal(points$y[1], sum(!moved$nodes$satisfied, na.rm = TRUE))
  expect_equal(points$y[-1], model$dissatisfied)
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
