# The aesthetic checks are geometry, so they are tested on drawings whose
# geometry is known. graphr() routes `x` and `y` to the manual layout (see
# graph_layout.R), which places the nodes exactly where these tests put them.

square <- function(net) graphr(net, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))

# Crossings ----

test_that("check_crossings() counts a proper crossing and no other", {
  skip_on_cran()
  # A four-cycle on the corners of a square draws its ties along the sides,
  # which meet only at their ends.
  cycle <- square(manynet::create_ring(4))
  expect_equal(attr(check_crossings(cycle), "total"), 0)
  expect_false(attr(check_crossings(cycle), "sampled"))
  # Adding both diagonals adds exactly one crossing, in the middle.
  k4 <- square(manynet::create_explicit(A-B, B-C, C-D, D-A, A-C, B-D))
  crossed <- check_crossings(k4)
  expect_equal(attr(crossed, "total"), 1)
  # The two ties that cross are the two diagonals, and each counts the other
  expect_equal(sum(crossed == 1), 2L)
})

test_that("check_crossings() samples above its cap", {
  skip_on_cran()
  sw <- graphr(manynet::ison_southern_women, layout = "circle")
  full <- check_crossings(sw)
  part <- check_crossings(sw, max_full = 20L)
  expect_false(attr(full, "sampled"))
  expect_true(attr(part, "sampled"))
  # The sample scores fewer ties, so it cannot find more crossings
  expect_lt(attr(part, "total"), attr(full, "total"))
  expect_true(any(is.na(part)))
})

# Slopes, lengths, and angles ----

test_that("check_slopes() counts the slopes a square draws", {
  skip_on_cran()
  cycle <- square(manynet::create_ring(4))
  slopes <- check_slopes(cycle)
  # Two sides run flat and two run upright
  expect_equal(attr(slopes, "distinct"), 2L)
  expect_setequal(round(slopes), c(0, 90))
  # A coarser tolerance cannot find more slopes than a finer one
  expect_lte(attr(check_slopes(cycle, tolerance = 45), "distinct"),
             attr(slopes, "distinct"))
})

test_that("check_lengths() reports equal sides as no variance", {
  skip_on_cran()
  lengths <- check_lengths(square(manynet::create_ring(4)))
  expect_equal(attr(lengths, "variance"), 0)
  expect_equal(attr(lengths, "cv"), 0)
  # Each side is one, and the diagonal of the drawing is the square root of two
  expect_equal(attr(lengths, "max"), 1/sqrt(2))
  expect_equal(attr(lengths, "total"), 4/sqrt(2))
})

test_that("check_angles() reports the resolution and what it could be", {
  skip_on_cran()
  # Two ties meet at each corner of a square, at a right angle
  angles <- check_angles(square(manynet::create_ring(4)))
  expect_equal(attr(angles, "min"), 90)
  expect_equal(attr(angles, "ideal"), 180)
  # A star drawn as a star spaces its ties as evenly as the degree allows
  star <- check_angles(graphr(manynet::create_star(5), layout = "star"))
  expect_equal(attr(star, "min"), attr(star, "ideal"))
  # A node of one tie has no angle to report, and the centre of the star does
  expect_equal(sum(!is.na(star)), 1L)
})

# Loops, and plots that cannot be checked ----

test_that("the checks leave a loop out", {
  skip_on_cran()
  # A loop joins a node to itself, so it has neither slope nor length.
  looped <- graphr(manynet::create_explicit(A-A, A-B, B-C))
  expect_true(is.na(check_lengths(looped)[1]))
  expect_true(is.na(check_slopes(looped)[1]))
  expect_true(is.na(check_crossings(looped)[1]))
  # It is still one of the ties, so the scores line up with the network
  expect_length(check_lengths(looped), 3L)
})

test_that("the checks refuse a plot they cannot read", {
  skip_on_cran()
  plain <- ggplot2::ggplot(data.frame(a = 1:2, b = 1:2))
  for (fn in list(check_crossings, check_slopes, check_lengths, check_angles,
                  check_drawing))
    expect_error(fn(plain), "node coordinates")
})

# The summary ----

test_that("check_drawing() gathers the checks into one row", {
  skip_on_cran()
  sw <- manynet::ison_southern_women
  drawn <- check_drawing(graphr(sw, layout = "circle"))
  expect_s3_class(drawn, "check_drawing")
  expect_equal(nrow(drawn), 1L)
  expect_equal(drawn$nodes, as.integer(manynet::net_nodes(sw)))
  expect_equal(drawn$ties, as.integer(manynet::net_ties(sw)))
  # Each column repeats the headline number of its own check
  expect_equal(drawn$stress, as.numeric(check_stress(graphr(sw, layout = "circle"))))
  # The ceiling travels with the score it is read against
  expect_equal(drawn$angle_ideal,
               attr(check_angles(graphr(sw, layout = "circle")), "ideal"))
  expect_output(print(drawn), "crossings")
  # A row of numbers says nothing about which way each column is read
  expect_output(print(drawn), "angle_min, where higher is better")
  # Two layouts stack, which is how two layouts are compared
  both <- rbind(circle = drawn,
                stress = check_drawing(graphr(sw, layout = "stress")))
  expect_equal(nrow(both), 2L)
  # A scaling layout draws the path distances better than a circle does
  expect_lt(both["stress", "stress"], both["circle", "stress"])
})
