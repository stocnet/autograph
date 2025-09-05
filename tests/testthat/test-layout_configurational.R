test_that("layout dyad works", {
  res <- layout_dyad(create_explicit(A,B))
  expect_equal(res, data.frame(x = c(0, 1),
                               y = c(0, 0)) )
})

test_that("layout triad works", {
  res <- layout_triad(create_explicit(A,B,C))
  expect_equal(res, data.frame(x = c(0, 2, 4),
                               y = c(0, 3.5, 0) ) )
})

test_that("layout tetrad works", {
  res <- layout_tetrad(create_explicit(A,B,C,D))
  expect_equal(res, data.frame(x = c(0,0,1,1),
                               y = c(0,1,0,1)) )
})

test_that("layout pentad works", {
  res <- layout_pentad(create_explicit(A,B,C,D,E))
  expect_equal(res, data.frame(x = c(0, -0.9511, -0.5878, 0.5878, 0.9511),
                               y = c(1, 0.3090, -0.8090, -0.8090, 0.3090)) )
})

test_that("layout hexad works", {
  res <- layout_hexad(create_explicit(A,B,C,D,E,F))
  expect_equal(res, data.frame(x = c(1, 0.5, -0.5, -1, -0.5, 0.5),
                  y = c(0, sqrt(3)/2, sqrt(3)/2, 0, -sqrt(3)/2, -sqrt(3)/2)) )
})
