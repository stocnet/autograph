test_that("match colors works", {
  expect_equal(match_color("#4575b4", c("blue","red")), "blue")
})

test_that("is dark works", {
  expect_equal(unname(is_dark(c("#000","#FFF"))), c(TRUE, FALSE))
})
