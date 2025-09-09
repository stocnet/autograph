test_that("valence layout works", {
  edges <- data.frame(from = c("A", "B"),
                      to   = c("B", "C"),
                      weight = c(3, 3),
                      sign = c(1, -1))  # 1 = positive, -1 = negative
  lo <- layout_valence(edges, times = 500)
  expect_true(sqrt(sum((lo[1,] - lo[2,])^2)) < sqrt(sum((lo[1,] - lo[3,])^2)))
})
