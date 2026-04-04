test_that("plotting degree distributions are the same", {
  # expect_snapshot_file(ggsave("degdist.png", plot(res)), "degdist.png")
})

test_that("plotting node measures works", {
  p <- plot(netrics::node_by_deg(ison_dolphins))
  expect_s3_class(p, "ggplot")  
})

test_that("plotting tie measures works", {
  p <- plot(netrics::tie_by_betweenness(ison_dolphins))
  expect_s3_class(p, "ggplot")  
})
