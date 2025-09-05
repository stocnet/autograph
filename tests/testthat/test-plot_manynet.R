res <- manynet::node_deg(ison_dolphins)

test_that("plotting degree distributions are the same", {
  # expect_snapshot_file(ggsave("degdist.png", plot(res)), "degdist.png")
})

test_that("plotting node measures works", {
  p <- plot(manynet::node_deg(ison_dolphins))
  expect_s3_class(p, "ggplot")  
})

test_that("plotting tie measures works", {
  p <- plot(manynet::tie_betweenness(ison_dolphins))
  expect_s3_class(p, "ggplot")  
})
