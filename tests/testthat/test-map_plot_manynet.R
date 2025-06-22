res <- manynet::node_deg(ison_dolphins)

test_that("plotting degree distributions are the same", {
  # expect_snapshot_file(ggsave("degdist.png", plot(res)), "degdist.png")
})
