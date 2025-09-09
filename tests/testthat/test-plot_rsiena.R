# load(test_path("testdata", "gofi.RData"))
# 
# test_that("GOF plot is the same", {
#   # expect_snapshot_file(ggsave("gofi.png", plot(gofi)), "gofi.png")
# })

# test_that("plotting degree distributions are the same", {
#   expect_snapshot_file(ggsave("degdist.png", plot(res)), "degdist.png")
# })

test_that("siena selection table plotting works",{
  p <- plot(siena_selection)
  expect_s3_class(p, "ggplot")
})

test_that("siena influence table plotting works",{
  p <- plot(siena_influence)
  expect_s3_class(p, "ggplot")
})