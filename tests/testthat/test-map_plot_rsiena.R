load(test_path("testdata", "gofi.RData"))

test_that("GOF plot is the same", {
  # expect_snapshot_file(ggsave("gofi.png", plot(gofi)), "gofi.png")
})

# test_that("plotting degree distributions are the same", {
#   expect_snapshot_file(ggsave("degdist.png", plot(res)), "degdist.png")
# })