# test_that("setting theme offers info if empty", {
#   expect_message(stocnet_theme(), "is currently set to")
#   # expect_message(stocnet_theme(), "following themes are available")
# })

test_that("setting theme provides correct palette", {
  stocnet_theme("default")
  expect_equal(getOption("snet_cat"), c("#1B9E77","#4575b4","#d73027",
                                        "#66A61E","#E6AB02","#D95F02","#7570B3",
                                        "#A6761D","#E7298A","#666666"))
  stocnet_theme("iheid")
  expect_equal(getOption("snet_cat"), c("#006564","#0094D8","#622550",
                                        "#268D2B","#3E2682","#820C2B",
                                        "#008F92","#006EAA","#A8086E"))
  stocnet_theme("ethz")
  expect_equal(getOption("snet_cat"), c("#215CAF","#007894","#627313",
                                        "#8E6713","#B7352D","#A7117A","#6F6F6F"))
})

test_that("setting fonts works", {
  stocnet_theme("iheid")
  p <- plot(monan_gof)
  expect_equal(p$theme$text$family, "Helvetica")
  skip_on_os("windows") # Arial not available on Windows by default
  stocnet_theme("ethz")
  p <- plot(monan_gof)
  expect_equal(p$theme$text$family, "Arial")
})
