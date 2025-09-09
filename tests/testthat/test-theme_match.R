test_that("match colors works", {
  expect_equal(match_color("#4575b4", c("blue","red")), "blue")
})

test_that("is dark works", {
  expect_equal(unname(is_dark(c("#000","#FFF"))), c(TRUE, FALSE))
})

test_that("Single color input returns correct match", {
  res <- match_color("#4575b4", c("#006564", "#0094D8", "#622550"))
  expect_length(res, 1)
  expect_true(res %in% c("#006564", "#0094D8", "#622550"))
})

test_that("Multiple color input returns correct number of unique matches", {
  colors <- c("#4575b4", "#d73027", "#1a9850")
  palette <- c("#006564", "#0094D8", "#622550", "#268D2B", "#820C2B")
  res <- match_color(colors, palette)
  expect_length(res, length(colors))
  expect_true(all(res %in% palette))
  expect_equal(length(unique(res)), length(res))
})

test_that("Throws error when input colors exceed palette length", {
  expect_error(
    match_color(c("#4575b4", "#d73027", "#1a9850", "#ffcc00"),
                c("#006564", "#0094D8", "#622550")),
    "Not enough unique colors"
  )
})

test_that("Identical input colors receive different palette matches", {
  res <- match_color(c("#4575b4", "#4575b4"), c("#006564", "#0094D8", "#622550"))
  expect_length(res, 2)
  expect_equal(length(unique(res)), 2)
})

test_that("Handles single palette color with single input", {
  res <- match_color("#ff0000", "#00ff00")
  expect_equal(res, "#00ff00")
})

test_that("Handles input equal to palette size", {
  input <- rep("#000000", 5)
  palette <- c("#111111", "#222222", "#333333", "#444444", "#555555")
  res <- match_color(input, palette)
  expect_length(res, 5)
  expect_equal(length(unique(res)), 5)
})
