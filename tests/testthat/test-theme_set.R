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
  stocnet_theme("uzh")
  expect_equal(getOption("snet_cat"), c("#0028A5","#4AC9E3","#A4D233",
                                        "#FFC845","#FC4C02","#BF0D3E",
                                        "#BDC9E8","#DBF4F9","#ECF6D6",
                                        "#FFF4DA","#FFDBCC","#FBC6D4",
                                        "#7596FF","#B7E9F4","#DBEDAD",
                                        "#FFE9B5","#FEB799","#F78CAA",
                                        "#3062FF","#92DFEE","#C8E485",
                                        "#FFDE8F","#FE9367","#F3537F",
                                        "#001E7C","#1EA7C4","#7CA023",
                                        "#F3AB00","#BD3902","#8F0A2E",
                                        "#001452","#147082","#536B18",
                                        "#A27200","#7E2601","#60061F"))
  stocnet_theme("unibe")
  expect_equal(getOption("snet_cat"), c("#466553","#668271","#8aa092","#afbfb5","#d6ded9",
                                        "#007ea2","#5294b4","#85adc6","#b0c7d9","#d8e2ec",
                                        "#203a5d","#4a5575","#757792","#a1a0b4","#d0ced9",
                                        "#8a1e22","#a14540","#b86f65","#d19d93","#e8cdc6",
                                        "#5a3217","#754e31","#927157","#b49b87","#d7cac0",
                                        "#36b5b6","#75c4c5","#a0d3d4","#c4e3e3","#e2f1f2",
                                        "#ec627d","#f08797","#f4a9b1","#f8c8cc","#fce4e7",
                                        "#4767af","#6e82c0","#949fd1","#b9bee1","#dcdef1",
                                        "#c2b600","#cfc43c","#dcd274","#e8e1a4","#f4f0d3",
                                        "#ee7402","#f3923e","#f7af70","#fbcba1","#fde6d1"))
  stocnet_theme("rainbow")
  expect_equal(getOption("snet_cat"), c('#E8ECFB', '#D9CCE3', '#D1BBD7', 
                                        '#CAACCB', '#BA8DB4', '#AE76A3', 
                                        '#AA6F9E', '#994F88', '#882E72', 
                                        '#1965B0', '#437DBF', '#5289C7', 
                                        '#6195CF', '#7BAFDE', 
                                        '#4EB265', '#90C987', '#CAE0AB', 
                                        '#F7F056', '#F7CB45', '#F6C141', 
                                        '#F4A736', '#F1932D', '#EE8026', 
                                        '#E8601C', '#E65518', '#DC050C', 
                                        '#A5170E', '#72190E', '#42150A'))
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
