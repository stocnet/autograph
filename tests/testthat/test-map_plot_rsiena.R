# library(RSiena)
# mynet <- sienaDependent(array(c(s501, s502), dim=c(50, 50, 2)))
# mycov  <- coCovar(s50a[,1])
# mydata <- sienaDataCreate(mynet, mycov)
# myeff <- getEffects(mydata)
# myeff <- includeEffects(myeff, simX, interaction1="mycov")
# myalgorithm <- sienaAlgorithmCreate(nsub=2, n3=100, seed=1291)
# ans <- siena07(myalgorithm, data=mydata, effects=myeff, silent=TRUE, batch=TRUE)
# res <- selectionTable(ans, mydata, "mynet", "mycov")
# 
# test_that("selection plot is the same", {
#   expect_snapshot_file(plot(res))
# })
