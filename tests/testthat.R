library(testthat)
library(autograph)

# Parallel workers crash intermittently on CRAN's Windows machine (0xC0000005)
# and hide the call that faults, so run the files serially there. Read before
# Config/testthat/parallel, so local and CI runs stay parallel.
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  Sys.setenv(TESTTHAT_PARALLEL = "false")
}

stocnet_theme("default")
test_check("autograph")
