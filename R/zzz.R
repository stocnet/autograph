.onAttach <- function(...) {

  # suppressMessages(suppressPackageStartupMessages(library("manynet", warn.conflicts = FALSE)))
  if (!interactive()) return()
  
  # options(manynet_verbosity = getOption("manynet_verbosity", "verbose"))
  options(stocnet_theme = getOption("stocnet_theme", "default"))
  # options(cli.theme = manynet_console_theme())
  # options(cli.progress_clear = TRUE)
  
  # pkgs <- as.data.frame(utils::available.packages(utils::contrib.url(getOption("repos"))))
  # 
  # cran_version <- pkgs[pkgs$Package == "manynet","Version"]

  local_version <- utils::packageVersion("autograph")
  snet_info("You are using {.pkg autograph} version {.version {local_version}}.")
  old.list <- as.data.frame(utils::old.packages())
  behind_cran <- "autograph" %in% old.list$Package
  
  greet_startup_cli <- function() {
    tips <- c(
      "i" = "There are lots of ways to contribute to {.pkg autograph} at {.url https://github.com/stocnet/autograph/}.",
      "i" = "Please let us know any bugs, issues, or feature requests at {.url https://github.com/stocnet/autograph/issues}. It's really helpful!",
      # "i" = "To eliminate package startup messages, use: `suppressPackageStartupMessages(library({.pkg autograph}))`.",
      "i" = "Changing the theme of all your graphs and plots is straightforward with `stocnet_theme()`",
      # "i" = "If there are too many messages in the console, run `options(manynet_verbosity = 'quiet')`",
      "i" = "Visit the website to learn more: {.url https://stocnet.github.io/autograph/}.",
      "i" = "We recommend the 'Function Overview' page online to discover new analytic opportunities: {.url https://stocnet.github.io/autograph/reference/index.html}.",
      # "i" = "Star me at {.url https://github.com/users/follow?target=jhollway}.",
      # "i" = "You can list all the tutorials available in {.pkg autograph} using {.fn run_tute}, and run them too!",
      "i" = "Discover all the {.emph stocnet} R packages at {.url https://github.com/stocnet/}."
    )
    snet_info(sample(tips, 1))
  }

  if (interactive()) {
    if (behind_cran) {
      msg <- "A new version of autograph is available with bug fixes and new features."
      packageStartupMessage(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        utils::update.packages("autograph")
      }
    } else {
      greet_startup_cli()
    }
  }

}


