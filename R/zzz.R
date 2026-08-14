# nocov start
.onAttach <- function(...) {

  # suppressMessages(suppressPackageStartupMessages(library("manynet", warn.conflicts = FALSE)))
  if (!interactive()) return()
  
  # options(manynet_verbosity = getOption("manynet_verbosity", "verbose"))
  options(stocnet_theme = getOption("stocnet_theme", "default"))
  # options(cli.theme = manynet_console_theme())
  # options(cli.progress_clear = TRUE)

  local_version <- utils::packageVersion("autograph")
  snet_info("You are using {.auto autograph} version {.version {local_version}}.")

  greet_startup_cli <- function() {
    tips <- c(
      # "i" = "Theming graphs and plots is straightforward with `stocnet_theme()`",
      "i" = "Theme set to {.code {getOption('stocnet_theme')}}. Use {.fn stocnet_theme} to change the theme."
      # "i" = "Please share bugs, issues, or feature requests at {.url https://github.com/stocnet/autograph/issues}.",
      # "i" = "To eliminate package startup messages, use: `suppressPackageStartupMessages(library({.pkg autograph}))`.",
      # "i" = "If there are too many messages in the console, run `options(manynet_verbosity = 'quiet')`",
      # "i" = "Visit the website to learn more: {.url https://stocnet.github.io/autograph/}."
      # "i" = "We recommend the 'Function Overview' page online to discover new analytic opportunities: {.url https://stocnet.github.io/autograph/reference/index.html}.",
    )
    snet_info(sample(tips, 1))
  }

  greet_startup_cli()

}
# nocov end


