# nocov start
.onAttach <- function(...) {

  # suppressMessages(suppressPackageStartupMessages(library("manynet", warn.conflicts = FALSE)))
  
  # A theme the user chose with `stocnet_theme(persist = TRUE)` becomes the
  # default, but an option set in this session still wins.
  saved_theme <- read_theme_pref()
  options(stocnet_theme = getOption("stocnet_theme",
                                    if (is.null(saved_theme)) "default" else saved_theme))
  # Apply the palettes too, so a persisted theme takes effect on the first plot
  # rather than only after `stocnet_theme()` is called again.
  if (!is.null(saved_theme)) {
    set_highlight_theme(saved_theme)
    # The ink was missing here, so a persisted dark theme -- "neon" above all
    # -- came back with its near-black ground and the default dark ink.
    set_ink_theme(saved_theme)
    set_divergent_theme(saved_theme)
    set_background_theme(saved_theme)
    set_categorical_theme(saved_theme)
    set_missing_theme(saved_theme)
    set_font_theme(saved_theme)
  }

  # The medium is remembered separately from the theme, and says where the
  # plot will be seen rather than how it should look. See ?stocnet_medium.
  saved_medium <- read_medium_pref()
  options(stocnet_medium = getOption("stocnet_medium",
                                     if (is.null(saved_medium)) "screen" else saved_medium))

  if (!interactive()) return()

  local_version <- utils::packageVersion("autograph")
  snet_info("You are using {.auto autograph} version {.version {local_version}},",
            "with theme {.code {getOption('stocnet_theme')}}.")

  # Only after the interactive() guard above: a script or a check run should
  # never reach into the IDE, whatever is remembered.
  completion_on <- isTRUE(read_pref("completion")) && .completion_activate()

  # One short status line. The theme is always there, because there is always a
  # theme. The medium and the completion appear only when they are away from
  # their defaults, so the common case stays to a few words. What is left out of
  # the status line can still be reached through a tip below.
  greet_startup_cli <- function() {
    medium <- getOption("stocnet_medium")
    status <- paste0("Theme {.code ", getOption("stocnet_theme"), "}")
    if (!identical(medium, "screen"))
      status <- paste0(status, ", medium {.code ", medium, "}")
    if (completion_on) status <- paste0(status, ", completion {.code on}")
    # snet_info(c("i" = paste0(status, ".")))

    tips <- c(
      "i" = "Change the theme with {.run [stocnet_theme()](autograph::stocnet_theme())}.",
      "i" = "Keep a theme for later sessions with {.code stocnet_theme(persist = TRUE)}.",
      "i" = "Set output medium with {.run [stocnet_medium()](autograph::stocnet_medium())}. Currently {.code getOption('stocnet_medium')}",
      "i" = "Autocomplete arguments with {.run [stocnet_completion()](autograph::stocnet_completion())}."
      # "i" = "Share bugs, issues, or feature requests at {.url https://github.com/stocnet/autograph/issues}.",
      # "i" = "Explore changes since the last version with {.run [news(package = 'autograph')](utils::news(package = 'autograph'))}.",
      # "i" = "Visit {.url https://stocnet.github.io/autograph/} to learn more.",
      # "i" = "Discover new functions at {.url https://stocnet.github.io/autograph/reference/index.html}.",
      # "i" = "Discover {.emph stocnet} R packages at {.url https://github.com/stocnet/}."
    )
    # Do not offer to switch on what is already on, or to set what the status
    # line already reports.
    if (completion_on) tips <- tips[-4]
    if (!identical(medium, "screen")) tips <- tips[-3]
    snet_info(sample(tips, 1))
  }

  greet_startup_cli()
}
# nocov end
