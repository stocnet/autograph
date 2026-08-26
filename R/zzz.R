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
  snet_info("You are using {.auto autograph} version {.version {local_version}}.")
  snet_info(c("i" = "Theme set to {.code {getOption('stocnet_theme')}}. Use {.fn stocnet_theme} to change the theme."))

  # Only after the interactive() guard above: a script or a check run should
  # never reach into the IDE, whatever is remembered.
  if (isTRUE(read_pref("completion")) && .completion_activate())
    snet_info("Completion of argument values is on. Use {.fn stocnet_completion} to switch it off.")
}
# nocov end


