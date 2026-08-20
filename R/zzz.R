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
    set_divergent_theme(saved_theme)
    set_background_theme(saved_theme)
    set_categorical_theme(saved_theme)
    set_font_theme(saved_theme)
  }

  if (!interactive()) return()

  # Only after the interactive() guard above: a script or a check run should
  # never reach into the IDE, whatever is remembered.
  if (isTRUE(read_pref("completion")) && .completion_activate())
    snet_info("Completion of argument values is on. Use {.fn stocnet_completion} to switch it off.")

  local_version <- utils::packageVersion("autograph")
  snet_info("You are using {.auto autograph} version {.version {local_version}}.")
  snet_info(c("i" = "Theme set to {.code {getOption('stocnet_theme')}}. Use {.fn stocnet_theme} to change the theme."))

}
# nocov end


