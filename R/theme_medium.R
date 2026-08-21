#' Setting the medium a plot is made for
#' @description
#'   A theme says how a plot should look.
#'   A medium says where it will be seen, which is a separate question:
#'   the same institutional theme serves a figure worked on at a desk,
#'   projected in a lecture theatre, printed in an article,
#'   and read on a phone, but each of those wants a different size of text
#'   and, in one case, a different ground.
#'   `stocnet_medium()` sets the medium for all subsequent plots,
#'   as `stocnet_theme()` sets the theme, and leaves the theme alone.
#'   
#'   If no medium is specified (i.e. the function is called without argument),
#'   the current medium is reported.
#'   The default medium is "screen".
#' @details
#'   The media available are:
#'   
#'   - "screen", the default, which draws as `{autograph}` always has.
#'   - "presentation", which enlarges text by half, for a figure read from
#'     the back of a room.
#'   - "mobile", which enlarges text further, for a figure read in a narrow
#'     column on a handheld screen.
#'     Keep such a figure to one point, with few categories:
#'     a legend of more than about seven keys, or more than about three panels
#'     from `graphs()`, will not survive the width.
#'   - "print", which leaves text at its usual size but draws on white,
#'     whatever ground the theme prefers.
#'     A dark or tinted ground costs ink and is often not reproduced.
#'   
#'   The medium scales text, not marks.
#'   Node sizes are relative to the layout they sit in,
#'   so enlarging them without enlarging the layout would crowd it.
#'   Where a figure needs larger nodes as well, set `node_size` in [graphr()].
#'   
#'   The medium does not set the size of the file written.
#'   Give `ggplot2::ggsave()` the width, height and resolution the medium
#'   calls for as well.
#' @name theme_medium
#' @family themes
#' @param medium String naming a medium.
#'   By default "screen".
#'   The following media are currently available:
#'   `r autograph:::medium_opts`.
#'   This string can be capitalised or not.
#' @param persist Logical, by default FALSE.
#'   If TRUE, the medium is remembered across sessions,
#'   by writing it to the user's configuration directory
#'   (see `tools::R_user_dir()`).
#'   Nothing is written to disk unless this is set explicitly.
#'   Use `stocnet_medium(persist = FALSE)` when setting a medium
#'   to forget a previously persisted choice.
#' @returns `stocnet_medium()` sets the medium to be used across all
#'   stocnet packages. The medium is written to an option and held there.
#'   `ag_size()` returns the multiplier the current medium applies to text
#'   sizes, which is 1 unless the medium says otherwise.
#' @examples
#' stocnet_medium("presentation")
#' ag_size()
#' stocnet_medium("screen")
#' @export
stocnet_medium <- function(medium = NULL, persist = FALSE){
  if(is.null(medium)){
    medium <- getOption("stocnet_medium", default = "screen")
    snet_info("Medium is currently set to {.emph {medium}}.",
              "The following media are available: {.emph {medium_opts}}.")
  } else {
    if(!is.character(medium) || length(medium) != 1L)
      manynet::snet_abort(
        "{.arg medium} should be the name of a single medium, given as a string.",
        "The media available are {.val {medium_opts}}.")
    medium <- .match_name(tolower(medium), medium_opts, "medium",
                          what = "medium")
    options(stocnet_medium = medium)
    snet_success("Medium set to {.emph {medium}}.")
    if(persist){
      if(write_medium_pref(medium))
        snet_success("Medium will be remembered in future sessions.")
    } else forget_medium_pref()
  }
}

#' @rdname theme_medium
#' @export
set_stocnet_medium <- stocnet_medium

medium_opts <- c("screen", "presentation", "mobile", "print")

# How much larger the text is in each medium. "print" is left at 1: a printed
# figure is held at reading distance like any other page, so what it needs is
# not larger text but a ground that reproduces.
medium_sizes <- c(screen = 1, presentation = 1.5, mobile = 1.8, print = 1)

#' @rdname theme_medium
#' @export
ag_size <- function(){
  unname(medium_sizes[getOption("stocnet_medium", default = "screen")])
}

# Text drawn by a geom, or set on a theme element directly, does not pass
# through the base_size that ag_themer() scales, so it is scaled here. Marks
# are deliberately left alone: a node's size is relative to the layout it sits
# in, and enlarging nodes without enlarging the layout would crowd it.
ag_text_size <- function(size) size * ag_size()

# The medium overrides the theme's ground only for print, and only in that
# direction: the ink and the palettes are the theme's own in every medium.
medium_background <- function(){
  if(getOption("stocnet_medium", default = "screen") == "print") "#FFFFFF"
  else NULL
}

# See write_pref() in autograph_utilities.R.
write_medium_pref <- function(medium) write_pref("medium", medium)

forget_medium_pref <- function() forget_pref("medium")

read_medium_pref <- function(){
  medium <- read_pref("medium")
  if(is.null(medium) || !is.character(medium) || length(medium) != 1L ||
     !medium %in% medium_opts) return(NULL)
  medium
}
