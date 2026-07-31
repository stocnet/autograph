# Argument validation shared by graphr(), graphs(), grapht() and stocnet_theme().
#
# The aim is that a mistyped argument fails immediately, here, with a message
# naming the argument and suggesting what the user probably meant -- rather than
# falling through to ggplot2, grid or igraph and surfacing as something like
# "Unknown colour name: wealthh" or "object 'layout_tbl_graph_stresss' not found".
#
# Note that `snet_abort()` is used rather than `snet_warn()`/`snet_info()`
# because those respect `options(snet_verbosity = "quiet")` and so can be
# silenced, whereas validation must always be heard.

# Suggestions ----

# Closest candidate in `valid` to `value`, or NULL when nothing is close enough
# to be worth suggesting. The threshold scales with the length of the string so
# that a one-character slip in a short name still matches, without proposing a
# wild guess for a long one.
.suggest_name <- function(value, valid) {
  if (length(value) != 1L || !is.character(value) || !length(valid)) return(NULL)
  dists <- utils::adist(value, valid, ignore.case = TRUE)[1, ]
  best <- which.min(dists)
  if (!length(best)) return(NULL)
  threshold <- max(1, floor(nchar(value) / 3))
  if (dists[best] > threshold) return(NULL)
  valid[best]
}

# Matching ----

#' Resolve a user-supplied name against the set of names that would work
#'
#' @param value The value the user supplied.
#' @param valid Character vector of names that are valid here.
#' @param arg Name of the argument, used in the message.
#' @param what What `valid` contains, e.g. "node attribute", used in the message.
#' @param extra Values that are valid but are not names, e.g. colour names for
#'   `node_color`. Matched case-insensitively.
#' @param allow A predicate for values that are valid but cannot be enumerated,
#'   e.g. a hex colour code.
#' @param extra_desc A sentence describing `extra` for the error message.
#' @param show The subset of `valid` worth listing in the message, when listing
#'   all of them would be more noise than help. Defaults to all of `valid`.
#' @return The value to use downstream: `value` itself when it matches exactly
#'   or is in `extra`, or the correctly-cased name when it differs only in case.
#'   Aborts otherwise.
#' @noRd
.match_name <- function(value, valid, arg, what = "attribute",
                        extra = NULL, allow = NULL, extra_desc = NULL,
                        show = valid) {
  # Only length-1 character values are checked; anything else (a number, a
  # multi-element expression) is passed through for the caller to handle.
  if (is.null(value) || length(value) != 1L || !is.character(value)) return(value)
  if (value %in% valid) return(value)
  # A capitalisation slip is common enough, and unambiguous enough, to fix
  # silently rather than to reject.
  cased <- valid[tolower(valid) == tolower(value)]
  if (length(cased) == 1L) {
    manynet::snet_info("Interpreting {.arg {arg}} = {.val {value}} as {.val {cased}}.")
    return(cased)
  }
  if (!is.null(extra)) {
    # Return the canonical spelling: R's colour names, for instance, are
    # lowercase, so "Red" has to become "red" before it reaches grid.
    hit <- extra[tolower(extra) == tolower(value)]
    if (length(hit) == 1L) return(hit)
  }
  if (!is.null(allow) && isTRUE(allow(value))) return(value)
  .abort_no_match(value, valid, arg, what, extra_desc, show)
}

.abort_no_match <- function(value, valid, arg, what, extra_desc = NULL,
                            show = valid) {
  suggestion <- .suggest_name(value, valid)
  msg <- "Could not find {.val {value}}, given as {.arg {arg}}, among the {what}s available."
  if (!is.null(suggestion)) msg <- paste(msg, "Did you mean {.val {suggestion}}?")
  shown <- sort(show)
  if (length(shown)) {
    msg <- paste(msg, if (identical(shown, sort(valid))) {
      "The {what}s available are {.val {shown}}."
    } else "{.pkg autograph} provides the {what}s {.val {shown}}.")
  } else if (!length(valid)) {
    msg <- paste(msg, "There are no {what}s to choose from here.")
  }
  if (!is.null(extra_desc)) msg <- paste(msg, extra_desc)
  manynet::snet_abort(msg)
}

# Fixed sets of options ----

# A friendlier match.arg(): the base message ("'arg' should be one of "legend",
# "caption", "keep"") names the internal variable rather than the argument the
# user wrote, and offers no suggestion.
.check_choice <- function(value, choices, arg) {
  # An untouched default is the whole vector of choices; take the first, as
  # match.arg() does.
  if (identical(value, choices)) return(choices[1])
  if (length(value) != 1L || !is.character(value))
    manynet::snet_abort(
      "{.arg {arg}} should be one of {.or {.val {choices}}}, given as a single string.")
  if (value %in% choices) return(value)
  cased <- choices[tolower(choices) == tolower(value)]
  if (length(cased) == 1L) return(cased)
  suggestion <- .suggest_name(value, choices)
  msg <- "{.arg {arg}} should be one of {.or {.val {choices}}}, but {.val {value}} was given."
  if (!is.null(suggestion)) msg <- paste(msg, "Did you mean {.val {suggestion}}?")
  manynet::snet_abort(msg)
}

# Aesthetics ----

# Each of these resolves one graphr()/grapht() aesthetic argument, which may
# name a node or tie attribute or give a literal value, and aborts naming the
# argument if it is neither. They replace the advisory .check_node_variables()
# and .check_edge_variables() notes, which could be silenced and which let the
# bad value through to fail later inside ggplot2 or grid.

.is_hex_color <- function(x) grepl("^#[0-9A-Fa-f]{3,8}$", x)

.color_desc <- paste("A colour name such as {.val red},",
                     "or a hex code such as {.val #4576B5},",
                     "can also be given here.")

# ggplot2 accepts these shape names, plus the numbers 0 to 25 and any single
# character to plot as a glyph.
.shape_names <- c("square open", "circle open", "triangle open", "plus", "cross",
                  "diamond open", "triangle down open", "square cross", "asterisk",
                  "diamond plus", "circle plus", "star", "square plus",
                  "circle cross", "square triangle", "triangle square", "square",
                  "circle small", "triangle", "diamond", "circle", "bullet",
                  "circle filled", "square filled", "diamond filled",
                  "triangle filled", "triangle down filled")

.is_shape_code <- function(x) {
  nchar(x) <= 1 || (grepl("^[0-9]+$", x) && as.numeric(x) %in% 0:25)
}

.check_node_color <- function(g, node_color, arg = "node_color") {
  .match_name(node_color, igraph::vertex_attr_names(g), arg,
              what = "node attribute", extra = grDevices::colors(),
              allow = .is_hex_color, extra_desc = .color_desc)
}

.check_edge_color <- function(g, edge_color, arg = "edge_color") {
  .match_name(edge_color, igraph::edge_attr_names(g), arg,
              what = "tie attribute", extra = grDevices::colors(),
              allow = .is_hex_color, extra_desc = .color_desc)
}

.check_node_shape <- function(g, node_shape) {
  .match_name(node_shape, igraph::vertex_attr_names(g), "node_shape",
              what = "node attribute", extra = .shape_names,
              allow = .is_shape_code,
              extra_desc = paste("A shape name such as {.val circle} or",
                                 "{.val square}, or a number from 0 to 25,",
                                 "can also be given here."))
}

.check_node_size <- function(g, node_size) {
  .match_name(node_size, igraph::vertex_attr_names(g), "node_size",
              what = "node attribute",
              extra_desc = "A number, such as {.code node_size = 6}, can also be given here.")
}

.check_edge_size <- function(g, edge_size) {
  .match_name(edge_size, igraph::edge_attr_names(g), "edge_size",
              what = "tie attribute",
              extra_desc = "A number, such as {.code edge_size = 2}, can also be given here.")
}

.check_node_group <- function(g, node_group) {
  .match_name(node_group, igraph::vertex_attr_names(g), "node_group",
              what = "node attribute")
}

# Layout arguments ----

# Several of autograph's layouts need one value per node -- a membership, a
# level, a rank -- which the user supplies as an extra argument to graphr().
# Getting it wrong otherwise surfaces as "argument "rank" is missing, with no
# default", which says nothing about how to supply it.
.abort_layout_arg <- function(arg, layout, n) {
  manynet::snet_abort(
    "The {.val {layout}} layout needs a {.arg {arg}} for each node.",
    "Please give {.arg {arg}} either the name of a node attribute, as in",
    "{.code graphr(net, layout = \"{layout}\", {arg} = \"myattribute\")},",
    "or a vector holding one value for each of the {n} nodes.")
}

# Networks ----

# Coerce `.data` to a tidygraph, reporting what was given rather than letting
# the failure surface as a missing method for an internal generic.
.check_network <- function(.data, arg = ".data") {
  out <- tryCatch(manynet::as_tidygraph(.data), error = function(e) e)
  if (!inherits(out, "error")) return(out)
  cls <- class(.data)
  manynet::snet_abort(
    "{.arg {arg}} should be a network that {.pkg manynet} recognises,",
    "such as an igraph or tidygraph object, a network or matrix,",
    "or an edgelist data frame, but an object of class {.cls {cls}} was given.",
    "See {.fn manynet::as_tidygraph} for the formats that can be converted.")
}

# Layouts ----

# The layout names ggraph::create_layout() can actually resolve: those with a
# `layout_tbl_graph_*` function in ggraph (which includes the graphlayouts
# layouts) or in autograph, plus igraph's algorithms, which ggraph accepts
# either in full or with the `as_`/`in_`/`with_`/`on_` prefix dropped.
# Derived at call time so the list cannot drift from what is installed.
.valid_layouts <- function() {
  from_ns <- function(ns) {
    nms <- tryCatch(ls(asNamespace(ns), all.names = TRUE), error = function(e) character())
    sub("^layout_tbl_graph_", "", grep("^layout_tbl_graph_", nms, value = TRUE))
  }
  igraph_layouts <- sub("^layout_", "", grep("^layout_(as|in|with|on)_|^layout_(nicely|randomly|components)$",
                                             getNamespaceExports("igraph"), value = TRUE))
  sort(unique(c(from_ns("ggraph"), .autograph_layouts(), igraph_layouts,
                sub("^(as|in|with|on)_", "", igraph_layouts))))
}

.check_layout <- function(layout) {
  if (is.null(layout)) return(layout)
  if (is.function(layout)) {
    manynet::snet_abort(
      "{.arg layout} should be the {.emph name} of a layout, given as a string,",
      "rather than a layout function.",
      "For example, use {.code layout = \"fr\"} instead of",
      "{.code layout = igraph::layout_with_fr}.")
  }
  if (!is.character(layout) || length(layout) != 1L) {
    manynet::snet_abort(
      "{.arg layout} should be a single layout name, but a value of length",
      "{length(layout)} was given.")
  }
  # There are around seventy layouts once ggraph, graphlayouts and igraph are
  # counted, which is too many to list. Only autograph's own layouts are named,
  # since those are the ones not documented elsewhere, and the rest are pointed
  # to by package.
  .match_name(layout, .valid_layouts(), "layout", what = "layout",
              show = .autograph_layouts(),
              extra_desc = paste("Layouts provided by {.pkg ggraph},",
                                 "{.pkg graphlayouts} and {.pkg igraph},",
                                 "such as {.val stress} or {.val fr},",
                                 "can also be named here."))
}

.autograph_layouts <- function() {
  nms <- tryCatch(ls(asNamespace("autograph"), all.names = TRUE),
                  error = function(e) character())
  sub("^layout_tbl_graph_", "", grep("^layout_tbl_graph_", nms, value = TRUE))
}
