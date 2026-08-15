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

# Labels ----

# `labels` is more than a switch: it can also select *which* nodes to label,
# by rank on a measure, by a mark or logical attribute, or by naming nodes
# outright. This resolves any of those into one of four normalised forms --
# FALSE, TRUE, a rank depth carrying the criterion to rank by, or a character
# vector of node names -- which .infer_labels() then turns into a selection.
# Node names are the normal form for an explicit selection because graphr()
# drops isolates after this check, which would shift every node's position.

.label_criteria <- function() c("degree", "betweenness", "cutpoints", "random")

.label_desc <- paste(
  "A number of ranks to label, such as {.code labels = 5},",
  "a measure to rank nodes by ({.val degree}, {.val betweenness},",
  "{.val cutpoints} or {.val random}), or the names of the nodes to label,",
  "can also be given here.")

.check_labels <- function(g, labels, arg = "labels") {
  # Without node names there is nothing to draw, whatever was asked for.
  if (!manynet::is_labelled(g)) return(FALSE)
  if (is.null(labels)) return(FALSE)
  n <- as.numeric(manynet::net_nodes(g))
  nms <- manynet::node_names(g)
  len <- length(labels)
  if (is.logical(labels)) {
    if (len == 1L) {
      if (is.na(labels)) .abort_labels_type(labels, arg)
      return(labels)
    }
    if (len != n)
      manynet::snet_abort(
        "{.arg {arg}} should be a single value or one value for each of the",
        "{n} nodes in the network, but {len} value{?s} {?was/were} given.")
    return(nms[!is.na(labels) & labels])
  }
  if (is.numeric(labels)) {
    if (len == 1L) {
      if (is.na(labels) || labels <= 0 || labels != round(labels))
        manynet::snet_abort(
          "{.arg {arg}} should be a positive whole number of ranks to label,",
          "as in {.code {arg} = 5}, but {.val {labels}} was given.")
      # Asking for more ranks than there are nodes asks for all of them.
      if (labels >= n) return(TRUE)
      crit <- names(labels)
      crit <- if (is.null(crit) || !nzchar(crit)) "degree" else
        .check_choice(crit, .label_criteria(), arg)
      return(structure(as.integer(labels), criterion = crit))
    }
    bad <- labels[is.na(labels) | labels < 1 | labels > n |
                    labels != round(labels)]
    n_bad <- length(bad)
    if (n_bad)
      manynet::snet_abort(
        "{.arg {arg}} should be the positions of the nodes to label, between",
        "1 and {n}, the number of nodes in the network,",
        "but {n_bad} of the values given {?is/are} not: {.val {bad}}.")
    return(nms[unique(labels)])
  }
  if (is.character(labels)) {
    if (len == 1L) {
      # A node attribute takes precedence over a criterion, and a criterion over
      # a node name, as .check_node_color() prefers an attribute to a colour.
      value <- .match_name(labels, igraph::vertex_attr_names(g), arg,
                           what = "node attribute",
                           extra = unique(c(.label_criteria(), nms)),
                           show = igraph::vertex_attr_names(g),
                           extra_desc = .label_desc)
      if (value %in% igraph::vertex_attr_names(g))
        return(.labels_from_attribute(g, value, arg))
      if (value %in% .label_criteria())
        # Every criterion but "random" has a maximum to take, so one rank is
        # enough; a random selection has to be given a size instead.
        return(structure(if (value == "random") min(10L, as.integer(n)) else 1L,
                         criterion = value))
      return(value)
    }
    unknown <- setdiff(labels, nms)
    n_unknown <- length(unknown)
    if (n_unknown) {
      suggestion <- .suggest_name(unknown[1], nms)
      msg <- paste("{.arg {arg}} should name nodes in the network, but",
                   "{n_unknown} of the names given {?was/were} not found",
                   "among them: {.val {unknown}}.")
      if (!is.null(suggestion))
        msg <- paste(msg, "Did you mean {.val {suggestion}}?")
      manynet::snet_abort(msg)
    }
    return(labels)
  }
  .abort_labels_type(labels, arg)
}

.labels_from_attribute <- function(g, attribute, arg) {
  vals <- manynet::node_attribute(g, attribute)
  if (!is.logical(vals))
    manynet::snet_abort(
      "{.arg {arg}} can name a node attribute marking which nodes to label,",
      "but {.val {attribute}} holds {.cls {class(vals)}} values rather than",
      "{.cls logical} ones.",
      "A measure can be given instead, as in {.code {arg} = \"degree\"}.")
  manynet::node_names(g)[!is.na(vals) & vals]
}

.abort_labels_type <- function(labels, arg) {
  manynet::snet_abort(
    "{.arg {arg}} should be {.code TRUE} or {.code FALSE}, a number of ranks",
    "to label, the name of a node attribute or measure, or a vector selecting",
    "which nodes to label, but a value of class {.cls {class(labels)}}",
    "was given.")
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

# Layout applicability ----

# Several layouts only make sense for particular kinds of network: a ladder
# pairs two equally sized modes off, the configurational layouts place an exact
# number of nodes at fixed coordinates, and a layered layout ranks nodes by
# path depth. Given anything else they used to either draw a meaningless plot
# (railway, alluvial and hierarchy on one-mode input) or fail somewhere
# downstream with a message about the internals ("replacement has 5 rows, data
# has 8"). Declaring the requirement here means graphr() can say what it needs
# and fall back to a layout that works, and the test suite can read the same
# table rather than keeping its own copy of this knowledge.
#
# `check` is a predicate over manynet's marks; `need` completes the sentence
# "The {layout} layout needs ...". Layouts with no entry are unconstrained.
.layout_requirements <- function() {
  n_nodes <- function(g) as.integer(manynet::net_nodes(g))
  exactly <- function(n) list(check = function(g, ...) n_nodes(g) == n,
                              need = paste("a network of exactly", n, "nodes"))
  twomode <- list(check = function(g, ...) manynet::is_twomode(g),
                  need = "a two-mode (bipartite) network")
  list(
    alluvial   = twomode,
    railway    = twomode,
    hierarchy  = twomode,
    # Being two-mode is not sufficient: manynet::to_matching() cannot pair off
    # every two-mode network, and where it fails it does so with a message
    # about differing numbers of rows. Probing it is cheap for the sizes these
    # layouts are used at, and a plot beats that error. Note the network need
    # not have a *perfect* matching -- ison_southern_women has none but lays
    # out fine -- so is_perfect_matching() is not the test.
    matching = list(
      check = function(g, ...) manynet::is_twomode(g) &&
        !inherits(tryCatch(manynet::to_matching(g), error = function(e) e),
                  "error"),
      need = "a two-mode network that a matching can be found for"),
    ladder = list(
      check = function(g, ...) manynet::is_twomode(g) &&
        length(unique(table(manynet::node_is_mode(g)))) == 1L,
      need = "a two-mode network whose two modes are equally sized"),
    layered = list(
      check = function(g, ...) manynet::is_directed(g) && manynet::is_acyclic(g),
      need = "a directed acyclic network"),
    valence = list(
      check = function(g, ...) manynet::is_signed(g),
      need = "a signed network"),
    # `concentric` and `multilevel` are deliberately absent. They also need
    # more than a bare one-mode network, but unlike the layouts above the user
    # can supply what is missing -- a `membership` or a `level` -- and
    # .abort_layout_arg() already says exactly how. Substituting would replace
    # that instruction with a worse message. Substitute only where no argument
    # could rescue the layout; where one could, ask for it.
    configuration = list(
      check = function(g, ...) n_nodes(g) >= 2L && n_nodes(g) <= 6L,
      need = "a network of between 2 and 6 nodes"),
    dyad = exactly(2), triad = exactly(3), tetrad = exactly(4),
    pentad = exactly(5), hexad = exactly(6)
  )
}

# Does `layout` apply to `g`? TRUE when nothing is declared for it.
.layout_applies <- function(g, layout, ...) {
  req <- .layout_requirements()[[layout]]
  if (is.null(req)) return(TRUE)
  isTRUE(tryCatch(req$check(g, ...), error = function(e) FALSE))
}

# Return the layout to actually use. Where the requested one does not apply,
# fall back to whatever graphr() would have chosen unasked, and say so, rather
# than failing downstream or drawing something meaningless.
.check_layout_applies <- function(g, layout, ...) {
  if (is.null(layout) || !is.character(layout) || length(layout) != 1L)
    return(layout)
  if (.layout_applies(g, layout, ...)) return(layout)
  need <- .layout_requirements()[[layout]]$need
  alt <- .infer_layout(g, NULL)
  # The inferred fallback has its own requirement (e.g. "hierarchy" needs two
  # modes), so guard against substituting one unusable layout for another.
  if (identical(alt, layout) || !.layout_applies(g, alt, ...)) alt <- "stress"
  manynet::snet_info(
    "The {.val {layout}} layout needs {need}, so {.val {alt}} is used instead.",
    "Use {.code layout = \"{alt}\"} to choose this explicitly,",
    "or see {.fn graphr} for the other layouts available.")
  alt
}
