# Tab-completion of argument *values* for graphr(), graphs(), grapht() and
# stocnet_theme(), in RStudio.
#
# `graphr(fict_lotr, node_color = ` gives the user no way to see which node
# variables the network holds. graph_checks.R already knows every set of values
# these arguments accept, and .abort_no_match() lists them once a wrong value is
# given; this offers the same sets *before* a value is typed.
#
# R has no supported hook for completing argument values. `.DollarNames` covers
# `$` only, and `utils::rc.options("custom.completer")` is a single global slot
# that RStudio honours by handing the whole session over to R's own completion
# engine, which is weaker than RStudio's. So this wraps one RStudio function
# instead, as the polars package does, and delegates every line it does not
# recognise. That is a private API, so activation is at the user's request
# (see stocnet_completion()), every path falls back to the original on error,
# and deactivation restores what was there before.
#
# The parsing and candidate functions below know nothing about RStudio, and are
# tested directly; only .completion_wrap() touches the IDE.

# Candidates ----

# The functions whose arguments are completed. Anything else is left to RStudio.
.completion_funs <- function() c("graphr", "graphs", "grapht", "stocnet_theme",
                                 "set_stocnet_theme")

# Values for `arg` in a call to `fun` on the network `g`, as a data frame of
# the value itself, the label RStudio shows in brackets beside it, and a line of
# detail after that. No rows where there is nothing to offer.
#
# Attributes come first: they are the values that cannot be looked up in the
# documentation. Colour names are deliberately not offered for `node_color`:
# six hundred of them would bury the handful of attributes that are the point.
.completion_values <- function(arg, g = NULL, fun = "graphr") {
  if (is.null(arg) || !nzchar(arg)) return(.completion_frame())
  nodes <- .completion_attr_values(g, "node")
  ties <- .completion_attr_values(g, "tie")
  out <- switch(
    arg,
    "node_color" = ,
    "node_colour" = nodes,
    "node_size" = nodes,
    "node_group" = nodes,
    "membership" = nodes,
    "level" = nodes,
    "rank" = nodes,
    "node_shape" = rbind(nodes, .completion_frame(.shape_names, "shape")),
    "edge_color" = ,
    "edge_colour" = ties,
    "edge_size" = ties,
    "center" = rbind(nodes, .completion_frame(c("events", "actors"), "mode")),
    "labels" = rbind(
      .completion_attr_values(g, "node", logical_only = TRUE),
      .completion_frame(.label_criteria(), "measure"),
      .completion_frame(
        if (!is.null(g) && manynet::is_labelled(g)) manynet::node_labels(g),
        "node")),
    "layout" = .completion_layout_values(),
    "theme" = .completion_frame(theme_opts, "theme"),
    # An argument whose default is a vector of choices, as `isolates` and
    # `based_on` are, carries its own candidates. Reading them from the formals
    # means a new choice needs no change here.
    .completion_frame(.completion_choices(fun, arg), "option"))
  out[!duplicated(out$value) & !is.na(out$value), , drop = FALSE]
}

.completion_frame <- function(value = character(), label = "", meta = "") {
  value <- as.character(value)
  data.frame(value = value,
             label = rep_len(if (length(value)) label else character(), length(value)),
             meta = rep_len(if (length(value)) meta else character(), length(value)),
             stringsAsFactors = FALSE)
}

# Node or tie attributes, labelled with the kind of variable each one holds, so
# that a variable worth colouring by can be told from one that is not.
.completion_attr_values <- function(g, what = c("node", "tie"),
                                    logical_only = FALSE) {
  what <- match.arg(what)
  nms <- .completion_attrs(g, what)
  if (!length(nms)) return(.completion_frame())
  vals <- lapply(nms, function(nm) tryCatch(
    if (what == "node") igraph::vertex_attr(g, nm) else igraph::edge_attr(g, nm),
    error = function(e) NULL))
  if (logical_only) {
    keep <- vapply(vals, is.logical, logical(1))
    nms <- nms[keep]
    vals <- vals[keep]
    if (!length(nms)) return(.completion_frame())
  }
  .completion_frame(nms,
                    vapply(vals, .completion_kind, character(1)),
                    vapply(vals, .completion_detail, character(1)))
}

.completion_attrs <- function(g, what = c("node", "tie")) {
  what <- match.arg(what)
  if (is.null(g)) return(character())
  nms <- tryCatch(
    if (what == "node") igraph::vertex_attr_names(g) else igraph::edge_attr_names(g),
    error = function(e) character())
  # `name` holds the node labels rather than a variable to map an aesthetic to.
  setdiff(nms, if (what == "node") "name" else character())
}

# What kind of variable this is, in the words graphr()'s documentation uses.
.completion_kind <- function(x) {
  if (is.null(x)) return("")
  if (inherits(x, "node_mark") || inherits(x, "tie_mark") || is.logical(x)) return("mark")
  if (is.factor(x)) return("factor")
  if (is.character(x)) return("character")
  if (is.numeric(x)) return("numeric")
  class(x)[[1L]]
}

# A line about the values themselves, since which variable to map an aesthetic
# to depends on how many categories it has, or over what range it runs.
.completion_detail <- function(x) {
  if (is.null(x) || !length(x)) return("")
  if (is.logical(x)) return(paste(sum(x, na.rm = TRUE), "of", length(x)))
  if (is.numeric(x)) {
    rng <- suppressWarnings(range(x, na.rm = TRUE))
    if (any(!is.finite(rng))) return("")
    return(paste(format(rng[[1L]], trim = TRUE, digits = 3), "to",
                 format(rng[[2L]], trim = TRUE, digits = 3)))
  }
  vals <- as.character(x)
  lvls <- unique(vals[!is.na(vals)])
  # Few enough categories to read at a glance are worth naming outright.
  listed <- paste(lvls, collapse = ", ")
  if (length(lvls) <= 4L && nchar(listed) <= 32L) return(listed)
  paste(length(lvls), "categories")
}

# Layouts, labelled with the package that draws them, since which ones suit a
# network is documented package by package.
.completion_layout_values <- function() {
  own <- .autograph_layouts()
  # `.valid_layouts()` keeps the retired autograph names, so that giving one is
  # still valid and gets renamed rather than refused. They are not offered,
  # here or as `own`, so subtract them rather than let them fall through to
  # `rest` and be labelled as somebody else's layouts.
  rest <- setdiff(.valid_layouts(), c(own, .deprecated_layouts()))
  ggraph <- sub("^layout_tbl_graph_", "",
                grep("^layout_tbl_graph_",
                     tryCatch(ls(asNamespace("ggraph"), all.names = TRUE),
                              error = function(e) character()), value = TRUE))
  rbind(.completion_frame(own, "autograph"),
        .completion_frame(rest, ifelse(rest %in% ggraph, "ggraph", "igraph")))
}

# Logical node attributes, which `labels` accepts as a selection of nodes.
.completion_marks <- function(g) {
  .completion_attr_values(g, "node", logical_only = TRUE)$value
}

.completion_choices <- function(fun, arg) {
  fmls <- .completion_formals(fun)
  if (!arg %in% names(fmls)) return(character())
  default <- fmls[[arg]]
  if (!is.call(default) || !identical(default[[1L]], quote(c))) return(character())
  vals <- tryCatch(eval(default), error = function(e) NULL)
  if (!is.character(vals) || length(vals) < 2L) return(character())
  vals
}

.completion_formals <- function(fun) {
  fn <- tryCatch(get(fun, envir = asNamespace("autograph")), error = function(e) NULL)
  if (!is.function(fn)) return(NULL)
  formals(fn)
}

# Parsing the line ----

# Where the cursor sits in `line`: which function is being called, which of its
# arguments is being given a value, how much of that value is typed, and whether
# the cursor is inside a string. Returns NULL when the line is not a call to one
# of .completion_funs().
#
# The line is scanned character by character rather than parsed, because a line
# that is still being typed does not parse: `graphr(fict_lotr, node_color = "`
# has an open bracket and an unterminated string.
.completion_context <- function(line) {
  if (!is.character(line) || length(line) != 1L || !nzchar(line)) return(NULL)
  frames <- .completion_scan(line)
  if (!length(frames)) return(NULL)
  # The value being typed always belongs to the innermost frame. The call it
  # belongs to may be one frame further out, since `labels = c("Alice", "Be` is
  # a documented way of writing a selection.
  inner <- frames[[length(frames)]]
  value <- .completion_value_text(.completion_last_chunk(line, inner))
  fun <- NULL
  for (i in rev(seq_along(frames))) {
    callee <- .completion_callee(substr(line, 1L, frames[[i]]$open - 1L))
    if (is.null(callee)) return(NULL)
    if (callee %in% .completion_funs()) { fun <- callee; frame <- frames[[i]]; break }
    if (!identical(callee, "c")) return(NULL)
  }
  if (is.null(fun)) return(NULL)
  chunks <- .completion_chunks(line, frame)
  arg <- .completion_argname(fun, chunks)
  if (is.null(arg)) return(NULL)
  list(fun = fun, arg = arg, token = value$token, quoted = value$quoted,
       data = .completion_data_text(fun, chunks))
}

# The brackets still open at the end of the line, outermost first, each with the
# positions of the commas directly inside it. A comma inside a nested call, a
# string or a bracketed index belongs to that frame, not to this one.
.completion_scan <- function(line) {
  chars <- strsplit(line, "", fixed = TRUE)[[1L]]
  stack <- integer()      # positions of the brackets still open
  kinds <- character()    # which bracket each one is
  commas <- list()        # top-level comma positions, one entry per open bracket
  quoting <- ""
  escaped <- FALSE
  for (i in seq_along(chars)) {
    ch <- chars[[i]]
    if (escaped) { escaped <- FALSE; next }
    if (nzchar(quoting)) {
      if (ch == "\\") escaped <- TRUE else if (ch == quoting) quoting <- ""
      next
    }
    if (ch %in% c("\"", "'", "`")) { quoting <- ch; next }
    if (ch %in% c("(", "[", "{")) {
      stack <- c(stack, i); kinds <- c(kinds, ch); commas <- c(commas, list(integer()))
      next
    }
    if (ch %in% c(")", "]", "}")) {
      n <- length(stack)
      if (n) { stack <- stack[-n]; kinds <- kinds[-n]; commas <- commas[-n] }
      next
    }
    if (ch == "," && length(stack)) {
      n <- length(stack)
      commas[[n]] <- c(commas[[n]], i)
    }
  }
  n <- length(stack)
  if (!n || kinds[[n]] != "(") return(list())
  keep <- which(kinds == "(")
  lapply(keep, function(i) list(open = stack[[i]], commas = commas[[i]]))
}

.completion_last_chunk <- function(line, frame) {
  chunks <- .completion_chunks(line, frame)
  chunks[[length(chunks)]]
}

# The function being called: the identifier before the open bracket, with any
# `pkg::` prefix dropped.
.completion_callee <- function(before) {
  m <- regmatches(before, regexpr("[A-Za-z._][A-Za-z0-9._]*\\s*$", before))
  if (!length(m) || !nzchar(trimws(m))) return(NULL)
  trimws(m)
}

# The arguments given so far, as written, split on the top-level commas.
.completion_chunks <- function(line, scan) {
  starts <- c(scan$open, scan$commas) + 1L
  ends <- c(scan$commas - 1L, nchar(line))
  mapply(substr, list(line), starts, ends, USE.NAMES = FALSE)
}

.completion_named <- function(chunk) {
  # `(?!=)` so that a comparison, `x == 1`, is not read as naming an argument.
  m <- regexpr("^\\s*[A-Za-z._][A-Za-z0-9._]*\\s*=(?!=)", chunk, perl = TRUE)
  if (m == -1L) return(NULL)
  trimws(sub("=$", "", regmatches(chunk, m)))
}

# Which argument the last chunk fills. A named chunk says so itself; an unnamed
# one takes the next formal that no chunk has claimed by name, as R's own
# argument matching does.
.completion_argname <- function(fun, chunks) {
  last <- chunks[[length(chunks)]]
  named <- .completion_named(last)
  if (!is.null(named)) return(named)
  fmls <- names(.completion_formals(fun))
  if (is.null(fmls)) return(NULL)
  claimed <- unlist(lapply(chunks, .completion_named))
  free <- setdiff(fmls, c(claimed, "..."))
  rank <- sum(vapply(chunks, function(x) is.null(.completion_named(x)), logical(1)))
  if (rank > length(free)) return(NULL)
  free[[rank]]
}

# How much of the value is typed, and whether it is inside quotes.
.completion_value_text <- function(chunk) {
  rest <- sub("^\\s*[A-Za-z._][A-Za-z0-9._]*\\s*=(?![=])", "", chunk, perl = TRUE)
  rest <- sub("^\\s+", "", rest)
  q <- regmatches(rest, regexpr("^[\"']", rest))
  if (length(q) && nzchar(q)) {
    list(token = substring(rest, 2L), quoted = TRUE)
  } else list(token = rest, quoted = FALSE)
}

# The expression given as the network, as written.
.completion_data_text <- function(fun, chunks) {
  fmls <- names(.completion_formals(fun))
  if (is.null(fmls) || !length(fmls)) return(NULL)
  first <- fmls[[1L]]
  for (chunk in chunks) {
    named <- .completion_named(chunk)
    if (identical(named, first))
      return(trimws(sub("^\\s*[A-Za-z._][A-Za-z0-9._]*\\s*=", "", chunk)))
  }
  if (!is.null(.completion_named(chunks[[1L]]))) return(NULL)
  trimws(chunks[[1L]])
}

# The network the call names, or NULL. Only a symbol is looked up: evaluating
# `graphr(to_undirected(net), ...)` would run code every time Tab is pressed.
.completion_object <- function(text, envir = parent.frame()) {
  if (is.null(text) || !nzchar(text)) return(NULL)
  if (!grepl("^[A-Za-z._][A-Za-z0-9._]*$", text)) return(NULL)
  obj <- tryCatch(get0(text, envir = envir), error = function(e) NULL)
  if (is.null(obj)) return(NULL)
  tryCatch(manynet::as_igraph(obj), error = function(e) NULL)
}

# What to offer for `line`, or NULL where there is nothing to add. The token is
# returned alongside the values because RStudio replaces it with the choice.
.completion_suggest <- function(line, envir = parent.frame()) {
  ctx <- .completion_context(line)
  if (is.null(ctx)) return(NULL)
  g <- .completion_object(ctx$data, envir)
  vals <- tryCatch(.completion_values(ctx$arg, g, ctx$fun),
                   error = function(e) .completion_frame())
  vals <- .completion_matches(vals, ctx$token)
  if (!nrow(vals)) return(NULL)
  list(token = ctx$token, values = vals, quoted = ctx$quoted)
}

# Values the typed token could grow into: those starting with it, then those
# merely containing it, as RStudio's own fuzzy matching does.
.completion_matches <- function(values, token) {
  if (!nrow(values)) return(values)
  if (is.null(token) || !nzchar(token)) return(values)
  starts <- startsWith(tolower(values$value), tolower(token))
  if (any(starts)) return(values[starts, , drop = FALSE])
  values[grepl(tolower(token), tolower(values$value), fixed = TRUE), , drop = FALSE]
}

# The RStudio hook ----

# RStudio answers every completion request through one function in its
# `tools:rstudio` environment, which receives the line as typed. Wrapping that
# function, rather than one of the `.rs.getCompletions*` helpers, is what makes
# a value inside quotes reachable: by the time the helpers are called RStudio
# has already decided the string is a file path, and no longer knows which
# function or argument it belongs to.
.completion_rpc <- ".rs.rpc.get_completions"
.completion_saved <- ".rs.rpc.get_completions.autograph"

.completion_env <- function() {
  tryCatch(as.environment("tools:rstudio"), error = function(e) NULL)
}

.completion_active <- function() {
  env <- .completion_env()
  !is.null(env) && exists(.completion_saved, envir = env, inherits = FALSE)
}

.completion_activate <- function() {
  env <- .completion_env()
  if (is.null(env)) return(FALSE)
  if (.completion_active()) return(TRUE)
  original <- tryCatch(get(.completion_rpc, envir = env, inherits = FALSE),
                       error = function(e) NULL)
  if (!is.function(original)) return(FALSE)
  wrapper <- .completion_wrap(original, env)
  if (is.null(wrapper)) return(FALSE)
  tryCatch({
    assign(.completion_saved, original, envir = env)
    assign(.completion_rpc, wrapper, envir = env)
    TRUE
  }, error = function(e) FALSE)
}

.completion_deactivate <- function() {
  env <- .completion_env()
  if (is.null(env) || !.completion_active()) return(FALSE)
  tryCatch({
    assign(.completion_rpc, get(.completion_saved, envir = env, inherits = FALSE),
           envir = env)
    rm(list = .completion_saved, envir = env)
    TRUE
  }, error = function(e) FALSE)
}

# A replacement for RStudio's function, with the same formals as the version
# installed, so that however many arguments this RStudio passes, and in whatever
# order, they reach the original untouched. The call is forwarded as written
# rather than argument by argument, so an argument RStudio did not supply is
# never forced.
.completion_wrap <- function(original, env) {
  wrapper <- function() {
    completions <- tryCatch(.completion_rstudio(environment(), env),
                            error = function(e) NULL)
    if (!is.null(completions)) return(completions)
    call <- sys.call()
    call[[1L]] <- original
    eval(call, parent.frame())
  }
  formals(wrapper) <- formals(original)
  environment(wrapper) <- list2env(list(original = original, env = env,
                                        .completion_rstudio = .completion_rstudio),
                                   parent = asNamespace("autograph"))
  wrapper
}

# Completions in RStudio's own shape, or NULL to let RStudio answer. `frame`
# holds the arguments RStudio was called with; only `line` is read from it, so
# no other argument is forced.
.completion_rstudio <- function(frame, env) {
  line <- get0("line", envir = frame, ifnotfound = NULL)
  if (!is.character(line) || length(line) != 1L) return(NULL)
  suggestion <- .completion_suggest(line, globalenv())
  if (is.null(suggestion)) return(NULL)
  make <- tryCatch(get(".rs.makeCompletions", envir = env), error = function(e) NULL)
  if (!is.function(make)) return(NULL)
  types <- tryCatch(get(".rs.acCompletionTypes", envir = env),
                    error = function(e) list())
  # RStudio shows `packages` in brackets beside a COLUMN completion, and `meta`
  # after that, so the kind of variable and a line about its values are visible
  # without leaving the popup.
  make(token = suggestion$token,
       results = suggestion$values$value,
       packages = suggestion$values$label,
       meta = suggestion$values$meta,
       # Values given without quotes are inserted with them, since every one of
       # these arguments takes its value as a string.
       quote = !suggestion$quoted,
       type = if (is.null(types$COLUMN)) types$STRING else types$COLUMN,
       excludeOtherCompletions = TRUE)
}

# The user-facing switch ----

#' Completing argument values as you type
#'
#' @description
#'   `graphr()` and its relatives take the names of node and tie variables,
#'   layouts, and themes as strings, which means remembering what a network
#'   holds. This offers those names to RStudio's completion system, so that
#'   writing `graphr(fict_lotr, node_color = "` and pressing Tab lists the
#'   variables `fict_lotr` holds, `layout = "` lists the layouts available,
#'   and so on for every argument with a known set of values.
#'
#'   This is off until it is asked for, because it works by replacing one of
#'   RStudio's internal functions. That function is not part of a public
#'   interface, so a future version of RStudio can change it. Nothing else about
#'   completion changes: any line that is not one of these calls is passed to
#'   RStudio untouched, as is any line this cannot make sense of.
#'
#'   `stocnet_completion(FALSE)` puts RStudio's function back.
#' @param activate Logical, by default TRUE.
#'   If TRUE, completion of argument values is switched on.
#'   If FALSE, RStudio's own completions are restored.
#'   If missing, the current state is reported and nothing changes.
#' @param persist Logical, by default FALSE.
#'   If TRUE, the choice is remembered across sessions,
#'   by writing it to the user's configuration directory
#'   (see `tools::R_user_dir()`).
#'   Nothing is written to disk unless this is set explicitly.
#'   Use `stocnet_completion(persist = FALSE)` when activating
#'   to forget a previously persisted choice.
#' @returns Invisibly, TRUE where completion is now active and FALSE otherwise.
#'   Called for the effect it has on the IDE.
#' @family mapping
#' @name completion
#' @examples
#' \dontrun{
#' # In RStudio, switch completion on for this session:
#' stocnet_completion()
#' # Then type graphr(fict_lotr, node_color = " and press Tab.
#' # To switch it off again:
#' stocnet_completion(FALSE)
#' }
#' @export
stocnet_completion <- function(activate, persist = FALSE) {
  if (missing(activate)) {
    if (.completion_active()) {
      manynet::snet_info("Completion of argument values is {.emph on}.")
    } else if (is.null(.completion_env())) {
      manynet::snet_info(
        "Completion of argument values is available in {.emph RStudio} only.")
    } else {
      manynet::snet_info(c(
        "Completion of argument values is {.emph off}.",
        "i" = "Use {.fn stocnet_completion} to switch it on."))
    }
    return(invisible(.completion_active()))
  }
  if (!is.logical(activate) || length(activate) != 1L || is.na(activate))
    manynet::snet_abort("{.arg activate} should be either TRUE or FALSE.")
  if (activate) {
    if (is.null(.completion_env())) {
      manynet::snet_info(c(
        "Completion of argument values works in {.emph RStudio} only.",
        "i" = "Nothing has been changed."))
      return(invisible(FALSE))
    }
    if (!.completion_activate()) {
      manynet::snet_warn(c(
        "Completion of argument values could not be switched on.",
        "i" = "This version of RStudio may complete arguments differently."))
      return(invisible(FALSE))
    }
    manynet::snet_success("Completion of argument values is on.")
  } else {
    .completion_deactivate()
    manynet::snet_success("Completion of argument values is off.")
  }
  if (persist) {
    if (write_pref("completion", activate))
      manynet::snet_success("This will be remembered in future sessions.")
  } else forget_pref("completion")
  invisible(.completion_active())
}

#' @rdname completion
#' @export
set_completion <- stocnet_completion
