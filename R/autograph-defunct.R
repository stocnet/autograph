# Compatibility with older {manynet} ------------------------------------------

# autograph declares the oldest {manynet} it works with, which is the version
# on CRAN. Two functions it uses arrived in manynet 2.3.0, so each is reached
# through a shim that calls manynet where the function is there and does the
# same thing itself where it is not. Each shim tests for the function rather
# than for the version string, because a pre-release dev build can carry the
# version without yet exporting the function. Delete a shim once its minimum
# is raised past 2.3.0. Both branches reach the function through
# `getExportedValue()` rather than `::`, since `R CMD check` resolves a `::`
# call against the installed manynet and reports the newer name as missing
# even where the call is never reached.

# `manynet::delete_isolates()` is manynet 2.3.0's name for `to_no_isolates()`,
# which it otherwise leaves unchanged.
.ag_delete_isolates <- function(.data) {
  fn <- if (.ag_has_manynet("delete_isolates")) "delete_isolates" else
    "to_no_isolates"
  getExportedValue("manynet", fn)(.data)
}

# `manynet::is_multilevel()` marks TRUE a network whose nodes fall into two or
# more levels that are tied both within and between. The fallback repeats
# manynet's own igraph method: levels are read from the 'lvl' attribute that
# `to_multilevel()` writes, and otherwise from the modes of a two-mode
# network. A 'stocnet' is coerced to an 'igraph' first, as manynet's default
# method does, so a network of three or more levels is read as two.
.ag_is_multilevel <- function(.data) {
  if (.ag_has_manynet("is_multilevel"))
    return(getExportedValue("manynet", "is_multilevel")(.data))
  .data <- manynet::as_igraph(.data)
  if ("lvl" %in% igraph::vertex_attr_names(.data))
    return(length(unique(igraph::vertex_attr(.data, "lvl"))) > 1)
  if (!manynet::is_twomode(.data)) return(FALSE)
  # A tie-less network is neither, and is returned before `tie_is_twomode()`,
  # which cannot name an empty measure.
  if (manynet::net_ties(.data) == 0) return(FALSE)
  between <- manynet::tie_is_twomode(.data)
  any(between) && any(!between)
}

.ag_has_manynet <- function(fn) {
  fn %in% getNamespaceExports("manynet")
}

# Compatibility with older {goldfish} -----------------------------------------

# The goldfish overview draws each panel from a goldfish diagnostic, several of
# which arrived after the version on CRAN. Reaching them through
# `getExportedValue()` rather than `::` keeps `R CMD check` from resolving the
# call against whichever goldfish is installed and reporting the newer names as
# missing. Where the function really is missing the error this raises is caught
# by `gf_overview_try()`, which leaves that panel out, exactly as it does for a
# fit that stores no such primitive.
.ag_goldfish <- function(fn) {
  getExportedValue("goldfish", fn)
}

