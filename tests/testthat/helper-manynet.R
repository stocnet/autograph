# manynet 2.3.0 ships several of its networks in a list-based class, where
# 2.2.3 and earlier shipped every network as an igraph. It also spells some
# tie attributes differently there: a layer is recorded as "layer" rather than
# as "type", and a sign as a negative weight rather than as a "sign".
#
# graphr() and its siblings coerce whatever network they are given, so the
# internal helpers beneath them only ever see a coerced network. A test that
# calls one of those helpers directly, or that reaches into igraph itself,
# therefore coerces first, so that it reads the same network under either
# manynet. A network that is already an igraph is unchanged by this.
ag_net <- function(x) manynet::as_tidygraph(x)

# Whether the installed manynet exports a function, for a test of behaviour
# that only the newer manynet can offer. Tests for the function rather than
# for the version, as the package itself does, since a development build can
# carry a version string without the function.
manynet_has <- function(fn) fn %in% getNamespaceExports("manynet")

# manynet raised snet_warn() from a cli alert to a warning condition in 2.3.2.
# CRAN still ships 2.3.1, where the same call prints (and only where the
# verbosity allows it) rather than raising anything a test can catch, so a
# deprecation that warns here announces nothing there. Probed rather than read
# off the version, as manynet_has() is, and probed once, since the answer
# cannot change within a session.
snet_warns <- local({
  known <- NULL
  function() {
    if (is.null(known)) {
      known <<- FALSE
      withCallingHandlers(
        suppressMessages(manynet::snet_warn("probing snet_warn")),
        warning = function(w) {
          known <<- TRUE
          invokeRestart("muffleWarning")
        })
    }
    known
  }
})

# A deprecation, or any other snet_warn(), asserted against either manynet.
# Where the installed manynet raises a warning, the warning and its wording are
# asserted. Where it does not, the call is only asked to run, since there is no
# condition to catch: the test then still covers the code path, and the
# assertion returns as soon as manynet 2.3.2 reaches CRAN.
expect_snet_warning <- function(expr, regexp, label = NULL) {
  if (snet_warns()) testthat::expect_warning(expr, regexp, label = label)
  else testthat::expect_no_error(expr)
}
