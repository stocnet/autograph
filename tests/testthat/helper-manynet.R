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
