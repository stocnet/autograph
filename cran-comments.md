## Resubmission

This is a resubmission of 1.2.0, which failed the incoming checks with a test error
on Windows and on Debian. Both are fixed here.

* `layout_valence()` started its nodes at random points, and a start that placed two
  nodes close together gave a force large enough to spoil the layout. The nodes now
  start on a circle, the force is bounded, and the test sets a seed.
* On Windows, the tutorial test failed on a deprecation warning that the tutorial code
  does not raise itself: `netrics::tie_by_closeness()` (0.4.1) calls
  `manynet::to_ties()`, which manynet deprecates in 2.3.0. netrics 1.0.0, to be
  submitted, calls the current function. The test now fails only if the tutorial calls
  a deprecated function itself, so it passes with either netrics version.

The full test suite passes with each of these three pairs: manynet 2.2.3 with
netrics 0.4.0, manynet 2.3.1 with netrics 0.4.1 (the pair that failed on Windows),
and manynet 2.3.0 with netrics 1.0.0. manynet 2.3.1 and netrics 1.0.0 are to be
submitted; this version does not require either of them.

## Test environments

* local R installation, aarch64-apple-darwin20, R 4.5.1
* macOS 14.7.6 (on Github), R 4.5.1
* Microsoft Windows Server 2022 10.0.20348 (on Github), R 4.5.1
* Ubuntu 24.04.2 (on Github), R 4.5.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## User filespace

This version adds an optional `persist` argument to `stocnet_theme()`. When, and only when, a user
passes `persist = TRUE`, the chosen theme is written to `tools::R_user_dir("autograph", "config")`.
Nothing is written on load, on attach, or by any default code path, and the package is fully
functional if the directory is absent or unwritable. No other location is written to.

## Backward/forward compatibility

This version works and tests alongside both manynet 2.2.3 and 2.3.0.
manynet 2.3.0 ships several networks in a list-based class, 
and spells a layer as the tie attribute "layer" rather than "type" and 
a sign as a negative weight rather than as a "sign".
Which tie attribute records the layer is now read from the network, 
so a multiplex network is still coloured by layer under either spelling.
Signs are read through `manynet::tie_signs()`, in `graphr()` and in `layout_valence()`, 
rather than from a "sign" tie attribute.
Attribute names are read through `manynet::net_node_attributes()`/`net_tie_attributes()`, 
which accept a network in either class.
`layout_concentric()`, `layout_multilevel()` and `layout_lineage()` coerce what they are given, 
as the other layouts already did.
`grapht()` and `graphs()` fall back to `manynet::to_times()` where `to_waves()` 
cannot split a network, which covers a panel recording its waves as "time" 
(e.g. `ison_monks`) and a diffusion result.
Each guard tests for the function or attribute rather than for the manynet version, 
so a development build is treated by what it offers.

