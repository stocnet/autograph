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

