# Deprecated layout names

Each of these draws what its replacement draws, after saying so. They
are kept so that a call naming the older layout still draws, and will be
removed.

- "hierarchy" is now "layered", which is what the layout does to a
  two-mode network, where the two modes are two layers and neither is
  above the other in any hierarchy.

- "alluvial" is now "lineage". The name is held for a plot of changing
  membership composition over time.

- "multilevel" is now "levels", which `{graphlayouts}` does not also
  use.

- "dyad", "triad", "tetrad", "pentad" and "hexad" are now all
  "configuration", which already picks the one matching the number of
  nodes. The functions of those names are not deprecated.

Note that `.deprecated_layouts()` lists these, so that neither the
completions nor the functional audit offers a retired name.

## Usage

``` r
layout_hierarchy(.data, ...)

layout_tbl_graph_hierarchy(.data, ...)

layout_alluvial(.data, ...)

layout_tbl_graph_alluvial(.data, ...)

layout_multilevel(.data, ...)

layout_tbl_graph_multilevel(.data, ...)

layout_tbl_graph_dyad(.data, ...)

layout_tbl_graph_triad(.data, ...)

layout_tbl_graph_tetrad(.data, ...)

layout_tbl_graph_pentad(.data, ...)

layout_tbl_graph_hexad(.data, ...)
```

## Arguments

- .data:

  Some `{manynet}` compatible network data.

- ...:

  Arguments passed on to the replacement layout.

## Value

Returns a table of nodes' x and y coordinates.
