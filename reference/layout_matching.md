# Matching layout

This layout works to position nodes opposite their matching nodes. See
`manynet::to_matching()` for more details on the matching procedure.

## Usage

``` r
layout_tbl_graph_matching(.data, center = NULL, circular = FALSE, times = 1)
```

## Arguments

  - .data:
    
    Some `{manynet}` compatible network data.

  - center, circular, times:
    
    Extra parameters required for `{tidygraph}` compatibility.

## Value

Returns a table of nodes' x and y coordinates.
