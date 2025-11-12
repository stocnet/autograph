# Layered layout

Layered layout

## Usage

``` r
layout_tbl_graph_layered(.data, center = NULL, circular = FALSE, times = 4)
```

## Arguments

  - .data:
    
    Some `{manynet}` compatible network data.

  - center, circular:
    
    Extra parameters required for `{tidygraph}` compatibility.

  - times:
    
    Integer of sweeps that the algorithm will pass through. By default
    4.

## Value

Returns a table of coordinates.

## Examples

``` r
ties <- data.frame(
  from = c("A", "A", "B", "C", "D", "F", "F", "E"),
  to   = c("B", "C", "D", "E", "E", "E", "G", "G"),
  stringsAsFactors = FALSE)

coords <- layout_tbl_graph_layered(ties, times = 6)
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
#> Warning: argument is not numeric or logical: returning NA
coords
#>    x y
#> A NA 4
#> B  2 3
#> C  1 3
#> D  1 2
#> F NA 4
#> E  1 1
#> G  1 0
```
