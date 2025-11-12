# Valence-based layout

Valence-based layout

## Usage

``` r
layout_valence(
  .data,
  times = 500,
  center = NULL,
  circular = FALSE,
  repulsion_coef = 1,
  attraction_coef = 0.05
)

layout_tbl_graph_valence(
  .data,
  times = 500,
  center = NULL,
  circular = FALSE,
  repulsion_coef = 1,
  attraction_coef = 0.05
)
```

## Arguments

  - .data:
    
    Some `{manynet}` compatible network data.

  - times:
    
    Integer of sweeps that the algorithm will pass through. By default
    4.

  - center, circular:
    
    Extra parameters required for `{tidygraph}` compatibility.

  - repulsion\_coef:
    
    Coefficient for global repulsion force. Default is 1.

  - attraction\_coef:
    
    Coefficient for edge-based attraction/repulsion force. Default is
    0.05.

## Examples

``` r
edges <- data.frame(
  from = c("A", "B", "C", "D"),
  to   = c("B", "C", "D", "A"),
  weight = c(2, 3, 1, 4),
  sign = c(1, -1, 1, -1)  # 1 = positive, -1 = negative
  )
graphr(as_igraph(edges), layout="valence")
```
