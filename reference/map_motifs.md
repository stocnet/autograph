# Plotting tabular motifs

These functions will plot graphs of the motifs used in a vector of
results of e.g. a triad census.

## Usage

``` r
# S3 method for class 'node_motif'
plot(x, ...)

# S3 method for class 'network_motif'
plot(x, ...)
```

## Arguments

- x:

  An object of "node_motif" class, e.g. resulting from a call to
  [`manynet::node_by_triad()`](https://stocnet.github.io/manynet/reference/defunct.html).

- ...:

  Other arguments to be passed on.

## Value

`plot.node_motif()` returns a set of graphs that illustrate the motifs
mentioned in the results from a node_motif function in `{manynet}`.

`plot.network_motif()` returns a set of graphs that illustrate the
motifs mentioned in the results from a net_motif function in
`{manynet}`.
