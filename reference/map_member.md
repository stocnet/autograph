# Plotting categorical memberships

This plotting method operates on "node\_member" class objects from the
`{manynet}` package, plotting the dendrogram of their membership.

## Usage

``` r
# S3 method for class 'node_member'
plot(x, ...)

# S3 method for class 'matrix'
plot(x, ..., membership = NULL)
```

## Arguments

  - x:
    
    An object of "node\_member" class, for example as a result of
    running `manynet::node_in_community()`.

  - ...:
    
    Other arguments to be passed on.

  - membership:
    
    A "node\_member" membership vector.

## Value

`plot.node_member()` returns a dendrogram, with labels colored to
indicate the different clusters, and with the optimal cutpoint shown by
a dashed highlight line.

`plot.matrix()` returns a plot of an adjacency or incidency matrix,
potentially with the rows and columns reordered to illustrate an
additional membership vector.

## Examples

``` r
plot(manynet::node_in_walktrap(ison_southern_women, "e"))

plot(as_matrix(ison_adolescents),
  membership = node_in_walktrap(ison_adolescents, "e"))

plot(as_matrix(ison_southern_women),
  membership = node_in_walktrap(ison_southern_women, "e"))
```
