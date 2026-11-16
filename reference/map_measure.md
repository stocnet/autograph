# Plotting logical marks Plotting numeric measures

These functions plot distributions for node, tie, and network measures,
as defined in the `{manynet}` package.

## Usage

``` r
# S3 method for class 'node_measure'
plot(x, type = c("h", "d"), ...)

# S3 method for class 'tie_measure'
plot(x, type = c("h", "d"), ...)

# S3 method for class 'network_measures'
plot(x, ...)

# S3 method for class 'network_measure'
plot(x, ..., labels = NULL)
```

## Arguments

- x:

  An object of "node_measure", "tie_measure", "network_measure", or
  "network_measures" class.

- type:

  For node and tie measures, whether the plot should be "h" a histogram
  or "d" a density plot. By default "h".

- ...:

  Other arguments to be passed on.

- labels:

  For network measures with a curve, such as robustness, an optional
  character vector of labels, one for each result. By default, the names
  given in the call are used, and otherwise the measure's own name.

## Value

`plot.node_measure()` and `plot.tie_measure()` returns a histogram
and/or density plot of the distribution of the measure.

`plot.network_measures()` returns a plot of the measure traced over
time.

`plot.network_measure()` returns a plot of the curve that some network
measures carry, such as the robustness curve returned by
`netrics::net_by_robustness()`. The share of nodes in the largest
component is drawn against the share of nodes removed. The dashed line
is the best case, where every remaining node stays in one component, and
the area under it is the largest robustness possible, 0.5. Several
results can be passed together to compare them in one plot. A measure
without a curve is a single value, so nothing is plotted.

## Examples

``` r
plot(netrics::node_by_deg(ison_karateka))

plot(netrics::tie_by_betweenness(ison_karateka))

plot(res_netrics_robust$failure)

plot(res_netrics_robust$failure,
     res_netrics_robust$attack)
```
