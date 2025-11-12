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
```

## Arguments

  - x:
    
    An object of "node\_measure", "tie\_measure", or "network\_measures"
    class.

  - type:
    
    For node and tie measures, whether the plot should be "h" a
    histogram or "d" a density plot. By default "h".

  - ...:
    
    Other arguments to be passed on.

## Value

`plot.node_measure()` and `plot.tie_measure()` returns a histogram
and/or density plot of the distribution of the measure.

`plot.network_measures()` returns a plot of the measure traced over
time.

## Examples

``` r
plot(manynet::node_deg(ison_karateka))

plot(manynet::tie_betweenness(ison_karateka))
```
