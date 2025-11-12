# Plotting methods for MRQAP models

These plotting methods are for results obtained by fitting an MRQAP
model. The S3 classes are "netlm" or "netlogit", and so are compatible
with the results from either the `{sna}` or `{migraph}` packages.

## Usage

``` r
# S3 method for class 'netlm'
plot(x, ...)

# S3 method for class 'netlogit'
plot(x, ...)
```

## Arguments

  - x:
    
    An object obtained by fitting an MRQAP model to some data. For
    example, `migraph::net_regression()`.

  - ...:
    
    Further arguments to be passed on to plot.

## Value

A plot showing the location of observed statistics compared to the
distribution of statistics from permuted networks.

## Examples

``` r
# Here's something I cooked up with migraph earlier:
plot(res_migraph_reg)
```
