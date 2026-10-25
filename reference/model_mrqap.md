# Plotting methods for MRQAP models

These plotting methods are for results obtained by fitting an MRQAP
model. The S3 class "netreg" comes from `{infernet}`, and includes its
"netreg_lm", "netreg_glm", and "netreg_css" results. The S3 classes
"netlm" and "netlogit" come from the `{sna}` package or from earlier
versions of `{migraph}`.

## Usage

``` r
# S3 method for class 'netreg'
plot(x, ...)

# S3 method for class 'netlm'
plot(x, ...)

# S3 method for class 'netlogit'
plot(x, ...)
```

## Arguments

- x:

  An object obtained by fitting an MRQAP model to some data. For
  example, `infernet::net_regression()`.

- ...:

  Further arguments to be passed on to plot.

## Value

A plot showing the location of observed statistics compared to the
distribution of statistics from permuted networks. The vertical lines
mark the 2.5% and 97.5% quantiles of each distribution. A term with no
permuted statistics, such as the intercept where predictors are
permuted, shows only its observed statistic.

## Examples

``` r
# Here's something I cooked up with infernet earlier:
plot(res_infernet_reg)

# And with an earlier version of migraph:
plot(res_migraph_reg)
```
