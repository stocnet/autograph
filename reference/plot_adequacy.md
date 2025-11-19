# Plotting adequacy diagnostics

These plotting methods are for diagnosing the adequacy of model
specification, such as those used in goldfish. These plots are useful
for identifying whether there might be significant outliers affecting
the results or significant time heterogeneity.

## Usage

``` r
# S3 method for class 'outliers.goldfish'
plot(x, ...)

# S3 method for class 'changepoints.goldfish'
plot(x, ...)
```

## Arguments

  - x:
    
    An object of class "outliers.goldfish" or "changepoints.goldfish".

  - ...:
    
    Additional plotting parameters, currently unused.

## Value

The function shows a line plot tracing the statistics obtained at each
simulation step, as well as a density plot showing the distribution of
the statistics over the entire simulation.

## Examples

``` r
plot(goldfish_outliers)

plot(goldfish_changepoints)
```
