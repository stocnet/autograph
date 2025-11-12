# Plotting methods for CUG and QAP tests

These plotting methods are for results obtained by testing some
statistic against those produced in a reference distribution of
conditional uniform graphs or as a quadratic assignment procedure. The
S3 class is "network\_test".

## Usage

``` r
# S3 method for class 'network_test'
plot(x, ..., threshold = 0.95, tails = c("two", "one"))
```

## Arguments

  - x:
    
    An object obtained from a conditional uniform graph or quadratic
    assignment procedure test. For example,
    `migraph::test_permutation()`.

  - ...:
    
    Other arguments to be passed on.

  - threshold:
    
    The empirical threshold to shade in the plot.

  - tails:
    
    By default "two" indicating a two-tailed test, but "one" for a
    one-tailed test is also available.

## Value

A distribution of the simulated or permuted statistics, with 2.5% shaded
at each end, and a line highlighting where the observed statistic lies
on this distribution.

## Examples

``` r
# Here's something I cooked up with migraph earlier:
plot(res_migraph_test)
```
