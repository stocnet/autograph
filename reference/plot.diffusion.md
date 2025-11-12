# Plotting diffusion models

Plotting diffusion models

## Usage

``` r
# S3 method for class 'diff_model'
plot(x, ..., all_steps = TRUE)

# S3 method for class 'diffs_model'
plot(x, ...)

# S3 method for class 'learn_model'
plot(x, ...)
```

## Arguments

  - x:
    
    A "diff\_model" of "diffs\_model" class of object. E.g. as a result
    from `manynet::play_diffusion()`.

  - ...:
    
    Other arguments to be passed.

  - all\_steps:
    
    Whether all steps should be plotted or just those where there is
    change in the distributions.

## Value

`plot.diff_model()` returns a bar chart of the number of new infected
nodes at each time point, as well as an overlay line plot of the total
of infected

## Examples

``` r
plot(res_manynet_diff)

plot(res_migraph_diff)
#> Warning: pseudoinverse used at -0.015
#> Warning: neighborhood radius 2.015
#> Warning: reciprocal condition number  3.9979e-17
#> Warning: There are other near singularities as well. 4.0602
#> Warning: pseudoinverse used at -0.015
#> Warning: neighborhood radius 2.015
#> Warning: reciprocal condition number  3.9979e-17
#> Warning: There are other near singularities as well. 4.0602
#> Warning: pseudoinverse used at -0.015
#> Warning: neighborhood radius 2.015
#> Warning: reciprocal condition number  3.9979e-17
#> Warning: There are other near singularities as well. 4.0602
#> Warning: pseudoinverse used at -0.015
#> Warning: neighborhood radius 2.015
#> Warning: reciprocal condition number  3.9979e-17
#> Warning: There are other near singularities as well. 4.0602

plot(play_learning(ison_networkers, beliefs = runif(net_nodes(ison_networkers))))
```
