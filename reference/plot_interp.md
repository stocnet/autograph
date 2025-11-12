# Plotting effects interpretation

These functions support the interpretation of network and behavior
effects found in stochastic actor-oriented models. They are S3 plotting
methods for objects of class "selectionTable" or "influenceTable",
created using `RSiena::selectionTable()` or `RSiena::influenceTable()`,
respectively. They plot how the evaluation function for selection or
influence changes based on ego's value and alter's value of some
covariate. This helps to interpret the effect of that covariate on the
network dynamics or behavior dynamics, respectively.

## Usage

``` r
# S3 method for class 'selectionTable'
plot(x, quad = TRUE, separation = 0, ...)

# S3 method for class 'influenceTable'
plot(x, separation = 0, ...)
```

## Arguments

  - x:
    
    An object of class "selectionTable" or "influenceTable", created
    using `RSiena::selectionTable()` or `RSiena::influenceTable()`,
    respectively.

  - quad:
    
    When TRUE (the default), a quadratic function (average and total
    alter) is plotted. Use `quad = FALSE` for similarity effects.

  - separation:
    
    This can be used to make the curves visually distinguishable if they
    overlap too much without it. An advisable value then is, e.g., 0.01.

  - ...:
    
    Other arguments to be passed.

## Value

A plot showing how the selection/influence evaluation function changes
based on ego's value and alter's value of some covariate.

## Details

These functions were originally written by Tom Snijders, and adapted for
use in the `{autograph}` package.

## References

For plotting selection tables, please consult the RSiena manual,
Sections 13.1 and 13.3.

For plotting selection tables, please consult the RSiena manual,
Sections 13.2 and 13.4.

## See also

Other RSiena: `plot_gof`

Other RSiena: `plot_gof`

## Author

Tom Snijders

Thanks to Steffen Triebel and Rene Veenstra for corrections.

## Examples

``` r
plot(siena_selection)
#> Warning: The `base_family` theme element is not defined in the element hierarchy.

plot(siena_influence)
#> Ignoring unknown labels:
#> • linetype : "\"mybeh\" alter value"
#> Warning: The `base_family` theme element is not defined in the element hierarchy.
```
