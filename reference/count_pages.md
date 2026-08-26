# How many pages a paged diagnostic figure has

The page count of
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on a per-term
diagnostic, derivable **without rendering** so a loop can write every
page.

A method that only discovers it is on the last page once it gets there
cannot be scripted, and scripting is the case this exists for: fits go
to a cluster, so a figure has to be producible with nobody at a screen
to press return.

## Usage

``` r
count_pages(x, nrow = 2, ncol = 2)
```

## Arguments

- x:

  a diagnostic object with one panel per term – as returned by
  `test_gof()`, `test_time()`, `diagnose_onset()`, or a fitted goldfish
  model.

- nrow, ncol:

  panels per page, matching what will be passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

A single integer, at least 1.

## Examples

``` r
count_pages(goldfish_gof)
#> [1] 1
```
