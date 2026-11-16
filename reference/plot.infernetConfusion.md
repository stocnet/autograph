# Plotting how well a model classifies ties

This plotting method is for the result of
`infernet::diagnose_confusion()`, which compares the ties that a model
of a binary outcome predicts with the ties that were observed. The S3
class is "infernetConfusion".

- The ROC curve gives the true positive rate against the false positive
  rate at every threshold. The diagonal is a model that guesses, and the
  area under the curve (AUC) is in the title.

- The confusion matrix gives the mean count of each cell over the draws
  of the predicted probabilities, with its 95% interval.

- The metrics give the accuracy, sensitivity, specificity, precision,
  and F1 on one scale from 0 to 1.

## Usage

``` r
# S3 method for class 'infernetConfusion'
plot(x, panel = c("all", "roc", "confusion", "metrics"), ...)
```

## Arguments

- x:

  An object of class "infernetConfusion". For example,
  `infernet::diagnose_confusion()`.

- panel:

  By default "all", which shows the three panels together. "roc",
  "confusion", or "metrics" shows one panel.

- ...:

  Further arguments to be passed on to plot.

## Value

A ggplot2 object, or a patchwork of three of them.

## Examples

``` r
# Needs 'infernet' (https://github.com/stocnet/infernet)
if (FALSE) { # \dontrun{
  fit <- infernet::estimate_mrqap(. ~ ego(Age) + alter(Age),
                                  manynet::ison_adolescents |>
                                    manynet::mutate_nodes(Age = 12:19),
                                  times = 20, control = list(seed = 1))
  plot(infernet::diagnose_confusion(fit, times = 100))
} # }
```
