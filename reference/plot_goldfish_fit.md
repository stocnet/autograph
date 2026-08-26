# Plotting a goldfish model fit at a glance

One call, four diagnostic panels: whether any interval is badly fitted,
whether any coefficient drifts, whether each effect's contribution is
spread over the sequence, and whether the waiting times are what the
model says they are.

## Usage

``` r
# S3 method for class 'result.goldfish'
plot(x, ..., effects = 4)

# S3 method for class 'goldfishFit'
plot(x, ..., effects = 4)
```

## Arguments

- x:

  A fitted model of class `goldfishFit`.

- ...:

  Additional plotting parameters, currently unused.

- effects:

  The number of effects to draw in the Schoenfeld panel.

## Value

A patchwork composition of the available panels.

## Details

`plot.result.goldfish()` is an alias for `plot.goldfishFit()`, kept so
that a fit from a goldfish that still stamps the old class name plots as
before. It will be removed.

Everything is drawn from what the **fit already stores** — no evaluation
pass and no preprocessed statistics — so the figure costs a plot and not
a re-fit. The consequence is that a panel needing a primitive the fit
did not store is **left out** rather than erroring: which panels appear
is itself a readout of what was requested at estimation.

- deviance:

  the per-interval log-likelihood with outlying intervals marked. Needs
  the `"loglik"` primitive.

- scaled Schoenfeld:

  a smooth per effect against the fitted estimate, flat under a constant
  coefficient. Needs `"scores"` on a multinomial sub-model, and
  `"conditional_scores"` on an exact-time one, where the score carries
  an exposure term the Schoenfeld residual does not.

- cumulative score:

  each effect's standardized process against its Brownian-bridge band.
  Needs `"scores"`.

- waiting times:

  the Cox-Snell residuals against the unit exponential they follow under
  the model. Exact-time sub-models only: an ordinal likelihood
  conditions the timing away, so there is no waiting time to check.

The Schoenfeld panel is capped at the `effects` most worth looking at,
ranked by their cumulative-score statistic, since a model with a dozen
terms makes a facet grid unreadable at overview size.

## Examples

``` r
plot(goldfish_fit)
#> This fit stores no diagnostic primitive to plot.
```
