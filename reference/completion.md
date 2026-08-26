# Completing argument values as you type

[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
and its relatives take the names of node and tie variables, layouts, and
themes as strings, which means remembering what a network holds. This
offers those names to RStudio's completion system, so that writing
`graphr(fict_lotr, node_color = "` and pressing Tab lists the variables
`fict_lotr` holds, `layout = "` lists the layouts available, and so on
for every argument with a known set of values.

This is off until it is asked for, because it works by replacing one of
RStudio's internal functions. That function is not part of a public
interface, so a future version of RStudio can change it. Nothing else
about completion changes: any line that is not one of these calls is
passed to RStudio untouched, as is any line this cannot make sense of.

`stocnet_completion(FALSE)` puts RStudio's function back.

## Usage

``` r
stocnet_completion(activate, persist = FALSE)

set_completion(activate, persist = FALSE)
```

## Arguments

- activate:

  Logical, by default TRUE. If TRUE, completion of argument values is
  switched on. If FALSE, RStudio's own completions are restored. If
  missing, the current state is reported and nothing changes.

- persist:

  Logical, by default FALSE. If TRUE, the choice is remembered across
  sessions, by writing it to the user's configuration directory (see
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)).
  Nothing is written to disk unless this is set explicitly. Use
  `stocnet_completion(persist = FALSE)` when activating to forget a
  previously persisted choice.

## Value

Invisibly, TRUE where completion is now active and FALSE otherwise.
Called for the effect it has on the IDE.

## See also

Other mapping:
[`check_layout`](https://stocnet.github.io/autograph/reference/check_layout.md),
[`layout_concentric()`](https://stocnet.github.io/autograph/reference/layout_concentric.md),
[`layout_configuration()`](https://stocnet.github.io/autograph/reference/layout_configuration.md),
[`layout_correspondence()`](https://stocnet.github.io/autograph/reference/layout_correspondence.md),
[`layout_layered()`](https://stocnet.github.io/autograph/reference/layout_layered.md),
[`layout_levels()`](https://stocnet.github.io/autograph/reference/layout_levels.md),
[`layout_matching()`](https://stocnet.github.io/autograph/reference/layout_matching.md),
[`layout_scaling()`](https://stocnet.github.io/autograph/reference/layout_scaling.md),
[`layout_valence()`](https://stocnet.github.io/autograph/reference/layout_valence.md),
[`plot_graphr`](https://stocnet.github.io/autograph/reference/plot_graphr.md),
[`plot_graphs`](https://stocnet.github.io/autograph/reference/plot_graphs.md),
[`plot_grapht`](https://stocnet.github.io/autograph/reference/plot_grapht.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# In RStudio, switch completion on for this session:
stocnet_completion()
# Then type graphr(fict_lotr, node_color = " and press Tab.
# To switch it off again:
stocnet_completion(FALSE)
} # }
```
