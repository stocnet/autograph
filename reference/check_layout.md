# Checking how well a layout draws its ties

These functions score a drawing rather than the network it draws, so
that a layout can be compared with another on the same network.

`check_span()` reports how many rows of nodes each tie crosses. A
layered layout should send most ties to the next row down, and a long
tie is one that skips rows to get where it is going.

`check_offset()` reports how far each tie travels sideways, as a share
of the width of the whole drawing. A tie that drops straight down scores
zero.

`check_stress()` reports how far the distances drawn depart from the
distances through the network. A layout that draws two nodes twice as
far apart as two others should be drawing a path twice as long.

## Usage

``` r
check_span(x)

check_offset(x)

check_stress(x)
```

## Source

Kruskal, Joseph B. 1964. "Multidimensional scaling by optimizing
goodness of fit to a nonmetric hypothesis", *Psychometrika* 29(1): 1-27.
[doi:10.1007/BF02289565](https://doi.org/10.1007/BF02289565)

## Arguments

- x:

  A plot, as
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  returns.

## Value

`check_span()` returns one whole number for each tie, with `total` and
`mean` attributes holding the sum and the average.

`check_offset()` returns one number between 0 and 1 for each tie, with a
`mean` attribute.

`check_stress()` returns a single number of 0 or more, with a `scale`
attribute holding the factor the drawn distances were scaled by, and a
`pairs` attribute holding how many pairs were scored.

## Details

`check_span()` and `check_offset()` answer different questions, and a
layered layout needs both answered. `check_span()` asks whether the rows
were well chosen, and `check_offset()` asks whether the nodes were well
placed within them. The "layered" layout minimises each in turn, and its
`ranks` and `alignment` arguments choose how.

Which axis holds the rows is read from the plot, as the axis on which
the nodes take fewer distinct positions. This is the y axis for
"layered" and the x axis for "lineage", so the same score can be
compared across the two. For a layout with no rows at all, such as
"stress", `check_span()` reports the distance in that axis' ranks, which
is not meaningful; the function is for layered layouts.

`check_stress()` applies to any layout, since every layout draws its
nodes some distance apart, and the score is the share of the path
distances that the drawn distances get wrong. It is Kruskal's stress-1,
so 0 is a perfect drawing, and Kruskal read 20% as poor, 10% as fair, 5%
as good, and 2.5% as excellent. Those figures were set for psychometric
data rather than for networks, which are harder: most pairs of nodes in
a small-world network sit two or three steps apart, and a plane holds
few such distances at once, so a score near 30% is ordinary and one near
5% is rare. A layout that never set out to draw path distances, such as
"layered", "circle" or "configuration", scores poorly by design.

The score belongs to the drawing rather than to the network, which is
what separates it from the share of distance variance that
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
reports beside it. Draw one network two ways and the stress changes,
since one drawing holds its distances better than the other; the share
of variance does not, since two dimensions can hold just as much of that
network either way. A network whose variance is held poorly sets a floor
that no layout gets under.

The drawn distances are scaled to the path distances before they are
compared, since a layout may place its nodes on any scale it likes, and
the ties are counted unweighted, as
[`layout_scaling()`](https://stocnet.github.io/autograph/reference/layout_scaling.md)
counts them. Where a network is disconnected, the pairs with no path
between them are left out of the score.

## See also

Other mapping:
[`completion`](https://stocnet.github.io/autograph/reference/completion.md),
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
thrones <- manynet::to_uniplex(manynet::fict_thrones, "parent")
# The default graph is drawn once here, since each check reads the same plot.
drawn <- graphr(thrones)
# How long are the ties of the default layout?
attr(check_span(drawn), "total")
#> [1] 270
# How straight are they?
attr(check_offset(drawn), "mean")
#> [1] 0.03420039
# Compare with the layers igraph would have chosen:
# attr(check_span(graphr(thrones, ranks = "compact")), "total")
# Which layout draws the path distances best?
check_stress(graphr(manynet::ison_southern_women, layout = "scaling"))
#> [1] 0.3129352
#> attr(,"scale")
#> [1] 1.146146
#> attr(,"pairs")
#> [1] 992
check_stress(graphr(manynet::ison_southern_women, layout = "circle"))
#> [1] 0.5157822
#> attr(,"scale")
#> [1] 1.46639
#> attr(,"pairs")
#> [1] 992
```
