# Checking how well a layout draws a network

These functions score a drawing rather than the network it draws, so
that one layout can be compared with another on the same network. Each
measures one of the qualities a layout algorithm may set out to
optimise.

`check_crossings()` reports how many other ties each tie crosses. A
drawing with no crossings at all is a planar drawing.

`check_slopes()` reports the slope each tie is drawn at. The number of
distinct slopes is the slope number of the drawing, and a drawing of few
slopes reads as an orderly one.

`check_lengths()` reports how long each tie is drawn, as a share of the
diagonal of the drawing.

`check_angles()` reports the smallest angle between the ties that meet
at each node. The smallest of these is the angular resolution of the
drawing, and a wide angle is what keeps two ties from reading as one.

`check_stress()` reports how far the distances drawn depart from the
distances through the network. A layout that draws two nodes twice as
far apart as two others should be drawing a path twice as long.

`check_span()` reports how many rows of nodes each tie crosses. A
layered layout should send most ties to the next row down, and a long
tie is one that skips rows to get where it is going.

`check_offset()` reports how far each tie travels sideways, as a share
of the width of the whole drawing. A tie that drops straight down scores
zero.

`check_drawing()` runs every check above except the two for rows, and
returns their headline numbers in one row.

## Usage

``` r
check_span(x)

check_offset(x)

check_stress(x)

check_crossings(x, max_full = 2000L)

check_slopes(x, tolerance = 1)

check_lengths(x)

check_angles(x)

check_drawing(x)
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

- max_full:

  The largest number of ties to compare in full. By default 2000.

- tolerance:

  How many degrees apart two slopes must be to count as two. By default
  1.

## Value

`check_crossings()` returns one whole number for each tie, being how
many other ties it crosses, with `total` and `mean` attributes, and a
`sampled` attribute saying whether the ties were sampled.

`check_slopes()` returns one angle in degrees for each tie, between 0
and 180, with `distinct` and `tolerance` attributes.

`check_lengths()` returns one number between 0 and 1 for each tie, with
`total`, `max`, `mean`, `variance` and `cv` attributes.

`check_angles()` returns one angle in degrees for each node, with `min`,
`mean` and `ideal` attributes.

Each of those returns `NA` for a loop, or for a node with fewer than two
ties.

`check_stress()` returns a single number of 0 or more, with a `scale`
attribute holding the factor the drawn distances were scaled by, and a
`pairs` attribute holding how many pairs were scored.

`check_span()` returns one whole number for each tie, with `total` and
`mean` attributes holding the sum and the average.

`check_offset()` returns one number between 0 and 1 for each tie, with a
`mean` attribute.

`check_drawing()` returns a one row data frame of class "check_drawing",
holding the headline number from each check. Printing it names the
direction each column is read in.

## Reading the scores

`check_drawing()` reports one number from each check, and the columns
are not all read the same way. Every column is better small except
`angle_min`, which is better large. `nodes`, `ties` and `angle_ideal`
are not scores at all: the first two say what was drawn, so that two
rows of one network can be told from two rows of different ones, and
`angle_ideal` is the ceiling that `angle_min` is read against. No layout
wins on every column. A layout is chosen by which columns the reader of
the figure needs, rather than by how many of them it wins.

Each column is read as follows.

- `crossings` counts the pairs of ties that cross, so fewer is better,
  and zero is a planar drawing. Zero is not always available: a network
  of more than three times its nodes, less six, ties cannot be drawn on
  a plane without a crossing, and a two-mode network of more than twice
  its nodes, less four, ties cannot either. Where that floor is above
  zero, read the score against another layout of the same network rather
  than against zero.

- `slopes` counts the directions the ties are drawn in, so fewer is
  better. The fewest any drawing can use is half the largest degree,
  rounded up, since the ties at the busiest node need that many
  directions to leave it by.

- `length_total`, `length_max` and `length_cv` are all better small.
  Each length is a share of the diagonal, so it runs between 0 and 1,
  and 1 is a tie drawn corner to corner. A `length_cv` of 0 means every
  tie is drawn the same length. The total grows with the number of ties,
  so compare two drawings of one network by their total, and two
  networks by the `mean` attribute of `check_lengths()`.

- `angle_min` is the angular resolution in degrees, and is better large.
  Read it as a share of `angle_ideal`, which is 360 degrees divided by
  the largest degree, and is the best any drawing of that network could
  do.

- `stress` is Kruskal's stress-1, so 0 is a perfect drawing. Kruskal
  read 20% as poor, 10% as fair, 5% as good, and 2.5% as excellent.
  Those figures were set for psychometric data rather than for networks,
  which are harder: most pairs of nodes in a small-world network sit two
  or three steps apart, and a plane holds few such distances at once, so
  a score near 30% is ordinary and one near 5% is rare. A layout that
  never set out to draw path distances, such as "layered", "circle" or
  "configuration", scores poorly by design.

`check_span()` and `check_offset()` are left out of the row, since they
measure rows of nodes and so only mean something for a layered layout.
Run them beside it where the layout has rows. Both are better small.
They answer different questions, and a layered layout needs both
answered: `check_span()` asks whether the rows were well chosen, and
`check_offset()` asks whether the nodes were well placed within them.
The "layered" layout minimises each in turn, and its `ranks` and
`alignment` arguments choose how.

The scores belong to the drawing rather than to the network, which is
what separates `check_stress()` from the share of distance variance that
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
reports beside it. Draw one network two ways and the stress changes,
since one drawing holds its distances better than the other; the share
of variance does not, since two dimensions can hold just as much of that
network either way. A network whose variance is held poorly sets a floor
that no layout gets under.

## How the drawing is measured

Every check reads the straight line between two nodes.
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
may draw a tie as an arc, a fan, or a bundle, in which case the score is
an approximation of what is drawn. A loop is left out, since it joins a
node to itself and so has neither direction nor length on the plane.

`check_crossings()` counts a proper crossing, where two ties meet away
from their ends. Two ties that share a node are not counted, since they
must meet there. Comparing every pair of ties costs the square of their
number, so above `max_full` ties an evenly spaced sample of the ties is
scored instead, and the `sampled` attribute records this. The sample is
taken by position rather than at random, so that the same drawing scores
the same on every call.

`check_slopes()` reads a slope as an angle between 0 and 180 degrees,
since a tie drawn from left to right has the same slope as the same tie
drawn from right to left. Two angles closer than `tolerance` are counted
as one slope, because two lines that differ by a fraction of a degree
read as parallel.

`check_lengths()` divides by the diagonal of the drawing, since a layout
may place its nodes on any scale it likes, and two layouts can only be
compared once both are on the same one. The spread is reported twice,
since the two answer different questions: `variance` is on the scale of
the drawing, and `cv` divides by the mean, so only `cv` compares a
drawing of long ties with a drawing of short ones.

`check_angles()` compares the directions of the ties at a node, so a
node with fewer than two ties scores `NA`. Two ties drawn one on top of
the other count once, since a multiplex pair is drawn apart by
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
rather than at the same angle.

`check_stress()` scales the drawn distances to the path distances before
it compares them, since a layout may place its nodes on any scale, and
the ties are counted unweighted, as
[`layout_scaling()`](https://stocnet.github.io/autograph/reference/layout_scaling.md)
counts them. Where a network is disconnected, the pairs with no path
between them are left out of the score.

`check_span()` and `check_offset()` read the rows from the plot, as the
axis on which the nodes take fewer distinct positions. This is the y
axis for "layered" and the x axis for "lineage", so the same score can
be compared across the two. For a layout with no rows at all, such as
"stress", `check_span()` reports the distance in that axis' ranks, which
is not meaningful; the two are for layered layouts.

## See also

[`check_colors()`](https://stocnet.github.io/autograph/reference/check_colors.md),
which scores a palette rather than a drawing.

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
sw <- manynet::ison_southern_women
# Every check at once, for two layouts side by side
circled <- graphr(sw, layout = "circle")
rbind(stress = check_drawing(graphr(sw, layout = "stress")),
      circle = check_drawing(circled))
#>        nodes ties crossings slopes length_total length_max length_cv angle_min
#> stress    32   89       197     71       18.921      0.297     0.218     0.193
#> circle    32   89      2652     29       58.537      0.707     0.096     5.625
#>        angle_ideal stress
#> stress      25.714  0.286
#> circle      25.714  0.516
#> # Lower is better, except angle_min, where higher is better.
#> # nodes, ties and angle_ideal are context, not scores.
# How many crossings does the default layout draw?
attr(check_crossings(graphr(sw)), "total")
#> [1] 619
# How many slopes does a circle draw, and how evenly does it draw its ties?
attr(check_slopes(circled), "distinct")
#> [1] 29
attr(check_lengths(circled), "cv")
#> [1] 0.09550886
# Is there room between the ties that meet at a node?
attr(check_angles(circled), "min")
#> [1] 5.625
# And where the layout has rows, how long and how straight are its ties?
thrones <- manynet::to_uniplex(manynet::fict_thrones, "parent")
drawn <- graphr(thrones)
attr(check_span(drawn), "total")
#> [1] 270
attr(check_offset(drawn), "mean")
#> [1] 0.03420039
```
