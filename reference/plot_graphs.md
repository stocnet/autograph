# Easily graph a set of networks with sensible defaults

This function provides users with an easy way to graph lists of network
data for comparison.

It builds upon this package's `graphr()` function, and inherits all the
same features and arguments. See `graphr()` for more. However, it uses
the `{patchwork}` package to plot the graphs side by side and, if
necessary, in successive rows. This is useful for lists of networks that
represent, for example, ego or component subgraphs of a network, or a
list of a network's different types of tie or across time. By default
just the first and last network will be plotted, but this can be
overridden by the "waves" parameter.

Where the graphs are of the same network (same nodes), the graphs may
share a layout to facilitate comparison. By default, successive graphs
will use the layout calculated for the "first" network, but other
options include the "last" layout, or a mix, "both", of them.

## Usage

``` r
graphs(netlist, waves, based_on = c("first", "last", "both"), ...)
```

## Arguments

  - netlist:
    
    A list of manynet-compatible networks.

  - waves:
    
    Numeric, the number of plots to be displayed side-by-side. If
    missing, the number of plots will be reduced to the first and last
    when there are more than four plots. This argument can also be
    passed a vector selecting the waves to plot.

  - based\_on:
    
    Whether the layout of the joint plots should be based on the "first"
    or the "last" network, or "both".

  - ...:
    
    Additional arguments passed to `graphr()`.

## Value

Multiple `ggplot2::ggplot()` objects displayed side-by-side.

## See also

Other mapping: `layout_configuration()`, `layout_partition`,
`plot_graphr`, `plot_grapht`

## Examples

``` r
#graphs(to_egos(ison_adolescents))
#graphs(to_egos(ison_adolescents), waves = 8)
#graphs(to_egos(ison_adolescents), waves = c(2, 4, 6))
#graphs(play_diffusion(ison_adolescents))
```
