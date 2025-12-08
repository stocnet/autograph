# Changelog

## autograph 0.5.1

### Plotting

  - Improved `plot.gof.ergm()` (closes
    [\#31](https://github.com/stocnet/autograph/issues/31))
      - Now works on directed and two-mode networks and dyadwise shared
        partners
      - Now has more informative error message if statistic not
        available
      - Now accepts more descriptive and gof-formula consistent
        statistic names

## autograph 0.5.0

CRAN release: 2025-11-19

### Package

  - Shortened startup messages

### Plotting

  - Added `plot.goldfish.changepoints()`
  - Added `plot.goldfish.outliers()`
  - Added `plot.mnet()` to avoid `igraph::plot.igraph()` dispatching

### Graphing

  - Restructured scripts for improved maintenance and development in the
    future
  - Fixed `ggplot2::geom_violin()` call in `graphr()` to avoid warnings
    with ggplot2 v4.0.0
  - Improved `graphr()` internals to rely on a NULL “layout” parameter
    and `.infer_layout()`
  - Improved `graphr()` to use `match_color()` for diffusion palettes
  - Fixed `graphs()` handling of changing networks

## autograph 0.4.2

### Package

  - Added ggpizzas to README

### Plotting

  - Fixed node overflow issue and restored marginal density plot in
    `plot.ag_conv()` by dropping `+.ggplot()`
  - Added automatic legend labelling where node\_size or node\_color or
    edge\_color is given

### Theming

  - Added “hwu” theme for Heriot-Watt University
  - Improved highlight and divergent theme setting to use switch
  - Added some discussion about colour-blindedness to palettes
    documentation and viz tute
  - Added some instruction on how to customise themes more specifically

## autograph 0.4.1

CRAN release: 2025-09-11

### Package

  - Improved startup message to ignore class

### Theming

  - Added “iast” theme for the Institute of Advanced Studies, Toulouse

### Plotting

  - Improved ergm convergence diagnostic plotting by working on the
    results object, mostly avoiding startup conflict warning
  - Dropped marginal density plot in `plot.ag_conv()` to fix
    [ggplot2](https://ggplot2.tidyverse.org) v4.0.0-related node
    overflow issue

## autograph 0.4.0

CRAN release: 2025-09-10

### Package

  - Fixed DOI in CITATION
  - Improved README introduction
  - Improved README graphing illustration, including igraph comparison
    figure
  - Improved README plotting illustration, added SAOM/ERGM GOF
    comparison figure

### Theming

  - Improved `stocnet_theme()` documentation
  - Improved `stocnet_theme()` to register a font family for plots
      - A vector of potential fonts is included for some themes
      - The first font found on the system will be used, and user
        notified
      - If no fonts are found, the default R font (“sans”) will be used
      - A message is printed to inform the user if the default is used,
        and how to install missing fonts via
        [extrafont](https://github.com/fbertran/extrafont)
  - Added font options for “iheid”, “oxf”, “ethz”, “uzh”, and “rug”
    themes
  - Added `ag_font()` for retrieving the registered font family
  - Improved `match_colors()` documentation
  - Exported `is_dark()` and made it vectorised
  - Added `match_colors()` and `is_dark()` tests
  - Dropped `theme_*()` functions in favour of `stocnet_theme()`
  - Dropped `theme_*()` tests
  - Added `stocnet_theme()` tests
  - Added “cmu” theme for Carnegie Mellon University

### Graphing

  - Improved `graphr()` by using registered fonts where available
  - Improved `graphr()` by using `ag_qualitative()` for discrete colour
    scales

### Plotting

  - Improved `plot.selectionTable()` and `plot.influenceTable()`
    documentation by consolidating them together into one help file
  - Improved `plot.ag_conv()`, `plot.ag_gof()`, and plot\_interp by
    using registered fonts where available
  - Improved `plot.sienaGOF()` to use lower case auxiliary statistic
    description

### Data

  - Renamed `res_monan_traces` to `monan_conv`
  - Renamed `res_monan_gof` to `monan_gof`
  - Renamed `res_ergm_gof` to `ergm_gof`
  - Renamed `res_siena_gof` to `siena_gof`
  - Renamed `res_siena_influence` to `siena_influence`
  - Renamed `res_siena_selection` to `siena_selection`

## autograph 0.3.1

### Package

  - Fixed old citation style issue for CRAN

### Plotting

  - Added `plot.ag_conv()` as a new plotting method for convergence
    diagnostics
      - Plots MCMC traces as a line plot with loess smoothing
        highlighting the trend
      - Plots overall density plot of the samples on the right margin
  - Improved plotting of `{MoNAn}` trace objects by using
    `plot.ag_conv()`
  - Added `plot.mcmc.list()` for plotting MCMC samples from `ergm::ergm`
    results objects, using `plot.ag_conv()`

### Data

  - Added `ergm_res` for testing and illustration of `plot.mcmc.list()`,
    use `ergm_res$sample` to access the MCMC sample

## autograph 0.3.0

### Package

  - Added package documentation
  - Added citation
  - Dropped dependencies [tidyr](https://tidyr.tidyverse.org),
    [cli](https://cli.r-lib.org), and
    [concaveman](https://joelgombin.github.io/concaveman/)
  - Added more description to the function overview sections on the
    website
  - Added CODECOV\_TOKEN to Github secrets for test coverage reporting

### Graphing

  - Fixed `graphr()` not using theme colours for node and edge
    aesthetics

### Plotting

  - Added new plot class and method for centralising GOF plotting
      - Improved `plot.sienaGOF()` and `plot.gof.stats.monan()` to use
        new plotting method
      - Added `plot.gof.ergm()` for plotting ERGM GOF objects
      - Improved GOF plotting by adding boxplot within the violins
      - Improved GOF plotting by adding crosses for outliers
      - Improved GOF plotting by adding dashed line for 0.05 and 0.95
        quantile bounds
      - Improved GOF plotting by dropping statistics without variance
      - Improved GOF plotting by using `cumulative = FALSE` by default
  - Fixed issue with pre-v1.3.20 RSiena::gof() objects, thanks
    [@TomSnijders](https://github.com/TomSnijders)
  - Added testing of GOF plotting
  - Added testing of measures plotting

### Theming

  - Added `set_stocnet_theme()` alias for `stocnet_theme()`
  - Fixed issue with ‘oxf’ and ‘unige’ themes not being recognised

### Data

  - Added `res_ergm_gof` for testing and illustration of
    `plot.gof.ergm()`

### Tutorial

  - Added visualisation tutorial from manynet
      - Improved introduction
      - Improved section on titles, labels, and legends
      - Improved section explaining base and grid graphics
      - Added section demonstrating difference between `igraph::plot()`,
        `ggraph::ggraph()`, and `graphr()`
      - Added table describing the main arguments of `graphr()` and the
        visualisation dimensions to which they relate
      - Added section showing how to use “node\_shape” (poorly)
      - Improved section showing how to use “node\_colour” and
        “node\_group”
      - Added section showing how to use “node\_size”
      - Added section showing how to set a theme
      - Added section showing how to use “edge\_colour” and “edge\_size”
      - Improved section introducing
        [patchwork](https://patchwork.data-imaginist.com), `graphs()`,
        and `grapht()`
      - Added plotting section to viz tutorial

## autograph 0.2.0

CRAN release: 2025-08-22

### Package

  - Added (currently) necessary dependencies
  - Removed unnecessary manynet dependencies

### Plotting

  - Added `graphr()`, `graphs()`, and `grapht()` from manynet for quick
    plotting of one or more graphs
  - Improved `+.ggplot()` to check whether second object inherits ggplot
    class (thanks [@teunbrand](https://github.com/teunbrand) for fixing
    [\#110](https://github.com/stocnet/autograph/issues/110))
  - Fixed issue with edge\_linetype in signed, directed networks with
    weights (closed
    [\#14](https://github.com/stocnet/autograph/issues/14))
  - Updated README with examples of `graphr()` and others

### Themes

  - Added themes for Oxford and UNIGE (closed
    [\#1](https://github.com/stocnet/autograph/issues/1))
  - Improved `stocnet_theme()` to add option to “stocnet\_theme”
    (previously “snet\_theme”)
  - Added `is_dark()` to check whether a theme is dark or light
  - Added `match_color()` to return closest matching colors from a
    palette

### Layouts

  - Added layout\_valence() for signed graphs (closed
    [\#6](https://github.com/stocnet/autograph/issues/6))
  - Added configurational layouts from manynet
  - Added aliases for various layout\_tbl\_graph\_\*() functions

## autograph 0.1.2

CRAN release: 2025-07-02

### Package

  - Fixed line break issues in DESCRIPTION

### Themes

  - Fixed default for `ag_sequential()`

### Layouts

  - Fixed “layout\_layered” to accept typical ggraph parameters

### manynet

  - Added return value statements

### migraph

  - Added return value statements

### RSiena

  - Added return value statements

### MoNAn

  - Added `plot.traces.monan()`
  - Added `plot.gof.stats.monan()`

## autograph 0.1.1

### Package

  - Fixed DESCRIPTION issues

## autograph 0.1.0

### Package

  - Initial setup, with all the goodies

### Theming

  - Added `stocnet_theme()` for setting a theme that can then be reused
    across successive plots (closes
    [\#3](https://github.com/stocnet/autograph/issues/3))
      - For example, `stocnet_theme("iheid")` for IHEID colour theme
  - Added `ag_*()` palettes, for example:
      - `ag_highlight()` to select the highlight colours of a particular
        palette
  - Added `match_color()` for matching one or more hexcodes to a
    palette’s offerings

### Plotting

  - Added manynet plot methods
      - Added `plot.diff_model()`
      - Added `plot.matrix()`
      - Added `plot.learn_model()`
      - Added `plot.network_measures()`
      - Added `plot.network_motif()`
      - Added `plot.tie_measure()`
      - Added `plot.node_measure()`
      - Added `plot.node_member()`
      - Added `plot.node_members()`
      - Added `plot.node_motif()`
  - Added migraph plot methods
      - Added `plot.netlm()`
      - Added `plot.netlogit()`
      - Added `plot.network_test()`
      - Added `plot.diffs_model()`
  - Added RSiena plot methods
      - Added `plot.sienaGOF()`
      - Added `plot.influenceTable()`
      - Added `plot.selectionTable()`

### Layouts

  - Added `layout_tbl_graph_matching()` for positioning nodes according
    to their `manynet::to_matching()` partners
  - Added first version of `layout_tbl_graph_layered()` for layering
    nodes according to a Sugiyama-type layout

### Data

  - Added some precooked results for testing and illustration
