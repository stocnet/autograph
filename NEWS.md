# autograph 0.4.0

## Package

- Fixed DOI in CITATION
- Improved README introduction
- Improved README graphing illustration, including igraph comparison figure
- Improved README plotting illustration, added SAOM/ERGM GOF comparison figure

## Theming

- Improved `stocnet_theme()` documentation
- Improved `stocnet_theme()` to register a font family for plots
  - A vector of potential fonts is included for some themes
  - The first font found on the system will be used, and user notified
  - If no fonts are found, the default R font ("sans") will be used
  - A message is printed to inform the user if the default is used,
  and how to install missing fonts via `{extrafont}`
- Added font options for "iheid", "oxf", "ethz", "uzh", and "rug" themes
- Added `ag_font()` for retrieving the registered font family
- Improved `match_colors()` documentation
- Exported `is_dark()` and made it vectorised
- Added `match_colors()` and `is_dark()` tests
- Dropped `theme_*()` functions in favour of `stocnet_theme()`
- Dropped `theme_*()` tests
- Added `stocnet_theme()` tests
- Added "cmu" theme for Carnegie Mellon University

## Graphing

- Improved `graphr()` by using registered fonts where available
- Improved `graphr()` by using `ag_qualitative()` for discrete colour scales

## Plotting

- Improved `plot.selectionTable()` and `plot.influenceTable()` documentation
by consolidating them together into one help file
- Improved `plot.ag_conv()`, `plot.ag_gof()`, and plot_interp by using 
registered fonts where available
- Improved `plot.sienaGOF()` to use lower case auxiliary statistic description

## Data

- Renamed `res_monan_traces` to `monan_conv`
- Renamed `res_monan_gof` to `monan_gof`
- Renamed `res_ergm_gof` to `ergm_gof`
- Renamed `res_siena_gof` to `siena_gof`
- Renamed `res_siena_influence` to `siena_influence`
- Renamed `res_siena_selection` to `siena_selection`

# autograph 0.3.1

## Package

- Fixed old citation style issue for CRAN

## Plotting

- Added `plot.ag_conv()` as a new plotting method for convergence diagnostics
  - Plots MCMC traces as a line plot with loess smoothing highlighting the trend
  - Plots overall density plot of the samples on the right margin
- Improved plotting of `{MoNAn}` trace objects by using `plot.ag_conv()`
- Added `plot.mcmc.list()` for plotting MCMC samples from `ergm::ergm` results objects,
using `plot.ag_conv()`

## Data

- Added `ergm_res` for testing and illustration of `plot.mcmc.list()`,
use `ergm_res$sample` to access the MCMC sample

# autograph 0.3.0

## Package

- Added package documentation
- Added citation
- Dropped dependencies `{tidyr}`, `{cli}`, and `{concaveman}`
- Added more description to the function overview sections on the website
- Added CODECOV_TOKEN to Github secrets for test coverage reporting

## Graphing

- Fixed `graphr()` not using theme colours for node and edge aesthetics

## Plotting

- Added new plot class and method for centralising GOF plotting
  - Improved `plot.sienaGOF()` and `plot.gof.stats.monan()` to use new plotting method
  - Added `plot.gof.ergm()` for plotting ERGM GOF objects
  - Improved GOF plotting by adding boxplot within the violins
  - Improved GOF plotting by adding crosses for outliers
  - Improved GOF plotting by adding dashed line for 0.05 and 0.95 quantile bounds
  - Improved GOF plotting by dropping statistics without variance
  - Improved GOF plotting by using `cumulative = FALSE` by default
- Fixed issue with pre-v1.3.20 RSiena::gof() objects, thanks @TomSnijders
- Added testing of GOF plotting
- Added testing of measures plotting

## Theming

- Added `set_stocnet_theme()` alias for `stocnet_theme()`
- Fixed issue with 'oxf' and 'unige' themes not being recognised

## Data

- Added `res_ergm_gof` for testing and illustration of `plot.gof.ergm()`

## Tutorial

- Added visualisation tutorial from manynet
  - Improved introduction
  - Improved section on titles, labels, and legends
  - Improved section explaining base and grid graphics
  - Added section demonstrating difference between `igraph::plot()`, `ggraph::ggraph()`, and `graphr()`
  - Added table describing the main arguments of `graphr()` and the visualisation dimensions to which they relate
  - Added section showing how to use "node_shape" (poorly)
  - Improved section showing how to use "node_colour" and "node_group"
  - Added section showing how to use "node_size"
  - Added section showing how to set a theme
  - Added section showing how to use "edge_colour" and "edge_size"
  - Improved section introducing `{patchwork}`, `graphs()`, and `grapht()`
  - Added plotting section to viz tutorial

# autograph 0.2.0

## Package

- Added (currently) necessary dependencies
- Removed unnecessary manynet dependencies

## Plotting

- Added `graphr()`, `graphs()`, and `grapht()` from manynet for quick plotting of one or more graphs
- Improved `+.ggplot()` to check whether second object inherits ggplot class (thanks @teunbrand for fixing #110)
- Fixed issue with edge_linetype in signed, directed networks with weights (closed #14)
- Updated README with examples of `graphr()` and others

## Themes

- Added themes for Oxford and UNIGE (closed #1)
- Improved `stocnet_theme()` to add option to "stocnet_theme" (previously "snet_theme")
- Added `is_dark()` to check whether a theme is dark or light
- Added `match_color()` to return closest matching colors from a palette

## Layouts

- Added layout_valence() for signed graphs (closed #6)
- Added configurational layouts from manynet
- Added aliases for various layout_tbl_graph_*() functions

# autograph 0.1.2

## Package

- Fixed line break issues in DESCRIPTION

## Themes

- Fixed default for `ag_sequential()`

## Layouts

- Fixed "layout_layered" to accept typical ggraph parameters

## manynet

- Added return value statements

## migraph

- Added return value statements

## RSiena

- Added return value statements

## MoNAn

- Added `plot.traces.monan()`
- Added `plot.gof.stats.monan()`

# autograph 0.1.1

## Package

- Fixed DESCRIPTION issues

# autograph 0.1.0

## Package

- Initial setup, with all the goodies

## Theming

- Added `stocnet_theme()` for setting a theme that can then be reused across
successive plots (closes #3)
  - For example, `stocnet_theme("iheid")` for IHEID colour theme
- Added `ag_*()` palettes, for example:
  - `ag_highlight()` to select the highlight colours of a particular palette
- Added `match_color()` for matching one or more hexcodes to a palette's
offerings

## Plotting

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

## Layouts

- Added `layout_tbl_graph_matching()` for positioning nodes according to their
`manynet::to_matching()` partners
- Added first version of `layout_tbl_graph_layered()` for layering nodes
according to a Sugiyama-type layout

## Data

- Added some precooked results for testing and illustration
