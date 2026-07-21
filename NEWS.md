# autograph 1.1.1

## Graphing

- Fixed `grapht()` example, which took too long to run, by couching it in `\donttest{}`
- Improved `graphs()` to accept bare longitudinal or dynamic networks
  - Splits it into waves or time slices automatically (consistent with `grapht()`), 
  instead of erroring with "invalid to use names()<- on an S4 object of class 'dgCMatrix'"

# autograph 1.1.0

## Package

- Test coverage raised by introducing functional testing infrastructure (`tests/testthat/helper-functional.R` and `test-functional_*.R`) for:
  - the layout family
  - the `plot.*` S3 method family
  - the `ag_*` palette accessors across all themes
  - `graphr()`'s aesthetic arguments each enumerated automatically and audited against fixture grids
- Removed the unused internal helper `seq_nodes()`
- Excluded the interactive-only palette helper `ggpizza()` from coverage
- Added `{migraph}` to Suggests (used in tests only)

## Graphing

- Improved how `graphr()` treats labels
  - Fixed labels overlapping nodes (closes #13): labels now keep clear of node borders automatically by giving ggrepel each node's true rendered size, with `label_dist` adding a further points-based gap (mirroring igraph's `vertex.label.dist`) and `label_repel = FALSE` selecting a fixed offset instead of repulsion. 
  - This also fixed a pre-existing bug where non-repelled labels rendered with a fully transparent fill under this package's theme, making them invisible over nodes. 
- Improved `grapht()` has been rewritten for smoother, more consistent animations of dynamic networks
  - Node positions now transition seamlessly between waves using the dynamic stress layout
    from `{graphlayouts}` (`layout_as_dynamic()`), with a new `alpha` argument controlling
    layout stability; other layouts are computed once on the aggregate network and held fixed
  - Changing node composition is now handled properly: every node that ever appears gets a
    stable position and fades in and out in place as it enters and exits the network
  - New `isolates` argument (`"keep"` or `"fade"`) controls whether temporarily isolated nodes
    stay visible or fade out; `keep_isolates` is deprecated
  - Dynamic (time-stamped, event-based) networks such as `irps_nuclear` are now split
    automatically into cumulative time slices via `manynet::to_slices()`, so a single
    dynamic network object passed to `grapht()` works without manual conversion
  - Interval (spell) networks that record tie `begin`/`end` lifespans, such as `irps_wwi`,
    are now split automatically into one snapshot per change point showing the ties active
    in that spell, so `grapht(irps_wwi)` works directly (previously it errored because such
    networks are dynamic but carry no `time` attribute for `to_slices()`); `irps_wwi` is now
    a runnable example in the documentation
  - `grapht()` now uses the dynamic stress layout by default even for two-mode networks
    (rather than a hierarchy layout, which collapsed many nodes onto a line), suppresses
    node labels by default for networks with more than 30 nodes to keep frames legible,
    and fades densely overlapping ties so they read as a density gradient rather than a
    solid mass
  - Fixed an error when animating networks whose node names contain non-ASCII characters
  - Waves without any ties are no longer silently dropped
  - Closer visual parity with `graphr()`: directed networks get arrowheads on segments trimmed
    at the target node, signed networks distinguish positive/negative ties by linetype and
    colour, mapped aesthetics use the same palettes with factor levels consistent across
    frames, and legends transition along with the animation
  - Aesthetic-resolution helpers are now shared between `graphr()` and `grapht()`
    (new R/graph_aes.R), so styling cannot drift between static and animated plots
  - Added a test suite for `grapht()` (no gif rendering required)
  - Now aborts with a clear message when its input cannot be split into waves or slices, instead of failing much later with a cryptic igraph error (closes #40); the underlying cause — `to_waves()` silently ignoring a time attribute not named "wave" — will be fixed in `{manynet}` 2.2.2, and the tutorial example now uses a `wave` attribute, which splits correctly with `{manynet}` 2.2.1
- Added an `edge_bundle` argument to `graphr()` for bundling edges in dense networks (closes #19): 
  - `TRUE`/`"force"` uses force-directed bundling, with `"path"` and `"minimal"` selecting the other non-hierarchical algorithms
  - colour/width/linetype mappings are preserved and directed networks keep their arrowheads. 
  - This wires up ggraph's non-hierarchical bundling geoms (added in ggraph 2.2.0), which were previously imported but never called, so the ggraph dependency is now `(>= 2.2.0)`
- Fixed `edge_size = 0` not fully suppressing edges on directed networks (closes #50): arrowhead length was hard-coded regardless of `edge_size`, leaving a visible arrowhead when the line was hidden. Arrow length now scales with the resolved edge width (capped so heavily-weighted edges don't get oversized heads) and is omitted entirely when the width is 0
- Fixed two-mode auto-shapes assigning circles to the second mode: the first mode now takes circles and the second squares, as intended
- Fixed `graphr()` returning an empty plot for networks consisting only of isolates (e.g. the empty dyad/triad motifs): isolates are now kept whenever removing them would empty the graph
- Fixed `graphs()` erroring on lists containing tie-less networks (e.g. `plot()` on motif censuses): panels sharing a layout now keep isolates so every node has a coordinate in every wave
- Fixed `graphr()` erroring on weight or size attributes carrying measure classes (e.g. `tie_measure` results from `{netrics}` stored as attributes)
- Fixed a vector-recycling warning in `graphs()`' ego-network detection

## Tutorials

- Fixed the "Tying up loose ends" exercise in the visualisation tutorial erroring on `tie_closeness()` (closes #39): the tutorial now loads `{netrics}` and uses its measure functions (`tie_by_closeness()`, `tie_is_triangular()`), and every tutorial code chunk is now exercised by the functional tests below
- Reworked the "Visualising Networks" tutorial to match the structure and features of the `{manynet}` v2.2 tutorials
  - Rebranded the tutorial in autograph red, with larger, more readable text and matching 'Run code' buttons
  - Added a checkbox Aims section, "Catching up", "Going further", "Beginner note", and "In brief" callout boxes, per-page mini-tables of contents, and free play sections with a choose-your-own-data difficulty ladder
  - Added hover-over glossary terms throughout and a closing Summary section with a function overview table and glossary
  - Added quiz questions with feedback, and hints for the coding exercises
  - New coverage of `edge_bundle`, `label_repel`/`label_dist`, the `isolates` argument, `snap` grid-snapping, autograph's own special-purpose layouts, and programmatic export with `ggsave()`
  - New sections on directed networks (automatic, width-scaled arrowheads and manual control via `edge_size`), automatic mode shapes in two-mode networks, and manually adjusting a layout's coordinate table before passing it back via `x`/`y`
  - Added artist-themed gifs throughout, including as quiz-answer feedback
- Added a static, read-only version of the tutorial as a pkgdown article ("Tutorials" menu on the website), as in `{manynet}`
- Added functional testing of all tutorial code chunks (`tests/testthat/test-tutorials_autograph.R`), mirroring `{manynet}`'s tutorial testing infrastructure

## Layouts

- Fixed `layout_tbl_graph_layered()` ordering nodes by the names rather than the positions of their neighbours in adjacent layers, which degraded every barycenter sweep to NA and raised warnings
- Replaced deprecated `dplyr::case_match()` with `dplyr::recode_values()`

## Plotting

- Fixed `plot.matrix()` erroring when no `membership` argument was supplied, for both one-mode and two-mode matrices

# autograph 1.0.3

## Plotting

- Fixed the error produced by a namespace reference in a call in `ergm_res` by serializing it, moving to extdata, and loading it via `load_ergm_res()`

# autograph 1.0.2

## Plotting

- Fixed the error produced by a namespace reference in a call in `ergm_res`

# autograph 1.0.1

## Package

- Updated the logo

## Plotting

- Fixed the error in some flavours by removing environment references in included objects
- Closed #44 by adding example that uses `ggplot2::scale_colour_discrete()` to tweak colour output

## Tutorials

- Fixed the tutorial to use netrics functions

# autograph 1.0.0

## Package

- Updated startup message to declare the set theme

## Graphing

- Improved `node_shape` to accept more categories
- Moved `node_is_isolate` and `node_adoption_time` to internal helpers to reduce dependencies
- Fixed #47 by overriding shape to fillable (21)
- Fixed #10 and #52 by adding label distance parameter
- Fixed #17 and #40 by improving `grapht()`

## Plotting

- Closed #37 by adding EgoAlter sienaGOF plot
- Fixed #36 by using `.to_factor()` in `plot.ag_gof()` to correct numeric ordering of statistics, and dropped cumulative correction in `plot.sienaGOF()`
- Fixed #38 by dropping linetype in favour of colours even for bw theme in `plot.influenceTable()`
- Fixed option reference from `snet_theme` to `stocnet_theme` in `plot.selectionTable()` and `plot.influenceTable()`
- Applied `ag_font()` to migraph model plots for consistent typography
- Reorganised plot scripts by purpose: summaries, tests, and analysis
- Fixed #46 by not appending "X" to the names when creating the data frame and moving factor coercion later in the process

## Tutorial

- Fixed #41 by updating tutorials to use fill aesthetics (`scale_fill_*()`) instead of colour aesthetics (`scale_colour_*()`)

# autograph 0.6.0

## Package

- Updated logo

## Graphing

- `graphr()` now auto-dispatches to `graphs()` when passed a list of graphs
- `graphs()` no longer requires `{patchwork}` to be loaded separately
- Closed #12 by changing node aesthetics from `color` to `fill` using fillable shape codes (21–25) for nodes to support fill-based colour scales
- Consolidated legend modifications into internal `graph_legends()` helper
- Improved legend labelling:
  - edge sign legend now labelled "Sign"
  - edge weight/width now labelled "Weight"/"Width"
  - node mode/shape legend now labelled "Mode"/"Shape"
- Closed #8 by adding "isolates" argument to `graphr()`:
  - `"legend"` (default) removes isolates from the graph but notes them in the legend
  - `"caption"` removes isolates from the graph but notes them in the caption
  - `"keep"` retains isolates in the graph as-is (closes #12)
  
## Theming

- Removed `scale_*()` functions as redundant with better theme support and `match_color()`

## Layouts

- Replaced `{Rgraphviz}` (Bioconductor) with `igraph::layout_with_sugiyama` for Sugiyama/hierarchy layouts, removing the Bioconductor dependency
- Improved Sugiyama layout with dummy node insertion and barycenter crossing minimisation for better edge routing
- Fixed #18 for lattice layout snapping by rotating the layout to optimise edge verticality and horizontality

# autograph 0.5.1

## Plotting

- Improved `plot.gof.ergm()` (closes #31)
  - Now works on directed and two-mode networks and dyadwise shared partners
  - Now has more informative error message if statistic not available
  - Now accepts more descriptive and gof-formula consistent statistic names

# autograph 0.5.0

## Package

- Shortened startup messages

## Plotting

- Added `plot.goldfish.changepoints()`
- Added `plot.goldfish.outliers()`
- Added `plot.mnet()` to avoid `igraph::plot.igraph()` dispatching

## Graphing

- Restructured scripts for improved maintenance and development in the future
- Fixed `ggplot2::geom_violin()` call in `graphr()` to avoid warnings with ggplot2 v4.0.0
- Improved `graphr()` internals to rely on a NULL "layout" parameter and `.infer_layout()`
- Improved `graphr()` to use `match_color()` for diffusion palettes
- Fixed `graphs()` handling of changing networks

# autograph 0.4.2

## Package

- Added ggpizzas to README

## Plotting

- Fixed node overflow issue and restored marginal density plot in `plot.ag_conv()` by dropping `+.ggplot()`
- Added automatic legend labelling where node_size or node_color or edge_color is given

## Theming

- Added "hwu" theme for Heriot-Watt University
- Improved highlight and divergent theme setting to use switch
- Added some discussion about colour-blindedness to palettes documentation and viz tute
- Added some instruction on how to customise themes more specifically

# autograph 0.4.1

## Package

- Improved startup message to ignore class

## Theming

- Added "iast" theme for the Institute of Advanced Studies, Toulouse

## Plotting

- Improved ergm convergence diagnostic plotting by working on the results object, 
mostly avoiding startup conflict warning
- Dropped marginal density plot in `plot.ag_conv()` to fix `{ggplot2}` v4.0.0-related node overflow issue

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
