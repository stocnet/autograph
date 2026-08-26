# Package index

## Theming

Functions for tailoring graphs with themes, scales, and palettes.

- [`stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
  [`set_stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
  : Setting a consistent theme for all plots
- [`stocnet_medium()`](https://stocnet.github.io/autograph/reference/theme_medium.md)
  [`set_stocnet_medium()`](https://stocnet.github.io/autograph/reference/theme_medium.md)
  [`ag_size()`](https://stocnet.github.io/autograph/reference/theme_medium.md)
  : Setting the medium a plot is made for
- [`ag_base()`](https://stocnet.github.io/autograph/reference/ag_call.md)
  [`ag_ink()`](https://stocnet.github.io/autograph/reference/ag_call.md)
  [`ag_missing()`](https://stocnet.github.io/autograph/reference/ag_call.md)
  [`ag_highlight()`](https://stocnet.github.io/autograph/reference/ag_call.md)
  [`ag_positive()`](https://stocnet.github.io/autograph/reference/ag_call.md)
  [`ag_negative()`](https://stocnet.github.io/autograph/reference/ag_call.md)
  [`ag_qualitative()`](https://stocnet.github.io/autograph/reference/ag_call.md)
  [`ag_sequential()`](https://stocnet.github.io/autograph/reference/ag_call.md)
  [`ag_divergent()`](https://stocnet.github.io/autograph/reference/ag_call.md)
  [`ag_font()`](https://stocnet.github.io/autograph/reference/ag_call.md)
  : Consistent palette calls
- [`list_fonts()`](https://stocnet.github.io/autograph/reference/list_fonts.md)
  : Listing the fonts available to R
- [`simulate_colorblind()`](https://stocnet.github.io/autograph/reference/theme_colorblind.md)
  [`check_separation()`](https://stocnet.github.io/autograph/reference/theme_colorblind.md)
  [`check_contrast()`](https://stocnet.github.io/autograph/reference/theme_colorblind.md)
  : Checking colours for colour blindness, print, and legibility
- [`match_color()`](https://stocnet.github.io/autograph/reference/theme_match.md)
  [`is_dark()`](https://stocnet.github.io/autograph/reference/theme_match.md)
  : Matching colors across palettes

## Graphing

Functions for graphing networks.
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
graphs any manynet-compatible class object automagically.
[`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)
and
[`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
do the same for multiple networks and dynamic networks, respectively.

- [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  : Easily graph networks with sensible defaults
- [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)
  : Easily graph a set of networks with sensible defaults
- [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
  [`print(`*`<grapht>`*`)`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
  : Easily animate dynamic networks with sensible defaults
- [`stocnet_completion()`](https://stocnet.github.io/autograph/reference/completion.md)
  [`set_completion()`](https://stocnet.github.io/autograph/reference/completion.md)
  : Completing argument values as you type

## Plotting

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods exist
for results and other objects for stocnet packages.

- [`plot(`*`<node_measure>`*`)`](https://stocnet.github.io/autograph/reference/map_measure.md)
  [`plot(`*`<tie_measure>`*`)`](https://stocnet.github.io/autograph/reference/map_measure.md)
  [`plot(`*`<network_measures>`*`)`](https://stocnet.github.io/autograph/reference/map_measure.md)
  : Plotting logical marks Plotting numeric measures
- [`plot(`*`<node_member>`*`)`](https://stocnet.github.io/autograph/reference/map_member.md)
  [`plot(`*`<matrix>`*`)`](https://stocnet.github.io/autograph/reference/map_member.md)
  : Plotting categorical memberships
- [`plot(`*`<node_motif>`*`)`](https://stocnet.github.io/autograph/reference/map_motifs.md)
  [`plot(`*`<network_motif>`*`)`](https://stocnet.github.io/autograph/reference/map_motifs.md)
  : Plotting tabular motifs
- [`plot(`*`<netlm>`*`)`](https://stocnet.github.io/autograph/reference/model_mrqap.md)
  [`plot(`*`<netlogit>`*`)`](https://stocnet.github.io/autograph/reference/model_mrqap.md)
  : Plotting methods for MRQAP models
- [`plot(`*`<diff_model>`*`)`](https://stocnet.github.io/autograph/reference/plot.diffusion.md)
  [`plot(`*`<diffs_model>`*`)`](https://stocnet.github.io/autograph/reference/plot.diffusion.md)
  [`plot(`*`<learn_model>`*`)`](https://stocnet.github.io/autograph/reference/plot.diffusion.md)
  : Plotting diffusion models
- [`plot(`*`<network_test>`*`)`](https://stocnet.github.io/autograph/reference/plot.network_test.md)
  : Plotting methods for CUG and QAP tests
- [`plot(`*`<diagnose_outliers>`*`)`](https://stocnet.github.io/autograph/reference/plot_adequacy.md)
  [`plot(`*`<outliers.goldfish>`*`)`](https://stocnet.github.io/autograph/reference/plot_adequacy.md)
  [`plot(`*`<diagnose_changepoints>`*`)`](https://stocnet.github.io/autograph/reference/plot_adequacy.md)
  [`plot(`*`<changepoints.goldfish>`*`)`](https://stocnet.github.io/autograph/reference/plot_adequacy.md)
  [`plot(`*`<goldfishOutliers>`*`)`](https://stocnet.github.io/autograph/reference/plot_adequacy.md)
  [`plot(`*`<goldfishChangepoints>`*`)`](https://stocnet.github.io/autograph/reference/plot_adequacy.md)
  [`plot(`*`<goldfishMargins>`*`)`](https://stocnet.github.io/autograph/reference/plot_adequacy.md)
  [`plot(`*`<goldfishGOF>`*`)`](https://stocnet.github.io/autograph/reference/plot_adequacy.md)
  [`plot(`*`<goldfishTimeTest>`*`)`](https://stocnet.github.io/autograph/reference/plot_adequacy.md)
  [`plot(`*`<goldfishOnset>`*`)`](https://stocnet.github.io/autograph/reference/plot_adequacy.md)
  : Plotting adequacy diagnostics
- [`plot(`*`<ag_conv>`*`)`](https://stocnet.github.io/autograph/reference/plot_convergence.md)
  [`plot(`*`<traces.monan>`*`)`](https://stocnet.github.io/autograph/reference/plot_convergence.md)
  [`plot(`*`<ergm>`*`)`](https://stocnet.github.io/autograph/reference/plot_convergence.md)
  [`load_ergm_res()`](https://stocnet.github.io/autograph/reference/plot_convergence.md)
  : Plotting convergence diagnostics
- [`plot(`*`<ag_gof>`*`)`](https://stocnet.github.io/autograph/reference/plot_gof.md)
  [`plot(`*`<gof.stats.monan>`*`)`](https://stocnet.github.io/autograph/reference/plot_gof.md)
  [`plot(`*`<sienaGOF>`*`)`](https://stocnet.github.io/autograph/reference/plot_gof.md)
  [`plot(`*`<gof.ergm>`*`)`](https://stocnet.github.io/autograph/reference/plot_gof.md)
  : Plotting goodness-of-fit results
- [`plot(`*`<result.goldfish>`*`)`](https://stocnet.github.io/autograph/reference/plot_goldfish_fit.md)
  [`plot(`*`<goldfishFit>`*`)`](https://stocnet.github.io/autograph/reference/plot_goldfish_fit.md)
  : Plotting a goldfish model fit at a glance
- [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  : Easily graph networks with sensible defaults
- [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)
  : Easily graph a set of networks with sensible defaults
- [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
  [`print(`*`<grapht>`*`)`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
  : Easily animate dynamic networks with sensible defaults
- [`plot(`*`<selectionTable>`*`)`](https://stocnet.github.io/autograph/reference/plot_interp.md)
  [`plot(`*`<influenceTable>`*`)`](https://stocnet.github.io/autograph/reference/plot_interp.md)
  : Plotting effects interpretation
- [`count_pages()`](https://stocnet.github.io/autograph/reference/count_pages.md)
  : How many pages a paged diagnostic figure has

## Layouts

Functions for laying out the nodes in a graph. Included here are some
improved or additional layouts to those offered in
[igraph](https://r.igraph.org/) and
[ggraph](https://ggraph.data-imaginist.com) by default.

- [`layout_concentric()`](https://stocnet.github.io/autograph/reference/layout_concentric.md)
  [`layout_tbl_graph_concentric()`](https://stocnet.github.io/autograph/reference/layout_concentric.md)
  : Concentric layout
- [`layout_configuration()`](https://stocnet.github.io/autograph/reference/layout_configuration.md)
  [`layout_tbl_graph_configuration()`](https://stocnet.github.io/autograph/reference/layout_configuration.md)
  [`layout_dyad()`](https://stocnet.github.io/autograph/reference/layout_configuration.md)
  [`layout_triad()`](https://stocnet.github.io/autograph/reference/layout_configuration.md)
  [`layout_tetrad()`](https://stocnet.github.io/autograph/reference/layout_configuration.md)
  [`layout_pentad()`](https://stocnet.github.io/autograph/reference/layout_configuration.md)
  [`layout_hexad()`](https://stocnet.github.io/autograph/reference/layout_configuration.md)
  : Layout algorithms based on configurational positions
- [`layout_correspondence()`](https://stocnet.github.io/autograph/reference/layout_correspondence.md)
  [`layout_tbl_graph_correspondence()`](https://stocnet.github.io/autograph/reference/layout_correspondence.md)
  : Correspondence layout
- [`layout_layered()`](https://stocnet.github.io/autograph/reference/layout_layered.md)
  [`layout_tbl_graph_layered()`](https://stocnet.github.io/autograph/reference/layout_layered.md)
  [`layout_lineage()`](https://stocnet.github.io/autograph/reference/layout_layered.md)
  [`layout_tbl_graph_lineage()`](https://stocnet.github.io/autograph/reference/layout_layered.md)
  [`layout_railway()`](https://stocnet.github.io/autograph/reference/layout_layered.md)
  [`layout_tbl_graph_railway()`](https://stocnet.github.io/autograph/reference/layout_layered.md)
  [`layout_ladder()`](https://stocnet.github.io/autograph/reference/layout_layered.md)
  [`layout_tbl_graph_ladder()`](https://stocnet.github.io/autograph/reference/layout_layered.md)
  : Layered layouts
- [`layout_levels()`](https://stocnet.github.io/autograph/reference/layout_levels.md)
  [`layout_tbl_graph_levels()`](https://stocnet.github.io/autograph/reference/layout_levels.md)
  : Levels layout
- [`layout_matching()`](https://stocnet.github.io/autograph/reference/layout_matching.md)
  [`layout_tbl_graph_matching()`](https://stocnet.github.io/autograph/reference/layout_matching.md)
  : Matching layout
- [`layout_scaling()`](https://stocnet.github.io/autograph/reference/layout_scaling.md)
  [`layout_tbl_graph_scaling()`](https://stocnet.github.io/autograph/reference/layout_scaling.md)
  : Scaling layout
- [`layout_valence()`](https://stocnet.github.io/autograph/reference/layout_valence.md)
  [`layout_tbl_graph_valence()`](https://stocnet.github.io/autograph/reference/layout_valence.md)
  : Valence layout
- [`check_span()`](https://stocnet.github.io/autograph/reference/check_layout.md)
  [`check_offset()`](https://stocnet.github.io/autograph/reference/check_layout.md)
  [`check_stress()`](https://stocnet.github.io/autograph/reference/check_layout.md)
  : Checking how well a layout draws its ties

## Results objects from other packages

- [`res_migraph_reg`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`res_migraph_test`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`res_migraph_diff`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`res_manynet_diff`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`siena_gof`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`siena_influence`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`siena_selection`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`monan_conv`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`monan_gof`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`ergm_gof`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`goldfish_outliers`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`goldfish_changepoints`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`goldfish_margins`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`goldfish_gof`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`goldfish_time`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`goldfish_onset`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  [`goldfish_fit`](https://stocnet.github.io/autograph/reference/made_earlier.md)
  : Precooked results for demonstrating plotting
