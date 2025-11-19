# Package index

## Theming

Functions for tailoring graphs with themes, scales, and palettes.

<!-- end list -->

  - `stocnet_theme()` `set_stocnet_theme()` : Setting a consistent theme
    for all plots
  - `ag_base()` `ag_highlight()` `ag_positive()` `ag_negative()`
    `ag_qualitative()` `ag_sequential()` `ag_divergent()` `ag_font()` :
    Consistent palette calls
  - `match_color()` `is_dark()` : Matching colors across palettes
  - `scale_fill_iheid()` `scale_colour_iheid()` `scale_color_iheid()`
    `scale_edge_colour_iheid()` `scale_edge_color_iheid()`
    `scale_fill_centres()` `scale_colour_centres()`
    `scale_color_centres()` `scale_edge_colour_centres()`
    `scale_edge_color_centres()` `scale_fill_sdgs()`
    `scale_colour_sdgs()` `scale_color_sdgs()`
    `scale_edge_colour_sdgs()` `scale_edge_color_sdgs()`
    `scale_fill_ethz()` `scale_colour_ethz()` `scale_color_ethz()`
    `scale_edge_colour_ethz()` `scale_edge_color_ethz()`
    `scale_fill_uzh()` `scale_colour_uzh()` `scale_color_uzh()`
    `scale_edge_colour_uzh()` `scale_edge_color_uzh()`
    `scale_fill_rug()` `scale_colour_rug()` `scale_color_rug()`
    `scale_edge_colour_rug()` `scale_edge_color_rug()` : Themed scales
    for further customization

## Plotting methods

Functions for graphing networks and plotting results. `graphr()` graphs
any manynet-compatible class object automagically. `graphs()` and
`grapht()` do the same for multiple networks and dynamic networks,
respectively. `plot()` methods exist for results and other objects for
stocnet packages.

<!-- end list -->

  - `graphr()` : Easily graph networks with sensible defaults
  - `graphs()` : Easily graph a set of networks with sensible defaults
  - `grapht()` : Easily animate dynamic networks with sensible defaults
  - `plot(<node_measure>)` `plot(<tie_measure>)`
    `plot(<network_measures>)` : Plotting logical marks Plotting numeric
    measures
  - `plot(<node_member>)` `plot(<matrix>)` : Plotting categorical
    memberships
  - `plot(<node_motif>)` `plot(<network_motif>)` : Plotting tabular
    motifs
  - `plot(<netlm>)` `plot(<netlogit>)` : Plotting methods for MRQAP
    models
  - `plot(<diff_model>)` `plot(<diffs_model>)` `plot(<learn_model>)` :
    Plotting diffusion models
  - `plot(<network_test>)` : Plotting methods for CUG and QAP tests
  - `plot(<outliers.goldfish>)` `plot(<changepoints.goldfish>)` :
    Plotting adequacy diagnostics
  - `plot(<ag_conv>)` `plot(<traces.monan>)` `plot(<ergm>)` : Plotting
    convergence diagnostics
  - `plot(<ag_gof>)` `plot(<gof.stats.monan>)` `plot(<sienaGOF>)`
    `plot(<gof.ergm>)` : Plotting goodness-of-fit results
  - `plot(<selectionTable>)` `plot(<influenceTable>)` : Plotting effects
    interpretation

## Layouts

Functions for laying out the nodes in a graph. Included here are some
improved or additional layouts to those offered in
[igraph](https://r.igraph.org/) and
[ggraph](https://ggraph.data-imaginist.com) by default.

<!-- end list -->

  - `layout_configuration()` `layout_tbl_graph_configuration()`
    `layout_dyad()` `layout_tbl_graph_dyad()` `layout_triad()`
    `layout_tbl_graph_triad()` `layout_tetrad()`
    `layout_tbl_graph_tetrad()` `layout_pentad()`
    `layout_tbl_graph_pentad()` `layout_hexad()`
    `layout_tbl_graph_hexad()` : Layout algorithms based on
    configurational positions
  - `layout_tbl_graph_layered()` : Layered layout
  - `layout_tbl_graph_matching()` : Matching layout
  - `layout_concentric()` `layout_tbl_graph_concentric()`
    `layout_multilevel()` `layout_tbl_graph_multilevel()`
    `layout_lineage()` `layout_tbl_graph_lineage()` `layout_hierarchy()`
    `layout_tbl_graph_hierarchy()` `layout_alluvial()`
    `layout_tbl_graph_alluvial()` `layout_railway()`
    `layout_tbl_graph_railway()` `layout_ladder()`
    `layout_tbl_graph_ladder()` : Layout algorithms based on bi- or
    other partitions
  - `layout_valence()` `layout_tbl_graph_valence()` : Valence-based
    layout

## Results objects from other packages

<!-- end list -->

  - `res_migraph_reg` `res_migraph_test` `res_migraph_diff`
    `res_manynet_diff` `siena_gof` `siena_influence` `siena_selection`
    `monan_conv` `monan_gof` `ergm_res` `ergm_gof` `goldfish_outliers`
    `goldfish_changepoints` : Precooked results for demonstrating
    plotting
