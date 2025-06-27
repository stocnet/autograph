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
