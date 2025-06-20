# autograph 0.1.0

## Package

- Initial setup, with all the goodies

## Theming

- Added `stocnet_theme()` for setting a theme that can then be reused across
successive plots.
  - For example, `stocnet_theme("iheid")` for IHEID colour theme
- Added `ag_*()` palettes, for example:
  - `ag_highlight()` to select the highlight colours of a particular palette

## Plotting

- Added manynet plot methods
- Added migraph plot methods
- Added RSiena plot methods

## Layouts

- Added `layout_tbl_graph_matching()` for positioning nodes according to their
`manynet::to_matching()` partners
- Added first version of `layout_tbl_graph_layered()` for layering nodes
according to a Sugiyama-type layout

## Data

- Added some precooked results for testing and illustration
