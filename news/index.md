# Changelog

## autograph 1.1.2

### Package

- Improved the declared dependencies
  - Removed [knitr](https://yihui.org/knitr/) from Suggests: it was used
    solely by the tutorial tests, which now extract the tutorials’ `{r}`
    chunks with the same small scanner used in
    [manynet](https://stocnet.github.io/manynet/) and
    [netrics](https://stocnet.github.io/netrics/) (verified to yield an
    identical expression set to
    [`knitr::purl()`](https://rdrr.io/pkg/knitr/man/knit.html) on the
    autograph tutorial)
  - Removed [tidygraph](https://tidygraph.data-imaginist.com) from
    Imports: its only functional use was reading the edgelist’s target
    column in `.infer_end_cap()`, which now uses
    [`igraph::as_edgelist()`](https://r.igraph.org/reference/as_edgelist.html)
    (verified to give identical end caps)
  - Promoted
    [graphlayouts](https://github.com/schochastics/graphlayouts) from
    Suggests to Imports, since it is required for
    [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)’s
    *default* “stress” layout (without it, wave-to-wave node transitions
    silently degraded to a static aggregate layout) and is installed
    regardless as [ggraph](https://ggraph.data-imaginist.com) imports
    it; its
    [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html)/`thisRequires()`
    guards have been removed
  - Declared a minimum [manynet](https://stocnet.github.io/manynet/)
    version (`>= 2.2.1`)
- Updated the GitHub Actions workflows
  - Updated the actions to their latest major versions
    (`actions/checkout@v7`, `actions/upload-artifact@v7`,
    `actions/download-artifact@v8`), replacing some long-outdated `@v2`
    pins
  - Updated the website deploy job’s `r-lib/actions/setup-pandoc` from
    `@v1` to `@v2`, matching every other `r-lib/actions` step
  - Added checks that metadata and tutorial vignettes correspond
- Improved the test suite while reducing what CRAN has to run
  - The functional audits now fail rather than skip when
    `AUTOGRAPH_STRICT_AUDIT=true`, which the CI check step now sets, so
    a broken layout or plot method can no longer pass CI green
  - Fixed the layout audit’s fixture and argument maps, which paired
    several layouts with networks they cannot lay out; because `skip()`
    aborts the enclosing `test_that()`, the first such mismatch had been
    silently preventing every later layout from being audited at all
    (the layout audit goes from 21 to 108 assertions)
  - Coverage is now measured with `NOT_CRAN=true`, without which every
    `skip_on_cran()` test — most of the suite — was skipped while covr
    ran, badly under-reporting coverage
  - `release` and `pkgdown` no longer run under `if: always()`, so a
    failing `R CMD check` can no longer tag a release or deploy the
    website
  - Replaced
    [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)’s
    sweep over every bundled
    [manynet](https://stocnet.github.io/manynet/) dataset with a
    representative sample, and dropped the `plot.*` smoke tests now
    subsumed by the plot-method audit; CRAN-visible test time falls
    while CRAN-visible assertions rise
  - Added an edge-case audit (`test-functional_errors.R`), an audit of
    the user-facing `layout_*` aliases, and examples for the `ag_*`
    palette accessors and configurational layouts, none of which were
    previously covered
  - Theme-mutating tests now restore the previous theme with
    [`on.exit()`](https://rdrr.io/r/base/on.exit.html), so global theme
    state cannot leak between parallel test workers
- Updated the website and README
  - Updated favicons
  - Split Graphing from Plotting functions
  - Updated README to send visualisation examples to the website
  - Fixed README double logoing on pkgdown build, and added alttext
- Updated CONTRIBUTING with the package architecture, conventions, and
  dev commands
- Updated remaining base R [`stop()`](https://rdrr.io/r/base/stop.html),
  [`warning()`](https://rdrr.io/r/base/warning.html) and
  [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) calls to the
  [manynet](https://stocnet.github.io/manynet/) cli interface

### Graphing

- Fixed
  [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)/[`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
  erroring (“Can’t combine `..1` and `..2` ”) on a longitudinal network
  whose changing node attributes are stored as non-character vectors
  (e.g. the logical `active` flag and numeric height/mass in
  `fict_starwars`)
  - Such networks now split into waves via a guarded
    [`to_waves()`](https://stocnet.github.io/manynet/reference/modif_split.html)
    that coerces the offending attributes when
    [manynet](https://stocnet.github.io/manynet/)’s splitter cannot
    combine them
- Fixed `graphr(..., snap = TRUE)` erroring (“‘-’ only defined for
  equally-sized data frames”) whenever a node sat exactly on a grid
  point
  - [`depth_first_recursive_search()`](https://stocnet.github.io/autograph/reference/depth_first_recursive_search.md)
    compared each node against a distance vector that still included its
    own zero self-distance, so an exact hit selected that entry and
    yielded an empty grid point; the self-distance is now dropped before
    the nearest vacant point is chosen
  - two-mode networks hit this on their very first node, since their
    coordinates are exactly 0 or 1
- Improved
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  to ignore `snap = TRUE` for layered layouts (“hierarchy”, “railway”,
  “ladder”, “alluvial”, “multilevel”, “lineage”, “layered”)
  - These layouts encode rank, mode, or generation along an axis, which
    square-grid snapping would collapse
- Fixed
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  erroring with “argument "node_color" is missing, with no default” when
  passed a list of networks; the call is now forwarded to
  [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)
  as written, rather than argument by argument
- Improved how
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  checks the attribute names given to its aesthetic arguments
  - A mistyped node or tie attribute name now errors immediately, naming
    the argument and offering the closest match:
    `graphr(net, node_color = "welth")` reported “Unknown colour name:
    welth” at draw time, and now reports that “welth” was not found
    among the node attributes and asks whether “wealth” was meant
  - The same applies to `node_shape` (previously “Shape aesthetic
    contains invalid value”), `node_size` and `edge_size` (previously
    “Aesthetics must be either length 1 or the same as the data (8)”),
    `node_group`, and `edge_color`
- Improved the error when the input is not a network, which now names
  the argument and the class given, rather than reporting a missing
  method for
  [`as_tidygraph()`](https://stocnet.github.io/manynet/reference/coerce_graph.html)
- Fixed `isolates` being validated only when the network happened to
  contain isolates, so the same typo errored on one network and was
  ignored on another
- Fixed `node_size` values between 0 and 1 being silently multiplied by
  ten; `node_size = 0.5` now means 0.5, while a *vector* of proportions
  is still rescaled to stay visible
- Fixed
  [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)
  producing empty panels by checking `waves` against the number of
  networks available
- Added a set of internal argument checks (`R/graph_checks.R`) shared by
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md),
  [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md),
  [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md),
  the `layout_*()` functions, and
  [`stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
  - An unrecognised value now errors immediately, naming the argument
    and offering the closest match, rather than falling through to
    [ggplot2](https://ggplot2.tidyverse.org), `{grid}`, or
    [`match.arg()`](https://rdrr.io/r/base/match.arg.html) (so
    `isolates = "drop"` reports `isolates`, rather than “‘arg’ should be
    one of …”)
  - A value that differs only in capitalisation is now used as intended,
    with a note, instead of being rejected: `node_color = "Wealth"`
    finds the `wealth` attribute
  - Note that these checks are stricter than before: a mistyped
    attribute name used to be ignored silently or to fail later, and now
    stops the call
- Updated group-reduction note in
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  and constant-colour note shared by
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  and
  [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
  so that their wording cannot drift

### Plotting

- Improved
  [`plot.node_member()`](https://stocnet.github.io/autograph/reference/map_member.md)
  to draw its dendrogram with
  [ggraph](https://ggraph.data-imaginist.com)
  - Passes `hclust` object’s own merge heights to the dendrogram layout
    to reproduce the previous plot’s leaf order, merge heights, cluster
    label colours, and cutpoint line
  - Branches now drawn in
    [`ag_base()`](https://stocnet.github.io/autograph/reference/ag_call.md)
    rather than black, matching the height axis and so respecting the
    active
    [`stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
  - Removed [ggdendro](https://andrie.github.io/ggdendro/) from Imports
- Updated a stale doc cross-reference in
  [`plot.node_member()`](https://stocnet.github.io/autograph/reference/map_member.md),
  which pointed at `manynet::node_in_community()`; that function moved
  to [netrics](https://stocnet.github.io/netrics/) in manynet 2.0.0
- Updated terse or dead-end messages to say what to do next, including
  [`plot.node_motif()`](https://stocnet.github.io/autograph/reference/map_motifs.md)/[`plot.network_motif()`](https://stocnet.github.io/autograph/reference/map_motifs.md)
  (“Cannot plot these motifs yet, sorry.”),
  [`match_color()`](https://stocnet.github.io/autograph/reference/theme_match.md),
  [`plot.diff_model()`](https://stocnet.github.io/autograph/reference/plot.diffusion.md),
  and the concentric and hierarchy layouts (“Duplicated nodes in
  layers!”)
- Updated zero-variance note shared by three GOF plot methods so that
  their wording cannot drift

### Layouts

- Improved the error on an unrecognised `layout`, which now names the
  argument, suggests the nearest layout, and lists autograph’s own
  layouts, rather than reporting “object ‘layout_tbl_graph_stresss’ not
  found”
  - Passing a layout *function*
    (e.g. `layout = igraph::layout_with_fr`), rather than its name, now
    says so instead of erroring with “invalid indexing”
- Added checks of the `membership`/`level`/`rank` arguments that the
  partition layouts require, which name the argument and offer the
  closest match

### Theming

- Improved
  [`stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
  to error with the nearest match on an unrecognised theme name, instead
  of warning and silently leaving the previous theme in place; giving
  more than one theme name is also caught

### Tutorials

- Updated visualization tutorial to use colour/color consistently

## autograph 1.1.1

CRAN release: 2026-07-21

### Graphing

- Fixed
  [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
  example, which took too long to run, by couching it in `\donttest{}`
- Improved
  [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)
  to accept bare longitudinal or dynamic networks
  - Splits it into waves or time slices automatically (consistent with
    [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)),
    instead of erroring with “invalid to use names()\<- on an S4 object
    of class ‘dgCMatrix’”
- Fixed various errors and warnings in
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)/[`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)
  on signed, directed, or changing networks:
  - Fixed recycling warnings (“longer object length is not a multiple of
    shorter object length”) in
    [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
    on a complex network
    - arc geom’s `strength` parameter now excludes loop edges, which are
      drawn separately by
      [`geom_edge_loop0()`](https://ggraph.data-imaginist.com/reference/geom_edge_loop.html)
  - Fixed spurious warning (“no non-missing arguments to max; returning
    -Inf”) in
    [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
    on a changing network without any adoption events
    (e.g. `fict_potter`)
    - such networks now rendered as standard changing networks rather
      than routed through the diffusion node-colour mapping
  - Fixed fails at draw time with “invalid hex digit in ‘color’ or
    ‘lty’” in
    [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
    on a signed multiplex network (e.g. `fict_marvel`)
    - ties on non-signed layers (which have a missing sign) are now
      drawn solid/positive rather than passing `NA` to grid
  - Fixed error with “Aesthetics must be either length 1 or the same as
    the data” in
    [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)/[`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)
    on signed longitudinal or directed networks
    (e.g. `to_waves(ison_monks)`)
    - per-tie sign linetypes are now mapped through
      [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) (with
      an identity scale) so ggraph’s edge geoms expand and subset them
      consistently with edge colour and width
  - Fixed `graphr(..., snap = TRUE)` erroring on two-mode networks:
    their default “hierarchy” layout is now left un-snapped (with an
    informative message) since collapsing its layered coordinates onto a
    square grid is not meaningful, while snapping a two-mode network
    under a force-directed layout continues to work; also hardened the
    grid-snapping search so a coordinate landing exactly on a grid point
    is no longer mistaken for a node’s zero self-distance

### Tutorials

- Updated visualisation tutorial with a bit more structuring of further
  options

## autograph 1.1.0

### Package

- Test coverage raised by introducing functional testing infrastructure
  (`tests/testthat/helper-functional.R` and `test-functional_*.R`) for:
  - the layout family
  - the `plot.*` S3 method family
  - the `ag_*` palette accessors across all themes
  - [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)’s
    aesthetic arguments each enumerated automatically and audited
    against fixture grids
- Removed the unused internal helper
  [`seq_nodes()`](https://stocnet.github.io/manynet/reference/progress.html)
- Excluded the interactive-only palette helper `ggpizza()` from coverage
- Added [migraph](https://stocnet.github.io/migraph/) to Suggests (used
  in tests only)

### Graphing

- Improved how
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  treats labels
  - Fixed labels overlapping nodes (closes
    [\#13](https://github.com/stocnet/autograph/issues/13)): labels now
    keep clear of node borders automatically by giving ggrepel each
    node’s true rendered size, with `label_dist` adding a further
    points-based gap (mirroring igraph’s `vertex.label.dist`) and
    `label_repel = FALSE` selecting a fixed offset instead of repulsion.
  - This also fixed a pre-existing bug where non-repelled labels
    rendered with a fully transparent fill under this package’s theme,
    making them invisible over nodes.
- Improved
  [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
  has been rewritten for smoother, more consistent animations of dynamic
  networks
  - Node positions now transition seamlessly between waves using the
    dynamic stress layout from
    [graphlayouts](https://github.com/schochastics/graphlayouts)
    (`layout_as_dynamic()`), with a new `alpha` argument controlling
    layout stability; other layouts are computed once on the aggregate
    network and held fixed
  - Changing node composition is now handled properly: every node that
    ever appears gets a stable position and fades in and out in place as
    it enters and exits the network
  - New `isolates` argument (`"keep"` or `"fade"`) controls whether
    temporarily isolated nodes stay visible or fade out; `keep_isolates`
    is deprecated
  - Dynamic (time-stamped, event-based) networks such as `irps_nuclear`
    are now split automatically into cumulative time slices via
    [`manynet::to_slices()`](https://stocnet.github.io/manynet/reference/modif_split.html),
    so a single dynamic network object passed to
    [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
    works without manual conversion
  - Interval (spell) networks that record tie `begin`/`end` lifespans,
    such as `irps_wwi`, are now split automatically into one snapshot
    per change point showing the ties active in that spell, so
    `grapht(irps_wwi)` works directly (previously it errored because
    such networks are dynamic but carry no `time` attribute for
    [`to_slices()`](https://stocnet.github.io/manynet/reference/modif_split.html));
    `irps_wwi` is now a runnable example in the documentation
  - [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
    now uses the dynamic stress layout by default even for two-mode
    networks (rather than a hierarchy layout, which collapsed many nodes
    onto a line), suppresses node labels by default for networks with
    more than 30 nodes to keep frames legible, and fades densely
    overlapping ties so they read as a density gradient rather than a
    solid mass
  - Fixed an error when animating networks whose node names contain
    non-ASCII characters
  - Waves without any ties are no longer silently dropped
  - Closer visual parity with
    [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md):
    directed networks get arrowheads on segments trimmed at the target
    node, signed networks distinguish positive/negative ties by linetype
    and colour, mapped aesthetics use the same palettes with factor
    levels consistent across frames, and legends transition along with
    the animation
  - Aesthetic-resolution helpers are now shared between
    [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
    and
    [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
    (new R/graph_aes.R), so styling cannot drift between static and
    animated plots
  - Added a test suite for
    [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
    (no gif rendering required)
  - Now aborts with a clear message when its input cannot be split into
    waves or slices, instead of failing much later with a cryptic igraph
    error (closes
    [\#40](https://github.com/stocnet/autograph/issues/40)); the
    underlying cause —
    [`to_waves()`](https://stocnet.github.io/manynet/reference/modif_split.html)
    silently ignoring a time attribute not named “wave” — will be fixed
    in [manynet](https://stocnet.github.io/manynet/) 2.2.2, and the
    tutorial example now uses a `wave` attribute, which splits correctly
    with [manynet](https://stocnet.github.io/manynet/) 2.2.1
- Added an `edge_bundle` argument to
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  for bundling edges in dense networks (closes
  [\#19](https://github.com/stocnet/autograph/issues/19)):
  - `TRUE`/`"force"` uses force-directed bundling, with `"path"` and
    `"minimal"` selecting the other non-hierarchical algorithms
  - colour/width/linetype mappings are preserved and directed networks
    keep their arrowheads.
  - This wires up ggraph’s non-hierarchical bundling geoms (added in
    ggraph 2.2.0), which were previously imported but never called, so
    the ggraph dependency is now `(>= 2.2.0)`
- Fixed `edge_size = 0` not fully suppressing edges on directed networks
  (closes [\#50](https://github.com/stocnet/autograph/issues/50)):
  arrowhead length was hard-coded regardless of `edge_size`, leaving a
  visible arrowhead when the line was hidden. Arrow length now scales
  with the resolved edge width (capped so heavily-weighted edges don’t
  get oversized heads) and is omitted entirely when the width is 0
- Fixed two-mode auto-shapes assigning circles to the second mode: the
  first mode now takes circles and the second squares, as intended
- Fixed
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  returning an empty plot for networks consisting only of isolates
  (e.g. the empty dyad/triad motifs): isolates are now kept whenever
  removing them would empty the graph
- Fixed
  [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)
  erroring on lists containing tie-less networks
  (e.g. [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on
  motif censuses): panels sharing a layout now keep isolates so every
  node has a coordinate in every wave
- Fixed
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  erroring on weight or size attributes carrying measure classes
  (e.g. `tie_measure` results from
  [netrics](https://stocnet.github.io/netrics/) stored as attributes)
- Fixed a vector-recycling warning in
  [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)’
  ego-network detection

### Tutorials

- Fixed the “Tying up loose ends” exercise in the visualisation tutorial
  erroring on
  [`tie_closeness()`](https://stocnet.github.io/manynet/reference/defunct.html)
  (closes [\#39](https://github.com/stocnet/autograph/issues/39)): the
  tutorial now loads [netrics](https://stocnet.github.io/netrics/) and
  uses its measure functions
  ([`tie_by_closeness()`](https://stocnet.github.io/netrics/reference/measure_centralities_close.html),
  [`tie_is_triangular()`](https://stocnet.github.io/netrics/reference/mark_triangles.html)),
  and every tutorial code chunk is now exercised by the functional tests
  below
- Reworked the “Visualising Networks” tutorial to match the structure
  and features of the [manynet](https://stocnet.github.io/manynet/) v2.2
  tutorials
  - Rebranded the tutorial in autograph red, with larger, more readable
    text and matching ‘Run code’ buttons
  - Added a checkbox Aims section, “Catching up”, “Going further”,
    “Beginner note”, and “In brief” callout boxes, per-page mini-tables
    of contents, and free play sections with a choose-your-own-data
    difficulty ladder
  - Added hover-over glossary terms throughout and a closing Summary
    section with a function overview table and glossary
  - Added quiz questions with feedback, and hints for the coding
    exercises
  - New coverage of `edge_bundle`, `label_repel`/`label_dist`, the
    `isolates` argument, `snap` grid-snapping, autograph’s own
    special-purpose layouts, and programmatic export with
    [`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  - New sections on directed networks (automatic, width-scaled
    arrowheads and manual control via `edge_size`), automatic mode
    shapes in two-mode networks, and manually adjusting a layout’s
    coordinate table before passing it back via `x`/`y`
  - Added artist-themed gifs throughout, including as quiz-answer
    feedback
- Added a static, read-only version of the tutorial as a pkgdown article
  (“Tutorials” menu on the website), as in
  [manynet](https://stocnet.github.io/manynet/)
- Added functional testing of all tutorial code chunks
  (`tests/testthat/test-tutorials_autograph.R`), mirroring
  [manynet](https://stocnet.github.io/manynet/)’s tutorial testing
  infrastructure

### Layouts

- Fixed
  [`layout_tbl_graph_layered()`](https://stocnet.github.io/autograph/reference/layout_layered.md)
  ordering nodes by the names rather than the positions of their
  neighbours in adjacent layers, which degraded every barycenter sweep
  to NA and raised warnings
- Replaced deprecated
  [`dplyr::case_match()`](https://dplyr.tidyverse.org/reference/case_match.html)
  with
  [`dplyr::recode_values()`](https://dplyr.tidyverse.org/reference/recode-and-replace-values.html)

### Plotting

- Fixed
  [`plot.matrix()`](https://stocnet.github.io/autograph/reference/map_member.md)
  erroring when no `membership` argument was supplied, for both one-mode
  and two-mode matrices

## autograph 1.0.3

CRAN release: 2026-05-01

### Plotting

- Fixed the error produced by a namespace reference in a call in
  `ergm_res` by serializing it, moving to extdata, and loading it via
  [`load_ergm_res()`](https://stocnet.github.io/autograph/reference/plot_convergence.md)

## autograph 1.0.2

### Plotting

- Fixed the error produced by a namespace reference in a call in
  `ergm_res`

## autograph 1.0.1

### Package

- Updated the logo

### Plotting

- Fixed the error in some flavours by removing environment references in
  included objects
- Closed [\#44](https://github.com/stocnet/autograph/issues/44) by
  adding example that uses
  [`ggplot2::scale_colour_discrete()`](https://ggplot2.tidyverse.org/reference/scale_colour_discrete.html)
  to tweak colour output

### Tutorials

- Fixed the tutorial to use netrics functions

## autograph 1.0.0

CRAN release: 2026-04-04

### Package

- Updated startup message to declare the set theme

### Graphing

- Improved `node_shape` to accept more categories
- Moved `node_is_isolate` and `node_adoption_time` to internal helpers
  to reduce dependencies
- Fixed [\#47](https://github.com/stocnet/autograph/issues/47) by
  overriding shape to fillable (21)
- Fixed [\#10](https://github.com/stocnet/autograph/issues/10) and
  [\#52](https://github.com/stocnet/autograph/issues/52) by adding label
  distance parameter
- Fixed [\#17](https://github.com/stocnet/autograph/issues/17) and
  [\#40](https://github.com/stocnet/autograph/issues/40) by improving
  [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)

### Plotting

- Closed [\#37](https://github.com/stocnet/autograph/issues/37) by
  adding EgoAlter sienaGOF plot
- Fixed [\#36](https://github.com/stocnet/autograph/issues/36) by using
  `.to_factor()` in
  [`plot.ag_gof()`](https://stocnet.github.io/autograph/reference/plot_gof.md)
  to correct numeric ordering of statistics, and dropped cumulative
  correction in
  [`plot.sienaGOF()`](https://stocnet.github.io/autograph/reference/plot_gof.md)
- Fixed [\#38](https://github.com/stocnet/autograph/issues/38) by
  dropping linetype in favour of colours even for bw theme in
  [`plot.influenceTable()`](https://stocnet.github.io/autograph/reference/plot_interp.md)
- Fixed option reference from `snet_theme` to `stocnet_theme` in
  [`plot.selectionTable()`](https://stocnet.github.io/autograph/reference/plot_interp.md)
  and
  [`plot.influenceTable()`](https://stocnet.github.io/autograph/reference/plot_interp.md)
- Applied
  [`ag_font()`](https://stocnet.github.io/autograph/reference/ag_call.md)
  to migraph model plots for consistent typography
- Reorganised plot scripts by purpose: summaries, tests, and analysis
- Fixed [\#46](https://github.com/stocnet/autograph/issues/46) by not
  appending “X” to the names when creating the data frame and moving
  factor coercion later in the process

### Tutorial

- Fixed [\#41](https://github.com/stocnet/autograph/issues/41) by
  updating tutorials to use fill aesthetics (`scale_fill_*()`) instead
  of colour aesthetics (`scale_colour_*()`)

## autograph 0.6.0

CRAN release: 2026-03-01

### Package

- Updated logo

### Graphing

- [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  now auto-dispatches to
  [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)
  when passed a list of graphs
- [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)
  no longer requires [patchwork](https://patchwork.data-imaginist.com)
  to be loaded separately
- Closed [\#12](https://github.com/stocnet/autograph/issues/12) by
  changing node aesthetics from `color` to `fill` using fillable shape
  codes (21–25) for nodes to support fill-based colour scales
- Consolidated legend modifications into internal `graph_legends()`
  helper
- Improved legend labelling:
  - edge sign legend now labelled “Sign”
  - edge weight/width now labelled “Weight”/“Width”
  - node mode/shape legend now labelled “Mode”/“Shape”
- Closed [\#8](https://github.com/stocnet/autograph/issues/8) by adding
  “isolates” argument to
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md):
  - `"legend"` (default) removes isolates from the graph but notes them
    in the legend
  - `"caption"` removes isolates from the graph but notes them in the
    caption
  - `"keep"` retains isolates in the graph as-is (closes
    [\#12](https://github.com/stocnet/autograph/issues/12))

### Theming

- Removed `scale_*()` functions as redundant with better theme support
  and
  [`match_color()`](https://stocnet.github.io/autograph/reference/theme_match.md)

### Layouts

- Replaced `{Rgraphviz}` (Bioconductor) with
  [`igraph::layout_with_sugiyama`](https://r.igraph.org/reference/layout_with_sugiyama.html)
  for Sugiyama/hierarchy layouts, removing the Bioconductor dependency
- Improved Sugiyama layout with dummy node insertion and barycenter
  crossing minimisation for better edge routing
- Fixed [\#18](https://github.com/stocnet/autograph/issues/18) for
  lattice layout snapping by rotating the layout to optimise edge
  verticality and horizontality

## autograph 0.5.1

### Plotting

- Improved
  [`plot.gof.ergm()`](https://stocnet.github.io/autograph/reference/plot_gof.md)
  (closes [\#31](https://github.com/stocnet/autograph/issues/31))
  - Now works on directed and two-mode networks and dyadwise shared
    partners
  - Now has more informative error message if statistic not available
  - Now accepts more descriptive and gof-formula consistent statistic
    names

## autograph 0.5.0

CRAN release: 2025-11-19

### Package

- Shortened startup messages

### Plotting

- Added `plot.goldfish.changepoints()`
- Added `plot.goldfish.outliers()`
- Added `plot.mnet()` to avoid
  [`igraph::plot.igraph()`](https://r.igraph.org/reference/plot.igraph.html)
  dispatching

### Graphing

- Restructured scripts for improved maintenance and development in the
  future
- Fixed
  [`ggplot2::geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.html)
  call in
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  to avoid warnings with ggplot2 v4.0.0
- Improved
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  internals to rely on a NULL “layout” parameter and `.infer_layout()`
- Improved
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  to use
  [`match_color()`](https://stocnet.github.io/autograph/reference/theme_match.md)
  for diffusion palettes
- Fixed
  [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)
  handling of changing networks

## autograph 0.4.2

### Package

- Added ggpizzas to README

### Plotting

- Fixed node overflow issue and restored marginal density plot in
  [`plot.ag_conv()`](https://stocnet.github.io/autograph/reference/plot_convergence.md)
  by dropping `+.ggplot()`
- Added automatic legend labelling where node_size or node_color or
  edge_color is given

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
- Dropped marginal density plot in
  [`plot.ag_conv()`](https://stocnet.github.io/autograph/reference/plot_convergence.md)
  to fix [ggplot2](https://ggplot2.tidyverse.org) v4.0.0-related node
  overflow issue

## autograph 0.4.0

CRAN release: 2025-09-10

### Package

- Fixed DOI in CITATION
- Improved README introduction
- Improved README graphing illustration, including igraph comparison
  figure
- Improved README plotting illustration, added SAOM/ERGM GOF comparison
  figure

### Theming

- Improved
  [`stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
  documentation
- Improved
  [`stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
  to register a font family for plots
  - A vector of potential fonts is included for some themes
  - The first font found on the system will be used, and user notified
  - If no fonts are found, the default R font (“sans”) will be used
  - A message is printed to inform the user if the default is used, and
    how to install missing fonts via
    [extrafont](https://github.com/fbertran/extrafont)
- Added font options for “iheid”, “oxf”, “ethz”, “uzh”, and “rug” themes
- Added
  [`ag_font()`](https://stocnet.github.io/autograph/reference/ag_call.md)
  for retrieving the registered font family
- Improved `match_colors()` documentation
- Exported
  [`is_dark()`](https://stocnet.github.io/autograph/reference/theme_match.md)
  and made it vectorised
- Added `match_colors()` and
  [`is_dark()`](https://stocnet.github.io/autograph/reference/theme_match.md)
  tests
- Dropped `theme_*()` functions in favour of
  [`stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
- Dropped `theme_*()` tests
- Added
  [`stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
  tests
- Added “cmu” theme for Carnegie Mellon University

### Graphing

- Improved
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  by using registered fonts where available
- Improved
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  by using
  [`ag_qualitative()`](https://stocnet.github.io/autograph/reference/ag_call.md)
  for discrete colour scales

### Plotting

- Improved
  [`plot.selectionTable()`](https://stocnet.github.io/autograph/reference/plot_interp.md)
  and
  [`plot.influenceTable()`](https://stocnet.github.io/autograph/reference/plot_interp.md)
  documentation by consolidating them together into one help file
- Improved
  [`plot.ag_conv()`](https://stocnet.github.io/autograph/reference/plot_convergence.md),
  [`plot.ag_gof()`](https://stocnet.github.io/autograph/reference/plot_gof.md),
  and plot_interp by using registered fonts where available
- Improved
  [`plot.sienaGOF()`](https://stocnet.github.io/autograph/reference/plot_gof.md)
  to use lower case auxiliary statistic description

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

- Added
  [`plot.ag_conv()`](https://stocnet.github.io/autograph/reference/plot_convergence.md)
  as a new plotting method for convergence diagnostics
  - Plots MCMC traces as a line plot with loess smoothing highlighting
    the trend
  - Plots overall density plot of the samples on the right margin
- Improved plotting of `{MoNAn}` trace objects by using
  [`plot.ag_conv()`](https://stocnet.github.io/autograph/reference/plot_convergence.md)
- Added `plot.mcmc.list()` for plotting MCMC samples from
  [`ergm::ergm`](https://rdrr.io/pkg/ergm/man/ergm.html) results
  objects, using
  [`plot.ag_conv()`](https://stocnet.github.io/autograph/reference/plot_convergence.md)

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
- Added CODECOV_TOKEN to Github secrets for test coverage reporting

### Graphing

- Fixed
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  not using theme colours for node and edge aesthetics

### Plotting

- Added new plot class and method for centralising GOF plotting
  - Improved
    [`plot.sienaGOF()`](https://stocnet.github.io/autograph/reference/plot_gof.md)
    and
    [`plot.gof.stats.monan()`](https://stocnet.github.io/autograph/reference/plot_gof.md)
    to use new plotting method
  - Added
    [`plot.gof.ergm()`](https://stocnet.github.io/autograph/reference/plot_gof.md)
    for plotting ERGM GOF objects
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

- Added
  [`set_stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
  alias for
  [`stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
- Fixed issue with ‘oxf’ and ‘unige’ themes not being recognised

### Data

- Added `res_ergm_gof` for testing and illustration of
  [`plot.gof.ergm()`](https://stocnet.github.io/autograph/reference/plot_gof.md)

### Tutorial

- Added visualisation tutorial from manynet
  - Improved introduction
  - Improved section on titles, labels, and legends
  - Improved section explaining base and grid graphics
  - Added section demonstrating difference between `igraph::plot()`,
    [`ggraph::ggraph()`](https://ggraph.data-imaginist.com/reference/ggraph.html),
    and
    [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  - Added table describing the main arguments of
    [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
    and the visualisation dimensions to which they relate
  - Added section showing how to use “node_shape” (poorly)
  - Improved section showing how to use “node_colour” and “node_group”
  - Added section showing how to use “node_size”
  - Added section showing how to set a theme
  - Added section showing how to use “edge_colour” and “edge_size”
  - Improved section introducing
    [patchwork](https://patchwork.data-imaginist.com),
    [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md),
    and
    [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
  - Added plotting section to viz tutorial

## autograph 0.2.0

CRAN release: 2025-08-22

### Package

- Added (currently) necessary dependencies
- Removed unnecessary manynet dependencies

### Plotting

- Added
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md),
  [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md),
  and
  [`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md)
  from manynet for quick plotting of one or more graphs
- Improved `+.ggplot()` to check whether second object inherits ggplot
  class (thanks [@teunbrand](https://github.com/teunbrand) for fixing
  [\#110](https://github.com/stocnet/autograph/issues/110))
- Fixed issue with edge_linetype in signed, directed networks with
  weights (closed
  [\#14](https://github.com/stocnet/autograph/issues/14))
- Updated README with examples of
  [`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
  and others

### Themes

- Added themes for Oxford and UNIGE (closed
  [\#1](https://github.com/stocnet/autograph/issues/1))
- Improved
  [`stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
  to add option to “stocnet_theme” (previously “snet_theme”)
- Added
  [`is_dark()`](https://stocnet.github.io/autograph/reference/theme_match.md)
  to check whether a theme is dark or light
- Added
  [`match_color()`](https://stocnet.github.io/autograph/reference/theme_match.md)
  to return closest matching colors from a palette

### Layouts

- Added layout_valence() for signed graphs (closed
  [\#6](https://github.com/stocnet/autograph/issues/6))
- Added configurational layouts from manynet
- Added aliases for various layout_tbl_graph\_\*() functions

## autograph 0.1.2

CRAN release: 2025-07-02

### Package

- Fixed line break issues in DESCRIPTION

### Themes

- Fixed default for
  [`ag_sequential()`](https://stocnet.github.io/autograph/reference/ag_call.md)

### Layouts

- Fixed “layout_layered” to accept typical ggraph parameters

### manynet

- Added return value statements

### migraph

- Added return value statements

### RSiena

- Added return value statements

### MoNAn

- Added
  [`plot.traces.monan()`](https://stocnet.github.io/autograph/reference/plot_convergence.md)
- Added
  [`plot.gof.stats.monan()`](https://stocnet.github.io/autograph/reference/plot_gof.md)

## autograph 0.1.1

### Package

- Fixed DESCRIPTION issues

## autograph 0.1.0

### Package

- Initial setup, with all the goodies

### Theming

- Added
  [`stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
  for setting a theme that can then be reused across successive plots
  (closes [\#3](https://github.com/stocnet/autograph/issues/3))
  - For example, `stocnet_theme("iheid")` for IHEID colour theme
- Added `ag_*()` palettes, for example:
  - [`ag_highlight()`](https://stocnet.github.io/autograph/reference/ag_call.md)
    to select the highlight colours of a particular palette
- Added
  [`match_color()`](https://stocnet.github.io/autograph/reference/theme_match.md)
  for matching one or more hexcodes to a palette’s offerings

### Plotting

- Added manynet plot methods
  - Added
    [`plot.diff_model()`](https://stocnet.github.io/autograph/reference/plot.diffusion.md)
  - Added
    [`plot.matrix()`](https://stocnet.github.io/autograph/reference/map_member.md)
  - Added
    [`plot.learn_model()`](https://stocnet.github.io/autograph/reference/plot.diffusion.md)
  - Added
    [`plot.network_measures()`](https://stocnet.github.io/autograph/reference/map_measure.md)
  - Added
    [`plot.network_motif()`](https://stocnet.github.io/autograph/reference/map_motifs.md)
  - Added
    [`plot.tie_measure()`](https://stocnet.github.io/autograph/reference/map_measure.md)
  - Added
    [`plot.node_measure()`](https://stocnet.github.io/autograph/reference/map_measure.md)
  - Added
    [`plot.node_member()`](https://stocnet.github.io/autograph/reference/map_member.md)
  - Added `plot.node_members()`
  - Added
    [`plot.node_motif()`](https://stocnet.github.io/autograph/reference/map_motifs.md)
- Added migraph plot methods
  - Added
    [`plot.netlm()`](https://stocnet.github.io/autograph/reference/model_mrqap.md)
  - Added
    [`plot.netlogit()`](https://stocnet.github.io/autograph/reference/model_mrqap.md)
  - Added
    [`plot.network_test()`](https://stocnet.github.io/autograph/reference/plot.network_test.md)
  - Added
    [`plot.diffs_model()`](https://stocnet.github.io/autograph/reference/plot.diffusion.md)
- Added RSiena plot methods
  - Added
    [`plot.sienaGOF()`](https://stocnet.github.io/autograph/reference/plot_gof.md)
  - Added
    [`plot.influenceTable()`](https://stocnet.github.io/autograph/reference/plot_interp.md)
  - Added
    [`plot.selectionTable()`](https://stocnet.github.io/autograph/reference/plot_interp.md)

### Layouts

- Added
  [`layout_tbl_graph_matching()`](https://stocnet.github.io/autograph/reference/layout_matching.md)
  for positioning nodes according to their
  [`manynet::to_matching()`](https://stocnet.github.io/manynet/reference/modif_paths.html)
  partners
- Added first version of
  [`layout_tbl_graph_layered()`](https://stocnet.github.io/autograph/reference/layout_layered.md)
  for layering nodes according to a Sugiyama-type layout

### Data

- Added some precooked results for testing and illustration
