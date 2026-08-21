# autograph 1.2.0

## Package

- Removed the CRAN version check from `.onAttach()`, making `library(autograph)` faster to attach
- Added `{goldfish}` to `Enhances`
- Added `{systemfonts}` to `Suggests`
- Added a `website-builds` job to `prchecks.yml`, reporting whether the site builds
  - `count_pages()` was missing from the reference index, which stopped it
- Updated CONTRIBUTING with conventions for function names, `NEWS.md` bullets,
  the website reference index, and keeping tutorials and articles in step
- Added `stocnet_completion()` to offer values an argument accepts as RStudio completions
  - `graphr(fict_lotr, node_color =` Tab lists the node variables `fict_lotr` holds
  - Covers node and tie variables, layouts, label criteria, themes, and defaults such as `isolates`
  - Off unless asked for, since it replaces/encapsulates one of RStudio's internal functions; `stocnet_completion(FALSE)` restores it
  - A `persist` argument remembers the choice, as `stocnet_theme()` does
  - Values labelled with its class and its categories or range; a layout with the package that draws it

## Layouts

- Fixed `layout_multilevel()` to identify modes without having to name a `level`
- Added `method`, `alpha`, `beta`, `FUN1` and `FUN2` arguments to `layout_multilevel()`, exposing how `graphlayouts::layout_as_multilevel()` lays out and projects the levels
- Fixed meaningless layout inference, e.g. `railway` for a one-mode network, by checking whether layout is appropriate and reverts to default if not
- Fixed `layout_tbl_graph_layered()` returning `NA` coordinates for unrankable nodes, which failed at draw time with "missing value where TRUE/FALSE needed"; such nodes are now placed in a layer below the ranked ones
- Removed unreachable `getNNvec()`
- Fixed `layout_hierarchy()` centring on a node of the second mode yet reading first mode labels

## Graphing

- Improved `graphr()` to draw multilevel networks of interlocking one-mode and two-mode layers (identified by `manynet::is_multilevel()`) with the "multilevel" layout by default
  - Fixed tie opacity so that those between levels fade behind those within them
  - Fixed default node size in multilevel layout, which is now taken from how many nodes there are at each level rather than in the whole network
  - Fixed labelling to be plain text nudged away instead of white-boxed labels
- Fixed tie colouring in multiplex networks to color layers not signs by default
  - Signs are still drawn as linetypes
  - Added a legend for the tie linetype wherever it is the only thing showing the signs, and is titled by whatever color is showing
- Improved node shape legend of a two-mode network to name the modes where the network records them instead of default "One" and "Two"
- Fixed size of self-loops to draw as a fraction of how far the layout spreads rather than at a fixed diameter of one coordinate unit
- Improved `node_group` to draw overlapping hulls (closes #51)
  - e.g. `graphr(ison_adolescents, node_group = netrics::node_x_clique())`

## Theming

- Added a `persist` argument to `stocnet_theme()`, remembering a theme
  - `persist = TRUE` writes it to `tools::R_user_dir("autograph", "config")`
  - Nothing written to disk unless passed explicitly
  - Setting a theme without it clears any choice persisted earlier
- Improved font detection in `stocnet_theme()` via `{systemfonts}`
  - Added `list_fonts()` for listing the font families R can see
- Improved theme backgrounds to reach every plot, not only the graphs
  - Plot themes are now built with the `ag_theme_*()` wrappers
  - Blanked elements stay blank, so a graph keeps no axis text or coordinates
  - Ties, nodes, and labels with no colour take `ag_ink()` and the ground
- Added `ag_ink()` for the colour a theme writes with
  - Axis text, reference lines, and other chrome take it, not `ag_base()`
  - Frees `ag_base()` to be light where that sets it off from the highlight
- Added `simulate_colorblind()` and `contrast_colors()` for checking palettes
  - Simulates deuteranopia, protanopia, and tritanopia (Machado et al. 2009)
  - Scores a pair by its worst case across those and normal vision
- Improved every theme's categorical palette for colour-blind viewers
  - `ag_qualitative()` uses most distinct, own colors first not mixtures
  - Samples across the palette only where it holds too few colours
  - Kept "rainbow" in its own order, since fidelity to a spectrum is its point
- Improved some highlight pairs
  - Fixed "neon" highlight pair, a cyan and a green 12.7 apart
  - Fixed "ethz" and "cmu" highlight pairs by lightening their greys
- Fixed divergent palettes pairing a red pole with a green or teal one, such as in "ethz"
- Added a "clay" theme inspired by palette and fonts of Anthropic's Claude

## Plotting

- Added `plot.goldfishFit()` for the four diagnostic panels a fit can supply
  - Deviance trace, Schoenfeld smooths, score processes, and waiting times
  - Draws only from what the fit stores, leaving a missing panel out
  - Draws the waiting-time panel for exact-time models only
  - Draws the compact term strings the test itself carries, which do not
    repeat where an effect appears over two networks
  - Reports how many terms the Schoenfeld panel dropped
  - Fixed the Schoenfeld panel to select terms by column position
    - The labels it matched on intersect the effect names on the intercept
      alone, so it drew one term where four were asked for
- Added `plot.goldfishGOF()` for each effect's cumulative score process
  - Draws the Brownian-bridge bands the effect's p-value was read from
  - Draws x on the object's own process time, named for the clock it records
  - Inverts the distribution the event-clock p-value comes from
- Added `plot.goldfishTimeTest()` for the scaled Schoenfeld residuals
  - Draws a smooth per effect, with the fitted estimate as the reference
  - Colours the scatter by period under `method = "periods"`
- Added `plot.goldfishOnset()` for the parameter path and information accrual
  - Windows both panels on the excursion, so each coefficient gets its scales
  - Draws the proportional diagonal, the departure from which is the finding
  - Added `view = c("both", "path", "accrual")` to select a single panel
- Added `plot.goldfishMargins()` for observed against expected activity
  - Draws martingale residuals where the model class defines a compensator
  - Draws the calibration ratio where it does not, from the recorded scales
  - Draws the `top` actors furthest from the reference, and counts the rest
  - Draws level against shape where goldfish supplies `dispersion`
  - Names both omissions: under two completed spans, and beyond `top`
- Added a `page` argument to `plot()` on the per-term diagnostics
  - Applies to `goldfishGOF`, `goldfishTimeTest`, and `goldfishOnset`
  - Added `count_pages()`, reporting the count without rendering
  - Renamed from `ag_pages()`, since `ag_` is for the theme accessors
  - Errors with the page count where `page` is past the last
  - Leaves each figure as it was where `page` is omitted
- Renamed the goldfish classes to a package prefix and a camelCase noun
  - `goldfishOutliers`, `goldfishChangepoints`, `goldfishOnset`,
    `goldfishMargins`, `goldfishGOF`, `goldfishTimeTest`, `goldfishScoreTest`,
    and `goldfishFit`
  - A name such as `test_gof` is what a sibling package would pick too, and
    two packages emitting one class string cannot be told apart by dispatch
  - Renamed the dispatch methods and the precooked fixtures to match
  - Kept the older class names as aliases, so such objects plot as before
  - Documented the convention in CONTRIBUTING, for the whole ecosystem
- Improved `plot.goldfishOutliers()` and `plot.goldfishChangepoints()`
  - Renamed from `plot.outliers.goldfish()` and `plot.changepoints.goldfish()`
  - Read the metadata each object carries rather than inferring it
  - Plot the `.series` column, so a diagnostic called with `effect =` is
    drawn as that term's series rather than as a log-likelihood trace
  - Fixed both to facet on process, so no line crosses a process boundary
  - Fixed `plot.goldfishChangepoints()` to draw each process's own breaks
  - Fixed `plot.goldfishOutliers()` to read the now-logical `outlier` column
  - Rewrote `plot.goldfishChangepoints()` for the tibble with a `cpt` column
    - Labels the axis with break times only where they are numbers,
      so a dated event stream keeps its date scale
- Added precooked `goldfish_margins`, `goldfish_gof`, `goldfish_time`,
  and `goldfish_onset`, and refreshed the two older fixtures
  - Each is stamped with the goldfish version that produced it, 1.9.21
  - `goldfish_outliers` comes from a receiver-choice model of the calls
  - The others come from event models of the `fisheries_treaties` layer
- Replaced `cli::cli_abort()` in `gf_facet_paged()` with `snet_abort()`
- Replaced em dashes in `R/plot_diagnostics.R` since only ASCII is portable
- Fixed signed branch of `plot.matrix()` hard-coding its poles

## Tutorials

- Moved the decorative gifs in the visualisation tutorial into quiz answer feedback, so that they reward an answer in the interactive tutorial rather than appearing in the static pkgdown article, which drops quiz chunks

# autograph 1.1.2

## Package

- Improved the declared dependencies
  - Removed `{knitr}` from Suggests: it was used solely by the tutorial tests, which now extract the tutorials' `{r}` chunks with the same small scanner used in `{manynet}` and `{netrics}` (verified to yield an identical expression set to `knitr::purl()` on the autograph tutorial)
  - Removed `{tidygraph}` from Imports: its only functional use was reading the edgelist's target column in `.infer_end_cap()`, which now uses `igraph::as_edgelist()` (verified to give identical end caps)
  - Promoted `{graphlayouts}` from Suggests to Imports, since it is required for `grapht()`'s *default* "stress" layout (without it, wave-to-wave node transitions silently degraded to a static aggregate layout) and is installed regardless as `{ggraph}` imports it; its `requireNamespace()`/`thisRequires()` guards have been removed
  - Declared a minimum `{manynet}` version (`>= 2.2.1`)
- Updated the GitHub Actions workflows
  - Updated the actions to their latest major versions (`actions/checkout@v7`, `actions/upload-artifact@v7`, `actions/download-artifact@v8`), replacing some long-outdated `@v2` pins
  - Updated the website deploy job's `r-lib/actions/setup-pandoc` from `@v1` to `@v2`, matching every other `r-lib/actions` step
  - Added checks that metadata and tutorial vignettes correspond
- Improved the test suite while reducing what CRAN has to run
  - The functional audits now fail rather than skip when `AUTOGRAPH_STRICT_AUDIT=true`, which the CI check step now sets, so a broken layout or plot method can no longer pass CI green
  - Fixed the layout audit's fixture and argument maps, which paired several layouts with networks they cannot lay out; because `skip()` aborts the enclosing `test_that()`, the first such mismatch had been silently preventing every later layout from being audited at all (the layout audit goes from 21 to 108 assertions)
  - Coverage is now measured with `NOT_CRAN=true`, without which every `skip_on_cran()` test — most of the suite — was skipped while covr ran, badly under-reporting coverage
  - `release` and `pkgdown` no longer run under `if: always()`, so a failing `R CMD check` can no longer tag a release or deploy the website
  - Replaced `graphr()`'s sweep over every bundled `{manynet}` dataset with a representative sample, and dropped the `plot.*` smoke tests now subsumed by the plot-method audit; CRAN-visible test time falls while CRAN-visible assertions rise
  - Added an edge-case audit (`test-functional_errors.R`), an audit of the user-facing `layout_*` aliases, and examples for the `ag_*` palette accessors and configurational layouts, none of which were previously covered
  - Theme-mutating tests now restore the previous theme with `on.exit()`, so global theme state cannot leak between parallel test workers
- Updated the website and README
  - Updated favicons
  - Split Graphing from Plotting functions
  - Updated README to send visualisation examples to the website
  - Fixed README double logoing on pkgdown build, and added alttext
- Updated CONTRIBUTING with the package architecture, conventions, and dev commands
- Updated remaining base R `stop()`, `warning()` and `stopifnot()` calls to the `{manynet}` cli interface

## Graphing

- Improved `graphr()`'s `labels` argument to label a *selection* of the nodes, where previously the only alternative to labelling every node was labelling none
  - `labels` now also accepts a depth of ranks (`labels = 5`), a measure to rank by (`labels = "betweenness"`, or `labels = c(betweenness = 5)` for both), the name of a logical node attribute, or a logical/name/position vector of the nodes to label
  - Selection is by rank rather than by headcount, so nodes tied at the cut are labelled together, using `netrics::node_is_max()`; a two-mode or multilevel network is ranked within each mode or level, so a dense level cannot crowd the others out of the labelling
  - Networks of more than 30 nodes now label only their most central nodes by default, reporting how many; `labels = TRUE` still labels every node. `manynet::fict_marvel` went from 194 overlapping labels to 10
  - Labels are drawn from the selected rows rather than by blanking the rest, so no space is reserved (and nothing is repelled away from) labels that are not drawn
  - `grapht()` resolves the selection once across all waves, so the same nodes stay labelled from frame to frame, and `graphs()` resolves it once for all its panels; `grapht()`'s own default above 30 nodes remains no labels at all
  - `{netrics}` is only suggested, so an automatic selection falls back to a random sample when it is not installed, and a measure asked for by name says what to install
- Fixed `graphs()`/`grapht()` erroring ("Can't combine `..1` <character> and `..2` <logical>") on a longitudinal network whose changing node attributes are stored as non-character vectors (e.g. the logical `active` flag and numeric height/mass in `fict_starwars`)
  - Such networks now split into waves via a guarded `to_waves()` that coerces the offending attributes when `{manynet}`'s splitter cannot combine them
- Fixed `graphr(..., snap = TRUE)` erroring ("'-' only defined for equally-sized data frames") whenever a node sat exactly on a grid point
  - `depth_first_recursive_search()` compared each node against a distance vector that still included its own zero self-distance, so an exact hit selected that entry and yielded an empty grid point; the self-distance is now dropped before the nearest vacant point is chosen
  - two-mode networks hit this on their very first node, since their coordinates are exactly 0 or 1
- Improved `graphr()` to ignore `snap = TRUE` for layered layouts ("hierarchy", "railway", "ladder", "alluvial", "multilevel", "lineage", "layered")
  - These layouts encode rank, mode, or generation along an axis, which square-grid snapping would collapse
- Fixed `graphr()` erroring with "argument \"node_color\" is missing, with no default" when passed a list of networks; the call is now forwarded to `graphs()` as written, rather than argument by argument
- Improved how `graphr()` checks the attribute names given to its aesthetic arguments
  - A mistyped node or tie attribute name now errors immediately, naming the argument and offering the closest match: `graphr(net, node_color = "welth")` reported "Unknown colour name: welth" at draw time, and now reports that "welth" was not found among the node attributes and asks whether "wealth" was meant
  - The same applies to `node_shape` (previously "Shape aesthetic contains invalid value"), `node_size` and `edge_size` (previously "Aesthetics must be either length 1 or the same as the data (8)"), `node_group`, and `edge_color`
- Improved the error when the input is not a network, which now names the argument and the class given, rather than reporting a missing method for `as_tidygraph()`
- Fixed `isolates` being validated only when the network happened to contain isolates, so the same typo errored on one network and was ignored on another
- Fixed `node_size` values between 0 and 1 being silently multiplied by ten; `node_size = 0.5` now means 0.5, while a *vector* of proportions is still rescaled to stay visible
- Fixed `graphs()` producing empty panels by checking `waves` against the number of networks available
- Added a set of internal argument checks (`R/graph_checks.R`) shared by `graphr()`, `graphs()`, `grapht()`, the `layout_*()` functions, and `stocnet_theme()`
  - An unrecognised value now errors immediately, naming the argument and offering the closest match, rather than falling through to `{ggplot2}`, `{grid}`, or `match.arg()` (so `isolates = "drop"` reports `isolates`, rather than "'arg' should be one of ...")
  - A value that differs only in capitalisation is now used as intended, with a note, instead of being rejected: `node_color = "Wealth"` finds the `wealth` attribute
  - Note that these checks are stricter than before: a mistyped attribute name used to be ignored silently or to fail later, and now stops the call
- Updated group-reduction note in `graphr()` and constant-colour note shared by `graphr()` and `grapht()` so that their wording cannot drift

## Plotting

- Improved `plot.node_member()` to draw its dendrogram with `{ggraph}`
  - Passes `hclust` object's own merge heights to the dendrogram layout to reproduce the previous plot's leaf order, merge heights, cluster label colours, and cutpoint line
  - Branches now drawn in `ag_base()` rather than black, matching the height axis and so respecting the active `stocnet_theme()`
  - Removed `{ggdendro}` from Imports
- Updated a stale doc cross-reference in `plot.node_member()`, which pointed at `manynet::node_in_community()`; that function moved to `{netrics}` in manynet 2.0.0
- Updated terse or dead-end messages to say what to do next, including `plot.node_motif()`/`plot.network_motif()` ("Cannot plot these motifs yet, sorry."), `match_color()`, `plot.diff_model()`, and the concentric and hierarchy layouts ("Duplicated nodes in layers!")
- Updated zero-variance note shared by three GOF plot methods so that their wording cannot drift

## Layouts

- Improved the error on an unrecognised `layout`, which now names the argument, suggests the nearest layout, and lists autograph's own layouts, rather than reporting "object 'layout_tbl_graph_stresss' not found"
  - Passing a layout *function* (e.g. `layout = igraph::layout_with_fr`), rather than its name, now says so instead of erroring with "invalid indexing"
- Added checks of the `membership`/`level`/`rank` arguments that the partition layouts require, which name the argument and offer the closest match

## Theming

- Improved `stocnet_theme()` to error with the nearest match on an unrecognised theme name, instead of warning and silently leaving the previous theme in place; giving more than one theme name is also caught

## Tutorials

- Added a colour blindness section to the visualisation tutorial
  - Covers `simulate_colorblind()`, `contrast_colors()`, and how palettes are ordered
  - Notes that the "rainbow" theme is not a colour-blind safe scheme
- Added a note on installing a theme's fonts to the visualisation tutorial
- Updated the README with the case for colour-blind readable palettes
- Updated visualization tutorial to use colour/color consistently
- Updated the Labels section of the visualisation tutorial to cover selecting which nodes to label, replacing the `mutate(name = ifelse(...))` workaround it used to recommend
  - `fict_lotr`, the tutorial's running example, has 36 nodes, so its graphs now name only its most central characters; the surrounding prose says so and shows how to choose otherwise
  - Regenerated `vignettes/articles/visualising-networks.Rmd` and the pre-rendered tutorial HTML to match

# autograph 1.1.1

## Graphing

- Fixed `grapht()` example, which took too long to run, by couching it in `\donttest{}`
- Improved `graphs()` to accept bare longitudinal or dynamic networks
  - Splits it into waves or time slices automatically (consistent with `grapht()`), 
  instead of erroring with "invalid to use names()<- on an S4 object of class 'dgCMatrix'"
- Fixed various errors and warnings in `graphr()`/`graphs()` on signed, directed, or changing networks:
  - Fixed recycling warnings ("longer object length is not a multiple of shorter object length") in `graphr()` on a complex network
    - arc geom's `strength` parameter now excludes loop edges, which are drawn separately by `geom_edge_loop0()`
  - Fixed spurious warning ("no non-missing arguments to max; returning -Inf") in `graphr()` on a changing network without any adoption events (e.g. `fict_potter`)
    - such networks now rendered as standard changing networks rather than routed through the diffusion node-colour mapping
  - Fixed fails at draw time with "invalid hex digit in 'color' or 'lty'" in `graphr()` on a signed multiplex network (e.g. `fict_marvel`)
    - ties on non-signed layers (which have a missing sign) are now drawn solid/positive rather than passing `NA` to grid
  - Fixed error with "Aesthetics must be either length 1 or the same as the data" in `graphr()`/`graphs()` on signed longitudinal or directed networks (e.g. `to_waves(ison_monks)`)
    - per-tie sign linetypes are now mapped through `aes()` (with an identity scale) so ggraph's edge geoms expand and subset them consistently with edge colour and width
  - Fixed `graphr(..., snap = TRUE)` erroring on two-mode networks: their default "hierarchy" layout is now left un-snapped (with an informative message) since collapsing its layered coordinates onto a square grid is not meaningful, while snapping a two-mode network under a force-directed layout continues to work; also hardened the grid-snapping search so a coordinate landing exactly on a grid point is no longer mistaken for a node's zero self-distance

## Tutorials

- Updated visualisation tutorial with a bit more structuring of further options

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
