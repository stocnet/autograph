# Contributing

Contributions to `autograph`, 
whether in the form of issue identification, bug fixes, new code or documentation 
are encouraged and welcome.

Please note that the `autograph` project is released with a 
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). 
By contributing to this project, you agree to abide by its terms.

## Git

`stocnet` projects are maintained using the git version control system.
A plain-English introduction to git can be found [here](https://blog.red-badger.com/2016/11/29/gitgithub-in-plain-english).
I recommend you read this before continuing. 
A more recent motivation can be found [here](https://www.r-bloggers.com/2024/04/git-gud-version-control-best-practices/).
It will explain the basics of git version control, committing and repos, pulling and pushing,
branching and merging.

### Fork

Using git from the command line on your lap- or desktop can be intimidating,
but I recommend [Fork](https://git-fork.com) software for Mac and Windows.
This allows mostly visual management of commits, diffs, branches, etc.
There are various other git software packages available, but this one is fairly fully featured.

The Github page allows to access the issues assigned to you and check the commits.
You can also access the documents in the repository, 
although this won't be necessary after you have cloned it on your computer via Fork.

### Cloning

Once you have downloaded Fork, the first thing you have to do is to 
clone the remote repository on your computer. 
Before cloning, you will be able to choose on which `branch` you want to work: 
develop or main. 

### Pull 

This command allows you to `pull` changes from the remote repository to your local repository on Sourcetree.
Make sure you do that before starting working on your files so you have the newest versions. 
When pulling, make sure you choose master or develop, 
depending on the branch you decided to work with. 
Once you pulled, you have now all the new commits and files and 
you can start working on your assigned tasks.
Note that you can access and open the files either from the Finder or from Fork. 
Some documents might be stored using Large File Storage (LFS) to save space on the repository. 

### Commit and Push

Once you have made modifications on a file and saved them, it will appear in your `commit` window. 
Here you can control one last time your file, write the commit message with the 
issue reference (see below) and commit. 
Once your commit is ready, you can `push` them to the origin/main repository.
Note that you can click the "push immediately" box in the commit window 
if you don't want to do it in two steps. 
If you are working on a separate branch, 
it is important to select this branch when pushing to origin/main.

### Branching and CI

- `main` is the release branch; `develop` is the working branch (clone/work on `develop`).
- PRs into `main` trigger [prchecks.yml](workflows/prchecks.yml): R CMD check (macOS/Windows/Linux), binary build, codecov, lintr, spell check, a check that vignette articles and tutorials stay in sync, and PR metadata checks (DESCRIPTION version bump, PR title/description conventions).
- Merges/pushes to `main` trigger [pushrelease.yml](workflows/pushrelease.yml): check, auto-bump version tag, GitHub release with binaries, then pkgdown site deploy.

## Package architecture

### Project overview

`autograph` is an R package (part of the [stocnet](https://github.com/stocnet) ecosystem) providing the *visual layer* for network analysis:
automatic `ggplot2`/`ggraph`-based plotting and consistent theming for network data and network-analytic results.
It offers three graph-drawing entry points (`graphr()`, `graphs()`, `grapht()`)
plus a large family of `plot()` S3 methods dispatched on result objects from other packages
(`migraph`, `netrics`, `RSiena`, `ergm`, `MoNAn`, `goldfish`).
Division of labour to keep in mind when adding functions:

- `{manynet}`: network classes/coercion (`as_*()`) and network-level logical tests (e.g. `is_directed()`, `is_twomode()`).
- `{netrics}`: everything analytic — marks, measures, memberships, motifs — at the node, tie, and network level.
- `{autograph}` (this package): drawing graphs and plotting analytic, modelling, or diagnostic results, along with deep (often institutional) theming. *All* plot methods should live here.
- `{migraph}`: testing and modelling, e.g. QAP/MRQAP and diffusion models.

In terms of style, we are aiming for sensible defaults in terms of user experience.
As a ggplot2 object, most everything can be tweaked before or afterwards,
but the default presentation should already be aesthetically pleasing,
informative, and consistent.

### Common commands

This is a standard R package developed with `devtools`/`roxygen2`.
Run these from an R console with the working directory set to the package root (or via `Rscript -e`).

- Load package for interactive development: `devtools::load_all()`
- Regenerate docs & NAMESPACE after editing roxygen comments: `devtools::document()`
- Run full test suite: `devtools::test()`
- Run a single test file: `devtools::test(filter = "graphr")` (matches `test-graphr.R`), or `testthat::test_file("tests/testthat/test-plot_gof.R")`
- Full package check (mirrors CI): `devtools::check()` or `rcmdcheck::rcmdcheck()`
- Lint: `lintr::lint_package()`
- Spell check: `spelling::spell_check_package()`
- Rebuild `README.md` from `README.Rmd`: `devtools::build_readme()`
- Build pkgdown site locally: `pkgdown::build_site()`

There is no non-R build system — no package.json/Makefile.
Roxygen is configured with `markdown = TRUE`; `NAMESPACE` and all `man/*.Rd` files are generated — never hand-edit them.

### Graph drawing pipeline (`graphr()`/`graphs()`/`grapht()`)

`graphr()` ([R/graphr.R](../R/graphr.R)) is the single-network entry point and the core of the package.
It accepts any `{manynet}`-compatible network object and builds a `ggplot2`/`ggraph` plot
through an internal pipeline of helper functions, each in its own file:

- `graph_layout()` ([R/graph_layout.R](../R/graph_layout.R)) — resolves the layout algorithm (igraph/ggraph/graphlayouts, or autograph's own layouts), builds the `ggraph` layout object, and optionally snaps coordinates to a grid.
- `graph_nodes()`, `graph_edges()`, `graph_labels.R`, `graph_legends.R`, `graph_aes.R`, `graph_checks.R` — each layer adds/styles one visual component (node aes, edge aes, text labels, legends) onto the `ggraph` plot, resolving arguments that may be literal values (e.g. `node_size = 6`) or references to a node/tie attribute name (e.g. `node_size = "wealth"`).
- Note that users are not expected to call any of these `graph_*()` functions themselves; exported modularity makes development, debugging, and testing easier.

`graphs()` ([R/graphs.R](../R/graphs.R)) calls `graphr()` per network in a list and arranges the results with `{patchwork}`
(deliberately chosen over gridExtra/cowplot/ggpubr because it doesn't interfere with ggplot2 themes —
see the comment in [R/autograph_utilities.R](../R/autograph_utilities.R)).

`grapht()` ([R/grapht.R](../R/grapht.R)) animates a longitudinal/dynamic network over time using `{gganimate}`/`{gifski}`.

Custom layout algorithms not provided by igraph/ggraph/graphlayouts live in their own `layout_*.R` files
(`layout_configurational.R`, `layout_grid.R`, `layout_layered.R`, `layout_matching.R`,
`layout_partition.R`, `layout_valence.R`) and follow the `layout_tbl_graph_*()` naming convention.

### Plot-method dispatch (`plot_*.R`)

The rest of the package implements `plot.<class>` S3 methods so that `plot(result)` "just works"
for objects returned by other stocnet/network packages,
without the user needing to know which autograph function to call.
Methods are grouped by the *kind of result object*, not by source package:

| File | Covers |
|---|---|
| `plot_analysis.R` | node/tie/network measures, motifs, memberships (`node_measure`, `tie_measure`, `network_measures`, `node_member`, `node_motif`, `network_motif`, `matrix`) |
| `plot_summaries.R` | diffusion/learning model summaries (`diff_model`, `diffs_model`, `learn_model`, `mnet`) |
| `plot_gof.R` | goodness-of-fit objects (`gof.ergm`, `sienaGOF`, `gof.stats.monan`, autograph's own `ag_gof`) |
| `plot_diagnostics.R` | adequacy diagnostics and model fits, currently goldfish's (`goldfishOutliers`, `goldfishChangepoints`, `goldfishOnset`, `goldfishMargins`, `goldfishGOF`, `goldfishTimeTest`, `goldfishFit`) |
| `plot_convergence.R`, `plot_tests.R`, `plot_interp.R` | convergence traces, statistical tests, and interpretation plots for `netlm`/`netlogit`/`ergm` etc. |
| `plot_manydata.R` | 'many' data plots; the whole file is commented out at present |

New `plot.*` methods must be registered in NAMESPACE via roxygen `@method`/`@export` tags — run `devtools::document()` after adding one.
Suggestions for new plot methods are welcome.

### Class names across the stocnet ecosystem

S3 dispatch matches exact class strings, so two packages that emit the same class string
collide: `autograph` cannot tell the objects apart, and neither can a user's `inherits()` check.
A name such as `test_gof` or `margin_table` is the name any sibling package would pick for the
same idea, so it is not safe.

The rule for every stocnet package is: **name a class after the package plus a noun, in camelCase**
(`<pkg><Thing>`), following RSiena's `sienaFit`, `sienaGOF` and `sienaAlgorithm`.
camelCase keeps a class visually distinct from the snake_case user-facing functions.

Two things the convention does not use:

- **No dot suffix.** A dot in a class string creates no inheritance. R dispatches on exact class
  strings, and all S3 inheritance comes from the class vector, so `foo.goldfish` does not match an
  object of class `"diagnose_outliers.goldfish"`. A suffix such as `.goldfish` is convention only.
- **No shared parent class.** `autograph` draws a different figure for each diagnostic, so a
  fallback method would have nothing to do. `autograph` standardises by coercion instead, as it
  already does for other objects.

The goldfish diagnostic classes follow this rule.
Five older class names remain in [R/autograph-defunct.R](../R/autograph-defunct.R) as aliases
forwarding to the renamed methods, so that an object classed the way an earlier autograph
expected still plots:
`diagnose_outliers` and `diagnose_changepoints` (the names goldfish 1.9.21 stamps),
`outliers.goldfish` and `changepoints.goldfish` (the two the draft methods were written against),
and `result.goldfish` (the fit class every goldfish stamps, back to the version on CRAN).
An alias restores dispatch, not the old column contract: each forwards to a method that reads the
current columns.
Delete each alias once the oldest supported goldfish is past the rename.

### Function names

Two naming families, and they do not mix:

- **User-facing functions are snake_case, and usually `verb_noun`**: `graphr()`, `match_color()`,
  `is_dark()`, `simulate_colorblind()`, `contrast_colors()`, `list_fonts()`, `stocnet_theme()`.
  This is the convention across the stocnet suite, so a user meets one style everywhere.
- **The `ag_` prefix is for the theme accessors only**: `ag_base()`, `ag_ink()`, `ag_highlight()`,
  `ag_positive()`, `ag_negative()`, `ag_qualitative(n)`, `ag_sequential(n)`, `ag_divergent(n)`,
  `ag_font()`. Each returns the autograph-specific value the current theme holds for one role,
  and each reads an `snet_*` option. Do not give `ag_` to a function that does something else,
  even a small one: a new verb belongs in the snake_case family.
  Internal helpers may take `ag_` where they build such a value (`ag_ground()`, `ag_theme_*()`).

### Theming

[R/theme_palette_set.R](../R/theme_palette_set.R) implements `stocnet_theme()` (alias `set_stocnet_theme()`),
which sets an R option (`stocnet_theme`, default `"default"`) read by every plotting function in the package.
Institutional and stylistic palettes (`default`, `bw`, `crisp`, `neon`, `clay`, `iheid`, `ethz`, `uzh`, `rug`,
`unibe`, `oxf`, `unige`, `cmu`, `iast`, `hwu`, `rainbow`) are defined in [R/theme_palette_set.R](../R/theme_palette_set.R)
and exposed via the `ag_` accessors listed above, documented together under `ag_call`.
Users can override individual palette colours via `options()` (e.g. `options(snet_highlight = ...)`)
rather than editing theme code.
[R/theme_match.R](../R/theme_match.R) maps a plot/result object to its appropriate theme treatment.

Three roles, kept separate, because they pull in different directions:
the **base** is an unhighlighted mark, and may be light where that is what separates it from a
dark brand highlight; the **ink** (`ag_ink()`) is what a plot writes with, and must stay legible;
the **highlight** is the brand colour.
Reference lines, axis text, and other chrome take `ag_ink()`, never `ag_base()`.

Every plot is drawn on the theme's ground. Build plot themes with the `ag_theme_*()` wrappers in
[R/theme_palette_set.R](../R/theme_palette_set.R) (`ag_theme_minimal()`, `ag_theme_void()`, and so on) rather than
calling `ggplot2::theme_minimal()` directly, so that a theme with a background other than white
reaches every plot and not only the graphs.

[R/theme_colorblind.R](../R/theme_colorblind.R) holds the colour-blindness tools: `simulate_colorblind()` and
`contrast_colors()`, and the internal `colorblind_sort()` that each theme's categorical palette passes
through when the theme is set.
A palette added to a theme therefore does not need hand-ordering, but it does need to survive the
audit in `tests/testthat/test-functional_themes.R`, which requires the first few colours to stay
apart under each type of colour blindness.
A palette whose own order carries meaning is exempted by adding it to `colorblind_unsorted`;
`"rainbow"` is the only member, and is sampled across its length instead of taken from the front.

The **medium** is separate from the theme, and lives in
[R/theme_medium.R](../R/theme_medium.R): `stocnet_medium()` says where a plot will be seen
(`"screen"`, `"presentation"`, `"mobile"`, `"print"`), not how it should look.
It scales text through `ag_size()` and `ag_text_size()`, and `"print"` overrides the ground to white.
Text set on a geom or on a theme element directly does not pass through `base_size`, so wrap it in
`ag_text_size()`; marks are deliberately left unscaled, since a node's size is relative to its layout.

Because `autograph` re-exports several `ggplot2` symbols (see [R/reexports_ggplot2.R](../R/reexports_ggplot2.R)),
loading `autograph` last in a session is recommended so its `plot()` methods take precedence over other packages'.

### Precooked/vignette data

`data/*.rda` holds pre-computed example results (ERGM/SAOM GOF, goldfish changepoints/outliers,
MoNAn convergence/GOF, migraph diffs/regressions/tests) used in examples, vignettes, and tests,
documented in [R/data_precooked.R](../R/data_precooked.R).
`inst/extdata` holds a serialized `ergm_res` object loaded via `load_ergm_res()`,
deliberately kept out of `data/` and serialized rather than stored as a live object
because namespace references inside `ergm` model objects don't survive a plain `save()`/reload across package versions.

### Dependencies

`autograph` `Depends` on `manynet` (network data structures and coercion) and
`Imports` `ggplot2` (>= 4.0.0), `ggraph`, `graphlayouts`, `igraph`, `dplyr`, and `patchwork`.
`ergm` and `RSiena` are listed under `Enhances` (their `plot.*` methods are only invoked if those
packages are installed and such results are passed in), and `gganimate`, `gifski`, `ggforce`,
`migraph`, and `netrics` are `Suggests`-only,
so code paths depending on them should guard with `requireNamespace()`
(see the `thisRequires()` helper in [R/autograph_utilities.R](../R/autograph_utilities.R))
or be skipped gracefully when unavailable.

The declared minimum of each `stocnet` dependency is the version on CRAN, so that CI can install it.
Where `autograph` needs something that only a newer, unreleased version has,
reach it through a shim in [R/autograph_utilities.R](../R/autograph_utilities.R)
rather than by raising the minimum.
Test for the function with `.ag_has_manynet()` rather than for the version string,
because a pre-release development build can carry the version without yet exporting the function.
Call the function with `getExportedValue()` and not `::`,
because `R CMD check` resolves a `::` call against the installed package
and reports the newer name as missing even where the call is never reached.
Delete each shim once the minimum is raised past the version that added the function.

### Tests

`tests/testthat/` uses testthat edition 3 with parallel execution
(`Config/testthat/parallel: true` in DESCRIPTION).
`tests/testthat.R` sets `stocnet_theme("default")` before running the suite so theme state doesn't leak between runs.
Test files are organised by the same grouping as the `R/` source files
(e.g. `test-graphr.R`, `test-layout_partition.R`, `test-theme_match.R`).

In addition, the `test-functional_*.R` files implement *functional* (family-enumerating) testing,
mirroring the approach in `{manynet}`: layout algorithms, `plot.<class>` methods, palette accessors,
and `graphr()`'s aesthetic arguments are enumerated automatically from the namespace and run over a
fixture grid, so new layouts/methods/palettes are audited without writing new tests.
Helpers live in [tests/testthat/helper-functional.R](../tests/testthat/helper-functional.R).
Non-conformant combinations are *skipped* with a greppable `AUDIT [...]` message rather than failed,
so the audits double as a to-do list locally;
CI sets `AUTOGRAPH_STRICT_AUDIT: true` so the same cases fail there instead.
[tests/testthat/helper-tutorials.R](../tests/testthat/helper-tutorials.R) extracts and evaluates the
code chunks of the learnr tutorials in `inst/tutorials/`, so tutorial code that errors or raises a
deprecation warning fails the suite (rendering the tutorials themselves is deliberately not tested).

### Tutorials and articles

The learnr tutorials in `inst/tutorials/` are the source.
`vignettes/articles/*.Rmd` are their static pkgdown twins, and are *generated*
from them by [data-raw/build_tutorial_articles.R](../data-raw/build_tutorial_articles.R).
Never edit an article by hand: the next regeneration discards the edit,
and [prchecks.yml](workflows/prchecks.yml) fails the PR for drift meanwhile.

After adding or changing functionality, ask whether a reader learning the
package would meet it, and if so:

1. Edit the tutorial in `inst/tutorials/<tute>/*.Rmd`.
   Add the new function to the topic it belongs to, in an `exercise=TRUE`
   chunk, with a sentence saying what it is for.
   New sections need an entry in that topic's page-toc,
   and are worth a line in its closing "In brief" callout.
2. Re-render the tutorial HTML in place
   (`rmarkdown::render()` on the tutorial `.Rmd`), and commit it.
3. Re-run `Rscript data-raw/build_tutorial_articles.R`, and commit the
   regenerated article.
4. Run `testthat::test_file("tests/testthat/test-tutorials_autograph.R")`,
   which purls and evaluates every chunk, so new tutorial code is tested.

Where the change is worth showing off rather than only teaching,
it also belongs in `README.Rmd` — which is knit to `README.md` with
`devtools::build_readme()`, never edited directly — and its figures land in
`man/figures/`, from where the website serves them.

### Website

The site is built by `{pkgdown}` from [pkgdown/_pkgdown.yml](../pkgdown/_pkgdown.yml)
and deployed from [pushrelease.yml](workflows/pushrelease.yml) on a merge to `main`.

**Every exported function must appear in the `reference:` index.**
A topic left out of it fails the build, so the site stops updating.
Add a new function to the section it belongs to,
or add a new section where it starts a family,
and prefer naming the topic (`theme_colorblind`) over widening a `starts_with()` pattern.
A helper that users are not meant to call takes `@keywords internal` instead.
The `reference:` titles are also the headings used in `NEWS.md` (see below),
so keep the two in step.

Check before opening a PR:

```r
pkgdown::check_pkgdown()          # every topic is in the index
pkgdown::build_site(preview = FALSE)  # everything else
```

[prchecks.yml](workflows/prchecks.yml) runs both in the `website-builds` job,
so a PR reports whether the site *can* be built without deploying it.

### `NEWS.md` conventions

`NEWS.md` groups each version's changes under `##` headings that mirror the website
function overview (`pkgdown/_pkgdown.yml` `reference:` titles).
Lead with `## Package` (package-wide/website/infrastructure changes),
then the function families in overview order:
`## Graphing` (graphr/graphs/grapht and internal functions, `## Plotting` (all plot.* methods), `## Layouts`, `## Theming`.
Put `## Tutorials` and `## Data` near the end.
Each heading appears at most once per version.

Start each bullet with a verb matching the change type:

- `Added ...` — new functionality
- `Fixed ...` — bug fixes; if it relates to a GitHub issue, suffix with `(closing #123)`
- `Renamed ... to ...` — function or data name migrations
- `Improved ...` — functional updates to existing behaviour
- `Updated ...` — documentation changes

Any of these verbs can also lead a sub-bullet.

Keep every bullet to one line of fewer than 81 characters ideally
(a few more or less is fine).
If a bullet wraps, it holds too much:
shorten it, or split it into a lead bullet and sub-bullets.
Each bullet stands on its own, and states what changed,
not why or how unless there is space for context.
Explanation belongs in the function documentation or the vignettes.

Where several bullets describe parallel changes, reuse the sentence structure,
so that a reader sees the parallelism at a glance.
Use one word for one thing throughout a version's entries,
rather than varying the wording for effect.

If a cited GitHub issue was **not** authored by @jhollway, thank the author with an
`@`-tag in the bullet.
Cluster related changes (e.g. several fixes to the same function, or sub-points of one
feature) as indented sub-bullets under a lead bullet, to improve readability.
Where several changes concern one function, lead with an `Improved ...` bullet that
names the function, and put the individual `Fixed ...`/`Added ...` points beneath it,
so the cluster groups by function rather than by change type.
Under an `Improved ...` lead bullet, do not name the function again in the
sub-bullets, since the lead bullet already carries it.
Sub-bullets indent by two spaces, and nest at most one level further (four spaces).
A sub-bullet does not need a verb: it can state the consequence, the previous
behaviour, or an example call.
The more entries a version holds, the more this structure matters,
so group first and only then write the bullets.



