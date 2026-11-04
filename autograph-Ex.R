pkgname <- "autograph"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('autograph')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ag_call")
### * ag_call

flush(stderr()); flush(stdout())

### Name: ag_call
### Title: Consistent palette calls
### Aliases: ag_call ag_base ag_ink ag_missing ag_highlight ag_positive
###   ag_negative ag_qualitative ag_sequential ag_divergent ag_font

### ** Examples

# Single colours from the currently active theme
ag_base()
ag_ink()
ag_highlight()
ag_missing()
ag_positive()
ag_negative()
# Palettes of a requested length
ag_qualitative(3)
ag_sequential(5)
ag_divergent(5)
# The accessors follow whichever theme is set
ag_font()



cleanEx()
nameEx("check_colors")
### * check_colors

flush(stderr()); flush(stdout())

### Name: check_colors
### Title: Checking colours for colour blindness and legibility
### Aliases: check_colors check_separation check_contrast

### ** Examples

# How well does the current theme's palette separate five categories?
check_separation(ag_qualitative(5))
# The closest pair in it
min(check_separation(ag_qualitative(5)), na.rm = TRUE)
# And the closest pair once it is printed in greyscale
min(attr(check_separation(ag_qualitative(5)), "grey"), na.rm = TRUE)
# A red and a green that only look different to some viewers
check_separation(c("#B7352D", "#627313"))[1, 2]
# Can the current theme's ink be read on its ground?
check_contrast(ag_ink())[1, 2]



cleanEx()
nameEx("check_layout")
### * check_layout

flush(stderr()); flush(stdout())

### Name: check_layout
### Title: Checking how well a layout draws a network
### Aliases: check_layout check_span check_offset check_stress
###   check_crossings check_slopes check_lengths check_angles check_drawing

### ** Examples

sw <- manynet::ison_southern_women
# Every check at once, for two layouts side by side
circled <- graphr(sw, layout = "circle")
rbind(stress = check_drawing(graphr(sw, layout = "stress")),
      circle = check_drawing(circled))
# How many crossings does the default layout draw?
attr(check_crossings(graphr(sw)), "total")
# How many slopes does a circle draw, and how evenly does it draw its ties?
attr(check_slopes(circled), "distinct")
attr(check_lengths(circled), "cv")
# Is there room between the ties that meet at a node?
attr(check_angles(circled), "min")
# And where the layout has rows, how long and how straight are its ties?
thrones <- manynet::to_uniplex(manynet::fict_thrones, "parent")
drawn <- graphr(thrones)
attr(check_span(drawn), "total")
attr(check_offset(drawn), "mean")



cleanEx()
nameEx("completion")
### * completion

flush(stderr()); flush(stdout())

### Name: completion
### Title: Completing argument values as you type
### Aliases: completion stocnet_completion set_completion

### ** Examples

## Not run: 
##D # In RStudio, switch completion on for this session:
##D stocnet_completion()
##D # Then type graphr(fict_lotr, node_color = " and press Tab.
##D # To switch it off again:
##D stocnet_completion(FALSE)
## End(Not run)



cleanEx()
nameEx("count_pages")
### * count_pages

flush(stderr()); flush(stdout())

### Name: count_pages
### Title: How many pages a paged diagnostic figure has
### Aliases: count_pages

### ** Examples

count_pages(goldfish_gof)



cleanEx()
nameEx("layout_concentric")
### * layout_concentric

flush(stderr()); flush(stdout())

### Name: layout_concentric
### Title: Concentric layout
### Aliases: layout_concentric layout_tbl_graph_concentric

### ** Examples

#graphr(ison_southern_women, layout = "concentric", membership = "type",
#           node_color = "type", node_size = 3)



cleanEx()
nameEx("layout_configuration")
### * layout_configuration

flush(stderr()); flush(stdout())

### Name: layout_configuration
### Title: Layout algorithms based on configurational positions
### Aliases: layout_configuration layout_tbl_graph_configuration
###   layout_dyad layout_triad layout_tetrad layout_pentad layout_hexad

### ** Examples

# "configuration" picks the layout matching the number of nodes
graphr(manynet::create_ring(4), layout = "configuration")
# the specific configurations are also available as functions
layout_tetrad(manynet::create_ring(4))



cleanEx()
nameEx("layout_correspondence")
### * layout_correspondence

flush(stderr()); flush(stdout())

### Name: layout_correspondence
### Title: Correspondence layout
### Aliases: layout_correspondence layout_tbl_graph_correspondence

### ** Examples

graphr(manynet::ison_southern_women, layout = "correspondence")



cleanEx()
nameEx("layout_layered")
### * layout_layered

flush(stderr()); flush(stdout())

### Name: layout_layered
### Title: Layered layouts
### Aliases: layout_layered layout_tbl_graph_layered layout_lineage
###   layout_tbl_graph_lineage layout_railway layout_tbl_graph_railway
###   layout_ladder layout_tbl_graph_ladder

### ** Examples

#graphr(ison_southern_women, layout = "layered", center = "events",
#           node_color = "type", node_size = 3)
#graphr(ison_southern_women, layout = "lineage")
# ison_adolescents |>
#   mutate_nodes(year = rep(c(1985, 1990, 1995, 2000), times = 2)) |>
#   graphr(layout = "lineage", ranks = "year")



cleanEx()
nameEx("layout_levels")
### * layout_levels

flush(stderr()); flush(stdout())

### Name: layout_levels
### Title: Levels layout
### Aliases: layout_levels layout_tbl_graph_levels

### ** Examples

# fict_marvel interlocks a one-mode layer of ties among its characters
# with a two-mode layer of their affiliations, so it is laid out this way
# by default; the levels need not be named.
graphr(manynet::fict_marvel, labels = FALSE)



cleanEx()
nameEx("layout_scaling")
### * layout_scaling

flush(stderr()); flush(stdout())

### Name: layout_scaling
### Title: Scaling layout
### Aliases: layout_scaling layout_tbl_graph_scaling

### ** Examples

graphr(manynet::ison_southern_women, layout = "scaling")



cleanEx()
nameEx("layout_spatial")
### * layout_spatial

flush(stderr()); flush(stdout())

### Name: layout_spatial
### Title: Spatial layout
### Aliases: layout_spatial layout_tbl_graph_spatial

### ** Examples

net <- manynet::create_ring(4) |>
  manynet::mutate_nodes(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
graphr(net)



cleanEx()
nameEx("layout_valence")
### * layout_valence

flush(stderr()); flush(stdout())

### Name: layout_valence
### Title: Valence layout
### Aliases: layout_valence layout_tbl_graph_valence

### ** Examples

edges <- data.frame(
  from = c("A", "B", "C", "D"),
  to   = c("B", "C", "D", "A"),
  weight = c(2, 3, 1, 4),
  sign = c(1, -1, 1, -1)  # 1 = positive, -1 = negative
  )
graphr(as_igraph(edges), layout="valence")



cleanEx()
nameEx("list_fonts")
### * list_fonts

flush(stderr()); flush(stdout())

### Name: list_fonts
### Title: Listing the fonts available to R
### Aliases: list_fonts

### ** Examples

head(list_fonts())



cleanEx()
nameEx("map_measure")
### * map_measure

flush(stderr()); flush(stdout())

### Name: map_measure
### Title: Plotting logical marks Plotting numeric measures
### Aliases: map_measure plot.node_measure plot.tie_measure
###   plot.network_measures

### ** Examples

plot(netrics::node_by_deg(ison_karateka))
plot(netrics::tie_by_betweenness(ison_karateka))



cleanEx()
nameEx("map_member")
### * map_member

flush(stderr()); flush(stdout())

### Name: map_member
### Title: Plotting categorical memberships
### Aliases: map_member plot.node_member plot.matrix

### ** Examples

plot(netrics::node_in_walktrap(ison_southern_women, "e"))
plot(as_matrix(ison_adolescents),
  membership = netrics::node_in_walktrap(ison_adolescents, "e"))
plot(as_matrix(ison_southern_women),
  membership = netrics::node_in_walktrap(ison_southern_women, "e"))



cleanEx()
nameEx("mdate_scales")
### * mdate_scales

flush(stderr()); flush(stdout())

### Name: mdate_scales
### Title: Scales for messy dates
### Aliases: mdate_scales scale_x_mdate scale_y_mdate

### ** Examples

## Don't show: 
if (requireNamespace("messydates", quietly = TRUE)) withAutoprint({ # examplesIf
## End(Don't show)
library(ggplot2)
dates <- messydates::as_messydate(c("2012-01-01", "2012-06", "2013~"))
df <- data.frame(date = dates, y = 1:3)
ggplot(df, aes(x = date, y = y)) + geom_point()
ggplot(df, aes(x = date, y = y)) + geom_point() +
  scale_x_mdate(FUN = messydates::vmax, date_labels = "%Y-%m")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("model_mrqap")
### * model_mrqap

flush(stderr()); flush(stdout())

### Name: model_mrqap
### Title: Plotting methods for MRQAP models
### Aliases: model_mrqap plot.netreg plot.netlm plot.netlogit

### ** Examples

# Here's something I cooked up with infernet earlier:
plot(res_infernet_reg)
# And with an earlier version of migraph:
plot(res_migraph_reg)



cleanEx()
nameEx("plot.diffusion")
### * plot.diffusion

flush(stderr()); flush(stdout())

### Name: plot.diffusion
### Title: Plotting diffusion models
### Aliases: plot.diffusion plot.diff_model plot.diffs_model
###   plot.learn_model

### ** Examples

plot(res_manynet_diff)
plot(res_migraph_diff)
plot(play_learning(ison_networkers, beliefs = runif(net_nodes(ison_networkers))))



cleanEx()
nameEx("plot.infernetConfusion")
### * plot.infernetConfusion

flush(stderr()); flush(stdout())

### Name: plot.infernetConfusion
### Title: Plotting how well a model classifies ties
### Aliases: plot.infernetConfusion

### ** Examples

# Needs 'infernet' (https://github.com/stocnet/infernet)
## Not run: 
##D   fit <- infernet::estimate_mrqap(. ~ ego(Age) + alter(Age),
##D                                   manynet::ison_adolescents |>
##D                                     manynet::mutate_nodes(Age = 12:19),
##D                                   times = 20, control = list(seed = 1))
##D   plot(infernet::diagnose_confusion(fit, times = 100))
## End(Not run)



cleanEx()
nameEx("plot.network_test")
### * plot.network_test

flush(stderr()); flush(stdout())

### Name: plot.network_test
### Title: Plotting methods for CUG and QAP tests
### Aliases: plot.network_test

### ** Examples

# Here's something I cooked up with migraph earlier:
plot(res_migraph_test)



cleanEx()
nameEx("plot_adequacy")
### * plot_adequacy

flush(stderr()); flush(stdout())

### Name: plot.diagnose_outliers
### Title: Plotting adequacy diagnostics
### Aliases: plot.diagnose_outliers plot.outliers.goldfish
###   plot.diagnose_changepoints plot.changepoints.goldfish plot_adequacy
###   plot.goldfishOutliers plot.goldfishChangepoints plot.goldfishMargins
###   plot.goldfishGOF plot.goldfishTimeTest plot.goldfishOnset

### ** Examples

plot(goldfish_outliers)
plot(goldfish_changepoints)
plot(goldfish_margins)
plot(goldfish_gof)
plot(goldfish_time)
plot(goldfish_onset)



cleanEx()
nameEx("plot_convergence")
### * plot_convergence

flush(stderr()); flush(stdout())

### Name: plot_convergence
### Title: Plotting convergence diagnostics
### Aliases: plot_convergence plot.ag_conv plot.traces.monan plot.ergm
###   load_ergm_res

### ** Examples

plot(monan_conv)
ergm_res <- load_ergm_res()
plot(ergm_res)



cleanEx()
nameEx("plot_gof")
### * plot_gof

flush(stderr()); flush(stdout())

### Name: plot_gof
### Title: Plotting goodness-of-fit results
### Aliases: plot_gof plot.ag_gof plot.gof.stats.monan plot.sienaGOF
###   plot.gof.ergm

### ** Examples

plot(monan_gof)
plot(siena_gof, cumulative = TRUE)
plot(ergm_gof, statistic = "espart")



cleanEx()
nameEx("plot_goldfish_fit")
### * plot_goldfish_fit

flush(stderr()); flush(stdout())

### Name: plot.result.goldfish
### Title: Plotting a goldfish model fit at a glance
### Aliases: plot.result.goldfish plot_goldfish_fit plot.goldfishFit

### ** Examples

plot(goldfish_fit)



cleanEx()
nameEx("plot_graphr")
### * plot_graphr

flush(stderr()); flush(stdout())

### Name: plot_graphr
### Title: Easily graph networks with sensible defaults
### Aliases: plot_graphr graphr

### ** Examples

graphr(ison_adolescents)
ison_adolescents |>
  mutate_nodes(color = rep(c("introvert","extrovert"), times = 4),
         size = c(3, 6, 3, 3, 3, 6, 6, 3)) |>
  mutate_ties(ecolor = rep(c("friends", "acquaintances"), times = 5)) |>
  graphr(node_color = "color", node_size = "size",
         edge_size = 1.5, edge_color = "ecolor")
graphr(ison_southern_women, labels = TRUE, label_dist = 10)
# Label a selection of the nodes rather than all of them
graphr(ison_southern_women, labels = 2)
graphr(ison_adolescents, labels = c("Alice", "Betty"))
## No test: 
graphr(ison_southern_women, labels = TRUE, label_repel = FALSE)
graphr(ison_southern_women, labels = "betweenness")
graphr(manynet::generate_random(40, 0.1), edge_bundle = TRUE)
# Larger arrowheads than the width of the ties would give, or none at all
graphr(ison_networkers, edge_arrows = 4)
graphr(ison_networkers, edge_arrows = FALSE)
graphr(manynet::generate_random(80, 0.2), backbone = TRUE)
## End(No test)



cleanEx()
nameEx("plot_graphs")
### * plot_graphs

flush(stderr()); flush(stdout())

### Name: plot_graphs
### Title: Easily graph a set of networks with sensible defaults
### Aliases: plot_graphs graphs

### ** Examples

#graphs(to_egos(ison_adolescents))
#graphs(to_egos(ison_adolescents), waves = 8)
#graphs(to_egos(ison_adolescents), waves = c(2, 4, 6))
#graphs(play_diffusion(ison_adolescents))



cleanEx()
nameEx("plot_grapht")
### * plot_grapht

flush(stderr()); flush(stdout())

### Name: plot_grapht
### Title: Easily animate dynamic networks with sensible defaults
### Aliases: plot_grapht grapht print.grapht

### ** Examples

# A dynamic signed network of shifting European alliances 1872-1918,
# split automatically into snapshots of the ties active in each spell.
# Wrapped in \donttest{} because rendering the animation to a .gif is
# slow, not because the code is unsafe to run.
## No test: 
grapht(irps_wwi)
## End(No test)



cleanEx()
nameEx("plot_interp")
### * plot_interp

flush(stderr()); flush(stdout())

### Name: plot_interp
### Title: Plotting effects interpretation
### Aliases: plot_interp plot.selectionTable plot.influenceTable

### ** Examples

plot(siena_selection)
plot(siena_selection) + ggplot2::scale_colour_discrete()
plot(siena_influence)



cleanEx()
nameEx("theme_colorblind")
### * theme_colorblind

flush(stderr()); flush(stdout())

### Name: theme_colorblind
### Title: Simulating how colours look to a colour-blind viewer
### Aliases: theme_colorblind simulate_colorblind

### ** Examples

simulate_colorblind(c("#d73027", "#4575b4"), "deutan")
# A milder deuteranomaly, and the same colours in greyscale
simulate_colorblind(c("#d73027", "#4575b4"), "deutan", severity = 0.5)
simulate_colorblind(c("#d73027", "#4575b4"), "grey")



cleanEx()
nameEx("theme_match")
### * theme_match

flush(stderr()); flush(stdout())

### Name: theme_match
### Title: Matching colors across palettes
### Aliases: theme_match match_color is_dark

### ** Examples

match_color("#4575b4")
is_dark(c("#000","#FFF"))



cleanEx()
nameEx("theme_medium")
### * theme_medium

flush(stderr()); flush(stdout())

### Name: theme_medium
### Title: Setting the medium a plot is made for
### Aliases: theme_medium stocnet_medium set_stocnet_medium ag_size

### ** Examples

stocnet_medium("presentation")
ag_size()
stocnet_medium("screen")



cleanEx()
nameEx("theme_set")
### * theme_set

flush(stderr()); flush(stdout())

### Name: theme_set
### Title: Setting a consistent theme for all plots
### Aliases: theme_set stocnet_theme set_stocnet_theme

### ** Examples

stocnet_theme("default")
plot(netrics::node_by_degree(ison_karateka))
stocnet_theme("uzh")
plot(netrics::node_by_degree(ison_karateka))



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
