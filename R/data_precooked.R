#' Precooked results for demonstrating plotting
#' @name made_earlier
#' @description
#'   These are all pre-cooked results objects, saved here to save time in
#'   testing and demonstrating how autograph plots look.
NULL

# infernet objects ####

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(res_infernet_reg)
"res_infernet_reg"

# netrics objects ####

#' @details
#'   `res_netrics_robust` is a list of two robustness results from
#'   `netrics::net_by_robustness()`, each a "network_measure" with a "curve"
#'   attribute, on a 50-node scale-free network:
#'   `failure`, robustness to random failure (100 simulations),
#'   and `attack`, robustness to an adaptive attack on degree.
#'   It was made with:
#'   ```r
#'   set.seed(1234)
#'   g <- manynet::generate_scalefree(50, 1)
#'   res_netrics_robust <- list(
#'     failure = netrics::net_by_robustness(g, times = 100),
#'     attack = netrics::net_by_robustness(g,
#'                node_measure = netrics::node_by_degree))
#'   ```
#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(res_netrics_robust)
"res_netrics_robust"

# migraph objects ####

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(res_migraph_reg)
"res_migraph_reg"

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(res_migraph_test)
"res_migraph_test"

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(res_migraph_diff)
"res_migraph_diff"

# manynet objects ####

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(res_manynet_diff)
"res_manynet_diff"

# RSiena objects ####

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(siena_gof)
"siena_gof"

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(siena_influence)
"siena_influence"

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(siena_selection)
"siena_selection"

# MoNAn objects ####

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(monan_conv)
"monan_conv"

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(monan_gof)
"monan_gof"

# ergm objects ####

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(ergm_gof)
"ergm_gof"

# goldfish objects ####

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(goldfish_outliers)
"goldfish_outliers"

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(goldfish_changepoints)
"goldfish_changepoints"

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(goldfish_margins)
"goldfish_margins"

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(goldfish_gof)
"goldfish_gof"

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(goldfish_time)
"goldfish_time"

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(goldfish_onset)
"goldfish_onset"

#' @docType data
#' @keywords datasets
#' @rdname made_earlier
#' @usage data(goldfish_fit)
"goldfish_fit"

