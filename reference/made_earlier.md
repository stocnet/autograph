# Precooked results for demonstrating plotting

These are all pre-cooked results objects, saved here to save time in
testing and demonstrating how autograph plots look.

## Usage

``` r
data(res_migraph_reg)

data(res_migraph_test)

data(res_migraph_diff)

data(res_manynet_diff)

data(siena_gof)

data(siena_influence)

data(siena_selection)

data(monan_conv)

data(monan_gof)

data(ergm_res)

data(ergm_gof)
```

## Format

An object of class `netlm` of length 15.

An object of class `network_test` of length 9.

An object of class `diffs_model` (inherits from `data.frame`) with 20
rows and 11 columns.

An object of class `diff_model` (inherits from `tbl_df`, `tbl`,
`data.frame`) with 4 rows and 10 columns.

An object of class `sienaGOF` of length 1.

An object of class `influenceTable` (inherits from `data.frame`) with 25
rows and 4 columns.

An object of class `selectionTable` (inherits from `data.frame`) with 25
rows and 4 columns.

An object of class `traces.monan` of length 3.

An object of class `gof.stats.monan` of length 2.

An object of class `ergm` of length 35.

An object of class `gof.ergm` (inherits from `gof`) of length 30.
