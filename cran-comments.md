## Test environments

* local R installation, aarch64-apple-darwin20, R 4.5.1
* macOS 14.7.6 (on Github), R 4.5.1
* Microsoft Windows Server 2022 10.0.20348 (on Github), R 4.5.1
* Ubuntu 24.04.2 (on Github), R 4.5.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## User filespace

This version adds an optional `persist` argument to `stocnet_theme()`. When, and only when, a user
passes `persist = TRUE`, the chosen theme is written to `tools::R_user_dir("autograph", "config")`.
Nothing is written on load, on attach, or by any default code path, and the package is fully
functional if the directory is absent or unwritable. No other location is written to.
