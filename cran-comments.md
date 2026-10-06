## Test environments

* local R installation, aarch64-apple-darwin23, R 4.6.1
* macOS 26.5.2 (on GitHub), R 4.6.1
* Microsoft Windows Server 2022 10.0.26100 (on GitHub), R 4.6.1
* Ubuntu 24.04.4 (on GitHub), R 4.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes

- 1.2.3 showed an intermittent test ERROR on r-release-windows-x86_64 only (an R session crash, exit code -1073741819), which did not appear on r-devel-windows, r-oldrel-windows, or any other flavour, and which we could not reproduce. The reported file skips all of its tests on CRAN, so the crash came from a parallel test worker rather than from that file. The tests therefore now run serially on CRAN, which removes the worker and, should the crash recur, reports the test that causes it