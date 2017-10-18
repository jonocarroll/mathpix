## Test environments
* local ubuntu 16.04.2, R 3.4.1
* rhub::check_on_windows() x86_64-w64-mingw32 (64-bit), 3.4.2
* rhub::check_on_linux() x86_64-pc-linux-gnu (64-bit), R 3.4.2
* rhub::check_on_macos() x86_64-apple-darwin15.6.0 (64-bit), R 3.4.1
* rhub::check_for_cran() "windows-x86_64-devel" "ubuntu-gcc-release" "fedora-clang-devel" 
* travis-ci Ubuntu 14.04.5 LTS, R 3.4.2
* appveyor i386-w64-mingw32/i386 (32-bit), r 3.4.2
* win-builder (R-devel, R-release) via devtools::build_win()

## R CMD check results

0 errors | 0 warnings | 0 notes

## win-builder results

R-devel: OK
R-release: 1 NOTE on Maintainer

## r-hub results

1 ERROR due to incorrect testthat configuration on Windows Server
1 NOTE due to ORCID on fedora-clang-devel

## Reverse dependencies

None.
