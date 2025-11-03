## Resubmission

This is a resubmission. In this version I have:

* Removed the explicit C++11 specification from the Makevars and the Makevars.win file as requested, to ensure forward compatibility with RcppArmadillo and to use R's modern default C++ standard.


## Test environments

* Mac OS X 15.7 (local machine):
  - release R 4.5.1

* Rhub
  - linux R-* (any version) ubuntu-latest on GitHub
  - m1-san R-* (any version) macos-15 on GitHub, ASAN + UBSAN on macOS
  - macos R-* (any version) macos-13 on GitHub
  - macos-arm64 R-* (any version) macos-latest on GitHub
  - windows R-* (any version) windows-latest on GitHub

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs


