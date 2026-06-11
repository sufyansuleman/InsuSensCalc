## Submission summary

This is an update of InsuSensCalc, already on CRAN (0.0.1 -> 0.1.0).

It corrects several insulin sensitivity index formulas after verifying each
against its original publication and Table 2 of the package's source paper
(Suleman 2024). As a result, some index values change relative to 0.0.1; these
changes are documented in NEWS.md. The update also adds regression tests and
removes the unused 'tidyr' dependency.

## R CMD check results

0 errors | 0 warnings | 0 notes

Locally a single NOTE appears ("checking for future file timestamps ... unable
to verify current time"), which is a local clock-access artifact and is not
expected on CRAN's check systems.

## Test environments

* Local: Windows 11, R 4.4.0
* win-builder (R-devel and R-release): pending

## Reverse dependencies

There are no reverse dependencies on CRAN.

## Notes

* Examples run on the bundled example_data and complete in well under 5 seconds.
* DOIs in the Description (<doi:10.1002/oby.23503>, <doi:10.1210/clinem/dgae275>,
  <doi:10.1210/jc.2010-1144>) resolve to the cited publications.
* The package writes no files and makes no changes to the user's environment.
