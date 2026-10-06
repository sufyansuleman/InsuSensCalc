## Submission summary

This is an update of InsuSensCalc, already on CRAN (0.1.0 -> 0.2.0).

It corrects two insulin sensitivity index formulas after verifying each against
its original publication:

* 'Quicki' now uses log base 10 as published (Katz 2000), rather than the
  natural logarithm.
* 'Firi' is now inverted to represent insulin sensitivity, consistent with the
  rest of the package and with what its own documentation stated, and takes
  glucose in mmol/L as in the original definition (Duncan 1995).

Both corrections change the numeric output of the affected column relative to
0.1.0. Each is exactly recoverable by a documented conversion, and NEWS.md gives
the conversion formula for each so that existing results can be converted
without recomputation.

The update also adds documentation recording, for every index, the publication
that defined it and the population that publication studied, and a section
listing where this implementation departs from the published definition.

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
