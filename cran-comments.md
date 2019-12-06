## Test environments

- local: x86_64-pc-linux-gnu
- travis: release, devel, osx-release
- appveyor: release
- r-hub
    - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    - Ubuntu Linux 16.04 LTS, R-release, GCC
- win-builder
    - x86_64-w64-mingw32 (64-bit), R version 3.6.1 (2019-07-05)
    - x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2019-12-02 r77499)

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

There are currently no downstream dependencies for this package.

## Remarks

CRAN noticed that pandoc is missing from SystemRequirements.
The problem is fixed by removing the dependency on Pandoc.
