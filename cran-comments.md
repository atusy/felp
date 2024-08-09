## Test environments

- local: x86_64-pc-linux-gnu (R-4.3)
- devtools
    - `check_rhub()`
    - `check_win_devel()`
    - `check_win_release()`
- GitHub Actions
    - {os: macOS-latest,   r: 'release'}
    - {os: windows-latest, r: 'release'}
    - {os: ubuntu-latest,   r: 'devel', http-user-agent: 'release'}
    - {os: ubuntu-latest,   r: 'release'}
    - {os: ubuntu-latest,   r: 'oldrel-1'}

## R CMD check results

0 errors | 0 warnings | 1 notes

1 note says that some of the URIs are possibly invalid, but they are valid.

```
* checking CRAN incoming feasibility ... [10s] NOTE
Maintainer: 'Atsushi Yasumoto <atusy.rpkg@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://atusy.shinyapps.io/fuzzyhelp/
    From: README.md
    Status: 202
    Message: Accepted
  URL: https://felp.atusy.net/
    From: DESCRIPTION
    Status: Error
    Message: Recv failure: Connection was reset
  URL: https://helpr.atusy.net/
    From: README.md
    Status: Error
    Message: Recv failure: Connection was reset
```

## revdepcheck results

There are currently no downstream dependencies for this package.
