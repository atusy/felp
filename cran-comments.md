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

0 errors | 0 warnings | 0 notes

## revdepcheck results

There are currently no downstream dependencies for this package.
