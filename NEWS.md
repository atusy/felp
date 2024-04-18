# felp 0.5.0

- On `fuzzyhelp()`, help/vignette contents gain syntax highlights and links. It formerly used `Rd2HTML()` to generate HTML contents. Now the contents inherit from a help server with `startDynamicHelp()`, which means they are exactly same as the contents of `help()` or `vignette()`.
- On `fuzzyhelp()`, demo contents can be previewed.
- On `fuzzyhelp(background = FALSE)`, "Done" and "Cancel" buttons are removed because the feature of the "Done" requires the Shiny app run on the main thread.

# felp 0.4.0

- Support `fuzzyhelp` to run Shiny App in background without blocking user terminal.
  The new behavior is enabled by default and can be disabled by passing `FALSE` to the `background` argument or to the `fuzzyhelp.background` option (#18, #20).
- Fixed wrong behaviors of anchors in the HTML help in the UI of the `fuzzyhelp` function.
  A click on a anchor should not cause nesting of the UI when href of the anchor is an ID.
  Instead, the click should scroll the window to show the element with the corresponding ID (#17).

# felp 0.3.0

- Added `fuzzyhelp` function which launches Shiny Gadget to search help topics fuzzily, and preview the result. Done button will also launch `help` function. 
  There is also RStudio Addin named "Fuzzy Search on R Help", which launches the `fuzzyhelp` function (#12, #13, #14, #15).
- Fix notes from CRAN checks (fcad5274ee79db4b243810930eb7be95981cbe7f)


# felp 0.2.3

- Corrected problems noticed by CRAN
    - avoid package 'MASS' in examples because it is not a build-in package for some platforms
    - internally import `prettycode::default_style` without using it to suppress a NOTE, "All declared Imports should be used."

# felp 0.2.2

- Update tests so to be independent from pandoc, and improve code coverage.
- Add logo and CRAN badge, download badge, and favicon

# felp 0.2.1

Just internal changes and tests to be ready on CRAN.

- Refactored to suppress notes
- Add tests
- Use Travis CI, AppVeyor, Codecov for test with CI and visualize code coverage.

# felp 0.2.0

`felp` is now a short of "**f**unctional h**elp**" to help things in addition to functions. 
It was formerly "**f**unction h**elp**"

## Major changes

- `felp()` and `?` returns structure of a value specified to the first argument
  if possible. If function is specified, the source of function is returned 
  instead of the structure.
- `felp()`
    - improves consistency with `utils::help` in terms of arguments
    - supports to display package documentation just like `help(package = )`
- Pseudo-postfix operators
    - `?.` supports arguments other than functions.
    - `?p` is added to display document of a package.
- Updates on documents with `pkgdown` site

# felp 0.1.3.9000

Tagged, but not released.

## Major changes

- felp() has more consistent arguments to utils::help by changing a name of first argument to "topic" from "x".
- Pseudo-postfix operator ?. supports arguments other than functions.

## Minor changes

- felp() is simplified.


# felp 0.1.3

- Officially support `function?.` form to print function and its help simultaneously.
    - This feature was already present in felp 0.1.1 without knowing it.
    - Before 0.1.3, `.` of `function?.` can be any like `function?hoge`. 
      This behavior is changed because of conflicts with `type?topic` form of ``utils::`?` ``.

# felp 0.1.2

- Use `prettycode:::print.function()`

# felp 0.1.1

- Added S3 version of `?`
    - A method `?.function` provides `print.function()` and `help()` simultaneously.
    - A default method `?.default` is equivalent to utils::`?`
- Unexported `print.function` as it may conflict with auto-completions of RStudio.

## Help wanted

R CMD check results gives a warning, but I have no idea to solve it.

```
❯ checking Rd \usage sections ... WARNING
  Undocumented arguments in documentation object '?'
    ‘e1’ ‘e2’
  
  Bad \usage lines found in documentation object '?':
    <unescaped bksl>method{?}{function}(e1, e2)
    <unescaped bksl>method{?}{default}(e1, e2)
  
  Functions with \usage entries need to have the appropriate \alias
  entries, and all their arguments documented.
  The \usage entries must correspond to syntactically valid R code.
  See chapter ‘Writing R documentation files’ in the ‘Writing R
  Extensions’ manual.
```

# felp 0.1.0

A first version

- `felp()` and `print.function()` shows help and source of a function simultaneously.
