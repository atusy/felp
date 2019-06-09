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
