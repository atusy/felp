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
- Unexported `print.function` as it may conflict with autocompletions of RStudio.

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
