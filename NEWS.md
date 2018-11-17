# felp 0.1.1

- Added S3 version of `?`
    - A method `?.function` provides `print.function()` and `help()` simultaneously.
    - A default method `?.default` is equivalent to utils::`?`
- Unexported `print.function` as it may conflict with autocompletions of RStudio.

# felp 0.1.0

A first version

- `felp()` and `print.function()` shows help and source of a function simultaneously.
