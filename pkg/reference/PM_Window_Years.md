# Resolve a named performance-metric year window

Resolves \`window\` to a vector of projection years to pass as the
\`Years\` argument of an \[MSEtool::PM\] function, always first
restricted to years on/after \`MPStartYear\` via \[PM_Years()\].
\[MSEtool::PM\] functions apply that floor themselves when \`Years =
NULL\` (see \[PM_Years()\]), but have no equivalent for the
\`"last10"\`/\`"terminal"\` windows below, so this function remains the
way to get those.

## Usage

``` r
PM_Window_Years(object, window = c("all", "last10", "terminal"))
```

## Arguments

- object:

  An \`mse\`/\`hist\`/\`om\` object.

- window:

  Character. One of \`"all"\` (every year from \`MPStartYear\` onwards),
  \`"last10"\` (the final 10 of those years), or \`"terminal"\` (the
  final year only).

## Value

Numeric vector of years.
