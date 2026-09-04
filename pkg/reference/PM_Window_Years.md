# Resolve a named performance-metric year window

Resolves \`window\` to a vector of projection years to pass as the
\`Years\` argument of an \[MSEtool::PM\] function, always first
restricted to years on/after \`MPStartYear\` via \[PM_Years()\].
\[MSEtool::PM\] functions apply that floor themselves when \`Years =
NULL\` (see \[PM_Years()\]), but have no equivalent for the named
windows below, so this function remains the way to get those.

## Usage

``` r
PM_Window_Years(
  object,
  window = c("all", "last10", "short", "medium", "long", "terminal")
)
```

## Arguments

- object:

  An \`mse\`/\`hist\`/\`om\` object.

- window:

  Character. One of \`"all"\` (every year from \`MPStartYear\` onwards),
  \`"last10"\` (the final 10 of those years), \`"short"\` (the first
  10), \`"medium"\` (everything between \`"short"\` and \`"long"\`),
  \`"long"\` (the final 10), or \`"terminal"\` (the final year only).

## Value

Numeric vector of years.

## Details

\`"short"\`/\`"medium"\`/\`"long"\` split the full active-year range
into three consecutive, non-overlapping chunks – the first 10 years, the
last 10 years, and the middle 10 when there are 30 active years.
