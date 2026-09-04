# Pick the highest-yield compliant row from a \[Tune_Grid()\] result

Pick the highest-yield compliant row from a \[Tune_Grid()\] result

## Usage

``` r
Best_Compliant(PM, sort_by = "Yield_All")
```

## Arguments

- PM:

  A wide performance-metric \`data.frame\`, as returned in the \`PM\`
  element of \[Tune_Grid()\]'s result (must have logical \`Pass\` and
  numeric \`Yield_All\` columns).

- sort_by:

  Character. Column to maximise among compliant (\`Pass == TRUE\`) rows.
  Default \`'Yield_All'\`.

## Value

A one-row \`data.frame\` (the best-compliant row), or \`NULL\` with a
message if no row in \`PM\` is compliant.
