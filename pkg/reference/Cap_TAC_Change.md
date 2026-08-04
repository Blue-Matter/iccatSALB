# Cap the year-on-year change in a management procedure's TAC recommendation

Bounds \`NewTAC\` to within \`+/- MaxChange\` proportion of \`LastTAC\`.
If \`NewTAC\` is not finite, \`LastTAC\` is returned instead, and a
warning is recorded in \`Log\`.

## Usage

``` r
Cap_TAC_Change(NewTAC, LastTAC, MaxChange)
```

## Arguments

- NewTAC:

  Numeric. Proposed TAC.

- LastTAC:

  Numeric. Previous TAC; used both as the fallback value and as the base
  of the \`MaxChange\` cap.

- MaxChange:

  Numeric. Maximum proportional change (up or down) permitted relative
  to \`LastTAC\` (e.g. \`0.4\` allows -40%/+40%).

## Value

A \`list\` with elements \`TAC\` (the capped TAC) and \`Log\` (\`NULL\`,
or a warning message if \`NewTAC\` was non-finite).
