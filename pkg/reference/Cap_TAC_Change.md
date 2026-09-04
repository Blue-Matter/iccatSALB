# Cap the year-on-year change in a management procedure's TAC recommendation

Bounds \`NewTAC\` to within \`-MaxChange\[1\]\`/\`+MaxChange\[2\]\`
proportion of \`LastTAC\`. If \`NewTAC\` is not finite, \`LastTAC\` is
returned instead, and a warning is recorded in \`Log\`.

## Usage

``` r
Cap_TAC_Change(NewTAC, LastTAC, MaxChange)
```

## Arguments

- NewTAC:

  Numeric. Proposed TAC.

- LastTAC:

  Numeric. Previous TAC.

- MaxChange:

  Numeric, length 1 or 2. Maximum proportional decrease/ increase
  permitted relative to \`LastTAC\`. A single value (e.g. \`0.4\`)
  applies symmetrically (-40%/+40%); \`c(MaxDown, MaxUp)\` (e.g.
  \`c(0.4, 0.2)\`) applies asymmetrically.

## Value

A \`list\` with elements \`TAC\` (the capped TAC) and \`Log\` (\`NULL\`,
or a warning message if \`NewTAC\` was non-finite).
