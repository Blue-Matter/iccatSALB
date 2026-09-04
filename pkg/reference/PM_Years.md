# Restrict performance-metric years to those on/after an OM's MPStartYear

Performance metrics should exclude projection years before an operating
model's \`MPStartYear\` (interim-advice years, before any CMP has
actually set a TAC).

## Usage

``` r
PM_Years(object, Years = NULL)
```

## Arguments

- object:

  An \`mse\`/\`hist\`/\`om\` object (or similar), passed to
  \[MSEtool::MPStartYear()\] and \[MSEtool::Years()\].

- Years:

  Numeric vector of candidate years. Default \`NULL\` uses all
  projection years of \`object\`.

## Value

Numeric vector of years, restricted to \`\>= MPStartYear(object)\`
(unchanged if \`MPStartYear(object)\` is \`NULL\`).

## Details

This function remains necessary for: custom, non-\`MSEtool::PM\_\*\`
functions such as \[PM_Depletion_Min()\]
