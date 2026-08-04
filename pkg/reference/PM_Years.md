# Restrict performance-metric years to those on/after an OM's MPStartYear

Performance metrics should exclude projection years before an operating
model's \`MPStartYear\` (interim-advice years, before any CMP has
actually set a TAC). As of the current \[MSEtool::PM\] functions, this
is applied automatically (via an internal \`.FilterMPActiveYears()\`)
when their \`Years\` argument is left \`NULL\` - so calling e.g.
\[MSEtool::PM_Status()\] or \[MSEtool::PM_Yield()\] directly, with
\`Years = NULL\`, already excludes interim years, and passing this
function's output to them is redundant (though harmless - the two floors
are idempotent).

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
functions such as \[PM_Depletion_Min()\], which do not get the automatic
floor; and as the basis for \[PM_Window_Years()\]'s
\`"last10"\`/\`"terminal"\` windows, which have no \`MSEtool::PM\_\*\`
equivalent.
