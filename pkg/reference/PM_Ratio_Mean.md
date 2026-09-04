# Mean SB/SBMSY or F/FMSY ratio over a performance-metric window

For each simulation, averages the annual \`SB/SBMSY\` (or \`F/FMSY\`)
ratio over \`Years\`.

## Usage

``` r
PM_Ratio_Mean(
  object,
  Ratio = c("SB_SBMSY", "F_FMSY"),
  Years = NULL,
  silent = TRUE
)
```

## Arguments

- object:

  An \`mse\` object (or list of \`mse\` objects, combined via
  \[MSEtool::CombineMSE()\]).

- Ratio:

  Character. \`"SB_SBMSY"\` or \`"F_FMSY"\`.

- Years:

  Numeric vector of years to evaluate over. Default \`NULL\` uses
  \[PM_Years()\].

- silent:

  Logical. Suppress the \[MSEtool::CombineMSE()\] summary message when
  \`object\` is a \`list\`. Default \`TRUE\`.

## Value

An \[MSEtool::pm-class\] object.
