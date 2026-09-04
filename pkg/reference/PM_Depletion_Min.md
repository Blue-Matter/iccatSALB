# Lowest depletion (SB/SB0) reached during a performance-metric window

For each simulation, finds the lowest annual spawning depletion (SB/SB0)
reached during \`Years\`.

## Usage

``` r
PM_Depletion_Min(object, Years = NULL, silent = TRUE)
```

## Arguments

- object:

  An \`mse\` object (or list of \`mse\` objects, combined via
  \[MSEtool::CombineMSE()\]).

- Years:

  Numeric vector of years to evaluate over. Default \`NULL\` uses
  \[PM_Years()\].

- silent:

  Logical. Suppress the \[MSEtool::CombineMSE()\] summary message when
  \`object\` is a \`list\`. Default \`TRUE\`.

## Value

An \[MSEtool::pm-class\] object.
