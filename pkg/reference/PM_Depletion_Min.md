# Lowest depletion (SB/SB0) reached during a performance-metric window

For each simulation, finds the lowest annual spawning depletion (SB/SB0)
reached during \`Years\`. Unlike \[MSEtool::PM_Safety()\] (a probability
of never falling below a limit), this reports the depletion level
itself, on its natural (0-1) scale, in \`@Stat\` (\`Sim x Stock x MP\`);
average over simulations with e.g. \`apply(result@Stat, c(2, 3),
mean)\`. \`@Prob\`/\`@Mean\` are left \`NA\`, consistent with other
natural-scale (non-probability) \`PM\_\*\` functions such as
\[MSEtool::PM_Yield()\].

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
