# Tidy per-simulation, per-year SB/SBMSY, F/FMSY, and Yield time series

Pulls the three time series needed for \`Slick::Timeseries()\`/
\`Slick::Kobe()\` \`Value\` arrays, and total \`Yield\` into one long
\`data.frame\`, covering both the \`Historical\` and \`Projection\`
periods.

## Usage

``` r
Extract_MSE_Timeseries(MSE, silent = TRUE)
```

## Arguments

- MSE:

  An \`mse\` object (or list of \`mse\` objects, combined via
  \[MSEtool::CombineMSE()\]).

- silent:

  Logical. Suppress the \[MSEtool::CombineMSE()\] summary message when
  \`MSE\` is a \`list\`. Default \`TRUE\`.

## Value

A tidy \`data.frame\` with columns \`Sim\`, \`Stock\`, \`Year\`,
\`Period\`, \`MP\`, \`Variable\` (\`'SB_SBMSY'\`, \`'F_FMSY'\`, or
\`'Yield'\`), \`Value\`.
