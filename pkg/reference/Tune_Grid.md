# Project a set of MP variants and tabulate performance metrics in one call

Projects \`MPs\` over \`Hist\` (a single \`hist\` object, or a \`list\`
of them, combined via \[MSEtool::CombineMSE()\]), runs \[Calc_PMs()\],
and reshapes the result into one row per MP with
\`Pass_Safety\`/\`Pass_Status\`/ \`Pass\` flags computed against
\`SafetyMin\`/\`StatusMin\`.

## Usage

``` r
Tune_Grid(
  MPs,
  Hist,
  SafetyMin = 0.85,
  StatusMin = 0.6,
  BlimFrac = 0.4,
  StabilityThreshold = 0.2,
  HistYieldRef = NULL,
  parallel = FALSE,
  nSim = NULL,
  silent = TRUE
)
```

## Arguments

- MPs:

  Character vector of MP names, or a named list of \`class='mp'\`
  functions.

- Hist:

  A \`hist\` object, or a \`list\` of them (pooled across via
  \[MSEtool::CombineMSE()\]).

- SafetyMin, StatusMin:

  Numeric. Compliance thresholds for \`Pass_Safety\` (\`Safety_All \>=
  SafetyMin\`) and \`Pass_Status\` (\`Status_All \>= StatusMin\`).

- BlimFrac, StabilityThreshold:

  Passed to \[Calc_PMs()\].

- HistYieldRef:

  Numeric or \`NULL\`. If supplied, adds a \`Yield_pct\` column (\`100
  \* Yield_All / HistYieldRef\`) to the result.

- parallel:

  Logical, passed to \[MSEtool::Project()\]. Default \`FALSE\`. Corrupts
  tuning process. Don't use.

- nSim:

  Integer or \`NULL\`. Passed to \[MSEtool::Project()\]'s \`nSim\`
  argument

- silent:

  Logical. Suppress \`Project()\`/\`Calc_PMs()\` progress output.

## Value

A \`list\` with \`MSE\` (the projected \`mse\` object, pooled if
\`Hist\` was a list) and \`PM\` (a wide \`data.frame\`, one row per MP,
columns \`\<PM\>\_\<Window\>\` for every metric in \[Calc_PMs()\] plus
\`Yield_pct\` (if \`HistYieldRef\` supplied), \`Pass_Safety\`,
\`Pass_Status\`, \`Pass\`).
