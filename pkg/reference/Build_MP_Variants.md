# Build a set of named MP variants from a generator and a parameter grid

Generates \`class = 'mp'\` management-procedure functions from a single
\`generator\` (a CMP generator such as \[define_cmps\]'s
\`IndexRate_MP\`, \`MCC_HCR\`, etc.), one variant per row of \`grid\`.

## Usage

``` r
Build_MP_Variants(generator, grid, prefix = "MP")
```

## Arguments

- generator:

  A \`class = 'mp'\` CMP generator function, as defined in
  \`analysis/04-Define-CMPs.R\`.

- grid:

  A \`data.frame\`, one row per variant. Column names must match
  \`generator\`'s own argument names (e.g. \`tunepar\`,
  \`BBmsy_scalar\`). An optional \`Name\` column sets each variant's
  name directly. Otherwise names are generated as \`\<prefix\>\<row
  number\>\`.

- prefix:

  Character. Prefix used for auto-generated names when \`grid\` has no
  \`Name\` column. Default \`'MP'\`.

## Value

A named \`list\` of \`class = 'mp'\` functions, one per row of \`grid\`.
Pass directly to \[Tune_Grid()\], or \`list2env()\` into an environment
before calling \[MSEtool::Project()\] with the resulting names.
