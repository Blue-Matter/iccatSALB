# List MSE files and build an OM index

Scans \`dir\` for \`.mse\` files (recursively) and parses each path into
\`OM_Type\` (\`"Reference"\`, or the sub-directory name under
\`Robustness/\`), \`Family\` (the MP family/group the file was run and
saved under – see \`MP_Groups\` in \`analysis/04-Define-CMPs.R\`), and
\`OM\` (the Growth x M grid cell, e.g. \`"G_25-M_25"\`).

## Usage

``` r
GetMSEFiles(dir = "objects/MSE")
```

## Arguments

- dir:

  Directory to search for \`.mse\` files.

## Value

A \`data.frame\` with columns \`Family\`, \`OM_Type\`, \`OM\`, \`File\`,
\`Growth\`, \`M\`, \`OM_Name\`.
