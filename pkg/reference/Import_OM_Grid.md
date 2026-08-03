# Import a grid of SS3 models into MSEtool operating model objects

Loops over \`grid\`, importing each SS3 run directory via
\[MSEtool::ImportSS()\] and saving the resulting \`OM\` object to
\`out_dir/\<om_name\>.om\` via \[MSEtool::Save()\].

## Usage

``` r
Import_OM_Grid(grid, out_dir, om_specs)
```

## Arguments

- grid:

  A \`data.frame\` with columns \`run_dir\` and \`om_name\`.

- out_dir:

  Character. Directory to save \`.om\` files to; created if necessary.

- om_specs:

  A named list of arguments passed through to \[MSEtool::ImportSS()\]
  (e.g. \`Name\`, \`nSim\`, \`pYear\`, \`Agency\`, \`StockName\`,
  \`CommonName\`, \`Interval\`, \`DataLag\`).

## Value

Invisibly, a character vector of the saved \`.om\` file paths.
