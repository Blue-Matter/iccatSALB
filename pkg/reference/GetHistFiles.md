# List saved Hist files for a projection run

Recursively finds all \`.hist\` files under \`path\` (as saved by
\[Simulate_OM_Dir()\]) and classifies each as belonging to the
\`Reference\` or \`Robustness\` operating model set based on its
location.

## Usage

``` r
GetHistFiles(path = "objects/Hist")
```

## Arguments

- path:

  Character. Directory to search for \`.hist\` files.

## Value

A \`data.frame\` with columns:

- OM_Type:

  \`"Reference"\` or \`"Robustness"\`.

- File:

  Path to the \`.hist\` file, relative to \`path\`.

- Name:

  Base file name, including the \`.hist\` extension.

- OM_Name:

  \`"Base Case"\` for \`Reference\` files; the Robustness subdirectory
  name (e.g. \`"R1"\`) for \`Robustness\` files.

- Run:

  Logical, \`TRUE\` by default. Set to \`FALSE\` to skip specific rows
  in a projection loop.
