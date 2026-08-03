# Build a robustness operating model grid by downweighting one survey index

Creates a robustness counterpart of an existing OM grid (e.g. as
produced by \[Prepare_OM_Grid()\]) by copying each cell's run directory
and setting the lambda for \`index_name\` to \`value\` via
\[Downweight_Index()\]. The growth/M grid structure is otherwise
preserved, so R1/R2-style robustness sets remain comparable
cell-for-cell against the reference grid.

## Usage

``` r
Downweight_Index_Grid(grid, index_name, out_base_dir, value = 0)
```

## Arguments

- grid:

  A \`data.frame\` as produced by \[Build_OM_Grid()\] (typically after
  \[prepare_om_grid()\] and \[run_ss3_grid()\] have been run on it, so
  \`run_dir\` points at completed SS3 runs).

- index_name:

  Character. Fleet name of the index to downweight, passed to
  \[Downweight_Index()\].

- out_base_dir:

  Character. Base directory under which robustness run directories are
  created; each cell's directory name is taken from
  \`basename(grid\$run_dir)\`.

- value:

  Numeric. Lambda value to assign to \`index_name\` (default \`0\`).

## Value

A \`data.frame\` like \`grid\`, with \`run_dir\` updated to the new
robustness run directories.
