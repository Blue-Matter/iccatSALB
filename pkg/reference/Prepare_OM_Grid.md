# Prepare SS3 run directories for an operating model grid

For each non-reference row of \`grid\`, copies the reference SS3 model
files into \`run_dir\` via \[r4ss::copy_SS_inputs()\] and overwrites
growth and natural mortality parameters using \[Modify_Growth_M()\]. The
reference cell (\`is_ref == TRUE\`) is left untouched, since it already
exists at \`ref_dir\`.

## Usage

``` r
Prepare_OM_Grid(grid, ref_dir, growth_scenarios, M_scenarios)
```

## Arguments

- grid:

  A \`data.frame\` as produced by \[Build_OM_Grid()\].

- ref_dir:

  Character. Reference SS3 model directory to copy from.

- growth_scenarios, M_scenarios:

  Named lists keyed by \`grid\$g_scen\` / \`grid\$m_scen\` labels (e.g.
  \`list(G_25 = list(Linf = ..., K = ..., t0 = ...), ...)\` and
  \`list(M_25 = 0.x, ...)\`).

## Value

Invisibly, \`grid\` (files are written as a side effect).
