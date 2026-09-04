# Build a reference operating model grid specification

Creates a \`data.frame\` with a grid of growth (\`g_levels\`) x natural
mortality (\`m_levels\`) scenarios for SS3-based operating model
conditioning.

## Usage

``` r
Build_OM_Grid(
  g_levels = c(25, 50, 75),
  m_levels = c(25, 50, 75),
  ref_dir,
  base_dir = "data-raw/assessment"
)
```

## Arguments

- g_levels, m_levels:

  Numeric vectors of growth/natural-mortality scenario labels (e.g.
  \`c(25, 50, 75)\`), combined via \[expand.grid()\].

- ref_dir:

  Character. Path to the reference SS3 model directory, assigned to the
  reference grid cell's \`run_dir\`.

- base_dir:

  Character. Base directory under which non-reference scenario run
  directories are created.

## Value

A \`data.frame\` with columns \`g_scen\`, \`m_scen\`, \`om_name\`,
\`is_ref\`, and \`run_dir\`.
