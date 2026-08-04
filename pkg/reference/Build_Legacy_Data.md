# Build a legacy Data object with a combined abundance index

Converts a \`data\` object (as passed to a management procedure) to the
legacy single-simulation \`Data\` class expected by
\`DLMtool\`/\`SAMtool\` management procedures, via
\[MSEtool::ConvertData()\]. The \`Ind\` slot (and \`CV_Ind\`, if
\`CV_Ind\` is supplied) is then overwritten using \`index_fun\`, so that
multiple survey/CPUE series can be folded into a single combined index
rather than relying on \[MSEtool::ConvertData()\]'s default of selecting
a single series.

## Usage

``` r
Build_Legacy_Data(Data, index_fun, CV_Ind = NA)
```

## Arguments

- Data:

  A \`data\` object, as passed to a management procedure.

- index_fun:

  Function taking \`Data\` and returning a named numeric vector of
  combined index values (one per year), used to overwrite the converted
  object's \`Ind\` slot.

- CV_Ind:

  Numeric. If supplied, used as a constant CV for the combined index
  (assigned to \`Data@CV_Ind\`); if \`NA\` (default), \`CV_Ind\` is left
  as set by \[MSEtool::ConvertData()\].

## Value

A legacy \`Data\` object (\`nsim = 1\`).
