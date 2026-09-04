# Write named MP-variant definitions into a CMP-definition script

Write named MP-variant definitions into a CMP-definition script

## Usage

``` r
Write_MP_Variants(generator_name, variants, file, section = generator_name)
```

## Arguments

- generator_name:

  Character. Name of the generator function already defined in \`file\`
  (or sourced before it), e.g. \`'MCC_HCR'\`. \`file\` must already
  contain a \`class(\<generator_name\>) \<- 'mp'\` line.

- variants:

  A named \`list\` of named \`list\`s – outer names become the new
  variant object names, inner lists are \`formals()\` overrides, e.g.
  \`list(MCC_90 = list(tunepar = 0.90), MCC_95 = list(tunepar =
  0.95))\`. All outer names must share a common stem (see Description).

- file:

  Character. Path to the target R script (e.g.
  \`'analysis/04-Define-CMPs.R'\`).

- section:

  Unused; retained for backward compatibility with existing calls.
  Family identity is now derived from \`variants\`' own names.

## Value

\`file\`, invisibly.
