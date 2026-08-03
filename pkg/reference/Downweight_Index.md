# Set the likelihood weight (lambda) for a survey index in an SS3 control file

Sets the lambda (objective-function multiplier) applied to a named
survey/CPUE index's likelihood contribution. If an explicit lambda row
for that fleet/likelihood-component combination already exists in
\`ctl\$lambdas\`, its \`value\` is overwritten; otherwise a new row is
appended. Used to build robustness operating models in which one index
is treated as uninformative (\`value = 0\`) or partially downweighted
during OM conditioning, while other data components are left untouched.

## Usage

``` r
Downweight_Index(inputs, index_name, value = 0, like_comp = 1, phase = 1)
```

## Arguments

- inputs:

  A list of SS3 inputs as returned by \[r4ss::SS_read()\], containing
  \`ctl\` and \`dat\` elements.

- index_name:

  Character. Fleet name of the survey index to downweight.

- value:

  Numeric. Lambda value to assign (default \`0\`).

- like_comp:

  Integer. SS3 likelihood-component code for survey/index data (default
  \`1\`).

- phase:

  Integer. Phase from which the lambda applies, used only when adding a
  new row (default \`1\`).

## Value

The modified \`inputs\` list, with an updated \`ctl\$lambdas\` table.

## Details

The index name is matched against \`dat\$fleetinfo\$fleetname\` (or
\`dat\$fleetnames\`, depending on \`r4ss\` version) to resolve the
numeric fleet code used in the lambda table.
