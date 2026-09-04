# Set the likelihood weight (lambda) for a survey index in an SS3 control file

Sets the lambda applied to a named survey/CPUE index's likelihood
contribution.

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
