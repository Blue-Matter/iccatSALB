# Modify growth and natural mortality parameters in an SS3 control file

Updates the initial values for the von Bertalanffy growth parameters
(\`L_at_Amin\`, \`L_at_Amax\`, \`VonBert_K\`) and the Lorenzen-scaled
natural mortality parameter (\`NatM_p_1\`) in an SS3 control list, as
returned by \[r4ss::SS_read()\].

## Usage

``` r
Modify_Growth_M(inputs, Linf, K, t0, M)
```

## Arguments

- inputs:

  A list of SS3 inputs as returned by \[r4ss::SS_read()\], containing
  \`ctl\$MG_parms\`.

- Linf, K, t0:

  Numeric. Von Bertalanffy asymptotic length, growth coefficient, and
  theoretical age at zero length

- M:

  Numeric. Lorenzen-reference natural mortality scalar assigned to
  \`NatM_p_1_Fem_GP_1\`.

## Value

The modified \`inputs\` list.
