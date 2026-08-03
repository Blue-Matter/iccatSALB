# Run SS3 across a set of model directories in parallel

Runs \[r4ss::run()\] across multiple SS3 model directories in parallel,
using \[MSEtool::SetupParallel()\] to configure the future backend.
Parallel workers are left running on exit (consistent with
\[MSEtool::SetupParallel()\]'s design) so that subsequent parallel steps
in the same session (e.g. further \`run_ss3_grid()\` calls, or MSE
simulation) can reuse them; call \[MSEtool::DisableParallel()\]
explicitly once all parallel work is finished.

## Usage

``` r
Run_SS3_Grid(
  run_dirs,
  exe = "ss3",
  extras = "",
  skipfinished = FALSE,
  workers = NULL,
  parallel = TRUE
)
```

## Arguments

- run_dirs:

  Character vector of SS3 model directories to run.

- exe:

  Character. Name or path of the SS3 executable, passed to
  \[r4ss::run()\].

- extras:

  Character. Extra command-line arguments passed to \[r4ss::run()\]
  (e.g. \`"-nohess"\`).

- skipfinished:

  Logical, passed to \[r4ss::run()\].

- workers:

  Integer. Number of parallel workers, passed to
  \[MSEtool::SetupParallel()\]. Defaults to \`length(run_dirs)\`, capped
  at \[future::availableCores()\].

- parallel:

  Logical. If \`TRUE\`, calls \[MSEtool::SetupParallel()\]. Default
  \`TRUE\`.

## Value

Invisibly, \`run_dirs\`.
