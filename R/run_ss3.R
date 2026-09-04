#' Run SS3 across a set of model directories in parallel
#'
#' Runs [r4ss::run()] across multiple SS3 model directories in parallel,
#' using [MSEtool::SetupParallel()]
#'
#' @param run_dirs Character vector of SS3 model directories to run.
#' @param exe Character. Name or path of the SS3 executable, passed to
#'   [r4ss::run()].
#' @param extras Character. Extra command-line arguments passed to
#'   [r4ss::run()] (e.g. `"-nohess"`).
#' @param skipfinished Logical, passed to [r4ss::run()].
#' @param workers Integer. Number of parallel workers, passed to
#'   [MSEtool::SetupParallel()]. Defaults to `length(run_dirs)`, capped at
#'   [future::availableCores()].
#' @param parallel Logical. If `TRUE`, calls [MSEtool::SetupParallel()].
#'  Default `TRUE`.
#'
#' @return Invisibly, `run_dirs`.
#' @export
Run_SS3_Grid <- function(run_dirs, exe = "ss3", extras = "",
                         skipfinished = FALSE, workers = NULL,
                         parallel = TRUE) {

  if (length(run_dirs) == 0) return(invisible(run_dirs))

  if (parallel) {
    if (is.null(workers)) {
      workers <- min(length(run_dirs), future::availableCores())
    }
    MSEtool::SetupParallel(workers = workers)
  }

  furrr::future_walk(run_dirs, function(d) {
    r4ss::run(dir = d, exe = exe, skipfinished = skipfinished, extras = extras)
  })

  if (parallel)
    MSEtool::DisableParallel()

  invisible(run_dirs)
}

