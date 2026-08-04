#' Simulate historical dynamics for a directory of operating models
#'
#' Recursively finds all `.om` files under `om_dir` (as saved by
#' [MSEtool::Save()], e.g. via [Import_OM_Grid()]), runs [MSEtool::Simulate()]
#' on each, and saves the resulting `Hist` object under `hist_dir`.
#'
#' The directory structure of `om_dir` is mirrored under `hist_dir`, so e.g.
#' `om_dir/Reference/G_25-M_25.om` is saved to
#' `hist_dir/Reference/G_25-M_25.hist`.
#'
#' @param om_dir Character. Directory to search for `.om` files.
#' @param hist_dir Character. Directory to save `.hist` files to; created
#'   (including subdirectories) if necessary.
#' @param recursive Logical. If `TRUE` (default), searches subdirectories of
#'   `om_dir` as well.
#'
#' @return Invisibly, a character vector of the saved `.hist` file paths.
#' @export
Simulate_OM_Dir <- function(om_dir, hist_dir, recursive = TRUE) {

  if (!dir.exists(om_dir)) return(invisible(character(0)))

  om_files <- list.files(om_dir, pattern = "\\.om$", recursive = recursive)
  if (!length(om_files)) return(invisible(character(0)))

  hist_files <- sub("\\.om$", ".hist", om_files)

  out_paths <- vapply(seq_along(om_files), function(i) {
    OM   <- readRDS(file.path(om_dir, om_files[i]))
    Hist <- MSEtool::Simulate(OM, silent = TRUE)

    out_path <- file.path(hist_dir, hist_files[i])
    out_dir  <- dirname(out_path)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    MSEtool::Save(Hist, out_path, overwrite = TRUE)
    out_path
  }, character(1))

  invisible(out_paths)
}
