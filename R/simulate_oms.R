#' Simulate historical dynamics for a directory of operating models
#'
#' Recursively finds all `.om` files under `om_dir` (as saved by
#' [MSEtool::Save()], runs [MSEtool::Simulate()]
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

#' List saved Hist files for a projection run
#'
#' Recursively finds all `.hist` files under `path` (as saved by
#' [Simulate_OM_Dir()]) and classifies each as belonging to the `Reference`
#' or `Robustness` operating model set based on its location.
#'
#' @param path Character. Directory to search for `.hist` files.
#'
#' @return A `data.frame` with columns:
#'   \describe{
#'     \item{OM_Type}{`"Reference"` or `"Robustness"`.}
#'     \item{File}{Path to the `.hist` file, relative to `path`.}
#'     \item{Name}{Base file name, including the `.hist` extension.}
#'     \item{OM_Name}{`"Base Case"` for `Reference` files; the Robustness
#'       subdirectory name (e.g. `"R1"`) for `Robustness` files.}
#'     \item{Run}{Logical, `TRUE` by default. Set to `FALSE` to skip
#'       specific rows in a projection loop.}
#'   }
#' @export
GetHistFiles <- function(path = 'objects/Hist') {
  files <- list.files(path, pattern = '\\.hist$',
                      recursive = TRUE,
                      full.names = TRUE)

  rel_dir <- dirname(sub(paste0('^', path, '/'), '', files))
  is_rob  <- startsWith(rel_dir, 'Robustness')

  OM_Type <- ifelse(is_rob, 'Robustness', 'Reference')
  OM_Name <- ifelse(is_rob, basename(rel_dir), 'Base Case')

  data.frame(OM_Type = OM_Type,
             File = files,
             Name = basename(files),
             OM_Name = OM_Name,
             Run = TRUE,
             stringsAsFactors = FALSE) |>
    dplyr::filter(Name != 'Ref.hist')
}
