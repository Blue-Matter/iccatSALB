#' List MSE files and build an OM index
#'
#' Scans `dir` for `.mse` files (recursively) and parses each path into
#' `OM_Type` (`"Reference"`, or the sub-directory name under `Robustness/`),
#' `Family` (the MP family/group the file was run and saved under -- see
#' `MP_Groups` in `analysis/04-Define-CMPs.R`), and `OM` (the Growth x M
#' grid cell, e.g. `"G_25-M_25"`).
#'
#'
#' @param dir Directory to search for `.mse` files.
#' @return A `data.frame` with columns `Family`, `OM_Type`, `OM`, `File`,
#'   `Growth`, `M`, `OM_Name`.
#' @export
GetMSEFiles <- function(dir = 'objects/MSE') {

  MSE_files <- list.files(dir, pattern = '\\.mse$', recursive = TRUE, full.names = TRUE)
  rel_path  <- sub(paste0('^', dir, '/'), '', MSE_files)
  parts     <- strsplit(rel_path, '/')

  FileIndex <- do.call(rbind, lapply(seq_along(parts), function(i) {
    p <- parts[[i]]
    n <- length(p)
    if (n < 3) stop('Unexpected path depth: ', paste(p, collapse = '/'))

    OM      <- p[n]
    Family  <- p[n - 1]
    OM_Type <- if (n == 3) p[1] else p[2]  # 'Reference', or 'R1'/'R2'

    data.frame(Family = Family, OM_Type = OM_Type, OM = sub('\\.mse$', '', OM),
               File = MSE_files[i], stringsAsFactors = FALSE)
  }))

  om_split <- regmatches(FileIndex$OM, regexec('^G_(\\d+)-M_(\\d+)$', FileIndex$OM))
  FileIndex$Growth  <- as.integer(sapply(om_split, `[`, 2))
  FileIndex$M       <- as.integer(sapply(om_split, `[`, 3))
  FileIndex$OM_Name <- paste(FileIndex$OM_Type, FileIndex$OM, sep = '_')

  FileIndex
}


