#' Build a set of named MP variants from a generator and a parameter grid
#'
#' Generates `class = 'mp'` management-procedure functions from a single
#' `generator` (a CMP generator such as [define_cmps]'s `IndexRate_MP`,
#' `MCC_HCR`, etc.), one variant per row of `grid`.
#'
#' @param generator A `class = 'mp'` CMP generator function, as defined in
#'   `analysis/04-Define-CMPs.R`.
#' @param grid A `data.frame`, one row per variant. Column names must match
#'   `generator`'s own argument names (e.g. `tunepar`, `BBmsy_scalar`). An
#'   optional `Name` column sets each variant's name directly. Otherwise
#'   names are generated as `<prefix><row number>`.
#' @param prefix Character. Prefix used for auto-generated names when `grid`
#'   has no `Name` column. Default `'MP'`.
#'
#' @return A named `list` of `class = 'mp'` functions, one per row of
#'   `grid`. Pass directly to [Tune_Grid()], or `list2env()` into an
#'   environment before calling [MSEtool::Project()] with the resulting
#'   names.
#'
#' @export
Build_MP_Variants <- function(generator, grid, prefix = 'MP') {

  if (is.null(grid$Name)) grid$Name <- paste0(prefix, seq_len(nrow(grid)))
  params <- grid[, setdiff(names(grid), 'Name'), drop = FALSE]

  MPs <- stats::setNames(vector('list', nrow(grid)), grid$Name)
  for (i in seq_len(nrow(grid))) {
    MP <- generator
    for (nm in names(params)) formals(MP)[[nm]] <- params[[i, nm]]
    class(MP) <- 'mp'
    MPs[[i]] <- MP
  }
  MPs
}

#' Project a set of MP variants and tabulate performance metrics in one call
#'
#' Projects `MPs` over `Hist` (a single `hist` object, or a `list` of them,
#' combined via [MSEtool::CombineMSE()]), runs [Calc_PMs()], and
#' reshapes the result into one row per MP with `Pass_Safety`/`Pass_Status`/
#' `Pass` flags computed against `SafetyMin`/`StatusMin`.
#'
#'
#' @param MPs Character vector of MP names, or a named list of `class='mp'`
#'   functions.
#' @param Hist A `hist` object, or a `list` of them (pooled across via
#'   [MSEtool::CombineMSE()]).
#' @param SafetyMin,StatusMin Numeric. Compliance thresholds for
#'   `Pass_Safety` (`Safety_All >= SafetyMin`) and `Pass_Status`
#'   (`Status_All >= StatusMin`).
#' @param BlimFrac,StabilityThreshold Passed to [Calc_PMs()].
#' @param HistYieldRef Numeric or `NULL`. If supplied, adds a `Yield_pct`
#'   column (`100 * Yield_All / HistYieldRef`) to the result.
#' @param parallel Logical, passed to [MSEtool::Project()]. Default `FALSE`.
#' Corrupts tuning process. Don't use.
#' @param nSim Integer or `NULL`. Passed to [MSEtool::Project()]'s `nSim` argument
#' @param silent Logical. Suppress `Project()`/`Calc_PMs()` progress output.
#'
#' @return A `list` with `MSE` (the projected `mse` object, pooled if `Hist`
#'   was a list) and `PM` (a wide `data.frame`, one row per MP, columns
#'   `<PM>_<Window>` for every metric in [Calc_PMs()] plus `Yield_pct`
#'   (if `HistYieldRef` supplied), `Pass_Safety`, `Pass_Status`, `Pass`).
#'
#' @export
Tune_Grid <- function(MPs, Hist, SafetyMin = 0.85, StatusMin = 0.60,
                       BlimFrac = 0.4, StabilityThreshold = 0.2,
                       HistYieldRef = NULL, parallel = FALSE, nSim = NULL,
                       silent = TRUE) {

  if (is.list(MPs) && !is.character(MPs)) {
    list2env(MPs, envir = globalenv())
    MPs <- names(MPs)
  }

  if (is.list(Hist) && !isS4(Hist)) {
    MSE_list <- lapply(Hist, function(h)
      MSEtool::Project(h, MPs = MPs, parallel = parallel, nSim = nSim, silent = silent))
    MSE <- MSEtool::CombineMSE(MSE_list, silent = silent)
  } else {
    MSE <- MSEtool::Project(Hist, MPs = MPs, parallel = parallel, nSim = nSim, silent = silent)
  }

  PM <- Calc_PMs(MSE, BlimFrac = BlimFrac, StabilityThreshold = StabilityThreshold, silent = silent)

  Wide <- PM |>
    dplyr::mutate(PMWin = paste0(.data$PM, '_', .data$Window)) |>
    dplyr::select('MP', 'PMWin', 'Mean') |>
    tidyr::pivot_wider(names_from = 'PMWin', values_from = 'Mean') |>
    dplyr::mutate(
      Pass_Safety = .data$Safety_All >= SafetyMin,
      Pass_Status = .data$Status_All >= StatusMin,
      Pass        = .data$Pass_Safety & .data$Pass_Status
    )

  if (!is.null(HistYieldRef)) {
    Wide <- Wide |> dplyr::mutate(Yield_pct = 100 * .data$Yield_All / HistYieldRef, .after = 'Yield_All')
  }

  list(MSE = MSE, PM = Wide)
}

#' Pick the highest-yield compliant row from a [Tune_Grid()] result
#'
#' @param PM A wide performance-metric `data.frame`, as returned in the `PM`
#'   element of [Tune_Grid()]'s result (must have logical `Pass` and numeric
#'   `Yield_All` columns).
#' @param sort_by Character. Column to maximise among compliant (`Pass ==
#'   TRUE`) rows. Default `'Yield_All'`.
#'
#' @return A one-row `data.frame` (the best-compliant row), or `NULL` with a
#'   message if no row in `PM` is compliant.
#' @export
Best_Compliant <- function(PM, sort_by = 'Yield_All') {
  passing <- PM[PM$Pass, ]
  if (nrow(passing) == 0) {
    message('No compliant row found.')
    return(NULL)
  }
  passing[which.max(passing[[sort_by]]), ]
}

#' Write named MP-variant definitions into a CMP-definition script
#'
#' @param generator_name Character. Name of the generator function already
#'   defined in `file` (or sourced before it), e.g. `'MCC_HCR'`. `file` must
#'   already contain a `class(<generator_name>) <- 'mp'` line.
#' @param variants A named `list` of named `list`s -- outer names become the
#'   new variant object names, inner lists are `formals()` overrides, e.g.
#'   `list(MCC_90 = list(tunepar = 0.90), MCC_95 = list(tunepar = 0.95))`.
#'   All outer names must share a common stem (see Description).
#' @param file Character. Path to the target R script (e.g.
#'   `'analysis/04-Define-CMPs.R'`).
#' @param section Unused; retained for backward compatibility with existing
#'   calls. Family identity is now derived from `variants`' own names.
#'
#' @return `file`, invisibly.
#' @export
Write_MP_Variants <- function(generator_name, variants, file, section = generator_name) {

  heading <- '## ---- Tuned variants ----'

  code <- character(0)
  for (nm in names(variants)) {
    params <- variants[[nm]]
    code <- c(code, sprintf('%s <- %s', nm, generator_name))
    for (p in names(params)) {
      code <- c(code, sprintf('formals(%s)$%s <- %s', nm, p, deparse(params[[p]])))
    }
    code <- c(code, sprintf("class(%s) <- 'mp'", nm), '')
  }

  stems <- unique(sub('[0-9]+$', '', names(variants)))
  if (length(stems) != 1) {
    stop('Write_MP_Variants: all names in `variants` must share a common stem ',
         '(the name with its trailing digits stripped).')
  }
  stem <- stems

  txt <- if (file.exists(file)) readLines(file) else character(0)

  class_line <- sprintf("class(%s) <- 'mp'", generator_name)
  anchor_idx <- which(trimws(txt) == class_line)
  if (length(anchor_idx) != 1) {
    stop(sprintf("Write_MP_Variants: expected exactly one '%s' line in %s, found %d.",
                 class_line, file, length(anchor_idx)))
  }

  i <- anchor_idx + 1
  while (i <= length(txt) && trimws(txt[i]) == '') i <- i + 1

  fresh <- !(i <= length(txt) && trimws(txt[i]) == heading)
  if (fresh) {
    txt <- append(txt, c('', heading), after = anchor_idx)
    heading_idx <- anchor_idx + 2
  } else {
    heading_idx <- i
  }

  j <- heading_idx + 1
  while (j <= length(txt) && !grepl('^# ----', txt[j])) j <- j + 1
  block_end <- j - 1

  block <- if (block_end >= heading_idx + 1) txt[(heading_idx + 1):block_end] else character(0)

  if (fresh) {
    trailer  <- if (any(trimws(block) != '')) block else character(0)
    new_block <- c('', code, trailer)
  } else {
    is_assign_line <- function(line) {
      parts <- strsplit(line, ' <- ', fixed = TRUE)[[1]]
      length(parts) == 2 && parts[2] == generator_name && grepl('^[[:alnum:]_.]+$', parts[1])
    }
    assign_pos <- which(vapply(block, is_assign_line, logical(1)))

    preamble <- if (length(assign_pos) && assign_pos[1] > 1) {
      block[seq_len(assign_pos[1] - 1)]
    } else if (!length(assign_pos)) block else character(0)

    new_block <- preamble
    inserted <- FALSE
    if (length(assign_pos)) {
      seg_start <- assign_pos
      seg_end   <- c(assign_pos[-1] - 1, length(block))
      for (k in seq_along(seg_start)) {
        seg <- block[seg_start[k]:seg_end[k]]
        seg_name <- sub(' <- .*$', '', seg[1])
        seg_stem <- sub('[0-9]+$', '', seg_name)
        if (identical(seg_stem, stem)) {
          if (!inserted) { new_block <- c(new_block, code); inserted <- TRUE }
        } else {
          new_block <- c(new_block, seg)
        }
      }
    }
    if (!inserted) new_block <- c(new_block, code)
  }

  txt <- c(txt[seq_len(heading_idx)], new_block,
           if (block_end < length(txt)) txt[(block_end + 1):length(txt)] else character(0))

  writeLines(txt, file)
  invisible(file)
}
