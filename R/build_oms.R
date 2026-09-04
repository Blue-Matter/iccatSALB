#' Build a reference operating model grid specification
#'
#' Creates a `data.frame` with a grid of growth (`g_levels`) x natural
#' mortality (`m_levels`) scenarios for SS3-based operating model
#' conditioning.
#'
#' @param g_levels,m_levels Numeric vectors of growth/natural-mortality
#'   scenario labels (e.g. `c(25, 50, 75)`), combined via [expand.grid()].
#' @param ref_dir Character. Path to the reference SS3 model directory,
#'   assigned to the reference grid cell's `run_dir`.
#' @param base_dir Character. Base directory under which non-reference
#'   scenario run directories are created.
#'
#' @return A `data.frame` with columns `g_scen`, `m_scen`, `om_name`,
#'   `is_ref`, and `run_dir`.
#' @export
Build_OM_Grid <- function(g_levels = c(25, 50, 75),
                          m_levels = c(25, 50, 75),
                          ref_dir,
                          base_dir = "data-raw/assessment") {

  grid <- expand.grid(g_scen = paste0("G_", g_levels),
                      m_scen = paste0("M_", m_levels),
                      stringsAsFactors = FALSE)

  grid$om_name <- paste(grid$g_scen, grid$m_scen, sep = "-")
  grid$is_ref  <- grid$g_scen == "G_50" & grid$m_scen == "M_50"
  grid$run_dir <- ifelse(grid$is_ref, ref_dir, file.path(base_dir, grid$om_name))

  grid
}


#' Modify growth and natural mortality parameters in an SS3 control file
#'
#' Updates the initial values for the von Bertalanffy growth parameters
#' (`L_at_Amin`, `L_at_Amax`, `VonBert_K`) and the Lorenzen-scaled natural
#' mortality parameter (`NatM_p_1`) in an SS3 control list, as returned by
#' [r4ss::SS_read()].
#'
#' @param inputs A list of SS3 inputs as returned by [r4ss::SS_read()],
#'   containing `ctl$MG_parms`.
#' @param Linf,K,t0 Numeric. Von Bertalanffy asymptotic length, growth
#'   coefficient, and theoretical age at zero length
#' @param M Numeric. Lorenzen-reference natural mortality scalar assigned to
#'   `NatM_p_1_Fem_GP_1`.
#'
#' @return The modified `inputs` list.
#' @export
Modify_Growth_M <- function(inputs, Linf, K, t0, M) {

  inputs$ctl$MG_parms["L_at_Amin_Fem_GP_1", "INIT"] <- vonbert_L_at_Amin(Linf, K, t0)
  inputs$ctl$MG_parms["L_at_Amax_Fem_GP_1", "INIT"] <- Linf
  inputs$ctl$MG_parms["VonBert_K_Fem_GP_1", "INIT"] <- K
  inputs$ctl$MG_parms["NatM_p_1_Fem_GP_1", "INIT"]  <- M

  inputs
}

vonbert_L_at_Amin <- function(Linf, K, t0, Amin = 0) {
  Linf * (1 - exp(-K * (Amin - t0)))
}

#' Prepare SS3 run directories for an operating model grid
#'
#' For each non-reference row of `grid`, copies the reference SS3 model
#' files into `run_dir` via [r4ss::copy_SS_inputs()] and overwrites growth
#' and natural mortality parameters using [Modify_Growth_M()]. The reference
#' cell (`is_ref == TRUE`) is left untouched, since it already exists at
#' `ref_dir`.
#'
#' @param grid A `data.frame` as produced by [Build_OM_Grid()].
#' @param ref_dir Character. Reference SS3 model directory to copy from.
#' @param growth_scenarios,M_scenarios Named lists keyed by `grid$g_scen` /
#'   `grid$m_scen` labels (e.g. `list(G_25 = list(Linf = ..., K = ..., t0 =
#'   ...), ...)` and `list(M_25 = 0.x, ...)`).
#'
#' @return Invisibly, `grid` (files are written as a side effect).
#' @export
Prepare_OM_Grid <- function(grid, ref_dir, growth_scenarios, M_scenarios) {

  for (i in seq_len(nrow(grid))) {

    if (isTRUE(grid$is_ref[i])) next

    run_dir <- grid$run_dir[i]
    growth  <- growth_scenarios[[grid$g_scen[i]]]
    m       <- M_scenarios[[grid$m_scen[i]]]

    if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE)

    r4ss::copy_SS_inputs(dir.old   = ref_dir,
                         dir.new  = run_dir,
                         copy_exe = FALSE,
                         copy_par = TRUE,
                         overwrite = TRUE,
                         verbose   = FALSE)

    inputs <- r4ss::SS_read(dir = run_dir)
    inputs <- Modify_Growth_M(inputs, Linf = growth$Linf, K = growth$K,
                              t0 = growth$t0, M = m)
    r4ss::SS_write(inputs, dir = run_dir, overwrite = TRUE)
  }

  invisible(grid)
}

#' Set the likelihood weight (lambda) for a survey index in an SS3 control file
#'
#' Sets the lambda applied to a named survey/CPUE index's likelihood contribution.
#'
#'
#' @param inputs A list of SS3 inputs as returned by [r4ss::SS_read()],
#'   containing `ctl` and `dat` elements.
#' @param index_name Character. Fleet name of the survey index to  downweight.
#' @param value Numeric. Lambda value to assign (default `0`).
#' @param like_comp Integer. SS3 likelihood-component code for survey/index
#'   data (default `1`).
#' @param phase Integer. Phase from which the lambda applies, used only when
#'   adding a new row (default `1`).
#'
#' @return The modified `inputs` list, with an updated `ctl$lambdas` table.
#' @export
Downweight_Index <- function(inputs, index_name, value = 0, like_comp = 1, phase = 1) {

  fleetnames <- inputs$dat$fleetinfo$fleetname
  if (is.null(fleetnames)) fleetnames <- inputs$dat$fleetnames

  fleet_num <- which(fleetnames == index_name)

  if (length(fleet_num) != 1) {
    stop("Could not uniquely match index_name '", index_name,
         "' to a fleet in inputs$dat (", length(fleet_num), " match(es) found).",
         call. = FALSE)
  }

  lambdas <- inputs$ctl$lambdas
  match_row <- which(lambdas$like_comp == like_comp & lambdas$fleet == fleet_num)

  if (length(match_row) == 1) {
    lambdas$value[match_row] <- value
  } else {
    new_row <- data.frame(like_comp = like_comp, fleet = fleet_num,
                          phase = phase, value = value, sizefreq_method = 1)
    new_row <- new_row[intersect(names(new_row), names(lambdas))]
    lambdas <- rbind(lambdas, new_row)
  }

  inputs$ctl$lambdas  <- lambdas
  inputs$ctl$N_lambdas <- nrow(lambdas)

  inputs
}

#' Build a robustness operating model grid by downweighting one survey index
#'
#' Creates a robustness counterpart of an existing OM grid (e.g. as produced
#' by [Prepare_OM_Grid()]) by copying each cell's run directory and setting
#' the lambda for `index_name` to `value` via [Downweight_Index()].
#'
#' @param grid A `data.frame` as produced by [Build_OM_Grid()]
#' @param index_name Character. Fleet name of the index to downweight,
#'   passed to [Downweight_Index()].
#' @param out_base_dir Character. Base directory under which robustness run
#'   directories are created; each cell's directory name is taken from
#'   `basename(grid$run_dir)`.
#' @param value Numeric. Lambda value to assign to `index_name` (default
#'   `0`).
#'
#' @return A `data.frame` like `grid`, with `run_dir` updated to the new
#'   robustness run directories.
#' @export
Downweight_Index_Grid <- function(grid, index_name, out_base_dir, value = 0) {

  new_grid <- grid
  new_grid$run_dir <- file.path(out_base_dir, basename(grid$run_dir))

  for (i in seq_len(nrow(grid))) {

    src_dir <- grid$run_dir[i]
    dst_dir <- new_grid$run_dir[i]

    if (!dir.exists(dst_dir)) dir.create(dst_dir, recursive = TRUE)

    src_files <- list.files(src_dir)
    file.copy(file.path(src_dir, src_files), file.path(dst_dir, src_files),
              overwrite = TRUE)

    inputs <- r4ss::SS_read(dir = dst_dir)
    inputs <- Downweight_Index(inputs, index_name = index_name, value = value)
    r4ss::SS_write(inputs, dir = dst_dir, overwrite = TRUE)
  }

  new_grid
}


#' Import a grid of SS3 models into MSEtool operating model objects
#'
#' Loops over `grid`, importing each SS3 run directory via
#' [MSEtool::ImportSS()] and saving the resulting `OM` object to
#' `out_dir/<om_name>.om` via [MSEtool::Save()].
#'
#' @param grid A `data.frame` with columns `run_dir` and `om_name`.
#' @param out_dir Character. Directory to save `.om` files to.
#' @param om_specs A named list of arguments passed through to
#'   [MSEtool::ImportSS()] (e.g. `Name`, `nSim`, `pYear`, `Agency`,
#'   `StockName`, `CommonName`, `Interval`, `DataLag`).
#'
#' @return Invisibly, a character vector of the saved `.om` file paths.
#' @export
Import_OM_Grid <- function(grid, out_dir, om_specs) {

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  out_paths <- vapply(seq_len(nrow(grid)), function(i) {
    OM <- do.call(MSEtool::ImportSS, c(list(SSDir = grid$run_dir[i]), om_specs))
    out_path <- file.path(out_dir, paste0(grid$om_name[i], ".om"))
    MSEtool::Save(OM, out_path, overwrite = TRUE)
    out_path
  }, character(1))

  invisible(out_paths)
}

