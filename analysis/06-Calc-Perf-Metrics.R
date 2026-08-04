library(iccatSALB)
library(MSEtool)

source('analysis/00-Specifications.R')


# ---- Load MSE objects ----
# Mirrors objects/Hist: objects/MSE/Ref.mse (base case),
# objects/MSE/Reference/*.mse (grid), objects/MSE/Robustness/R1|R2/*.mse
read_mse_dir <- function(dir) {
  MSE_files <- list.files(dir, pattern = '\\.mse$', full.names = TRUE)
  purrr::map(MSE_files, readRDS) |>
    purrr::set_names(tools::file_path_sans_ext(basename(MSE_files)))
}

MSESets <- list(
  Reference = read_mse_dir('objects/MSE/Reference'),
  R1        = read_mse_dir('objects/MSE/Robustness/R1'),
  R2        = read_mse_dir('objects/MSE/Robustness/R2')
)

# ---- Performance metric calculation ----

# Extracts a 'pm' object's Stock x MP summary (Mean, or Stat averaged over
# Sim) as a named-by-MP vector. Assumes a single (spawning) stock, as in
# this project's OMs.
mp_summary <- function(pm_obj, use = c('Mean', 'Stat')) {
  use <- match.arg(use)
  arr <- if (use == 'Mean') pm_obj@Mean else apply(pm_obj@Stat, c(2, 3), mean, na.rm = TRUE)
  stopifnot(nrow(arr) == 1)
  stats::setNames(as.numeric(arr[1, ]), dimnames(arr)[[2]])
}

# Calculates all PMs for a single 'mse' object (or a list of 'mse' objects,
# pooled via CombineMSE()), one row per MP. All PMs are restricted to years
# on/after MPStartYear (see PM_Years()/PM_Window_Years()).
Calc_PMs <- function(MSE) {

  YearsAll  <- PM_Window_Years(MSE, 'all')
  YearsL10  <- PM_Window_Years(MSE, 'last10')
  YearsTerm <- PM_Window_Years(MSE, 'terminal')

  Status_All  <- PM_Status(MSE, Years = YearsAll)
  Status_L10  <- PM_Status(MSE, Years = YearsL10)
  Status_Term <- PM_Status(MSE, Years = YearsTerm)

  # Blim fixed at 40% of the (sim-median) SBMSY, per Resolution 24-09
  Blim   <- BlimFrac * stats::median(SBMSY(MSE))
  Safety <- PM_Safety(MSE, Blim = Blim, Years = YearsAll)

  Yield         <- PM_Yield(MSE, Years = YearsAll)
  Stability     <- PM_Stability(MSE, Threshold = StabilityThreshold, Years = YearsAll)
  DepletionMin  <- PM_Depletion_Min(MSE, Years = YearsAll)

  MPnames <- Status_All@MPs

  dplyr::tibble(
    MP              = MPnames,
    Status_All      = mp_summary(Status_All)[MPnames],
    Status_Last10   = mp_summary(Status_L10)[MPnames],
    Status_Terminal = mp_summary(Status_Term)[MPnames],
    Safety          = mp_summary(Safety)[MPnames],
    Yield           = mp_summary(Yield, 'Stat')[MPnames],
    Stability       = mp_summary(Stability)[MPnames],
    Depletion_Min   = mp_summary(DepletionMin, 'Stat')[MPnames]
  )
}

# ---- Calculate PMs: pooled by OM set, and per individual OM ----

PooledPMs <- purrr::imap_dfr(MSESets, function(MSEList, set_name) {
  Calc_PMs(MSEList) |> dplyr::mutate(OMSet = set_name, OM = 'Pooled', .before = 1)
})

IndividualPMs <- purrr::imap_dfr(MSESets, function(MSEList, set_name) {
  purrr::imap_dfr(MSEList, function(MSE, om_name) {
    Calc_PMs(MSE) |> dplyr::mutate(OMSet = set_name, OM = om_name, .before = 1)
  })
})

PerformanceMetrics <- dplyr::bind_rows(PooledPMs, IndividualPMs)

if (!dir.exists('objects')) dir.create('objects')
saveRDS(PerformanceMetrics, 'objects/PerformanceMetrics.rds')
