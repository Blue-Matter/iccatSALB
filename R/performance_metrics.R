#' Restrict performance-metric years to those on/after an OM's MPStartYear
#'
#' Performance metrics should exclude projection years before an operating
#' model's `MPStartYear` (interim-advice years, before any CMP has actually
#' set a TAC).
#'
#' This function remains necessary for: custom, non-`MSEtool::PM_*`
#' functions such as [PM_Depletion_Min()]
#'
#' @param object An `mse`/`hist`/`om` object (or similar), passed to
#'   [MSEtool::MPStartYear()] and [MSEtool::Years()].
#' @param Years Numeric vector of candidate years. Default `NULL` uses all
#'   projection years of `object`.
#'
#' @return Numeric vector of years, restricted to `>= MPStartYear(object)`
#'   (unchanged if `MPStartYear(object)` is `NULL`).
#' @export
PM_Years <- function(object, Years = NULL) {

  if (is.null(Years)) Years <- MSEtool::Years(object, 'Projection')

  MPStartYear <- MSEtool::MPStartYear(object)
  if (!is.null(MPStartYear)) Years <- Years[Years >= MPStartYear]

  Years
}

#' Resolve a named performance-metric year window
#'
#' Resolves `window` to a vector of projection years to pass as the `Years`
#' argument of an [MSEtool::PM] function, always first restricted to years
#' on/after `MPStartYear` via [PM_Years()]. [MSEtool::PM] functions apply
#' that floor themselves when `Years = NULL` (see [PM_Years()]), but have no
#' equivalent for the named windows below, so this function remains the way
#' to get those.
#'
#' `"short"`/`"medium"`/`"long"` split the full active-year range into three
#' consecutive, non-overlapping chunks -- the first 10 years, the last 10
#' years, and the middle 10 when there are 30 active years.
#'
#' @param object An `mse`/`hist`/`om` object.
#' @param window Character. One of `"all"` (every year from `MPStartYear`
#'   onwards), `"last10"` (the final 10 of those years), `"short"` (the
#'   first 10), `"medium"` (everything between `"short"` and `"long"`),
#'   `"long"` (the final 10), or `"terminal"` (the final year only).
#'
#' @return Numeric vector of years.
#' @export
PM_Window_Years <- function(object, window = c('all', 'last10', 'short', 'medium', 'long', 'terminal')) {
  window <- match.arg(window)

  Years <- PM_Years(object)

  switch(window,
    all      = Years,
    last10   = utils::tail(Years, 10),
    short    = utils::head(Years, 10),
    long     = utils::tail(Years, 10),
    medium   = setdiff(Years, union(utils::head(Years, 10), utils::tail(Years, 10))),
    terminal = utils::tail(Years, 1)
  )
}


.PM_Array <- function(df, valcol) {
  simN   <- sort(unique(df$Sim))
  stockN <- unique(df$Stock)
  mpN    <- unique(df$MP)

  arr <- array(NA_real_, dim = c(length(simN), length(stockN), length(mpN)),
               dimnames = list(Sim = simN, Stock = stockN, MP = mpN))

  idx <- cbind(match(df$Sim, simN), match(df$Stock, stockN), match(df$MP, mpN))
  arr[idx] <- df[[valcol]]
  arr
}

#' Lowest depletion (SB/SB0) reached during a performance-metric window
#'
#' For each simulation, finds the lowest annual spawning depletion (SB/SB0)
#' reached during `Years`.
#'
#' @param object An `mse` object (or list of `mse` objects, combined via
#'   [MSEtool::CombineMSE()]).
#' @param Years Numeric vector of years to evaluate over. Default `NULL`
#'   uses [PM_Years()].
#' @param silent Logical. Suppress the [MSEtool::CombineMSE()] summary
#'   message when `object` is a `list`. Default `TRUE`.
#'
#' @return An [MSEtool::pm-class] object.
#' @export
PM_Depletion_Min <- function(object, Years = NULL, silent = TRUE) {

  if (is.list(object) && !isS4(object))
    object <- MSEtool::CombineMSE(object, silent = silent)

  Years <- PM_Years(object, Years)

  df <- MSEtool::SB_SB0(object, df = TRUE, Reduce = FALSE)
  df <- df[df$Period == 'Projection' & df$Year %in% Years, ]

  min_df <- df |>
    dplyr::group_by(.data$Sim, .data$Stock, .data$MP) |>
    dplyr::summarise(Value = min(.data$Value, na.rm = TRUE), .groups = 'drop')

  StatArr <- .PM_Array(min_df, 'Value')
  ProbArr <- StatArr
  ProbArr[] <- NA_real_
  MeanArr <- apply(ProbArr, c(2, 3), mean, na.rm = TRUE)
  dim(MeanArr) <- dim(StatArr)[c(2, 3)]
  dimnames(MeanArr) <- dimnames(StatArr)[c(2, 3)]

  methods::new('pm',
               Name    = 'Depletion_Min',
               Caption = 'Lowest annual SB/SB0 (depletion) reached during the period',
               Stat    = StatArr,
               Ref     = NA_real_,
               Prob    = ProbArr,
               Mean    = MeanArr,
               MPs     = sort(unique(min_df$MP)),
               Years   = sort(unique(df$Year)))
}

#' Mean SB/SBMSY or F/FMSY ratio over a performance-metric window
#'
#' For each simulation, averages the annual `SB/SBMSY` (or `F/FMSY`) ratio
#' over `Years`.
#'
#' @param object An `mse` object (or list of `mse` objects, combined via
#'   [MSEtool::CombineMSE()]).
#' @param Ratio Character. `"SB_SBMSY"` or `"F_FMSY"`.
#' @param Years Numeric vector of years to evaluate over. Default `NULL`
#'   uses [PM_Years()].
#' @param silent Logical. Suppress the [MSEtool::CombineMSE()] summary
#'   message when `object` is a `list`. Default `TRUE`.
#'
#' @return An [MSEtool::pm-class] object.
#' @export
PM_Ratio_Mean <- function(object, Ratio = c('SB_SBMSY', 'F_FMSY'), Years = NULL, silent = TRUE) {
  Ratio <- match.arg(Ratio)

  if (is.list(object) && !isS4(object))
    object <- MSEtool::CombineMSE(object, silent = silent)

  Years <- PM_Years(object, Years)

  df <- switch(Ratio,
    SB_SBMSY = MSEtool::SB_SBMSY(object, df = TRUE),
    F_FMSY   = MSEtool::F_FMSY(object, df = TRUE)
  )
  df <- df[df$Period == 'Projection' & df$Year %in% Years, ]

  mean_df <- df |>
    dplyr::group_by(.data$Sim, .data$Stock, .data$MP) |>
    dplyr::summarise(Value = mean(.data$Value, na.rm = TRUE), .groups = 'drop')

  StatArr <- .PM_Array(mean_df, 'Value')
  ProbArr <- StatArr
  ProbArr[] <- NA_real_
  MeanArr <- apply(ProbArr, c(2, 3), mean, na.rm = TRUE)
  dim(MeanArr) <- dim(StatArr)[c(2, 3)]
  dimnames(MeanArr) <- dimnames(StatArr)[c(2, 3)]

  methods::new('pm',
               Name    = Ratio,
               Caption = paste0('Mean annual ', gsub('_', '/', Ratio), ' ratio over the period'),
               Stat    = StatArr,
               Ref     = NA_real_,
               Prob    = ProbArr,
               Mean    = MeanArr,
               MPs     = sort(unique(mean_df$MP)),
               Years   = sort(unique(df$Year)))
}
class(PM_Ratio_Mean) <- 'pm'


.PM_SingleWindow <- c('Safety', 'Stability', 'Stability30', 'ATV', 'Depletion_Min')


.PM_Code <- function(PM, Window) {
  ifelse(PM %in% .PM_SingleWindow, PM, paste(PM, Window, sep = '_'))
}

.Fix_Stability_NaN <- function(pm) {
  pm@Stat[is.nan(pm@Stat)] <- 0
  pm@Prob[is.nan(pm@Prob)] <- 1
  pm@Mean <- apply(pm@Prob, c(2, 3), mean, na.rm = TRUE)
  pm
}

.Fix_AAVY_NaN <- function(pm) {
  pm@Stat[is.nan(pm@Stat)] <- 0
  pm
}

.PM_Panel <- function(MSE, BlimFrac = 0.4, StabilityThreshold = 0.2, silent = TRUE) {

  if (is.list(MSE) && !isS4(MSE))
    MSE <- MSEtool::CombineMSE(MSE, silent = silent)

  YearsShort  <- PM_Window_Years(MSE, 'short')
  YearsMedium <- PM_Window_Years(MSE, 'medium')
  YearsLong   <- PM_Window_Years(MSE, 'long')

  Lim <- BlimFrac

  list(
    list(PM = 'Yield',
         Window = 'All',
         pm = MSEtool::PM_Yield(MSE, silent = silent)),
    list(PM = 'Yield',
         Window = 'Short',
         pm = MSEtool::PM_Yield(MSE, Years = YearsShort, silent = silent)),
    list(PM = 'Yield',
         Window = 'Medium',
         pm = MSEtool::PM_Yield(MSE, Years = YearsMedium, silent = silent)),
    list(PM = 'Yield',
         Window = 'Long',
         pm = MSEtool::PM_Yield(MSE, Years = YearsLong, silent = silent)),
    list(PM = 'Safety',
         Window = 'All',
         pm = MSEtool::PM_Safety(MSE, Lim = Lim, Definition = 'SBiomass', silent = silent)),
    list(PM = 'Stability',
         Window = 'All',
         pm = .Fix_Stability_NaN(MSEtool::PM_Stability(MSE, Threshold = StabilityThreshold, silent = silent))),
    list(PM = 'ATV',
         Window = 'All',
         pm = .Fix_AAVY_NaN(MSEtool::PM_AAVY(MSE, silent = silent))),
    list(PM = 'Status',
         Window = 'All',
         pm = MSEtool::PM_Status(MSE, Definition = 'SBiomass', silent = silent)),
    list(PM = 'Status',
         Window = 'Short',
         pm = MSEtool::PM_Status(MSE, Definition = 'SBiomass', Years = YearsShort, silent = silent)),
    list(PM = 'Status',
         Window = 'Medium',
         pm = MSEtool::PM_Status(MSE, Definition = 'SBiomass', Years = YearsMedium, silent = silent)),
    list(PM = 'Status',
         Window = 'Long',
         pm = MSEtool::PM_Status(MSE, Definition = 'SBiomass', Years = YearsLong, silent = silent)),
    list(PM = 'Depletion_Min', Window = 'All',
         pm = PM_Depletion_Min(MSE, silent = silent)),
    list(PM = 'SB_SBMSY',
         Window = 'All',
         pm = PM_Ratio_Mean(MSE, Ratio = 'SB_SBMSY', silent = silent)),
    list(PM = 'SB_SBMSY',
         Window = 'Short',
         pm = PM_Ratio_Mean(MSE, Ratio = 'SB_SBMSY', Years = YearsShort, silent = silent)),
    list(PM = 'SB_SBMSY',
         Window = 'Medium',
         pm = PM_Ratio_Mean(MSE, Ratio = 'SB_SBMSY', Years = YearsMedium, silent = silent)),
    list(PM = 'SB_SBMSY',
         Window = 'Long',
         pm = PM_Ratio_Mean(MSE, Ratio = 'SB_SBMSY', Years = YearsLong, silent = silent)),
    list(PM = 'F_FMSY',
         Window = 'All',
         pm = PM_Ratio_Mean(MSE, Ratio = 'F_FMSY', silent = silent)),
    list(PM = 'F_FMSY',
         Window = 'Short',
         pm = PM_Ratio_Mean(MSE, Ratio = 'F_FMSY', Years = YearsShort, silent = silent)),
    list(PM = 'F_FMSY',
         Window = 'Medium',
         pm = PM_Ratio_Mean(MSE, Ratio = 'F_FMSY', Years = YearsMedium, silent = silent)),
    list(PM = 'F_FMSY',
         Window = 'Long',
         pm = PM_Ratio_Mean(MSE, Ratio = 'F_FMSY', Years = YearsLong, silent = silent))
  )
}

#' Tabulate a standard set of performance metrics, for comparing CMPs
#'
#' Runs a fixed panel of performance metrics eg [MSEtool::PM_Yield()],
#' [MSEtool::PM_Safety()], [MSEtool::PM_Stability()], and [MSEtool::PM_Status()],
#'  and stacks their `@Mean` (`Stock x MP`) slots into one tidy table,
#'  for quick side-by-side comparison of candidate CMPs.
#'
#'
#' @param MSE An `mse` object (or list of `mse` objects, combined via
#'   [MSEtool::CombineMSE()]).
#' @param BlimFrac Numeric. Fraction of `SBMSY` used as the safety limit,
#'   passed straight through as `Lim` (with `Definition = 'SBiomass'`).
#' @param StabilityThreshold Numeric. Passed to [MSEtool::PM_Stability()].
#' @param StabilityThreshold30 Numeric. A second, looser [MSEtool::PM_Stability()]
#'   check (reported as `Stability30`) alongside the Resolution-24-09-standard
#'   20\% one -- e.g. useful when a CMP clears a 30\% year-on-year change bound
#'   comfortably but not the stricter 20\% one, distinguishing "somewhat
#'   stable" from "not stable at all". `ATV` ([MSEtool::PM_AAVY()], mean
#'   year-on-year yield variability, unthresholded) is also always included.
#' @param silent Logical. Suppress the [MSEtool::CombineMSE()] summary
#'   message when `MSE` is a `list`. Default `TRUE`.
#'
#' @return A tidy `data.frame` with columns `PM`, `Window`, `PI`, `Stock`,
#'   `MP`, `Mean`. `PI` is the `PM`/`Window` pair collapsed into the single
#'   code expected by `analysis/08-Create-Slick.R`'s `PM_Codes` (see
#'   [.PM_Code()]).
#' @export
Calc_PMs <- function(MSE, BlimFrac = 0.4, StabilityThreshold = 0.2, silent = TRUE) {

  PMs <- .PM_Panel(MSE,
                   BlimFrac = BlimFrac,
                   StabilityThreshold = StabilityThreshold,
                   silent = silent)

  dplyr::bind_rows(lapply(PMs, function(x) {
    MeanArr <- if (x$PM == 'Depletion_Min') {
      apply(x$pm@Stat, c(2, 3), min, na.rm = TRUE)
    } else if (x$PM %in% c('SB_SBMSY', 'F_FMSY', 'ATV')) {
      apply(x$pm@Stat, c(2, 3), mean, na.rm = TRUE)
    } else {
      x$pm@Mean
    }
    df <- as.data.frame.table(MeanArr, responseName = 'Mean')
    names(df)[1:2] <- c('Stock', 'MP')
    df$Stock  <- as.character(df$Stock)
    df$MP     <- as.character(df$MP)
    df$PM     <- x$PM
    df$Window <- x$Window
    df$PI     <- .PM_Code(x$PM, x$Window)
    df[, c('PM', 'Window', 'PI', 'Stock', 'MP', 'Mean')]
  }))
}
class(PM_Depletion_Min) <- 'pm'

#' Per-simulation values for the standard PM panel
#'
#' @description
#' Same fixed panel of performance metrics as [Calc_PMs()] (`PM`/`Window`
#' combinations), but keeps every simulation's own value instead of
#' collapsing to `@Mean`. Intended for populating `Slick::Boxplot()`/
#' `Slick::Quilt()`/`Slick::Spider()` `Value` arrays, which need a `Sim`
#' dimension so that averaging over a subset of selected OMs (in the `Slick`
#' `App()`) is a mean over the raw per-sim values, not a mean of the
#' per-OM means [Calc_PMs()] would give.
#'
#' For probability-scale PMs (`Safety`, `Stability`, `Status`) the per-sim
#' value is read from `@Prob` (already the 0/1 indicator that `@Mean`
#' averages -- confirmed identical to `@Mean` when re-averaged). For
#' natural-scale PMs (`Yield`, `Depletion_Min`, `SB_SBMSY`, `F_FMSY`), which
#' leave `@Prob` all `NA`, the value is read from `@Stat` instead (the same
#' slot [Calc_PMs()] averages for these).
#'
#' `Safety`, `Stability`, `Status`, and `Depletion_Min` are all naturally on
#' a 0-1 scale and safe to use directly in a `Spider()` chart (which
#' requires every PI on a 0-1 or 0-100 scale); `Yield`, `SB_SBMSY`, and
#' `F_FMSY` are not and should be left out of `Spider()`.
#'
#' @inheritParams Calc_PMs
#'
#' @return A tidy `data.frame` with columns `PM`, `Window`, `PI`, `Stock`,
#'   `MP`, `Sim`, `Value`. `PI` is the `PM`/`Window` pair collapsed into the
#'   single code expected by `analysis/08-Create-Slick.R`'s `PM_Codes` (see
#'   [.PM_Code()]).
#' @export
Calc_PM_Stats <- function(MSE,
                          BlimFrac = 0.4,
                          StabilityThreshold = 0.2,
                          silent = TRUE) {

  PMs <- .PM_Panel(MSE,
                   BlimFrac = BlimFrac,
                   StabilityThreshold = StabilityThreshold,
                   silent = silent)

  dplyr::bind_rows(lapply(PMs, function(x) {
    StatArr <- if (all(is.na(x$pm@Prob))) x$pm@Stat else x$pm@Prob
    df <- as.data.frame.table(StatArr, responseName = 'Value')
    names(df)[1:3] <- c('Sim', 'Stock', 'MP')
    df$Sim    <- as.integer(as.character(df$Sim))
    df$Stock  <- as.character(df$Stock)
    df$MP     <- as.character(df$MP)
    df$PM     <- x$PM
    df$Window <- x$Window
    df$PI     <- .PM_Code(x$PM, x$Window)
    df[, c('PM', 'Window', 'PI', 'Stock', 'MP', 'Sim', 'Value')]
  }))
}

#' Tidy per-simulation, per-year SB/SBMSY, F/FMSY, and Yield time series
#'
#' Pulls the three time series needed for `Slick::Timeseries()`/
#' `Slick::Kobe()` `Value` arrays, and total `Yield` into one
#' long `data.frame`, covering both the `Historical` and `Projection` periods.
#'
#'
#' @param MSE An `mse` object (or list of `mse` objects, combined via
#'   [MSEtool::CombineMSE()]).
#' @param silent Logical. Suppress the [MSEtool::CombineMSE()] summary
#'   message when `MSE` is a `list`. Default `TRUE`.
#'
#' @return A tidy `data.frame` with columns `Sim`, `Stock`, `Year`, `Period`,
#'   `MP`, `Variable` (`'SB_SBMSY'`, `'F_FMSY'`, or `'Yield'`), `Value`.
#' @export
Extract_MSE_Timeseries <- function(MSE, silent = TRUE) {

  if (is.list(MSE) && !isS4(MSE))
    MSE <- MSEtool::CombineMSE(MSE, silent = silent)

  keep <- c('Sim', 'Stock', 'Year', 'Period', 'MP', 'Variable', 'Value')

  sb <- MSEtool::SB_SBMSY(MSE, df = TRUE)
  ff <- MSEtool::F_FMSY(MSE, df = TRUE)

  yd <- MSEtool::Removals(MSE, df = TRUE, byFleet = TRUE, Reduce = TRUE) |>
    dplyr::group_by(.data$Sim, .data$Stock, .data$Year, .data$Period, .data$MP) |>
    dplyr::summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = 'drop') |>
    dplyr::mutate(Variable = 'Yield')

  dplyr::bind_rows(sb[, keep], ff[, keep], as.data.frame(yd)[, keep])
}

