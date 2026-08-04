#' Restrict performance-metric years to those on/after an OM's MPStartYear
#'
#' Performance metrics should exclude projection years before an operating
#' model's `MPStartYear` (interim-advice years, before any CMP has actually
#' set a TAC). As of the current [MSEtool::PM] functions, this is applied
#' automatically (via an internal `.FilterMPActiveYears()`) when their
#' `Years` argument is left `NULL` - so calling e.g. [MSEtool::PM_Status()]
#' or [MSEtool::PM_Yield()] directly, with `Years = NULL`, already excludes
#' interim years, and passing this function's output to them is redundant
#' (though harmless - the two floors are idempotent).
#'
#' This function remains necessary for: custom, non-`MSEtool::PM_*`
#' functions such as [PM_Depletion_Min()], which do not get the automatic
#' floor; and as the basis for [PM_Window_Years()]'s `"last10"`/`"terminal"`
#' windows, which have no `MSEtool::PM_*` equivalent.
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
#' equivalent for the `"last10"`/`"terminal"` windows below, so this
#' function remains the way to get those.
#'
#' @param object An `mse`/`hist`/`om` object.
#' @param window Character. One of `"all"` (every year from `MPStartYear`
#'   onwards), `"last10"` (the final 10 of those years), or `"terminal"`
#'   (the final year only).
#'
#' @return Numeric vector of years.
#' @export
PM_Window_Years <- function(object, window = c('all', 'last10', 'terminal')) {
  window <- match.arg(window)

  Years <- PM_Years(object)

  switch(window,
    all      = Years,
    last10   = utils::tail(Years, 10),
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
#' @description
#' For each simulation, finds the lowest annual spawning depletion (SB/SB0)
#' reached during `Years`. Unlike [MSEtool::PM_Safety()] (a probability of
#' never falling below a limit), this reports the depletion level itself, on
#' its natural (0-1) scale, in `@Stat` (`Sim x Stock x MP`); average over
#' simulations with e.g. `apply(result@Stat, c(2, 3), mean)`. `@Prob`/`@Mean`
#' are left `NA`, consistent with other natural-scale (non-probability) `PM_*`
#' functions such as [MSEtool::PM_Yield()].
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
class(PM_Depletion_Min) <- 'pm'
