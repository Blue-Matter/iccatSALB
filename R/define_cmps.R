#' Build a legacy Data object with a combined abundance index
#'
#' @description
#' Converts a `data` object (as passed to a management procedure) to the
#' legacy single-simulation `Data` class expected by `DLMtool`/`SAMtool`
#' management procedures, via [MSEtool::ConvertData()]. The `Ind` slot (and
#' `CV_Ind`, if `CV_Ind` is supplied) is then overwritten using `index_fun`,
#' so that multiple survey/CPUE series can be folded into a single combined
#' index rather than relying on [MSEtool::ConvertData()]'s default of
#' selecting a single series.
#'
#' @param Data A `data` object, as passed to a management procedure.
#' @param index_fun Function taking `Data` and returning a named numeric
#'   vector of combined index values (one per year), used to overwrite the
#'   converted object's `Ind` slot.
#' @param CV_Ind Numeric. If supplied, used as a constant CV for the
#'   combined index (assigned to `Data@CV_Ind`); if `NA` (default), `CV_Ind`
#'   is left as set by [MSEtool::ConvertData()].
#'
#' @return A legacy `Data` object (`nsim = 1`).
#' @export
Build_Legacy_Data <- function(Data, index_fun, CV_Ind = NA) {

  data <- MSEtool::ConvertData(Data, silent = TRUE)

  Index <- index_fun(Data)
  data@Ind <- matrix(Index, 1, length(Index))

  if (!is.na(CV_Ind)) data@CV_Ind <- array(CV_Ind, dim(data@Ind))

  data
}
#' Build a legacy Data object with a multiple indices
#'
#' @seealso [Build_Legacy_Data()]
#' @export
Build_Legacy_Data_MultiIndex <- function(Data, CV_Ind = NA) {
  data <- MSEtool::ConvertData(Data, silent = TRUE)

  Val <- Data@Survey@Value
  CV  <- Data@Survey@CV

  use_cols <- which(!is.na(Val[nrow(Val), ]))
  Val <- Val[, use_cols, drop = FALSE]
  CV  <- CV[,  use_cols, drop = FALSE]

  nYear <- length(data@Year)
  nInd  <- ncol(Val)
  yr_idx <- match(data@Year, rownames(Val))

  AddInd    <- array(NA_real_, dim = c(1, nInd, nYear))
  CV_AddInd <- array(NA_real_, dim = c(1, nInd, nYear))
  for (j in seq_len(nInd)) {
    AddInd[1, j, ]    <- Val[yr_idx, j]
    CV_AddInd[1, j, ] <- if (!is.na(CV_Ind)) CV_Ind else CV[yr_idx, j]
  }

  data@AddInd     <- AddInd
  data@CV_AddInd  <- CV_AddInd
  data@AddIunits  <- rep(1, nInd)
  data@AddIndType <- rep(1, nInd)
  data
}

#' Cap the year-on-year change in a management procedure's TAC recommendation
#'
#' Bounds `NewTAC` to within `+/- MaxChange` proportion of `LastTAC`. If
#' `NewTAC` is not finite, `LastTAC` is returned instead, and a warning is
#' recorded in `Log`.
#'
#' @param NewTAC Numeric. Proposed TAC.
#' @param LastTAC Numeric. Previous TAC; used both as the fallback value and
#'   as the base of the `MaxChange` cap.
#' @param MaxChange Numeric. Maximum proportional change (up or down)
#'   permitted relative to `LastTAC` (e.g. `0.4` allows -40\%/+40\%).
#'
#' @return A `list` with elements `TAC` (the capped TAC) and `Log` (`NULL`,
#'   or a warning message if `NewTAC` was non-finite).
#' @export
Cap_TAC_Change <- function(NewTAC, LastTAC, MaxChange) {

  if (!is.finite(NewTAC)) {
    return(list(TAC = LastTAC, Log = list(warning = "non-finite TAC; using previous TAC")))
  }

  deltaTAC <- NewTAC / LastTAC
  if (deltaTAC > (1 + MaxChange)) NewTAC <- LastTAC * (1 + MaxChange)
  if (deltaTAC < (1 - MaxChange)) NewTAC <- LastTAC * (1 - MaxChange)

  list(TAC = NewTAC, Log = NULL)
}
