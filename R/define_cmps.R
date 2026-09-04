
#' Cap the year-on-year change in a management procedure's TAC recommendation
#'
#' Bounds `NewTAC` to within `-MaxChange[1]`/`+MaxChange[2]` proportion of
#' `LastTAC`. If `NewTAC` is not finite, `LastTAC` is returned instead, and a
#' warning is recorded in `Log`.
#'
#' @param NewTAC Numeric. Proposed TAC.
#' @param LastTAC Numeric. Previous TAC.
#' @param MaxChange Numeric, length 1 or 2. Maximum proportional decrease/
#'   increase permitted relative to `LastTAC`. A single value (e.g. `0.4`)
#'   applies symmetrically (-40\%/+40\%); `c(MaxDown, MaxUp)` (e.g.
#'   `c(0.4, 0.2)`) applies asymmetrically.
#'
#' @return A `list` with elements `TAC` (the capped TAC) and `Log` (`NULL`,
#'   or a warning message if `NewTAC` was non-finite).
#' @export
Cap_TAC_Change <- function(NewTAC, LastTAC, MaxChange) {

  if (!is.finite(NewTAC)) {
    return(list(TAC = LastTAC, Log = list(warning = "non-finite TAC; using previous TAC")))
  }

  MaxChange <- rep_len(MaxChange, 2)
  MaxDown <- MaxChange[1]
  MaxUp   <- MaxChange[2]

  deltaTAC <- NewTAC / LastTAC
  if (deltaTAC > (1 + MaxUp))   NewTAC <- LastTAC * (1 + MaxUp)
  if (deltaTAC < (1 - MaxDown)) NewTAC <- LastTAC * (1 - MaxDown)

  list(TAC = NewTAC, Log = NULL)
}
