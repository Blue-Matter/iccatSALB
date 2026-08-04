library(iccatSALB)
library(MSEtool)

# ---- Combined Abundance Index ----
# Combines the abundance indices available in the terminal year into a single
# index series, using a weighted mean across those indices in each year,
# weighted by the inverse of their CV (equal weighting where a CV is not
# available for a given index/year).
CombinedIndex <- function(Data) {
  Val <- Data@Survey@Value
  CV  <- Data@Survey@CV

  use_cols <- which(!is.na(Val[nrow(Val), ]))

  Val <- Val[, use_cols, drop = FALSE]
  CV  <- CV[, use_cols, drop = FALSE]

  Wt <- 1 / CV
  Wt[!is.finite(Wt)] <- NA
  Wt[is.na(Wt) & !is.na(Val)] <- 1

  Index <- rowSums(Val * Wt, na.rm = TRUE) / rowSums(Wt, na.rm = TRUE)
  names(Index) <- rownames(Val)
  Index
}

# ----- Surplus Production Model ----
SP_FMSY <- function(Data, MSY_frac=1, MaxChange=0.2, ...) {

  data <- Build_Legacy_Data_MultiIndex(Data, CV = 0.2)
  nInd <- dim(data@AddInd)[2]

  # TRY SPICT?

  do_Assessment <- SAMtool::SP(
    x = 1,
    Data = data,
    AddInd = seq_len(nInd),
    start  = list(n = 1, dep = 0.9),
    fix_n  = TRUE,     # Fox model, BMSY/K = 0.37 — matches continuity run
    fix_dep = TRUE,    # dep = 0.9 anchor, approximating Beta(0.9, CV=0.1)
    prior  = list(r = c(0.2, 1))   # exact match to JABBA's r ~ ln(log(0.2), 1)
  )


  Rec <- SAMtool::HCR_MSY(Assessment = do_Assessment, MSY_frac = MSY_frac)

  Cap <- Cap_TAC_Change(as.numeric(Rec@TAC), LastTAC(Data), MaxChange)

  advice <- Advice()
  advice@TAC <- Cap$TAC
  if (!is.null(Cap$Log)) advice@Log <- Cap$Log
  advice
}
class(SP_FMSY) <- 'mp'


SP_75FMSY <- SP_FMSY
formals(SP_75FMSY)$MSY_frac <- 0.75
class(SP_75FMSY) <- 'mp'

# ---- Index Ratio ----

IRatio <- function(Data, MaxChange=0.4) {

  data <- Build_Legacy_Data(Data, CombinedIndex)

  Rec <- DLMtool::Iratio(1, data, reps=1)

  Cap <- Cap_TAC_Change(as.numeric(Rec@TAC), LastTAC(Data), MaxChange)

  advice <- Advice()
  advice@TAC <- Cap$TAC
  if (!is.null(Cap$Log)) advice@Log <- Cap$Log
  advice
}
class(IRatio) <- 'mp'

ISlope <- function(Data, MaxChange=0.4) {

  data <- Build_Legacy_Data(Data, CombinedIndex)

  Rec <- DLMtool::Islope1(1, data, reps=1, xx=0)

  Cap <- Cap_TAC_Change(as.numeric(Rec@TAC), LastTAC(Data), MaxChange)

  advice <- Advice()
  advice@TAC <- Cap$TAC
  if (!is.null(Cap$Log)) advice@Log <- Cap$Log
  advice
}
class(ISlope) <- 'mp'

# ---- Stepped TAC ----

# Based on MCC methods adopted for NSWO

MCC1 <- function(Data, tunepar = 1) {

  # 2024 catch
  landings <- Landings(Data)  |> Value()
  ind <- MSEtool::LastHistYearInd(Data)

  TACbase <- sum(landings[ind,]) * tunepar

  HistRefYears <- 2007:2010
  Index <- CombinedIndex(Data)
  Ibase <- mean(Index[match(HistRefYears, names(Index))], na.rm = TRUE)
  Icurr <- mean(tail(Index, 3))
  Irat  <- Icurr / Ibase

  # TAC multiplier by index ratio (Icurr/Ibase); below the lowest breakpoint
  # the stock is considered in poor condition and a fixed low TAC is used
  # instead of a multiplier
  IratBreaks <- c(0.50, 0.75, 1.20, 1.30, 1.40, 1.50, 1.60, 1.70)
  TACmult    <- c(0.75, 1.00, 1.20, 1.30, 1.40, 1.50, 1.60, 1.70)
  fixed_low_TAC <- 5000

  ind <- findInterval(Irat, IratBreaks)
  TAC <- if (ind == 0) fixed_low_TAC else TACbase * TACmult[ind]

  Advice(TAC = TAC)
}
class(MCC1) <- 'mp'

MCC2 <- MCC1
formals(MCC2)$tunepar <- 1.1
class(MCC2) <- 'mp'
