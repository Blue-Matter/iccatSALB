library(iccatSALB)
library(MSEtool)


# ---- 1. Model-Based Surplus Production (SPiCT) ----
# DROPPED -- could not get decent performance
Build_SPiCT_Inp <- function(Data, CV_Ind = NA, ini = NULL) {

  Val <- methods::slot(Data, 'Survey')@Value
  CV  <- methods::slot(Data, 'Survey')@CV
  use_cols <- which(!is.na(Val[nrow(Val), ]))
  Val <- Val[, use_cols, drop = FALSE]
  CV  <- CV[,  use_cols, drop = FALSE]
  nInd <- ncol(Val)

  Year  <- as.numeric(rownames(Val))
  Catch <- rowSums(Landings(Data) |> Value(), na.rm = TRUE) +
           rowSums(Discards(Data) |> Value(), na.rm = TRUE)

  timeI     <- vector('list', nInd)
  obsI      <- vector('list', nInd)
  stdevfacI <- vector('list', nInd)
  for (j in seq_len(nInd)) {
    val <- Val[, j]
    cv  <- if (!is.na(CV_Ind)) rep(CV_Ind, length(val)) else CV[, j]
    use <- which(!is.na(val))
    timeI[[j]]     <- Year[use]
    obsI[[j]]      <- val[use]
    stdevfacI[[j]] <- cv[use]
  }

  baseline_cv <- mean(unlist(stdevfacI), na.rm = TRUE)
  stdevfacI <- lapply(stdevfacI, function(cv) cv / baseline_cv)

  if (!is.null(ini)) {
    mismatched <- vapply(c('logq', 'logsdi'), function(nm) {
      !is.null(ini[[nm]]) && length(ini[[nm]]) != nInd
    }, logical(1))
    if (any(mismatched)) ini <- NULL
  }

  inp <- list(
    timeC     = Year,
    obsC      = as.numeric(Catch),
    timeI     = timeI,
    obsI      = obsI,
    stdevfacI = stdevfacI
  )
  if (!is.null(ini)) inp$ini <- ini
  inp
}


SPiCT_Fit <- function(inp, dteuler = 0.25, n_fixed = 2,
                       r_center = 0.8, logr_prior_sd = 0.3,
                       K_center = 48549, logK_sd = 0.2) {

  inp$dteuler      <- dteuler
  inp$reportall    <- FALSE
  inp$do.sd.report <- FALSE

  inp$phases$logn <- -1
  inp$ini$logn    <- log(n_fixed)
  inp$priors$logr <- c(log(r_center), logr_prior_sd, 1)
  if (!is.na(logK_sd)) inp$priors$logK <- c(log(K_center), logK_sd, 1)

  fit <- tryCatch({
    inp <- spict::check.inp(inp, verbose = FALSE)
    suppressWarnings(spict::fit.spict(inp, verbose = FALSE))
  }, error = function(e) NULL)

  pdHess <- FALSE
  if (!is.null(fit)) {
    hess <- tryCatch(
      stats::optimHess(fit$opt$par, fit$obj$fn, fit$obj$gr),
      error = function(e) NULL
    )
    pdHess <- !is.null(hess) && !is.character(try(chol(hess), silent = TRUE))
  }

  converged <- !is.null(fit) && isTRUE(fit$opt$convergence == 0) && pdHess
  if (!converged) return(NULL)
  fit
}

SPiCT_HCR <- function(Data, tunepar = 1, Responsiveness = 1,
                       BBmsy_scalar = 1, limitB = 0, MaxChange = c(0.3, 0.5), CV_Ind = 0.2,
                       dteuler = 0.25, n_fixed = 2,
                       r_center = 0.8, logr_prior_sd = 0.3,
                       K_center = 48549, logK_sd = 0.2, ...) {

  SPiCT_ini <- Data@Misc$SPiCT_ini
  inp <- Build_SPiCT_Inp(Data, CV_Ind = CV_Ind, ini = SPiCT_ini)
  fit <- SPiCT_Fit(inp, dteuler = dteuler, n_fixed = n_fixed,
                    r_center = r_center, logr_prior_sd = logr_prior_sd,
                    K_center = K_center, logK_sd = logK_sd)

  PrevTAC <- LastTAC(Data)
  TAC <- NA_real_
  if (!is.null(fit)) {
    rep <- fit$obj$report()
    Bmsy_est <- rep$Bmsy
    B_est    <- exp(utils::tail(fit$pl$logB, 1))

    BBmsy <- (B_est / Bmsy_est) * BBmsy_scalar
    Mod   <- exp(log(BBmsy) * Responsiveness)
    if (limitB > 0 && BBmsy < limitB) Mod <- min(Mod, 1)

    TAC <- PrevTAC * Mod * tunepar

    par_fixed <- fit$opt$par
    SPiCT_ini <- split(unname(par_fixed), names(par_fixed))
  } else {
    TAC <- PrevTAC
  }

  Cap <- iccatSALB::Cap_TAC_Change(TAC, PrevTAC, MaxChange)

  advice <- Advice()
  advice@TAC <- Cap$TAC
  if (!is.null(Cap$Log)) advice@Log <- Cap$Log
  advice@Misc <- list(SPiCT_ini = SPiCT_ini)
  advice
}
class(SPiCT_HCR) <- 'mp'


# ---- 2. Model-Free Index Rate ----
ActiveIndices <- function(Data, IndexSource = c('Survey', 'CPUE')) {
  IndexSource <- match.arg(IndexSource)
  Val <- methods::slot(Data, IndexSource)@Value
  colnames(Val)[!is.na(Val[nrow(Val), ])]
}

IndexRefLevel <- function(Data, Indices, Years = 2007:2010,
                           IndexSource = c('Survey', 'CPUE')) {
  IndexSource <- match.arg(IndexSource)
  Val <- methods::slot(Data, IndexSource)@Value
  yr_idx <- match(as.character(Years), rownames(Val))
  colMeans(Val[yr_idx, Indices, drop = FALSE], na.rm = TRUE)
}

Index_Weights <- function(Data,
                          Indices,
                          WeightMethod = c('equal', 'invCV', 'invVar'),
                          IndexSource = c('Survey', 'CPUE')) {

  WeightMethod <- match.arg(WeightMethod)
  IndexSource  <- match.arg(IndexSource)
  CV <- methods::slot(Data, IndexSource)@CV
  CV_terminal <- CV[nrow(CV), Indices]
  Wt <- switch(WeightMethod,
    invCV  = 1 / CV_terminal,
    invVar = 1 / CV_terminal^2,
    equal  = rep(1, length(Indices))
  )
  Wt[!is.finite(Wt)] <- 1
  stats::setNames(Wt, Indices)
}

IndexRate_MP <- function(Data,
                         tunepar = 1,
                         Indices = c('Indx_CTP-LL_TB2', 'Indx_BR_URY-LL', 'Indx_ZAF-BB'),
                         WeightMethod = c('equal', 'invCV', 'invVar'),
                         CalibYears = 1,
                         Smooth = FALSE, ENPMult = 0.3,
                         RecentYears = 1,
                         TrendYears = 5, TrendHorizon = 2,
                         HCRControlPointsIndex = c(0.3, 1),
                         HCRControlPointsRate = c(0, 1),
                         RampType = c('linear', 'smooth'),
                         Responsiveness = 0.5,
                         DeltaDown = c(0.01, 0.5), DeltaUp = c(0.01, 0.5),
                         TACRange = NULL,
                         ...) {
  WeightMethod <- match.arg(WeightMethod)
  RampType <- match.arg(RampType)

  idx <- if (is.null(Indices)) ActiveIndices(Data) else intersect(Indices, ActiveIndices(Data))
  target <- IndexRefLevel(Data, Indices = idx)
  Wt <- Index_Weights(Data, idx, WeightMethod = WeightMethod)

  if (is.null(TACRange)) {
    Removals <- rowSums(Landings(Data) |> Value(), na.rm = TRUE) +
                rowSums(Discards(Data) |> Value(), na.rm = TRUE)
    TACRange <- c(0, max(Removals, na.rm = TRUE))
  }

  MSEtool::IndexRate(Data,
                     IndexSource = 'Survey',
                     Indices = idx,
                     IndexTarget = target,
                     IndexWeight = Wt,
                     CalibYears = CalibYears,
                     Smooth = Smooth, ENPMult = ENPMult,
                     RecentYears = RecentYears,
                     TrendYears = TrendYears, TrendHorizon = TrendHorizon,
                     HCRControlPointsIndex = HCRControlPointsIndex,
                     HCRControlPointsRate = HCRControlPointsRate,
                     RampType = RampType,
                     Responsiveness = Responsiveness,
                     IndexFactor = tunepar,
                     DeltaDown = DeltaDown, DeltaUp = DeltaUp,
                     TACRange = TACRange, ...)
}
class(IndexRate_MP) <- 'mp'

## ---- Tuned variants ----

IndexRate_76 <- IndexRate_MP
formals(IndexRate_76)$tunepar <- 0.76
class(IndexRate_76) <- 'mp'

IndexRate_MC20_72 <- IndexRate_MP
formals(IndexRate_MC20_72)$tunepar <- 0.72
formals(IndexRate_MC20_72)$DeltaDown <- c(0.01, 0.2)
formals(IndexRate_MC20_72)$DeltaUp <- c(0.01, 0.2)
class(IndexRate_MC20_72) <- 'mp'

# ---- 3. Stepped Index-Based (MCC) ----
# Based on the MCC design ICCAT adopted for North Atlantic swordfish.

CombinedIndex <- function(Data, WeightMethod = c('equal', 'invCV', 'invVar'),
                           IndexSource = c('Survey', 'CPUE'), Indices = NULL) {
  WeightMethod <- match.arg(WeightMethod)
  IndexSource  <- match.arg(IndexSource)
  Val <- methods::slot(Data, IndexSource)@Value

  use_cols <- which(!is.na(Val[nrow(Val), ]))
  Val <- Val[, use_cols, drop = FALSE]
  if (!is.null(Indices)) Val <- Val[, intersect(colnames(Val), Indices), drop = FALSE]
  idx <- colnames(Val)

  Wt_vec <- Index_Weights(Data, idx, WeightMethod = WeightMethod, IndexSource = IndexSource)
  Wt <- matrix(Wt_vec, nrow(Val), length(Wt_vec), byrow = TRUE, dimnames = dimnames(Val))
  Wt[is.na(Val)] <- NA

  Index <- rowSums(Val * Wt, na.rm = TRUE) / rowSums(Wt, na.rm = TRUE)
  names(Index) <- rownames(Val)
  Index
}

MCC_HCR <- function(Data, tunepar = 1, HistRefYears = 2000:2024, RecentYears = 1,
                     IratBreaks = c(0.65, 0.90, 1.10, 1.3, 1.4, 1.5, 1.6, 1.7),
                     TACmult    = c(0.45, 0.80, 1,    1.2, 1.3, 1.4, 1.5, 1.6, 1.7),
                     WeightMethod = c('equal', 'invCV', 'invVar'), Indices = NULL, ...) {

  WeightMethod <- match.arg(WeightMethod)
  Index <- CombinedIndex(Data, WeightMethod = WeightMethod, Indices = Indices)
  ref_idx <- match(as.character(HistRefYears), names(Index))
  Ibase <- mean(Index[ref_idx], na.rm = TRUE)
  Icurr <- mean(utils::tail(Index, RecentYears), na.rm = TRUE)
  Irat  <- Icurr / Ibase

  bin <- findInterval(Irat, IratBreaks) + 1

  landings <- Landings(Data) |> Value()
  LHInd    <- MSEtool::LastHistYearInd(Data)
  TACbase  <- sum(landings[LHInd, ]) * tunepar

  TAC <- TACbase * TACmult[bin]

  advice <- Advice()
  advice@TAC <- TAC
  advice
}
class(MCC_HCR) <- 'mp'

## ---- Tuned variants ----

MCC_95 <- MCC_HCR
formals(MCC_95)$tunepar <- 0.95
class(MCC_95) <- 'mp'

MCC_98 <- MCC_HCR
formals(MCC_98)$tunepar <- 0.98
class(MCC_98) <- 'mp'



# ---- 5. Ensemble ----

Ensemble_MP <- function(Data, tunepar = 1,
                         Combiner = c('mean', 'median', 'min'),
                         Components = c('MCC', 'IndexRate'),
                         Weights = c(MCC = 1, IndexRate = 1),
                         MCC_tunepar = 0.95, IR_tunepar = 0.71, IT_tunepar = 1.03,
                         IR_Indices = c('Indx_CTP-LL_TB2', 'Indx_BR_URY-LL', 'Indx_ZAF-BB'),
                         IR_DeltaDown = c(0.01, 0.5), IR_DeltaUp = c(0.01, 0.5),
                         ...) {
  Combiner <- match.arg(Combiner)
  Adv <- list()
  if ('MCC' %in% Components)
    Adv$MCC <- MCC_HCR(Data, tunepar = MCC_tunepar)@TAC
  if ('IndexRate' %in% Components)
    Adv$IndexRate <- IndexRate_MP(Data,
                                  tunepar = IR_tunepar,
                                  Indices = IR_Indices,
                                  DeltaDown = IR_DeltaDown,
                                  DeltaUp = IR_DeltaUp)@TAC
  TACs <- unlist(Adv)
  Wt <- Weights[names(TACs)]
  TAC <- switch(Combiner,
    min    = min(TACs, na.rm = TRUE),
    median = stats::median(TACs, na.rm = TRUE),
    mean   = stats::weighted.mean(TACs, Wt, na.rm = TRUE)
  )
  TAC <- TAC * tunepar
  Advice(TAC = TAC, TACType = 'Removals', TACUnit = Data@Landings@Units)
}
class(Ensemble_MP) <- 'mp'

## ---- Tuned variants ----

Ensemble_MCCIR_103 <- Ensemble_MP
formals(Ensemble_MCCIR_103)$tunepar <- 1.03
class(Ensemble_MCCIR_103) <- 'mp'

Ensemble_MCCIR20_102 <- Ensemble_MP
formals(Ensemble_MCCIR20_102)$tunepar <- 1.02
formals(Ensemble_MCCIR20_102)$IR_tunepar <- 0.72
formals(Ensemble_MCCIR20_102)$IR_DeltaDown <- c(0.01, 0.2)
formals(Ensemble_MCCIR20_102)$IR_DeltaUp <- c(0.01, 0.2)
class(Ensemble_MCCIR20_102) <- 'mp'




# ---- Final set of CMPs ----
MP_Groups <- list(
  Reference         = c('refFMSY', 'refFMSY75', 'refFMSY50', 'refFCurr', 'NoFishing'),
  MCC               = c('MCC_98'),
  IndexRate         = c('IndexRate_76'),
  IndexRate_MC20    = c('IndexRate_MC20_72'),
  Ensemble          = c('Ensemble_MCCIR_103'),
  EnsembleIR20      = c('Ensemble_MCCIR20_102')
)

