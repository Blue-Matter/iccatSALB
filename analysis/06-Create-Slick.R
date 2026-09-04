library(iccatSALB)
library(MSEtool)
library(Slick)
library(patchwork)

source('analysis/00-Specifications.R')
source('analysis/04-Define-CMPs.R')

FileIndex <- GetMSEFiles('objects/MSE')
Families_Present <- intersect(names(MP_Groups), unique(FileIndex$Family))
demoted_families  <- setdiff(unique(FileIndex$Family), names(MP_Groups))

if (length(demoted_families))
  cli::cli_inform("Family/families on disk but no longer in {.arg MP_Groups} (excluded from this Slick build): {.val {demoted_families}}.")

Families_Present <- setdiff(Families_Present, c('Reference', 'MultiIndexAgreement'))

MP_Groups     <- MP_Groups[Families_Present]
FileIndex     <- FileIndex[FileIndex$Family %in% Families_Present, ]
MP_Groups$MCC <- 'MCC_98'
nMP           <- length(unlist(MP_Groups))

# ---- OM design ----
OM_Design <- unique(FileIndex[, c('OM_Type', 'OM', 'Growth', 'M', 'OM_Name')])
OM_Type_Order <- c('Reference', 'R1', 'R2')
OM_Design <- OM_Design[order(match(OM_Design$OM_Type, OM_Type_Order),
                              OM_Design$Growth, OM_Design$M), ]
rownames(OM_Design) <- NULL
nOM <- nrow(OM_Design)

# ---- MP metadata ----
MP_all      <- unlist(MP_Groups, use.names = FALSE)
MP_FamilyNm <- rep(names(MP_Groups), lengths(MP_Groups))
names(MP_FamilyNm) <- MP_all

MP_ShortCode <- function(x) {
  x |>
    sub('^MCC_[0-9]+',                  'MCC',    x = _) |>
    sub('^IndexRate_All3_MC20_[0-9]+',  'IR_20',  x = _) |>
    sub('^IndexRate_All3_[0-9]+',       'IR',     x = _) |>
    sub('^Ensemble_MCCIR20_[0-9]+',     'Ens_20', x = _) |>
    sub('^Ensemble_MCCIR_[0-9]+',       'Ens',    x = _) |>
    sub('^MIA_[0-9]+',                  'MIA',    x = _)
}

MP_ShortLabel <- function(x) {
  x |>
    sub('^MCC_[0-9]+',                  'Mostly Constant Catch',      x = _) |>
    sub('^IndexRate_All3_MC20_[0-9]+',  'Index Rate 20% Cap',         x = _) |>
    sub('^IndexRate_All3_[0-9]+',       'Index Rate',                 x = _) |>
    sub('^Ensemble_MCCIR20_[0-9]+',     'Ensemble 20% Cap',           x = _) |>
    sub('^Ensemble_MCCIR_[0-9]+',       'Ensemble',                   x = _) |>
    sub('^MIA_[0-9]+',                  'Multi-Index MCC',            x = _)
}

MP_Code        <- MP_ShortCode(MP_all)
MP_Label       <- MP_ShortLabel(MP_all)

MP_Description <- c(
  'Stepped index-based Mostly Constant Catch HCR (based on the MP adopted for north Atlantic swordfish). TAC fixed at a multiple of the last historical catch, stepped by which bin a the combined-index ratio falls into.',
  'Catch-per-index rate calibrated from recent history, throttled by a hockey-stick HCR on current index / target, using all 3 survey indices.',
  'Same as `IndexRate`, with a +/-20% per-cycle TAC change cap.',
  "Average of `MCC` and `IndexRate` TAC advice.",
  "Same as `Ensemble`, but blending `IndexRate`'s +/-20% per-cycle TAC change cap variant instead of the uncapped one."
)

# ---- PM panel (Boxplot / Quilt) and Spider subset ----

PM_Codes <- c('Safety',
              'Status_All',
              'SB_SBMSY_All',
              'F_FMSY_All',
              'Yield_All',

              'Depletion_Min',
              'Stability',
              'ATV',

              'Status_Short',
              'Status_Medium',
              'Status_Long',

              'Yield_Short',
              'Yield_Medium',
              'Yield_Long',

              'SB_SBMSY_Short',
              'SB_SBMSY_Medium',
              'SB_SBMSY_Long',

              'F_FMSY_Short',
              'F_FMSY_Medium',
              'F_FMSY_Long')

nPI_pm <- length(PM_Codes)

PM_Label <- c(
  Safety              = 'Safety',
  Status_All          = 'Status (all years)',
  SB_SBMSY_All        = 'SB/SBMSY (all years)',
  F_FMSY_All          = 'F/FMSY (all years)',
  Yield_All           = 'Yield (all years)',

  Depletion_Min       = 'Minimum depletion',
  Stability         = 'Stability (20% threshold)',
  ATV                 = 'Average TAC variability',

  Status_Short        = 'Status (years 1-10)',
  Status_Medium       = 'Status (years 11-20)',
  Status_Long         = 'Status (years 21-30)',

  Yield_Short         = 'Yield (years 1-10)',
  Yield_Medium        = 'Yield (years 11-20)',
  Yield_Long          = 'Yield (years 21-30)',

  SB_SBMSY_Short      = 'SB/SBMSY (years 1-10)',
  SB_SBMSY_Medium     = 'SB/SBMSY (years 11-20)',
  SB_SBMSY_Long       = 'SB/SBMSY (years 21-30)',

  F_FMSY_Short        = 'F/FMSY (years 1-10)',
  F_FMSY_Medium       = 'F/FMSY (years 11-20)',
  F_FMSY_Long         = 'F/FMSY (years 21-30)',

  Rel_Yield           = 'Relative yield'
)

PM_Description <- c(
  Safety              = paste0('Probability that SB/SBMSY never drops below ', BlimFrac, ' at any point in the projection.'),
  Status_All          = 'Probability that SB > SBMSY and F < FMSY simultaneously, over all projection years.',
  SB_SBMSY_All        = 'Mean annual SB/SBMSY ratio over all projection years.',
  F_FMSY_All          = 'Mean annual F/FMSY ratio over all projection years.',
  Yield_All           = 'Mean annual catch over all projection years.',

  Depletion_Min       = 'Lowest annual SB/SB0 (depletion) reached during the projection.',
  Stability         = paste0('Probability that TAC changes by less than ', 100 * StabilityThreshold, '% between consecutive management cycles.'),
  ATV                = 'Mean yield variability between management cycles across the projection.',

  Status_Short        = 'Probability that SB > SBMSY and F < FMSY simultaneously, over the first 10 projection years (short-term).',
  Status_Medium       = 'Probability that SB > SBMSY and F < FMSY simultaneously, over the middle 10 projection years (medium-term).',
  Status_Long         = 'Probability that SB > SBMSY and F < FMSY simultaneously, over the final 10 projection years (long-term).',

  Yield_Short         = 'Mean annual catch over the first 10 projection years (short-term).',
  Yield_Medium        = 'Mean annual catch over the middle 10 projection years (medium-term).',
  Yield_Long          = 'Mean annual catch over the final 10 projection years (long-term).',

  SB_SBMSY_Short      = 'Mean annual SB/SBMSY ratio over the first 10 projection years (short-term).',
  SB_SBMSY_Medium     = 'Mean annual SB/SBMSY ratio over the middle 10 projection years (medium-term).',
  SB_SBMSY_Long       = 'Mean annual SB/SBMSY ratio over the final 10 projection years (long-term).',

  F_FMSY_Short        = 'Mean annual F/FMSY ratio over the first 10 projection years (short-term).',
  F_FMSY_Medium       = 'Mean annual F/FMSY ratio over the middle 10 projection years (medium-term).',
  F_FMSY_Long         = 'Mean annual F/FMSY ratio over the final 10 projection years (long-term).',

  Rel_Yield           = 'Mean annual catch (all years) relative to the highest-yielding MP in the same OM.'
)

Spider_Codes <- c('Safety',
                  'Stability',
                  'Status_All',
                  'Rel_Yield',
                  'Depletion_Min')

Headline_Codes  <- c('Safety',
                     'Status_All',
                     'Yield_All',
                     'Depletion_Min',
                     'Stability',
                     'ATV')

Tradeoff_Codes  <- c('Safety',
                     'Status_All',
                     'Yield_All',
                     'Depletion_Min')

TS_Codes <- c('SB_SBMSY', 'F_FMSY', 'Yield')
TS_Label <- c(SB_SBMSY = 'SB/SBMSY', F_FMSY = 'F/FMSY', Yield = 'Yield')
TS_Description <- c(
  SB_SBMSY = 'Spawning biomass relative to SBMSY.',
  F_FMSY   = 'Fishing mortality relative to FMSY.',
  Yield    = 'Total catch (t) across all fleets.'
)

TS_Target <- c(SB_SBMSY = 1,        F_FMSY = NA, Yield = NA)
TS_Limit  <- c(SB_SBMSY = BlimFrac, F_FMSY = 1,  Yield = NA)

# ---- Pre-allocate accumulator arrays ----
nsim <- OMSpecs$nSim

PM_Value <- array(NA_real_, dim = c(nsim, nOM, nMP, nPI_pm),
                   dimnames = list(Sim = seq_len(nsim), OM = OM_Design$OM_Name,
                                    MP = MP_all, PI = PM_Codes))

TS_Value <- NULL
Time     <- NULL
TimeNow  <- NULL

# ---- Sequence through one `.mse` in memory at a time ----
cli::cli_progress_bar(
  "Processing OMs",
  total = nrow(FileIndex)
)

for (i in seq_len(nrow(FileIndex))) {
  row    <- FileIndex[i, ]
  om_idx <- match(row$OM_Name, OM_Design$OM_Name)
  cli::cli_progress_step(
    "[{i}/{nrow(FileIndex)}] {format(row$Family, width = 19)} {format(row$OM_Type, width = 9)} {row$OM}",
    msg_done = "[{i}/{nrow(FileIndex)}] {row$Family} done"
  )

  MSE <- readRDS(row$File)

  mp_names <- intersect(names(MSE@MPs), MP_all)
  if (length(mp_names) == 0)
    cli::cli_abort("None of the MPs in {.file {row$File}} are in the kept {.arg MP_Groups} set.")
  mp_idx <- match(mp_names, MP_all)

  # ---- PM panel (per-sim) -> Boxplot / Quilt / Spider ----
  pmstats     <- Calc_PM_Stats(MSE, BlimFrac = BlimFrac, StabilityThreshold = StabilityThreshold)
  pmstats     <- pmstats[pmstats$MP %in% mp_names & pmstats$PM != 'Stability30', ]
  pi_idx      <- match(pmstats$PI, PM_Codes)
  mp_idx_pm   <- match(pmstats$MP, MP_all)
  if (anyNA(pi_idx))
    cli::cli_abort("`Calc_PM_Stats()` returned PI{?s} not in {.arg PM_Codes}: {.val {unique(pmstats$PI[is.na(pi_idx)])}}.")

  PM_Value[cbind(pmstats$Sim, om_idx, mp_idx_pm, pi_idx)] <- pmstats$Value

  # ---- Time series (per-sim, per-year) -> Timeseries / Kobe ----
  ts <- Extract_MSE_Timeseries(MSE)

  if (is.null(TS_Value)) {
    Time    <- sort(unique(ts$Year))
    TimeNow <- max(ts$Year[ts$Period == 'Historical'])
    nTS     <- length(Time)

    TS_Value <- array(NA_real_, dim = c(nsim, nOM, nMP, length(TS_Codes), nTS),
                       dimnames = list(Sim = seq_len(nsim), OM = OM_Design$OM_Name,
                                        MP = MP_all, PI = TS_Codes, Time = Time))
  }

  proj <- ts[ts$Period == 'Projection' & ts$MP %in% mp_names, ]
  TS_Value[cbind(proj$Sim, om_idx, match(proj$MP, MP_all),
                 match(proj$Variable, TS_Codes), match(proj$Year, Time))] <- proj$Value

  hist <- ts[ts$Period == 'Historical', ]
  for (h in seq_len(nrow(hist))) {
    TS_Value[, om_idx, mp_idx, match(hist$Variable[h], TS_Codes), match(hist$Year[h], Time)] <- hist$Value[h]
  }

  rm(MSE)
  gc()
}

# ---- OMs ----
OMs_obj <- Slick::OMs()

Slick::Factors(OMs_obj) <- data.frame(
  Factor = c(rep('OM_Type', 3), rep('Growth', 3), rep('M', 3)),
  Level  = c('Reference', 'R1', 'R2', '25', '50', '75', '25', '50', '75'),
  Description = c(
    'Reference Grid: 9 OMs with two axes Growth and Natural Mortality, each with 3 levels.',
    'Robustness 1 Grid: Same grid structure but conditioned with Brazilianâ€“Uruguay index excluded from the likelihood.',
    'Robustness 2 Grid: Same grid structure but conditioned with Chinese Taipei index excluded from the likelihood.',
    '25th percentile growth curve (Linf, K, t0)',
    '50th percentile growth curve (Linf, K, t0)',
    '75th percentile growth curve (Linf, K, t0)',
    '25th percentile natural mortality (M = 0.29)',
    '50th percentile natural mortality (M = 0.36)',
    '75th percentile natural mortality (M = 0.44)'
  )
)

Slick::Design(OMs_obj) <- data.frame(
  OM_Type = OM_Design$OM_Type,
  Growth  = as.character(OM_Design$Growth),
  M       = as.character(OM_Design$M)
)
rownames(Slick::Design(OMs_obj)) <- OM_Design$OM_Name

Slick::Preset(OMs_obj) <- list(
  Reference = list(1, 1:3, 1:3),
  R1        = list(2, 1:3, 1:3),
  R2        = list(3, 1:3, 1:3),
  All       = list(1:3, 1:3, 1:3)
)

# ---- MPs ----
MPs_obj <- Slick::MPs(Code = MP_Code, Label = MP_Label, Description = MP_Description)

Slick::Preset(MPs_obj) <- list(
  All = seq_along(MP_all)
)


# ---- Boxplot / Quilt / Spider ----
Boxplot_obj <- Slick::Boxplot(Code = PM_Codes, Label = unname(PM_Label[PM_Codes]),
                               Description = unname(PM_Description[PM_Codes]),
                               Value = PM_Value)


Prob01_Codes <- c('Safety',
                  'Stability',
                  'Status_All',
                  'Status_Short',
                  'Status_Medium',
                  'Status_Long')

Quilt_Codes <- setdiff(PM_Codes, c('ATV', 'Depletion_Min'))
nPI_quilt   <- length(Quilt_Codes)

Quilt_MinValue <- rep(NA_real_, nPI_quilt)
Quilt_MaxValue <- rep(NA_real_, nPI_quilt)
Quilt_MinValue[match(Prob01_Codes, Quilt_Codes)] <- 0
Quilt_MaxValue[match(Prob01_Codes, Quilt_Codes)] <- 1

Quilt_obj <- Slick::Quilt(Code = Quilt_Codes, Label = unname(PM_Label[Quilt_Codes]),
                           Description = unname(PM_Description[Quilt_Codes]),
                           Value = PM_Value[, , , Quilt_Codes, drop = FALSE],
                           MinValue = Quilt_MinValue, MaxValue = Quilt_MaxValue)

non_windowed_PM <- c('Safety', 'Stability', 'ATV', 'Depletion_Min')

PM_Preset <- list(
  Summary    = match(Headline_Codes, PM_Codes),
  All        = seq_along(PM_Codes),
  ShortTerm  = match(c(grep('_Short$',  PM_Codes, value = TRUE), non_windowed_PM), PM_Codes),
  MediumTerm = match(c(grep('_Medium$', PM_Codes, value = TRUE), non_windowed_PM), PM_Codes),
  LongTerm   = match(c(grep('_Long$',   PM_Codes, value = TRUE), non_windowed_PM), PM_Codes)
)

Quilt_non_windowed_PM <- setdiff(non_windowed_PM, c('ATV', 'Depletion_Min'))

Quilt_Preset <- list(
  Summary    = match(setdiff(Headline_Codes, c('ATV', 'Depletion_Min')), Quilt_Codes),
  All        = seq_along(Quilt_Codes),
  ShortTerm  = match(c(grep('_Short$',  Quilt_Codes, value = TRUE), Quilt_non_windowed_PM), Quilt_Codes),
  MediumTerm = match(c(grep('_Medium$', Quilt_Codes, value = TRUE), Quilt_non_windowed_PM), Quilt_Codes),
  LongTerm   = match(c(grep('_Long$',   Quilt_Codes, value = TRUE), Quilt_non_windowed_PM), Quilt_Codes)
)

Slick::Preset(Boxplot_obj) <- PM_Preset
Slick::Preset(Quilt_obj)   <- Quilt_Preset

.Agg_PM <- function(codes) {
  arr <- array(NA_real_, dim = c(nOM, nMP, length(codes)),
               dimnames = list(OM = OM_Design$OM_Name, MP = MP_all, PI = codes))
  for (code in codes) {
    FUN <- if (code == 'Depletion_Min') min else mean
    arr[, , code] <- apply(PM_Value[, , , code, drop = FALSE], c(2, 3), FUN, na.rm = TRUE)
  }
  arr
}

Spider_PM_Codes <- setdiff(Spider_Codes, 'Rel_Yield')
Spider_Value <- array(NA_real_, dim = c(nOM, nMP, length(Spider_Codes)),
                       dimnames = list(OM = OM_Design$OM_Name, MP = MP_all, PI = Spider_Codes))
Spider_Value[, , Spider_PM_Codes] <- .Agg_PM(Spider_PM_Codes)

if ('Rel_Yield' %in% Spider_Codes) {
  Yield_OM_MP <- apply(PM_Value[, , , 'Yield_All'], c(2, 3), mean, na.rm = TRUE)
  Spider_Value[, , 'Rel_Yield'] <- Yield_OM_MP / apply(Yield_OM_MP, 1, max, na.rm = TRUE)
}

Spider_obj <- Slick::Spider(Code = Spider_Codes, Label = unname(PM_Label[Spider_Codes]),
                             Description = unname(PM_Description[Spider_Codes]),
                             Value = Spider_Value)


Tradeoff_Value <- .Agg_PM(Tradeoff_Codes)

Tradeoff_obj <- Slick::Tradeoff(Code = Tradeoff_Codes, Label = unname(PM_Label[Tradeoff_Codes]),
                                 Description = unname(PM_Description[Tradeoff_Codes]),
                                 Value = Tradeoff_Value)

Slick::Preset(Tradeoff_obj) <- Filter(length, list(
  All             = seq_along(Tradeoff_Codes),
  YieldCompliance = match(c('Yield_All', 'Safety', 'Status_All'), Tradeoff_Codes),
  ShortTerm       = match(grep('_Short$',  Tradeoff_Codes, value = TRUE), Tradeoff_Codes),
  MediumTerm      = match(grep('_Medium$', Tradeoff_Codes, value = TRUE), Tradeoff_Codes),
  LongTerm        = match(grep('_Long$',   Tradeoff_Codes, value = TRUE), Tradeoff_Codes)
))


# ---- Timeseries / Kobe ----
Timeseries_obj <- Slick::Timeseries(Code = TS_Codes, Label = unname(TS_Label[TS_Codes]),
                                     Description = unname(TS_Description[TS_Codes]),
                                     Value = TS_Value, Time = Time, TimeNow = TimeNow,
                                     Target = unname(TS_Target[TS_Codes]),
                                     Limit  = unname(TS_Limit[TS_Codes]))



Kobe_obj <- Slick::Kobe(Code = c('SB_SBMSY', 'F_FMSY'),
                         Label = unname(TS_Label[c('SB_SBMSY', 'F_FMSY')]),
                         Description = unname(TS_Description[c('SB_SBMSY', 'F_FMSY')]),
                         Value = TS_Value[, , , c('SB_SBMSY', 'F_FMSY'), which(Time >= 2028), drop = FALSE],
                         Time = Time[which(Time >= 2028), drop = FALSE], Target = c(1, 1), Limit = c(BlimFrac, 1))


# ---- Assemble and save ----
Introduction <- "
  The International Commission for the Conservation of Atlantic Tunas (ICCAT) intends to adopt a simulation-tested management procedure (MP) for southern Atlantic albacore (SALB) in 2027 ([ICCAT MSE Roadmap](https://www.iccat.int/mse/Docs/MSE_Roadmap_ENG.pdf)). To prepare for this, an MSE Technical Team was established in 2025, and work has been conducted to develop a management strategy evaluation (MSE) framework, specify a set of operating models (OMs) describing plausible hypotheses of the fishery dynamics, and a construct range of candidate management procedures (CMPs) for this stock.

The preliminary results presented in this Slick object are intended for review by the Albacore Species Group and the broader Standing Committee of Research and Statistics (SCRS). Feedback from these groups will be used to revise the MSE framework and analysis as necessary, with the aim of presenting the final MSE results in September 2027, on schedule for the Commission’s workplan for the adoption of a management procedure.

This work was funded by the Marine Stewardship Council, The Ocean Foundation, FCF Ltd./Bumblebee SeaFoods, Trimarine, ICV Africa, and Tuna Alliance Ltd.

**The results shown here do not necessarily reflect the viewpoints of ICCAT or other funders and in no ways anticipate ICCAT future policy for this fishery.**

"

slick <- Slick::Slick(Title = 'South Atlantic Albacore (SALB)',
                      Author = c('Adrian Hordyk &  SALB MSE Technical Team'),
                      Email = c('adrian@bluematterscience.com'),
                      Introduction = Introduction,
                      MPs = MPs_obj,
                      OMs = OMs_obj,
                      Boxplot = Boxplot_obj,
                      Kobe = Kobe_obj,
                      Quilt = Quilt_obj,
                      Spider = Spider_obj,
                      Tradeoff = Tradeoff_obj,
                      Timeseries = Timeseries_obj)




print(Slick::Check(slick))

if (!dir.exists('objects')) dir.create('objects')
saveRDS(slick, 'objects/slick.rds')

MSEtool::Save(slick, '../SlickLibrary/Slick_Objects/South_Atlantic_Albacore.slick')

# ---- Run App ---
slick <- readRDS('objects/slick.rds')

Slick::App(slick = slick)  # launch the interactive App

# ---- Make manual plots ----

FigDir <- 'analysis/figures/Slick'
if (!dir.exists(FigDir)) dir.create(FigDir, recursive = TRUE)

OM_Sets <- c('Reference', 'R1', 'R2')

## ---- Timeseries: SB/SBMSY, F/FMSY, Yield ----
for (om_set in OM_Sets) {
  om_idx <- which(OM_Design$OM_Type == om_set)

  TS_Codes <- slick@Timeseries@Code

  # Medians
  p_sb <- Slick::plotTimeseries(slick, PI = match('SB_SBMSY', TS_Codes), OMs = om_idx, includeQuants = FALSE)
  p_f  <- Slick::plotTimeseries(slick, PI = match('F_FMSY',   TS_Codes), OMs = om_idx, includeQuants = FALSE)
  p_y  <- Slick::plotTimeseries(slick, PI = match('Yield',    TS_Codes), OMs = om_idx, includeQuants = FALSE)

  p <- p_sb / p_f / p_y

  ggplot2::ggsave(file.path(FigDir, paste0('Timeseries/', om_set, '.png')),
                   p, width = 10, height = 12, dpi = 300,  create.dir = TRUE)

  # By MP with quantiles
  p_sb <- Slick::plotTimeseries(slick, PI = match('SB_SBMSY', TS_Codes),
                                OMs = om_idx, byMP = TRUE,
                                includeHist = FALSE)

  ggplot2::ggsave(file.path(FigDir, paste0('Timeseries/', om_set, '/SB_SBMSY.png')),
                  p_sb, width = 10, height = 6, dpi = 300, create.dir = TRUE)

  p_f <- Slick::plotTimeseries(slick, PI = match('F_FMSY', TS_Codes),
                               OMs = om_idx, byMP = TRUE,
                               includeHist = FALSE)

  ggplot2::ggsave(file.path(FigDir, paste0('Timeseries/', om_set, '/F_FMSY.png')),
                  p_f, width = 10, height = 6, dpi = 300)

  p_y <- Slick::plotTimeseries(slick, PI = match('Yield', TS_Codes),
                               OMs = om_idx, byMP = TRUE,
                               includeHist = FALSE)

  ggplot2::ggsave(file.path(FigDir, paste0('Timeseries/', om_set, '/Yield.png')),
                  p_y, width = 10, height = 6, dpi = 300)

}

## ---- Kobe and Kobe-time ----
for (om_set in OM_Sets) {
  om_idx <- which(OM_Design$OM_Type == om_set)

  p_kobe <- Slick::plotKobe(slick, xPI = 1, yPI = 2,
                            Time = FALSE, OMs = om_idx,
                            percentile = NULL)

  ggplot2::ggsave(file.path(FigDir, paste0('Kobe/', om_set, '.png')),
                   p_kobe, width = 8, height = 7, dpi = 300, create.dir = TRUE)

  p_kobe_time <- Slick::plotKobe(slick, xPI = 1, yPI = 2, Time = TRUE, OMs = om_idx)
  ggplot2::ggsave(file.path(FigDir, paste0('Kobe/', om_set, '_Time.png')),
                   p_kobe_time, width = 10, height = 5, dpi = 300)
}

## ---- Quilt tables ----
Quilt_Primary_Codes  <- setdiff(Headline_Codes, 'ATV')
Quilt_Extended_Codes <- c(Quilt_Primary_Codes,
                           'Yield_Short', 'Yield_Medium', 'Yield_Long',
                           'Status_Short', 'Status_Medium', 'Status_Long')

Save_Quilt_Table <- function(pi_codes, om_idx, file_stub) {
  pi_idx    <- match(pi_codes, Quilt_Codes)
  slick_sub <- Slick::FilterSlick(slick, OMs = om_idx, PIs = pi_idx, plot = 'Quilt')
  tbl       <- Slick::plotQuilt(slick_sub, kable = TRUE)

  prob_cols <- intersect(Prob01_Codes, pi_codes)
  if (length(prob_cols))
    tbl <- flextable::colformat_double(tbl, j = prob_cols, digits = 2)

  dir <- file.path(FigDir, 'Quilt')
  if (!dir.exists(dir))
    dir.create(dir, recursive = TRUE)
  out_html <- file.path(dir, paste0(file_stub, '.html'))

  htmltools::save_html(flextable::htmltools_value(tbl), file = out_html)

  out_docx <- file.path(dir, paste0(file_stub, '.docx'))
  flextable::save_as_docx(tbl, path = out_docx)

  out_png <- file.path(dir, paste0(file_stub, '.png'))
  saved_png <- tryCatch({
    flextable::save_as_image(tbl, path = out_png)
    TRUE
  }, error = function(e) FALSE)

  if (saved_png) {
    # fix transparent background issue
    img <- png::readPNG(out_png)
    if (length(dim(img)) == 3 && dim(img)[3] == 4) {
      alpha <- img[, , 4]
      for (ch in 1:3) img[, , ch] <- img[, , ch] * alpha + 1 * (1 - alpha)
      png::writePNG(img[, , 1:3], out_png)
    }
  } else {
    cli::cli_inform(c(
      "Could not render {.file {out_png}} ({.pkg webshot2}/{.pkg magick} not installed).",
      "i" = "Saved {.file {out_html}} and {.file {out_docx}} instead -- open one to screenshot."
    ))
  }
}

for (om_set in OM_Sets) {
  om_idx <- which(OM_Design$OM_Type == om_set)

  Save_Quilt_Table(Quilt_Primary_Codes,  om_idx, paste0(om_set, '_Primary'))
  Save_Quilt_Table(Quilt_Extended_Codes, om_idx, paste0(om_set, '_Extended'))
}

