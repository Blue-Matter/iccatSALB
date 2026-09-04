library(iccatSALB)
library(MSEtool)

source('analysis/00-Specifications.R')
source('analysis/04-Define-CMPs.R')

Hist     <- readRDS('objects/Hist/Ref.hist')
RefFiles <- GetHistFiles('objects/Hist') |> dplyr::filter(OM_Type == 'Reference')
RefHists <- lapply(RefFiles$File, readRDS)


Round <- function(generator, grid, prefix, Hist, nSim = NULL) {
  if (is.null(grid$Name)) grid$Name <- paste0(prefix, seq_len(nrow(grid)))
  MPs <- Build_MP_Variants(generator, grid, prefix = prefix)
  out <- Tune_Grid(MPs, Hist, HistYieldRef = HistYieldRef, nSim = nSim)
  out$PM <- dplyr::left_join(out$PM, grid, by = c('MP' = 'Name'))
  out
}

Spanning_Set <- function(PM, n_breach = 2) {
  boundary <- Best_Compliant(PM)
  if (is.null(boundary)) return(NULL)
  tunepar0 <- boundary$tunepar
  above <- sort(PM$tunepar[PM$tunepar > tunepar0])[seq_len(n_breach)]
  data.frame(tunepar = c(tunepar0, above))
}

Narrow_Bracket <- function(center, step, n_each_side = 2) {
  data.frame(tunepar = round(seq(center - n_each_side * step, center + n_each_side * step, by = step), 6))
}

# ---- MCC ----

WM_Grid <- data.frame(tunepar = 0.78, WeightMethod = c('equal', 'invCV', 'invVar'))
WM_Check <- Round(MCC_HCR, WM_Grid, 'MCC_wm', Hist, nSim = 30)
print(as.data.frame(WM_Check$PM), digits = 4)

# Stage 2: coarse, central OM, nSim=30 -- wide bracket, cheap.
MCC_Stage2 <- Round(MCC_HCR, data.frame(tunepar = seq(0.88, 0.92, by = 0.05)), 'MCC_c', Hist, nSim = 30)
print(as.data.frame(MCC_Stage2$PM), digits = 4)


# Stage 3: fine, central OM, full nSim
MCC_center2 <- Best_Compliant(MCC_Stage2$PM)$tunepar
MCC_Stage3 <- Round(MCC_HCR, data.frame(tunepar = seq(MCC_center2 - 0.04, MCC_center2 + 0.06, by = 0.01)), 'MCC_f', Hist)
print(as.data.frame(MCC_Stage3$PM), digits = 4)

# Stage 4: pooled 9-OM grid. A narrow bracket around Stage 3's
# boundary.
MCC_center3 <- Best_Compliant(MCC_Stage3$PM)$tunepar
rng <-  Narrow_Bracket(MCC_center3, 0.01)
rng <- data.frame(tunepar = seq(0.92, by = 0.01, length.out = 10))
MCC_Stage4 <- Round(MCC_HCR, rng, 'MCC_p', RefHists)
print(as.data.frame(MCC_Stage4$PM), digits = 4)

MCC_Final <- Spanning_Set(MCC_Stage4$PM)
MCC_Final <- data.frame(tunepar = c(0.95, 0.98))

print(MCC_Final)

Write_MP_Variants(generator_name= 'MCC_HCR',
                  variants= stats::setNames(lapply(MCC_Final$tunepar, \(x) list(tunepar = x)),
                   paste0('MCC_', round(100 * MCC_Final$tunepar))),
  file = 'analysis/04-Define-CMPs.R')
