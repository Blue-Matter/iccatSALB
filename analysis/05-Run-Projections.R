library(MSEtool)

source('analysis/00-Specifications.R')
source('analysis/04-Define-CMPs.R')

# ---- MPs to test ----
MPs <- c(
  'SP_FMSY',
  'SP_75FMSY',
  'IRatio',
  'ISlope',
  'MCC1',
  'MCC2'
)


# TODO -
# - MP development and testing !
# - try Spict and a few model free approaches - adust MCC methods and use ITarget approaches
# - challenging because of lack of contrast in indices
# - update TSD

# ---- Hist objects to project ----

Hist_files <- list.files('objects/Hist', pattern = '\\.hist$', recursive = TRUE)
Hist_files <- data.frame(File = Hist_files, Run = TRUE, stringsAsFactors = FALSE)

# Set `Run` to FALSE to skip specific Hist objects, e.g.:
# Hist_files$Run[Hist_files$File == 'Reference/G_75-M_75.hist'] <- FALSE


## ---- TESTING ----

hist_fl <- Hist_files$File[1]
Hist <- readRDS(file.path('objects/Hist', hist_fl))
SetupParallel()
MSE  <- MSEtool::Project(Hist, MPs = MPs, parallel = TRUE)
DisableParallel()

Log(MSE)

Calc_PMs(MSE)

MSYLandings(MSE)


DataList <- PPD(MSE)
Data <- DataList$SP_FMSY$`4`$Albacore |> DataTrim(2025)
SP_FMSY(Data)
LastTAC(Data)

## ---- END TESTING ----


# ---- Run projections ----
for (i in seq_len(nrow(Hist_files))) {
  if (!Hist_files$Run[i]) next

  hist_fl <- Hist_files$File[i]

  Hist <- readRDS(file.path('objects/Hist', hist_fl))

  MSEtool::SetupParallel()
  MSE  <- MSEtool::Project(Hist, MPs = MPs, parallel = TRUE)
  MSEtool::DisableParallel()

  mse_fl  <- gsub('\\.hist$', '.mse', hist_fl)
  mse_dir <- file.path('objects/MSE', dirname(mse_fl))
  if (!dir.exists(mse_dir))
    dir.create(mse_dir, recursive = TRUE)

  MSEtool::Save(MSE, file.path('objects/MSE', mse_fl), overwrite = TRUE)
}


