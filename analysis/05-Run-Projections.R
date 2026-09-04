library(iccatSALB)
library(MSEtool)

source('analysis/00-Specifications.R')
source('analysis/04-Define-CMPs.R')

# ---- Families to run ----


Families <- data.frame(Family = names(MP_Groups),
                       Run = TRUE,
                       stringsAsFactors = FALSE)

# Set Run = FALSE to skip a family entirely, e.g.:
# Families$Run[Families$Family == 'MCC'] <- FALSE

# ---- OMs to run ----
OM_Sets_to_Run <- c('R1', 'R2')  # e.g.; NULL runs everything

Hist_files <- GetHistFiles('objects/Hist')
Hist_files$OM_Set <- dirname(sub('^objects/Hist/', '', Hist_files$File))

if (!is.null(OM_Sets_to_Run)) {
  Hist_files$Run <- Hist_files$OM_Name %in% OM_Sets_to_Run
}

# Set Run = FALSE to skip specific OMs on top of the above, e.g.:
# Hist_files$Run[Hist_files$Name == 'G_75-M_75.hist' & Hist_files$OM_Type == 'Robustness'] <- FALSE

Families$Run <- FALSE
Families$Run[2] <- TRUE

# ---- Run projections, one family x one OM at a time ----

for (fam_i in seq_len(nrow(Families))) {
  if (!Families$Run[fam_i]) next

  Family <- Families$Family[fam_i]
  MPs <- MP_Groups[[Family]]

  for (om_i in seq_len(nrow(Hist_files))) {
    if (!Hist_files$Run[om_i]) next

    OM_Set  <- Hist_files$OM_Set[om_i]
    hist_fl <- Hist_files$File[om_i]
    mse_fl  <- file.path('objects/MSE', OM_Set, Family,
                          sub('\\.hist$', '.mse', Hist_files$Name[om_i]))
    mse_dir <- dirname(mse_fl)
    if (!dir.exists(mse_dir)) dir.create(mse_dir, recursive = TRUE)

    cat(sprintf('---- %s | %s ----\n', OM_Set, Family))

    tryCatch({
      Hist <- readRDS(hist_fl)
      MSE  <- MSEtool::Project(Hist, MPs = MPs, parallel = FALSE, silent = FALSE)
      MSEtool::Save(MSE, mse_fl, overwrite = TRUE)
    }, error = function(e) {
      cat(sprintf('  FAILED (%s / %s): %s\n', OM_Set, Family, conditionMessage(e)))
    })
  }
}



