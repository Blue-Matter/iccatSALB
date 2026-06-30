library(MSEtool)

source('analysis/00-Specifications.R')

OM_files <- data.frame(File     = list.files('objects/OM'),
                       Simulate = TRUE)

for (i in seq_len(nrow(OM_files))) {
  if (!OM_files[i,2]) next

  om_fl   <- OM_files[i,1]
  OM      <- readRDS(file.path('objects/OM', om_fl))
  Hist    <- Simulate(OM, silent = TRUE)
  hist_fl <- gsub('.om', '.hist', om_fl)
  Save(Hist, file.path('objects/Hist', hist_fl), overwrite = TRUE)

}


