library(MSEtool)

source('analysis/00-Specifications.R')

simulate_dir <- function(om_dir, hist_dir) {
  if (!dir.exists(om_dir)) return(invisible())

  om_files <- list.files(om_dir, pattern = '\\.om$')
  if (!length(om_files)) return(invisible())

  if (!dir.exists(hist_dir))
    dir.create(hist_dir, recursive = TRUE)

  for (om_fl in om_files) {
    OM      <- readRDS(file.path(om_dir, om_fl))
    Hist    <- MSEtool::Simulate(OM, silent = TRUE)
    hist_fl <- gsub('\\.om$', '.hist', om_fl)
    MSEtool::Save(Hist, file.path(hist_dir, hist_fl), overwrite = TRUE)
  }
}

# ---- Base reference model (objects/OM/Ref.om) ----
simulate_dir('objects/OM', 'objects/Hist')

# ---- Reference OM grid ----
simulate_dir('objects/OM/Reference', 'objects/Hist/Reference')

# ---- Robustness OMs ----
simulate_dir('objects/OM/Robustness', 'objects/Hist/Robustness')

