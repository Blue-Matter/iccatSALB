library(MSEtool)

source('analysis/00-Specifications.R')
source('analysis/04-Define-CMPs.R')

# ---- MPs to test ----
MPs <- c(
  'SP_FMSY',
  'SP_75FMSY',
  'IRatio',
  'ISlope',
  'CC24000',
  'CC28000',
  'MCC1',
  'MCC2'
)

# ---- Hist objects to project ----
hist_dirs <- c('objects/Hist', 'objects/Hist/Reference', 'objects/Hist/Robustness')
hist_dirs <- hist_dirs[dir.exists(hist_dirs)]

Hist_files <- do.call(rbind, lapply(hist_dirs, function(d) {
  fls <- list.files(d, pattern = '\\.hist$', full.names = FALSE)
  if (!length(fls)) return(NULL)
  data.frame(Dir = d, File = fls, Run = TRUE, stringsAsFactors = FALSE)
}))


# Set `Run` to FALSE to skip specific Hist objects, e.g.:
# Hist_files$Run[Hist_files$File == 'G_75-M_75.hist'] <- FALSE

# ---- Run projections ----
for (i in seq_len(nrow(Hist_files))) {
  if (!Hist_files$Run[i]) next

  hist_dir <- Hist_files$Dir[i]
  hist_fl  <- Hist_files$File[i]

  Hist <- readRDS(file.path(hist_dir, hist_fl))
  MSE  <- MSEtool::Project(Hist, MPs = MPs)

  mse_dir <- gsub('^objects/Hist', 'objects/MSE', hist_dir)
  if (!dir.exists(mse_dir))
    dir.create(mse_dir, recursive = TRUE)

  mse_fl <- gsub('\\.hist$', '.mse', hist_fl)
  MSEtool::Save(MSE, file.path(mse_dir, mse_fl), overwrite = TRUE)
}
