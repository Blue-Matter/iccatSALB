library(iccatSALB)

source("analysis/00-Specifications.R")

ss3_exe <- normalizePath("ss3.exe", mustWork = TRUE)


# ---- Reference Model ----

RefOM <- MSEtool::ImportSS(
  SSDir         = RefDir,
  Name          = OMSpecs$Name,
  nSim          = OMSpecs$nSim,
  pYear         = OMSpecs$pYear,
  Agency        = OMSpecs$Agency,
  StockName     = OMSpecs$StockName,
  CommonName    = OMSpecs$StockName,
  Interval      = OMSpecs$Interval,
  DataLag       = OMSpecs$DataLag,
  MPStartYear   = OMSpecs$MPStartYear,
  InterimAdvice = OMSpecs$InterimAdvice
)

# ---- Reference Grid (growth x M) ----

OM_grid <- Build_OM_Grid(
  g_levels = c(25, 50, 75),
  m_levels = c(25, 50, 75),
  ref_dir  = RefDir,
  base_dir = "data-raw/assessment"
)

# growth/M scenario definitions are in 00-Specifications.R
growth_scenarios <- mget(unique(OM_grid$g_scen))
M_scenarios      <- mget(unique(OM_grid$m_scen))

Prepare_OM_Grid(
  grid             = OM_grid,
  ref_dir          = RefDir,
  growth_scenarios = growth_scenarios,
  M_scenarios      = M_scenarios
)

Run_SS3_Grid(
  run_dirs     = OM_grid$run_dir[!OM_grid$is_ref],
  exe          = ss3_exe,
  extras       = "-nohess",
  skipfinished = FALSE
)


Grid_OM_Specs <- list(
  Name       = OMSpecs$Name,
  nSim       = OMSpecs$nSim,
  pYear      = OMSpecs$pYear,
  Agency     = OMSpecs$Agency,
  StockName  = OMSpecs$StockName,
  CommonName = OMSpecs$StockName,
  Interval   = OMSpecs$Interval,
  DataLag    = OMSpecs$DataLag
)


Import_OM_Grid(
  grid     = OM_grid,
  out_dir  = "objects/OM/Reference",
  om_specs = Grid_OM_Specs
)




Hist <- readRDS('objects/Hist/Reference/001.hist')

# ---- Robustness Models ----

# TODO: add beta estimation/implementation in MSEtool

## ---- R1: Down-weight Surv_Indx_BR_URY-LL_Phz1 Index ----

drop_fleet <- 'Surv_Indx_BR_URY-LL_Phz1'

R1_Grid <- OM_grid

R1_Grid$run_dir <- file.path('data-raw/robustness/R1', basename(R1_Grid$run_dir ))
R1_Grid$is_ref <- NULL

for (i in seq_len(nrow(R1_Grid))) {

  # copy already modified files over
  fls <- list.files(OM_grid$run_dir[i])
  if (!dir.exists(R1_Grid$run_dir[i]))
    dir.create(R1_Grid$run_dir[i], recursive = TRUE)

  file.copy(file.path(OM_grid$run_dir[i], fls),
            file.path(R1_Grid$run_dir[i], fls),
            )


  ctl <- r4ss::SS_readctl(file = file.path(R1_Grid$run_dir[i],"control.ss_new"),
                          datlist = r4ss::SS_readdat(file.path(R1_Grid$run_dir[i],"data.ss_new")))

  fleet_ind <- which(rownames(ctl$lambdas) == drop_fleet)

  ctl$lambdas$value <- 0

  r4ss::SS_writectl(ctl, file.path(R1_Grid$run_dir[i],"control.ss_new"), overwrite = TRUE)

}

# run SS for robustness OM



## ---- R2: Down-weight Surv_Indx_CTP-LL_TB2_Phz1 Index ----

'Surv_Indx_CTP-LL_TB2_Phz1'

# write files

# run SS for robustness OM



MSEtool::DisableParallel()





