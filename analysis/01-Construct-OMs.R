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

MSEtool::Save(RefOM, 'objects/OM/Ref.om', overwrite = TRUE)

# ---- Reference Grid (growth x M) ----

OM_grid <- iccatSALB::Build_OM_Grid(
  g_levels = c(25, 50, 75),
  m_levels = c(25, 50, 75),
  ref_dir  = RefDir,
  base_dir = "data-raw/assessment"
)

# growth/M scenario definitions are in 00-Specifications.R
growth_scenarios <- mget(unique(OM_grid$g_scen))
M_scenarios      <- mget(unique(OM_grid$m_scen))

iccatSALB::Prepare_OM_Grid(
  grid             = OM_grid,
  ref_dir          = RefDir,
  growth_scenarios = growth_scenarios,
  M_scenarios      = M_scenarios
)

Grid_OM_Specs <- list(
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

iccatSALB::Run_SS3_Grid(
  run_dirs     = OM_grid$run_dir[!OM_grid$is_ref],
  exe          = ss3_exe,
  extras       = "-nohess",
  skipfinished = FALSE
)


iccatSALB::Import_OM_Grid(
  grid     = OM_grid,
  out_dir  = "objects/OM/Reference",
  om_specs = Grid_OM_Specs
)

# ---- Robustness Models ----

# Each robustness scenario downweights one survey index to zero (lambda = 0)
# across the full growth x M grid, then re-runs and imports the resulting OMs.
Robustness_Scenarios <- c(
  R1 = 'Indx_BR_URY-LL',
  R2 = 'Indx_CTP-LL_TB2'
)

for (r_name in names(Robustness_Scenarios)) {

  R_Grid <- iccatSALB::Downweight_Index_Grid(
    grid         = OM_grid,
    index_name   = Robustness_Scenarios[[r_name]],
    out_base_dir = file.path('data-raw/robustness', r_name)
  )

  iccatSALB::Run_SS3_Grid(
    run_dirs     = R_Grid$run_dir,
    exe          = ss3_exe,
    extras       = "-nohess",
    skipfinished = FALSE
  )

  iccatSALB::Import_OM_Grid(
    grid     = R_Grid,
    out_dir  = file.path('objects/OM/Robustness', r_name),
    om_specs = Grid_OM_Specs
  )
}

MSEtool::DisableParallel()





