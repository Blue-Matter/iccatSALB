library(MSEtool)

source('analysis/00-Specifications.R')

# ---- Reference Model ----

RefOM <- ImportSS(SSDir      = RefDir,
                  Name       = OMSpecs$Name,
                  nSim       = OMSpecs$nSim,
                  pYear      = OMSpecs$pYear,
                  Agency     = OMSpecs$Agency,
                  StockName  = OMSpecs$StockName,
                  CommonName = OMSpecs$StockName,
                  Interval   = OMSpecs$Interval,
                  DataLag    = OMSpecs$DataLag
                  )

Save(RefOM, 'objects/OM/Ref.om')


# ---- Reference Grid ----
source('analysis/00-Specifications.R')

RefOM <- readRDS('objects/OM/Ref.om')

OM_grid <- expand.grid(g_scen = paste0('G_', c(25, 50, 75)),
                        m_scen = paste0('M_', c(25, 50, 75)),
                        stringsAsFactors = FALSE)

OM_grid$om_name <- paste(OM_grid$g_scen, OM_grid$m_scen, sep = '-')
OM_grid$is_ref  <- OM_grid$g_scen == 'G_50' & OM_grid$m_scen == 'M_50'
OM_grid$run_dir <- ifelse(OM_grid$is_ref,
                           RefDir,
                           file.path('data-raw/assessment', OM_grid$om_name))

## ---- Prepare SS3 run directories (copy inputs + modify parameters) ----
for (i in seq_len(nrow(OM_grid))) {

  if (OM_grid$is_ref[i]) next

  g_scen  <- OM_grid$g_scen[i]
  m_scen  <- OM_grid$m_scen[i]
  run_dir <- OM_grid$run_dir[i]

  growth <- get(g_scen)
  m      <- get(m_scen)

  if (!dir.exists(run_dir))
    dir.create(run_dir)

  # copy SS3 files
  r4ss::copy_SS_inputs(dir.old   = RefDir,
                       dir.new   = run_dir,
                       copy_exe  = TRUE,
                       copy_par  = TRUE,
                       overwrite = TRUE,
                       verbose   = FALSE)

  # modify growth and M
  inputs <- r4ss::SS_read(dir = run_dir)

  # growth
  inputs$ctl$MG_parms['L_at_Amin_Fem_GP_1', 'INIT'] <- vonbert_L_at_Amin(growth$Linf, growth$K, growth$t0)
  inputs$ctl$MG_parms['L_at_Amax_Fem_GP_1', 'INIT'] <- growth$Linf
  inputs$ctl$MG_parms['VonBert_K_Fem_GP_1', 'INIT']  <- growth$K

  # M (Lorenzen reference scalar)
  inputs$ctl$MG_parms['NatM_p_1_Fem_GP_1', 'INIT'] <- m

  # write modified inputs back to run_dir
  r4ss::SS_write(inputs, dir = run_dir, overwrite = TRUE)
}

## ---- Run SS3 in parallel ----
run_dirs <- OM_grid$run_dir[!OM_grid$is_ref]

library(furrr)
future::plan(future::multisession, workers = max(1, nrow(OM_grid) - 1))

furrr::future_walk(run_dirs, function(d) {
  r4ss::run(dir = d, exe = 'ss3', skipfinished = FALSE, extras = '-nohess')
})

future::plan(future::sequential)

## ---- Import OMs ----
for (i in seq_len(nrow(OM_grid))) {

  run_dir <- OM_grid$run_dir[i]
  om_name <- OM_grid$om_name[i]

  OM <- ImportSS(SSDir      = run_dir,
                 Name       = OMSpecs$Name,
                 nSim       = OMSpecs$nSim,
                 pYear      = OMSpecs$pYear,
                 Agency     = OMSpecs$Agency,
                 StockName  = OMSpecs$StockName,
                 CommonName = OMSpecs$StockName,
                 Interval   = OMSpecs$Interval,
                 DataLag    = OMSpecs$DataLag
  )

  Save(OM, file.path('objects/OM', paste0(om_name, '.om')))
}


# ---- Robustness Models ----



