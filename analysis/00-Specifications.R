here::i_am('analysis/00-Specifications.R')

if (packageVersion('MSEtool') < '4.0.0') {
  cli::cli_abort(c(
    "MSEtool v4+ is required for this analysis.",
    "i" = "Install the development version with {.code pak::pak('blue-matter/MSEtool')}"
  ))
}

# ---- Reference Assessment ----
RefDir <- here::here('data-raw', 'assessment', 'S05')

# ---- InterimAdvice ----
SSData <- MSEtool::ImportSSData(RefDir, silent = TRUE)
Hist_Landings <- MSEtool::Landings(SSData) |> MSEtool::Value() |> utils::tail(4)


mu <- apply(Hist_Landings, 2, mean)
sd <- apply(Hist_Landings, 2, sd)

FleetNms <- colnames(Hist_Landings)
nFleet <- length(mu)

Years <- 2025:2027

InterimAdvice <- data.frame(Year = rep(Years, each = nFleet),
                            Stock = 'Albacore',
                            Fleet = rep(FleetNms, times = length(Years)),
                            Type = 'TAC',
                            Mean = rep(mu, times = length(Years)),
                            SD    = rep(sd, times = length(Years))
                            )

# ---- OM Specifications ----
OMSpecs <- list(
  nSim      = 100, # number of simulations
  pYear     = 33,  # number of projection years
  Interval  = 3,   # management interval
  Name      = 'Southern Atlantic Albacore',
  StockName = "Albacore",
  Species   = "Thunnus alalunga",
  Region    = 'South Atlantic',
  Agency    = 'ICCAT',
  DataLag   = 2,
  MPStartYear = 2028,
  InterimAdvice = InterimAdvice
)



# ---- OM Grid Factors and Levels ----

G_25 <- list(Linf = 115.93, K = 0.235, t0 = -0.561)
G_50 <- list(Linf = 121.24, K = 0.238, t0 = -0.891)
G_75 <- list(Linf = 125.02, K = 0.237, t0 = -1.217)

# M_25_vec <- c(0.777832, 0.559865, 0.459483, 0.402809, 0.367193, 0.343299, 0.326565,
#           0.314489, 0.305585, 0.298917, 0.293864, 0.29, 0.287026, 0.284725,
#           0.282937, 0.28023)
#
# M_50_vec <- c(0.965585, 0.695005, 0.570393, 0.500039, 0.455826, 0.426165, 0.405391,
#           0.3904, 0.379347, 0.371069, 0.364796, 0.36, 0.356308, 0.353451,
#           0.351232, 0.347871)
#
# M_75_vec <- c(1.18016, 0.849451, 0.697147, 0.611159, 0.55712, 0.520868, 0.495478,
#           0.477155, 0.463646, 0.453529, 0.445862, 0.44, 0.435488, 0.431996,
#           0.429283, 0.425176)

M_25 <- 0.29
M_50 <- 0.36
M_75 <- 0.44

# Helper functions

vonbert_L_at_Amin <- function(Linf, K, t0, Amin = 0) {
  Linf * (1 - exp(-K * (Amin - t0)))
}

