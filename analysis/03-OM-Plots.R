

# ---- load_files ----
library(MSEtool)
library(ggplot2)
here::i_am('analysis/03-OM-Plots.R')

source(here::here('analysis', '00-Specifications.R'))

read_hist_dir <- function(dir) {
  Hist_files <- list.files(dir, pattern = '\\.hist$', full.names = TRUE)
  purrr::map(Hist_files, readRDS) |>
    purrr::set_names(tools::file_path_sans_ext(basename(Hist_files)))
}

HistList <- read_hist_dir(here::here('objects', 'Hist', 'Reference'))
OM_names  <- names(HistList)

# ---- load_robustness_files ----

Robustness_dirs <- list.files(here::here('objects', 'Hist', 'Robustness'), full.names = TRUE) |>
  purrr::set_names(basename)

RobustnessHistList <- purrr::map(Robustness_dirs, read_hist_dir)

AllHistList <- c(list(Reference = HistList), RobustnessHistList)

# ---- lifehistory ----

LifeHistory <- purrr::imap_dfr(HistList, function(Hist, om_name) {
  Stock <- Hist@OM@Stock[[OMSpecs$StockName]]

  Length_df <- Array2DF(Stock@Length@MeanAtAge[1, , 1, drop = FALSE]) |>
    dplyr::mutate(Variable = 'Length-at-age')

  NatMort_df <- Array2DF(Stock@NaturalMortality@MeanAtAge[1, , 1, drop = FALSE]) |>
    dplyr::mutate(Variable = 'M-at-age')

  dplyr::bind_rows(Length_df, NatMort_df) |>
    dplyr::transmute(OM = om_name, Age, Value, Variable)
})

p_lifehistory <- ggplot(LifeHistory, aes(x = Age, y = Value, colour = OM)) +
  geom_line() +
  facet_wrap(~ Variable, scales = 'free_y') +
  labs(x = 'Age', y = NULL, colour = 'OM') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  expand_limits(y=0)

print(p_lifehistory)

# ---- ssb ----

SB <- purrr::imap_dfr(HistList, function(Hist, om_name) {
  SBiomass(Hist, df = TRUE) |>
    dplyr::mutate(OM = om_name)
})

SB <- SB |>
  dplyr::mutate(
    Growth = paste('Growth:', gsub('G_(\\d+)-M_\\d+', '\\1', OM)),
    NatMort = paste('M:', gsub('G_\\d+-M_(\\d+)', '\\1', OM))
  )

p_sb <- ggplot(SB, aes(x = Year, y = Value, colour = OM)) +
  stat_summary(fun = median, geom = 'line') +
  facet_grid(NatMort ~ Growth) +
  labs(x = 'Year', y = 'SB') +
  expand_limits(y = c(0, 1)) +
  theme_bw() +
  theme(legend.position = 'none')

print(p_sb)

# ---- depletion ----

SBSB0 <- purrr::imap_dfr(HistList, function(Hist, om_name) {
  SB_SB0(Hist, df = TRUE) |>
    dplyr::mutate(OM = om_name)
})


p_sbsb0 <- ggplot(SBSB0, aes(x = Year, y = Value, colour = OM)) +
  stat_summary(fun = median, geom = 'line') +
  labs(x = 'Year', y = expression(SB/SB[0]), colour = 'OM') +
  expand_limits(y = c(0, 1)) +
  theme_bw() +
  theme(legend.position = 'bottom')

print(p_sbsb0)

# ---- depletion_msy ----

SBSB_MSY <- purrr::imap_dfr(HistList, function(Hist, om_name) {
  SB_SBMSY(Hist, df = TRUE) |>
    dplyr::mutate(OM = om_name)
})

p_sbsbmsy <- ggplot(SBSB_MSY, aes(x = Year, y = Value, colour = OM)) +
  geom_hline(yintercept = 1, linetype = 2) +
  stat_summary(fun = median, geom = 'line') +
  labs(x = 'Year', y = expression(SB/SB[MSY]), colour = 'OM') +
  expand_limits(y = c(0, 1)) +
  theme_bw() +
  theme(legend.position = 'bottom')

print(p_sbsbmsy)

# ---- F_msy ----

F_FMSY <- purrr::imap_dfr(HistList, function(Hist, om_name) {
  F_FMSY(Hist, df = TRUE) |>
    dplyr::mutate(OM = om_name)
})

p_f_fmsy <- ggplot(F_FMSY, aes(x = Year, y = Value, colour = OM)) +
  geom_hline(yintercept = 1, linetype = 2) +
  stat_summary(fun = median, geom = 'line') +
  labs(x = 'Year', y = expression(F/F[MSY]), colour = 'OM') +
  expand_limits(y = c(0, 1)) +
  theme_bw() +
  theme(legend.position = 'bottom')

print(p_f_fmsy)

# ---- refpoints_table ----

TerminalF_FMSY <- F_FMSY |>
  dplyr::group_by(OM) |>
  dplyr::filter(Year == max(Year)) |>
  dplyr::summarise(F_FMSY = median(Value), .groups = 'drop')

TerminalSB_SBMSY <- SBSB_MSY |>
  dplyr::group_by(OM) |>
  dplyr::filter(Year == max(Year)) |>
  dplyr::summarise(SB_SBMSY = median(Value), .groups = 'drop')

RefPoints <- purrr::imap_dfr(HistList, function(Hist, om_name) {
  data.frame(
    OM = om_name,
    FMSY = median(FMSY(Hist)),
    SBMSY = median(SBMSY(Hist)),
    SPRMSY = median(SPRMSY(Hist)),
    MSY = median(MSYLandings(Hist))
  )
}) |>
  dplyr::left_join(TerminalF_FMSY, by = 'OM') |>
  dplyr::left_join(TerminalSB_SBMSY, by = 'OM')

knitr::kable(
  RefPoints,
  digits = 3,
  col.names = c(
    'OM',
    'F<sub>MSY</sub>',
    'SB<sub>MSY</sub>',
    'SPR<sub>MSY</sub>',
    'MSY',
    'F/F<sub>MSY</sub>',
    'SB/SB<sub>MSY</sub>'
  ),
  escape = FALSE
)

# ---- Robustness Comparison ----

extract_grid_metric <- function(HistLists, metric_fun) {
  purrr::imap_dfr(HistLists, function(HistList, model_name) {
    purrr::imap_dfr(HistList, function(Hist, om_name) {
      metric_fun(Hist, df = TRUE) |>
        dplyr::mutate(OM = om_name, Model = model_name)
    })
  }) |>
    dplyr::mutate(
      Growth  = paste('Growth:', gsub('G_(\\d+)-M_\\d+', '\\1', OM)),
      NatMort = paste('M:', gsub('G_\\d+-M_(\\d+)', '\\1', OM))
    )
}

plot_grid_metric <- function(data, ylab, hline = NA) {
  p <- ggplot(data, aes(x = Year, y = Value, colour = Model))

  if (!is.na(hline)) p <- p + geom_hline(yintercept = hline, linetype = 2)

  p +
    stat_summary(fun = median, geom = 'line') +
    facet_grid(NatMort ~ Growth) +
    labs(x = 'Year', y = ylab, colour = 'Model') +
    expand_limits(y = c(0, 1)) +
    theme_bw() +
    theme(legend.position = 'bottom')
}

# ---- sb_robustness ----

SB_Robustness <- extract_grid_metric(AllHistList, MSEtool::SBiomass)

p_sb_robustness <- plot_grid_metric(SB_Robustness, ylab = 'SB')

print(p_sb_robustness)

# ---- depletion_robustness ----

SBSB0_Robustness <- extract_grid_metric(AllHistList, MSEtool::SB_SB0)

p_sbsb0_robustness <- plot_grid_metric(SBSB0_Robustness, ylab = expression(SB/SB[0]))

print(p_sbsb0_robustness)

# ---- depletion_msy_robustness ----

SBSBMSY_Robustness <- extract_grid_metric(AllHistList, MSEtool::SB_SBMSY)

p_sbsbmsy_robustness <- plot_grid_metric(SBSBMSY_Robustness, ylab = expression(SB/SB[MSY]), hline = 1)

print(p_sbsbmsy_robustness)

# ---- f_msy_robustness ----

F_FMSY_Robustness <- extract_grid_metric(AllHistList, MSEtool::F_FMSY)

p_ffmsy_robustness <- plot_grid_metric(F_FMSY_Robustness, ylab = expression(F/F[MSY]), hline = 1)

print(p_ffmsy_robustness)

