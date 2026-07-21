

# ---- load_files ----
library(MSEtool)
library(ggplot2)
here::i_am('analysis/03-OM-Plots.R')

source(here::here('analysis', '00-Specifications.R'))

Hist_files <- list.files(here::here('objects', 'Hist', 'Reference'), pattern = '\\.hist$', full.names = TRUE)

OM_names   <- tools::file_path_sans_ext(basename(Hist_files))

HistList <- purrr::map(Hist_files, readRDS) |>
  purrr::set_names(OM_names)


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
  theme(legend.position = 'bottom')

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
