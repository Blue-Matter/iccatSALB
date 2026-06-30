

# ---- load_files ----
library(MSEtool)
library(ggplot2)
here::i_am('analysis/21-OM-Plots.R')

source(here::here('analysis', '00-Specifications.R'))

Hist_files <- list.files(here::here('objects', 'Hist'), pattern = '\\.hist$', full.names = TRUE)
Hist_files <- Hist_files[1:9]
# TODO: move to Reference directory

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
  labs(x = 'Age', y = NULL, colour = 'Operating Model') +
  theme_bw()

print(p_lifehistory)

# ---- depletion ----

SBSB0 <- purrr::imap_dfr(HistList, function(Hist, om_name) {
  SB_SB0(Hist, df = TRUE) |>
    dplyr::mutate(OM = om_name)
})

p_sbsb0 <- ggplot(SBSB0, aes(x = Year, y = Value, colour = OM)) +
  stat_summary(fun = median, geom = 'line') +
  labs(x = 'Year', y = expression(SB/SB[0]), colour = 'Operating Model') +
  expand_limits(y = c(0, 1)) +
  theme_bw()

print(p_sbsb0)

# ---- depletion_msy ----

SBSB_MSY <- purrr::imap_dfr(HistList, function(Hist, om_name) {
  SB_SBMSY(Hist, df = TRUE) |>
    dplyr::mutate(OM = om_name)
})

p_sbsbmsy <- ggplot(SBSB_MSY, aes(x = Year, y = Value, colour = OM)) +
  stat_summary(fun = median, geom = 'line') +
  labs(x = 'Year', y = expression(SB/SB[MSY]), colour = 'Operating Model') +
  expand_limits(y = c(0, 1)) +
  theme_bw()

print(p_sbsbmsy)

