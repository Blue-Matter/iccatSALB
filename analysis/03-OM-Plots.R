
# Run inside TSD Operating Models chapter

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


Robustness_dirs <- list.files(here::here('objects', 'Hist', 'Robustness'), full.names = TRUE) |>
  purrr::set_names(basename)

RobustnessHistList <- purrr::map(Robustness_dirs, read_hist_dir)

AllHistList <- c(list(Reference = HistList), RobustnessHistList)

# ---- life-history ----

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

# ---- plot_index_fits ----
p1 <- PlotIndexFit(AllHistList$Reference$`G_50-M_50`, 'Survey',
                   nsim = 0, ribbon = TRUE,
                   Fleets = c('Indx_CTP-LL_TB2', 'Indx_BR_URY-LL', 'Indx_ZAF-BB')) +
  ggplot2::theme(axis.title.x = element_blank(),
                 axis.text.x = element_blank()) +
  ggplot2::labs(y = 'Index (relative True)')

p2 <- PlotIndexFit(AllHistList$R1$`G_50-M_50`, 'Survey', ribbon = TRUE, nsim = 0,
                   Fleets = c('Indx_CTP-LL_TB2', 'Indx_BR_URY-LL', 'Indx_ZAF-BB')) +
  ggplot2::theme(axis.title.x = element_blank(),
                 axis.text.x = element_blank()) +
  ggplot2::labs(y = 'Index (relative True)')

p3 <- PlotIndexFit(AllHistList$R2$`G_50-M_50`, 'Survey', ribbon = TRUE, nsim = 0,
                   Fleets = c('Indx_CTP-LL_TB2', 'Indx_BR_URY-LL', 'Indx_ZAF-BB')) +
  ggplot2::labs(y = 'Index (relative True)')

p4 <- patchwork::wrap_plots(p1, p2, p3, ncol = 1, guides = 'collect') +
  patchwork::plot_annotation(tag_levels = 'a',
                             tag_suffix = ')')


ggplot2::ggsave(file.path(here::here(), 'analysis/figures/index_fit.png'), p4,
                width = 8, height = 7)
print(p4)

# ---- stock_status_calc ----

result <- AllHistList |>
  purrr::imap(\(inner_list, level1_name) {
    inner_list |>
      purrr::imap(\(hist_obj, level2_name) {
        sb <- SB_SBMSY(hist_obj) |>
          dplyr::filter(Year == max(Year), Sim == 1) |>
          dplyr::rename(SB_SBMSY = Value)

        f <- F_FMSY(hist_obj) |>
          dplyr::filter(Year == max(Year), Sim == 1) |>
          dplyr::rename(F_FMSY = Value)

        dplyr::left_join(sb, f, by = "Year")
      }) |>
      purrr::list_rbind(names_to = "level2")
  }) |>
  purrr::list_rbind(names_to = "level1")

status_range <- function(set, col) {
  v <- result[[col]][result$level1 == set]
  round(range(v), 2)
}

# ---- summarize_stock_status ----

result_table <- result |>
  dplyr::select(level1, level2, SB_SBMSY, F_FMSY)

result_wide <- result_table |>
  tidyr::pivot_wider(
    id_cols = level2,
    names_from = level1,
    values_from = c(SB_SBMSY, F_FMSY),
    names_glue = "{level1}_{.value}"
  )

col_order <- c(
  "level2",
  "Reference_SB_SBMSY", "Reference_F_FMSY",
  "R1_SB_SBMSY", "R1_F_FMSY",
  "R2_SB_SBMSY", "R2_F_FMSY"
)

result_wide <- result_wide |>
  dplyr::select(dplyr::all_of(col_order))

ft <- result_wide |>
  flextable::flextable() |>
  flextable::set_header_labels(
    level2 = "Growth/Natural Mortality",
    Reference_SB_SBMSY = "SB/SBMSY", Reference_F_FMSY = "F/FMSY",
    R1_SB_SBMSY = "SB/SBMSY", R1_F_FMSY = "F/FMSY",
    R2_SB_SBMSY = "SB/SBMSY", R2_F_FMSY = "F/FMSY"
  ) |>
  flextable::add_header_row(
    values = c("", "Reference", "Reference", "R1", "R1", "R2", "R2"),
    top = TRUE
  ) |>
  flextable::merge_h(part = "header") |>
  flextable::colformat_double(j = -1, digits = 2) |>
  flextable::theme_vanilla() |>
  flextable::bold(part = "header") |>
  flextable::align(align = "center", part = "header") |>
  flextable::align(j = -1, align = "center", part = "body") |>
  flextable::vline(j = c(1, 3, 5), border = officer::fp_border(color = "gray70")) |>
  flextable::autofit()

ft

# ---- make_word_table ----
doc <- officer::read_docx() |>
  flextable::body_add_flextable(ft)

print(doc, target = "analysis/figures/SB_F_wide_table.docx")


# ---- depletion_msy_robustness ----

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
    ) |>
    dplyr::mutate(Model = factor(Model, levels=unique(Model), ordered = TRUE))
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
    theme(legend.position = 'bottom') +
    scale_color_manual(values = c('black', 'orange', 'darkred'))

}

SBSBMSY_Robustness <- extract_grid_metric(AllHistList, MSEtool::SB_SBMSY)

p_sbsbmsy_robustness <- plot_grid_metric(SBSBMSY_Robustness, ylab = expression(SB/SB[MSY]), hline = 1) +
  geom_hline(yintercept = 1, linetype = 2, color = 'darkgray') +
  geom_hline(yintercept = 0.4, linetype = 2, color = 'darkgray')

print(p_sbsbmsy_robustness)

# ---- f_msy_robustness ----

F_FMSY_Robustness <- extract_grid_metric(AllHistList, MSEtool::F_FMSY)

p_ffmsy_robustness <- plot_grid_metric(F_FMSY_Robustness, ylab = expression(F/F[MSY]), hline = 1)

print(p_ffmsy_robustness)


# ---- refpoints_calc ----

refpoints_result <- AllHistList |>
  purrr::imap(\(inner_list, level1_name) {
    inner_list |>
      purrr::imap(\(hist_obj, level2_name) {
        data.frame(
          FMSY = median(FMSY(hist_obj)),
          SBMSY = median(SBMSY(hist_obj)),
          MSY = median(MSYLandings(hist_obj))
        )
      }) |>
      purrr::list_rbind(names_to = "level2")
  }) |>
  purrr::list_rbind(names_to = "level1")

# ---- refpoints_table ----

refpoints_wide <- refpoints_result |>
  tidyr::pivot_wider(
    id_cols = level2,
    names_from = level1,
    values_from = c(FMSY, SBMSY, MSY),
    names_glue = "{level1}_{.value}"
  )

refpoints_col_order <- c(
  "level2",
  "Reference_FMSY", "Reference_SBMSY", "Reference_MSY",
  "R1_FMSY", "R1_SBMSY", "R1_MSY",
  "R2_FMSY", "R2_SBMSY", "R2_MSY"
)

refpoints_wide <- refpoints_wide |>
  dplyr::select(dplyr::all_of(refpoints_col_order))

ft_refpoints <- refpoints_wide |>
  flextable::flextable() |>
  flextable::set_header_labels(
    level2 = "Growth/Natural Mortality",
    Reference_FMSY = "FMSY", Reference_SBMSY = "SBMSY", Reference_MSY = "MSY",
    R1_FMSY = "FMSY", R1_SBMSY = "SBMSY", R1_MSY = "MSY",
    R2_FMSY = "FMSY", R2_SBMSY = "SBMSY", R2_MSY = "MSY"
  ) |>
  flextable::add_header_row(
    values = c("", "Reference", "Reference", "Reference",
               "R1", "R1", "R1", "R2", "R2", "R2"),
    top = TRUE
  ) |>
  flextable::merge_h(part = "header") |>
  flextable::colformat_double(j = c("Reference_FMSY", "R1_FMSY", "R2_FMSY"), digits = 2) |>
  flextable::colformat_double(
    j = c("Reference_SBMSY", "R1_SBMSY", "R2_SBMSY", "Reference_MSY", "R1_MSY", "R2_MSY"),
    digits = 0
  ) |>
  flextable::theme_vanilla() |>
  flextable::bold(part = "header") |>
  flextable::align(align = "center", part = "header") |>
  flextable::align(j = -1, align = "center", part = "body") |>
  flextable::vline(j = c(1, 4, 7), border = officer::fp_border(color = "gray70")) |>
  flextable::autofit()

ft_refpoints


# ---- make_word_ref_points_table ----

doc <- officer::read_docx() |>
  flextable::body_add_flextable(ft_refpoints)

print(doc, target = "analysis/figures/ref_points_wide_table.docx")


