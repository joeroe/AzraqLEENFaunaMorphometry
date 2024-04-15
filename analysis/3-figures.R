
# FIGURES AND TABLES FOR MARTIN ET AL. PAPER ----------------------------------

library("patchwork")
library("directlabels")
library("gt")

# Figure sizing for Elsevier journals
# See https://www.elsevier.com/authors/policies-and-guidelines/artwork-and-media-instructions/artwork-sizing
w1col <- 90
w1.5col <- 140
w2col <- 190

# Figure 1: Map of the southern Levant, showing sites used in the analysis.
# (Generated in QGIS)

# Figure 2: Radiocarbon chronology of sites used in the analysis
fig2 <- plot_chronology(sites, radiocarbon_sum, cultural_periods)
ggsave("analysis/figures/fig2.pdf", fig2, device = cairo_pdf,
       width = w2col, height = w2col * 2/3, units = "mm")
ggsave("analysis/figures/fig2.png", fig2,
       width = w2col, height = w2col * 2/3, units = "mm")

# Figure 3: Relative body size of gazelle through time, using all elements and
#   measurements.
fig3 <- plot_mcts(gazella_sim, Z, climate_periods, "Relative body size (Z)",
                  consolidate = TRUE)
ggsave("analysis/figures/fig3.pdf", fig3, device = cairo_pdf,
        width = w2col, height = w2col * 1/3, units = "mm")
ggsave("analysis/figures/fig3.png", fig3,
        width = w2col, height = w2col * 1/3, units = "mm")

# Figure 4: Relative body size of gazelle through time, using specific element
#   measurements: Astragalus GLl and Bd
fig4a <- plot_mcts(filter(gazella_sim, Element == "Astragalus"), GLI,
                   climate_periods, "GLl", consolidate = TRUE)

fig4b <- plot_mcts(filter(gazella_sim, Element == "Astragalus"), Bd,
                   climate_periods, "Bd", consolidate = TRUE)

fig4 <- fig4a / fig4b
ggsave("analysis/figures/fig4.pdf", fig4, device = cairo_pdf,
       width = w2col, height = w2col * 2/3, units = "mm")
ggsave("analysis/figures/fig4.png", fig4,
       width = w2col, height = w2col * 2/3, units = "mm")

# Figure 5: Distributions of relative body size at all sites, arranged by date.
#   Only sites with N > 25 are included.
gazella %>%
  mutate(
    Period = case_match(
      SiteCode,
      sites_early_epipal ~ "Early Epipalaeolithic",
      sites_late_epipal ~ "Late Epipalaeolithic",
      sites_early_neo ~ "Early Neolithic",
      sites_late_neo ~ "Late Neolithic"
    ),
    Period = factor(Period, rev(c("Early Epipalaeolithic", "Late Epipalaeolithic",
                                  "Early Neolithic", "Late Neolithic")))
  ) %>%
  left_join(sites, by = "Site") %>%
  add_count(Site) %>%
  filter(n > 25) %>%
  ggplot(aes(x = Z, y = factor(Site, rev(sites$Site)))) +
  facet_grid(rows = vars(Period), scales = "free_y", space = "free_y") +
  ggridges::geom_density_ridges(panel_scaling = FALSE, quantile_lines = TRUE, quantiles = 2) +
  scale_x_continuous(limits = c(-3, 3)) +
  labs(x = "Relative body size (Z)", y = NULL) +
  theme_minlines() +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing = unit(0, "mm"),
    panel.border = element_blank()
  ) ->
  fig5

ggsave("analysis/figures/fig5.pdf", fig5, device = cairo_pdf,
       width = w1col, height = w1col * 1.5, units = "mm")
ggsave("analysis/figures/fig5.png", fig5,
       width = w1col, height = w1col * 1.5, units = "mm")

# Figure 6: scatterplot of gazelle scapula BG vs. GLP from KHIV (all phases)
fig6 <- gazella |>
  filter(str_starts(Site, "Kharaneh IV")) |>
  ggplot(aes(GLP, BG)) +
  geom_point() +
  labs(x = "GLP (mm)", y = "BG (mm)") +
  theme_minlines() +
  theme(panel.grid.major.x = element_blank())

ggsave("analysis/figures/fig6.pdf", fig6, device = cairo_pdf,
       width = w1col, height = w1col, units = "mm")
ggsave("analysis/figures/fig6.png",
       width = w1col, height = w1col, units = "mm")

# Figure 7: Relative body size of Lepus through time, using all elements and
#   measurements.
fig7 <- plot_mcts(lepus_sim, Z, climate_periods, "Relative body size (Z)",
                  consolidate = TRUE)
ggsave("analysis/figures/fig7.pdf", fig7, device = cairo_pdf,
       width = w2col, height = w2col * 1/3, units = "mm")
ggsave("analysis/figures/fig7.png", fig7,
       width = w2col, height = w2col * 1/3, units = "mm")

# Figure 8: z score hare elements
lepus_all <- lepus_all |>
  mutate(
    Period = case_match(
      SiteCode,
      sites_early_epipal ~ "Early Epipalaeolithic",
      sites_late_epipal ~ "Late Epipalaeolithic",
      sites_early_neo ~ "Early Neolithic",
      sites_late_neo ~ "Late Neolithic"
    ),
    Period = factor(Period, c("Early Epipalaeolithic", "Late Epipalaeolithic",
                              "Early Neolithic", "Late Neolithic"))
  )

fig8a <- lepus_all |>
  filter(Element == "Calcaneum") |>
  drop_na(GL, GB) |>
  mutate(GL = as.numeric(GL)) |>
  ggplot(aes(GL, GB, colour = Period)) +
  geom_point() +
  scale_colour_brewer(palette = "PuOr") +
  labs(x = "GL (mm)", y = "GB (mm)") +
  theme_minlines() +
  theme(panel.grid.major.x = element_blank(), legend.position = "bottom")

# TODO: problem - doesn't match draft! Possibly because we've since removed SHUB6??
fig8b <- lepus_all |>
  filter(Element == "Humerus") |>
  ggplot(aes(Bd, fill = Period)) +
  geom_histogram(binwidth = 0.3) +
  scale_fill_brewer(palette = "PuOr") +
  labs(x = "Bd (mm)") +
  theme_minlines() +
  theme(panel.grid.major.x = element_blank(), legend.position = "bottom")

fig8 <- fig8a / fig8b
ggsave("analysis/figures/fig8.pdf", fig8, device = cairo_pdf,
       width = w1col, height = w1col * 2, units = "mm")
ggsave("analysis/figures/fig8.png", fig8,
       width = w1col, height = w1col * 2, units = "mm")


# Figure 9: z score vulpes histogram
vulpes |>
  mutate(
    Period = case_match(
      SiteCode,
      sites_early_epipal ~ "Early Epipalaeolithic",
      sites_late_epipal ~ "Late Epipalaeolithic",
      sites_early_neo ~ "Early Neolithic",
      sites_late_neo ~ "Late Neolithic"
    ),
    Period = factor(Period, c("Early Epipalaeolithic", "Late Epipalaeolithic",
                              "Early Neolithic", "Late Neolithic"))
  ) |>
  ggplot(aes(Z, fill = Period)) +
  geom_histogram(binwidth = 0.3) +
  scale_fill_brewer(palette = "PuOr") +
  labs(x = "Relative body size (Z)", y = "N") +
  theme_minlines() +
  theme(panel.grid.major.x = element_blank(), legend.position = "bottom") ->
  fig9

ggsave("analysis/figures/fig9.pdf", fig9, device = cairo_pdf,
       width = w1col, height = w1col, units = "mm")
ggsave("analysis/figures/fig9.png", fig9,
       width = w1col, height = w1col, units = "mm")

# Figure 10: maps of modern biogeography
fig10 <- ggplot() +
  geom_sf(data = ne_afroeurasia, fill = "white") +
  geom_sf(data = iucn_ranges, mapping = aes(fill = binomial), alpha = 0.7, colour = NA) +
  scale_fill_manual(values = rep(c("#e41a1c", "#377eb8"), 3), name = NULL) +
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 30)) +
  scale_y_continuous(breaks = seq(from = 80, to = -80, by = -30)) +
  # coord_sf() +
  facet_wrap(vars(genus), nrow = 1) +
  theme_minlines() +
  theme(strip.text = element_text(face = "italic"),
        legend.text = element_text(face = "italic"),
        legend.position = "bottom",
        panel.grid.major = element_line(colour = "grey"),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("analysis/figures/fig10.pdf", fig10, device = cairo_pdf,
       width = w1.5col, height = w1.5col * 2/3, units = "mm")
ggsave("analysis/figures/fig10.png", fig10,
       width = w1.5col, height = w1.5col * 2/3, units = "mm")


# Table 1: Sample sizes used in metric analysis, by taxon and period
bind_rows(Gazella = gazella, Lepus = lepus, Vulpes = vulpes, .id = "Taxon") %>%
  mutate(Period = recode(SiteCode,
                         "DHW_LN" = "Late Neo",
                         "JN" = "Late Neo",
                         "BQ27" = "Late Neo",
                         "WJ13" = "Late Neo",
                         "WJ25" = "Late Neo",
                         "DHW_PPNB" = "PPNB",
                         "WJ7" = "PPNB",
                         "SHUB1_Final" = "Late Epipal",
                         "SHUB1_Late" = "Late Epipal",
                         "AZ18" = "Late Epipal",
                         "WJ22_Upper" = "Late Epipal",
                         "SHUB1_Early" = "Late Epipal",
                         "WJ22_Middle" = "Middle Epipal",
                         "AZ17" = "Middle Epipal",
                         "WJ8" = "Middle Epipal",
                         "WJ22_Lower" = "Middle Epipal",
                         "KHIV_D" = "Early Epipal",
                         "KHIV_B" = "Early Epipal",
                         "AQD" = "Early Epipal",
                         "WJ6_Upper" = "Early Epipal",
                         "KHIV_A" = "Early Epipal",
                         "AQB" = "Early Epipal",
                         "AQA" = "Early Epipal",
                         "UW18_Upper" = "Initial Epipal",
                         "KHIV" = "Early Epipal")) %>%
  group_by(Period, Taxon) %>%
  summarise(N = n()) %>%
  pivot_wider(names_from = Taxon, values_from = N) %>%
  ungroup() %>%
  left_join(cultural_periods, by = c("Period" = "period")) %>%
  transmute(Period, Age_cal_BP = paste(start, end, sep = "–"), Gazella, Lepus, Vulpes) %>%
  mutate(Period = fct_relevel(Period,
                              c("Late Neo",
                                "PPNB",
                                "Late Epipal",
                                "Middle Epipal",
                                "Early Epipal",
                                "Initial Epipal"))) %>%
  mutate(Period = fct_relabel(Period, ~gsub("Neo", "Neolithic", .x)),
         Period = fct_relabel(Period, ~gsub("Epipal", "Epipalaeolithic", .x))) %>%
  arrange(Period) %>%
  mutate(Gazella = replace_na(Gazella, 0),
         Lepus = replace_na(Lepus, 0),
         Vulpes = replace_na(Vulpes, 0)) %>%
  gt() %>%
  cols_label(Age_cal_BP = "Age cal BP") %>%
  cols_align("left", vars(Period, Age_cal_BP)) ->
  table1
gtsave(table1, "table1.html", "analysis/figures")
