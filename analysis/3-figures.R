
# FIGURES AND TABLES FOR MARTIN ET AL. PAPER ----------------------------------

library("patchwork")
library("directlabels")
library("gt")

# Figure sizing for Elsevier journals
# See https://www.elsevier.com/authors/policies-and-guidelines/artwork-and-media-instructions/artwork-sizing
w1col <- 90
w1.5col <- 140
w2col <- 190

# Figure A: Map of the southern Levant, showing sites used in the analysis.
# (Generated in QGIS)

# Figure B: Radiocarbon chronology of sites used in the analysis
figB <- plot_chronology(sites, radiocarbon_sum, cultural_periods)
ggsave("analysis/figures/figB.pdf", figB, device = cairo_pdf,
       width = w2col, height = w2col * 2/3, units = "mm")

# Figure C: Relative body size of gazelle through time, using all elements and
#   measurements.
figC <- plot_mcts(gazella_sim, Z, climate_periods, "Relative body size (Z)",
                  consolidate = TRUE)
ggsave("analysis/figures/figC.pdf", figC, device = cairo_pdf,
        width = w2col, height = w2col * 1/3, units = "mm")

# Figure D: Relative body size of gazelle through time, using specific element
#   measurements: Astragalus GLl and Bd
figD1 <- plot_mcts(filter(gazella_sim, Element == "Astragalus"), GLI,
                   climate_periods, "GLl", consolidate = TRUE)

figD2 <- plot_mcts(filter(gazella_sim, Element == "Astragalus"), Bd,
                   climate_periods, "Bd", consolidate = TRUE)

figD <- figD1 / figD2
ggsave("analysis/figures/figD.pdf", figD, device = cairo_pdf,
       width = w2col, height = w2col * 2/3, units = "mm")

# Figure E: Distributions of relative body size at all sites, arranged by date.
#   Only sites with N > 25 are included.
# TODO: Fix order!
gazella %>%
  mutate(Site = as_factor(Site)) %>%
  group_by(Site) %>%
  mutate(N = n(),
         midZ = median(Z, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Site = fct_relevel(Site, rev(sites$Site))) %>%
  # mutate(Site = fct_relabel(Site, ~paste0(.x, " [N=", N, "]"))) %>%
  filter(N > 25) %>%
  ggplot(aes(x = Z, y = Site, group = Site)) +
  ggridges::geom_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous(limits = c(-3, 3)) +
  labs(x = "Relative body size (Z)", y = NULL) +
  theme_minlines() +
  theme(axis.text.y = element_text(hjust = 0, vjust = 0),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing = unit(0, "mm"),
        panel.border = element_blank()) ->
  figE

ggsave("analysis/figures/figE.pdf", figE, device = cairo_pdf,
       width = w1col, height = w1col * 1.5, units = "mm")

# Figure F: Relative body size of Lepus through time, using all elements and
#   measurements.
figF <- plot_mcts(lepus_sim, Z, climate_periods, "Relative body size (Z)",
                  consolidate = TRUE)
ggsave("analysis/figures/figF.pdf", figF, device = cairo_pdf,
       width = w2col, height = w2col * 1/3, units = "mm")

# Figure G: Relative body size of Vulpes through time, using all elements and
#   measurements
figG <- plot_mcts(vulpes_sim, Z, climate_periods, "Relative body size (Z)",
                  consolidate = TRUE)
ggsave("analysis/figures/figG.pdf", figG, device = cairo_pdf,
       width = 190, height = 50, units = "mm")

# Figure H: maps of modern biogeography
# TODO: Improve colours. Maybe ggpattern?
figH <- ggplot() +
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
ggsave("analysis/figures/figH.pdf", figH, device = cairo_pdf,
       width = w1.5col, height = w1.5col * 2/3, units = "mm")

# Figure I: comparison of Z distributions at adjacent sites:
# * Wadi Jilat EPAL vs. Neo
# * Dhuweila PPNB vs. Neo
# * Shubayqa 1 Early, Late and Final
gazella %>%
  mutate(Site = replace(Site,
                        SiteCode %in% c("WJ22_Upper", "WJ22_Middle", "WJ22_Lower",
                                        "WJ6_Upper", "WJ8", "WJ7", "WJ13", "WJ25"),
                        "Wadi Jilat")) %>%
  mutate(Site = replace(Site,
                        SiteCode %in% c("DHW_PPNB", "DHW_LN"),
                        "Dhuweila")) %>%
  mutate(Site = replace(Site,
                        SiteCode %in% c("SHUB1_Early", "SHUB1_Late", "SHUB1_Final"),
                        "Shubayqa 1")) %>%
  filter(Site %in% c("Wadi Jilat", "Dhuweila", "Shubayqa 1")) %>%
  mutate(Period = recode(SiteCode,
                         WJ22_Upper = "Epipalaeolithic",
                         WJ22_Middle = "Epipalaeolithic",
                         WJ22_Lower = "Epipalaeolithic",
                         WJ6_Upper = "Epipalaeolithic",
                         WJ8 = "Epipalaeolithic",
                         WJ7 = "Neolithic",
                         WJ13 = "Neolithic",
                         WJ25 = "Neolithic",
                         DHW_PPNB = "PPNB",
                         DHW_LN = "Late Neolithic",
                         SHUB1_Early = "Early Natufian",
                         SHUB1_Late = "Late/Final Natufian",
                         SHUB1_Final = "Late/Final Natufian")) %>%
  group_by(Period) %>%
  mutate(midZ = median(Z)) ->
  gazella_adj
gazella_adj$Site <- factor(gazella_adj$Site, levels = c("Wadi Jilat",
                                                        "Shubayqa 1",
                                                        "Dhuweila"))
gazella_adj$Period <- factor(gazella_adj$Period, levels = c("Neolithic",
                                                            "Epipalaeolithic",
                                                            "Late/Final Natufian",
                                                            "Early Natufian",
                                                            "Late Neolithic",
                                                            "PPNB"))

figI <- ggplot(gazella_adj, aes(x = Z, label = Period, colour = Period)) +
  facet_wrap(vars(Site), nrow = 1) +
  geom_density(fill = NA) +
  scale_colour_manual(values = rep(c("#e41a1c", "#377eb8"), 3), guide = guide_none()) +
  scale_x_continuous(limits = c(-4.5, 4.5), breaks = seq(-4, 4, by = 1)) +
  labs(x = "Relative body size (Z)", y = NULL) +
  theme_minlines()
figI <- direct.label(figI, method = list("top.bumptwice", fontfamily = "Arial", cex = 0.65))
ggsave("analysis/figures/figI.pdf", figI, device = cairo_pdf,
       width = w2col, height = w2col * 1/3, units = "mm")

# Figure I: Histogram of gazelle scapula BG (showing sexual dimorphism)
figJ <- gazella %>%
  filter(Element == "Scapula") %>%
  filter(!is.na(GLP) | !is.na(BG)) %>%
  select(Site, SiteCode, Specimen, GLP, BG) %>%
  mutate(Period = recode(SiteCode,
                         "AQA" = "Early Epipal.",
                         "AQB" = "Early Epipal.",
                         "KHIV" = "Early Epipal.",
                         "KHIV_A" = "Early Epipal.",
                         "KHIV_B" = "Early Epipal.",
                         "KHIV_C" = "Early Epipal.",
                         "KHIV_D" = "Early Epipal.",
                         "SHUB1_Early" = "Mid–Late Epipal.",
                         "SHUB1_Late" = "Mid–Late Epipal.",
                         "SHUB6_Early" = "Mid–Late Epipal.",
                         "WJ13" = "Neolithic",
                         "WJ22_Middle" = "Mid–Late Epipal.",
                         "WJ22_Upper" = "Mid–Late Epipal.",
                         "WJ6_Upper" = "Early Epipal.",
                         "WJ7" = "Neolithic")) %>%
  ggplot(aes(x = BG, fill = Period)) +
  geom_histogram(binwidth = 0.2) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Scapula BG", y = NULL) +
  theme_minlines()
ggsave("analysis/figures/figJ.pdf", figJ, device = cairo_pdf,
       width = w1.5col, height = w1.5col * 2/3, units = "mm")

# Table A: Sample sizes used in metric analysis, by taxon and period
bind_rows(Gazella = gazella, Lepus = lepus, Vulpes = vulpes, .id = "Taxon") %>%
  mutate(Period = recode(SiteCode,
                         "DHW_LN" = "Late Neo",
                         "JN" = "Late Neo",
                         "BQ27" = "Late Neo",
                         "WJ13" = "Late Neo",
                         "WJ25" = "Late Neo",
                         "DHW_PPNB" = "PPNB",
                         "WJ7" = "PPNB",
                         "SHUB6_Early" = "Late Epipal",
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
  tableA
gtsave(tableA, "tableA.html", "analysis/figures")
