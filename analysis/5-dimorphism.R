# APPENDIX: SEXUAL DIMORPHISM ---------------------------------------------

# Can the observed body size changes be explained by sexual dimorphism?
# I.e. are male/female specimens disproportionately represented in different
#   periods?
# To investigate this we performed a brief analysis of two metrics known to be
#   strongly sexually dimorphic – scapula GLP and BG.

# Data: gazella scapula GLP and BG
gazella %>%
  filter(Element == "Scapula") %>%
  filter(!is.na(GLP) | !is.na(BG)) %>%
  select(Site, SiteCode, Specimen, GLP, BG) |>
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
  ) ->
  gazella_dimorph

# Sample sizes
gazella_dimorph %>%
  mutate(GLP = !is.na(GLP), BG = !is.na(BG)) %>%
  janitor::tabyl(GLP, BG)

# BG Histogram by period

figS4 <- gazella_dimorph %>%
  ggplot(aes(x = BG, fill = Period)) +
  geom_histogram(binwidth = 0.2) +
  scale_colour_brewer(palette = "PuOr") +
  theme_minlines() +
  theme(legend.position = "bottom")

# Scatter plots by period
figS5 <- gazella_dimorph %>%
  ggplot(aes(x = GLP, y = BG, colour = Period)) +
  geom_point() +
  scale_colour_brewer(palette = "PuOr") +
  theme_minlines() +
  theme(legend.position = "bottom")

