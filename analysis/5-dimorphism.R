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
  select(Site, SiteCode, Specimen, GLP, BG) ->
  gazella_dimorph

# Sample sizes
gazella_dimorph %>%
  mutate(GLP = !is.na(GLP), BG = !is.na(BG)) %>%
  janitor::tabyl(GLP, BG)

# Scatter plots by period
gazella_dimorph %>%
  mutate(Period = recode(SiteCode,
                         "AQA" = "Early–Mid Epipal.",
                         "AQB" = "Early–Mid Epipal.",
                         "KHIV" = "Early–Mid Epipal.",
                         "KHIV_A" = "Early–Mid Epipal.",
                         "KHIV_B" = "Early–Mid Epipal.",
                         "KHIV_C" = "Early–Mid Epipal.",
                         "KHIV_D" = "Early–Mid Epipal.",
                         "SHUB1_Early" = "Late Epipal.",
                         "SHUB1_Late" = "Late Epipal.",
                         "SHUB6_Early" = "Late Epipal.",
                         "WJ13" = "Neolithic",
                         "WJ22_Middle" = "Early–Mid Epipal.",
                         "WJ22_Upper" = "Early–Mid Epipal.",
                         "WJ6_Upper" = "Early–Mid Epipal.",
                         "WJ7" = "Neolithic")) %>%
  ggplot(aes(x = GLP, y = BG, colour = Period)) +
  geom_point() +
  scale_colour_brewer(type = "qual", palette = "Set1") +
  theme_minlines()

# BG Histogram by period

gazella_dimorph %>%
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
  theme_minlines()
