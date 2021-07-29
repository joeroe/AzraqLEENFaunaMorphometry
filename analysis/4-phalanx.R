# APPENDIX: THE PHALANX EFFECT -------------------------------------------------

# Some others do not include phalanx measurements in morphometric analyses, but
# these make up a large proportion of our dataset.
# We therefore investigated whether phalanges had a different LSI distribution
# to other elements:
gazella %>%
  mutate(phalanges = Element %in% c("Phalanx1", "Phalanx2", "Phalanx3")) %>%
  ggplot(aes(x = LSI, colour = phalanges, fill = phalanges)) +
  geom_density(colour = "black", fill = "black", alpha = 0.67) +
  geom_density(alpha = 0.67) +
  scale_fill_discrete(name = NULL, guide = guide_legend(reverse = TRUE),
                      labels = c("Other elements", "Phalanges")) +
  scale_colour_discrete(name = NULL, guide = guide_legend(reverse = TRUE),
                        labels = c("Other elements", "Phalanges"))


# And whether including them changed the overall trend in the time series:
sim_phal <- sim_ages(gazella %>% filter(Element %in% c("Phalanx1",
                                                           "Phalanx2",
                                                           "Phalanx3")),
                     radiocarbon_sum, SiteCode, 100, LSI, Z)
sim_nophal <- sim_ages(gazella %>% filter(!Element %in% c("Phalanx1",
                                                              "Phalanx2",
                                                              "Phalanx3")),
                       radiocarbon_sum, SiteCode, 100, LSI, Z)
ggplot(gazella_sim, aes(x = calBP, y = Z)) +
  geom_smooth(colour = "black") +
  geom_smooth(data = sim_phal, colour = "green") +
  geom_smooth(data = sim_nophal, colour ="red")

# The results show that while phalanx LSI values are shifted lower than that of
# other elements (probably related to the choice of standard animal), the
# shape of the distributions are identical. Also, whilst the phalanges do
# diverge from the overall trend in places, this is true of all elements:

# We therefore decided to proceed with the analysis with phalanx measurements
# included.