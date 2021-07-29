# MONTE CARLO SIMULATION --------------------------------------------------

# Assign an absolute age to each specimen by repeatedly sampling from the site
# radiocarbon distribution
gazella_sim <- sim_ages(gazella, radiocarbon_sum, SiteCode, 10000, Element, GLI, Bd, LSI, Z)
lepus_sim <- sim_ages(lepus, radiocarbon_sum, SiteCode, 10000, Z)
vulpes_sim <- sim_ages(vulpes, radiocarbon_sum, SiteCode, 10000, Z)
