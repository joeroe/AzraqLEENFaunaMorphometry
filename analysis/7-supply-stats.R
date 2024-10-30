# 7-suppl-stats.R
# Supplementary statistics for manuscript text

# Mean gazelle Z 25-15 ka
mean(gazella_sim[gazella_sim$calBP <= 25000 & gazella_sim$calBP > 15000,]$Z, na.rm = TRUE)
sd(gazella_sim[gazella_sim$calBP <= 25000 & gazella_sim$calBP > 15000,]$Z, na.rm = TRUE)

# Mean gazelle Z 12.5-11.5 ka
mean(gazella_sim[gazella_sim$calBP <= 12500 & gazella_sim$calBP > 11500,]$Z, na.rm = TRUE)
sd(gazella_sim[gazella_sim$calBP <= 12500 & gazella_sim$calBP > 11500,]$Z, na.rm = TRUE)

# Mean gazelle Z 11-9 ka
mean(gazella_sim[gazella_sim$calBP <= 11000 & gazella_sim$calBP > 9000,]$Z, na.rm = TRUE)
sd(gazella_sim[gazella_sim$calBP <= 11000 & gazella_sim$calBP > 9000,]$Z, na.rm = TRUE)

# Mean gazelle Z 9-7.5 ka
mean(gazella_sim[gazella_sim$calBP <= 9000 & gazella_sim$calBP > 7.5,]$Z, na.rm = TRUE)
sd(gazella_sim[gazella_sim$calBP <= 9000 & gazella_sim$calBP > 7.5,]$Z, na.rm = TRUE)

# Mean gazelle astragalus GLI/Bd 21-17 ka
mean(gazella_sim[gazella_sim$calBP <= 21000 & gazella_sim$calBP > 17000,]$GLI, na.rm = TRUE)
sd(gazella_sim[gazella_sim$calBP <= 21000 & gazella_sim$calBP > 17000,]$GLI, na.rm = TRUE)
mean(gazella_sim[gazella_sim$calBP <= 21000 & gazella_sim$calBP > 17000,]$Bd, na.rm = TRUE)
sd(gazella_sim[gazella_sim$calBP <= 21000 & gazella_sim$calBP > 17000,]$Bd, na.rm = TRUE)

# Mean gazelle astragalus GLI/Bd 15-12 ka
mean(gazella_sim[gazella_sim$calBP <= 15000 & gazella_sim$calBP > 12000,]$GLI, na.rm = TRUE)
sd(gazella_sim[gazella_sim$calBP <= 15000 & gazella_sim$calBP > 12000,]$GLI, na.rm = TRUE)
mean(gazella_sim[gazella_sim$calBP <= 15000 & gazella_sim$calBP > 12000,]$Bd, na.rm = TRUE)
sd(gazella_sim[gazella_sim$calBP <= 15000 & gazella_sim$calBP > 12000,]$Bd, na.rm = TRUE)

# Mean gazelle astragalus GLI/Bd 12.5 ka
mean(gazella_sim[gazella_sim$calBP <= 12600 & gazella_sim$calBP > 12400,]$GLI, na.rm = TRUE)
sd(gazella_sim[gazella_sim$calBP <= 12600 & gazella_sim$calBP > 12400,]$GLI, na.rm = TRUE)
mean(gazella_sim[gazella_sim$calBP <= 12600 & gazella_sim$calBP > 12400,]$Bd, na.rm = TRUE)
sd(gazella_sim[gazella_sim$calBP <= 12600 & gazella_sim$calBP > 12400,]$Bd, na.rm = TRUE)

# Mean gazelle astragalus GLI/Bd 9–7.5 ka
mean(gazella_sim[gazella_sim$calBP <= 9000 & gazella_sim$calBP > 7500,]$GLI, na.rm = TRUE)
sd(gazella_sim[gazella_sim$calBP <= 9000 & gazella_sim$calBP > 7500,]$GLI, na.rm = TRUE)
mean(gazella_sim[gazella_sim$calBP <= 9000 & gazella_sim$calBP > 7500,]$Bd, na.rm = TRUE)
sd(gazella_sim[gazella_sim$calBP <= 9000 & gazella_sim$calBP > 7500,]$Bd, na.rm = TRUE)

# Median gazelle Z UW18Upper
median(gazella_sim[gazella_sim$SiteCode == "UW18_Upper",]$Z, na.rm = TRUE)

# Median gazelle Z SHUB1Early
median(gazella_sim[gazella_sim$SiteCode == "SHUB1_Early",]$Z, na.rm = TRUE)

# Median gazelle Z SHUB1Final
median(gazella_sim[gazella_sim$SiteCode == "SHUB1_Final",]$Z, na.rm = TRUE)

# Median gazelle Z WJ7
median(gazella_sim[gazella_sim$SiteCode == "WJ7",]$Z, na.rm = TRUE)

# Median gazelle Z DHW LNeo
median(gazella_sim[gazella_sim$SiteCode == "DHW_LN",]$Z, na.rm = TRUE)

# Mean hare Z 20-19 ka
mean(lepus_sim[lepus_sim$calBP <= 20000 & lepus_sim$calBP > 19000,]$Z, na.rm = TRUE)
sd(lepus_sim[lepus_sim$calBP <= 20000 & lepus_sim$calBP > 19000,]$Z, na.rm = TRUE)

# Mean hare Z 17.5 ka
mean(lepus_sim[lepus_sim$calBP <= 17600 & lepus_sim$calBP > 17400,]$Z, na.rm = TRUE)
sd(lepus_sim[lepus_sim$calBP <= 17600 & lepus_sim$calBP > 17400,]$Z, na.rm = TRUE)

# Mean hare Z 14.5-11.75 ka
mean(lepus_sim[lepus_sim$calBP <= 14500 & lepus_sim$calBP > 11750,]$Z, na.rm = TRUE)
sd(lepus_sim[lepus_sim$calBP <= 14500 & lepus_sim$calBP > 11750,]$Z, na.rm = TRUE)

# Mean hare Z 10.5-10 ka
mean(lepus_sim[lepus_sim$calBP <= 10500 & lepus_sim$calBP > 10000,]$Z, na.rm = TRUE)
sd(lepus_sim[lepus_sim$calBP <= 10500 & lepus_sim$calBP > 10000,]$Z, na.rm = TRUE)

# Mean hare Z 9-8.5 ka
mean(lepus_sim[lepus_sim$calBP <= 9000 & lepus_sim$calBP > 8500,]$Z, na.rm = TRUE)
sd(lepus_sim[lepus_sim$calBP <= 9000 & lepus_sim$calBP > 8500,]$Z, na.rm = TRUE)

# Mean hare calcaneum Early Epipal.
filter(lepus_all, SiteCode %in% sites_early_epipal & Element == "Calcaneum") |>
  summarise(mean_GL = mean(GL, na.rm = TRUE), sd_GL = sd(GL, na.rm = TRUE),
            mean_GB = mean(GB, na.rm = TRUE), sd_GB = sd(GB, na.rm = TRUE))

# Mean hare calcaneum Neolithic
filter(lepus_all, SiteCode %in% c(sites_early_neo, sites_late_neo) & Element == "Calcaneum") |>
  summarise(mean_GL = mean(GL, na.rm = TRUE), sd_GL = sd(GL, na.rm = TRUE),
            mean_GB = mean(GB, na.rm = TRUE), sd_GB = sd(GB, na.rm = TRUE))

# Mean hare distal humerus Shub Natufian
filter(lepus_all, SiteCode %in% c("SHUB1_Early", "SHUB1_Late") & Element == "Humerus") |>
  summarise(mean_Bd = mean(Bd, na.rm = TRUE), sd_Bd = sd(Bd, na.rm = TRUE))

# Mean hare distal humerus DHW PPNB
filter(lepus_all, SiteCode == "DHW_PPNB" & Element == "Humerus") |>
  summarise(mean_Bd = mean(Bd, na.rm = TRUE), sd_Bd = sd(Bd, na.rm = TRUE))

# Mean hare distal humerus DHW LNeo
filter(lepus_all, SiteCode == "DHW_LN" & Element == "Humerus") |>
  summarise(mean_Bd = mean(Bd, na.rm = TRUE), sd_Bd = sd(Bd, na.rm = TRUE))

# Mean fox Z Early Epipal
mean(vulpes[vulpes$SiteCode %in% sites_early_epipal,]$Z, na.rm = TRUE)
sd(vulpes[vulpes$SiteCode %in% sites_early_epipal,]$Z, na.rm = TRUE)

# Mean fox Z Late Epipal
mean(vulpes[vulpes$SiteCode %in% sites_late_epipal,]$Z, na.rm = TRUE)
sd(vulpes[vulpes$SiteCode %in% sites_late_epipal,]$Z, na.rm = TRUE)

# Mean fox Z Neo
mean(vulpes[vulpes$SiteCode %in% c(sites_early_neo, sites_late_neo),]$Z, na.rm = TRUE)
sd(vulpes[vulpes$SiteCode %in% c(sites_early_neo, sites_late_neo),]$Z, na.rm = TRUE)
