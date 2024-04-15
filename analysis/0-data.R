# MORPHOMETRIC DATA -----------------------------------------------------------

read_csv("./analysis/data/ejordan_gazella_metrics.csv",
         col_types = cols(
           .default = col_double(),
           Source = col_character(),
           Site = col_character(),
           Phase = col_character(),
           Context = col_character(),
           SiteCode = col_character(),
           Specimen = col_character(),
           Element = col_character(),
           GD = col_number()
         )) %>%
  filter(Element != "Navicular-cuboid") %>%
  filter(Element != "Patella") ->
  gazella
lepus <- read_csv("./analysis/data/ejordan_lepus_metrics.csv")
vulpes <- read_csv("./analysis/data/ejordan_vulpes_metrics.csv")

gazella_cols <- list(
  metadata = c("Source", "Site", "Phase", "Context", "SiteCode", "Specimen", "Element"),
  metrics = colnames(gazella)[8:44]
)
lepus_cols <- list(metadata = gazella_cols$metadata,
                   metrics = colnames(lepus[8:24]))
vulpes_cols <- list(metadata = gazella_cols$metadata,
                    metrics = colnames(vulpes[8:34]))

# Reference specimens
read_csv("analysis/data/nhm_gazella_metrics.csv") ->
  gazella_nhm
gazella_nhm_cols <- list(
  metadata = c("Source", "Specimen", "Taxon", "Sex", "Age", "Location", "Element"),
  metrics = colnames(gazella_nhm)[7:17]
)

# Count number of samples (before excluding)
n_gazella <- nrow(gazella)
n_gazella_metrics <- sum(!is.na(gazella[,gazella_cols$metrics]))

n_lepus <- nrow(lepus)
n_lepus_metrics <- sum(!is.na(lepus[,lepus_cols$metrics]))

n_vulpes <- nrow(vulpes)
n_vulpes_metrics <- sum(!is.na(vulpes[,vulpes_cols$metrics]))

# Exclude lengths (see Meadows 1999)
bad_metrics <- c("GLm", "SH", "SB", "GLpe", "GL", "MHP", "MWP", "LG", "LO", "HTC")

gazella <- exclude_lengths(gazella, bad_metrics, gazella_cols$metadata)
gazella_nhm <- exclude_lengths(gazella_nhm, bad_metrics, gazella_nhm_cols$metadata)
lepus_all <- lepus
lepus <- exclude_lengths(lepus, bad_metrics, lepus_cols$metadata)
vulpes <- exclude_lengths(vulpes, bad_metrics, vulpes_cols$metadata)

gazella_cols$metrics <- setdiff(gazella_cols$metrics, bad_metrics)
gazella_nhm_cols$metrics <- setdiff(gazella_nhm_cols$metrics, bad_metrics)
lepus_cols$metrics <- setdiff(lepus_cols$metrics, bad_metrics)
vulpes_cols$metrics <- setdiff(vulpes_cols$metrics, bad_metrics)


# SITE AND RADIOCARBON DATA ------------------------------------------------

# Sites
gazella %>%
  select(SiteCode, Site) %>%
  distinct() ->
  sites

vulpes %>%
  select(SiteCode, Site) %>%
  distinct() %>%
  rbind(sites) ->
  sites

lepus %>%
  select(SiteCode, Site) %>%
  distinct() %>%
  rbind(sites) ->
  sites

sites <- distinct(sites)

# Coordinates
read_csv("analysis/data/ejordan_sites.csv") %>%
  drop_na(longitude, latitude) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), remove = FALSE) ->
  site_coords

# No reliable radiocarbon dates for KHIV C
sites <- filter(sites, SiteCode != "KHIV_C")
gazella$SiteCode[gazella$SiteCode == "KHIV_C"] <- "KHIV"
lepus_all$SiteCode[lepus_all$SiteCode == "KHIV_C"] <- "KHIV"
lepus$SiteCode[lepus$SiteCode == "KHIV_C"] <- "KHIV"
vulpes$SiteCode[vulpes$SiteCode == "KHIV_C"] <- "KHIV"

# Coarse site chronology
sites_early_epipal <- c("AQA", "AQB", "AQD", "UW18_Upper", "KHIV", "KHIV_A",
                        "KHIV_B", "KHIV_D", "WJ6_Upper", "WJ22_Lower", "WJ8",
                        "WJ22_Middle", "AZ17")
sites_late_epipal <- c("SHUB1_Early", "WJ22_Upper", "AZ18", "SHUB1_Late",
                       "SHUB1_Final")
sites_early_neo <- c("WJ7", "DHW_PPNB")
sites_late_neo <- c("WJ25", "WJ13", "BQ27", "JN", "DHW_LN")

# Cultural chronology (after Garrard 2013)
data.frame(period = c("Initial Epipal",
                      "Early Epipal",
                      "Middle Epipal",
                      "Late Epipal",
                      "PPNA",
                      "PPNB",
                      "Late Neo"),
          start = c(24000, 21500, 17500, 14500, 11500, 10500, 9000),
          end = c(21500, 17500, 14500, 11500, 10500, 9000, 7000),
          row.names = c("IEP", "EEP", "MEP", "LEP", "PPNA", "PPNB", "LN"),
          stringsAsFactors = FALSE) -> cultural_periods

# Climate chronology (after Robinson et al. 2006)
data.frame(period = c("Last Glacial Maximum",
                      "",
                      "Bølling-Allerød",
                      "Younger\nDryas",
                      "Early Holocene"),
           start = c(23000, 19000, 14700, 12900, 11700),
           end = c(19000, 14700, 12900, 11700, 8200),
           row.names = c("LGM", "EP", "BA", "YD", "Holo"),
           stringsAsFactors = FALSE) -> climate_periods

# Radiocarbon
radiocarbon <- read_csv("analysis/data/ejordan_radiocarbon.csv")

# Discard outliers and sites not used in the analysis
radiocarbon %>%
  filter(site_code %in% sites$SiteCode) %>%
  filter(outlier != 1) ->
  radiocarbon

# Count number of dates per site
sites$ndates <- sapply(sites$SiteCode, function(site, dates) {
  nrow(dates[dates$site_code == site,])
}, dates = radiocarbon)
sites[sites$SiteCode == "KHIV",]$ndates <- nrow(radiocarbon[radiocarbon$site_code %in%
                                                             c("KHIV", "KHIV_A", "KHIV_B", "KHIV_C", "KHIV_D"),])

# Calibrate and sum dates per site
radiocarbon_sum <- calibrate_and_sum_c14(sites, radiocarbon)

# No radiocarbon dates for Azraq 18, but Garrard and Byrd (2013, p. 103) estimate
# it as 14700 – 12000 cal BP based on the Natufian lithic industry
data.frame(calBP = 14700:12000,
           PrDens = rep(1 / length(14700:12000), length(14700:12000))) %>%
  rcarbon::as.CalGrid() ->
  radiocarbon_sum$AZ18

# Order sites stratigraphically (by mean uncal age)
radiocarbon %>%
  group_by(site_code) %>%
  summarise(bp = as.integer(mean(cra))) %>%
  right_join(sites, by = c("site_code" = "SiteCode")) %>%
  select(SiteCode = site_code, Site, ndates, bp) ->
  sites
sites[sites$SiteCode == "AZ18",]$bp <- rcarbon::uncalibrate(median(14700:12000))$ccCRA
sites <- arrange(sites, bp)




# BIOGEOGRAPHIC DATA ------------------------------------------------------

# Define map projection: Lambert Azimuthal Equal Area centred on Damascus
laea_damascus <- "+proj=laea +lon_0=36.3 +lat_0=33.5"

# IUCN range maps. Citation:
# IUCN 2018. The IUCN Red List of Threatened Species. Version 2018-1.
#   http://www.iucnredlist.org. Downloaded on 17 December 2018.
# Filter by species IDs:
# * Gazella subgutturosa (8976)
# * Gazella marica (8977)
# * Lepus europaeus (41280)
# * Lepus capensis (41277)
# * Vulpes vulpes (23062)
# * Vulpes rueppelli (23053)
# (The geopackage included with this package only includes this subset.)
sf::read_sf("./analysis/data/iucn_ranges.gpkg") %>%
  filter(id_no %in% c(8976, 8977, 41280, 41277, 23062, 23053)) %>%
  filter(legend == "Extant (resident)") %>%
  sf::st_transform(laea_damascus) ->
  iucn_ranges

# Base map (Natural Earth, https://www.naturalearthdata.com/)
sf::st_read("./analysis/data/ne_110m_land_afroeurasia.gpkg", "ne_110m_land_afroeurasia") %>%
  sf::st_transform(laea_damascus) ->
  ne_afroeurasia
