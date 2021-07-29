# LOG STANDARD INDICES ----------------------------------------------------
# This is the standard method used in zooarchaeological morphometrics:
# individual measurements are scaled relative to the same measurements on the
# same element of a (usually modern) "standard animal". In this case, we are
# using our own standard Gazella subgutturosa, becausea there is no existing
# published standard.

# Standard animal (average of reference specimens)
gazella_nhm %>%
  group_by(Element) %>%
  summarise_at(gazella_nhm_cols$metrics, mean, na.rm = TRUE) ->
  gazella_std

# Check for missing standard animal metrics
check_std(gazella, gazella_std, gazella_cols$metrics, gazella_nhm_cols$metrics)

# Calculate average LSIs
gazella %>%
  lsi(gazella_std, gazella_cols$metadata, c("Element")) %>%
  select(gazella_cols$metrics) %>%
  rowMeans(na.rm = TRUE) ->
  gazella$LSI



# STANDARD SCORES ---------------------------------------------------------
# Unfortunately, we do not have standard measurements to match all the elements/
# measurements in the zooarchaeological assemblages. Also, I am not entirely
# sure the difference-of-logs methods does what zooarchaeologists want it to do.
# It transforms the data to a ratio of standard to specimen; there's no
# standardisation.
#
# Another approach is to use conventional statistical standardisation (standard
# scores or z-scores), which does not use an external standard, and therefore
# allows us to use our full sample.

# Standardise
gazella %>%
  group_by(Element) %>%
  mutate_at(gazella_cols$metrics, scale) ->
  gazella_z

gazella_z %>%
  ungroup() %>%
  select(gazella_cols$metrics) %>%
  transmute(mean = rowMeans(., na.rm = TRUE)) %>%
  as_vector() ->
  gazella$Z

rm(gazella_z)

lepus %>%
  group_by(Element) %>%
  mutate_at(lepus_cols$metrics, scale) ->
  lepus_z

lepus_z %>%
  ungroup() %>%
  select(lepus_cols$metrics) %>%
  transmute(mean = rowMeans(., na.rm = TRUE)) %>%
  as_vector() ->
  lepus$Z

rm(lepus_z)

vulpes %>%
  group_by(Element) %>%
  mutate_at(vulpes_cols$metrics, scale) ->
  vulpes_z

vulpes_z %>%
  ungroup() %>%
  select(vulpes_cols$metrics) %>%
  transmute(mean = rowMeans(., na.rm = TRUE)) %>%
  as_vector() ->
  vulpes$Z

rm(vulpes_z)
