#' Exclude length measurements
#'
#' Filter length measurements, that aren't correlated with body mass, from metric
#' data (see Meadows 1999), except for astragalus GLI.
#'
#' @param metrics A tibble of metric data using standard abbreviations
#'   (after von den Driesch 1976)
#' @param metadata List of non-metric column names
#'
#' @return A filtered tibble
#' @export
#'
#' @examples
exclude_lengths <- function(metrics, bad_metrics, metadata) {
  bad_metrics <- intersect(bad_metrics, setdiff(colnames(metrics), metadata))
  good_metrics <- setdiff(colnames(metrics), c(metadata, bad_metrics))

  if(!"GLI" %in% colnames(metrics)) {
    metrics$GLI <- rep(NA, nrow(metrics))
  }

  metrics %>%
    select(-bad_metrics) %>%
    mutate(GLI = replace(GLI, Element != "Astragalus", NA)) %>%
    filter(rowSums(is.na(.[,good_metrics])) != ncol(.[,good_metrics])) %>%
    return()
}

#' @export
check_std <- function(obs, std, obs_cols, std_cols) {
  not_all_na <- function(x) !all(is.na(x))
  okay <- function(x) {
    x <- replace(x, is.na(x), FALSE)
    if(length(x) < 2) x[2] <- FALSE
    if(x[1] == TRUE && x[2] == FALSE) return(FALSE)
    else return(TRUE)
  }

  obs %>%
    group_by(Element) %>%
    summarise_at(obs_cols, not_all_na) ->
    obs_lgl

  std %>%
    mutate_at(std_cols, is.finite) ->
    std_lgl

  union_all(obs_lgl, std_lgl) %>%
    group_by(Element) %>%
    summarise_at(obs_cols, okay) %>%
    return()
}

#' Log Standard Index
#'
#' Calculates the log standard index (LSI) or difference-of-logs index, which
#' standardises measurements from different skeletal elements with reference
#' to a "standard animal". Specifically:
#'
#'     LSI = log X - log S
#'
#' Where X is the measurement of the specimen and S is the same measurement of
#' the same element from a standard reference animal.
#'
#' @param obs Data frame or tibble of observed animal measurements
#' @param std Data frame or tibble of standard animal measurements
#' @param check TRUE (default) will print a warning when a measurement in obs is
#' not matched in std
#'
#' @return A tibble of log standard indexes.
#' @export
#'
#' @examples
lsi <- function(obs, std, obs_metadata, std_metadata) {
  obs %>%
    mutate(uid = rownames(.)) %>%
    reshape2::melt(id.vars = c("uid", obs_metadata), value.name = "obs") ->
    mobs

  std %>%
    reshape2::melt(id.vars = std_metadata, value.name = "std") ->
    mstd

  left_join(mobs, mstd, by = c("Element", "variable")) %>%
    mutate(lsi = log10(obs) - log10(std)) %>%
    select(-obs, -std) %>%
    reshape2::dcast(... ~ variable, value.var = "lsi") %>%
    select(-uid) %>%
    return()
}
