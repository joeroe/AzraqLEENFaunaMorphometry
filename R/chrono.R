# Sum by site
#' @export
calibrate_and_sum_c14 <- function(sites, radiocarbon) {
  # Calibrate
  with(radiocarbon,
       rcarbon::calibrate(cra, errors = error, id = lab_id,
                          calCurves = "intcal20", normalised = TRUE,
                          verbose = FALSE)
  ) -> cal_dates

  # Sum
  summed_dates <- lapply(sites$SiteCode, function(site) {
    message(paste("Summing calibrated dates from", site))
    cds <- cal_dates[radiocarbon$site_code == site]
    if(nrow(cds$metadata) > 0) {
      melted <- reshape2::melt(cds$grids, id.vars = NULL)
      min_bp <- min(melted[melted$variable == "calBP", "value"])
      max_bp <- max(melted[melted$variable == "calBP", "value"])
      invisible(spd <- rcarbon::spd(cds, timeRange = c(max_bp, min_bp),
                          datenormalised = TRUE,
                          spdnormalised = TRUE,
                          verbose = FALSE))
      return(spd$grid)
    }
    else {
      return(NA)
    }
  })
  names(summed_dates) <- sites$SiteCode

  # KHIV is a special case
  message("Aggregating data for KHIV (Phases A-D)")
  khiv_codes <- c("KHIV", "KHIV_A", "KHIV_B", "KHIV_C", "KHIV_D")
  summed_dates$KHIV <- rcarbon::spd(cal_dates[radiocarbon$site_code %in% khiv_codes],
                                    timeRange = c(21000, 15000),
                                    spdnormalised = TRUE)$grid

  return(summed_dates)
}

# Monte Carlo simulation of absolute ages
sample_cal_grid <- function(df, cal_grid) {
  if(length(cal_grid) == 1) cal_grid <- cal_grid[[1]]
  df$calBP <- sample(cal_grid$calBP, nrow(df), prob = cal_grid$PrDens,
                     replace = TRUE)
  return(df)
}

#' @export
sim_ages <- function(data, cal_grids, group, n, ...) {
  group <- enexpr(group)
  vars <- enexprs(...)

  get_cal_grid <- function(df, group, cal_grids) {
    df %>%
      select(!!group) %>%
      distinct() %>%
      as.character() ->
      group_val
    return(cal_grids[[group_val]])
  }

  data_length <- nrow(data)
  data %>%
    select(!!group, !!!vars) %>%
    slice(rep(1:nrow(.), each = n)) %>%
    magrittr::inset("nsim", value = rep(1:n, times = data_length)) %>%
    group_by(!!group) %>%
    do(sample_cal_grid(., get_cal_grid(., group, cal_grids))) %>%
    return()
}
