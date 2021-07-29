#' @export
theme_minlines <- function() {
  return(theme_linedraw(base_size = 7, base_family = "Arial") +
           theme(panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank(),
                 panel.grid.major.x = element_line(colour = "#333333"),
                 panel.grid.minor.x = element_blank(),
                 strip.background = element_blank(),
                 strip.text = element_text(size = rel(1), colour = "black"),
                 axis.text = element_text(size = rel(1)),
                 axis.title = element_text(size = rel(1)),
                 axis.ticks.length = unit(3, "pt"))
  )
}

#' @export
kacalbp_labels <- function(breaks, divisor = 1) {
  breaks[(breaks %% divisor) != 0] <- NA
  breaks <- breaks / 1000
  labels <- as.character(breaks)
  labels <- replace_na(labels, "")
  return(labels)
}

#' @export
scales_chrono <- function(periods, from=25000, to=7000, by=-500) {
  scale_x_reverse(
    breaks = seq(from=from, to=to, by=by),
    labels = kacalbp_labels(seq(from=from, to=to, by=by), by*2),
    limits = c(from, to),
    sec.axis = sec_axis(
      trans = ~.,
      name = NULL,
      breaks = c(periods$start,
        periods$end + ((periods$start-periods$end) / 2),
        periods$end) %>%
        sort(decreasing = TRUE) %>%
        unique(),
      labels = c("", rbind(periods$period, rep("", length(periods$period))))),
    expand = c(0, abs(by))
    )
}

#' @export
highlight_yd <- function() {
  annotate("rect", xmin=11700, xmax=12900, ymin = -Inf, ymax = Inf,
            fill = alpha("grey", 0.5), colour = NA)
}

#' @export
highlight_82k <- function() {
  annotate("rect", xmin = 8000, xmax = 8600, ymin = -Inf, ymax = Inf,
            fill = alpha("grey", 0.5), colour = NA)
}

# Plot radiocarbon chronology
#' @export
plot_chronology <- function(sites, summed_dates, periods) {
  df <- bind_rows(lapply(summed_dates, unclass), .id = "site")
  df$site <- factor(df$site, levels = sites$SiteCode,
                         labels = paste0(sites$Site, " [N=", sites$ndates, "]"))

  ggplot(df, aes(calBP, PrDens)) +
    facet_wrap(~ site, ncol = 1, scales = "free_y", strip.position = "left") +
    scales_chrono(periods) +
    scale_y_continuous(expand = c(0.3, 0), sec.axis = dup_axis(breaks = 0)) +
    geom_area(fill = "black", colour = "black", size = 0.25) +
    xlab("ka cal BP") +
    theme_minlines() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.spacing = unit(0, "mm"),
          panel.border = element_blank(),
          axis.line = element_line(),
          strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, hjust = 1)) %>%
    return()
}

# Plot osteometric data as a Monte Carlo time series
#' @export
plot_mcts <- function(sim, y, chrono_scale, y_label = NA, consolidate = FALSE) {
  if (!consolidate) {
    aesthetics <- aes(x = calBP, y = !!enquo(y), group = nsim)
    a <- 0.01
    g <- NULL
  }
  else {
    aesthetics <- aes(x = calBP, y = !!enquo(y))
    a <- 1
    g <- geom_quantile(quantiles = c(0.1, 0.9), lambda = 10, method = "rqss",
                  colour = "black", linetype = "longdash")
  }

  ggplot(sim, aesthetics) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr"), se = FALSE,
                n = 320, span = 1, colour = alpha("black", a), fullrange = TRUE) +
    g +
    highlight_yd() +
    highlight_82k() +
    scales_chrono(chrono_scale) +
    xlab("ka cal BP") +
    ylab(y_label) +
    theme_minlines() %>%
    return()
}
