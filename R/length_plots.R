#' Length plots to length-at-age data This function creates three plots in the
#' figs/ directory - one of mean length by age across years, one of raw length
#' observations by year, color coded by age, and one of standardized lengths by
#' year.
#'
#' @param data input data frame to plot; must have columns age_years, year,
#'   length_cm, and standardl
#' @param name common name of the species, this will be used in the file name
#'
#' @importFrom nmfspalette scale_color_nmfs
#' @export
length_plots <- function(data, name) {
  processed_summ <- data %>%
    group_by(age_years, year) %>%
    summarise("mean" = mean(length_cm))

  # Plot mean length by year and age
  p <- ggplot(processed_summ, aes_string(
    x = "year", y = "mean",
    group = "factor(age_years)", color = "factor(age_years)"
  )) +
    geom_path() +
    scale_color_nmfs(name = "Age (years)") +
    theme_classic()
  p

  ggsave(paste("figs/", "mean_laa_", name, ".png", sep = ""), p)

  # Set up base ggplot object & theme
  pbase <- ggplot(data, aes_string(x = "year",
    group = "factor(age_years)", color = "factor(age_years)")) +
    scale_color_nmfs(name = "Age (years)") +
    theme_classic()

  # Plot raw length observations by year and age
  p2 <- pbase +
    geom_point(aes_string(y = "length_cm")) +
    labs(y = "Length (cm)", title = "Length by year")
  p2
  ggsave(paste("figs/", "raw_length_", name, ".png", sep = ""), p2)

  # Plot standardized lengths by year and age
  p3 <- pbase + geom_point(aes_string(y = "standardl")) +
    geom_hline(yintercept = 3, color = nmfs_cols("dk_gray_1")) +
    geom_hline(yintercept = -3, color = nmfs_cols("dk_gray_2")) +
    labs(y = "Standardized length deviation", title = "Standard length deviations")
  p3
  ggsave(paste("figs/", "std_length_", name, ".png", sep = ""), p3)

  invisible(pbase)
}
