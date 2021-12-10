#' get_std_length
#'
#' This function will process a dataset and return the mean,
#' standard deviation, and count of length observations between the reference
#' period spanning \code{minref} - \code{maxref}
#'
#' @param data__ A data frame with columns years, age_years, and length_cm
#' @param minref minimum year to take the mean over
#' @param maxref maximum year to take the mean over
#'
#' @return a matrix of mean, sd, and n of length at age, where the summary
#'   statistics are calculated from 1995 - 2010
#' @export
#' @importFrom dplyr "%>%" group_by filter summarize select mutate left_join
#' @importFrom ggplot2 geom_path theme_classic ggsave geom_point labs
#'   aes_string scale_fill_gradient2 geom_tile geom_hline
#' @importFrom stats rnorm sd
#' @importFrom graphics matplot par
get_std_length <- function(data__, minref = 1995, maxref = 2010) {
  dataset <- data__ %>% filter((year >= minref) & (year <= maxref))
  length_age_matrix <- dataset %>%
    group_by(age_years) %>%
    summarise(meanl = mean(length_cm), sdl = sd(length_cm), count = n())
  length_age_matrix
}
