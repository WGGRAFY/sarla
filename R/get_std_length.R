#' get_std_length
#' This function will process a dataset and return the mean, standard deviation, and count of length observations between the reference period spanning 1995 - 2010
#'
#' @param data__ A data frame with columns years, age_years, and length_cm
#'
#' @return a matrix of mean, sd, and n of length at age, where the summary statistics are calculated from 1995 - 2010
#' @export
#'
#' @examples
get_std_length <- function(data__){
  dataset <- data__ %>% filter((year >= 1995)&(year <= 2010))
  length_age_matrix <- dataset %>%
    group_by(age_years) %>%
    summarise(meanl = mean(length_cm), sdl = sd(length_cm), count = n())
  return(length_age_matrix)
}
