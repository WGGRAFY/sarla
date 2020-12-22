get_std_length <- function(df){
  dataset <- df %>% filter((year >= 1995)&(year <= 2010))
  length_age_matrix <- dataset %>%
    group_by(age_years) %>%
    summarise(meanl = mean(length_cm), sdl = sd(length_cm), count = n())
  return(length_age_matrix)
}
