#' process_length_data
#'A function that takes processed length data and standardizes it to calculate the means.
#'
#' @param spp_data a tibble that has, at minimum, the columns \code{age_years}, \code{length_cm},\code{year}
#' @param common_ Common name of the species you are interested in
#' @param minimum_n The minimum sample size used to set the maximum age of fish observed
#' @param plot_bool TRUE or False should plots be made
#'
#' @return the processed length data
#' @export
process_length_data <- function(spp_data, common_, minimum_n, plot_bool=FALSE, ...){

  #Get mean stats
  mean_mat<- get_std_length(spp_data, ...)

  #Get rid of ages with fewer than minimum_n obs
  ages <- filter(mean_mat,count<minimum_n)%>% select(age_years)


  #process the data 1. join data with mean matrix, 2. remove ages with small sample size
  #3. group by ages and years 4. get mean length for each age-year combo
  #5. get difference between age-year combo and overall mean length for ages
  processed_data <- left_join(spp_data, mean_mat, by="age_years") %>%
    filter(!(age_years %in% ages)) %>%
    filter(sdl>0)

  if(plot_bool){
  plot_data <- processed_data %>%
    mutate(standardl = (length_cm-meanl)/sdl)



  length_plots(plot_data, name=common_)
  }
  processed_data <- processed_data %>%
    group_by(age_years, year) %>%
    mutate(meanal = mean(length_cm)) %>%
    mutate(standardl = (meanal-meanl)/sdl)

  return(processed_data)
}
