#' process_length_data
#'A function that takes the input raw data, selects a species' data from it, and filters and standardizes it.
#'
#' @param data__ A Warehouse data object containing survey data
#' @param common_ Common name of the species you are interested in
#' @param sex_ sex to look at, must be "M" or "F"
#' @param years_ The year at which the survey type changes, this can be NULL if you want the data from all or one survey across all years
#' @param survey_string A string in the `project` field that specifies which survey
#' @param minimum_n The minimum sample size used to set the maximum age of fish observed
#' @param plot_bool TRUE or False should plots be made
#'
#' @return the processed length data
#' @export
#'
#' @examples
process_length_data <- function(data__, common_, sex_, years_ = NULL,
                                survey_string, minimum_n, plot_bool=FALSE){

  #Select species data, add year column
  full_data <- data__ %>%
    filter(common_name==common_) %>%
    filter(sex==sex_) %>%
    mutate(year = substr(datetime_utc_iso, 1, 4))

  #Add project/date filter
  if(!is.na(years_)){
    spp_data <- full_data %>%
                    filter((str_detect(project, survey_string[1]) & (year < years_))|(str_detect(project, survey_string[2]) & (year >= years_)))
  } else{
    spp_data <- full_data %>% filter(str_detect(project, survey_string))
  }

  spp_data <- spp_data %>%
    select(age_years, year, length_cm, project) %>%
    mutate(year = as.numeric(year))

  #Get mean stats
  mean_mat<- get_std_length(spp_data)

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
