#' process_length_data
#'A function that takes the input raw data, selects a species' data from it, and filters and standardizes it.
#'
#' @param data__ A Warehouse data object containing survey data
#' @param common_ Common name of the species you are interested in
#' @param sex_ sex to look at, must be "M" or "F"
#' @param years_ The year at which the survey type changes, this can be NULL if you want the data from all or one survey across all years
#' @param survey_string A string in the `project` field that specifies which survey
#' @param minimum_n The minimum sample size used to set the maximum age of fish observed
#'
#' @return the processed length data
#' @export
#'
#' @examples
process_length_data <- function(data__, common_, sex_, years_ = NULL,
                                survey_string, minimum_n){

  #Select species data, add year column
  sable_full_data <- data__ %>%
    filter(common_name==common_) %>%
    filter(sex==sex_) %>%
    mutate(year = substr(datetime_utc_iso, 1, 4))

  #Add project/date filter
  if(!is.null(years_)){
    sable_data <- sable_full_data %>%
                    filter((str_detect(project, survey_string[1]) & (year < years_))|(str_detect(project, survey_string[2]) & (year >= years_)))
  } else{
    sable_data <- sable_full_data %>% filter(str_detect(project, survey_string))
  }

  sable_data <- sable_data %>%
    select(age_years, year, length_cm, project) %>%
    mutate(year = as.numeric(year))

  #Get mean stats
  mean_mat<- get_std_length(sable_data)

  #
  maxage <- filter(mean_mat,count<minimum_n)%>% select(age_years) %>% min()
  processed_data <- left_join(sable_data, mean_mat, by="age_years") %>%
    filter(age_years <= maxage) %>%
    mutate(standardl = (length_cm-meanl)/sdl)


  return(processed_data)
}
