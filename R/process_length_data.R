process_length_data <- function(data__, common_, sex_, years_ = NULL,
                                survey_string, minimum_n){

  #Select species data, add year column
  sable_full_data <- WareHouse.All.Ages.Env %>%
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
