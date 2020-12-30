require(dplyr)
require(stringr)
require(ggplot2)
require(tidyr)
require(nmfspalette)
load("./data/WareHouse_2019.RData")
str(WareHouse.All.Ages.Env)

#Peek at which spp has the most data
WareHouse.All.Ages.Env %>%
  group_by(common_name) %>%
  count() %>% filter(n>15000)


#sablefish so lets look at that first
spp <- c("sablefish", "darkblotched rockfish", "shortbelly rockfish", "Pacific hake",
         "Pacific sanddab", "lingcod", "petrale sole")
years <- c(2003, 2003, rep(NULL, 5))
surv_string <- list()
surv_string[[1]] <- surv_string[[2]] <- c("Triennial","Combination")
surv_string[4:7] <- "Triennial|Combination"
surv_string[[3]] <- "Combination"
model_data <- vector("list")
for(i in 1:length(spp)){
processed_data <- process_length_data(data__ = WareHouse.All.Ages.Env,
                                      common_ = spp[i],
                                      sex_ = "F",
                                      survey_string = surv_string[[i]],
                                      years_ = years[i],
                                      minimum_n = 10)

#widen data so it is by row = age and columns = year
model_data[[i]] <- processed_data %>%
  select(age_years, year, standardl) %>%
  arrange(year, age_years) %>%
  unique() %>%
  pivot_wider(names_from=age_years, values_from=standardl)

}




mod <- stan(file.path("src","base.stan"),
            iter = 2000,
            warmup = 1000,
            data = length_data)

shinystan(mod)
