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
for(i in 1:length(spp)){
processed_data <- process_length_data(data__ = WareHouse.All.Ages.Env,
                                      common_ = spp[i],
                                      sex_ = "F",
                                      survey_string = surv_string[[i]],
                                      years_ = years[i],
                                      minimum_n = 10)
length_plots(processed_data, name=spp[i])
}




mod <- stan(file.path("src","base.stan"),
            iter = 2000,
            warmup = 1000,
            data = length_data)

shinystan(mod)
