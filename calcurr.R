require(dplyr)
require(stringr)
require(ggplot2)
require(tidyr)
require(nmfspalette)
load("./data/WareHouse.All.Ages.Env, 23 Jul 2019.RData")
str(WareHouse.All.Ages.Env)

#Peek at which spp has the most data
WareHouse.All.Ages.Env %>%
  group_by(common_name) %>%
  count() %>% filter(n>15000)

spp <- "sablefish"

#sablefish so lets look at that first
processed_data <- process_length_data(data__ = WareHouse.All.Ages.Env,
                                      common_ = spp,
                                      sex_ = "F",
                                      survey_string = c("Triennial","Combination"),
                                      years_ = 2003,
                                      minimum_n = 10)

length_plots(processed_data, name=spp)


mod <- stan(file.path("src","base.stan"),
            iter = 2000,
            warmup = 1000,
            data = length_data)

shinystan(mod)
