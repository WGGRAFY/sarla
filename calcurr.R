require(dplyr)
require(stringr)
require(ggplot2)
require(tidyr)
require(nmfspalette)
require(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
load("./data/WareHouse_2019.RData")
str(WareHouse.All.Ages.Env)

#Peek at which spp has the most data
WareHouse.All.Ages.Env %>%
  group_by(common_name) %>%
  count() %>% filter(n>15000)

devtools::load_all(".")

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
                                      minimum_n = 10,
                                      plot_bool = F)

#widen data so it is by row = age and columns = year
model_data[[i]] <- processed_data %>%
  select(age_years, year, standardl) %>%
  unique() %>%
  arrange(age_years) %>%
  pivot_wider(names_from=age_years, values_from=standardl) %>%
  arrange(year)

}


#format input data for STAN
input_data <- list(Nages = 7,
                   Nyears = 22,
                   laa = t(as.matrix(model_data[[1]][-2,3:9])))

#Sys.setenv(PATH = paste("C:\\rtools40\\mingw_64\\bin", Sys.getenv("PATH"), sep=";"))

# sim data:
sim <- function(sigma_p = 0.2, sigma_o = 0.2) {
  Nages <- 7
  Nyears <- 22
  beta <- 0.3
  xaa <- matrix(nrow = Nages, ncol = Nyears)
  laa <- matrix(nrow = Nages, ncol = Nyears)

  for (i in 1:Nages) {
    xaa[i,1] <- rnorm(1, 0, sigma_p)
  }
  for (y in 1:Nyears) {
    xaa[1,y] <- rnorm(1, 0, sigma_p)
  }
  for (i in 2:Nages) {
    for(y in 2:Nyears){
      xaa[i,y] <- rnorm(1, beta*xaa[i-1,y-1], sigma_p)
    }
  }
  for (i in 1:Nages) {
    for(y in 1:Nyears){
      laa[i,y] <- rnorm(1, xaa[i,y], sigma_o)
    }
  }
  laa
}
dat <- sim()
matplot(t(dat), type = "l")



stan_dat <- list()
stan_dat$laa <- dat
stan_dat$Nages <-7
stan_dat$Nyears <- 22
# stopped here...

#Run base model
mod <- stan("./inst/stan/base.stan",
            iter = 2000, chains = 6,
            control = list(adapt_delta = 0.99),
            data = stan_dat, pars = c("xaa", "sigma_p", "sigma_o", "beta"))
mod

#Look at shinystan output
install.packages("shinystan")
require(shinystan)
shinmod <- as.shinystan(mod)

launch_Shinystan(Shinmod)