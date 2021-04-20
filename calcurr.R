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
  gamma_y <- rnorm(Nyears, 0, 0.2)

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
  for (y in 2:Nyears) {
    xaa[,y] <- xaa[,y] + gamma_y[y]
  }
  for (i in 1:Nages) {
    for(y in 1:Nyears){
      laa[i,y] <- rnorm(1, xaa[i,y], sigma_o)
    }
  }
  list(gamma_y = gamma_y, xaa = xaa, laa = laa)
}
set.seed(123)
dat <- sim()
par(mfrow = c(1, 1))
matplot(t(dat$xaa), type = "l", lty = 1)
matplot(t(dat$laa), type = "l", lty = 1)

stan_dat <- list()
stan_dat$laa <- dat$laa
stan_dat$Nages <- 7
stan_dat$Nyears <- 22

# pars <- c("gamma_y", "sigma_p", "sigma_o", "beta", "xaa", "laa_postpred")
pars <- c("sigma_p", "sigma_o", "beta", "xaa", "gamma_y")
# pars <- c("sigma_p", "sigma_o", "beta", "xaa")

x <- seq(0, 1, length.out = 100)
plot(x, dlnorm(x, log(0.2), 0.1), type = "l")

# init <- function() {
#   list(
#     sigma_o = rlnorm(1, log(0.2), 0.1),
#     sigma_p = rlnorm(1, log(0.2), 0.1),
#     beta = runif(1, 0.2, 0.6)
#   )
# }

stan_dat$sigma_o_prior <- c(log(0.2), 0.2)

mod <- stan("inst/stan/base.stan",
  iter = 1000,
  chains = 4,
  data = stan_dat,
  pars = pars
  # init = init,
  # control = list(adapt_delta = 0.99, max_treedepth = 20)
)
print(mod, pars = pars[1:3])

## Look at shinystan output
## install.packages("shinystan")
# require(shinystan)
# shinmod <- as.shinystan(mod)
# launch_shinystan(shinmod)

par(mfrow = c(1, 3))
post <- extract(mod)
hist(post$sigma_p);abline(v = 0.2)
hist(post$sigma_o, xlim = range(c(0.1, post$sigma_o)));abline(v = 0.2)
hist(post$beta);abline(v = 0.3)

post_xaa <- tidybayes::gather_draws(mod, xaa[i,y])
post_gamma_y <- tidybayes::gather_draws(mod, gamma_y[y])

xaa <- dat$xaa %>% reshape2::melt() %>%
  rename(i = Var1, y = Var2, .value = value) %>%
  as_tibble()

post_xaa %>%
  group_by(y, i) %>%
  summarise(
    lwr = quantile(.value, 0.025),
    upr = quantile(.value, 0.975),
    med = median(.value)) %>%
  ggplot(aes(y, med, ymin = lwr, ymax = upr)) +
  facet_wrap(~i) +
  geom_line(alpha = 1) +
  geom_ribbon(alpha = 0.5) +
  geom_line(data = xaa, colour = "red",
    mapping = aes(y, .value), inherit.aes = FALSE)

gamma_y <- data.frame(y = seq_along(dat$gamma_y) - 1, med = dat$gamma_y)
gamma_y <- gamma_y[-1,]

post_gamma_y %>%
  group_by(y) %>%
  summarise(
    lwr = quantile(.value, 0.025),
    upr = quantile(.value, 0.975),
    med = median(.value)) %>%
  ggplot(aes(y, med, ymin = lwr, ymax = upr)) +
  geom_line(alpha = 1) +
  geom_ribbon(alpha = 0.5) +
  geom_line(data = gamma_y, colour = "red",
    mapping = aes(y, med), inherit.aes = FALSE)
