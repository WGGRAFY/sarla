library(rstan)
options(mc.cores = parallel::detectCores())
library(dplyr)
library(ggplot2)

sim <- function(sigma_p = 0.2, sigma_o = 0.2, Nages = 7,
  Nyears = 22, beta = 0.5, gamma_y_sd = 0.2, delta_c_sd = 0.3) {

  xaa <- matrix(nrow = Nages, ncol = Nyears)
  laa <- matrix(nrow = Nages, ncol = Nyears)
  Ncohorts <- Nages + Nyears - 1
  gamma_y <- rnorm(Nyears, 0, gamma_y_sd)
  delta_c <- rnorm(Ncohorts, 0, delta_c_sd)

  # process noise:
  for (i in 1:Nages) {
    xaa[i, 1] <- rnorm(1, 0, sigma_p)
  }
  for (y in 1:Nyears) {
    xaa[1, y] <- rnorm(1, 0, sigma_p)
  }
  for (i in 2:Nages) {
    for(y in 2:Nyears){
      xaa[i,y] <- rnorm(1, beta*xaa[i-1, y-1], sigma_p)
    }
  }

  # year effects:
  for (y in 1:Nyears) {
    xaa[,y] <- xaa[,y] + gamma_y[y]
  }

  cohort_ids <- make_cohort_ids(Nages = Nages, Nyears = Nyears)

  # cohort effects:
  for (i in 1:Nages) {
    for(y in 1:Nyears){
      xaa[i, y] <- xaa[i, y] + delta_c[cohort_ids[i, y]]
    }
  }

  # observation noise:
  for (i in 1:Nages) {
    for(y in 1:Nyears){
      laa[i, y] <- rnorm(1, xaa[i, y], sigma_o)
    }
  }
  list(gamma_y = gamma_y, delta_c = delta_c,
    xaa = xaa, laa = laa, cohort_ids = cohort_ids)
}

make_cohort_ids <- function(Nages, Nyears) {
  # this was fun to figure out
  m <- matrix(NA, nrow = Nages, ncol = Nyears)
  Ncohorts <- Nages + Nyears - 1
  for (j in 1:Ncohorts) {
    for (i in 1:Nages) {
      for (y in 1:Nages) {
        y_index <- y + j - Nages
        i_index <- y
        if (i_index >= 1 && i_index <= Nages &&
            y_index >= 1 && y_index <= Nyears) {
          m[i_index, y_index] <- j
        }
      }
    }
  }
  m
}

set.seed(111)
dat <- sim(Nages = 7, Nyears = 22)
par(mfrow = c(1, 1))
matplot(t(dat$xaa), type = "l", lty = 1)
matplot(t(dat$laa), type = "l", lty = 1)

dat$xaa %>% reshape2::melt() %>%
  ggplot(aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2()

stan_dat <- list()
stan_dat$laa <- dat$laa
stan_dat$Nages <- 7
stan_dat$Nyears <- 22
stan_dat$Ncohorts <- 22 + 7 - 1
stan_dat$cohort_id <- make_cohort_ids(7, 22)

pars <- c("sigma_p", "sigma_o", "beta", "xaa", "gamma_y", "delta_c")

init <- function() {
  list(
    sigma_o = rlnorm(1, log(0.2), 0.1),
    sigma_p = rlnorm(1, log(0.2), 0.1),
    beta = runif(1, 0.2, 0.6),
    delta_c = rep(0, stan_dat$Ncohorts),
    gamma_y = rnorm(stan_dat$Nyears, 0, 0.1),
    delta_c_sd = runif(1, 0.1, 0.3),
    gamma_y_sd = runif(1, 0.1, 0.3)
  )
}

x <- seq(0, 1, length.out = 300)
plot(x, dlnorm(x, log(0.2), 0.1), type = "l")
stan_dat$sigma_o_prior <- c(log(0.2), 0.1)

mod <- stan("inst/stan/base.stan",
  iter = 800,
  chains = 6,
  data = stan_dat,
  pars = pars,
  init = init,
  control = list(adapt_delta = 0.95)
)
print(mod, pars = pars[c(1:3)])

par(mfrow = c(1, 3))
post <- extract(mod)
hist(post$sigma_p);abline(v = 0.2)
hist(post$sigma_o, xlim = range(c(0.1, post$sigma_o)));abline(v = 0.2)
hist(post$beta);abline(v = 0.3)

post_xaa <- tidybayes::gather_draws(mod, xaa[i,y])
post_gamma_y <- tidybayes::gather_draws(mod, gamma_y[y])
post_delta_c <- tidybayes::gather_draws(mod, delta_c[.c])

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

gamma_y <- data.frame(y = seq_along(dat$gamma_y), med = dat$gamma_y)

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

delta_c <- data.frame(.c = seq_along(dat$delta_c), med = dat$delta_c)

post_delta_c %>%
  group_by(.c) %>%
  summarise(
    lwr = quantile(.value, 0.025),
    upr = quantile(.value, 0.975),
    med = median(.value)) %>%
  ggplot(aes(.c, med, ymin = lwr, ymax = upr)) +
  geom_line(alpha = 1) +
  geom_ribbon(alpha = 0.5) +
  geom_line(data = delta_c, colour = "red",
    mapping = aes(.c, med), inherit.aes = FALSE)
