# library(sarla)
devtools::load_all()
library(dplyr)
library(ggplot2)

set.seed(999)
dat <- sarla_sim(
  Nages = 7, Nyears = 22, sigma_o = 0.2, beta = 0.7,
  gamma_y_sd = 0.2, delta_c_sd = 0, eta_c_sd = 0.2, sigma_p_X0 = 0
)

stan_dat <- plot_and_fill_data(dat, plot = T)
names(stan_dat)[1] <- "laa_obs"

fit <- fit_sarla(stan_dat,
  chains = 1,
  iter = 100
)

# Look at fitted model
fit$summary(variables = c("sigma_p", "sigma_o", "beta", "xaa[1,10]", "xaa[2,10]"))
fit$cmdstan_diagnose()

post <- posterior::as_draws_df(fit$draws())
pars <- names(post)
pars <- pars[!grepl("raw", pars)]
pars_main <- pars[unique(c(
  grep("_sd", pars),
  grep("sigma_", pars), grep("beta", pars), grep("sigma_", pars)
))]

bayesplot::mcmc_areas_ridges(fit$draws(pars_main))
bayesplot::mcmc_trace(fit$draws(pars_main))
bayesplot::mcmc_pairs(fit$draws(pars_main), off_diag_fun = "hex")

# stanfit <- rstan::read_stan_csv(fit$output_files())
# pairs(stanfit, pars = c("sigma_p", "sigma_o", "beta", "xaa[2,9]"))

# helper until all on CRAN and updated:
tidy_draws.CmdStanMCMC <- function(model, ...) {
  posterior::as_draws_df(model$draws())
}
post_xaa <- tidybayes::gather_draws(fit, xaa[i, y]) %>%
  rename(age = i, year = y)

xaa <- dat$xaa %>%
  reshape2::melt() %>%
  rename(age = Var1, year = Var2, .value = value) %>%
  as_tibble()

# seems like a useful summary to save
quantile_summary <- post_xaa %>%
  # filter(y >= stan_dat$Nages) %>%
  # mutate(y = y - stan_dat$Nages + 1) %>%
  group_by(year, age) %>%
  summarise(
    lwr = quantile(.value, 0.025),
    upr = quantile(.value, 0.975),
    med = median(.value)
  )

legend_def <- c("data" = "red", "median" = "black", "95 quantile" = "gray")
quantile_summary %>%
  filter(!(lwr == 0 & upr == 0)) %>%
  ggplot(aes(age, med, ymin = lwr, ymax = upr)) +
  facet_wrap(~year) +
  geom_line(alpha = 1, aes(colour = "red")) +
  geom_ribbon(alpha = 0.5) +
  geom_line(
    data = xaa, colour = "red",
    mapping = aes(age, .value), inherit.aes = FALSE
  ) +
  theme_minimal() +
  scale_colour_manual(values = legend_def) +
  theme(
    legend.position = c(.90, .05),
    legend.key.size = unit(0.1, "cm"),
    legend.title = element_text(size = "6"),
    legend.text = element_text(size = "6")
  )

if (stan_dat$est_cohort_effects) {
  post_delta_c <- tidybayes::gather_draws(fit, delta_c[y])
  delta_hat <- post_delta_c %>%
    group_by(y) %>%
    summarise(med = median(.value))
  plot(delta_hat$med, dat$delta_c)
  abline(a = 0, b = 1)
}

if (stan_dat$est_init_effects) {
  post_eta_c <- tidybayes::gather_draws(fit, eta_c[y])
  eta_hat <- post_eta_c %>%
    group_by(y) %>%
    summarise(med = median(.value))
  eta_lwr <- post_eta_c %>%
    group_by(y) %>%
    summarise(lwr = quantile(.value, 0.025))
  eta_upr <- post_eta_c %>%
    group_by(y) %>%
    summarise(upr = quantile(.value, 0.975))
  plot(dat$eta_c, eta_hat$med,
    xlab = "Fitted", ylab = "Actual",
    pch = 19, main = "Estimated cohort effect"
  )
  segments(dat$eta_c, eta_lwr$lwr, dat$eta_c, eta_upr$upr)
  abline(a = 0, b = 1)
}

if (stan_dat$est_year_effects) {
  post_gamma_y <- tidybayes::gather_draws(fit, gamma_y[y])
  gamma_hat <- post_gamma_y %>%
    group_by(y) %>%
    summarise(med = median(.value))
  plot(gamma_hat$med, dat$gamma_y,
    xlab = "Fitted", ylab = "Actual",
    pch = 19, main = "Estimated year effect"
  )
  abline(a = 0, b = 1)
}
