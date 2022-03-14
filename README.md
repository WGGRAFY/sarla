# sarla
State space autoregressive length-at-age model
---

## installing in `R`
You can install the `sarla` package from Github. The main branch right now works with a full matrix of data while we are adding support for missing data on the `missingdata` branch.

```{r}
remotes::install_github("WGGRAFY/sarla")
require(sarla)
```

## testing with simulated data
You can test with simulated data using the following code. Full code is in `inst`

```{r}
#simulate data
dat <- sarla_sim(
  Nages = 7, Nyears = 22, sigma_o = 0.2, beta = 0.7,
  gamma_y_sd = 0.2, delta_c_sd = 0, eta_c_sd = 0.2, sigma_p_X0 = 0
)

#create the dat model input object
stan_dat <- plot_and_fill_data(dat)

#Fit the model
fit <- fit_sarla(stan_dat,
  chains = 1,
  iter = 100
)
```

The `fit_sarla` function accepts any arguments that can be passed to `sample`. Documentation for `sample` [here](https://mc-stan.org/cmdstanr/reference/model-method-sample.html).
