# #' fit_stan_data
# #' A function that takes the input data and feeds it into the Stan sarla model
# #'
# #' @param input_data the stan data to fit
# #' @param ... Other arguments to pass to [plot_and_fill_data()].
# #' @export
# fit_stan_data <- function(input_data, ...) {
#   realdat <- list()
#   realdat$xaa_observed <- realdat$laa_observed <- input_data
#   realdat$Nages <- nrow(realdat$xaa_observed)
#   realdat$Nyears <- ncol(realdat$xaa_observed)
#   realdat$Ncohorts <- realdat$Nages + realdat$Nyears - 1
#   stan_dat <- plot_and_fill_data(realdat, ...)
#
#   stan_mod_file <- system.file("inst", "stan", "base.stan", package = "sarla")
#
#   mod <- cmdstan_model(stan_mod_file)
#   fit <- mod$sample(
#     data = stan_dat,
#     # seed = 2,
#     chains = 4,
#     # https://discourse.mc-stan.org/t/scale-parameter-is-0-but-must-be-0-can-i-do-anything-to-deal-with-this/19453/5
#     step_size = 0.1,
#     iter_sampling = 1500,
#     iter_warmup = 1500,
#     parallel_chains = 4,
#     adapt_delta = 0.98,
#     max_treedepth = 15
#   )
# }
#
