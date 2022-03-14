#' Fit a SARLA model
#'
#' @param data Stan data
#' @param chains Chains
#' @param iter Iterations
#' @param adapt_delta Adapt delta argument for sampling
#' @param ... Other arguments to pass to [cmdstanr::cmdstan_model()]. See the `$sampling()` method link from [cmdstanr::CmdStanModel()].
#'
#' @export
fit_sarla <- function(data, chains = 4, iter = 2000, adapt_delta = 0.95, ...) {
  mod <- cmdstanr::cmdstan_model(
    system.file("stan", "sarla.stan", package = "sarla")
  )
  fit <- mod$sample(
    data = data,
    chains = chains,
    adapt_delta = adapt_delta,
    ...
  )
  fit
}
