#' Fit a SARLA model
#'
#' @param data Stan data
#' @param chains Chains
#' @param iter Iterations
#' @param ... Other arguments to pass to [cmdstanr::cmdstan_model()]
#'
#' @export
fit_sarla <- function(data, chains = 4, iter = 2000, ...) {
  mod <- cmdstanr::cmdstan_model(
    system.file("stan", "sarla.stan", package = "sarla")
  )
  fit <- mod$sample(
    data = data,
    chains = chains,
    ...
  )
  fit
}
