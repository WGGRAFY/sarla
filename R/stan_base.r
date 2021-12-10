#' Run the stan model
#'
#' @param input_data double setting process error sd. 0.2 by default.
#' @param ... additional arguments to \code{\link{stan}}
#'@return returns the stan output
#'@export
stan_base <- function(input_data, ...){ 

  realdat <- list()
  realdat$xaa_observed <- realdat$laa_observed <-input_data
  realdat$Nages <- nrow(realdat$xaa_observed)
  realdat$Nyears <- ncol(realdat$xaa_observed)
  realdat$Ncohorts <- realdat$Nages + realdat$Nyears -1
  stan_dat <- plot_and_fill_data(realdat, ...)

  out <- rstan::sampling(stanmodels$base, data = stan_dat, ...)
    return(out)
}