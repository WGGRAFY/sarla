#' Run the stan model
#'
#' @param input_data double setting process error sd. 0.2 by default.
#' @param ... additional arguments to \code{\link{stan}}
#'@return returns the stan output
#'@export
stan_base <- function(input_data, ...){ 

  out <- rstan::sampling(sarla:::stanmodels$base, data = input_data, ...)
    return(out)
}
