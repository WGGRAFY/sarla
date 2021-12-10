#' Simulate a random walk with trends
#'
#' @param sigma_p double setting process error sd. 0.2 by default.
#' @param sigma_o double setting observation error sd. 0.1 by default.
#' @param Nages integer setting the number of ages. 7 by default.
#' @param Nyears integer setting the number of years. 22 by default.
#' @param beta double setting the degree of autocorrelation. 0.5 by default.
#' @param sigma_p_X0 double setting the sd of variation in starting values.
#'   Defaults to equivalent to \code{sigma_p}
#' @param gamma_y_sd double setting the sd of year variation. Default is 0.
#' @param delta_c_sd double setting the sd of cohort effect variation. Defaults
#'   to 0.
#' @param eta_c_sd double setting the sd of the initial size cohort effect.
#'   Defaults to 0.
#' @return list of simulated data
#' @export
sarla_sim <- function(sigma_p = 0.2, sigma_o = 0.1,
                Nages = 7, Nyears = 22,
                beta = 0.5,
                sigma_p_X0 = sigma_p,
                gamma_y_sd = 0, delta_c_sd = 0, eta_c_sd = 0) {
  Ncohorts <- Nages + Nyears - 1
  # note, includes values before time 0, hence Ncohorts:
  xaa <- matrix(nrow = Nages, ncol = Ncohorts)
  laa <- matrix(nrow = Nages, ncol = Ncohorts)
  gamma_y <- rnorm(Ncohorts, 0, gamma_y_sd) # note, includes values before time 0, hence Ncohorts
  delta_c <- rnorm(Ncohorts, 0, delta_c_sd)
  eta_c <- rnorm(Ncohorts, 0, eta_c_sd)
  X0 <- rnorm(1, 0, sigma_p_X0)
  cohort_ids <- make_cohort_ids(Nages = Nages, Nyears = Nyears + Nages - 1)

  # process noise:
  for (y in 1:Ncohorts) {
    xaa[1, y] <- X0 # 1st year shared
    xaa[1, y] <- xaa[1, y] + eta_c[y] # 1st year cohort effect
  }

  for (i in 2:Nages) {
    for (y in 2:Ncohorts) {
      if (!is.na(xaa[i - 1, y - 1])) {
        xaa[i, y] <- rnorm(1, beta * xaa[i - 1, y - 1] + delta_c[cohort_ids[i, y]], sigma_p)
      }
    }
  }

  # year effects:
  for (y in 1:Ncohorts) {
    xaa[, y] <- xaa[, y] + gamma_y[y]
  }

  # observation noise:
  for (i in 1:Nages) {
    for (y in 1:Ncohorts) {
      if (!is.na(xaa[i, y])) {
        laa[i, y] <- rnorm(1, xaa[i, y], sigma_o)
      }
    }
  }
  list(
    gamma_y = gamma_y, delta_c = delta_c, eta_c = eta_c,
    xaa = xaa, laa = laa, cohort_ids = cohort_ids,
    xaa_observed = xaa[, -c(1:(Nages - 1))],
    laa_observed = laa[, -c(1:(Nages - 1))],
    X0 = X0,
    Nages = Nages, Nyears = Nyears, Ncohorts = Ncohorts
  )
}
