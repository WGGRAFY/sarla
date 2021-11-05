#' Plot the simulated data object and use it to populate the stan input list.
#'
#'@param dat is simulated data object
#'@param year_effects scalar, 1 if year effects are estimated
#'@param cohort_effects scalar, 1 if cohort effects are estimated
#'@param init_effects scalar, 1 if initial size effects are estimated
#'@return stan_dat a list containing needed Stan slots
#'@export
plot_and_fill_data <- function(dat, year_effects = 1L,
                               cohort_effects = 0L,
                               init_effects = 1L){

  #Plot the observed data
  par(mfrow = c(1, 1))
  matplot(dat$xaa_observed, type = "l", lty = 1)
  matplot(dat$laa_observed, type = "l", lty = 1)

  #Plot observed deviations from mean by age and year
  dat$xaa %>%
    reshape2::melt(varnames=c("age", "year")) %>%
    ggplot(aes(year, age, fill = value)) +
    geom_tile() +
    scale_fill_gradient2()

  #populate stan data
  stan_dat <- list()
  stan_dat$laa <- dat$laa_observed
  stan_dat$Nages <- dat$Nages
  stan_dat$Nyears <- dat$Nyears
  stan_dat$Ncohorts <- dat$Ncohorts
  stan_dat$cohort_id <- make_cohort_ids(
    Nages = stan_dat$Nages,
    Nyears = stan_dat$Ncohorts
  )
  stan_dat$cohort_id[is.na(stan_dat$cohort_id)] <- 999L # magic number

  stan_dat$sigma_o_prior <- c(log(0.2), 0.5)

  stan_dat$est_year_effects <- year_effects
  stan_dat$est_cohort_effects <- cohort_effects
  stan_dat$est_init_effects <- init_effects

  stan_dat$N_gamma_y <- stan_dat$Ncohorts
  stan_dat$N_eta_c <- stan_dat$Ncohorts
  stan_dat$N_delta_c <- 0L

  stan_dat$n_proc_error <- stan_dat$Ncohorts*stan_dat$Nages

  return(stan_dat)

}
