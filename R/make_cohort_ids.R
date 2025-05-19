#' Makes a matrix populated with an id corresponding to the cohort of the
#' observation.
#'
#' @param Nages integer, number of ages
#' @param Nyears integer, number of years
#' @return a matrix with dimensions Nages x Nyears with the cohort ids.
#' @export
make_cohort_ids <- function(Nages, Nyears) {
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
  m <- m - min(m, na.rm = TRUE) + 1
}
