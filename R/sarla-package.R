#' The 'sarla' package.
#'
#' @description An autogressive state-space model in Stan that can estimate a
#' cohort, annual, or initial size effect on length-at-age data from
#' fish.
#'
#' @docType package
#' @name sarla-package
#' @aliases sarla
#' @import methods
#'
NULL

# from: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
# quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "age_years",
    "year",
    "count",
    "length_cm",
    "meanal",
    "meanl",
    "sdl"
  ))
}
