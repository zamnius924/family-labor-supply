# ============================================================================
# restrictions.R
# ----------------------------------------------------------------------------
# Functions for applying sample restrictions: years, representativeness, age.
# ============================================================================

#' Restrict data to a given range of years
restriction_year <- function(df, year_min, year_max) {
  propper_ind <- df %>% subset(year >= year_min & year <= year_max)
  return(propper_ind)
}

#' Restrict to representative subsample in a base year
restriction_repr <- function(df, codes, year) {
  propper_ind <- codes %>% subset(year == year & origsm == 1)
  repr_data <- df %>% subset(id_ind %in% propper_ind$id_ind)
  return(repr_data)
}

#' Restrict by age
restriction_age <- function(df, age_min, age_max) {
  propper_ind <- df %>% subset(age >= age_min & age <= age_max)
  return(propper_ind)
}

#' Combined sample restriction
#' @param df input data frame
#' @param codes code table with 'origsm' variable
#' @param restrict_repr logical, whether to enforce representativeness
#' @param restrict_repr_year base year for representativeness
#' @param year_min, year_max, age_min, age_max bounds
restrict_sample <- function(df, codes, restrict_repr, restrict_repr_year,
                            year_min, year_max, age_min, age_max) {
  if (restrict_repr == TRUE) {
    restricted_data <- df
    restricted_data <- restriction_year(restricted_data, year_min, year_max)
    restricted_data <- restriction_repr(restricted_data, codes,
                                        restrict_repr_year)
    restricted_data <- restriction_age(restricted_data, age_min, age_max)
  } else {
    restricted_data <- df
    restricted_data <- restriction_year(restricted_data, year_min, year_max)
    restricted_data <- restriction_age(restricted_data, age_min, age_max)
  }
  return(restricted_data)
}