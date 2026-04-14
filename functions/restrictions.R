# Функции для основных ограничений на выборку -----------------------------
restriction_year <- function(df, year_min, year_max) {
  # Ограничение выборки по годам
  propper_ind <- df %>% subset(year >= year_min & year <= year_max)
  return(propper_ind)
}


restriction_repr <- function(df, codes, year) {
  # Ограничение на репрезентативность в базовом году year
  propper_ind <- codes %>% subset(year == year & origsm == 1)
  repr_data <- df %>% subset(id_ind %in% propper_ind$id_ind)
  return(repr_data)
}


restriction_age <- function(df, age_min, age_max) {
  # Ограничение по возрасту
  propper_ind <- df %>% subset(age >= age_min & age <= age_max)
  return(propper_ind)
}


restrict_sample <- function(df, codes, restrict_repr, restrict_repr_year, year_min,
  year_max, age_min, age_max) {
  # Агрегированные ограничения: на год, на репрезентативность, на возраст
  if(restrict_repr == TRUE) {
    restricted_data <- df
    restricted_data <- restriction_year(restricted_data, year_min, year_max)
    restricted_data <- restriction_repr(restricted_data, codes, restrict_repr_year)
    restricted_data <- restriction_age(restricted_data, age_min, age_max)
  } else {
    restricted_data <- df
    restricted_data <- restriction_year(restricted_data, year_min, year_max)
    restricted_data <- restriction_age(restricted_data, age_min, age_max)
  }
  return(restricted_data)
}