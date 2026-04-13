data_mod <- expand.grid( # создаем сетку, чтобы правильно крепить лаги приростов
  id_hh = unique(data$id_hh), year = first_year:last_year
) %>% 
  left_join(
    data %>% 
      select(
        id_hh, id_ind, id_part, year, ageM = age, ageF = age_part,
        dwM = dw, dwF = dw_part, dc, dyM = dy, dyF = dy_part
      )
  ) %>%
  arrange(id_hh, year) %>% 
  group_by(id_hh) %>%
  mutate(
    # Генерируем лаги приростов переменных интереса
    dLwM = dplyr::lag(dwM),
    dLwF = dplyr::lag(dwF),
    dLc  = dplyr::lag(dc),
    dLyM = dplyr::lag(dyM),
    dLyF = dplyr::lag(dyF)
  ) %>% 
  mutate(
    # Генерируем возрастные группы обоих партнеров
    cohortM = case_when(
      ageM %in% 25:29 ~ 1,
      ageM %in% 30:34 ~ 2,
      ageM %in% 35:39 ~ 3,
      ageM %in% 40:44 ~ 4,
      ageM %in% 45:49 ~ 5,
      ageM %in% 50:55 ~ 6
    ),
    cohortF = case_when(
      ageF %in% 25:29 ~ 1,
      ageF %in% 30:34 ~ 2,
      ageF %in% 35:39 ~ 3,
      ageF %in% 40:44 ~ 4,
      ageF %in% 45:49 ~ 5,
      ageF %in% 50:55 ~ 6
    )
  )