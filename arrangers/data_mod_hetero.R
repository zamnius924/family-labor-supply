data_mod_hetero <- data_mod %>% 
  left_join(
    data %>% 
      ungroup() %>% 
      select(
        id_hh, year, diplom_lev, diplom_lev_part,
        earnM = earn, earnF = earn_part
      )
  ) %>% 
  mutate(
    educM = case_when(
      diplom_lev == 4 ~ 1, # 1 -- есть вышее
      diplom_lev != 4 ~ 2  # 2 -- нет высшего
    ),  
    educF = case_when(
      diplom_lev_part == 4 ~ 1, # 1 -- есть вышее
      diplom_lev_part != 4 ~ 2  # 2 -- нет высшего
    )
  )

data_mod_hetero <- data_mod_hetero %>% 
  mutate(
    educ = case_when(
      educM == 1 & educF == 1 ~ 1, # М -- есть высшее, Ж -- есть высшее
      educM == 1 & educF == 2 ~ 2, # М -- есть высшее, Ж -- нет высшего
      educM == 2 & educF == 1 ~ 3, # М -- нет высшего, Ж -- есть высшее
      educM == 2 & educF == 2 ~ 4  # М -- нет высшего, Ж -- нет высшего
    )
  )