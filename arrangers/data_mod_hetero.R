# ============================================================================
# data_mod_hetero.R
# ----------------------------------------------------------------------------
# Augment data_mod with education categories for both spouses.
# Produces 'data_mod_hetero'.
# ============================================================================

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
      diplom_lev == 4 ~ 1, # 1 = higher education
      diplom_lev != 4 ~ 2  # 2 = no higher education
    ),  
    educF = case_when(
      diplom_lev_part == 4 ~ 1, # 1 = higher education
      diplom_lev_part != 4 ~ 2  # 2 = no higher education
    )
  )

data_mod_hetero <- data_mod_hetero %>% 
  mutate(
    educ = case_when(
      educM == 1 & educF == 1 ~ 1, # both higher
      educM == 1 & educF == 2 ~ 2, # only male higher
      educM == 2 & educF == 1 ~ 3, # only female higher
      educM == 2 & educF == 2 ~ 4  # neither higher
    )
  )