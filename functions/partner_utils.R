# Скрепление данных партнеров ---------------------------------------------
partner_merge <- function(df, df_source) {
  df_merged <- df %>%
    left_join( # присоединяем индексы партнеров
      df_source$code_ind %>% select(id_ind, year, id_part)
    ) 

  df_merged <- df_merged %>% 
    left_join( # присоединяем характеристики партнеров
      df %>% select(
        id_part = id_ind, id_h, year, sex_part = sex, 
        lab_sup_year_part = lab_sup_year, wage_part = wage,
        sum_working_part = sum_working, earn_part = earn, 
        diplom_lev_part = diplom_lev, age_part = age,
        dlwage_part = dlwage, dlearn_part = dlearn, dlhours_part = dlhours
      )
    )

  return(df_merged)
}


partner_df <- function(df) {
  # Строим df относительно домохозяйства, состоящего из двух супругов
  df <- df %>%
    subset(sex == 1) %>% # строим относительно мужчин
    drop_na(id_part, sex_part) # удаляем, если нет данных по партнеру 
  # пол, потому что у некоторых индивидов есть индикатор, но нет данных
  
  return(df)
}


partner_id <- function(df) {
  # Создает индикатор уникальных пар
  df$couples <- df %>%
    select(id_ind, id_part) %>%
    apply(1, function(x) paste(sort(x), collapse = "; ")) # сшиваем id_ind и id_part
  
  couples <- data.frame(couples = unique(df$couples)) %>%
    mutate(id_hh = row_number()) # нумеруем уникальные пары
  
  df <- df %>%
    left_join(couples) %>% # пришиваем к исходным данным индикатор д/х
    select(id_hh, everything(), -couples) 
  
  return(df)
}