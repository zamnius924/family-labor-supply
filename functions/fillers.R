# Филлеры -----------------------------------------------------------------
filler_wage <- function(df) {
  # Заполним пропуски з/п в месяц данными за прошлый месяц по первой работе
  df <- df %>% 
    mutate(wage_month_filled = coalesce(wage_month, wage_last_month))
  
  ## Переменные з/п в месяц для второй и третьей работ
  # можно просто переименовать уже имеющиеся переменные в данных
  df <- df %>% 
    rename(
      wage_month_filled_2 = wage_last_month_2,
      wage_month_filled_3 = wage_last_month_3
    ) %>% 
    relocate(wage_month_filled_2, wage_month_filled_3, .after = last_col())
  
  return(df)
}


filler_lab_sup <- function(df) {
  # Заполним пропуски в часах в месяц
  df <- df %>%
    ## Отработанные часы в среднем за месяц
    mutate(
      # через отработанные часы в неделю (тут умножается на среднюю протяженность месяца в неделях)
      lab_sup_month_w_filled = lab_sup_week * days / 12 / 7,
      lab_sup_month_w_filled_2 = lab_sup_week_2 * days / 12 / 7,
      # через отработанные часы в день (22 рабочих дня в месяце)
      lab_sup_month_d_filled = lab_sup * 22,
      lab_sup_month_d_filled_2 = lab_sup_2 * 22
    ) %>% 

    ## Заполняем пропуски в month_w_filled:
    mutate(
      # 1) данными из lab_sup_last_month
      lab_sup_month_filled = coalesce(lab_sup_month_w_filled,
                                      lab_sup_last_month),
      lab_sup_month_filled_2 = coalesce(lab_sup_month_w_filled_2,
                                        lab_sup_last_month_2)
    ) %>% 
    mutate(
      # 2) данными из month_d_filled
      lab_sup_month_filled = coalesce(lab_sup_month_filled,
                                      lab_sup_month_d_filled),
      lab_sup_month_filled_2 = coalesce(lab_sup_month_filled_2,
                                        lab_sup_month_d_filled_2)
    ) %>%

    ## Теперь будем сравнивать полученные часы в месяц с часами за прошлый месяц
    mutate(
      lab_sup_dev = abs(lab_sup_month_filled - lab_sup_last_month),
      lab_sup_dev_2 = abs(lab_sup_month_filled_2 - lab_sup_last_month_2)
    ) %>%

    ## После сравнения мы будем проводить замену там, где отклонение больше 48 часов
    mutate(
      lab_sup_month_filled = case_when(
        lab_sup_dev >= 48 & is.na(lab_sup_dev) == FALSE ~ lab_sup_last_month,
        TRUE ~ lab_sup_month_filled
      ),
      lab_sup_month_filled_2 = case_when(
        lab_sup_dev_2 >= 48 & is.na(lab_sup_dev_2) == FALSE ~ lab_sup_last_month_2,
        TRUE ~ lab_sup_month_filled_2
      )
    ) %>% 

    ## Доделаем переменную для третьей работы
    mutate(
      lab_sup_month_filled_3 = case_when(
        regular == 2 ~ lab_sup_last_month_3,
        TRUE ~ NA_real_
      )
    ) %>%

    ## Удалим ненужные колонки
    select(-lab_sup_dev, -lab_sup_dev_2, 
           -lab_sup_month_w_filled, -lab_sup_month_w_filled_2,
           -lab_sup_month_d_filled, -lab_sup_month_d_filled_2)
  
  return(df)
}


corrector_lab_sup <- function(df) {
  df <- df %>% 
    ## Первая работа
    mutate(
      # 1) Не должно быть индивидов, которые working = 2, но предлагают труд или получают з/п
      lab_sup_month_filled = replace(lab_sup_month_filled, working == 2, NA),
      wage_month_filled = replace(wage_month_filled, working == 2, NA)
    ) %>% 
    mutate(
      # 2) Не должно быть работающих индивидов с нулевыми з/п и отработанными часами
      lab_sup_month_filled = replace(
        lab_sup_month_filled,
        working == 1 & lab_sup_month_filled == 0,
        NA
      ),
      wage_month_filled = replace(
        wage_month_filled,
        working == 1 & wage_month_filled == 0,
        NA
      )
    ) %>% 

    ## На второй работе есть два показателя working
    mutate(
      working_2 = replace(
        working_2.1,
        working_2.1 == 2 & working_2.2 == 1,
        1
      )
    ) %>% 
    mutate(
      # 1) не должно быть индивидов, которые working = 2, но предлагают труд или получают з/п
      lab_sup_month_filled_2 = replace(
        lab_sup_month_filled_2,
        working_2 == 2,
        NA
      ),
      wage_month_filled_2 = replace(
        wage_month_filled_2,
        working_2 == 2,
        NA
      )
    ) %>% 
    mutate(
      # 2) Не должно быть работающих индивидов с нулевыми з/п и отработанными часами
      lab_sup_month_filled_2 = replace(
        lab_sup_month_filled_2,
        working_2 == 1 & lab_sup_month_filled_2 == 0,
        NA
      ),
      wage_month_filled_2 = replace(
        wage_month_filled_2,
        working_2 == 1 & wage_month_filled_2 == 0,
        NA
      )
    ) %>% 

    ## Третья работа
    mutate(
      # 1) не должно быть индивидов, которые working = 2, но предлагают труд или получают з/п
      lab_sup_month_filled_3 = replace(
        lab_sup_month_filled_3,
        working_3 == 2,
        NA
      ),
      wage_month_filled_3 = replace(
        wage_month_filled_3,
        working_3 == 2,
        NA
      )
    ) %>%
    mutate(
      # 2) Не должно быть работающих индивидов с нулевыми з/п и отработанными часами
      lab_sup_month_filled_3 = replace(
        lab_sup_month_filled_3,
        working_3 == 1 & lab_sup_month_filled_3 == 0,
        NA
      ),
      wage_month_filled_3 = replace(
        wage_month_filled_3,
        working_3 == 1 & wage_month_filled_3 == 0,
        NA
      )
    ) %>% 

    ## Преобразование индикаторов работы
    mutate(
      # Перекодируем в формат 0/1
      working = abs(working - 2),
      working_2 = abs(working_2 - 2),
      working_3 = abs(working_3 - 2)
    ) %>% 
    relocate(working, working_2, working_3, .after = last_col())
  
  return(df)
}
