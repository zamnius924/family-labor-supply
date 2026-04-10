# Метрики труда -----------------------------------------------------------
### Создаем метрики предложения труда по трем работам
data_ind <- data_ind %>%
  filler_wage(.) %>% # три з/п
  filler_lab_sup(.) %>% # три отработанных часов
  corrector_lab_sup(.) # три индикатора работы + правки в созданных переменных


### Создаем агрегированные метрики предложения труда
## Номинальная часовая ставка з/п
data_ind <- data_ind %>%
  mutate(
    # Номинальная часовая ставка з/п по каждой работе 
    wage_1 = wage_month_filled / lab_sup_month_filled,
    wage_2 = wage_month_filled_2 / lab_sup_month_filled_2,
    wage_3 = wage_month_filled_3 / lab_sup_month_filled_3
  )

data_ind$wage <- data_ind %>% 
  select(wage_1, wage_2, wage_3) %>%
  apply(1, sum, na.rm = TRUE) %>% 
  na_if(0)

## Отработанные часы в месяц
data_ind$sum_lab_sup_month <- data_ind %>% 
  select(
    lab_sup_month_filled, lab_sup_month_filled_2, lab_sup_month_filled_3
  ) %>%
  apply(1, sum, na.rm = TRUE) %>% 
  na_if(0)

## Агрегированный индикатор работы
data_ind$sum_working <- data_ind %>% 
  select(working, working_2, working_3) %>%
  apply(1, sum, na.rm = TRUE) %>% 
  as.logical() %>% 
  as.numeric()

## Правки в агрегированные метрики
data_ind <- data_ind %>% 
  mutate(
    # Не должно быть индивидов, которые sum_working = 0, но предлагают труд или
    # получают з/п
    sum_lab_sup_month = replace(sum_lab_sup_month, sum_working == 0, NA),
    wage = replace(wage, sum_working == 0, NA)
  )

## Отработанные часы в год (с учетом отпусков и выходных)
data_ind <- data_ind %>%
  mutate(vacation = as.numeric(vacation)) %>% 
  tidyr::replace_na(list(vacation = 0)) %>% # убираем пропуски в отпусках
  mutate(
    lab_sup_year = sum_lab_sup_month * 12 - (vacation + holidays) * sum_lab_sup_month / 22
  ) %>% 
  mutate(lab_sup_year = replace(lab_sup_year, lab_sup_year < 0, NA)) %>%  # убираем тех, у кого часы отрицательные (в основном женщины в декрете)
  relocate(sum_working, lab_sup_year, wage, .after = last_col())




# Дополнительные метрики --------------------------------------------------
## Дефлируем на региональную инфляцию
data_ind <- data_ind %>% 
  mutate(
    wage = wage / CPI_reg * 100,
    consump_nd = consump_nd / CPI_reg * 100,
    fed_min_real = fed_min / CPI_reg * 100,
    grp_def = grp / CPI_reg * 100
  )

## Количество трудоспособных и нетрудоспособных членов семьи
data_ind <- data_ind %>% 
  mutate(
    work_age = work_age_f + work_age_m, # трудоспособные
    nwork_age = nwork_age_f + nwork_age_m # нетрудоспособные
  )

## Уровень образования
data_ind <- data_ind %>% 
  mutate(
    diplom_lev = case_when( # уровень образования ind
      diplom %in% c(1, 2, 3) ~ 1,
      diplom %in% c(4) ~ 2,
      diplom %in% c(5) ~ 3,
      diplom %in% c(6) ~ 4
    )
  )

## Годой реальный трудовой доход
data_ind$earn <- data_ind$wage * data_ind$lab_sup_year

## Доля работающих членов д/х
data_ind <- data_ind %>% 
  mutate(work_age_share = work_age / nfm)