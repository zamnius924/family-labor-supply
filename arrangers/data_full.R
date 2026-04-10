# Добавление данных партнера ----------------------------------------------
# Получаем выборку на уровне д/х
data <- data_ind %>% 
  partner_merge(., data_source) %>% # добавляем данные партнера
  partner_df(.) %>% # чистим данные партнеров
  partner_id() %>% # создаем уникальный id пары
  select( # Отбираем необходимые метрики
    id_hh, id_ind, id_part, year, sex, sex_part, 
    sum_working, sum_working_part, wage, wage_part,
    earn, earn_part, consump_nd, lab_sup_year, lab_sup_year_part, 
    dlwage, dlwage_part, dlearn, dlearn_part, dlcons, 
    dlhours, dlhours_part, age, age_part, diplom_lev, diplom_lev_part,
    region, status, fed_min_real, nfm, child_lit, child_teen, 
    work_age_share, grp_def
  )

data <- data %>% subset(year %in% first_year:last_year)