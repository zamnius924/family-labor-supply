# Пока не понятно, нужны ли функции 2 последние функции

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
      lab_sup_month_filled = coalesce(lab_sup_month_w_filled, lab_sup_last_month),
      lab_sup_month_filled_2 = coalesce(lab_sup_month_w_filled_2, lab_sup_last_month_2)
    ) %>% 
    mutate(
      # 2) данными из month_d_filled
      lab_sup_month_filled = coalesce(lab_sup_month_filled, lab_sup_month_d_filled),
      lab_sup_month_filled_2 = coalesce(lab_sup_month_filled_2, lab_sup_month_d_filled_2)
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
        TRUE ~ lab_sup_month_filled),
      lab_sup_month_filled_2 = case_when(
        lab_sup_dev_2 >= 48 & is.na(lab_sup_dev_2) == FALSE ~ lab_sup_last_month_2,
        TRUE ~ lab_sup_month_filled_2)
    ) %>% 

    ## Доделаем переменную для третьей работы
    mutate(
      lab_sup_month_filled_3 = case_when(
        regular == 2 ~ lab_sup_last_month_3,
        TRUE ~ NA_real_)
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
      lab_sup_month_filled = replace(lab_sup_month_filled, working == 1 & lab_sup_month_filled == 0, NA),
      wage_month_filled = replace(wage_month_filled, working == 1 & wage_month_filled == 0, NA)
    ) %>% 

    ## На второй работе есть два показателя working
    mutate(
      working_2 = replace(working_2.1, working_2.1 == 2 & working_2.2 == 1, 1)
    ) %>% 
    mutate(
      # 1) не должно быть индивидов, которые working = 2, но предлагают труд или получают з/п
      lab_sup_month_filled_2 = replace(lab_sup_month_filled_2, working_2 == 2, NA),
      wage_month_filled_2 = replace(wage_month_filled_2, working_2 == 2, NA)
    ) %>% 
    mutate(
      # 2) Не должно быть работающих индивидов с нулевыми з/п и отработанными часами
      lab_sup_month_filled_2 = replace(lab_sup_month_filled_2, working_2 == 1 & lab_sup_month_filled_2 == 0, NA),
      wage_month_filled_2 = replace(wage_month_filled_2, working_2 == 1 & wage_month_filled_2 == 0, NA)
    ) %>% 

    ## Третья работа
    mutate(
      # 1) не должно быть индивидов, которые working = 2, но предлагают труд или получают з/п
      lab_sup_month_filled_3 = replace(lab_sup_month_filled_3, working_3 == 2, NA),
      wage_month_filled_3 = replace(wage_month_filled_3, working_3 == 2, NA)
    ) %>%
    mutate(
      # 2) Не должно быть работающих индивидов с нулевыми з/п и отработанными часами
      lab_sup_month_filled_3 = replace(lab_sup_month_filled_3, working_3 == 1 & lab_sup_month_filled_3 == 0, NA),
      wage_month_filled_3 = replace(wage_month_filled_3, working_3 == 1 & wage_month_filled_3 == 0, NA)
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




# Скрепление данных партнеров ---------------------------------------------
partner_merge <- function(df, df_source) {
  df_merged <- df %>%
    left_join( # присоединяем индексы партнеров
      df_source$code_ind %>% select(id_ind, year, id_part)
    ) 
  
  df_merged <- df_merged %>% 
    left_join( # присоединяем характеристики партнеров
      df %>% select(
        id_part = id_ind, id_h, year, sex_part = sex, lab_sup_year_part = lab_sup_year, wage_part = wage,
        sum_working_part = sum_working, earn_part = earn, diplom_lev_part = diplom_lev, age_part = age,
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




# Чистка данных -----------------------------------------------------------
omit_me <- function(df) {
  # Проводим очистку темпов прироста дохода, з/п и потребления от выбросов
  df_log <- expand.grid(id_ind = unique(df$id_ind), year = first_year:last_year) %>%
    left_join(
      df %>% select(id_ind, year, wage, earn, consump_nd, lab_sup_year)
    ) %>%
    arrange(id_ind, year) %>%
    mutate(
      lwage = log(wage), 
      learn = log(earn), 
      lcons = log(consump_nd), 
      lhours = log(lab_sup_year)
    ) %>%
    group_by(id_ind) %>% 
    mutate(
      # Создаем переменные "скачков"
      jw = (lwage - dplyr::lag(lwage)) * (dplyr::lead(lwage) - lwage),
      je = (learn - dplyr::lag(learn)) * (dplyr::lead(learn) - learn),
      jc = (lcons - dplyr::lag(lcons)) * (dplyr::lead(lcons) - lcons),
      jh = (lhours - dplyr::lag(lhours)) * (dplyr::lead(lhours) - lhours)
    )
  
  # Убираем значения переменных в уровнях, где скачки были большими
  df_log$wage[df_log$jw < quantile(df_log$jw, prob = 0.0025, na.rm = TRUE)] <- NA
  df_log$earn[df_log$je < quantile(df_log$je, prob = 0.0025, na.rm = TRUE)] <- NA
  df_log$consump_nd[df_log$jc < quantile(df_log$jc, prob = 0.0025, na.rm = TRUE)] <- NA
  df_log$lab_sup_year[df_log$jh < quantile(df_log$jh, prob = 0.0025, na.rm = TRUE)] <- NA

  df <- df %>%
    select(-wage, -earn, -consump_nd, -lab_sup_year) %>% 
    left_join(
      df_log %>% 
        select(id_ind, year, wage, earn, consump_nd, lab_sup_year)
    ) %>%
    mutate(
      lwage = log(wage),
      learn = log(earn),
      lcons = log(consump_nd),
      lhours = log(lab_sup_year)
    )
  
  return(df)
}


omit_outlier <- function(df, g_wage_res, g_consump_res) {
  # Накладывает ограничения на темпы роста з/п и потребления
  df_expanded <- expand.grid( # создаем сетку по всем индивидам и всем годам
    id_ind = unique(df$id_ind), year = first_year:last_year
    ) %>%
    left_join( # наполняем сетку данными о переменных интереса + создаем индикатор присутствия в выборке
      df %>% 
        select(id_ind, year, consump_nd, wage, lab_sup_year, earn) %>% 
        mutate(present = 1)
    ) %>%
    arrange(id_ind, year) %>%
    group_by(id_ind) %>% 
    mutate( # создаем темпы роста переменных интереса
      g_consump = consump_nd / dplyr::lag(consump_nd),
      g_wage = wage / dplyr::lag(wage),
      g_lab_sup_year = lab_sup_year / dplyr::lag(lab_sup_year),
      g_earn = earn / dplyr::lag(earn)
    ) %>% 
    # накладываем ограничения на темпы роста
    subset(g_wage <= g_wage_res | is.na(g_wage)) %>% # з/п
    subset(g_consump <= g_consump_res | is.na(g_consump)) # потребление
  
  df_new <- df_expanded %>% 
    subset(present == 1) %>% # в расширенном df оставляем тех, кто был изначально в выборке
    select(id_ind, year, wage, consump_nd) %>%  # оставляем только обновленные данные по метрикам интереса
    left_join(df %>% select(-wage, -consump_nd)) # крепим исходные данные
  
  return(df_new)
}




# Создание метрик интереса ------------------------------------------------
metrics_diff <- function(df) {
  # Создаем датафрейм с темпами прироста метрик интереса
  df_new <- expand.grid(id_ind = unique(df$id_ind), year = first_year:last_year) %>%
    left_join(
      df %>% select(id_ind, year, lwage, learn, lcons, lhours)
    ) %>% 
    arrange(id_ind, year) %>% 
    group_by(id_ind) %>% 
    mutate(
      dlwage = lwage - dplyr::lag(lwage),
      dlearn = learn - dplyr::lag(learn),
      dlcons = lcons - dplyr::lag(lcons),
      dlhours = lhours - dplyr::lag(lhours)
    ) %>% 
    select(id_ind, year, dlwage, dlearn, dlcons, dlhours)
  
  # Прикрепляем полученные метрики к исходному датафрейму
  df <- df %>% 
    left_join(df_new, by = c("id_ind", "year"))
  
  return(df)
}




# Обработка результатов первого шага оценивания ---------------------------
results_first_stage_diff <- function(df, model_wM, model_wF, model_c, model_yM, model_yF, model_hM, model_hF) {
  # Для моделей, оцененных в РАЗНОСТЯХ
  res_w_male <- expand.grid(
    id_ind = unique(as.numeric(as.character(index(model_wM)[,1]))),
    year = first_year:last_year
    ) %>% # делаем сетку: для каждого i есть все значения t
    left_join( # заполняем сетку остатками
      cbind(index(model_wM), resid(model_wM)) %>%
        lapply(FUN = function(x) {as.numeric(paste(x))}) %>%
        as.data.frame(.) %>%
        rename(dw = "resid.model_wM.")
    ) %>% # внутри сетки строим первые разности
    arrange(id_ind, year)
  
  res_w_female <- expand.grid(
    id_part = unique(as.numeric(as.character(index(model_wF)[,1]))),
    year = first_year:last_year
    ) %>%
    left_join(
      cbind(index(model_wF), resid(model_wF)) %>%
        lapply(FUN = function(x) {as.numeric(paste(x))}) %>%
        as.data.frame(.) %>%
        rename(dw_part = "resid.model_wF.")
    ) %>%
    arrange(id_part, year)
  
  res_c <- expand.grid(
    id_hh = unique(as.numeric(as.character(index(model_c)[,1]))),
    year = first_year:last_year
    ) %>%
    left_join(
      cbind(index(model_c), resid(model_c)) %>%
        lapply(FUN = function(x) {as.numeric(paste(x))}) %>%
        as.data.frame(.) %>%
        rename(dc = "resid.model_c.")
    ) %>%
    arrange(id_hh, year)
  
  res_e_male <- expand.grid(
    id_ind = unique(as.numeric(as.character(index(model_yM)[,1]))),
    year = first_year:last_year
    ) %>%
    left_join(
      cbind(index(model_yM), resid(model_yM)) %>%
        lapply(FUN = function(x) {as.numeric(paste(x))}) %>%
        as.data.frame(.) %>%
        rename(dy = "resid.model_yM.")
    ) %>%
    arrange(id_ind, year)
  
  res_e_female <- expand.grid(
    id_part = unique(as.numeric(as.character(index(model_yF)[,1]))),
    year = first_year:last_year
    ) %>%
    left_join(
      cbind(index(model_yF), resid(model_yF)) %>%
        lapply(FUN = function(x) {as.numeric(paste(x))}) %>%
        as.data.frame(.) %>%
        rename(dy_part = "resid.model_yF.")
    ) %>%
    arrange(id_part, year)
  
  res_h_male <- expand.grid(
    id_ind = unique(as.numeric(as.character(index(model_hM)[,1]))),
    year = first_year:last_year
    ) %>%
    left_join(
      cbind(index(model_hM), resid(model_hM)) %>%
        lapply(FUN = function(x) {as.numeric(paste(x))}) %>%
        as.data.frame(.) %>%
        rename(dh = "resid.model_hM.")
    ) %>%
    arrange(id_ind, year)
  
  res_h_female <- expand.grid(
    id_part = unique(as.numeric(as.character(index(model_hF)[,1]))),
    year = first_year:last_year
    ) %>%
    left_join(
      cbind(index(model_hF), resid(model_hF)) %>%
        lapply(FUN = function(x) {as.numeric(paste(x))}) %>%
        as.data.frame(.) %>%
        rename(dh_part = "resid.model_hF.")
    ) %>%
    arrange(id_part, year)
  
  df <- df %>%
    left_join(res_w_male) %>% 
    left_join(res_w_female) %>% 
    left_join(res_c) %>% 
    left_join(res_e_male) %>% 
    left_join(res_e_female) %>% 
    left_join(res_h_male) %>% 
    left_join(res_h_female)
  
  return(df)
}




moments <- function(data, N, T, times, sims, kap, sig) {
  ###### Симулированные моменты ######
  
  k_h_u <- kap[1]
  k_h_v <- kap[2]
  sigma_v <- sig[1]
  sigma_u <- sig[2]
  sigma_eps <- sig[3]
  sigma_delta <- sig[4]
  sigma_gamma <- kap[3]
  k_c_u <- sig[5]
  k_c_v <- sig[6]
  
  sim_list <- list()
  
  for (j in 1:sims) {
    df <- data.frame(
      id_ind = rep(1:N, each = T),
      year = rep(seq(first_year, last_year, times), times = N))
    
    set.seed(j) # СЮДА СЧЕТЧИК ПОДСТАВИТЬ
    df <- df %>% group_by(id_ind) %>%
      mutate(
        # Генерируем шоки
        v = rnorm(n()) * sqrt(sigma_v), # перманентный шок з/п
        u = rnorm(n()) * sqrt(sigma_u), # транзитивный шок з/п
        eps = rnorm(n()) * sqrt(sigma_eps), # ошибка измерения з/п
        delta = rnorm(n()) * sqrt(sigma_delta), # ошибка измерения потребления
        gamma = rnorm(n()) * sqrt(sigma_gamma)) # ошибка измерения отработанных часов
    
    df <- df %>% group_by(id_ind) %>% 
      mutate(
        # Генерируем перманентные компоненты переменных интереса
        wp = cumsum(v),
        cp = cumsum(k_c_v * v),
        yp = cumsum((1 + k_h_v) * v)) %>% 
      mutate(
        # Генерируем переменные интереса
        w = wp + u + eps,
        c = cp + k_c_u * u + delta,
        y = yp + (1 + k_h_u) * u + eps + gamma) %>%
      select(id_ind, year, w, c, y) %>% 
      mutate(
        # Генерируем приросты переменных интереса
        dw = w - dplyr::lag(w),
        dc = c - dplyr::lag(c),
        dy = y - dplyr::lag(y)) %>% 
      select(id_ind, year, dw, dc, dy) %>% 
      mutate(
        # Генерируем лаги приростов переменных интереса
        dLw = dplyr::lag(dw),
        dLc = dplyr::lag(dc),
        dLy = dplyr::lag(dy)
      )
    
    df <- df %>% group_by(year) %>% summarise(
      dw_dw = mean(dw * dw, na.rm = TRUE),
      dw_dLw = mean(dw * dLw, na.rm = TRUE),
      # dc_dc = mean(dc * dc, na.rm = TRUE),
      # dc_dLc = mean(dc * dLc, na.rm = TRUE),
      dy_dy = mean(dy * dy, na.rm = TRUE),
      dy_dLy = mean(dy * dLy, na.rm = TRUE),
      # dc_dw = mean(dc * dw, na.rm = TRUE),
      # dc_dLw = mean(dc * dLw, na.rm = TRUE),
      # dLc_dw = mean(dLc * dw, na.rm = TRUE),
      dc_dy = mean(dc * dy, na.rm = TRUE),
      dc_dLy = mean(dc * dLy, na.rm = TRUE),
      dLc_dy = mean(dLc * dy, na.rm = TRUE),
      dy_dw = mean(dy * dw, na.rm = TRUE),
      dy_dLw = mean(dy * dLw, na.rm = TRUE),
      dLy_dw = mean(dLy * dw, na.rm = TRUE)) %>% 
      column_to_rownames("year")
    
    sim_list[[paste("sim", j, sep = "_")]] <- df
  }
  
  moments_sim <- Reduce("+", sim_list) / sims
  
  ###### Эмпирические моменты ######
  dt <- data %>%
    select(id_ind, year, dw, dh, dc, dy) %>%
    group_by(id_ind) %>%
    mutate(
      # Генерируем лаги приростов переменных интереса
      dLw = dplyr::lag(dw),
      dLh = dplyr::lag(dh),
      dLc = dplyr::lag(dc),
      dLy = dplyr::lag(dy)
    )
  
  moments_emp <- dt %>% group_by(year) %>% summarise(
    dw_dw = mean(dw * dw, na.rm = TRUE),
    dw_dLw = mean(dw * dLw, na.rm = TRUE),
    # dc_dc = mean(dc * dc, na.rm = TRUE),
    # dc_dLc = mean(dc * dLc, na.rm = TRUE),
    dy_dy = mean(dy * dy, na.rm = TRUE),
    dy_dLy = mean(dy * dLy, na.rm = TRUE),
    # dc_dw = mean(dc * dw, na.rm = TRUE),
    # dc_dLw = mean(dc * dLw, na.rm = TRUE),
    # dLc_dw = mean(dLc * dw, na.rm = TRUE),
    dc_dy = mean(dc * dy, na.rm = TRUE),
    dc_dLy = mean(dc * dLy, na.rm = TRUE),
    dLc_dy = mean(dLc * dy, na.rm = TRUE),
    dy_dw = mean(dy * dw, na.rm = TRUE),
    dy_dLw = mean(dy * dLw, na.rm = TRUE),
    dLy_dw = mean(dLy * dw, na.rm = TRUE)) %>% 
    column_to_rownames("year")
  
  V <- mean(as.matrix((moments_sim - moments_emp)**2), na.rm = TRUE)  # целевой функционал * 1e10
  
  print(V)
  
  return(list(moments_emp = moments_emp, moments_sim = moments_sim))
}





# Goodness of fit ---------------------------------------------------------
internal_fit <- function(kap, sig, df) {
  # сопоставление моментных условий для GMM_sim_6
  sigma_vM <- sig[[1]]
  sigma_uM <- sig[[2]]
  sigma_vF <- sig[[3]]
  sigma_uF <- sig[[4]]
  cov_uMF <- sig[[5]]
  cov_vMF <- sig[[6]]
  sigma_epsM <- sig[[7]]
  sigma_epsF <- sig[[8]]
  sigma_delta <- sig[[9]]
  k_c_uM <- sig[[10]]
  k_c_vM <- sig[[11]]
  k_c_uF <- sig[[12]]
  k_c_vF <- sig[[13]]
  
  k_hM_uM <- kap[[1]]
  k_hM_vM <- kap[[2]]
  k_hF_uF <- kap[[3]]
  k_hF_vF <- kap[[4]]
  k_hM_uF <- kap[[5]]
  k_hM_vF <- kap[[6]]
  k_hF_uM <- kap[[7]]
  k_hF_vM <- kap[[8]]
  sigma_gammaM <- kap[[9]]
  sigma_gammaF <- kap[[10]]
  sigma_psiM <- kap[[11]]
  sigma_psiF <- kap[[12]]
  
  ###### Теоретические моменты ###### 
  moments_sim <- data.frame(year = first_year:last_year) %>% 
    mutate(
      sigma_vM = sigma_vM, sigma_uM = sigma_uM,
      sigma_vF = sigma_vF, sigma_uF = sigma_uF,
      cov_uMF = cov_uMF, cov_vMF = cov_vMF,
      sigma_epsM = sigma_epsM, sigma_epsF = sigma_epsF,
      sigma_gammaM = sigma_gammaM, sigma_gammaF = sigma_gammaF,
      sigma_psiM = sigma_psiM, sigma_psiF = sigma_psiF,
      sigma_delta = sigma_delta) %>%
    mutate(
      cov_eps_gammaM = sigma_gammaM - 0.5 * (sigma_gammaM + sigma_psiM - sigma_epsM),
      cov_eps_gammaF = sigma_gammaF - 0.5 * (sigma_gammaF + sigma_psiF - sigma_epsF)) %>% 
    mutate(
      # Этап 1
      dwM_dwM = (sigma_uM + lag(sigma_uM)) +
        sigma_vM +
        (sigma_epsM + lag(sigma_epsM)),
      dwM_dLwM = - lag(sigma_uM) -
        lag(sigma_epsM),
      dwF_dwF = (sigma_uF + lag(sigma_uF)) +
        sigma_vF +
        (sigma_epsF + lag(sigma_epsF)),
      dwF_dLwF = - lag(sigma_uF) -
        lag(sigma_epsF),
      dwM_dwF = (cov_uMF + lag(cov_uMF)) +
        cov_vMF,
      dwM_dLwF = - lag(cov_uMF),
      dLwM_dwF = - lag(cov_uMF),
      dc_dc = (sigma_uM + lag(sigma_uM)) * k_c_uM^2 +
        (sigma_uF + lag(sigma_uF)) * k_c_uF^2 +
        sigma_vM * k_c_vM^2 +
        sigma_vF * k_c_vF^2 +
        (sigma_delta + lag(sigma_delta)) +
        (cov_uMF + lag(cov_uMF)) * 2 * k_c_uM * k_c_uF +
        cov_vMF * 2 * k_c_vM * k_c_vF,
      dc_dLc = - lag(sigma_uM) * k_c_uM^2 -
        lag(sigma_uF) * k_c_uF^2 -
        lag(sigma_delta) -
        lag(cov_uMF) * 2 * k_c_uM * k_c_uF,
      dc_dwM = (sigma_uM + lag(sigma_uM)) * k_c_uM +
        sigma_vM * k_c_vM +
        (cov_uMF + lag(cov_uMF)) * k_c_uF +
        cov_vMF * k_c_vF,
      dc_dwF = (sigma_uF + lag(sigma_uF)) * k_c_uF +
        sigma_vF * k_c_vF +
        (cov_uMF + lag(cov_uMF)) * k_c_uM +
        cov_vMF * k_c_vM,
      dc_dLwM = - lag(sigma_uM) * k_c_uM -
        lag(cov_uMF) * k_c_uF,
      dc_dLwF = - lag(sigma_uF) * k_c_uF -
        lag(cov_uMF) * k_c_uM,
      dLc_dwM = - lag(sigma_uM) * k_c_uM -
        lag(cov_uMF) * k_c_uF,
      dLc_dwF = - lag(sigma_uF) * k_c_uF -
        lag(cov_uMF) * k_c_uM,
      
      # Этап 2
      dyM_dyM = (sigma_uM + lag(sigma_uM)) * (1 + k_hM_uM)^2 +
        (cov_uMF + lag(cov_uMF)) * 2 * (1 + k_hM_uM) * k_hM_uF +
        (sigma_uF + lag(sigma_uF)) * k_hM_uF^2 +
        sigma_vM * (1 + k_hM_vM)^2 +
        cov_vMF * 2 * (1 + k_hM_vM) * k_hM_vF +
        sigma_vF * k_hM_vF^2 +
        (sigma_gammaM + lag(sigma_gammaM)),
      dyM_dLyM = - lag(sigma_uM) * (1 + k_hM_uM)^2 -
        cov_uMF * 2 * (1 + k_hM_uM) * k_hM_uF -
        lag(sigma_uF) * k_hM_uF^2 -
        lag(sigma_gammaM),
      dyF_dyF = (sigma_uF + lag(sigma_uF)) * (1 + k_hF_uF)^2 +
        (cov_uMF + lag(cov_uMF)) * 2 * (1 + k_hF_uF) * k_hF_uM +
        (sigma_uM + lag(sigma_uM)) * k_hF_uM^2 +
        sigma_vF * (1 + k_hF_vF)^2 +
        cov_vMF * 2 * (1 + k_hF_vF) * k_hF_vM +
        sigma_vM * k_hF_vM^2 +
        (sigma_gammaF + lag(sigma_gammaF)),
      dyF_dLyF = - lag(sigma_uF) * (1 + k_hF_uF)^2 -
        cov_uMF * 2 * (1 + k_hF_uF) * k_hF_uM -
        lag(sigma_uM) * k_hF_uM^2 -
        lag(sigma_gammaF),
      dyM_dyF = (sigma_uM + lag(sigma_uM)) * (1 + k_hM_uM) * k_hF_uM + 
        (cov_uMF + lag(cov_uMF)) * (1 + k_hM_uM) * (1 + k_hF_uF) +
        (cov_uMF + lag(cov_uMF)) * k_hM_uF * k_hF_uM +
        (sigma_uF + lag(sigma_uF)) * k_hM_uF * (1 + k_hF_uF) +
        sigma_vM * (1 + k_hM_vM) * k_hF_vM +
        cov_vMF * (1 + k_hM_vM) * (1 + k_hF_vF) +
        cov_vMF * k_hM_vF * k_hF_vM +
        sigma_vF * k_hM_vF * (1 + k_hF_vF),
      dyM_dLyF = - lag(sigma_uM) * (1 + k_hM_uM) * k_hF_uM -
        lag(cov_uMF) * (1 + k_hM_uM) * (1 + k_hF_uF) -
        lag(cov_uMF) * k_hM_uF * k_hF_uM -
        lag(sigma_uF) * k_hM_uF * (1 + k_hF_uF),
      dLyM_dyF = - lag(sigma_uM) * (1 + k_hM_uM) * k_hF_uM -
        lag(cov_uMF) * (1 + k_hM_uM) * (1 + k_hF_uF) -
        lag(cov_uMF) * k_hM_uF * k_hF_uM -
        lag(sigma_uF) * k_hM_uF * (1 + k_hF_uF),
      dyM_dwM = (sigma_uM + lag(sigma_uM)) * (1 + k_hM_uM) +
        (cov_uMF + lag(cov_uMF)) * k_hM_uF +
        sigma_vM * (1 + k_hM_vM) +
        cov_vMF * k_hM_vF +
        (cov_eps_gammaM + lag(cov_eps_gammaM)),
      dLyM_dwM = - lag(sigma_uM) * (1 + k_hM_uM) -
        lag(cov_uMF) * k_hM_uF -
        lag(cov_eps_gammaM),
      dyM_dLwM = - lag(sigma_uM) * (1 + k_hM_uM) -
        lag(cov_uMF) * k_hM_uF -
        lag(cov_eps_gammaM),
      dyF_dwF = (sigma_uF + lag(sigma_uF)) * (1 + k_hF_uF) +
        (cov_uMF + lag(cov_uMF)) * k_hF_uM +
        sigma_vF * (1 + k_hF_vF) +
        cov_vMF * k_hF_vM +
        (cov_eps_gammaF + lag(cov_eps_gammaF)),
      dLyF_dwF = - lag(sigma_uF) * (1 + k_hF_uF) -
        lag(cov_uMF) * k_hF_uM -
        lag(cov_eps_gammaF),
      dyF_dLwF = - lag(sigma_uF) * (1 + k_hF_uF) -
        lag(cov_uMF) * k_hF_uM -
        lag(cov_eps_gammaF),
      dyM_dwF = (cov_uMF + lag(cov_uMF)) * (1 + k_hM_uM) +
        (sigma_uF + lag(sigma_uF)) * k_hM_uF +
        cov_vMF * (1 + k_hM_vM) +
        sigma_vF * k_hM_vF,
      dyM_dLwF = - lag(cov_uMF) * (1 + k_hM_uM) -
        lag(sigma_uF) * k_hM_uF,
      dLyM_dwF = - lag(cov_uMF) * (1 + k_hM_uM) -
        lag(sigma_uF) * k_hM_uF,
      dyF_dwM = (cov_uMF + lag(cov_uMF)) * (1 + k_hF_uF) +
        (sigma_uM + lag(sigma_uM)) * k_hF_uM +
        cov_vMF * (1 + k_hF_vF) +
        sigma_vM * k_hF_vM,
      dyF_dLwM = - lag(cov_uMF) * (1 + k_hF_uF) -
        lag(sigma_uM) * k_hF_uM,
      dLyF_dwM = - lag(cov_uMF) * (1 + k_hF_uF) -
        lag(sigma_uM) * k_hF_uM) %>% 
    select(-sigma_vM, -sigma_uM, -sigma_vF, -sigma_uF, -cov_uMF, -cov_vMF,
      -sigma_epsM, -sigma_epsF, -sigma_gammaM, -sigma_gammaF, -sigma_psiM, -sigma_psiF,
      -sigma_delta, -cov_eps_gammaM, -cov_eps_gammaF) %>%
    apply(2, mean, na.rm = TRUE)
  
  ###### Эмпирические моменты ######
  moments_emp <- df %>%
    group_by(year) %>%
    summarise(
      # Этап 1
      dwM_dwM = mean(dwM * dwM, na.rm = TRUE),
      dwM_dLwM = mean(dwM * dLwM, na.rm = TRUE),
      dwF_dwF = mean(dwF * dwF, na.rm = TRUE),
      dwF_dLwF = mean(dwF * dLwF, na.rm = TRUE),
      dwM_dwF = mean(dwM * dwF, na.rm = TRUE),
      dwM_dLwF = mean(dwM * dLwF, na.rm = TRUE),
      dLwM_dwF = mean(dLwM * dwF, na.rm = TRUE),
      dc_dc = mean(dc * dc, na.rm = TRUE),
      dc_dLc = mean(dc * dLc, na.rm = TRUE),
      dc_dwM = mean(dc * dwM, na.rm = TRUE),
      dc_dwF = mean(dc * dwF, na.rm = TRUE),
      dc_dLwM = mean(dc * dLwM, na.rm = TRUE),
      dc_dLwF = mean(dc * dLwF, na.rm = TRUE),
      dLc_dwM = mean(dLc * dwM, na.rm = TRUE),
      dLc_dwF = mean(dLc * dwF, na.rm = TRUE),
      
      # Этап 2
      dyM_dyM = mean(dyM * dyM, na.rm = TRUE),
      dyM_dLyM = mean(dyM * dLyM, na.rm = TRUE),
      dyF_dyF = mean(dyF * dyF, na.rm = TRUE),
      dyF_dLyF = mean(dyF * dLyF, na.rm = TRUE),
      dyM_dyF = mean(dyM * dyF, na.rm = TRUE),
      dyM_dLyF = mean(dyM * dLyF, na.rm = TRUE),
      dLyM_dyF = mean(dLyM * dyF, na.rm = TRUE),
      dyM_dwM = mean(dyM * dwM, na.rm = TRUE),
      dLyM_dwM = mean(dLyM * dwM, na.rm = TRUE),
      dyM_dLwM = mean(dyM * dLwM, na.rm = TRUE),
      dyF_dwF = mean(dyF * dwF, na.rm = TRUE),
      dLyF_dwF = mean(dLyF * dwF, na.rm = TRUE),
      dyF_dLwF = mean(dyF * dLwF, na.rm = TRUE),
      dyM_dwF = mean(dyM * dwF, na.rm = TRUE),
      dyM_dLwF = mean(dyM * dLwF, na.rm = TRUE),
      dLyM_dwF = mean(dLyM * dwF, na.rm = TRUE),
      dyF_dwM = mean(dyF * dwM, na.rm = TRUE),
      dyF_dLwM = mean(dyF * dLwM, na.rm = TRUE),
      dLyF_dwM = mean(dLyF * dwM, na.rm = TRUE)) %>% 
    apply(2, mean, na.rm = TRUE)
  
  ###### Сводная таблица ######
  moments <- as.data.frame(rbind(moments_sim, moments_emp)) %>% 
    select(-year) %>% 
    rownames_to_column(var = "type")
  
  return(moments)
}





