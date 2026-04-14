# Создание метрик интереса ------------------------------------------------
metrics_diff <- function(df) {
  # Создаем датафрейм с темпами прироста метрик интереса
  df_new <- expand.grid(
    id_ind = unique(df$id_ind),
    year = first_year:last_year
  ) %>%
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
results_first_stage_diff <- function(df, model_wM, model_wF, model_c,
                                     model_yM, model_yF, model_hM, model_hF) {
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