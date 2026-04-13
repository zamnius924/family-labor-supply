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

# Оценка
model_pref_hetero <- list()
res_model_pref_hetero <- list()

start_model_pref_hetero <- list(
  bounds = data.frame(
    xmin = c(
      0, # k_hM_uM
      -1, # k_hM_vM
      0, # k_hF_uF
      -1, # k_hF_vF
      -1, # k_hM_uF
      -1, # k_hM_vF
      -1, # k_hF_uM
      -1  # k_hF_vM
    ), 
    xmax = c(
      1, # k_hM_uM
      1, # k_hM_vM
      1, # k_hF_uF
      1, # k_hF_vF
      1, # k_hM_uF
      1, # k_hM_vF
      1, # k_hF_uM
      1 # k_hF_vM
    )
  )
)

start_model_pref_hetero$start <- c(
  0.15, # k_hM_uM
  0.05, # k_hM_vM
  0.3, # k_hF_uF
  0.03, # k_hF_vF
  0, # k_hM_uF
  0, # k_hM_vF
  0, # k_hF_uM
  0  # k_hF_vM
) 

for (k in 1:4) {
  
  print(paste("Итерация", k, "из 4"))
  
  model_pref_hetero[[k]] <- fmincon(
    fn = GMM_model_pref_hetero,
    sig_1 = res_model_wage,
    sig_2 = res_model_pref, 
    data = subset(data_mod_hetero, educ == k),
    x0 = start_model_pref_hetero$start,
    lb = start_model_pref_hetero$bounds$xmin,
    ub = start_model_pref_hetero$bounds$xmax
  )

  res_model_pref_hetero$coef[[k]] <- c(
    model_pref_hetero[[k]]$par,
    model_pref_hetero[[k]]$convergence,
    model_pref_hetero[[k]]$value
  )
  res_model_pref_hetero$coef[[k]] <- split(
    res_model_pref_hetero$coef[[k]],
    rep(1:9, c(1, 1, 1, 1, 1, 1, 1, 1, 2))
  )
  names(res_model_pref_hetero$coef[[k]]) <- c(
    "k_hM_uM", "k_hM_vM", "k_hF_uF", "k_hF_vF", "k_hM_uF",
    "k_hM_vF", "k_hF_uM", "k_hF_vM"
  )
  
}