start_model_pref_cond <- list(
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

start_model_pref_cond$start <- c(
  0.15, # k_hM_uM
  0.05, # k_hM_vM
  0.3, # k_hF_uF
  0.03, # k_hF_vF
  0, # k_hM_uF
  0, # k_hM_vF
  0, # k_hF_uM
  0  # k_hF_vM
) 

sample_index <- unique(data_cond$df$id_hh)

res_model_pref_cond <- list()
res_model_pref_cond$coef <- data.frame()
res_model_pref_cond$sd <- data.frame()

for (t in 1:length(unique(data_cond$grid$num))) { 
  
  cat(c("\nИтерация:", t, "из", length(unique(data_cond$grid$num))), "\n")
  
  # Условные моменты
  moments_cond <- data_cond$coef %>%
    left_join(
      data_cond$grid %>%
        subset(num == t) %>%
        ungroup() %>%
        select(year, learnM, learnF)
    ) %>% 
    mutate(value = const + coef_learnM * learnM + coef_learnF * learnF) %>% 
    select(year, name, value) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    ungroup() %>% 
    add_row(year = 2000, .before = 1)
  
  # Оценка коэффициентов
  model_pref_cond <- fmincon(
    fn = GMM_model_pref_cond,
    sig_1 = res_model_wage,
    sig_2 = res_model_pref,
    moments_emp = moments_cond,
    x0 = start_model_pref_cond$start,
    lb = start_model_pref_cond$bounds$xmin,
    ub = start_model_pref_cond$bounds$xmax
  )
  
  res_model_pref_cond$coef <- rbind(
    res_model_pref_cond$coef, 
    c(t,
      model_pref_cond$par,
      model_pref_cond$convergence,
      model_pref_cond$value)
  )
  names(res_model_pref_cond$coef) <- c(
    "num", "k_hM_uM", "k_hM_vM", "k_hF_uF",
    "k_hF_vF", "k_hM_uF","k_hM_vF", "k_hF_uM", "k_hF_vM",
    "convergence", "value"
  )
  
  # Бутстрап
  boot_pref_cond <- boot(
    sample_index,
    bootstrap_cond,
    R = B,
    parallel = "multicore", 
    ncpus = ncpus,
    t = t
  )
  
  res_model_pref_cond$sd <- rbind(
    res_model_pref_cond$sd,
    c(t, apply(boot_pref_cond[["t"]][,1:8], 2, sd))
  )
  names(res_model_pref_cond$sd) <- c(
    "num", "k_hM_uM", "k_hM_vM", "k_hF_uF",
    "k_hF_vF", "k_hM_uF", "k_hM_vF", "k_hF_uM", "k_hF_vM"
  )
  
}

# Построение ДИ
res_model_pref_cond$CI_u <- res_model_pref_cond$coef[,2:9] + 1.96 * res_model_pref_cond$sd[,2:9]
res_model_pref_cond$CI_l <- res_model_pref_cond$coef[,2:9] - 1.96 * res_model_pref_cond$sd[,2:9]
res_model_pref_cond$CI_u$num <- res_model_pref_cond$coef$num
res_model_pref_cond$CI_l$num <- res_model_pref_cond$coef$num

res_model_pref_cond$coef <- as.data.frame(res_model_pref_cond$coef) %>%
  left_join(
    data_cond$grid %>% 
      select(numM, numF, num) %>% 
      distinct_all()
  )
res_model_pref_cond$CI_u <- as.data.frame(res_model_pref_cond$CI_u) %>%
  left_join(
    data_cond$grid %>%
      select(numM, numF, num) %>%
      distinct_all()
  )
res_model_pref_cond$CI_l <- as.data.frame(res_model_pref_cond$CI_l) %>%
  left_join(
    data_cond$grid %>%
      select(numM, numF, num) %>%
      distinct_all()
  )
