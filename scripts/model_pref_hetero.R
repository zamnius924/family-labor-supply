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