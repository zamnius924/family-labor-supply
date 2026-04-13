start_model_pref <- list(
  bounds = data.frame(
    xmin = c(
      0, # k_hM_uM
      -1, # k_hM_vM
      0, # k_hF_uF
      -1, # k_hF_vF
      -1, # k_hM_uF
      -1, # k_hM_vF
      -1, # k_hF_uM
      -1, # k_hF_vM
      0, # sigma_gammaM
      0, # sigma_gammaF
      0, # sigma_psiM
      0), # sigma_psiF
    xmax = c(
      1, # k_hM_uM
      1, # k_hM_vM
      1, # k_hF_uF
      1, # k_hF_vF
      1, # k_hM_uF
      1, # k_hM_vF
      1, # k_hF_uM
      1, # k_hF_vM
      1, # sigma_gammaM
      1, # sigma_gammaF
      1, # sigma_gammaM
      1 # sigma_gammaF
    )
  )
)

start_model_pref$start <- c(
  0.15, # k_hM_uM
  0.05, # k_hM_vM
  0.3, # k_hF_uF
  0.03, # k_hF_vF
  0, # k_hM_uF
  0, # k_hM_vF
  0, # k_hF_uM
  0, # k_hF_vM
  0.1, # sigma_gammaM
  0.1, # sigma_gammaF
  0.1, # sigma_psiM
  0.1 # sigma_psiF
) 

model_pref <- fmincon(
  fn = GMM_model_pref,
  sig = res_model_wage,
  data = data_mod,
  x0 = start_model_pref$start,
  lb = start_model_pref$bounds$xmin,
  ub = start_model_pref$bounds$xmax
)

res_model_pref <- c(model_pref$par, model_pref$convergence, model_pref$value)
res_model_pref <- split(
  res_model_pref,
  rep(1:13, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2))
)
names(res_model_pref) <- c("k_hM_uM", "k_hM_vM", "k_hF_uF", "k_hF_vF", 
  "k_hM_uF", "k_hM_vF", "k_hF_uM", "k_hF_vM",
  "sigma_gammaM", "sigma_gammaF", "sigma_psiM", "sigma_psiF")