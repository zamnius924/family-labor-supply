start_model_wage <- list(
  bounds = data.frame(
    xmin = c(
      0, # sigma_vM
      0, # sigma_uM
      0, # sigma_vF
      0, # sigma_uF
      -0.1, # cov_uMF
      -0.1, # cov_vMF
      0, # sigma_epsM
      0, # sigma_epsF
      0, # sigma_delta
      0, # k_c_uM
      0, # k_c_vM
      0, # k_c_uF
      0), # k_c_vF
    xmax = c(
      1, # sigma_vM
      1, # sigma_uM
      1, # sigma_vF
      1, # sigma_uF
      0.1, # cov_uMF
      0.1, # cov_vMF
      1, # sigma_epsM
      1, # sigma_epsF
      1, # sigma_delta
      1, # k_c_uM
      1, # k_c_vM
      1, # k_c_uF
      1 # k_c_vF
    )
  )
)

start_model_wage$start <- c(
  0.15, # sigma_vM
  0.04, # sigma_uM
  0.15, # sigma_vF
  0.06, # sigma_uF
  0, # cov_uMF
  0, # cov_vMF
  0.15, # sigma_epsM
  0.15, # sigma_epsF
  0.2, # sigma_delta
  0.07, # k_c_uM
  0.2, # k_c_vM
  0.05, # k_c_uF
  0.15 # k_c_vF
)

model_wage <- fmincon(
  fn = GMM_model_wage,
  data = data_mod,
  x0 = start_model_wage$start,
  lb = start_model_wage$bounds$xmin,
  ub = start_model_wage$bounds$xmax
)

res_model_wage <- c(model_wage$par, model_wage$convergence, model_wage$value)
res_model_wage <- split(
  res_model_wage, 
  rep(1:14, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2))
)
names(res_model_wage) <- c("sigma_vM", "sigma_uM", "sigma_vF", "sigma_uF",
  "cov_uMF", "cov_vMF", "sigma_epsM", "sigma_epsF",
  "sigma_delta", "k_c_uM", "k_c_vM", "k_c_uF", "k_c_vF")