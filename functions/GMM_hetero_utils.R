# ============================================================================
# GMM_hetero_utils.R
# ----------------------------------------------------------------------------
# GMM objective function and bootstrap helper for preference heterogeneity
# by education level (four groups based on higher education of spouses).
#
# Dependencies:  params.R (first_year, last_year)
#                data_mod_hetero (created by arrangers/data_mod_hetero.R)
#                res_model_wage, res_model_pref (from model_stage_2.R, model_stage_3.R)
# Called by:     scripts/model_pref_hetero.R, scripts/bootstrap_pref_hetero.R
# ============================================================================

#' GMM objective for preference parameters with education heterogeneity
GMM_model_pref_hetero <- function(kap, sig_1, sig_2, data) {
  
  ### Предопределение переменных
  kap <- split(kap, rep(1:8, c(1, 1, 1, 1, 1, 1, 1, 1)))
  k_hM_uM <- kap[[1]]
  k_hM_vM <- kap[[2]]
  k_hF_uF <- kap[[3]]
  k_hF_vF <- kap[[4]]
  k_hM_uF <- kap[[5]]
  k_hM_vF <- kap[[6]]
  k_hF_uM <- kap[[7]]
  k_hF_vM <- kap[[8]]
  sigma_vM <- sig_1[[1]]
  sigma_uM <- sig_1[[2]]
  sigma_vF <- sig_1[[3]]
  sigma_uF <- sig_1[[4]]
  cov_uMF <- sig_1[[5]]
  cov_vMF <- sig_1[[6]]
  sigma_epsM <- sig_1[[7]]
  sigma_epsF <- sig_1[[8]]
  sigma_delta <- sig_1[[9]]
  sigma_gammaM <- sig_2[[9]]
  sigma_gammaF <- sig_2[[10]]
  sigma_psiM <- sig_2[[11]]
  sigma_psiF <- sig_2[[12]]
  k_c_uM <- sig_1[[10]]
  k_c_vM <- sig_1[[11]]
  k_c_uF <- sig_1[[12]]
  k_c_vF <- sig_1[[13]]
  
  # Theoretical moments
  moments_sim <- data.frame(year = first_year:last_year) %>% 
    mutate(
      sigma_vM = sigma_vM, 
      sigma_uM = sigma_uM,
      sigma_vF = sigma_vF,
      sigma_uF = sigma_uF,
      cov_uMF = cov_uMF,
      cov_vMF = cov_vMF,
      sigma_epsM = sigma_epsM,
      sigma_epsF = sigma_epsF,
      sigma_gammaM = sigma_gammaM,
      sigma_gammaF = sigma_gammaF,
      sigma_psiM = sigma_psiM,
      sigma_psiF = sigma_psiF
    ) %>% 
    mutate(
      cov_eps_gammaM = sigma_gammaM - 0.5 * (sigma_gammaM + sigma_psiM - sigma_epsM),
      cov_eps_gammaF = sigma_gammaF - 0.5 * (sigma_gammaF + sigma_psiF - sigma_epsF)
    ) %>% 
    mutate(
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
        sigma_vF * k_hM_vF * (1 + k_hF_vF), # (sigma_gamma + lag(sigma_gamma))
      dyM_dLyF = - lag(sigma_uM) * (1 + k_hM_uM) * k_hF_uM -
        lag(cov_uMF) * (1 + k_hM_uM) * (1 + k_hF_uF) -
        lag(cov_uMF) * k_hM_uF * k_hF_uM -
        lag(sigma_uF) * k_hM_uF * (1 + k_hF_uF), # lag(sigma_gamma)
      dLyM_dyF = - lag(sigma_uM) * (1 + k_hM_uM) * k_hF_uM -
        lag(cov_uMF) * (1 + k_hM_uM) * (1 + k_hF_uF) -
        lag(cov_uMF) * k_hM_uF * k_hF_uM -
        lag(sigma_uF) * k_hM_uF * (1 + k_hF_uF), # lag(sigma_gamma)
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
        sigma_vF * k_hM_vF, # (cov_eps_gamma + lag(cov_eps_gamma))
      dyM_dLwF = - lag(cov_uMF) * (1 + k_hM_uM) -
        lag(sigma_uF) * k_hM_uF, # lag(cov_eps_gamma)
      dLyM_dwF = - lag(cov_uMF) * (1 + k_hM_uM) -
        lag(sigma_uF) * k_hM_uF, # lag(cov_eps_gamma)
      dyF_dwM = (cov_uMF + lag(cov_uMF)) * (1 + k_hF_uF) +
        (sigma_uM + lag(sigma_uM)) * k_hF_uM +
        cov_vMF * (1 + k_hF_vF) +
        sigma_vM * k_hF_vM, # (cov_eps_gamma + lag(cov_eps_gamma))
      dyF_dLwM = - lag(cov_uMF) * (1 + k_hF_uF) -
        lag(sigma_uM) * k_hF_uM, # lag(cov_eps_gamma)
      dLyF_dwM = - lag(cov_uMF) * (1 + k_hF_uF) -
        lag(sigma_uM) * k_hF_uM # lag(cov_eps_gamma)
    ) %>% 
    select(
      -sigma_vM, -sigma_uM, -sigma_vF, -sigma_uF, -cov_uMF, -cov_vMF,
      -sigma_epsM, -sigma_gammaM, -sigma_psiM, -cov_eps_gammaM,
      -sigma_epsF, -sigma_gammaF, -sigma_psiF, -cov_eps_gammaF
    )
  
  moments_sim <- moments_sim %>% 
    pivot_longer(!year, names_to = "name") %>% 
    arrange(factor(name, levels = colnames(moments_sim)[-1]))
  
  # Empirical moments
  moments_emp <- data %>% 
    group_by(year) %>%
    summarise(
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
      dLyF_dwM = mean(dLyF * dwM, na.rm = TRUE)
    ) 
  
  moments_emp <- moments_emp %>% 
    pivot_longer(!year, names_to = "name") %>% 
    arrange(factor(name, levels = colnames(moments_emp)[-1]))
  
  V <- mean((moments_emp$value - moments_sim$value) ** 2, na.rm = TRUE)
  
  # print(kap)
  # print(V)
  
  return(V)

}

#' Bootstrap function for education heterogeneity
bootstrap_hetero <- function(hh, i, k) {
  
  # Bootstrap sample of households
  boot_sample <- data.frame(id_hh = hh[i]) %>%
    left_join(data_mod_hetero %>% subset(educ == k))
  
  # Preference model estimation
  boot_model_pref_hetero <- fmincon(
    fn = GMM_model_pref_hetero,
    sig_1 = res_model_wage,
    sig_2 = res_model_pref, 
    data = boot_sample,
    x0 = start_model_pref_hetero$start,
    lb = start_model_pref_hetero$bounds$xmin,
    ub = start_model_pref_hetero$bounds$xmax
  )
  boot_res_model_pref_hetero <- c(
    boot_model_pref_hetero$par,
    boot_model_pref_hetero$convergence,
    boot_model_pref_hetero$value
  )
  
  return(boot_res_model_pref_hetero)
}
