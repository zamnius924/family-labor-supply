# ============================================================================
# GMM_cond_utils.R
# ----------------------------------------------------------------------------
# GMM objective function and bootstrap helper for conditional moment estimation
# (heterogeneity by household income). Uses pre‑computed projection coefficients
# to construct moments at any point of the income grid.
#
# Dependencies:  params.R (first_year, last_year)
#                data_cond (from arrangers/data_mod_cond.R)
#                res_model_wage, res_model_pref
# Called by:     scripts/model_pref_cond.R
# ============================================================================

#' GMM objective for preference parameters using conditional moments
GMM_model_pref_cond <- function(kap, sig_1, sig_2, moments_emp) {

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
  moments_emp <- moments_emp %>%
    pivot_longer(!year, names_to = "name") %>% 
    arrange(factor(name, levels = colnames(moments_emp)[-1]))
  
  V <- mean((moments_emp$value - moments_sim$value) ** 2, na.rm = TRUE)
  
  # print(kap)
  # print(V)
  
  return(V)

}

#' Bootstrap function for conditional moment estimation
bootstrap_cond <- function(hh, i, t) {
  
  # Bootstrap sample of households
  boot_sample <- data.frame(id_hh = hh[i]) %>%
    left_join(data_cond$df)
  
  # Re‑estimate projection coefficients for each moment on log earnings
  boot_coef <- boot_sample %>%
    pivot_longer(!c(id_hh, year, learnM, learnF)) %>% 
    right_join( # аналог moments_emp
      boot_sample %>%
        group_by(year) %>%
        summarise_all(mean, na.rm = TRUE) %>% 
        select(year, dyM_dyM:dLyF_dwM) %>%
        pivot_longer(!year) %>%
        na_omit(.) %>%
        select(-value)
    ) %>% 
    group_by(year, name) %>%
    do(
      lm(value ~ learnM + learnF, data = .) %>% coef %>% tidy
    ) %>% 
    pivot_wider(names_from = names, values_from = x) %>% 
    rename(const = '(Intercept)', coef_learnM = learnM, coef_learnF = learnF) %>% 
    arrange(factor(name, levels = colnames(data_cond$df)[-1]))
  
  # Predict moments at the same grid point as in the original estimation
  boot_moments_cond <- boot_coef %>%
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
  
  # Preference model estimation
  boot_model_pref_cond <- fmincon(
    fn = GMM_model_pref_cond,
    sig_1 = res_model_wage,
    sig_2 = res_model_pref,
    moments_emp = boot_moments_cond,
    x0 = start_model_pref_cond$start,
    lb = start_model_pref_cond$bounds$xmin,
    ub = start_model_pref_cond$bounds$xmax
  )
  boot_res_model_pref_cond <- c(
    boot_model_pref_cond$par,
    boot_model_pref_cond$convergence,
    boot_model_pref_cond$value
  )
  
  return(boot_res_model_pref_cond)
}