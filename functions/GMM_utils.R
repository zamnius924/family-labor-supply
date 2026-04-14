# ============================================================================
# GMM_utils.R
# ----------------------------------------------------------------------------
# GMM objective functions for wage process (stage 2) and preferences (stage 3),
# plus a bootstrap wrapper.
# ============================================================================

#' GMM objective for wage process (stage 2)
GMM_model_wage <- function(kap, data) {

  kap <- split(kap, rep(1:13, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)))
  sigma_vM <- kap[[1]]
  sigma_uM <- kap[[2]]
  sigma_vF <- kap[[3]]
  sigma_uF <- kap[[4]]
  cov_uMF <- kap[[5]]
  cov_vMF <- kap[[6]]
  sigma_epsM <- kap[[7]]
  sigma_epsF <- kap[[8]]
  sigma_delta <- kap[[9]]
  k_c_uM <- kap[[10]]
  k_c_vM <- kap[[11]]
  k_c_uF <- kap[[12]]
  k_c_vF <- kap[[13]]

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
      sigma_delta = sigma_delta
    ) %>% 
    mutate(
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
        cov_vMF, # + (sigma_eps + lag(sigma_eps))
      dwM_dLwF = - lag(cov_uMF), # lag(sigma_eps)
      dLwM_dwF = - lag(cov_uMF), # lag(sigma_eps)
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
        lag(cov_uMF) * k_c_uM
    ) %>% 
    select(
      -sigma_vM, -sigma_uM, -sigma_vF, -sigma_uF, -cov_uMF, -cov_vMF,
      -sigma_epsM, -sigma_epsF, -sigma_delta
    )
  
  moments_sim <- moments_sim %>% 
    pivot_longer(!year, names_to = "name") %>% 
    arrange(factor(name, levels = colnames(moments_sim)[-1]))
  
  # Empirical moments
  moments_emp <- data %>% 
    group_by(year) %>% 
    summarise(
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
      dLc_dwF = mean(dLc * dwF, na.rm = TRUE)
    )
  
  moments_emp <- moments_emp %>% 
    pivot_longer(!year, names_to = "name") %>% 
    arrange(factor(name, levels = colnames(moments_emp)[-1]))
  
  V <- mean((moments_emp$value - moments_sim$value) ** 2, na.rm = TRUE)

  #print(kap)
  #print(V)

  return(V)

}

#' GMM objective for preferences (stage 3)
GMM_model_pref <- function(kap, sig, data) {

  kap <- split(kap, rep(1:12, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)))
  k_hM_uM <- kap[[1]]
  k_hM_vM <- kap[[2]]
  k_hF_uF <- kap[[3]]
  k_hF_vF <- kap[[4]]
  k_hM_uF <- kap[[5]]
  k_hM_vF <- kap[[6]]
  k_hF_uM <- kap[[7]]
  k_hF_vM <- kap[[8]]
  sigma_vM <- sig[[1]]
  sigma_uM <- sig[[2]]
  sigma_vF <- sig[[3]]
  sigma_uF <- sig[[4]]
  cov_uMF <- sig[[5]]
  cov_vMF <- sig[[6]]
  sigma_epsM <- sig[[7]]
  sigma_epsF <- sig[[8]]
  sigma_delta <- sig[[9]]
  sigma_gammaM <- kap[[9]]
  sigma_gammaF <- kap[[10]]
  sigma_psiM <- kap[[11]]
  sigma_psiF <- kap[[12]]
  k_c_uM <- sig[[10]]
  k_c_vM <- sig[[11]]
  k_c_uF <- sig[[12]]
  k_c_vF <- sig[[13]]
  
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
      
      # dc_dyM = (sigma_uM + lag(sigma_uM)) * (1 + k_hM_uM) * k_c_uM +
      #   (cov_uMF + lag(cov_uMF)) * (1 + k_hM_uM) * k_c_uF +
      #   (cov_uMF + lag(cov_uMF)) * k_hM_uF * k_c_uM +
      #   (sigma_uF + lag(sigma_uF)) * k_hM_uF * k_c_uF +
      #   sigma_vM * (1 + k_hM_vM) * k_c_vM +
      #   cov_vMF * (1 + k_hM_vM) * k_c_vF +
      #   cov_vMF * k_hM_vF * k_c_vM +
      #   sigma_vF * k_hM_vF * k_c_vF,
      
      # dc_dy = (1 + k_h_u) * k_c_u * (sigma_u + lag(sigma_u)) + (1 + k_h_v) * k_c_v * sigma_v,
      # dc_dLy = - (1 + k_h_u) * k_c_u * lag(sigma_u),
      # dLc_dy = - (1 + k_h_u) * k_c_u * lag(sigma_u),
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
  
  #print(kap)
  #print(V)
  
  return(V)
  
}

#' Bootstrap function for baseline model
bootstrap <- function(hh, i) {
  
  # Bootstrap sample of households
  boot_sample <- data.frame(id_hh = hh[i]) %>%
    left_join(data_mod)
  
  # Wage model estimation
  boot_model_wage <- fmincon(
    fn = GMM_model_wage,
    x0 = start_model_wage$start,
    data = boot_sample
  )
  boot_res_model_wage <- c(
    boot_model_wage$par,
    boot_model_wage$convergence,
    boot_model_wage$value
  )
  
  # Preference model estimation
  boot_model_pref <- fmincon(
    fn = GMM_model_pref,
    x0 = start_model_pref$start,
    data = boot_sample,
    sig = boot_res_model_wage
  )
  boot_res_model_pref <- c(
    boot_model_pref$par,
    boot_model_pref$convergence,
    boot_model_pref$value
  )
  
  return(c(boot_res_model_wage, boot_res_model_pref))
}