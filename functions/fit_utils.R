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
      sigma_vM = sigma_vM, 
      sigma_uM = sigma_uM,
      sigma_vF = sigma_vF, 
      sigma_uF = sigma_uF,
      cov_uMF = cov_uMF, 
      cov_vMF = cov_vMF,
      sigma_epsM = sigma_epsM, 
      sigma_epsF = sigma_epsF,
      sigma_delta = sigma_delta,

      sigma_gammaM = sigma_gammaM, 
      sigma_gammaF = sigma_gammaF,
      sigma_psiM = sigma_psiM, 
      sigma_psiF = sigma_psiF
    ) %>%
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
    select(
      -sigma_vM, -sigma_uM, -sigma_vF, -sigma_uF, -cov_uMF, -cov_vMF,
      -sigma_epsM, -sigma_epsF, -sigma_gammaM, -sigma_gammaF, -sigma_psiM, -sigma_psiF,
      -sigma_delta, -cov_eps_gammaM, -cov_eps_gammaF
    ) %>%
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
