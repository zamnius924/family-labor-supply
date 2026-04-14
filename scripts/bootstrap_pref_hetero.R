for (k in 1:4) {
  
  print(paste("Итерация", k, "из 4"))
  
  sample_index <- unique(subset(data_mod_hetero, educ == k)$id_hh)
  
  boot_pref_hetero <- boot(
    sample_index,
    bootstrap_hetero,
    R = B, 
    parallel = "multicore",
    ncpus = ncpus,
    k = k)
  
  res_model_pref_hetero$sd[[k]] <- apply(boot_pref_hetero[["t"]][,1:8], 2, sd)
  names(res_model_pref_hetero$sd[[k]]) <- c(
    "k_hM_uM", "k_hM_vM", "k_hF_uF", "k_hF_vF",
    "k_hM_uF", "k_hM_vF", "k_hF_uM", "k_hF_vM"
  )
  
}

rm(boot_pref_hetero)

res_model_pref_hetero$table_coef <- t(rbind(
    data.frame(res_model_pref_hetero$coef[[1]])[1,1:8],
    data.frame(res_model_pref_hetero$coef[[2]])[1,1:8],
    data.frame(res_model_pref_hetero$coef[[3]])[1,1:8],
    data.frame(res_model_pref_hetero$coef[[4]])[1,1:8]
  )) %>% 
  as.data.frame(.) %>% 
  rownames_to_column(var = "par") %>% 
  pivot_longer(
    cols = V1:V4,
    names_to = "educ",
    values_to = "coef"
  )

res_model_pref_hetero$table_sd <- t(rbind(
    res_model_pref_hetero$sd[[1]],
    res_model_pref_hetero$sd[[2]],
    res_model_pref_hetero$sd[[3]],
    res_model_pref_hetero$sd[[4]]
  )) %>% 
  as.data.frame(.) %>% 
  rownames_to_column(var = "par") %>% 
  pivot_longer(
    cols = V1:V4,
    names_to = "educ",
    values_to = "sd"
  )

res_model_pref_hetero$table <- res_model_pref_hetero$table_coef %>% 
  left_join(res_model_pref_hetero$table_sd) %>% 
  mutate(
    CI_u = coef + 1.96 * sd,
    CI_l = coef - 1.96 * sd
  ) %>% 
  mutate(
    sex = case_when(
      grepl("hM", res_model_pref_hetero$table_coef$par) == TRUE ~ "Male",
      grepl("hF", res_model_pref_hetero$table_coef$par) == TRUE ~ "Female"
    )
  ) %>% 
  mutate(
    educM = case_when(
      educ %in% c("V1", "V2") ~ 1,
      educ %in% c("V3", "V4") ~ 2
    ),
    educF = case_when(
      educ %in% c("V1", "V3") ~ 1,
      educ %in% c("V2", "V4") ~ 2
    )
  )
