for (k in 1:4) {
  
  print(paste("Итерация", k, "из 4"))
  
  sample_index <- unique(subset(data_mod_hetero, educ == k)$id_hh)
  
  boot_pref_hetero <- boot(
    sample_index,
    bootstrap_hetero,
    R = 5, 
    parallel = "multicore",
    ncpus = 8,
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


# Прямые коэффициенты эластичности
res_model_pref_hetero$elast_own <- res_model_pref_hetero$table %>% 
  subset(par %in% c("k_hM_uM", "k_hM_vM", "k_hF_uF", "k_hF_vF")) %>% 
  mutate(elasticity = case_when(
    grepl("uM|uF", .$par) == TRUE ~ "Frisch",
    grepl("vM|vF", .$par) == TRUE ~ "Marshall"
  )) %>%
  ggplot() +
  geom_point(
    aes(x = elasticity,
        y = coef,
        color = as.factor(sex)),
    position = position_dodge(0.5)
  ) +
  geom_text(
    aes(x = elasticity,
        y = coef,
        label = round(coef, 3)), 
    nudge_x = 0.3 * sign((as.numeric(as.factor(res_model_pref_hetero$table$sex)) - 2) * 2 + 1)
  ) +
  geom_errorbar(
    aes(x = elasticity,
        ymin = CI_l,
        ymax = CI_u,
        color = as.factor(sex)),
    position = position_dodge(0.5), width = 0.2
  ) +
  facet_grid(
    educM ~ educF,
    labeller = labeller(
      educM = res_model_pref_hetero$labM,
      educF = res_model_pref_hetero$labF
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Elasticity concept",
    y = "Own labor supply elasticity",
    color = "Sex"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")



# Перекрестные коэффициенты эластичности
res_model_pref_hetero$elast_cross <- res_model_pref_hetero$table %>% 
  subset(par %in% c("k_hM_uF", "k_hM_vF", "k_hF_uM", "k_hF_vM")) %>% 
  mutate(elasticity = case_when(
    grepl("uM|uF", .$par) == TRUE ~ "Frisch",
    grepl("vM|vF", .$par) == TRUE ~ "Marshall"
  )) %>%
  ggplot() +
  geom_point(
    aes(x = elasticity,
        y = coef,
        color = as.factor(sex)),
    position = position_dodge(0.5)
  ) +
  geom_text(
    aes(x = elasticity,
        y = coef,
        label = round(coef, 3)), 
    nudge_x = 0.3 * sign((as.numeric(as.factor(res_model_pref_hetero$table$sex)) - 2) * 2 + 1)
  ) +
  geom_errorbar(
    aes(x = elasticity,
        ymin = CI_l,
        ymax = CI_u,
        color = as.factor(sex)),
    position = position_dodge(0.5), width = 0.2
  ) +
  facet_grid(
    educM ~ educF,
    labeller = labeller(
      educM = res_model_pref_hetero$labM,
      educF = res_model_pref_hetero$labF
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Elasticity concept",
    y = "Cross labor supply elasticity",
    color = "Sex") +
  theme_bw() +
  theme(legend.position = "bottom")
