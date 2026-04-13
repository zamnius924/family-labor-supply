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
