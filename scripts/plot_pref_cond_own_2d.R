# 2D графики --------------------------------------------------------------

plot_pref_cond_own_2d <- list()

### Прямые коэффициенты эластичности
# Оценки коэффициентов
plot_pref_cond_own_2d$coefM <- res_model_pref_cond$coef %>% 
  subset(numM %in% c("0.3", "0.9", "1.5"))
plot_pref_cond_own_2d$coefF <- res_model_pref_cond$coef %>%
  subset(numF %in% c("0.3", "0.9", "1.5")) # numF >= 0

# Доверительные интервалы
plot_pref_cond_own_2d$CIM <- rbind(
  res_model_pref_cond$CI_u %>% mutate(type = "u"), 
  res_model_pref_cond$CI_l %>% mutate(type = "l")
) %>% 
  subset(numM %in% c("0.3", "0.9", "1.5")) %>% 
  pivot_wider(
    names_from = type,
    values_from = !c(num, numM, numF, type)
  )
plot_pref_cond_own_2d$CIF <- rbind(
  res_model_pref_cond$CI_u %>% mutate(type = "u"), 
  res_model_pref_cond$CI_l %>% mutate(type = "l")
) %>% 
  subset(numF %in% c("0.3", "0.9", "1.5")) %>% # numF >= 0
  pivot_wider(
    names_from = type,
    values_from = !c(num, numM, numF, type)
  )

# График: эластичности для мужчин
plot_pref_cond_own_2d$fig1 <- ggplot() +
  geom_line(
    data = plot_pref_cond_own_2d$coefM, 
    aes(x = numF,
        y = k_hM_uM,
        color = as.factor(numM)), 
    linewidth = 1.5
  ) +
  geom_point(
    data = plot_pref_cond_own_2d$coefM,
    aes(x = numF,
        y = k_hM_uM,
        color = as.factor(numM)),
    size = 2
  ) +
  geom_ribbon(
    data = plot_pref_cond_own_2d$CIM,
    aes(x = numF,
        ymin = k_hM_uM_l,
        ymax = k_hM_uM_u,
        fill = as.factor(numM)),
    alpha = 0.1
  ) +
  geom_errorbar(
    data = plot_pref_cond_own_2d$CIM,
    aes(x = numF,
        ymin = k_hM_uM_l,
        ymax = k_hM_uM_u,
        color = as.factor(numM)), 
    size = 0.2,
    width = 0,
    position = position_dodge(0.05)
  ) +
  geom_line(
    data = plot_pref_cond_own_2d$CIM,
    aes(x = numF,
        y = k_hM_uM_l,
        color = as.factor(numM)),
    linetype = 2,
    linewidth = 0.2
  ) +
  geom_line(
    data = plot_pref_cond_own_2d$CIM,
    aes(x = numF,
        y = k_hM_uM_u,
        color = as.factor(numM)),
    linetype = 2,
    linewidth = 0.2
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  #ylim(-0.1, 1.2) +
  #labs(x = "Female income", y = "Male Frisch elasticity", color = "Male income", fill = "Male income") +
  labs(
    x = "Доход жены",
    y = "Эластиность по Фришу для мужчин",
    color = "Доход мужа",
    fill = "Доход мужа"
  ) +
  theme_classic() + 
  theme(legend.position = "bottom")

# График: эластичности для женщин
plot_pref_cond_own_2d$fig2 <- ggplot() +
  geom_line(
    data = plot_pref_cond_own_2d$coefF,
    aes(x = numM,
        y = k_hF_uF,
        color = as.factor(numF)),
    linewidth = 1.5
  ) +
  geom_point(
    data = plot_pref_cond_own_2d$coefF,
    aes(x = numM,
        y = k_hF_uF,
        color = as.factor(numF)),
    size = 2
  ) +
  geom_ribbon(
    data = plot_pref_cond_own_2d$CIF,
    aes(x = numM,
        ymin = k_hF_uF_l,
        ymax = k_hF_uF_u,
        fill = as.factor(numF)),
    alpha = 0.1
  ) +
  geom_errorbar(
    data = plot_pref_cond_own_2d$CIF,
    aes(x = numM,
        ymin = k_hF_uF_l,
        ymax = k_hF_uF_u,
        color = as.factor(numF)), 
    size = 0.2,
    width = 0,
    position = position_dodge(0.05)
  ) +
  geom_line(
    data = plot_pref_cond_own_2d$CIF,
    aes(x = numM,
        y = k_hF_uF_l,
        color = as.factor(numF)),
    linetype = 2,
    linewidth = 0.2
  ) +
  geom_line(
    data = plot_pref_cond_own_2d$CIF,
    aes(x = numM,
        y = k_hF_uF_u,
        color = as.factor(numF)),
    linetype = 2,
    linewidth = 0.2
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  #ylim(-0.1, 1.2) +
  #labs(x = "Male income", y = "Female Frisch elasticity", color = "Female income", fill = "Female income") +
  labs(
    x = "Доход мужа",
    y = "Эластиность по Фришу для женщин",
    color = "Доход жены",
    fill = "Доход жены"
  ) +
  theme_classic() + 
  theme(legend.position = "bottom")

# Общий график
plot_pref_cond_own_2d$fig <- plot_pref_cond_own_2d$fig1 + 
  plot_pref_cond_own_2d$fig2 + plot_layout(nrow = 1)
