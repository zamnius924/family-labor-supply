fit_plot <- list()

fit_plot$fit <- internal_fit(
  res_model_pref,
  res_model_wage,
  data_mod
) %>%
  pivot_longer(!type)
fit_plot$fit <- fit_plot$fit %>% 
  mutate(name = factor(
    name,
    levels = unique(fit_plot$fit$name)
  )) %>% 
  arrange(name)

# Разбивка по полотнам
fit_plot$fit_1 <- fit_plot$fit[1:34,]
fit_plot$fit_2 <- fit_plot$fit[35:68,]

fit_plot$plot_1 <- ggplot(
  data = fit_plot$fit_1, 
  aes(x = as.factor(name),
      y = value,
      fill = type)
) +
  geom_bar(
    stat = "identity",
    position = position_dodge()
  ) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() + 
  scale_x_discrete(labels = c(
    latex2exp::TeX("$cov(\\Delta w_{1,t};\\Delta w_{1,t})$"),
    latex2exp::TeX("$cov(\\Delta w_{1,t};\\Delta w_{1,t-1})$"),
    latex2exp::TeX("$cov(\\Delta w_{2,t};\\Delta w_{2,t})$"),
    latex2exp::TeX("$cov(\\Delta w_{2,t};\\Delta w_{2,t-1})$"),
    latex2exp::TeX("$cov(\\Delta w_{1,t};\\Delta w_{2,t})$"),
    latex2exp::TeX("$cov(\\Delta w_{1,t};\\Delta w_{2,t-1})$"),
    latex2exp::TeX("$cov(\\Delta w_{1,t-1};\\Delta w_{2,t})$"),
    latex2exp::TeX("$cov(\\Delta c_{t};\\Delta c_{t})$"),
    latex2exp::TeX("$cov(\\Delta c_{t};\\Delta c_{t-1})$"),
    latex2exp::TeX("$cov(\\Delta c_{t};\\Delta w_{1,t})$"),
    latex2exp::TeX("$cov(\\Delta c_{t};\\Delta w_{2,t})$"),
    latex2exp::TeX("$cov(\\Delta c_{t};\\Delta w_{1,t-1})$"),
    latex2exp::TeX("$cov(\\Delta c_{t};\\Delta w_{2,t-1})$"),
    latex2exp::TeX("$cov(\\Delta c_{t-1};\\Delta w_{1,t})$"),
    latex2exp::TeX("$cov(\\Delta c_{t-1};\\Delta w_{2,t})$"),
    latex2exp::TeX("$cov(\\Delta y_{1,t};\\Delta y_{1,t})$"),
    latex2exp::TeX("$cov(\\Delta y_{1,t};\\Delta y_{1,t-1})$")
  )) +
  scale_fill_discrete(
    name = element_blank(),
    #labels = c("Data", "Model")) +
    labels = c("Данные", "Модель")
  ) +
  labs(
    x = element_blank(), 
    y = element_blank()
  ) +
  theme_bw()

fit_plot$plot_2 <- ggplot(
  data = fit_plot$fit_2,
  aes(x = as.factor(name),
      y = value,
      fill = type)
) +
  geom_bar(
    stat = "identity",
    position = position_dodge()
  ) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() + 
  scale_x_discrete(labels = c(
    latex2exp::TeX("$cov(\\Delta y_{2,t};\\Delta y_{2,t})$"),
    latex2exp::TeX("$cov(\\Delta y_{2,t};\\Delta y_{2,t-1})$"),
    latex2exp::TeX("$cov(\\Delta y_{1,t};\\Delta y_{2,t})$"),
    latex2exp::TeX("$cov(\\Delta y_{1,t};\\Delta y_{2,t-1})$"),
    latex2exp::TeX("$cov(\\Delta y_{1,t-1};\\Delta y_{2,t})$"),
    latex2exp::TeX("$cov(\\Delta y_{1,t};\\Delta w_{1,t})$"),
    latex2exp::TeX("$cov(\\Delta y_{1,t-1};\\Delta w_{1,t})$"),
    latex2exp::TeX("$cov(\\Delta y_{1,t};\\Delta w_{1,t-1})$"),
    latex2exp::TeX("$cov(\\Delta y_{2,t};\\Delta w_{2,t})$"),
    latex2exp::TeX("$cov(\\Delta y_{2,t-1};\\Delta w_{2,t})$"),
    latex2exp::TeX("$cov(\\Delta y_{2,t};\\Delta w_{2,t-1})$"),
    latex2exp::TeX("$cov(\\Delta y_{1,t};\\Delta w_{2,t})$"),
    latex2exp::TeX("$cov(\\Delta y_{1,t};\\Delta w_{2,t-1})$"),
    latex2exp::TeX("$cov(\\Delta y_{1,t-1};\\Delta w_{2,t})$"),
    latex2exp::TeX("$cov(\\Delta y_{2,t};\\Delta w_{1,t})$"),
    latex2exp::TeX("$cov(\\Delta y_{2,t};\\Delta w_{1,t-1})$"),
    latex2exp::TeX("$cov(\\Delta y_{2,t-1};\\Delta w_{1,t})$")
  )) +
  scale_fill_discrete(
    name = element_blank(),
    #labels = c("Data", "Model")) +
    labels = c("Данные", "Модель")
  ) +
  labs(
    x = element_blank(), 
    y = element_blank()
  ) +
  theme_bw()

fit_plot$plot <- fit_plot$plot_1 + fit_plot$plot_2 + 
  plot_layout(nrow = 1, guides = "collect")

#print(fit_plot1 + fit_plot2 + plot_layout(nrow = 1, guides = "collect"))
