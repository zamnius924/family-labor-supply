fit <- internal_fit(res_model_pref, res_model_wage, data_mod) %>%
  pivot_longer(!type) %>%
  mutate(name = factor(name, levels = unique(fit$name))) %>% 
  arrange(name)

fit_1 <- fit[1:34,]
fit_2 <- fit[35:68,]

fit_plot1 <- ggplot(data = fit_1, aes(x = as.factor(name), y = value, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() + 
  scale_x_discrete(labels = c(
    TeX("$cov(\\Delta w_{1,t};\\Delta w_{1,t})$"),
    TeX("$cov(\\Delta w_{1,t};\\Delta w_{1,t-1})$"),
    TeX("$cov(\\Delta w_{2,t};\\Delta w_{2,t})$"),
    TeX("$cov(\\Delta w_{2,t};\\Delta w_{2,t-1})$"),
    TeX("$cov(\\Delta w_{1,t};\\Delta w_{2,t})$"),
    TeX("$cov(\\Delta w_{1,t};\\Delta w_{2,t-1})$"),
    TeX("$cov(\\Delta w_{1,t-1};\\Delta w_{2,t})$"),
    TeX("$cov(\\Delta c_{t};\\Delta c_{t})$"),
    TeX("$cov(\\Delta c_{t};\\Delta c_{t-1})$"),
    TeX("$cov(\\Delta c_{t};\\Delta w_{1,t})$"),
    TeX("$cov(\\Delta c_{t};\\Delta w_{2,t})$"),
    TeX("$cov(\\Delta c_{t};\\Delta w_{1,t-1})$"),
    TeX("$cov(\\Delta c_{t};\\Delta w_{2,t-1})$"),
    TeX("$cov(\\Delta c_{t-1};\\Delta w_{1,t})$"),
    TeX("$cov(\\Delta c_{t-1};\\Delta w_{2,t})$"),
    TeX("$cov(\\Delta y_{1,t};\\Delta y_{1,t})$"),
    TeX("$cov(\\Delta y_{1,t};\\Delta y_{1,t-1})$"))) +
  scale_fill_discrete(
    name = element_blank(),
    #labels = c("Data", "Model")) +
    labels = c("Данные", "Модель")) +
  labs(
    x = element_blank(), 
    y = element_blank()) +
  theme_bw()

fit_plot2 <- ggplot(data = fit_2, aes(x = as.factor(name), y = value, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() + 
  scale_x_discrete(labels = c(
    TeX("$cov(\\Delta y_{2,t};\\Delta y_{2,t})$"),
    TeX("$cov(\\Delta y_{2,t};\\Delta y_{2,t-1})$"),
    TeX("$cov(\\Delta y_{1,t};\\Delta y_{2,t})$"),
    TeX("$cov(\\Delta y_{1,t};\\Delta y_{2,t-1})$"),
    TeX("$cov(\\Delta y_{1,t-1};\\Delta y_{2,t})$"),
    TeX("$cov(\\Delta y_{1,t};\\Delta w_{1,t})$"),
    TeX("$cov(\\Delta y_{1,t-1};\\Delta w_{1,t})$"),
    TeX("$cov(\\Delta y_{1,t};\\Delta w_{1,t-1})$"),
    TeX("$cov(\\Delta y_{2,t};\\Delta w_{2,t})$"),
    TeX("$cov(\\Delta y_{2,t-1};\\Delta w_{2,t})$"),
    TeX("$cov(\\Delta y_{2,t};\\Delta w_{2,t-1})$"),
    TeX("$cov(\\Delta y_{1,t};\\Delta w_{2,t})$"),
    TeX("$cov(\\Delta y_{1,t};\\Delta w_{2,t-1})$"),
    TeX("$cov(\\Delta y_{1,t-1};\\Delta w_{2,t})$"),
    TeX("$cov(\\Delta y_{2,t};\\Delta w_{1,t})$"),
    TeX("$cov(\\Delta y_{2,t};\\Delta w_{1,t-1})$"),
    TeX("$cov(\\Delta y_{2,t-1};\\Delta w_{1,t})$"))) +
  scale_fill_discrete(
    name = element_blank(),
    #labels = c("Data", "Model")) +
    labels = c("Данные", "Модель")) +
  labs(
    x = element_blank(), 
    y = element_blank()) +
  theme_bw()

print(fit_plot1 + fit_plot2 + plot_layout(nrow = 1, guides = "collect"))
