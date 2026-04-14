# ============================================================================
# internal_fit.R
# ----------------------------------------------------------------------------
# Compute internal fit (model vs. data moments) and produce bar plots.
# ============================================================================

fit_plot <- list()

# Compute the fitted moments
fit_plot$fit <- internal_fit(
  res_model_pref,
  res_model_wage,
  data_mod
) %>%
  pivot_longer(!type)

# Factor the moment names for consistent ordering
fit_plot$fit <- fit_plot$fit %>% 
  mutate(name = factor(
    name,
    levels = unique(fit_plot$fit$name)
  )) %>% 
  arrange(name)

# Split into two panels for readability
fit_plot$fit_1 <- fit_plot$fit[1:34,]
fit_plot$fit_2 <- fit_plot$fit[35:68,]

# First panel (moments 1–34)
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
    labels = c("Data", "Model")
  ) +
  labs(
    x = element_blank(), 
    y = element_blank()
  ) +
  theme_bw()

# Second panel (moments 35–68)
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
    labels = c("Data", "Model")
  ) +
  labs(
    x = element_blank(), 
    y = element_blank()
  ) +
  theme_bw()

# Combine the two panels
fit_plot$plot <- fit_plot$plot_1 + fit_plot$plot_2 + 
  plot_layout(nrow = 1, guides = "collect")
  