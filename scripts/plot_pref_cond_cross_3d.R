# 3D графики --------------------------------------------------------------

plot_pref_cond_cross_3d <- list()

### Перекрестные коэффициенты эластичности
# Фриш
plot_pref_cond_cross_3d$fig1 <- plot_ly(
  x = seq(-1.5, 1.5, length.out = data_cond$grid_size),
  y = seq(-1.5, 1.5, length.out = data_cond$grid_size),
  scene = "scene"
) %>%
  add_surface(
    z = ~ unclass(xtabs(
      k_hM_uF ~ numF + numM,
      data = res_model_pref_cond$coef
    )),
    cmin = -0.6,
    cmax = 0.6,
    contours = list(z = list(show = TRUE, color = "white"))
  ) %>% 
  add_surface(
    z = ~ unclass(xtabs(
      k_hM_uF ~ numF + numM,
      data = res_model_pref_cond$CI_u
    )), 
    opacity = 0.25,
    colorscale = list(c(0, 1), c("rgb(107,184,255)", "rgb(90,90,124)"))
  ) %>% 
  add_surface(
    z = ~ unclass(xtabs(
      k_hM_uF ~ numF + numM,
      data = res_model_pref_cond$CI_l
    )), 
    opacity = 0.25,
    colorscale = list(c(0, 1), c("rgb(107,184,255)", "rgb(90,90,124)"))
  ) %>% 
  add_surface(
    z = matrix(0, ncol = data_cond$grid_size, nrow = data_cond$grid_size), 
    opacity = 0.4,
    colorscale = list(c(0, 1), c("rgb(255,107,184)", "rgb(128,0,64)"))
  ) %>% 
  layout(scene = list(
    xaxis = list(title = "Male income"), # "Male income (in standard deviations)"
    yaxis = list(title = "Female income"), # Female income (in standard deviations)
    zaxis = list(title = "Male Frisch cross-elasticity"), # Male Frisch labor supply elasticity
    camera = list(eye = list(x = -1.5, y = -1.5, z = 0.3))
  )) %>% 
  #config(mathjax = "cdn") %>%
  hide_colorbar()

plot_pref_cond_cross_3d$fig2 <- plot_ly(
  x = seq(-1.5, 1.5, length.out = data_cond$grid_size),
  y = seq(-1.5, 1.5, length.out = data_cond$grid_size),
  scene = "scene2"
) %>%
  add_surface(
    z = ~ unclass(xtabs(
      k_hF_uM ~ numF + numM,
      data = res_model_pref_cond$coef
    )),
    cmin = -0.6,
    cmax = 0.6,
    contours = list(z = list(show = TRUE, color = "white"))
  ) %>% 
  add_surface(
    z = ~ unclass(xtabs(
      k_hF_uM ~ numF + numM,
      data = res_model_pref_cond$CI_u
    )), 
    opacity = 0.25,
    colorscale = list(c(0, 1), c("rgb(107,184,255)", "rgb(90,90,124)"))
  ) %>% 
  add_surface(
    z = ~ unclass(xtabs(
      k_hF_uM ~ numF + numM,
      data = res_model_pref_cond$CI_l
    )), 
    opacity = 0.25,
    colorscale = list(c(0, 1), c("rgb(107,184,255)", "rgb(90,90,124)"))
  ) %>%
  add_surface(
    z = matrix(0, ncol = data_cond$grid_size, nrow = data_cond$grid_size), 
    opacity = 0.4,
    colorscale = list(c(0, 1), c("rgb(255,107,184)", "rgb(128,0,64)"))
  ) %>% 
  layout(scene2 = list(
    xaxis = list(title = "Male income"),
    yaxis = list(title = "Female income"),
    zaxis = list(title = "Female Frisch cross-elasticity"),
    camera = list(eye = list(x = -1.5, y = -1.5, z = 0.3))
  )) %>% 
  hide_colorbar()

# Маршалл
plot_pref_cond_cross_3d$fig3 <- plot_ly(
  x = seq(-1.5, 1.5, length.out = data_cond$grid_size),
  y = seq(-1.5, 1.5, length.out = data_cond$grid_size),
  scene = "scene3"
) %>%
  add_surface(
    z = ~ unclass(xtabs(
      k_hM_vF ~ numF + numM,
      data = res_model_pref_cond$coef
    )),
    cmin = -0.6,
    cmax = 0.6,
    contours = list(z = list(show = TRUE, color = "white"))
  ) %>% 
  add_surface(
    z = ~ unclass(xtabs(
      k_hM_vF ~ numF + numM,
      data = res_model_pref_cond$CI_u
    )), 
    opacity = 0.25,
    colorscale = list(c(0, 1), c("rgb(107,184,255)", "rgb(90,90,124)"))
  ) %>% 
  add_surface(
    z = ~ unclass(xtabs(
      k_hM_vF ~ numF + numM,
      data = res_model_pref_cond$CI_l
    )), 
    opacity = 0.25,
    colorscale = list(c(0, 1), c("rgb(107,184,255)", "rgb(90,90,124)"))
  ) %>% 
  add_surface(
    z = matrix(0, ncol = data_cond$grid_size, nrow = data_cond$grid_size), 
    opacity = 0.4,
    colorscale = list(c(0, 1), c("rgb(255,107,184)", "rgb(128,0,64)"))
  ) %>% 
  layout(scene3 = list(
    xaxis = list(title = "Male income"),
    yaxis = list(title = "Female income"),
    zaxis = list(title = "Male Marshallian cross-elasticity"),
    camera = list(eye = list(x = -1.5, y = -1.5, z = 0.6))
  )) %>% 
  hide_colorbar()

plot_pref_cond_cross_3d$fig4 <- plot_ly(
  x = seq(-1.5, 1.5, length.out = data_cond$grid_size),
  y = seq(-1.5, 1.5, length.out = data_cond$grid_size),
  scene = "scene4"
) %>%
  add_surface(
    z = ~ unclass(xtabs(
      k_hF_vM ~ numF + numM,
      data = res_model_pref_cond$coef
    )),
    cmin = -0.6,
    cmax = 0.6,
    contours = list(z = list(show = TRUE, color = "white"))
  ) %>% 
  add_surface(
    z = ~ unclass(xtabs(
      k_hF_vM ~ numF + numM,
      data = res_model_pref_cond$CI_u
    )), 
    opacity = 0.25,
    colorscale = list(c(0, 1), c("rgb(107,184,255)", "rgb(90,90,124)"))
  ) %>% 
  add_surface(
    z = ~ unclass(xtabs(
      k_hF_vM ~ numF + numM,
      data = res_model_pref_cond$CI_l
    )), 
    opacity = 0.25,
    colorscale = list(c(0, 1), c("rgb(107,184,255)", "rgb(90,90,124)"))
  ) %>% 
  add_surface(
    z = matrix(0, ncol = data_cond$grid_size, nrow = data_cond$grid_size), 
    opacity = 0.4,
    colorscale = list(c(0, 1), c("rgb(255,107,184)", "rgb(128,0,64)"))
  ) %>% 
  layout(scene4 = list(
    xaxis = list(title = "Male income"),
    yaxis = list(title = "Female income"),
    zaxis = list(title = "Female Marshallian cross-elasticity"),
    camera = list(eye = list(x = -1.5, y = -1.5, z = 0.6))
  )) %>% 
  hide_colorbar()

plot_pref_cond_cross_3d$fig <- subplot(
  plot_pref_cond_cross_3d$fig1,
  plot_pref_cond_cross_3d$fig2,
  plot_pref_cond_cross_3d$fig3,
  plot_pref_cond_cross_3d$fig4
)

plot_pref_cond_cross_3d$fig <- plot_pref_cond_cross_3d$fig %>% 
  layout(
    scene = list(
      domain = list(x = c(0, 0.7), y = c(0.5, 1)),
      aspectmode = "cube",
      #xaxis = list(title = "Male income"), # "Male income (in standard deviations)"
      #yaxis = list(title = "Female income"), # Female income (in standard deviations)
      #zaxis = list(title = "Male Frisch cross-elasticity")),
      xaxis = list(title = "Доход мужа"),
      yaxis = list(title = "Доход жены"),
      zaxis = list(title = "Эластичность по Фришу для мужчин")
    ),
    scene2 = list(
      domain = list(x = c(0.3, 1), y = c(0.5, 1)),
      aspectmode = "cube",
      #xaxis = list(title = "Male income"),
      #yaxis = list(title = "Female income"),
      #zaxis = list(title = "Female Frisch cross-elasticity")),
      xaxis = list(title = "Доход мужа"),
      yaxis = list(title = "Доход жены"),
      zaxis = list(title = "Эластичность по Фришу для женщин")
    ),
    scene3 = list(
      domain = list(x = c(0, 0.7), y = c(0, 0.5)),
      aspectmode = "cube",
      #xaxis = list(title = "Male income"),
      #yaxis = list(title = "Female income"),
      #zaxis = list(title = "Male Marshallian cross-elasticity")),
      xaxis = list(title = "Доход мужа"),
      yaxis = list(title = "Доход жены"),
      zaxis = list(title = "Эластичность по Маршаллу для мужчин")
    ),
    scene4 = list(
      domain = list(x = c(0.3, 1), y = c(0, 0.5)),
      aspectmode = "cube",
      #xaxis = list(title = "Male income"),
      #yaxis = list(title = "Female income"),
      #zaxis = list(title = "Female Marshallian cross-elasticity")))
      xaxis = list(title = "Доход мужа"),
      yaxis = list(title = "Доход жены"),
      zaxis = list(title = "Эластичность по Мрашаллу для женщин")
    )
  )