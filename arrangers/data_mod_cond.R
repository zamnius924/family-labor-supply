### Генерируем рабочие датасеты
data_cond <- list()

# Датасет для вспомогательных регрессий: id_hh, year, moment conditions, log income
data_cond$df <- data_mod %>%
  mutate(
    dyM_dyM = dyM * dyM,
    dyM_dLyM = dyM * dLyM,
    dyF_dyF = dyF * dyF,
    dyF_dLyF = dyF * dLyF,
    dyM_dyF = dyM * dyF,
    dyM_dLyF = dyM * dLyF,
    dLyM_dyF = dLyM * dyF,
    dyM_dwM = dyM * dwM,
    dLyM_dwM = dLyM * dwM,
    dyM_dLwM = dyM * dLwM,
    dyF_dwF = dyF * dwF,
    dLyF_dwF = dLyF * dwF,
    dyF_dLwF = dyF * dLwF,
    dyM_dwF = dyM * dwF,
    dyM_dLwF = dyM * dLwF,
    dLyM_dwF = dLyM * dwF,
    dyF_dwM = dyF * dwM,
    dyF_dLwM = dyF * dLwM,
    dLyF_dwM = dLyF * dwM
  )

data_cond$df <- data_cond$df %>%
  left_join(
    data %>%
      select(id_ind, id_part, id_hh, year, earn, earn_part) %>%
      mutate(learnM = log(earn), learnF = log(earn_part)) %>% 
      select(-earn, -earn_part)
  ) %>%
  ungroup() %>% 
  select(id_hh, year, dyM_dyM:last_col())


# Коэффициенты для каждого моментного условия в каждом году
data_cond$coef <- data_cond$df %>%
  pivot_longer(!c(id_hh, year, learnM, learnF)) %>% 
  right_join( # аналог moments_emp
    data_cond$df %>%
      group_by(year) %>%
      summarise_all(mean, na.rm = TRUE) %>% 
      select(year, dyM_dyM:dLyF_dwM) %>%
      pivot_longer(!year) %>%
      na_omit(.) %>%
      select(-value)
  ) %>% 
  group_by(year, name) %>%
  do(
    lm(value ~ learnM + learnF, data = .) %>% coef %>% tidy
  ) %>% 
  pivot_wider(names_from = names, values_from = x) %>% 
  rename(const = '(Intercept)', coef_learnM = learnM, coef_learnF = learnF) %>% 
  arrange(factor(name, levels = colnames(data_cond$df)[-1]))

# Размер сетки
data_cond$grid_size <- 11 # нечетное, чтобы 0 попадал

# Сетка по доходам
data_cond$axis <- data_cond$df %>%
  subset(year != 2000) %>% 
  select(year, learnM, learnF) %>% 
  pivot_longer(!year) %>% 
  group_by(name, year) %>%
  summarise(
    #mean = mean(value, na.rm = TRUE),
    min = mean(value, na.rm = TRUE) - 1.5 * sd(value, na.rm = TRUE),
    max = mean(value, na.rm = TRUE) + 1.5 * sd(value, na.rm = TRUE)
  ) %>% 
  rowwise() %>% 
  mutate(seq = list(seq(min, max, length.out = data_cond$grid_size))) %>% 
  select(-min, -max) %>% 
  unnest(cols = seq) %>%
  group_by(name, year) %>% 
  mutate(num = seq(-1.5, 1.5, length.out = data_cond$grid_size)) %>% 
  pivot_wider(names_from = name, values_from = seq) %>% 
  arrange(num, year)

data_cond$grid <- data_cond$axis %>%
  group_by(year) %>% 
  expand(learnM, learnF) %>% 
  left_join(data_cond$axis %>% select(year, numM = num, learnM)) %>% 
  left_join(data_cond$axis %>% select(year, numF = num, learnF)) %>% 
  arrange(numM, numF) %>% 
  group_by(numM, numF) %>% 
  mutate(num = cur_group_id())