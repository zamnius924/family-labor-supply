sample_index <- unique(data_mod$id_hh)

boot_results <- boot(
  sample_index,
  bootstrap,
  R = B,
  parallel = "multicore",
  ncpus = ncpus)

# Таблица с оценками и значимостями
results <- t(cbind(
  rbind(data.frame(res_model_wage)[1,-14], apply(boot_results[["t"]][,1:13], 2, sd)),
  rbind(data.frame(res_model_pref)[1,-13], apply(boot_results[["t"]][,16:27], 2, sd))
))
colnames(results) <- c("coef", "sd")

results <- as.data.frame(round(results, 3)) %>%
  mutate(T_stat = coef / sd) %>% 
  mutate(signif = case_when(
    abs(T_stat) < qnorm(0.95) ~ "",
    abs(T_stat) >= qnorm(0.95) & abs(T_stat) < qnorm(0.975) ~ "*",
    abs(T_stat) >= qnorm(0.975) & abs(T_stat) < qnorm(0.995) ~ "**",
    abs(T_stat) >= qnorm(0.995) ~ "***")
  ) %>% 
  select(coef, sd, signif)
