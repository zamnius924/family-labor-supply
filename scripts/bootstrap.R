# ============================================================================
# bootstrap.R
# ----------------------------------------------------------------------------
# Bootstrap inference for the baseline model (wage + preference parameters).
# Uses household clustering (resampling households with replacement).
# ============================================================================

# Vector of unique household IDs to resample
sample_index <- unique(data_mod$id_hh)

# Run bootstrap (parallel, using ncpus cores)
boot_results <- boot(
  sample_index,
  bootstrap,
  R = B,
  parallel = "multicore",
  ncpus = ncpus)

# Combine point estimates and bootstrap standard errors
results <- t(cbind(
  rbind(data.frame(res_model_wage)[1,-14],
        apply(boot_results[["t"]][,1:13], 2, sd)),
  rbind(data.frame(res_model_pref)[1,-13],
        apply(boot_results[["t"]][,16:27], 2, sd))
))
colnames(results) <- c("coef", "sd")

# Compute t‑statistics and significance stars
results <- as.data.frame(round(results, 3)) %>%
  mutate(T_stat = coef / sd) %>% 
  mutate(signif = case_when(
    abs(T_stat) < qnorm(0.95) ~ "",
    abs(T_stat) >= qnorm(0.95) & abs(T_stat) < qnorm(0.975) ~ "*",
    abs(T_stat) >= qnorm(0.975) & abs(T_stat) < qnorm(0.995) ~ "**",
    abs(T_stat) >= qnorm(0.995) ~ "***")
  ) %>% 
  select(coef, sd, signif)
