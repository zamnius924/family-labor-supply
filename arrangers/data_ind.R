# ============================================================================
# data_ind.R
# ----------------------------------------------------------------------------
# Apply sample restrictions, clean measurement error, and create individual‑
# level labor supply and earnings variables. Produces 'data_ind'.
# ============================================================================

data_ind <- restrict_sample(
    df = all_data, 
    codes = data_source$code_ind,
    restrict_repr = TRUE, 
    restrict_repr_year = last_year,
    year_min = first_year,
    year_max = last_year,
    age_min = 25, 
    age_max = 55
  ) %>% 
  as.data.frame(.)

# ------------------------------------------------------------
# Impute missing wages and hours
# ------------------------------------------------------------
data_ind <- data_ind %>%
  filler_wage(.) %>%
  filler_lab_sup(.) %>%
  corrector_lab_sup(.)

# ------------------------------------------------------------
# Aggregate over up to three jobs
# ------------------------------------------------------------
# Hourly wage for each job
data_ind <- data_ind %>%
  mutate(
    wage_1 = wage_month_filled / lab_sup_month_filled,
    wage_2 = wage_month_filled_2 / lab_sup_month_filled_2,
    wage_3 = wage_month_filled_3 / lab_sup_month_filled_3
  )

# Total wage (sum across jobs)
data_ind$wage <- data_ind %>% 
  select(wage_1, wage_2, wage_3) %>%
  apply(1, sum, na.rm = TRUE) %>% 
  na_if(0)

# Total monthly hours
data_ind$sum_lab_sup_month <- data_ind %>% 
  select(
    lab_sup_month_filled, lab_sup_month_filled_2, lab_sup_month_filled_3
  ) %>%
  apply(1, sum, na.rm = TRUE) %>% 
  na_if(0)

# Work indicator (any job)
data_ind$sum_working <- data_ind %>% 
  select(working, working_2, working_3) %>%
  apply(1, sum, na.rm = TRUE) %>% 
  as.logical() %>% 
  as.numeric()

# Clean inconsistencies
data_ind <- data_ind %>% 
  mutate(
    sum_lab_sup_month = replace(sum_lab_sup_month, sum_working == 0, NA),
    wage = replace(wage, sum_working == 0, NA)
  )

# ------------------------------------------------------------
# Annual hours accounting for vacations and holidays
# ------------------------------------------------------------
data_ind <- data_ind %>%
  mutate(vacation = as.numeric(vacation)) %>% 
  tidyr::replace_na(list(vacation = 0)) %>%
  mutate(
    lab_sup_year = sum_lab_sup_month * 12 - (vacation + holidays) * sum_lab_sup_month / 22
  ) %>% 
  mutate(lab_sup_year = replace(lab_sup_year, lab_sup_year < 0, NA)) %>%
  relocate(sum_working, lab_sup_year, wage, .after = last_col())

# ------------------------------------------------------------
# Deflate using regional CPI
# ------------------------------------------------------------
data_ind <- data_ind %>% 
  mutate(
    wage = wage / CPI_reg * 100,
    consump_nd = consump_nd / CPI_reg * 100,
    fed_min_real = fed_min / CPI_reg * 100,
    grp_def = grp / CPI_reg * 100
  )

# ------------------------------------------------------------
# Family composition
# ------------------------------------------------------------
data_ind <- data_ind %>% 
  mutate(
    work_age = work_age_f + work_age_m,
    nwork_age = nwork_age_f + nwork_age_m,
    work_age_share = work_age / nfm
  )

# ------------------------------------------------------------
# Education level (4 categories)
# ------------------------------------------------------------
data_ind <- data_ind %>% 
  mutate(
    diplom_lev = case_when(
      diplom %in% c(1, 2, 3) ~ 1,
      diplom %in% c(4) ~ 2,
      diplom %in% c(5) ~ 3,
      diplom %in% c(6) ~ 4
    )
  )

# ------------------------------------------------------------
# Annual real labour earnings
# ------------------------------------------------------------
data_ind$earn <- data_ind$wage * data_ind$lab_sup_year

# ------------------------------------------------------------
# Remove observations with zero consumption or wage
# ------------------------------------------------------------
data_ind <- data_ind %>%
  subset((wage > 0 | is.na(wage) == TRUE) &
  (consump_nd > 0 | is.na(consump_nd) == TRUE))

# ------------------------------------------------------------
# Outlier cleaning and growth rates
# ------------------------------------------------------------
data_ind <- omit_me(data_ind)
data_ind <- metrics_diff(data_ind)