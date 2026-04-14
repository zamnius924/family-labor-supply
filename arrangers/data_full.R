# ============================================================================
# data_full.R
# ----------------------------------------------------------------------------
# Convert individual‑level data into household‑level data by matching spouses.
# Produces 'data' (household panel).
# ============================================================================


data <- data_ind %>% 
  partner_merge(., data_source) %>% # attach spouse variables
  partner_df(.) %>% # keep only couples, male as reference
  partner_id() %>% # create unique household id
  select( 
    id_hh, id_ind, id_part, year, sex, sex_part, 
    sum_working, sum_working_part, wage, wage_part,
    earn, earn_part, consump_nd, lab_sup_year, lab_sup_year_part, 
    dlwage, dlwage_part, dlearn, dlearn_part, dlcons, 
    dlhours, dlhours_part, age, age_part, diplom_lev, diplom_lev_part,
    region, status, fed_min_real, nfm, child_lit, child_teen, 
    work_age_share, grp_def
  )

data <- data %>% subset(year %in% first_year:last_year)