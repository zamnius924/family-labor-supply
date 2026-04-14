# ============================================================================
# source_data.R
# ----------------------------------------------------------------------------
# Build the initial data_source list by linking RLMS individual, household,
# and kinship files. Create cross‑walk identifiers for individuals and spouses.
# ============================================================================

data_source <- list()

# ------------------------------------------------------------
# Individual identifier cross‑walk
# ------------------------------------------------------------
data_source$code_ind <- rlms$all_code_ind %>% 
  select(
    id_ind = idind, sex, aid_i, bid_i, cid_i, did_i,
    eid_i, fid_i, gid_i, hid_i, iid_i,
    jid_i, kid_i, lid_i, mid_i, nid_i,
    oid_i, pid_i, qid_i, rid_i, sid_i,
    tid_i, uid_i, vid_i, wid_i, xid_i, yid_i
  ) %>% 
  pivot_longer(
    cols = ends_with("_i"), 
    names_to = "id_i_name", 
    values_to = "id_i"
  ) %>%
  arrange(id_ind, id_i_name)

# Wave identifiers
data_source$code_year <- rlms$all_data_ind %>% 
  select(id_w, year) %>%
  distinct() %>% 
  arrange(year) %>%
  mutate(id_i_name = unique(data_source$code_ind$id_i_name))

# Combine: each individual in each wave gets a wave‑specific identifier
data_source$code_ind <- data_source$code_ind %>% 
  left_join(data_source$code_year) %>% 
  relocate(id_i, .after = id_ind)

# ------------------------------------------------------------
# Individual‑level data
# ------------------------------------------------------------
data_source$data_ind <- rlms$all_data_ind %>% 
  select(
    id_w, id_h, id_ind = idind, id_i,
    year, popul, age, origsm,
    status, psu, sex = h5, family = marst, 
    diplom, diplom_exp = diplom_1,
    working = j77, lab_sup = j6.1a, lab_sup_week = j6.2,
    lab_sup_last_month = j8, wage_last_month = j10, wage_month = j13.2,
    working_2.1 = j32, working_2.2 = j32.1, lab_sup_2 = j36.1a,
    lab_sup_week_2 = j36.2, lab_sup_last_month_2 = j38, wage_last_month_2 = j40,
    working_3 = j56, lab_sup_last_month_3 = j59, wage_last_month_3 = j57, 
    regular = j58, vacation = j21b, vac_status = j1, region) %>% 
  arrange(id_ind, year)

# Replace raw id_i with the cross‑walked version
data_source$data_ind <- data_source$data_ind %>%
  select(-id_i) %>% left_join(data_source$code_ind)

# ------------------------------------------------------------
# Household‑level data
# ------------------------------------------------------------
# Non‑durable consumption components
data_source$data_consump_nd_hh <- rlms$all_data_hh %>%
  select(e4, e8.1b, e8.2b, e8.3b, e9.1b,
    e9.2b, e9.3b, e9.4b, e9.4.1b, e9.5b,
    e9.6b, e9.7b, e9.8b, e9.9b, e9.10b,
    e9.11b, e11, e13.1b, e13.2b, e13.21b,
    e13.22b, e13.23b, e13.24b, e13.3b, e13.31b,
    e13.32b, e13.33b, e13.34b, e13.4b, e13.12b,
    e13.13b)
data_source$consump_nd_names <- paste(
  "consump_nd_",
  1:ncol(data_source$data_consump_nd_hh),
  sep = ""
) 
colnames(data_source$data_consump_nd_hh) <- data_source$consump_nd_names

# Household characteristics
data_source$data_hh <- rlms$all_data_hh %>%
  select(id_w, id_h, num_head = a8, nfm) %>%
  cbind(., data_source$data_consump_nd_hh) %>%
  arrange(id_h, id_w)

data_source[c("data_consump_nd_hh")] <- NULL

# Total non‑durable consumption (monthly, then annualised)
data_source$data_hh$consump_nd <- data_source$data_hh %>%
  select(all_of(data_source$consump_nd_names)) %>% 
  apply(1, sum, na.rm = TRUE)
data_source$data_hh$consump_nd <- data_source$data_hh$consump_nd * 12

# ------------------------------------------------------------
# Additional household characteristics (children, working‑age members)
# ------------------------------------------------------------
data_source$data_add <- rlms$all_data_add %>%
  select(id_h, id_w, child_lit = ncat1, child_teen = ncat2, work_age_m = ncat3,
    work_age_f = ncat4, nwork_age_m = ncat5, nwork_age_f = ncat6)

# ------------------------------------------------------------
# Kinship links (spouse identifiers)
# ------------------------------------------------------------
data_source$code_rel <- rlms$all_code_rel %>% 
  select(id_w = id_w, id_i = id_i, id_i_part = r01a1) %>%
  arrange(id_i, id_w)

# ------------------------------------------------------------
# Augment the cross‑walk with spouse information and representativeness
# ------------------------------------------------------------
data_source$code_ind <- data_source$code_ind %>%
  left_join(
    data_source$data_ind %>% select(id_ind, year, origsm)
  ) %>% 
  arrange(id_ind)

# Add id_i_part codes
data_source$code_ind <- data_source$code_ind %>% 
  left_join(data_source$code_rel)

# Add id_part codes
data_source$code_ind <- data_source$code_ind %>% 
  left_join(
    data_source$code_ind %>% select(id_part = id_ind, year, id_i_part = id_i),
    na_matches = "never"
  )

# Add marital status
data_source$code_ind <- data_source$code_ind %>% 
  left_join(
    data_source$data_ind %>% select(id_i, year, family)
  )

data_source$code_ind <- data_source$code_ind %>%
  select(year, id_ind, id_i, id_part, id_i_part, origsm, family, sex)

# ------------------------------------------------------------
# Map numeric region codes to readable names
# ------------------------------------------------------------
data_source$data_ind <- data_source$data_ind %>% 
  mutate(region_rus = case_when(
    region == 1 ~ "Ленинградская область", 
    region == 9 ~ "Краснодарский край", 
    region == 10 ~ "Удмуртская Республика",
    region == 12 ~ "Пермский край", 
    region == 14 ~ "Калужская область", 
    region == 33 ~ "Тамбовская область",
    region == 39 ~ "Вологодская область", 
    region == 45 ~ "Республика Татарстан", 
    region == 46 ~ "Курганская область",
    region == 47 ~ "Оренбургская область", 
    region == 48 ~ "Чувашская Республика", 
    region == 52 ~ "Ставропольский край",
    region == 58 ~ "Алтайский край", 
    region == 66 ~ "Красноярский край", 
    region == 67 ~ "Тверская область",
    region == 70 ~ "Саратовская область", 
    region == 71 ~ "Томская область", 
    region == 72 ~ "Липецкая область",
    region == 73 ~ "Красноярский край", 
    region == 77 ~ "Кабардино-Балкарская Республика", 
    region == 84 ~ "Алтайский край",
    region == 86 ~ "Ханты-Мансийский автономный округ – Югра", 
    region == 89 ~ "Республика Коми", 
    region == 92 ~ "Приморский край",
    region == 93 ~ "Амурская область", 
    region == 100 ~ "Саратовская область", 
    region == 105 ~ "Республика Коми",
    region == 106 ~ "Челябинская область", 
    region == 107 ~ "Челябинская область", 
    region == 116 ~ "Нижегородская область",
    region == 117 ~ "Пензенская область", 
    region == 129 ~ "Краснодарский край", 
    region == 135 ~ "Смоленская область",
    region == 136 ~ "Тульская область", 
    region == 137 ~ "Ростовская область", 
    region == 138 ~ "г. Москва",
    region == 140 ~ "г. Москва", 
    region == 141 ~ "г. Санкт-Петербург", 
    region == 142 ~ "Московская область",
    region == 161 ~ "Новосибирская область")
  )

