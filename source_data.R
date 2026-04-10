data_source <- list()

### Исправление кодов
# Создадим таблицу с кодами индивидов
data_source$code_ind <- rlms$all_code_ind %>% 
  select(
    id_ind = idind, sex, aid_i, bid_i, cid_i, did_i,
    eid_i, fid_i, gid_i, hid_i, iid_i,
    jid_i, kid_i, lid_i, mid_i, nid_i,
    oid_i, pid_i, qid_i, rid_i, sid_i,
    tid_i, uid_i, vid_i, wid_i, xid_i, yid_i) %>% 
  pivot_longer(
    cols = ends_with("_i"), 
    names_to = "id_i_name", 
    values_to = "id_i") %>%
  arrange(id_ind, id_i_name)

# Создадим таблицу с кодами волн
data_source$code_year <- rlms$all_data_ind %>% 
  select(id_w, year) %>%
  distinct() %>% 
  arrange(year) %>%
  mutate(id_i_name = unique(data_source$code_ind$id_i_name))

# Создадим таблицу с кодами индивидов и волн
# Здесь представлены индивиды, участвовавшие когда-либо в опросе, даже за годы,
# когда они не проходили интервью
data_source$code_ind <- data_source$code_ind %>% 
  left_join(data_source$code_year) %>% 
  relocate(id_i, .after = id_ind)


### ОБД индивиды
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

# Исправим коды в файле с индивидами
data_source$data_ind <- data_source$data_ind %>%
  select(-id_i) %>% left_join(data_source$code_ind)

# Здесь не должно быть пропусков
summary(data_source$data_ind$id_i)
summary(data_source$data_ind$id_h)


### ОБД домохозяства
# non-durable consumption data
data_source$data_consump_nd_hh <- rlms$all_data_hh %>% # сюда добавляем переменные по потреблению
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
    sep = "") # название всех переменных потребления
colnames(data_source$data_consump_nd_hh) <- data_source$consump_nd_names

# характеристики д/х
data_source$data_hh <- rlms$all_data_hh %>%
  select(id_w, id_h, num_head = a8, nfm) %>%
  cbind(., data_source$data_consump_nd_hh) %>%
  arrange(id_h, id_w)

# Здесь не должно быть пропусков
summary(data_source$data_hh$id_h)

# Удаляем вспомогательные данные по д/х
data_source[c("data_consump_nd_hh")] <- NULL


### Дополнительные данные 
# Данные по доходам и расходам
data_source$data_add <- rlms$all_data_add %>%
  select(id_h, id_w, child_lit = ncat1, child_teen = ncat2, work_age_m = ncat3,
    work_age_f = ncat4, nwork_age_m = ncat5, nwork_age_f = ncat6)

# Здесь не должно быть пропусков
summary(data_source$data_add$id_h)

# Коды по отношениям
data_source$code_rel <- rlms$all_code_rel %>% 
  select(id_w = id_w, id_i = id_i, id_i_part = r01a1) %>%
  arrange(id_i, id_w)

# Здесь не должно быть пропусков
summary(data_source$code_rel$id_i)


### Модифицируем коды индивидом
# Добавим к кодам индивида адрес репрезентативной выборки
data_source$code_ind <- data_source$code_ind %>%
  left_join(
    data_source$data_ind %>% select(id_ind, year, origsm)
  ) %>%  # Адрес репрезентативной выборки
  arrange(id_ind)

# Добавим к кодам индивида коды супругов id_i_part
data_source$code_ind <- data_source$code_ind %>% 
  left_join(data_source$code_rel)

# Добавим к кодам индивида коды супругов id_part
data_source$code_ind <- data_source$code_ind %>% 
  left_join(
    data_source$code_ind %>% select(id_part = id_ind, year, id_i_part = id_i),
    na_matches = "never"
  )

# Добавим супружеский статус
data_source$code_ind <- data_source$code_ind %>% 
  left_join(
    data_source$data_ind %>% select(id_i, year, family)
  )

# Переставим столбцы местами
data_source$code_ind <- data_source$code_ind %>%
  select(year, id_ind, id_i, id_part, id_i_part, origsm, family, sex)



