### Сводим все данные в общий файл
all_data <- data_source$data_ind %>%
  left_join(data_source$data_hh) %>% 
  left_join(data_source$data_add) %>% 
  left_join(add_sources$days) %>% 
  left_join(add_sources$GRP) %>% 
  left_join(add_sources$CPI_reg) %>%
  left_join(add_sources$fed_dist) %>%
  arrange(id_ind, year)