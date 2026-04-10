### Сделаем в данных переменную для регионов
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


### Добавим данные по региональной инфляции
# Базисный ИПЦ 2016-го года
add_sources$CPI_reg <- add_sources$CPI_reg %>% 
  pivot_longer(
    cols = colnames(add_sources$CPI_reg)[-1],
    names_to = "year", 
    values_to = "CPI_reg_chain") %>% 
  mutate(CPI_reg = 0) %>% 
  mutate(year = as.numeric(year)) %>% 
  rename(region_rus = region)

add_sources$CPI_reg[which(add_sources$CPI_reg$year == 2016),]$CPI_reg <- 100

for (s in unique(add_sources$CPI_reg$region_rus)) {
  for (t in 2015:first_year) {
    add_sources$CPI_reg[which(add_sources$CPI_reg$year == t & add_sources$CPI_reg$region_rus == s),]$CPI_reg <- 
      100 * add_sources$CPI_reg[which(add_sources$CPI_reg$year == t + 1 & add_sources$CPI_reg$region_rus == s),]$CPI_reg /
      add_sources$CPI_reg[which(add_sources$CPI_reg$year == t + 1 & add_sources$CPI_reg$region_rus == s),]$CPI_reg_chain
  }
  for (t in 2017:last_year) {
    add_sources$CPI_reg[which(add_sources$CPI_reg$year == t & add_sources$CPI_reg$region_rus == s),]$CPI_reg <- 
      add_sources$CPI_reg[which(add_sources$CPI_reg$year == t & add_sources$CPI_reg$region_rus == s),]$CPI_reg_chain *
      add_sources$CPI_reg[which(add_sources$CPI_reg$year == t - 1 & add_sources$CPI_reg$region_rus == s),]$CPI_reg / 100
  }
}


### Создадим недлительное потребление
data_source$data_hh$consump_nd <- data_source$data_hh %>%
  select(all_of(data_source$consump_nd_names)) %>% 
  apply(1, sum, na.rm = TRUE)
data_source$data_hh$consump_nd <- data_source$data_hh$consump_nd * 12 # переводим в годовое измерение


### Оставим во вспомогательных файлах только необходимые переменные
data_source$data_hh <- data_source$data_hh %>% 
  select(id_w, id_h, num_head, nfm, consump_nd)
add_sources$CPI_reg <- add_sources$CPI_reg %>% 
  select(-CPI_reg_chain)
add_sources$days <- add_sources$days %>% 
  select(-work_days, -publ_hol)
