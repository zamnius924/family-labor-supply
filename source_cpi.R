### Добавим данные по региональной инфляции
# Базисный ИПЦ 2016-го года
add_sources$CPI_reg <- add_sources$CPI_reg %>% 
  pivot_longer(
    cols = colnames(add_sources$CPI_reg)[-1],
    names_to = "year", 
    values_to = "CPI_reg_chain"
  ) %>% 
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