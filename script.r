{
  library(rlms)
  library(dplyr)
  library(plm)
  library(readxl)
  library(tidyr)
  library(ggplot2)
  library(haven)
}

source("functions/rlms_fix.R")
source("functions/add_functions.R")

{
  setwd("/Users/alexey/Desktop/Labor market research/data/RLMS_1994_2020 (28.VI.2022)")

  rlms <- list(
    # данные об индивидах
    all_data_ind = rlms_read("USER_RLMS-HSE_IND_1994_2020_v4_rus.sav"),
    # данные о д/х
    all_data_hh = rlms_read("USER_RLMS-HSE_HH_1994_2020_rus.sav"),
    # файл с доходами и расходами
    all_data_add = rlms_read("Доходы и расходы.sav"),
    # файл с родственными связями
    all_code_rel = read_sav("Идентификационные номера родственников.sav"),
    # файл с кодами индивидов
    all_code_ind = read_sav("Идентификационные номера индивидов.sav")
  )

  setwd("/Users/alexey/Desktop/Labor market research/code/Blundell & Co./repository")
}

source("arrangers/source_data.R")

#rm(rlms)




# Преобразование файлов с дополнительными данными -------------------------
# Параметры выборки
first_year <- 2000
last_year <- 2019
period <- last_year - first_year + 1


### Импортируем дополнительные файлы
add_sources <- list(
  # ИПЦ региональный
  CPI_reg = read_excel(
    "/Users/alexey/Desktop/Labor market research/data/CPI_reg_2.xlsx"
  ),
  # Выходные в году
  days = read_excel(
    "/Users/alexey/Desktop/Labor market research/data/days.xlsx"
  ),
  # ВРП
  GRP = read_excel(
    "~/Desktop/Labor market research/data/grp/grp.xlsx", 
    sheet = 3
  ) %>% 
    pivot_longer(
        cols = "1998":"2019", 
        names_to = "year", 
        values_to = "grp"
    ) %>% 
    mutate(year = as.numeric(year)),
  # МРОТ
  fed_dist = read_excel("/Users/alexey/Desktop/Labor market research/data/federal wage minimum/fed_min.xlsx")
)

source("arrangers/source_cpi.R")


### Оставим во вспомогательных файлах только необходимые переменные
data_source$data_hh <- data_source$data_hh %>% 
  select(id_w, id_h, num_head, nfm, consump_nd)
add_sources$CPI_reg <- add_sources$CPI_reg %>% 
  select(-CPI_reg_chain)
add_sources$days <- add_sources$days %>% 
  select(-work_days, -publ_hol)




# Основные ограничения на выборку -----------------------------------------
### Сводим все данные в общий файл
all_data <- data_source$data_ind %>%
  left_join(data_source$data_hh) %>% 
  left_join(data_source$data_add) %>% 
  left_join(add_sources$days) %>% 
  left_join(add_sources$GRP) %>% 
  left_join(add_sources$CPI_reg) %>%
  left_join(add_sources$fed_dist) %>%
  arrange(id_ind, year)


### Вводим основные ограничения на выборку => получаем выборку на индивидуальном уровне
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

source("arrangers/data_ind.R")

source("arrangers/data_full.R")




# models -----------------------------------------
data <- models_stage_1(data)