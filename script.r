{
  library(rlms)
  library(dplyr)
  library(plm)
  library(readxl)
  library(tidyr)
  library(ggplot2)
  library(haven)
  library(pracma)
  library(boot)
  library(tibble)
  library(latex2exp)
  library(patchwork)
  library(collapse)
  library(broom)
  library(plotly)
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

rm(rlms)




# Преобразование файлов с дополнительными данными -------------------------
# Параметры выборки
first_year <- 2000
last_year <- 2019
period <- last_year - first_year + 1

# Дополнительные параметры
B <- 1000


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
# Сводим все данные в общий файл
source("arrangers/data_all.R")

# Вводим основные ограничения на выборку => получаем выборку на индивидуальном уровне
source("arrangers/data_ind.R")

# Переводим данные на уровень д/х
source("arrangers/data_full.R")




# models -----------------------------------------
### Шаг 1
source("scripts/model_stage_1.R")

# Создание переменных для GMM
source("arrangers/data_mod.R")

### Internal fit
source("scripts/internal_fit.R")

### Шаг 2
source("scripts/model_stage_2.R")
res_model_wage

### Шаг 3
source("scripts/model_stage_3.R")
res_model_pref

### Бутстрап
source("scripts/bootstrap.R")
results

### Гетерогенность по уровню образования
source("arrangers/data_mod_hetero.R")
source("scripts/model_pref_hetero.R")
res_model_pref_hetero

source("scripts/bootstrap_pref_hetero.R")
res_model_pref_hetero$table

source("scripts/plot_pref_hetero.R")
res_model_pref_hetero$elast_own
res_model_pref_hetero$elast_cross

### Гетерогенность по доходу
source("arrangers/data_mod_cond.R")

data_cond
source("scripts/model_pref_cond.R")

res_model_pref_cond

source("scripts/plot_pref_cond_own_3d.R")
plot_pref_cond_own_3d$fig

source("scripts/plot_pref_cond_cross_3d.R")
plot_pref_cond_cross_3d$fig
