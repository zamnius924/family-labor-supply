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

rlms <- list(
  # данные об индивидах
  all_data_ind = rlms_read("data/USER_RLMS-HSE_IND_1994_2020_v4_rus.sav"),
  # данные о д/х
  all_data_hh = rlms_read("data/USER_RLMS-HSE_HH_1994_2020_rus.sav"),
  # файл с доходами и расходами
  all_data_add = rlms_read("data/Доходы и расходы.sav"),
  # файл с родственными связями
  all_code_rel = read_sav("data/Идентификационные номера родственников.sav"),
  # файл с кодами индивидов
  all_code_ind = read_sav("data/Идентификационные номера индивидов.sav")
)

source("arrangers/source_data.R")

rm(rlms)




# Преобразование файлов с дополнительными данными -------------------------
source("params.R")

### Импортируем дополнительные файлы
add_sources <- list(
  # ИПЦ региональный
  CPI_reg = read_excel(
    "data/CPI_reg.xlsx"
  ),
  # Выходные в году
  days = read_excel(
    "data/days.xlsx"
  ),
  # ВРП
  GRP = read_excel(
    "data/grp.xlsx", 
    sheet = 3
  ) %>% 
    pivot_longer(
        cols = "1998":"2019", 
        names_to = "year", 
        values_to = "grp"
    ) %>% 
    mutate(year = as.numeric(year)),
  # МРОТ
  fed_dist = read_excel("data/fed_min.xlsx")
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
source("functions/restrictions.R")
source("functions/fillers.R")
source("functions/omit_utils.R")
source("functions/estimation_utils.R")
source("arrangers/data_ind.R")

# Переводим данные на уровень д/х
source("functions/partner_utils.R")
source("arrangers/data_full.R")

rm(data_source, add_sources)




# models -----------------------------------------
### Шаг 1
source("scripts/model_stage_1.R")

# Создание переменных для GMM
source("arrangers/data_mod.R")

source("functions/GMM_utils.R")

### Шаг 2
source("scripts/model_stage_2.R")
res_model_wage

### Шаг 3
source("scripts/model_stage_3.R")
res_model_pref

### Internal fit
source("functions/fit_utils.R")
source("scripts/internal_fit.R")
fit_plot$plot

### Бутстрап
source("scripts/bootstrap.R")
results

### Гетерогенность по уровню образования
source("functions/GMM_hetero_utils.R")
source("arrangers/data_mod_hetero.R")
source("scripts/model_pref_hetero.R")
res_model_pref_hetero

source("scripts/bootstrap_pref_hetero.R")
source("scripts/plot_pref_hetero.R")
res_model_pref_hetero$elast_own
res_model_pref_hetero$elast_cross

### Гетерогенность по доходу
source("functions/GMM_cond_utils.R")
source("arrangers/data_mod_cond.R")

source("scripts/model_pref_cond.R")

source("scripts/plot_pref_cond_own_3d.R")
plot_pref_cond_own_3d$fig

source("scripts/plot_pref_cond_cross_3d.R")
plot_pref_cond_cross_3d$fig

source("scripts/plot_pref_cond_own_2d.R")
plot_pref_cond_own_2d$fig
