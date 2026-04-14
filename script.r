# ============================================================================
# script.R
# ----------------------------------------------------------------------------
# Replication code for "Family Labor Supply and Wage Shocks in an Emerging
# Market: Evidence from Russia" by Alexey Zamnius
#
# Workflow:
#   1. Load libraries and custom functions
#   2. Import raw RLMS data and auxiliary files (CPI, GRP, regional data)
#   3. Build individual and household datasets
#   4. First‑stage regressions (residuals for wages, consumption, earnings)
#   5. GMM estimation of wage process (stage 2)
#   6. GMM estimation of preferences (stage 3)
#   7. Internal fit and bootstrap inference
#   8. Heterogeneity by education and income (conditional moments)
#   9. Visualisation of elasticities (2D/3D plots)
# ============================================================================

# --------------------------------------------------------------------------
# 0. Environment setup
# --------------------------------------------------------------------------
# All paths are relative to the project root (requires a .here marker file or
# Rproj)
# --------------------------------------------------------------------------

{
  library(rlms)
  library(dplyr)
  library(plm)
  library(readxl)
  library(tidyr)
  library(ggplot2)
  library(haven)
  library(pracma)      # for fmincon
  library(boot)
  library(tibble)
  library(latex2exp)   # LaTeX in plots
  library(patchwork)   # combining ggplot2 plots
  library(collapse)
  library(broom)
  library(plotly)      # 3D surfaces
}

source("functions/rlms_fix.R")

# --------------------------------------------------------------------------
# 1. Load raw RLMS data (saved in .sav format)
# --------------------------------------------------------------------------
rlms <- list(
  # Individual-level data
  all_data_ind = rlms_read("data/USER_RLMS-HSE_IND_1994_2020_v4_rus.sav"),
  # Household-level data
  all_data_hh = rlms_read("data/USER_RLMS-HSE_HH_1994_2020_rus.sav"),
  # Income and expenditure file
  all_data_add = rlms_read("data/Доходы и расходы.sav"),
  # Kinship identifiers
  all_code_rel = read_sav("data/Идентификационные номера родственников.sav"),
  # Individual identifiers across waves
  all_code_ind = read_sav("data/Идентификационные номера индивидов.sav")
)

source("arrangers/source_data.R")

rm(rlms)   # free memory

# --------------------------------------------------------------------------
# 2. Load auxiliary data (CPI, working days, GRP, federal minimum wage)
# --------------------------------------------------------------------------
source("params.R")

add_sources <- list(
  CPI_reg = read_excel("data/CPI_reg.xlsx"),
  days    = read_excel("data/days.xlsx"),
  GRP     = read_excel("data/grp.xlsx", sheet = 3) %>% 
              pivot_longer(cols = "1998":"2019", 
                           names_to = "year", 
                           values_to = "grp") %>% 
              mutate(year = as.numeric(year)),
  fed_dist = read_excel("data/fed_min.xlsx")
)

source("arrangers/source_cpi.R")

# Keep only necessary variables from auxiliary data
data_source$data_hh <- data_source$data_hh %>% 
  select(id_w, id_h, num_head, nfm, consump_nd)
add_sources$CPI_reg <- add_sources$CPI_reg %>% select(-CPI_reg_chain)
add_sources$days    <- add_sources$days    %>% select(-work_days, -publ_hol)

# --------------------------------------------------------------------------
# 3. Build analysis datasets
# --------------------------------------------------------------------------
source("arrangers/data_all.R")      # merge all data sources
source("functions/restrictions.R")  # sample restriction functions
source("functions/fillers.R")       # wage/hour imputation
source("functions/omit_utils.R")    # outlier removal
source("functions/estimation_utils.R")  # first‑stage helper
source("arrangers/data_ind.R")      # individual‑level sample

source("functions/partner_utils.R") # spouse matching
source("arrangers/data_full.R")     # household‑level data

rm(data_source, add_sources)

# --------------------------------------------------------------------------
# 4. First‑stage regressions
# --------------------------------------------------------------------------
source("scripts/model_stage_1.R")

# Create variables for GMM (differences and lags)
source("arrangers/data_mod.R")

source("functions/GMM_utils.R")

# --------------------------------------------------------------------------
# 5. GMM stage 2: wage process
# --------------------------------------------------------------------------
source("scripts/model_stage_2.R")
res_model_wage

# --------------------------------------------------------------------------
# 6. GMM stage 3: preferences (labour supply elasticities)
# --------------------------------------------------------------------------
source("scripts/model_stage_3.R")
res_model_pref

# --------------------------------------------------------------------------
# 7. Internal fit (model vs. data moments)
# --------------------------------------------------------------------------
source("functions/fit_utils.R")
source("scripts/internal_fit.R")
fit_plot$plot

# --------------------------------------------------------------------------
# 8. Bootstrap inference for baseline model
# --------------------------------------------------------------------------
source("scripts/bootstrap.R")
results

# --------------------------------------------------------------------------
# 9. Heterogeneity by education
# --------------------------------------------------------------------------
source("functions/GMM_hetero_utils.R")
source("arrangers/data_mod_hetero.R")
source("scripts/model_pref_hetero.R")
res_model_pref_hetero

source("scripts/bootstrap_pref_hetero.R")
source("scripts/plot_pref_hetero.R")
res_model_pref_hetero$elast_own
res_model_pref_hetero$elast_cross

# --------------------------------------------------------------------------
# 10. Heterogeneity by income (conditional moments)
# --------------------------------------------------------------------------
source("functions/GMM_cond_utils.R")
source("arrangers/data_mod_cond.R")

source("scripts/model_pref_cond.R")

source("scripts/plot_pref_cond_own_3d.R")
plot_pref_cond_own_3d$fig

source("scripts/plot_pref_cond_cross_3d.R")
plot_pref_cond_cross_3d$fig

source("scripts/plot_pref_cond_own_2d.R")
plot_pref_cond_own_2d$fig