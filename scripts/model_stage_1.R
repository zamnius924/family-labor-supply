# ============================================================================
# model_stage_1.R
# ----------------------------------------------------------------------------
# First‑stage regressions (pooled OLS) to partial out controls from wage,
# consumption, earnings, and hours. Residuals are saved into the 'data' object.
# ============================================================================

# Male wage equation
model_wage_male <- plm(dlwage ~
    as.factor(age) + as.factor(diplom_lev) + 
    as.factor(year):as.factor(diplom_lev) +
    as.factor(region) + as.factor(year) + as.factor(status),
  data = subset(data, sum_working == 1),
  index = c("id_ind", "year"),
  model = "pooling")

# Female wage equation
model_wage_female <- plm(dlwage_part ~
    as.factor(age_part) + as.factor(diplom_lev_part) + 
    as.factor(year):as.factor(diplom_lev_part) +
    as.factor(region) + as.factor(year) + as.factor(status),
  data = subset(data, sum_working_part == 1),
  index = c("id_part", "year"),
  model = "pooling")

# Household consumption equation
model_consump <- plm(dlcons ~
    as.factor(age) + as.factor(diplom_lev) + 
    as.factor(year):as.factor(diplom_lev) +
    as.factor(age_part) + as.factor(diplom_lev_part) + 
    as.factor(year):as.factor(diplom_lev_part) +
    as.factor(region) + as.factor(year) + as.factor(status) +
    as.factor(child_lit) + as.factor(child_teen) + work_age_share +
    as.factor(sum_working) + as.factor(sum_working_part),
  data = data,
  index = c("id_hh", "year"),
  model = "pooling")

# Male earnings equation
model_earn_male <- plm(dlearn ~
    as.factor(age) + as.factor(diplom_lev) + 
    as.factor(year):as.factor(diplom_lev) +
    as.factor(age_part) + as.factor(diplom_lev_part) + 
    as.factor(year):as.factor(diplom_lev_part) +
    as.factor(region) + as.factor(year) + as.factor(status) +
    as.factor(child_lit) + as.factor(child_teen) + work_age_share +
    as.factor(sum_working_part),
  data = data,
  index = c("id_ind", "year"),
  model = "pooling")

# Female earnings equation
model_earn_female <- plm(dlearn_part ~
    as.factor(age) + as.factor(diplom_lev) + 
    as.factor(year):as.factor(diplom_lev) +
    as.factor(age_part) + as.factor(diplom_lev_part) + 
    as.factor(year):as.factor(diplom_lev_part) +
    as.factor(region) + as.factor(year) + as.factor(status) +
    as.factor(child_lit) + as.factor(child_teen) + work_age_share +
    as.factor(sum_working),
  data = data,
  index = c("id_part", "year"),
  model = "pooling")

# Male hours equation
model_hours_male <- plm(dlhours ~
    as.factor(child_lit) + as.factor(child_teen) + work_age_share,
  data = subset(data, sum_working == 1),
  index = c("id_ind", "year"),
  model = "pooling")

# Female hours equation
model_hours_female <- plm(dlhours_part ~ 
    as.factor(child_lit) + as.factor(child_teen) + work_age_share,
  data = subset(data, sum_working_part == 1),
  index = c("id_part", "year"),
  model = "pooling")

# Extract residuals and attach to 'data'
data <- results_first_stage_diff(
  data, model_wage_male, model_wage_female, model_consump,
  model_earn_male, model_earn_female, model_hours_male, model_hours_female
)

# Clean up to free memory
rm(model_wage_male, model_wage_female, model_consump,
  model_earn_male, model_earn_female,
  model_hours_male, model_hours_female)