models_stage_1 <- function(data) {

  model_wage_male <- plm(dlwage ~
      as.factor(age) + as.factor(diplom_lev) + 
      as.factor(year):as.factor(diplom_lev) +
      as.factor(region) + as.factor(year) + as.factor(status),
    data = subset(data, sum_working == 1),
    index = c("id_ind", "year"), model = "pooling")

  model_wage_female <- plm(dlwage_part ~
      as.factor(age_part) + as.factor(diplom_lev_part) + 
      as.factor(year):as.factor(diplom_lev_part) +
      as.factor(region) + as.factor(year) + as.factor(status),
    data = subset(data, sum_working_part == 1),
    index = c("id_part", "year"), model = "pooling")

  model_consump <- plm(dlcons ~
      as.factor(age) + as.factor(diplom_lev) + 
      as.factor(year):as.factor(diplom_lev) +
      as.factor(age_part) + as.factor(diplom_lev_part) + 
      as.factor(year):as.factor(diplom_lev_part) +
      as.factor(region) + as.factor(year) + as.factor(status) +
      as.factor(child_lit) + as.factor(child_teen) + work_age_share +
      as.factor(sum_working) + as.factor(sum_working_part),
    data = data, index = c("id_hh", "year"), model = "pooling")

  model_earn_male <- plm(dlearn ~
      as.factor(age) + as.factor(diplom_lev) + 
      as.factor(year):as.factor(diplom_lev) +
      as.factor(age_part) + as.factor(diplom_lev_part) + 
      as.factor(year):as.factor(diplom_lev_part) +
      as.factor(region) + as.factor(year) + as.factor(status) +
      as.factor(child_lit) + as.factor(child_teen) + work_age_share +
      as.factor(sum_working_part),
    data = data,
    index = c("id_ind", "year"), model = "pooling")

  model_earn_female <- plm(dlearn_part ~
      as.factor(age) + as.factor(diplom_lev) + 
      as.factor(year):as.factor(diplom_lev) +
      as.factor(age_part) + as.factor(diplom_lev_part) + 
      as.factor(year):as.factor(diplom_lev_part) +
      as.factor(region) + as.factor(year) + as.factor(status) +
      as.factor(child_lit) + as.factor(child_teen) + work_age_share +
      as.factor(sum_working),
    data = data,
    index = c("id_part", "year"), model = "pooling")

  model_hours_male <- plm(dlhours ~
      as.factor(child_lit) + as.factor(child_teen) + work_age_share,
    data = subset(data, sum_working == 1),
    index = c("id_ind", "year"), model = "pooling")

  model_hours_female <- plm(dlhours_part ~ 
      as.factor(child_lit) + as.factor(child_teen) + work_age_share,
    data = subset(data, sum_working_part == 1),
    index = c("id_part", "year"), model = "pooling")

  data <- results_first_stage_diff(
    data, model_wage_male, model_wage_female, model_consump,
    model_earn_male, model_earn_female, model_hours_male, model_hours_female
  )

  return(data)

}
