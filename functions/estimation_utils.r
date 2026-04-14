# ============================================================================
# estimation_utils.R
# ----------------------------------------------------------------------------
# Helper to compute first‑differences of logs and to extract residuals from
# first‑stage regressions.
# ============================================================================

#' Compute log differences (growth rates) for key variables
metrics_diff <- function(df) {
  df_new <- expand.grid(
    id_ind = unique(df$id_ind),
    year = first_year:last_year
  ) %>%
    left_join(
      df %>% select(id_ind, year, lwage, learn, lcons, lhours)
    ) %>% 
    arrange(id_ind, year) %>% 
    group_by(id_ind) %>% 
    mutate(
      dlwage  = lwage  - dplyr::lag(lwage),
      dlearn  = learn  - dplyr::lag(learn),
      dlcons  = lcons  - dplyr::lag(lcons),
      dlhours = lhours - dplyr::lag(lhours)
    ) %>% 
    select(id_ind, year, dlwage, dlearn, dlcons, dlhours)
  
  df <- df %>% 
    left_join(df_new, by = c("id_ind", "year"))
  
  return(df)
}

#' Attach residuals from first‑stage PLM models to the main data frame
#' 
#' @param df main data frame
#' @param model_wM, model_wF, model_c, model_yM, model_yF, model_hM, model_hF
#'        plm objects (pooled OLS) estimated in first differences.
results_first_stage_diff <- function(df, model_wM, model_wF, model_c,
                                     model_yM, model_yF, model_hM, model_hF) {
  
  # Residuals for male wage
  res_w_male <- expand.grid(
    id_ind = unique(as.numeric(as.character(index(model_wM)[,1]))),
    year = first_year:last_year
  ) %>%
    left_join(
      cbind(index(model_wM), resid(model_wM)) %>%
        lapply(FUN = function(x) as.numeric(paste(x))) %>%
        as.data.frame(.) %>%
        rename(dw = "resid.model_wM.")
    ) %>%
    arrange(id_ind, year)
  
  # Residuals for female wage
  res_w_female <- expand.grid(
    id_part = unique(as.numeric(as.character(index(model_wF)[,1]))),
    year = first_year:last_year
  ) %>%
    left_join(
      cbind(index(model_wF), resid(model_wF)) %>%
        lapply(FUN = function(x) as.numeric(paste(x))) %>%
        as.data.frame(.) %>%
        rename(dw_part = "resid.model_wF.")
    ) %>%
    arrange(id_part, year)
  
  # Residuals for consumption
  res_c <- expand.grid(
    id_hh = unique(as.numeric(as.character(index(model_c)[,1]))),
    year = first_year:last_year
  ) %>%
    left_join(
      cbind(index(model_c), resid(model_c)) %>%
        lapply(FUN = function(x) as.numeric(paste(x))) %>%
        as.data.frame(.) %>%
        rename(dc = "resid.model_c.")
    ) %>%
    arrange(id_hh, year)
  
  # Residuals for male earnings
  res_e_male <- expand.grid(
    id_ind = unique(as.numeric(as.character(index(model_yM)[,1]))),
    year = first_year:last_year
  ) %>%
    left_join(
      cbind(index(model_yM), resid(model_yM)) %>%
        lapply(FUN = function(x) as.numeric(paste(x))) %>%
        as.data.frame(.) %>%
        rename(dy = "resid.model_yM.")
    ) %>%
    arrange(id_ind, year)
  
  # Residuals for female earnings
  res_e_female <- expand.grid(
    id_part = unique(as.numeric(as.character(index(model_yF)[,1]))),
    year = first_year:last_year
  ) %>%
    left_join(
      cbind(index(model_yF), resid(model_yF)) %>%
        lapply(FUN = function(x) as.numeric(paste(x))) %>%
        as.data.frame(.) %>%
        rename(dy_part = "resid.model_yF.")
    ) %>%
    arrange(id_part, year)
  
  # Residuals for male hours
  res_h_male <- expand.grid(
    id_ind = unique(as.numeric(as.character(index(model_hM)[,1]))),
    year = first_year:last_year
  ) %>%
    left_join(
      cbind(index(model_hM), resid(model_hM)) %>%
        lapply(FUN = function(x) as.numeric(paste(x))) %>%
        as.data.frame(.) %>%
        rename(dh = "resid.model_hM.")
    ) %>%
    arrange(id_ind, year)
  
  # Residuals for female hours
  res_h_female <- expand.grid(
    id_part = unique(as.numeric(as.character(index(model_hF)[,1]))),
    year = first_year:last_year
  ) %>%
    left_join(
      cbind(index(model_hF), resid(model_hF)) %>%
        lapply(FUN = function(x) as.numeric(paste(x))) %>%
        as.data.frame(.) %>%
        rename(dh_part = "resid.model_hF.")
    ) %>%
    arrange(id_part, year)
  
  df <- df %>%
    left_join(res_w_male)   %>% 
    left_join(res_w_female) %>% 
    left_join(res_c)        %>% 
    left_join(res_e_male)   %>% 
    left_join(res_e_female) %>% 
    left_join(res_h_male)   %>% 
    left_join(res_h_female)
  
  return(df)
}