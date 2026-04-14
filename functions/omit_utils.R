# ============================================================================
# omit_utils.R
# ----------------------------------------------------------------------------
# Functions for outlier removal based on extreme growth rates.
# ============================================================================

#' Remove measurement error using "jump" detection in log levels
#' 
#' For each individual, we compute the product of forward and backward differences
#' in log wage, log earnings, log consumption, and log hours. Observations with
#' very negative products (implying a sharp reversal) are treated as measurement
#' error and set to NA.
omit_me <- function(df) {
  df_log <- expand.grid(
    id_ind = unique(df$id_ind),
    year = first_year:last_year
  ) %>%
    left_join(
      df %>% select(id_ind, year, wage, earn, consump_nd, lab_sup_year)
    ) %>%
    arrange(id_ind, year) %>%
    mutate(
      lwage  = log(wage), 
      learn  = log(earn), 
      lcons  = log(consump_nd), 
      lhours = log(lab_sup_year)
    ) %>%
    group_by(id_ind) %>% 
    mutate(
      jw = (lwage  - dplyr::lag(lwage))  * (dplyr::lead(lwage)  - lwage),
      je = (learn  - dplyr::lag(learn))  * (dplyr::lead(learn)  - learn),
      jc = (lcons  - dplyr::lag(lcons))  * (dplyr::lead(lcons)  - lcons),
      jh = (lhours - dplyr::lag(lhours)) * (dplyr::lead(lhours) - lhours)
    )
  
  # Drop the lowest 0.25% of jump products
  df_log$wage[df_log$jw < quantile(df_log$jw, prob = 0.0025, na.rm = TRUE)] <- NA
  df_log$earn[df_log$je < quantile(df_log$je, prob = 0.0025, na.rm = TRUE)] <- NA
  df_log$consump_nd[df_log$jc < quantile(df_log$jc, prob = 0.0025, na.rm = TRUE)] <- NA
  df_log$lab_sup_year[df_log$jh < quantile(df_log$jh, prob = 0.0025, na.rm = TRUE)] <- NA

  df <- df %>%
    select(-wage, -earn, -consump_nd, -lab_sup_year) %>% 
    left_join(
      df_log %>% select(id_ind, year, wage, earn, consump_nd, lab_sup_year)
    ) %>%
    mutate(
      lwage  = log(wage),
      learn  = log(earn),
      lcons  = log(consump_nd),
      lhours = log(lab_sup_year)
    )
  
  return(df)
}

#' Additional outlier removal based on growth rates (wage and consumption)
omit_outlier <- function(df, g_wage_res, g_consump_res) {
  df_expanded <- expand.grid(
    id_ind = unique(df$id_ind), year = first_year:last_year
  ) %>%
    left_join(
      df %>% 
        select(id_ind, year, consump_nd, wage, lab_sup_year, earn) %>% 
        mutate(present = 1)
    ) %>%
    arrange(id_ind, year) %>%
    group_by(id_ind) %>% 
    mutate(
      g_consump = consump_nd / dplyr::lag(consump_nd),
      g_wage    = wage / dplyr::lag(wage),
      g_lab_sup_year = lab_sup_year / dplyr::lag(lab_sup_year),
      g_earn    = earn / dplyr::lag(earn)
    ) %>% 
    subset(g_wage   <= g_wage_res   | is.na(g_wage)) %>%
    subset(g_consump <= g_consump_res | is.na(g_consump))
  
  df_new <- df_expanded %>% 
    subset(present == 1) %>%
    select(id_ind, year, wage, consump_nd) %>%
    left_join(df %>% select(-wage, -consump_nd))
  
  return(df_new)
}