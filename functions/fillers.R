# ============================================================================
# fillers.R
# ----------------------------------------------------------------------------
# Functions to impute missing wage and hours using alternative survey questions.
# ============================================================================

#' Fill missing monthly wage using last month's wage (first job)
filler_wage <- function(df) {
  df <- df %>% 
    mutate(wage_month_filled = coalesce(wage_month, wage_last_month))
  
  # For second and third jobs, simply rename existing variables
  df <- df %>% 
    rename(
      wage_month_filled_2 = wage_last_month_2, 
      wage_month_filled_3 = wage_last_month_3
    ) %>% 
    relocate(wage_month_filled_2, wage_month_filled_3, .after = last_col())
  
  return(df)
}

#' Fill missing monthly hours using weekly hours or daily hours
filler_lab_sup <- function(df) {
  df <- df %>%
    mutate(
      # Weekly hours -> monthly (multiply by average weeks per month)
      lab_sup_month_w_filled = lab_sup_week * days / 12 / 7,
      lab_sup_month_w_filled_2 = lab_sup_week_2 * days / 12 / 7,
      # Daily hours -> monthly (22 working days)
      lab_sup_month_d_filled = lab_sup * 22,
      lab_sup_month_d_filled_2 = lab_sup_2 * 22
    ) %>% 
    mutate(
      lab_sup_month_filled   = coalesce(lab_sup_month_w_filled,
                                        lab_sup_last_month),
      lab_sup_month_filled_2 = coalesce(lab_sup_month_w_filled_2,
                                        lab_sup_last_month_2)
    ) %>% 
    mutate(
      lab_sup_month_filled   = coalesce(lab_sup_month_filled,
                                        lab_sup_month_d_filled),
      lab_sup_month_filled_2 = coalesce(lab_sup_month_filled_2,
                                        lab_sup_month_d_filled_2)
    ) %>%
    # If the imputed value differs from last month's by more than 48 hours, keep last month's
    mutate(
      lab_sup_dev   = abs(lab_sup_month_filled   - lab_sup_last_month),
      lab_sup_dev_2 = abs(lab_sup_month_filled_2 - lab_sup_last_month_2)
    ) %>%
    mutate(
      lab_sup_month_filled = case_when(
        lab_sup_dev >= 48 & is.na(lab_sup_dev) == FALSE ~ lab_sup_last_month,
        TRUE ~ lab_sup_month_filled
      ),
      lab_sup_month_filled_2 = case_when(
        lab_sup_dev_2 >= 48 & is.na(lab_sup_dev_2) == FALSE ~ lab_sup_last_month_2,
        TRUE ~ lab_sup_month_filled_2
      )
    ) %>% 
    # Third job: only if regular work
    mutate(
      lab_sup_month_filled_3 = case_when(
        regular == 2 ~ lab_sup_last_month_3,
        TRUE ~ NA_real_
      )
    ) %>%
    select(-lab_sup_dev, -lab_sup_dev_2,
           -lab_sup_month_w_filled, -lab_sup_month_w_filled_2,
           -lab_sup_month_d_filled, -lab_sup_month_d_filled_2)
  
  return(df)
}

#' Clean inconsistent work indicators (e.g., working=2 but positive hours)
corrector_lab_sup <- function(df) {
  df <- df %>% 
    # First job
    mutate(
      lab_sup_month_filled = replace(lab_sup_month_filled, working == 2, NA),
      wage_month_filled    = replace(wage_month_filled,    working == 2, NA)
    ) %>% 
    mutate(
      lab_sup_month_filled = replace(
        lab_sup_month_filled,
        working == 1 & lab_sup_month_filled == 0,
        NA
      ),
      wage_month_filled = replace(
        wage_month_filled,
        working == 1 & wage_month_filled == 0,
        NA
      )
    ) %>% 
    # Second job (two possible working variables)
    mutate(
      working_2 = replace(working_2.1, working_2.1 == 2 & working_2.2 == 1, 1)
    ) %>% 
    mutate(
      lab_sup_month_filled_2 = replace(
        lab_sup_month_filled_2,
        working_2 == 2,
        NA
      ),
      wage_month_filled_2 = replace(
        wage_month_filled_2,
        working_2 == 2,
        NA
      )
    ) %>% 
    mutate(
      lab_sup_month_filled_2 = replace(
        lab_sup_month_filled_2,
        working_2 == 1 & lab_sup_month_filled_2 == 0,
        NA
      ),
      wage_month_filled_2 = replace(
        wage_month_filled_2,
        working_2 == 1 & wage_month_filled_2 == 0,
        NA
      )
    ) %>% 
    # Third job
    mutate(
      lab_sup_month_filled_3 = replace(
        lab_sup_month_filled_3,
        working_3 == 2,
        NA
      ),
      wage_month_filled_3 = replace(
        wage_month_filled_3,
        working_3 == 2,
        NA
      )
    ) %>%
    mutate(
      lab_sup_month_filled_3 = replace(
        lab_sup_month_filled_3,
        working_3 == 1 & lab_sup_month_filled_3 == 0,
        NA
      ),
      wage_month_filled_3 = replace(
        wage_month_filled_3,
        working_3 == 1 & wage_month_filled_3 == 0,
        NA
      )
    ) %>% 
    # Convert work indicators from 1/2 to 0/1
    mutate(
      working   = abs(working   - 2),
      working_2 = abs(working_2 - 2),
      working_3 = abs(working_3 - 2)
    ) %>% 
    relocate(working, working_2, working_3, .after = last_col())
  
  return(df)
}