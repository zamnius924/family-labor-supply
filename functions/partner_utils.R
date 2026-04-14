# ============================================================================
# partner_utils.R
# ----------------------------------------------------------------------------
# Functions to merge spouses into a household-level dataset and create unique
# couple identifiers.
# ============================================================================

#' Merge spouse characteristics using identifier tables
partner_merge <- function(df, df_source) {
  df_merged <- df %>%
    left_join(
      df_source$code_ind %>% select(id_ind, year, id_part)
    ) 
  
  df_merged <- df_merged %>% 
    left_join(
      df %>% select(
        id_part = id_ind, id_h, year, sex_part = sex, 
        lab_sup_year_part = lab_sup_year, wage_part = wage,
        sum_working_part = sum_working, earn_part = earn, 
        diplom_lev_part = diplom_lev, age_part = age,
        dlwage_part = dlwage, dlearn_part = dlearn, dlhours_part = dlhours
      )
    )
  
  return(df_merged)
}

#' Keep only couples where both spouses are present and male is the reference
partner_df <- function(df) {
  df <- df %>%
    subset(sex == 1) %>%                     # keep only males as reference
    drop_na(id_part, sex_part)               # drop if no spouse data
  return(df)
}

#' Create a unique household identifier for each couple
partner_id <- function(df) {
  df$couples <- df %>%
    select(id_ind, id_part) %>%
    apply(1, function(x) paste(sort(x), collapse = "; "))
  
  couples <- data.frame(couples = unique(df$couples)) %>%
    mutate(id_hh = row_number())
  
  df <- df %>%
    left_join(couples) %>%
    select(id_hh, everything(), -couples) 
  
  return(df)
}