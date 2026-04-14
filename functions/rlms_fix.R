# ============================================================================
# rlms_fix.R
# ----------------------------------------------------------------------------
# WARNING: This file modifies internal functions of the 'rlms' package using
#          assignInNamespace(). It may break after package updates.
# Purpose: Override rlms_cleanup() and rlms_labelled2factor.data.frame() to
#          handle RLMS data more consistently.
# ============================================================================

rlms_cleanup <- function (df, suppress = TRUE, empty2na = TRUE, nan2na = TRUE, 
          nine2na = TRUE, yesno = TRUE, apostrophe = TRUE, remove_empty = TRUE, 
          colnames_tolower = TRUE, verbose = FALSE) 
{
  if (verbose) {
    message("Cleanup options:")
    message("Convert '' to NA, empty2na = ", empty2na)
    message("Convert NaN to NA, nan2na = ", nan2na)
    message("Convert 99999990+ to NA, nine2na = ", nine2na)
    message("Convert column names to lowercase, colnames_tolower = ", 
            colnames_tolower)
    message("Standartise Yes/NO to yes/no, yesno = ", yesno)
    message("Remove redundant apostrophes, apostrophe = ", 
            apostrophe)
    message("Remove empty value label, remove_empty = ", 
            remove_empty)
  }
  if (colnames_tolower) {
    colnames(df) <- stringr::str_to_lower(colnames(df))
  }
  for (var in colnames(df)) {
    var_class <- class(df[[var]])
    if (verbose) {
    }
    if (nan2na) {
      df[[var]][is.nan(df[[var]])] <- NA
    }
    if ((nine2na) & (is.numeric(df[[var]]))) {
      df[[var]][df[[var]] > 99999990] <- NA
    }
    if ((empty2na) & (is.character(df[[var]]))) {
      df[[var]][df[[var]] == ""] <- NA
    }
    if (yesno) {
      if ("character" %in% var_class) { # (var_class == "character")
        df[[var]] <- rlms_yesno_standartize(df[[var]])
      }
      if (("labelled" %in% var_class) & length(attr(df[[var]], # (var_class == "labelled")
                                                  "labels") > 0)) {
        attr(attr(df[[var]], "labels"), "names") <- rlms_yesno_standartize(attr(attr(df[[var]], 
                                                                                     "labels"), "names"))
      }
    }
    if (apostrophe) {
      if ("character" %in% var_class) { # (var_class == "character")
        df[[var]] <- rlms_remove_apostrophe(df[[var]])
      }
      if (("labelled" %in% var_class) & length(attr(df[[var]], # (var_class == "labelled")
                                                  "labels") > 0)) {
        attr(attr(df[[var]], "labels"), "names") <- rlms_remove_apostrophe(attr(attr(df[[var]], 
                                                                                     "labels"), "names"))
      }
    }
    if (remove_empty) {
      value_labels <- get_labels(df[[var]])
      labels <- names(value_labels)
      values_with_empty_labels <- value_labels[labels == 
                                                 ""]
      if (length(values_with_empty_labels) > 0) {
        values_to_remove <- setdiff(values_with_empty_labels, 
                                    unique(df[[var]]))
        attr(df[[var]], "labels") <- value_labels[!value_labels %in% 
                                                    values_to_remove]
      }
    }
  }
  return(df)
}


rlms_labelled2factor.data.frame <- function (x, verbose = FALSE, ...) 
{
  if (verbose) {
    message("The option haven = 'factor' is experimental and subject to change.")
  }
  for (var in names(x)) {
    var_class <- class(x[[var]])
    if (verbose) {
    }
    variable_label <- attr(x[[var]], "label")
    if (any(is_labelled(x[[var]]), na.rm = TRUE)) { # (is_labelled(x[[var]]))
      if (all_labelled(x[[var]])) {
        x[[var]] <- as_factor_safe(x[[var]])
      }
      else if (all_but_one_labelled(x[[var]])) {
        x[[var]] <- as_factor_safe(x[[var]])
        message("Labelled variable ", var, " was considered as factor: it has only one unlabelled value.")
        message("This unlabelled value is neither minimal neither maximal.")
      }
      else if (all_but_rlmsna_labelled(x[[var]])) {
        x[[var]] <- as_factor_safe(x[[var]])
        message("Labelled variable ", var, " was considered as factor: all unlabelled values are bigger than 99999990.")
      }
      else {
        x[[var]] <- as.vector(x[[var]])
      }
    }
    attr(x[[var]], "label") <- variable_label
  }
  return(x)
}

# Override the package's internal functions
assignInNamespace("rlms_cleanup", rlms_cleanup, ns = "rlms")
assignInNamespace("rlms_labelled2factor.data.frame", rlms_labelled2factor.data.frame, ns = "rlms")