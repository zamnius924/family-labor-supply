# ============================================================================
# params.R
# ----------------------------------------------------------------------------
# Global parameters for the entire project.
# ============================================================================

# Sample period (years)
first_year <- 2000
last_year  <- 2019
period <- last_year - first_year + 1

# Bootstrap settings
B     <- 1000   # number of bootstrap replications
ncpus <- 8      # number of CPU cores for parallel bootstrap