# Data and Methodology Guide

## 1. Data sources

### 1.1 RLMS‑HSE (main survey)

The Russia Longitudinal Monitoring Survey of HSE (RLMS‑HSE) is a nationally representative household panel. We use waves 1994–2020 (version 4). The data are **not** redistributable; they must be obtained directly from the [RLMS‑HSE website](https://www.hse.ru/en/rlms/)

Required files (after registration):

- `USER_RLMS-HSE_IND_1994_2020_v4_rus.sav` – [individual‑level data](https://www.hse.ru/data/2025/11/18/1123886355/RLMS_IND_1994_2024_v3_rus.7z)
- `USER_RLMS-HSE_HH_1994_2020_rus.sav` – [household‑level data](https://www.hse.ru/data/2025/11/18/1535733168/RLMS_HH_1994_2024_rus_v5.zip)
- `Доходы и расходы.sav` – [income and expenditure module](https://www.hse.ru/data/2025/12/08/1394648477/HH_1994_2024_constr_rus.zip)
- `Идентификационные номера родственников.sav` – [kinship identifiers](https://www.hse.ru/data/2026/01/12/1566652928/ind_relatives_id_i_year_USER_RLMS-HSE_HH_1994_2024_rus.zip)
- `Идентификационные номера индивидов.sav` – [individual cross‑wave identifiers](https://www.hse.ru/data/2025/08/29/1579135340/r5_33i_user.zip)

### 1.2 Auxiliary data (constructed by the author from Rosstat)

These files are **not** part of the RLMS release. They are built from official Russian statistics and are available upon request or can be reconstructed as described below.

| File | Description | Source |
|------|-------------|--------|
| `CPI_reg.xlsx` | Regional consumer price index (base 2016) | Rosstat, regional CPI tables |
| `days.xlsx` | Number of working days and public holidays per year | Russian government decrees |
| `grp.xlsx` | Gross regional product (GRP) by region (1998–2019) | Rosstat, National Accounts |
| `fed_min.xlsx` | Federal minimum wage (nominal) | Russian legislation |

---

## 2. Sample selection and cleaning

All steps are implemented in the `arrangers/` and `functions/` folders. The main script `script.R` executes them in order.

### 2.1 Initial sample restrictions

- **Years**: 2000–2019 (balanced panel after differencing requires 2000 as the first year).
- **Age**: 25–55 years for both spouses (prime working age).
- **Representativeness**: Only households belonging to the representative subsample in the base year (2019) are retained.

### 2.2 Imputation of missing wages and hours

RLMS provides multiple measures of labour supply and earnings. We fill missing monthly wages and hours using the following hierarchy (functions `filler_wage()`, `filler_lab_sup()`):

- **Wages**: if monthly wage is missing, replace with “last month’s wage”.
- **Hours**:
  - Prefer weekly hours × (days/12/7) → monthly.
  - If missing, use daily hours × 22.
  - If still missing, use “last month’s hours”.
  - If the imputed value differs from last month’s by more than 48 hours, revert to last month’s value (to avoid measurement error).

### 2.3 Correction of work indicators

The variable `working` (1 = works, 2 = does not work) is recoded to 0/1. Observations with `working == 0` but positive hours or wages are set to missing. The same logic applies to second and third jobs.

### 2.4 Aggregation over up to three jobs

For each individual we compute:

- **Total monthly hours**: sum of hours from all jobs.
- **Total monthly wage**: sum of wages from all jobs.
- **Any work indicator**: `sum_working = 1` if at least one job is active.

### 2.5 Annual hours

Annual hours are calculated as:

`annual_hours = monthly_hours × 12 – (vacation_days + holidays) × (monthly_hours / 22)`

where 22 is the average number of working days per month. Observations with negative annual hours (mostly women on maternity leave) are set to missing.

### 2.6 Deflation

All nominal values (wages, consumption, GRP, minimum wage) are deflated using the regional CPI with 2016 as the base year:

`real_value = nominal_value / CPI_reg × 100`

### 2.7 Outlier removal (measurement error)

We apply the “jump” method (function `omit_me()`):

- For each individual, compute the product of forward and backward first differences in log wage, log earnings, log consumption, and log hours.
- Observations in the bottom 0.25% of this product distribution are considered measurement error and set to missing.

### 2.8 Log differences (growth rates)

For each individual we compute:

`dlwage = log(wage_t) – log(wage_{t-1})`  
`dlearn = log(earn_t) – log(earn_{t-1})`  
`dlcons = log(consump_t) – log(consump_{t-1})`  
`dlhours = log(hours_t) – log(hours_{t-1})`

### 2.9 Household (couple) formation

Using the kinship identifiers and spouse codes, we match husbands and wives into households. Only opposite‑sex married couples with non‑missing data for both spouses are retained. Each couple receives a unique household identifier `id_hh`.

---

## 3. Empirical methodology

The estimation proceeds in three stages.

### 3.1 First‑stage regressions (partialling out controls)

For each variable of interest (wage, consumption, earnings, hours) we run pooled OLS regressions on a set of controls. Residuals are saved for the second stage.

**Male wage growth equation (`dlwage`):**  
Controls: age dummies, education dummies, education × year interactions, region dummies, year dummies, employment status.

**Female wage growth equation (`dlwage_part`):** same controls, using spouse’s characteristics.

**Household consumption growth (`dlcons`):**  
Controls: age and education of both spouses, region, year, status, presence of children (preschool and teenage), share of working‑age members, and work indicators of both spouses.

**Earnings growth (`dlearn` and `dlearn_part`):** similar to consumption.

See `scripts/model_stage_1.R` for exact specifications.

### 3.2 Stage 2 – Wage process (joint male‑female)

We assume that log wages follow:

`w_{it} = p_{it} + u_{it} + ε_{it}`

with permanent shock `v` and transitory shock `u`. Shocks are correlated across spouses. The process is estimated by GMM using the following moment conditions (residuals from stage 1):

- Variances and autocovariances of `dwM`, `dwF`
- Cross‑covariances between spouses
- Covariances with consumption residuals (which are linked to wage shocks via parameters `k_c_u` and `k_c_v`).

The objective function `GMM_model_wage()` minimises the squared deviation between empirical and simulated moments over 13 parameters (variances of permanent and transitory shocks, covariances, measurement error variances, and consumption sensitivity parameters). Optimisation uses `fmincon` from the `pracma` package.

See `functions/GMM_utils.R` and `scripts/model_stage_2.R`.

### 3.3 Stage 3 – Preference parameters (labour supply elasticities)

We estimate a utility‑consistent labour supply equation where hours respond to wage shocks. The key parameters are:

- `k_hM_uM` – male Frisch own‑elasticity
- `k_hM_vM` – male Marshallian own‑elasticity
- `k_hF_uF` – female Frisch own‑elasticity
- `k_hF_vF` – female Marshallian own‑elasticity
- `k_hM_uF` – male Frisch cross‑elasticity (wife’s wage)
- `k_hM_vF` – male Marshallian cross‑elasticity
- `k_hF_uM` – female Frisch cross‑elasticity (husband’s wage)
- `k_hF_vM` – female Marshallian cross‑elasticity

Additional parameters `σ_γ` and `σ_ψ` capture measurement error in hours. The GMM objective `GMM_model_pref()` uses the wage process estimates from stage 2 as fixed inputs. Moments include variances and cross‑moments of earnings residuals (`dyM`, `dyF`) and their covariances with wage residuals.

See `scripts/model_stage_3.R`.

### 3.4 Bootstrap inference

To account for household‑level clustering and sampling uncertainty, we perform a **block bootstrap** (resampling households with replacement). For each bootstrap replication:

- Resample households from the original sample.
- Re‑estimate stage 2 and stage 3 models.
- Compute standard deviations of the parameters across `B = 1000` replications.

Bootstrap is implemented in `scripts/bootstrap.R` (baseline) and `scripts/bootstrap_pref_hetero.R` (education groups) using the `boot` package with parallel processing (`ncpus = 8`).

### 3.5 Heterogeneity analyses

#### By education

We split the sample into four groups based on whether each spouse has higher education:

1. Both higher
2. Only male higher
3. Only female higher
4. Neither higher

Preference parameters are re‑estimated separately for each group (`scripts/model_pref_hetero.R`). Bootstrap standard errors are computed within each group.

#### By income (conditional moments)

We allow elasticities to vary with household income. For each moment condition, we run year‑by‑year regressions of the moment on log earnings of husband and wife. The estimated coefficients `const`, `coef_learnM`, `coef_learnF` are then used to predict moments at any point of the income grid. Preference parameters are re‑estimated at each grid point (`scripts/model_pref_cond.R`). This yields elasticity surfaces as functions of both spouses’ incomes.

See `arrangers/data_mod_cond.R` for grid construction.

---
