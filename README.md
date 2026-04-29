# Family Labor Supply and Wage Shocks in an Emerging Market: Evidence from Russia

This repository contains the replication code for the empirical analysis presented in

> **Family Labor Supply and Wage Shocks in an Emerging Market: Evidence from Russia**  
> *Alexey Zamnius*  
> [https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4822593](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4822593)

## Overview

The project estimates Frisch and Marshallian own‑ and cross‑elasticities of labor supply for married couples in Russia, allowing for heterogeneity by:

- Education level (higher vs. non‑higher for each spouse)
- Household income (conditional moments)

The estimation proceeds in three stages:

1. **First‑stage regressions** – partial out controls (age, education, region, year, children) from wage, consumption, earnings, and hours growth.
2. **Wage process (stage 2)** – GMM estimation of the joint stochastic process for male and female wages (permanent and transitory shocks).
3. **Preferences (stage 3)** – GMM estimation of labor supply elasticities using the estimated wage process as input.

Bootstrap inference (household clustering) is used throughout.

## Repository structure

```
.
├── README.md
├── script.R          # main master script
├── params.R          # global parameters (years, bootstrap settings)
├── packages.txt      # list of required R packages (for manual installation)
├── renv.lock         # lock file for renv (exact package versions)
├── .Rprofile         # auto‑activates renv
│
├── arrangers/        # data preparation scripts
├── scripts/          # model estimation and plotting
├── functions/        # core functions used by the scripts
├── data/             # raw data (see below for details)
└── figures/          # generated plots (not included, created by scripts)
```

## Requirements

- **R version 4.5.1 or higher** (tested with 4.5.1)
- **R packages** – see `packages.txt` or `renv.lock`

## Reproduction instructions

> **Note:** A detailed guide covering data sources, cleaning steps, and the econometric methodology is available in [`DATA_METHODOLOGY_GUIDE.md`](DATA_METHODOLOGY_GUIDE.md).

### 1. Clone the repository

```bash
git clone https://github.com/zamnius924/family-labor-supply
cd family-labor-supply
```

### 2. Obtain the RLMS‑HSE data

The RLMS‑HSE data are **not** redistributable. You must obtain them directly from the [RLMS‑HSE website](https://www.hse.ru/en/rlms/).

After registration, download the following files (version 4 or later):

- `USER_RLMS-HSE_IND_1994_2020_v4_rus.sav`
- `USER_RLMS-HSE_HH_1994_2020_rus.sav`
- `Доходы и расходы.sav`
- `Идентификационные номера родственников.sav`
- `Идентификационные номера индивидов.sav`

Place them in the `data/` folder.

A detailed description of the data cleaning and processing steps is provided in the paper (Section 2.2 – Data and Sample).

### 3. Obtain auxiliary data

The following files are **not** part of the standard RLMS release. They are constructed from official Rosstat (Russian Federal State Statistics Service) sources:

- `CPI_reg.xlsx` – regional consumer price index (base 2016)
- `days.xlsx` – number of working days and public holidays per year
- `grp.xlsx` – gross regional product (GRP) by region
- `fed_min.xlsx` – federal minimum wage

### 4. Restore R environment

Open an R session in the project root and run:

```r
install.packages("renv")
renv::restore()
```

This will install exact versions of all required packages into a project‑local library (folder renv/library).

### 5. Run the analysis

```r
source("script.R")
```

All results (coefficient tables, bootstrap standard errors, plots) will be printed/displayed. Figures are generated interactively; you may save them manually.

## Notes

- The code uses **relative paths** and expects the folder structure exactly as above. No `setwd()` calls are used.
- The file `functions/rlms_fix.R` modifies two internal functions of the `rlms` package using `assignInNamespace`. This is a workaround for specific RLMS data quirks and may break after a package update. It is **not** required for other projects.
- Bootstrap may take several hours, especially the conditional moment part (grid of income points). You can reduce `B` in `params.R` for testing.

## Citation

If you use this code or the results, please cite the accompanying paper:

> Zamnius, Alexey, Estimating Labor Supply Elasticities in the Russian Federation Using a Two Earners Life Cycle Model. Available at SSRN: https://ssrn.com/abstract=4822593 or http://dx.doi.org/10.2139/ssrn.4822593

## License

This code is provided under the MIT License. The RLMS‑HSE data are subject to their own terms of use.