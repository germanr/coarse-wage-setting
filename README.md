# Replication Materials: Coarse Wage-Setting and Behavioral Firms

This repository contains replication code and data for:

**Reyes, Germán. "Coarse Wage-Setting and Behavioral Firms." *Review of Economics and Statistics* (forthcoming).**

## Overview

This paper shows that wage bunching at round numbers is partly driven by firm coarse wage-setting. Using data from over 200 million new hires in Brazil, I establish that contracted salaries cluster at round numbers—about a third of new hires receive round-numbered salaries. Firms that hire at round-numbered salaries have worse market outcomes. I develop a wage-posting model where optimization costs lead to coarse rounded wages and provide evidence supporting two model predictions.

## Requirements

- **Stata 15 or higher**
- Required Stata packages: `reghdfe`, `ftools`, `binscatter`, `estout`, `coefplot`

To install required packages:
```stata
ssc install reghdfe
ssc install ftools
ssc install binscatter
ssc install estout
ssc install coefplot
```

## Data

### Included Data
The following auxiliary datasets are included:
- `data/cpi_mthly.dta` - Monthly CPI data
- `data/cpi_yrly.dta` - Yearly CPI data
- `data/state_mw.dta` - State minimum wages
- `data/microregion.dta` - Microregion identifiers
- `data/pme_2013.dta` - PME household survey (2013)
- `data/pnad_2013.dta` - PNAD household survey (2013)
- `data/cba.dta` - Collective bargaining agreements

### Confidential Data (not included)
The main analysis uses RAIS (Relação Anual de Informações Sociais), Brazil's matched employer-employee administrative dataset. RAIS is confidential and requires application to the Brazilian Ministry of Labor.

**How to access RAIS:**
1. Researchers affiliated with Brazilian institutions can apply directly to the Ministry of Labor
2. Foreign researchers can access RAIS through partnerships with Brazilian research institutions
3. See the [DIEESE](https://www.dieese.org.br/) or [IPEA](https://www.ipea.gov.br/) for more information

## Folder Structure

```
coarse-wage-setting/
├── code/
│   ├── 0-master.do           # Master file - runs all analyses
│   ├── clean/                # Data cleaning do-files
│   │   ├── create-census-firms.do
│   │   ├── create-firms-panel.do
│   │   ├── create-new-hires-sample.do
│   │   └── estimate-bins-excess-mass.do
│   └── results/              # Tables and figures do-files
│       ├── tables.do
│       ├── tables-appendix.do
│       ├── figures.do
│       └── figures-appendix.do
├── data/                     # Auxiliary datasets
├── raw_data/                 # RAIS microdata (not included)
├── results/                  # Output tables and figures
└── paper/                    # LaTeX source files
```

## Replication Instructions

1. **Obtain RAIS data**: Apply for access to RAIS microdata (see above)

2. **Set up paths**: Open `code/0-master.do` and verify the path structure matches your system. The code uses portable paths via `c(username)`.

3. **Run the master file**:
```stata
do "code/0-master.do"
```

## Output Files

### Tables (in `results/`)
- Main regression tables
- Appendix tables with robustness checks

### Figures (in `results/`)
- Wage distribution histograms
- Bunching estimates
- Firm outcome regressions
- Robustness checks

## Citation

```bibtex
@article{reyes2024coarse,
  title={Coarse Wage-Setting and Behavioral Firms},
  author={Reyes, Germ{\'a}n},
  journal={Review of Economics and Statistics},
  year={forthcoming}
}
```

## Contact

**Germán Reyes**<br>
Assistant Professor of Economics<br>
Middlebury College<br>
Email: greyes@middlebury.edu<br>
Web: [germanr.com](https://www.germanr.com)
