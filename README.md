# Comparative Efficacy of Anti-CD20 Monoclonal Antibodies for RRMS
## A Systematic Review and Network Meta-Analysis

**Author:** Syeda Kanwal Ali Rizvi  
**Institution:** Middlesex University, London  
**Module:** BIO3888  
**Submission:** April 2026  

---

## Overview
This repository contains the screening pipeline and statistical analysis 
scripts used in the systematic review and network meta-analysis comparing 
the efficacy of ocrelizumab, ofatumumab, and ublituximab for 
relapsing-remitting multiple sclerosis (RRMS).

---

## Repository Contents

| File | Description |
|---|---|
| `NMA_AntiCD20_RRMS.R` | R script for network meta-analysis using netmeta |
| `MultiDB_Collection_Screening_AntiCD20_RRMS.ipynb` | Multi-database search and screening pipeline |
| `RuleBased_Screening_AntiCD20_RRMS.ipynb` | Rule-based keyword screening algorithm |
| `PRISMA_AntiCD20_RRMS.ipynb` | PRISMA flow diagram generator |
| `screening_INCLUDE.csv` | Final included records from screening |
| `screening_audit_trail.xlsx` | Complete audit trail - 238 records with decisions |

---

## How to reproduce the analysis

### Screening pipeline
1. Open `MultiDB_Collection_Screening_AntiCD20_RRMS.ipynb` in Google Colaboratory
2. Run all cells — this executes the search, deduplication, and screening
3. Outputs: `screening_INCLUDE.csv` and `screening_audit_trail.xlsx`

### Network meta-analysis
1. Open `NMA_AntiCD20_RRMS.R` in RStudio
2. Install required packages: `netmeta`, `meta`, `ggplot2`, `dplyr`
3. Run script — outputs forest plots, SUCRA rankings, and summary table

---

## Software requirements

**R version:** 4.5.3  
**Key R packages:** netmeta 3.3.1, meta 8.3.0, ggplot2 4.0.2  
**Python version:** 3.12.13  
**Environment:** Google Colaboratory  

---

## License
MIT License — see LICENSE file for details.

---

## Citation
Rizvi SKA (2026). Comparative efficacy of anti-CD20 monoclonal antibodies 
for RRMS: screening pipeline and analysis scripts. 
https://github.com/amiresque/Comparative-efficacy-of-anti-CD20s-for-RRMS-NMA-
