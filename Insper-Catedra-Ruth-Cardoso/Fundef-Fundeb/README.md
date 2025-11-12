# FUNDEF → FUNDEB (Brazil)

Analysis of municipal investment dynamics around the **2007 transition from FUNDEF to FUNDEB** in Brazil.

> [!WARNING]
> **Work in progress.**  
> This project is currently being **refactored** (scripts, variable definitions, and figures).  
> Interfaces and results may change, and some outputs are provisional.

## Overview
- **Goal:** quantify how the 2007 reform affected municipal spending/investment patterns.
- **Unit of analysis:** municipality × year.
- **Outcomes (examples):** education investment, capital expenditures, staffing, enrollment.
- **Methods:** panel **Difference-in-Differences** / **event study** with fixed effects and robustness checks
  (alternative windows, placebo years, sensitivity to controls/clustering).

## Repository layout
- **[Scripts/](./Scripts/)** – numbered pipeline (e.g., `01_*` prepare, `02_*` estimate, `03_*` figures).  
- `outputs/` – generated tables/figures (created at run time, may be git-ignored).

## Data access
Some inputs are **large and/or restricted**. The repository ships **code only**.  
Place local data in a non-tracked folder (e.g., `data/`) and update paths at the top of the scripts.

## Quick start
1. Open **Scripts/** and follow the numbered order.
2. Edit local paths and config toggles as indicated in script headers.
3. Run to produce tables/figures under `outputs/`.

---

Issues and PRs that improve portability, documentation, or robustness are welcome.
