# Insper — Cátedra Ruth Cardoso · RA Work

This area hosts code I produced as a **Research Assistant** for projects linked to the
Insper Cátedra Ruth Cardoso. It is organized by research line and designed for
reproducible pipelines (data prep → estimation → figures).

---

## Projects

### 1) [DST-work](./DST-work/)
Research on consequences of the **end of Daylight Saving Time (DST) in Brazil**.
This folder contains two related subprojects:

- **[DST-ENEM](./DST-work/DST-ENEM/)**  
  Effects of the DST policy change on **high-stakes exam performance (ENEM)**.  
  **Methods:**  **Difference-in-Discontinuities (DiDisc)** — following
  [Picchetti, Pinto & Shinoki (arXiv:2405.18531)](https://arxiv.org/abs/2405.18531) — and **Geographical RDD** where spatial cutoffs apply.

- **[DST-Homicides-Labor](./DST-work/DST-Homicides-Labor/)**  
  Effects on **public safety (homicides)** and **labor outcomes**.  
  **Methods:** **Difference-in-Discontinuities (DiDisc)** — following
  [Picchetti, Pinto & Shinoki (arXiv:2405.18531)](https://arxiv.org/abs/2405.18531) — and **Geographical RDD** where spatial cutoffs apply.

---

### 2) [Fundef-Fundeb](./Fundef-Fundeb/)
Analysis of **municipal investment** dynamics around Brazil’s **FUNDEF → FUNDEB reform (2007)**.

- **Question:** How did the 2007 reform reshape municipal spending/investment patterns?  
- **Units/Panel:** Municipality × year (coverage per data availability).  
- **Outcomes (examples):** education investment, capital expenditures, staffing, enrollment.  
- **Methods:** Continuous treatment **DiD** / **event-study**, with fixed effects and robustness checks (placebos, alternative windows, re-weighting).

---

## Data Access

Some inputs are **large and/or restricted**. To keep the repo lean and compliant, this space
ships **code only** (plus occasional synthetic stubs).

- Place your local data under a non-tracked folder such as `data/` (see each project’s README).
- Update paths at the top of the scripts or in `conf/local.yml` templates when available.

---

## Getting Started

1. Open a project folder above and read its local `README.md`.
2. Set up the environment:
   ```r
   # R (>= 4.3 recommended)
   install.packages(c("data.table","dplyr","fixest","did","ggplot2","readr"))
   # if the project includes renv:
   install.packages("renv"); renv::restore()
