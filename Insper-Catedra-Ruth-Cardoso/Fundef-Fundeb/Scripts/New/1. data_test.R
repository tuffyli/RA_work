# ============================================================================ #
# PAPER  : Novo FUNDEB e Resultados Educacionais Municipais                    #
# SCRIPT : 1_data_final.R  —  Complete Data Construction Pipeline             #
# AUTHOR : Tuffy Licciardi Issa                                                #
# LAST EDITED: 18/06/2026                                                      #
#                                                                              #
# PIPELINE OVERVIEW (run top-to-bottom for a full, clean build):              #
#                                                                              #
#   1. FINBRA     — Municipal education expenditures (2005–2021)              #
#   2. TRANSFERS  — FUNDEF/FUNDEB transfers received  (2000–2021)             #
#   3. SAEB/IDEB  — Test scores and approval rates    (2005–2021)             #
#                   ↳ EXPORT: painel_notas_transferencias_2000_2024.csv       #
#   4. SIOPE      — FUNDEB realised revenues          (2005–2024)             #
#                   ↳ Sections 4.2.1–4.2.2 are RUN-ONCE (flag-guarded)       #
#   5. SIMULATION — FUNDEF counterfactual coefficients (base year: 2006)      #
#   6. REGRESSION — Final panel joining all sources                           #
#   7. FLAGS      — Outlier flags on enrolment & spending growth              #
#                                                                              #
# EXTERNAL FILES THAT MUST EXIST BEFORE RUNNING:                              #
#   expected_2007_fundef_value.RDS — Counterfactual FUNDEF fund in 2007       #
#   pesos_saeb3.rds                — SAEB enrolment weights per municipality  #
#   censo_escolar_base_v2.rds      — Annual school-census panel (2005–2018)   #
#   amcs.dta                       — Minimum comparable areas crosswalk       #
#                                                                              #
# KEY INTERMEDIATE FILES CREATED BY THIS SCRIPT:                              #
#   finbra_2005_2012.rds                         Section 1.1                  #
#   finbra_2007_2021.rds                         Section 1.2                  #
#   FINBRA_EDU_05_21.csv                         Section 1.3  ← NEW STEP     #
#   fundef_fundeb_2000_2001.rds                  Section 2                    #
#   defla_fundef_fundeb_2000_2001.rds            Section 2                    #
#   painel_notas_transferencias_2000_2024.csv    Section 3  ← NEW EXPORT     #
#   censo_2006_filtrado.rds                      Section 5.1                  #
#   censo_2006_filtrado_mun.rds                  Section 5.1                  #
#   simulacao_const.rds                          Section 5.6                  #
#   regdf.rds                                    Section 6                    #
#   regdf_flags.rds                              Section 7                    #
# ============================================================================ #


# ============================================================================ #
# 0. SETUP ----
# ============================================================================ #

# ── 0.1 Libraries ──────────────────────────────────────────────────────────── #

# Core data manipulation and visualisation
library(tidyverse)
library(readxl)
library(writexl)
library(janitor)       # clean_names() standardises column names to snake_case

# Econometric models
library(lmtest)
library(fixest)        # Fast fixed-effects estimators (feols, feglm)
library(AER)           # IV regression (ivreg)
library(rdrobust)      # Regression discontinuity

# Matching and DiD
library(cobalt)
library(did)
library(MatchIt)

# Reporting and output
library(stargazer)
library(xtable)
library(knitr)
library(kableExtra)
library(broom)
library(fastDummies)

# Spatial and cartographic
library(sf)
library(geobr)
library(RColorBrewer)
library(ggnewscale)

# Plot utilities
library(scales)
library(ggbreak)
library(ggtext)

# Miscellaneous utilities
library(data.table)
library(httr2)         # HTTP requests for the FNDE Open Data API (Section 4)
library(glue)          # Parameterised string interpolation for URL building
library(jsonlite)
library(haven)         # Read Stata .dta files (amcs crosswalk)


# ── 0.2 Global Options ─────────────────────────────────────────────────────── #

# Suppress scientific notation so large BRL values are printed in full
options(scipen = 999)


# ── 0.3 Path Configuration ─────────────────────────────────────────────────── #
# All file paths in this script are built from these four roots.
# Update them to match your local or network drive setup BEFORE running.

PATH_GIOVANNI <- "Z:/Giovanni Zanetti/Av. Novo Fundeb"  # Shared project data
PATH_TUFFY    <- "Z:/Tuffy/Paper - Educ"                # Personal outputs
PATH_IFB      <- "Z:/Arquivos IFB"                      # Raw public microdata
PATH_BRASIL   <- "Z:/Tuffy/Paper - Brasil"              # Companion paper folder


# ── 0.4 Helper Function: Variable Labels ───────────────────────────────────── #
# Attaches a human-readable label to a column so it appears in View() and
# in some regression output packages (e.g. stargazer). Not required for
# computation but useful when inspecting or sharing the data.

add_label <- function(data, variable, label) {
  if (!variable %in% names(data)) {
    stop("Error: variable '", variable, "' not found in data frame.")
  }
  attr(data[[variable]], "label") <- label
  return(data)
}


# ============================================================================ #
# 1. FINBRA — Municipal Education Expenditures ----
# ============================================================================ #
# FINBRA (Finanças do Brasil) is the STN's annual municipal finance report.
# It records actual expenditures broken down by functional area for every
# Brazilian municipality. We use function 12 (Education) and its sub-functions.
#
# Two file formats exist across the sample period:
#   • 2005–2012: one Excel (.xlsx) file per year, spending aggregated by function
#   • 2013–2021: one CSV (.csv) file per year, spending at account-code level
#                (requires a pivot to go from long to wide)
#
# Section 1.3 combines both periods into FINBRA_EDU_05_21.csv,
# which is the file consumed by the regression assembly in Section 6.


# ── 1.1 FINBRA 2005–2012 (Excel format) ─────────────────────────────────── #

# Load the minimum-comparable-areas crosswalk used in spatial robustness checks.
# This is referenced in the companion analysis scripts; keeping it here ensures
# it is available throughout the session.
amcs <- read_dta(file.path(PATH_BRASIL, "amcs.dta"))

# Stack one xlsx file per year into a single data frame.
for (year in 2005:2012) {
  
  # Initialise an empty accumulator on the first iteration
  if (year == 2005) { finbra_2005_2012 <- data.frame() }
  
  filename <- file.path(
    PATH_GIOVANNI,
    "Dados/Gastos municipais/FINBRA/Despesas",
    paste0("finbra_", year, ".xlsx")
  )
  
  finbra_ano <- read_excel(filename) %>%
    mutate(ano = year) %>%
    select(ano, everything())   # move the year column to position 1
  
  finbra_2005_2012 <- bind_rows(finbra_2005_2012, finbra_ano)
  
  message("Year ", year, " — import complete.")
  print(colnames(finbra_ano))
  rm(finbra_ano, filename, year)
}

# ── Tidy the 2005–2012 panel ──
# The xlsx files contain two overlapping municipality-name columns
# ('Nome Caixa' and 'MUNICIPIO'). We coalesce them into a single NomMun column.
# codmun is constructed as [2-digit UF code] * 10,000 + [4-digit mun. code],
# giving a unique 6-digit identifier consistent with the transfer data.
finbra_2005_2012 <- finbra_2005_2012 %>%
  mutate(codmun = as.numeric(CdUF) * 10000 + as.numeric(CdMun)) %>%
  relocate(MUNICIPIO, .after = "Nome Caixa") %>%
  mutate(NomMun = toupper(coalesce(`Nome Caixa`, MUNICIPIO))) %>%
  select(-`Nome Caixa`, -MUNICIPIO) %>%
  relocate(NomMun, .after = UF) %>%
  relocate(codmun, .after = CdMun)

# Remove non-education functional areas (functions other than 12).
# The column ranges cover functions 01–11 and 13–28.
# The last two columns are administrative metadata, not spending values.
finbra_2005_2012 <- finbra_2005_2012 %>%
  select(-c(
    `Legislativa`:`Demais Subfunções 11`,
    `Cultura`:`Demais Subfunções 28`,
    ncol(finbra_2005_2012),
    ncol(finbra_2005_2012) - 1
  )) %>%
  arrange(ano, codmun)

# Save the cleaned 2005–2012 panel. Used as input to Section 1.3 below.
saveRDS(finbra_2005_2012, file.path(PATH_TUFFY, "Dados/finbra_2005_2012.rds"))


# ── 1.2 FINBRA 2013–2021 (CSV format) ───────────────────────────────────── #
# From 2013 onwards FINBRA is published as a long-form CSV at account-code level.
# We filter to function 12 (Education) plus the grand-total expenditure row,
# then pivot from long to wide so each sub-function becomes its own column.

for (year in 2013:2021) {
  
  if (year == 2013) { finbra_novo <- data.frame() }
  
  filename <- file.path(
    PATH_GIOVANNI,
    "Dados/Gastos municipais/FINBRA/Despesas",
    paste0("finbra_", year, ".csv")
  )
  
  finbra_ano <- read.csv2(filename, skip = 3, fileEncoding = "latin1") %>%
    mutate(
      ano    = year,
      # Strip the "Prefeitura Municipal de" prefix from the institution name,
      # then remove the trailing 5-character CNPJ suffix
      NomMun = Instituição %>%
        str_remove("Prefeitura Municipal de ") %>%
        str_sub(end = -6)
    ) %>%
    relocate(NomMun, .after = "Instituição") %>%
    select(-c(Instituição, Identificador.da.Conta)) %>%
    # Keep only Education sub-function rows (prefix "12" or "FU12") and the
    # three naming variants of the total expenditure row
    filter(
      str_starts(Conta, "12") | str_starts(Conta, "FU12") |
        Conta %in% c(
          "Despesas (Exceto Intra-Orçamentárias)",
          "Despesas (Exceto Intraorçamentárias)",
          "Despesas Exceto Intraorçamentárias"
        )
    ) %>%
    # Reshape: one row per municipality × accounting category (Coluna),
    # one column per account code
    pivot_wider(
      id_cols    = c(NomMun:Coluna, ano),
      names_from  = Conta,
      values_from = Valor
    )
  
  finbra_novo <- bind_rows(finbra_novo, finbra_ano)
  
  message("Year ", year, " — import complete.")
  rm(finbra_ano, filename, year)
}

# ── Rename to a stable schema ──
# Column names vary slightly across years due to system changes in the SICONFI
# platform; here we impose a single consistent naming convention.
colnames(finbra_novo) <- c(
  "nom_mun", "codigo_ibge", "uf", "populacao", "Coluna", "ano",
  "Despesas (Exceto Intra-Orçamentárias)", "Educação", "Ensino Fundamental",
  "Educação Infantil", "Ensino Médio", "Ensino Superior",
  "Demais Subfunções 12", "Educação de Jovens e Adultos",
  "Educação Especial", "Educação Básica", "Ensino Profissional",
  "Despesas (Exceto Intraorçamentárias)", "Administração Geral",
  "Despesas Exceto Intraorçamentárias", "FU12"
)

finbra_novo <- finbra_novo %>%
  mutate(
    # Coalesce the three naming variants of total expenditure into one column
    despesas_totais = coalesce(
      `Despesas (Exceto Intra-Orçamentárias)`,
      `Despesas (Exceto Intraorçamentárias)`,
      `Despesas Exceto Intraorçamentárias`
    ),
    # Coalesce the two naming variants of the residual education sub-function
    `Demais Subfunções 12` = coalesce(`Demais Subfunções 12`, `FU12`)
  ) %>%
  clean_names() %>%
  # Drop columns that have been coalesced into others or are irrelevant
  select(-c(administracao_geral, educacao_basica, starts_with("despesas_exceto"), fu12)) %>%
  # IBGE codes in the CSV have 7 digits (including a check digit at the end).
  # Remove the last digit to produce the 6-digit code used throughout the analysis.
  mutate(codigo_ibge = as.numeric(str_sub(as.character(codigo_ibge), 1, -2)))

saveRDS(finbra_novo, file.path(PATH_TUFFY, "Dados/finbra_2007_2021.rds"))


# ── 1.3 Combine Both Periods → FINBRA_EDU_05_21.csv ─────────────────────── #
# This step (originally in gastos_municipais.R by Giovanni Zanetti) harmonises
# the 2005–2012 column schema to match the 2013–2021 format, stacks both, and
# exports the combined CSV that Section 6 reads as df_fib.
#
# Key difference between periods:
#   • 2005–2012 rows have coluna = NA (that accounting category was not
#     reported before 2013). The downstream filter
#     `ano < 2013 | coluna == "Despesas Empenhadas"` handles this correctly.
#   • codmun in the 2005–2012 frame is already 6-digit and matches codigo_ibge
#     in the 2013–2021 frame; we simply rename it.

finbra_para_unir <- finbra_2005_2012 %>%
  rename(
    codigo_ibge = codmun,  # 6-digit already; just align the column name
    nom_mun     = NomMun,
    uf          = UF,
    populacao   = Populacao
  ) %>%
  clean_names() %>%          # converts any remaining Title-Case names to snake_case
  select(-c(cd_uf, cd_mun)) %>%  # component codes are redundant given codigo_ibge
  rename(despesas_totais = despesas_por_funcao)  # align with 2013+ column name

finbra_completo <- bind_rows(finbra_para_unir, finbra_novo)

write.csv2(
  finbra_completo,
  file.path(
    PATH_GIOVANNI,
    "Dados/Gastos municipais/FINBRA/Despesas/FINBRA_EDU_05_21.csv"
  ),
  row.names = FALSE
)

rm(finbra_para_unir, finbra_completo)


# ============================================================================ #
# 2. FUNDEF / FUNDEB Intergovernmental Transfers ----
# ============================================================================ #
# Brazil's two successive education funds (FUNDEF 2000–2006, FUNDEB 2007–2021)
# work through a constitutional redistribution mechanism: municipalities and
# states contribute a share of their fiscal revenues to a state-level fund,
# which is then redistributed in proportion to enrolled students.
#
# Under FUNDEF, only primary (EF) enrolments counted.
# Under FUNDEB, all levels of basic education (from creche to EM) were included.
#
# We load two series and stack them:
#   (a) transfers to municipalities — from the FNDE historical series CSV files
#   (b) transfers to state school networks — from a separate FNDE state-level CSV
#
# Both are collapsed from transfer-type level to municipality (or UF) × year level
# and deflated to 2021 BRL using the accumulated IPCA index.


# ── 2.1 Municipal Network (2000–2021) ────────────────────────────────────── #

caminho1 <- file.path(
  PATH_GIOVANNI,
  "Dados/Transferências Fundeb mun e ufs/transferências_para_municípios_2000_2006.csv"
)
caminho2 <- file.path(
  PATH_GIOVANNI,
  "Dados/Transferências Fundeb mun e ufs/transferências_para_municípios_2007_2021.csv"
)

# Stack the pre- and post-FUNDEB transition CSV files into one panel
df_anual <- read_csv2(caminho1, locale = locale(encoding = "latin1")) %>%
  clean_names() %>%
  bind_rows(
    read_csv2(caminho2, locale = locale(encoding = "latin1")) %>%
      clean_names()
  ) %>%
  arrange(codigo_ibge, ano) %>%
  select(-codigo_siafi)   # SIAFI code is redundant given the IBGE code

colnames(df_anual)

# Collapse from transfer-type level to municipality × year level.
# total_politica = full policy-relevant inflow (all FUNDEF/FUNDEB components).
# total_coun     = federal top-up (complementação) component only;
#                  zero for municipalities that did not receive top-up.
df_anual <- df_anual %>%
  group_by(codigo_ibge, ano) %>%
  summarise(
    uf             = first(uf),
    nome           = first(municipio),
    total_politica = sum(valor_consolidado, na.rm = TRUE),
    total_coun     = sum(
      valor_consolidado[transferencia %in% c(
        "FUNDEF - COUN", "FUNDEB - COUN", "FUNDEB - COUN VAAT",
        "FUNDEB - COUN VAAF", "AJUSTE FUNDEB - AJUSTE FUNDEB VAAF",
        "AJUSTE FUNDEB - AJUSTE FUNDEB VAAT"
      )],
      na.rm = TRUE
    ),
    .groups = "drop"
  )

summary(df_anual)


# ── 2.2 State Network (2007–2021) ────────────────────────────────────────── #
# State (Estadual) school networks are funded from the state government's
# share of the FUNDEF/FUNDEB fund. We load their transfers separately because
# identifying them via codigo_ibge requires a UF-level code crosswalk.

df_ufs <- read.csv2(
  file.path(
    PATH_GIOVANNI,
    "Dados/Transferências Fundeb mun e ufs/transferências_para_estados_2007_2021.csv"
  ),
  fileEncoding = "latin1"
) %>%
  clean_names() %>%
  left_join(
    read_excel(file.path(PATH_GIOVANNI, "Dados/Códigos_UF.xlsx")),
    by = "uf"
  ) %>%
  mutate(codigo_ibge = as.numeric(codigo_ibge)) %>%
  group_by(codigo_ibge, ano) %>%
  summarise(
    uf             = first(uf),
    nome           = first(nm_uf),
    total_politica = sum(valor_consolidado, na.rm = TRUE),
    total_coun     = sum(
      valor_consolidado[transferencia %in% c(
        "FUNDEF - COUN", "FUNDEB - COUN", "FUNDEB - COUN VAAT",
        "FUNDEB - COUN VAAF", "AJUSTE FUNDEB - AJUSTE FUNDEB VAAF",
        "AJUSTE FUNDEB - AJUSTE FUNDEB VAAT"
      )],
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# ── 2.3 Stack Municipal + State and Deflate ───────────────────────────────── #

# Combine both networks into one long panel indexed by (codigo_ibge, ano)
df <- bind_rows(df_anual, df_ufs)

# Save the nominal (non-deflated) version for reference
saveRDS(df, file.path(PATH_TUFFY, "Dados/fundef_fundeb_2000_2001.rds"))

# ── IPCA deflation ──
# Load the accumulated IPCA (Brazil's official CPI) from an Excel file.
# We normalise the cumulative index so that 2021 = 1, then divide nominal
# values by the index to express everything in constant 2021 BRL.
df_ipca <- read_excel(
  file.path(PATH_GIOVANNI, "Dados/IPCA_acumulado_ano.xlsx"),
  skip = 3
)
colnames(df_ipca) <- c("ano", "ipca")

df_ipca <- df_ipca %>%
  mutate(ano = as.numeric(ano)) %>%
  filter(ano %in% 2000:2021) %>%
  arrange(ano)

df_ipca <- df_ipca %>%
  mutate(
    ipca   = as.numeric(gsub(",", ".", ipca)),
    indice = cumprod(1 + as.numeric(ipca) / 100)
  ) %>%
  mutate(indice = indice / indice[ano == 2021])   # base year = 2021

# Apply the deflation index to both transfer aggregates
df <- df %>%
  left_join(df_ipca, by = "ano") %>%
  mutate(
    total_politica_d = total_politica / indice,   # total fund transfer, real BRL
    total_coun_d     = total_coun     / indice    # federal top-up, real BRL
  )

saveRDS(df, file.path(PATH_TUFFY, "Dados/defla_fundef_fundeb_2000_2001.rds"))

rm(df_ipca, df_anual, df_ufs, caminho1, caminho2)


# ============================================================================ #
# 3. SAEB / IDEB — Test Scores and Approval Rates ----
# ============================================================================ #
# IDEB (Índice de Desenvolvimento da Educação Básica) combines two components:
#   • SAEB scores — standardised exam in Mathematics and Portuguese Language
#   • Approval rates — share of enrolled students who pass and advance a grade
#
# Both are published biennially at municipality level by INEP, from 2005 onward.
# INEP publishes one wide-format Excel file per schooling cycle, with one
# column per biennial edition. We loop over the three cycles and reshape
# each file into a long panel (one row per municipality × year × cycle).
#
# The resulting df_nota joins exam scores onto the transfer panel (df) and is
# exported to CSV at the end of this section; Sections 5 and 6 read that CSV.

ciclos <- c(
  "anos_iniciais",  # Grades 1–5  (primary, 'EF inicial')
  "anos_finais",    # Grades 6–9  (lower secondary, 'EF final')
  "ensino_medio"    # Grades 10–12 (upper secondary, 'EM')
)

for (ciclo in ciclos) {
  
  # Initialise accumulators on the first iteration of the loop
  if (ciclo == "anos_iniciais") {
    df_apr  <- NULL
    df_saeb <- NULL
  }
  
  # Load the INEP spreadsheet for this cycle.
  # INEP stores all biennial editions (2005, 2007, …, 2023) as wide columns.
  ideb <- read_excel(
    file.path(
      PATH_GIOVANNI,
      "Dados/IDEB",
      paste0("divulgacao_", ciclo, "_municipios_2023"),
      paste0("divulgacao_", ciclo, "_municipios_2023"),
      paste0("divulgacao_", ciclo, "_municipios_2023.xlsx")
    ),
    skip = 9
  ) %>%
    clean_names() %>%
    rename(
      codigo_ibge = "co_municipio",
      nome        = "no_municipio",
      uf          = "sg_uf"
    )
  
  
  # ── 3.1 Approval Rates ───────────────────────────────────────────────────── #
  # Select vl_aprovacao_* columns and pivot from wide (one col per year)
  # to long (one row per year × grade). The regex captures the biennial year
  # and an INEP-internal grade code from the column name.
  apr <- ideb %>%
    select(c(1:4, starts_with("vl_aprovacao_")))
  
  apr <- apr %>%
    pivot_longer(
      cols          = matches("^vl_aprovacao_\\d{4}_(si_4|si|[1-4])$"),
      names_to      = c("ano", "serie"),
      names_pattern = "vl_aprovacao_(\\d{4})_(si_4|si|[1-4])",
      values_to     = "tx_aprov"
    ) %>%
    mutate(
      ano = as.integer(ano),
      # Map INEP's internal grade codes to meaningful series labels.
      # "si_4" is the cycle-average; numeric codes (1–4) represent grade ranks
      # within the cycle (the actual grade number depends on the cycle).
      serie_label = case_match(
        serie,
        "si_4" ~ ifelse(ciclo == "anos_iniciais", "iniciais",
                        ifelse(ciclo == "anos_finais", "finais", "em")),
        "si"   ~ "1",
        "1"    ~ ifelse(ciclo == "anos_iniciais", "2",
                        ifelse(ciclo == "anos_finais", "6", "1em")),
        "2"    ~ ifelse(ciclo == "anos_iniciais", "3",
                        ifelse(ciclo == "anos_finais", "7", "2em")),
        "3"    ~ ifelse(ciclo == "anos_iniciais", "4",
                        ifelse(ciclo == "anos_finais", "8", "3em")),
        "4"    ~ ifelse(ciclo == "anos_iniciais", "5",
                        ifelse(ciclo == "anos_finais", "9", "4em"))
      )
    ) %>%
    select(uf, codigo_ibge, nome, rede, ano, serie_label, tx_aprov) %>%
    pivot_wider(
      names_from   = serie_label,
      names_prefix = "tx_aprovacao_",
      values_from  = tx_aprov
    ) %>%
    mutate(across(starts_with("tx_aprovacao_"), ~ round(as.numeric(.), 1)))
  
  # Accumulate across cycles with a full join so every municipality × year
  # combination is retained even if it appears in only one cycle file
  if (is.null(df_apr)) {
    df_apr <- apr
  } else {
    df_apr <- full_join(df_apr, apr, by = c("codigo_ibge", "uf", "nome", "rede", "ano"))
  }
  
  
  # ── 3.2 SAEB Exam Scores ─────────────────────────────────────────────────── #
  # Select vl_nota_* columns and pivot similarly.
  # The cycle suffix embedded in the output column name marks which exam level
  # the score belongs to: _5_ = grade 5 (iniciais), _9_ = grade 9 (finais),
  # _em_ = upper secondary.
  saeb <- ideb %>%
    select(c(1:4, starts_with("vl_nota_")))
  
  saeb <- saeb %>%
    mutate(across(starts_with("vl_nota_"), ~ round(as.numeric(.), 2))) %>%
    pivot_longer(
      cols          = matches("^vl_nota_(media|matematica|portugues)_\\d{4}$"),
      names_to      = c("indicador", "ano"),
      names_pattern = "vl_nota_(media|matematica|portugues)_(\\d{4})",
      values_to     = "valor"
    ) %>%
    mutate(ano = as.integer(ano), valor = as.numeric(valor)) %>%
    pivot_wider(
      id_cols     = c(uf, codigo_ibge, nome, rede, ano),
      names_from  = indicador,
      names_prefix = paste0(
        "vl_nota_",
        ifelse(ciclo == "anos_iniciais", "5",
               ifelse(ciclo == "anos_finais", "9", "em")),
        "_"
      ),
      values_from = valor
    )
  
  if (is.null(df_saeb)) {
    df_saeb <- saeb
  } else {
    df_saeb <- full_join(df_saeb, saeb, by = c("uf", "codigo_ibge", "nome", "rede", "ano"))
  }
  
  message("SAEB/IDEB processing complete for cycle: ", ciclo)
  rm(saeb, ideb, apr)
}


# ── 3.3 Join Scores with Approval Rates, Then Join onto Transfer Panel ────── #
# df_ideb: one row per municipality × year × school network (rede),
# containing both score and approval-rate columns for all three cycles.
df_ideb <- full_join(
  df_saeb, df_apr,
  by = c("codigo_ibge", "rede", "uf", "ano", "nome")
)

# Merge the SAEB data onto the transfer panel (df) by municipality and year.
# This creates the core longitudinal dataset linking fund receipts to outcomes.
# The join is left so that every (codigo_ibge, ano) in the transfer panel is
# retained, even if no SAEB observation exists for that year.
df_nota <- left_join(df, df_ideb, by = c("ano", "codigo_ibge")) %>%
  rename(
    uf   = "uf.x",
    nome = "nome.x"
  ) %>%
  select(-c(uf.y, nome.y))

rm(df_apr, df_saeb, df_ideb, ciclo, ciclos)


# ── EXPORT: Notes + Transfers Panel ─────────────────────────────────────── #
# Write df_nota to disk. This CSV is required by Section 5 (which runs after
# a full environment reset via rm/gc) and by Section 6. Without this export,
# neither of those sections can be run independently or reproduced from scratch.
write.csv2(
  df_nota,
  file.path(
    PATH_GIOVANNI,
    "Dados/painel_notas_transferencias_2000_2024.csv"
  ),
  row.names = FALSE
)


# ============================================================================ #
# 4. SIOPE — FUNDEB Realised Revenues ----
# ============================================================================ #
# SIOPE (Sistema de Informações sobre Orçamentos Públicos em Educação) is the
# FNDE's database of municipal and state education budget declarations.
# We use it to extract the realised FUNDEB receipt and deduction items per
# municipality, enabling a liquid (net) transfer calculation.
#
# 4.2.1 — [RUN-ONCE] Download raw CSVs per UF × year from the FNDE API.
# 4.2.2 — [RUN-ONCE] Stack UF files into one CSV per calendar year.
# 4.2.3 — Import annual CSVs into a named list (runs on every execution).
# 4.2.4 – 4.2.6 — Liquid-transfer construction and join with df_nota
#          (preserved in commented form; activate when data quality checks pass).


# ── 4.1 Note on Missing Approval Rates ──────────────────────────────────── #
# Because IDEB was introduced in 2007, SAEB scores and approval rates are only
# available from 2005 (biennial) for EF and from 2017 for EM. Years 2000–2004
# (EF) and 2000–2016 (EM) have no approval rates in df_nota.
# Pre-2005 SAEB scores (2001, 2003) would be needed for parallel-trends testing
# but are not yet integrated; those rows remain NA in the panel.


# ── 4.2 FUNDEB Transfers from SIOPE ─────────────────────────────────────── #

### 4.2.1 [RUN-ONCE] Download Raw SIOPE Files from FNDE Open Data API ─────── ###
# Set RUN_ONCE_SIOPE_DOWNLOAD <- TRUE only on the very first run.
# Once files are written to PATH_GIOVANNI/Dados/SIOPE/, set it back to FALSE.

RUN_ONCE_SIOPE_DOWNLOAD <- FALSE   # ← change to TRUE on first run only

if (RUN_ONCE_SIOPE_DOWNLOAD) {
  
  # Destination directory for downloaded per-UF × year CSV files
  pasta_destino <- file.path(PATH_GIOVANNI, "Dados/SIOPE")
  if (!dir.exists(pasta_destino)) dir.create(pasta_destino, recursive = TRUE)
  
  # All 27 Brazilian UF abbreviations
  ufs <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA",
           "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN",
           "RO","RR","RS","SC","SE","SP","TO")
  
  # FNDE changed the reporting period code in 2017:
  # period 1 = annual summary (up to 2016); period 6 = December (2017 onward)
  f_periodo <- function(ano) ifelse(ano <= 2016, 1, 6)
  
  falhas <- list()   # log of download failures for post-hoc inspection
  
  for (ano in 2000:2024) {
    for (uf in ufs) {
      
      periodo <- f_periodo(ano)
      
      # Build an OData query URL with year, period, and UF parameters.
      # Only the columns we need are requested to minimise download size.
      url <- glue(
        "https://www.fnde.gov.br/olinda-ide/servico/DADOS_ABERTOS_SIOPE/versao/v1/odata/",
        "Receita_Siope(Ano_Consulta=@Ano_Consulta,Num_Peri=@Num_Peri,Sig_UF=@Sig_UF)",
        "?@Ano_Consulta={ano}&@Num_Peri={periodo}&@Sig_UF='{uf}'",
        "&$format=text/csv",
        "&$select=TIPO,NUM_ANO,NUM_PERI,COD_UF,SIG_UF,COD_MUNI,NOM_MUNI,",
        "COD_EXIB_FORMATADO,NOM_ITEM,IDN_CLAS,NOM_COLU,NUM_NIVE,NUM_ORDE,VAL_DECL"
      )
      
      caminho_arquivo <- file.path(pasta_destino, glue("siope_{uf}_{ano}_P{periodo}.csv"))
      
      resultado <- try({
        resp <- request(url) |> req_perform()
        writeBin(resp$body, caminho_arquivo)
        message(glue("✔ {uf}-{ano} downloaded."))
        TRUE
      }, silent = TRUE)
      
      if (inherits(resultado, "try-error") || isFALSE(resultado)) {
        falhas <- append(falhas, list(tibble(uf = uf, ano = ano, periodo = periodo)))
        message(glue("⚠ Failed: {uf}-{ano}."))
      }
    }
    rm(resp, uf, url, ano, periodo, resultado)
  }
  
  # Inspect this table to identify and retry any failed downloads
  log_erros <- bind_rows(falhas)
}


### 4.2.2 [RUN-ONCE] Stack Per-UF Files into One Annual CSV ────────────────── ###
# After 4.2.1, each UF × year has its own file. Here we merge all UFs for a
# given year into one consolidated file saved under SIOPE/Anos/.
# Run this section only after 4.2.1 has completed successfully.

RUN_ONCE_SIOPE_ANNUAL <- FALSE   # ← change to TRUE on first run (after 4.2.1)

if (RUN_ONCE_SIOPE_ANNUAL) {
  
  pasta_anos <- file.path(PATH_GIOVANNI, "Dados/SIOPE/Anos")
  if (!dir.exists(pasta_anos)) dir.create(pasta_anos, recursive = TRUE)
  
  for (ano in 2000:2024) {
    
    lista_ufs <- list()
    
    for (uf in ufs) {
      
      periodo      <- f_periodo(ano)
      nome_arquivo <- file.path(pasta_destino, glue("siope_{uf}_{ano}_P{periodo}.csv"))
      
      if (!file.exists(nome_arquivo)) {
        message(glue("❌ File not found: {nome_arquivo}"))
        next
      }
      
      df_uf_ano <- read.csv(nome_arquivo, fileEncoding = "UTF-8") %>%
        mutate(
          ANO      = ano,
          UF       = uf,
          COD_EXIB_FORMATADO = as.numeric(COD_EXIB_FORMATADO),
          COD_MUNI = as.numeric(COD_MUNI)
        ) %>%
        # Standardise strings: collapse whitespace, strip trailing punctuation
        mutate(across(where(is.character), ~ .x %>%
                        str_squish() %>%
                        str_remove("\\.+$") %>%
                        str_remove("\\s+$")))
      
      lista_ufs[[uf]] <- df_uf_ano
    }
    
    dados_ano <- bind_rows(lista_ufs)
    write.csv(dados_ano, file = file.path(pasta_anos, paste0(ano, ".csv")), row.names = FALSE)
    
    message(glue("✅ Year {ano} stacked and saved."))
    rm(lista_ufs, df_uf_ano, dados_ano)
  }
}


### 4.2.3 Import Annual SIOPE Files into a Named List ─────────────────────── ###
# Runs on every execution (not a run-once step).
# Reads the pre-built annual CSVs from SIOPE/Anos/ and applies year-specific
# item filters to retain only the FUNDEB receipt and deduction rows.
# Output: lista_anos — a named list where each element is one year's data.

pasta_anos <- file.path(PATH_GIOVANNI, "Dados/SIOPE/Anos")

lista_anos <- list()

for (ano in 2005:2024) {
  
  caminho_arquivo <- file.path(pasta_anos, paste0(ano, ".csv"))
  
  if (!file.exists(caminho_arquivo)) {
    message(glue::glue("❌ File not found for year {ano}"))
    next
  }
  
  # Select only the relevant columns; keep "Receitas Realizadas" and
  # "Deduções FUNDEB" accounting entries (the two sides of the net calculation)
  df2 <- read.csv(caminho_arquivo, fileEncoding = "UTF-8") %>%
    select(c(ANO, TIPO, UF, COD_UF, COD_MUNI, NOM_MUNI, NOM_ITEM, NOM_COLU, VAL_DECL)) %>%
    filter(NOM_COLU %in% c("Receitas Realizadas", "Deduções FUNDEB")) %>%
    mutate(
      TIPO     = as.character(TIPO),
      COD_UF   = as.numeric(COD_UF),
      # For state-type entries the municipality code is null; replace it with the UF code
      COD_MUNI = if_else(TIPO == "Estadual", COD_UF, COD_MUNI)
    )
  
  # ── Year-specific item name filters ────────────────────────────────────── #
  # Item names changed at each major policy transition (FUNDEF → FUNDEB →
  # Novo FUNDEB). Filters are applied year-by-year to retain the correct rows.
  
  # 2005: still under FUNDEF nomenclature
  if (ano == 2005) {
    df_filtrado <- df2 %>%
      mutate(
        # Harmonise any residual "FUNDEB" strings back to "FUNDEF" for this year
        NOM_ITEM = stringr::str_replace_all(
          NOM_ITEM, regex("FUNDEB", ignore_case = TRUE), "FUNDEF"
        )
      ) %>%
      filter(
        NOM_ITEM == "Transferências de Recursos do FUNDEF"     |
          str_starts(NOM_ITEM, "Transferências da Complementação") |
          NOM_ITEM == "DEDUÇÕES DA RECEITA CORRENTE"
      )
  }
  
  # 2006: transition year — both multigovernmental and FUNDEF-specific rows exist
  if (ano == 2006) {
    df_filtrado <- df2 %>%
      filter(
        NOM_ITEM == "Transferências Multigovernamentais"               |
          NOM_ITEM == "Transferências de Recursos do FUNDEF"           |
          str_starts(NOM_ITEM, "Transferências da Complementação")     |
          NOM_ITEM == "DEDUÇÕES DA RECEITA CORRENTE"
      )
  }
  
  # 2007–2020: FUNDEB nomenclature (federal complementation item kept separate)
  if (ano >= 2007 & ano <= 2020) {
    df_filtrado <- df2 %>%
      filter(
        NOM_ITEM == "DEDUÇÕES DA RECEITA CORRENTE"                                      |
          NOM_ITEM == "Transferências de Recursos do FUNDEB"                             |
          NOM_ITEM == "Transferências de Recursos da Complementação da União ao FUNDEB"  |
          NOM_ITEM == "Transferências Multigovernamentais"
      )
  }
  
  # 2021+: Novo FUNDEB renames items under Law 14.113/2020
  if (ano > 2020) {
    df_filtrado <- df2 %>%
      filter(
        (str_starts(NOM_ITEM, "Transferências de Recursos do Fundo de Manutenção") &
           str_ends(NOM_ITEM, regex("Fundeb$", ignore_case = TRUE)))                    |
          (str_starts(NOM_ITEM, "Transferências de Recursos de Complementação da União") &
             str_ends(NOM_ITEM, regex("Fundeb$", ignore_case = TRUE)))                  |
          (NOM_ITEM == "Receitas Correntes" & NOM_COLU == "Deduções FUNDEB")            |
          NOM_ITEM == "Transferências Multigovernamentais"
      ) %>%
      # Standardise the longer post-2020 item names back to the shorter 2007–2020 form
      # so that downstream code can use a single string match
      mutate(
        NOM_ITEM = case_when(
          str_detect(NOM_ITEM, regex(
            "Transferências de Recursos do Fundo de Manutenção.*Fundeb$", ignore_case = TRUE
          )) ~ "Transferências de Recursos do Fundeb",
          str_detect(NOM_ITEM, regex(
            "Transferências de Recursos de Complementação da União.*Fundeb$", ignore_case = TRUE
          )) ~ "Transferências de Recursos de Complementação da União ao Fundeb",
          TRUE ~ NOM_ITEM
        )
      ) %>%
      filter(NOM_COLU != "Deduções FUNDEB" | NOM_ITEM == "Receitas Correntes")
  }
  
  lista_anos[[as.character(ano)]] <- df_filtrado
  message(glue("✅ Year {ano} imported and filtered."))
  rm(df2, df_filtrado)
}


### 4.2.4 [PRESERVED] Compute Net FUNDEB Transfers ─────────────────────────── ###
# Net (liquid) transfer = principal FUNDEF/FUNDEB receipt − FUNDEB deductions.
# This block is preserved for when SIOPE data quality checks are complete.
# Activate by uncommenting and running after 4.2.3.

# lista_liq <- list()
#
# for (ano in 2005:2024) {
#   df_ano <- lista_anos[[as.character(ano)]] %>%
#     distinct(.keep_all = TRUE) %>%
#     select(-NOM_COLU) %>%
#     pivot_wider(names_from = NOM_ITEM, values_from = VAL_DECL, values_fill = 0) %>%
#     group_by(COD_MUNI, NOM_MUNI, UF, ANO)
#
#   if (ano <= 2006) {
#     df_ano <- df_ano %>%
#       mutate(
#         `Principal do FUNDEF/FUNDEB` = `Transferências de Recursos do FUNDEF`,
#         `Complementação da União`    = `Transferências da Complementação da União ao FUNDEF`,
#         `Deduções`                   = `DEDUÇÕES DA RECEITA CORRENTE`,
#         `Transferência Líquida`      = `Principal do FUNDEF/FUNDEB` - `Deduções`
#       ) %>% ungroup()
#
#   } else if (ano >= 2007 & ano <= 2020) {
#     df_ano <- df_ano %>%
#       mutate(
#         `Principal do FUNDEF/FUNDEB` = `Transferências de Recursos do FUNDEB`,
#         `Complementação da União`    = `Transferências de Recursos da Complementação da União ao FUNDEB`,
#         `Deduções`                   = `DEDUÇÕES DA RECEITA CORRENTE`,
#         `Transferência Líquida`      = `Principal do FUNDEF/FUNDEB` - `Deduções`
#       ) %>% ungroup()
#
#   } else {
#     df_ano <- df_ano %>%
#       mutate(
#         `Principal do FUNDEF/FUNDEB` = `Transferências de Recursos do Fundeb`,
#         `Complementação da União`    = `Transferências de Recursos de Complementação da União ao Fundeb`,
#         `Deduções`                   = `Receitas Correntes`,
#         `Transferência Líquida`      = `Principal do FUNDEF/FUNDEB` - `Deduções`
#       ) %>% ungroup()
#   }
#
#   df_ano <- df_ano %>%
#     select(c(ANO, TIPO, UF, COD_UF, NOM_MUNI, COD_MUNI,
#              `Principal do FUNDEF/FUNDEB`, `Complementação da União`,
#              `Deduções`, `Transferência Líquida`))
#   lista_liq[[as.character(ano)]] <- df_ano
#   rm(df_ano)
#   message(glue("✅ Year {ano} liquid transfer computed."))
# }
#
# transf <- bind_rows(lista_liq) %>% clean_names()


### 4.2.5 [PRESERVED] Deflate Net Transfers ────────────────────────────────── ###

# ipca <- read_excel(file.path(PATH_GIOVANNI, "Dados/IPCA_acumulado_ano.xlsx"), skip = 1)
# colnames(ipca) <- c("ano", "ipca")
# ipca <- ipca %>%
#   mutate(ano  = as.numeric(ano),
#          ipca = as.numeric(str_replace(ipca, ",", "."))) %>%
#   filter(ano %in% 2000:2024) %>%
#   arrange(ano) %>%
#   mutate(indice = cumprod(1 + ipca / 100)) %>%
#   mutate(indice = indice / indice[ano == 2021])
#
# transf <- transf %>%
#   left_join(ipca, by = "ano") %>%
#   mutate(across(
#     c(principal_do_fundef_fundeb, complementacao_da_uniao, deducoes, transferencia_liquida),
#     ~ . / indice
#   )) %>%
#   select(-c(indice, ipca))
#
# rm(ipca)


### 4.2.6 [PRESERVED] Join SIOPE Liquid Transfers with df_nota ──────────────── ###
# Once transf is ready, merge with the transfer + score panel and re-export.
# Uncomment and run after 4.2.5 passes all quality checks.

# colnames(transf) <- c("ano","tipo","uf","cod_uf","nome","codigo_ibge_n",
#                        "principal","complementacao","deducoes","transf_liquida")
#
# df_nota <- df_nota %>%
#   mutate(codigo_ibge_n = as.numeric(str_sub(as.character(codigo_ibge), 1, 6)))
#
# df <- left_join(df_nota, transf, by = c("codigo_ibge_n", "ano", "uf")) %>%
#   select(-nome.y) %>%
#   rename(nome = "nome.x")
#
# rm(df_nota, lista_liq, lista_anos, transf)
#
# write.csv2(df,
#   file.path(PATH_GIOVANNI, "Dados/painel_notas_transferencias_2000_2024.csv"))


# ============================================================================ #
# 5. SIMULATION — FUNDEF Counterfactual ----
# ============================================================================ #
# The core identification strategy asks: how much would each municipality have
# received in 2007 if FUNDEF distribution rules had continued, rather than the
# expanded FUNDEB rules that actually took effect?
#
# FUNDEF (2000–2006) counted only primary EF enrolments with two weights:
#   FD1 = 1.00 for grades 1–4
#   FD2 = 1.05 for grades 5–8 + special education
#
# FUNDEB (2007–2021) added early childhood (Infantil), upper secondary (EM),
# and EJA, with finer rural/urban/full-time distinctions and an indigenous weight.
#
# Municipalities with a high pre-existing share of Infantil or EM enrolments
# gained more from FUNDEB than their FUNDEF counterfactual, giving us exogenous
# variation in the change in per-student funding at the FUNDEF→FUNDEB transition.
#
# Steps:
#   5.1  Read 2006 Censo Escolar microdata → save school-level file
#   5.2  Aggregate school file to municipality level
#   5.3  Load FNDE's official 2007 FUNDEB pondering sheets
#   5.4  Build a single data frame combining FNDE weights with Censo enrolments
#   5.5  Simulate the FUNDEF distribution coefficient per municipality
#   5.6  Compute VAA (Valor Aluno-Ano) under actual FUNDEB and counterfactual


## 5.1 Censo Escolar 2006 — School-Level Microdata ─────────────────────────── ##
# The 2006 Censo is the last one before FUNDEB took effect.
# Its enrolment counts reflect the baseline FUNDEF-era composition.
# The raw pipe-delimited file is ~400 MB; we filter and save an RDS after the
# first load so subsequent runs skip the expensive read_delim call.

censo <- read_delim(
  file.path(
    PATH_IFB,
    "Censo Escolar/Bases Agregadas/2006/microdados_educação_básica_2006/DADOS/CENSOESC_2006.CSV"
  ),
  delim = "|",
  col_types = cols(
    .default   = col_double(),
    CODFUNC    = col_character(), MASCARA    = col_character(),
    DEP        = col_character(), LOC        = col_character(),
    UF         = col_character(), SIGLA      = col_character(),
    MUNIC      = col_character(), ED_INDIG   = col_character(),
    MAT_QUIL   = col_character(), MAT_ETNI   = col_character(),
    NIVELCRE   = col_character(), NIVELPRE   = col_character(),
    NIV_F1A4_8 = col_character(), NIV_F5A8_8 = col_character(),
    NIV_F9INI  = col_character(), NIV_F9FIM  = col_character(),
    NIVELMED   = col_character(), NIVM_INT   = col_character(),
    SUPL_AVA   = col_character(), SUPL_SAVA  = col_character(),
    EDPROFIS   = col_character(), ESP_EXCL   = col_character(),
    ESP_T_ES   = col_character(), ENS_INCL   = col_character(),
    ESC_ASSE   = col_character(), AREA_QUIL  = col_character(),
    ESP_S_RE   = col_character(), ESP_A_IN   = col_character(),
    ED_IN_LM   = col_character(), COD_ID_IND = col_character(),
    ED_IN_LP   = col_character(), ESC_T_IN   = col_character(),
    PRED_ESC   = col_character(), TEMPLO     = col_character(),
    DEF11C = col_double(), DEF11D = col_double(),
    DEF11E = col_double(), DEF11F = col_double(),
    NEF11C = col_double(), NEF11D = col_double(),
    NEF11E = col_double(), NEF11F = col_double(),
    DEF11G = col_double(), DEF11H = col_double(),
    DEF11I = col_double(), DEF11J = col_double(),
    NEF11G = col_double(), NEF11H = col_double(),
    NEF11I = col_double(), NEF11J = col_double()
  )
)


### 5.1.1 Aggregate Enrolments by School ─────────────────────────────────── ###
# Select and sum enrolment variables for each schooling modality.
# The 2006 census uses two grade-scale conventions simultaneously:
# the old 8-year and new 9-year EF tracks — we handle both.

mat_2006 <- censo %>%
  select(c(
    1:9,
    CODFUNC, ED_INDIG, MAT_QUIL, AREA_QUIL, ESC_ASSE,
    ED_IN_LM, ED_IN_LP, MAT_ETNI, ESC_T_IN,
    DEF11C:DEF11F, NEF11C:NEF11F,          # EF iniciais, 8-year track
    DE9F11C:DE9F11G, NE9F11C:NE9F11G,     # EF iniciais, 9-year track
    DEF11G:DEF11J, NEF11G:NEF11J,          # EF finais, 8-year track
    DE9F11H:DE9F11N, NE9F11H:NE9F11N,     # EF finais, 9-year track
    VEE1431:VEE1437,                        # Special-ed in EF by birth year
    VEE1619:VEE1691, VEE1719:VEE1791,      # Special-ed by grade series (1st yr EF)
    VEE1819:VEE1891, VEE1919:VEE1991,
    VEE1612:VEE1692, VEE1712:VEE1792,      # 2nd yr EF
    VEE1812:VEE1892, VEE1912:VEE1992,
    VEE1613:VEE1693, VEE1713:VEE1793,      # 3rd yr EF
    VEE1813:VEE1893, VEE1913:VEE1993,
    VEE1614:VEE1694, VEE1714:VEE1794,      # 4th yr EF
    VEE1814:VEE1894, VEE1914:VEE1994,
    VEE1615:VEE1695, VEE1715:VEE1795,      # 5th yr EF
    VEE1815:VEE1895, VEE1915:VEE1995,
    VEE1616:VEE1696, VEE1716:VEE1796,      # 6th yr EF
    VEE1816:VEE1896, VEE1916:VEE1996,
    VEE1617:VEE1697, VEE1717:VEE1797,      # 7th yr EF
    VEE1817:VEE1897, VEE1917:VEE1997,
    VEE1618:VEE1698, VEE1718:VEE1798,      # 8th yr EF
    VEE1818:VEE1898, VEE1918:VEE1998,
    DEM118, DEM119, DEM11A, DEM11B, DEM11C, # Upper secondary (EM)
    NEM118, NEM119, NEM11A, NEM11B, NEM11C,
    DPE119, NPE119,                          # Creche (day-care)
    DPE11D, NPE11D,                          # Pré-escola
    DES101F:DES101A, NES101F:NES101A         # EJA (adult education)
  )) %>%
  mutate(COD_UF = as.numeric(str_sub(as.character(CODMUNIC), 1, 2))) %>%
  mutate(
    # Sum both 8-year and 9-year EF tracks for iniciais and finais
    reg_in       = rowSums(across(c(DEF11C:NE9F11G)), na.rm = TRUE),
    reg_fin      = rowSums(across(c(DEF11G:NE9F11N)), na.rm = TRUE),
    # Special-education counts — two counting methods; take the maximum
    esp_iniciais    = rowSums(across(c(VEE1619:VEE1994)), na.rm = TRUE),
    esp_finais      = rowSums(across(c(VEE1615:VEE1998)), na.rm = TRUE),
    esp_soma        = esp_iniciais + esp_finais,
    esp_tot         = rowSums(across(c(VEE1431:VEE1437)), na.rm = TRUE),
    em_tot          = rowSums(across(c(DEM118:NEM11C)), na.rm = TRUE),
    creche          = rowSums(across(c(DPE119, NPE119)), na.rm = TRUE),
    pre_escola      = rowSums(across(c(DPE11D, NPE11D)), na.rm = TRUE),
    ed_inf_tot      = rowSums(across(c(DPE119:NPE11D)), na.rm = TRUE),
    eja_tot         = rowSums(across(c(DES101F:NES101A)), na.rm = TRUE),
    dif             = esp_iniciais + esp_finais - esp_tot,
    # Use the larger of the two special-ed counts as a conservative upper bound
    esp_total_final = pmax(esp_soma, esp_tot)
  ) %>%
  # Keep only active municipal and state schools (exclude federal and private)
  filter(
    !(DEP %in% c("Particular", "Federal")),
    CODFUNC == "Ativo"
  ) %>%
  # Rebuild CODMUNIC as [2-digit UF] + [last 5 digits] to match the 6-digit
  # convention used in the transfer data
  mutate(
    CODMUNIC = as.numeric(str_c(
      str_sub(as.character(CODMUNIC), 1, 2),
      str_sub(as.character(CODMUNIC), -5)
    ))
  ) %>%
  # Flag schools that serve indigenous or quilombola communities.
  # These receive a higher FUNDEB weight (FD_ind = 1.20) but would have been
  # counted as ordinary EF enrolments under FUNDEF.
  mutate(
    ind_quil = ifelse(
      (coalesce(ED_INDIG, "n")  == "s" |
         coalesce(MAT_QUIL, "n")  == "s" |
         coalesce(AREA_QUIL, "n") == "s" |
         coalesce(ED_IN_LM, "n")  == "s" |
         coalesce(ED_IN_LP, "n")  == "s"),
      1, 0
    )
  )

# Save the filtered school-level file; re-read immediately below to avoid
# re-running the expensive read_delim on every subsequent run of this script.
saveRDS(mat_2006, file.path(PATH_TUFFY, "Dados/censo_2006_filtrado.rds"))
mat_2006 <- readRDS(file.path(PATH_TUFFY, "Dados/censo_2006_filtrado.rds"))


### 5.1.2 Aggregate to Municipality Level ─────────────────────────────────── ###
# Sum all school-level counts to the municipality × administrative network
# (DEP) × indigenous flag level. The DEP distinction is needed because state
# and municipal networks have separate distribution coefficients.

mat_2006_munic <- mat_2006 %>%
  group_by(CODMUNIC, DEP, ind_quil) %>%
  summarise(
    nome        = first(MUNIC),
    no_uf       = first(UF),
    uf          = first(SIGLA),
    mat_reg_in  = sum(reg_in,         na.rm = TRUE),
    mat_reg_fin = sum(reg_fin,         na.rm = TRUE),
    mat_tot_esp = sum(esp_total_final, na.rm = TRUE),
    mat_tot_em  = sum(em_tot,          na.rm = TRUE),
    mat_tot_inf = sum(ed_inf_tot,      na.rm = TRUE),
    mat_tot_eja = sum(eja_tot,         na.rm = TRUE),
    .groups     = "drop"
  )

colnames(mat_2006_munic)
glimpse(mat_2006_munic %>%
          select(CODMUNIC, mat_reg_in, mat_reg_fin, mat_tot_esp,
                 mat_tot_em, mat_tot_inf, mat_tot_eja))

saveRDS(mat_2006_munic, file.path(PATH_TUFFY, "Dados/censo_2006_filtrado_mun.rds"))


## 5.2 FUNDEF Distribution Coefficient — Setup ──────────────────────────────── ##
# Reload the municipality-level aggregates from the saved RDS and attach
# descriptive labels for documentation purposes.

mat_2006 <- readRDS(file.path(PATH_TUFFY, "Dados/censo_2006_filtrado_mun.rds")) %>%
  add_label("mat_reg_in",  "Mat. Fund. Iniciais regular (CENSO)") %>%
  add_label("mat_reg_fin", "Mat. Fund. Finais regular (CENSO)")   %>%
  add_label("mat_tot_em",  "Mat. EM (CENSO)")                     %>%
  add_label("mat_tot_inf", "Mat. Infantil (CENSO)")               %>%
  add_label("mat_tot_eja", "Mat. EJA (CENSO)")                    %>%
  add_label("mat_tot_esp", "Mat. Especiais (CENSO)")


### 5.2.1 State-Level Special-Education Totals ─────────────────────────────── ###
# TA_esp = total special-education enrolments per state (UF).
# Under FUNDEF rules, special-ed was pooled at the state level in the denominator.

estaduais <- mat_2006 %>%
  group_by(uf) %>%
  summarise(TA_esp = sum(mat_tot_esp, na.rm = TRUE), .groups = "drop") %>%
  add_label("TA_esp", "Total de Mat. Especiais na UF")

mat_cd <- mat_2006 %>% left_join(estaduais, by = "uf")


### 5.2.2 Aggregate State Network Rows ─────────────────────────────────────── ###
# State (Estadual) schools have one coefficient per UF, not per municipality.
# We aggregate them to UF level and assign each a numeric codigo_ibge matching
# the state-level FUNDEF/FUNDEB transfer records.

mat_cd <- mat_cd %>%
  group_by(DEP, uf, nome = ifelse(DEP == "Estadual", "GOVERNO ESTADUAL", nome)) %>%
  summarise(
    codigo_ibge      = first(CODMUNIC),
    mat_reg_iniciais = sum(mat_reg_in,  na.rm = TRUE),
    mat_reg_finais   = sum(mat_reg_fin, na.rm = TRUE),
    mat_tot_esp      = sum(mat_tot_esp, na.rm = TRUE),
    TA_esp           = first(TA_esp),
    .groups          = "drop"
  ) %>%
  mutate(
    # For state networks, replace the municipality code with a 2-digit UF code
    # using the standard IBGE UF numbering system
    codigo_ibge = ifelse(
      nome != "GOVERNO ESTADUAL", codigo_ibge,
      case_when(
        uf == "AC" ~ 12, uf == "AL" ~ 27, uf == "AP" ~ 16, uf == "AM" ~ 13,
        uf == "BA" ~ 29, uf == "CE" ~ 23, uf == "DF" ~ 53, uf == "ES" ~ 32,
        uf == "GO" ~ 52, uf == "MA" ~ 21, uf == "MT" ~ 51, uf == "MS" ~ 50,
        uf == "MG" ~ 31, uf == "PA" ~ 15, uf == "PB" ~ 25, uf == "PR" ~ 41,
        uf == "PE" ~ 26, uf == "PI" ~ 22, uf == "RJ" ~ 33, uf == "RN" ~ 24,
        uf == "RS" ~ 43, uf == "RO" ~ 11, uf == "RR" ~ 14, uf == "SC" ~ 42,
        uf == "SP" ~ 35, uf == "SE" ~ 28, uf == "TO" ~ 17, TRUE ~ NA_real_
      )
    )
  ) %>%
  add_label("mat_reg_iniciais", "Mat. Fund. Iniciais regular (CENSO)") %>%
  add_label("mat_reg_finais",   "Mat. Fund. Finais regular (CENSO)")   %>%
  add_label("mat_tot_esp",      "Mat. Especiais total (CENSO)")        %>%
  add_label("TA_esp",           "Total Mat. Especiais do UF (CENSO)")


## 5.3 FNDE Official 2007 FUNDEB Pondering Sheets ─────────────────────────── ##
# The FNDE published one Excel file per UF with the official enrolment counts
# and distribution coefficients used in the first year of FUNDEB (2007).
# These are our reference for the actual FUNDEB allocation and for scaling
# the counterfactual.

lista_ufs <- list()
pasta     <- file.path(PATH_GIOVANNI, "Dados/ponderacao_matriculas_2007/excel/")
arquivos  <- list.files(path = pasta, pattern = "\\.xlsx$", full.names = TRUE)

for (file in arquivos) {
  
  df_uf <- read_excel(file, col_names = FALSE)
  
  # Impose a consistent column schema across all 27 UF sheets
  colnames(df_uf) <- c(
    "nome", "mat_creche", "mat_pre", "mat_fund_in_urb", "mat_fund_in_rur",
    "mat_fund_fin_urb", "mat_fund_fin_rur", "mat_fund_integ", "mat_em_urb",
    "mat_em_rur", "mat_em_prof", "mat_esp_frac", "mat_eja", "mat_ind",
    "coef_fundeb", "est_receitas_fundeb"
  )
  
  df_uf <- df_uf %>%
    filter(!is.na(mat_creche)) %>%    # remove empty rows at file boundaries
    distinct() %>%
    mutate(nome = str_squish(str_replace_all(nome, "[\r\n]", " "))) %>%
    # Convert numbers stored as strings with commas as decimal separators
    mutate(across(
      .cols = 2:ncol(.),
      .fns  = ~ as.numeric(gsub(",", ".", gsub("\\.", "", as.character(.))))
    )) %>%
    filter(complete.cases(.))
  
  lista_ufs[[str_sub(basename(file), 1, 2)]] <- df_uf
  rm(df_uf)
}

# Fix two known data-entry errors in state files
al      <- lista_ufs[["al"]] %>% mutate(nome = str_replace(nome, "TOLTAL BRASIL",          "TOTAL BRASIL"))
lista_ufs[["al"]] <- al; rm(al)

dis_fed <- lista_ufs[["df"]] %>% mutate(nome = str_replace(nome, "TOTAL -Distrito Federal", "DISTRITO FEDERAL"))
lista_ufs[["df"]] <- dis_fed; rm(dis_fed)


## 5.4 Single FNDE Data Frame ─────────────────────────────────────────────── ##
# Stack all 27 UF sheets, drop subtotal rows, and label each column.

df_fnde <- data.frame()

for (uf in names(lista_ufs)) {
  uf_df <- lista_ufs[[uf]] %>%
    filter(!grepl("total", nome, ignore.case = TRUE)) %>%
    mutate(uf = as.character(toupper(uf)))
  df_fnde <- bind_rows(df_fnde, uf_df)
  rm(uf_df, uf)
}

df_fnde <- df_fnde %>%
  select(c(nome, uf, coef_fundeb, est_receitas_fundeb, everything())) %>%
  rename(coef_est_fnde    = "coef_fundeb",
         receita_est_fnde = "est_receitas_fundeb") %>%
  add_label("coef_est_fnde",    "Coef. de Dist. p/ o FUNDEB (2007)") %>%
  add_label("receita_est_fnde", "Receita estimada FUNDEB (2007)")    %>%
  add_label("mat_creche",       "Mat. Creche (FNDE)")                %>%
  add_label("mat_pre",          "Mat. Pré-Escola (FNDE)")            %>%
  add_label("mat_fund_in_urb",  "Mat. Iniciais Urbana (FNDE)")       %>%
  add_label("mat_fund_in_rur",  "Mat. Iniciais Rural (FNDE)")        %>%
  add_label("mat_fund_fin_urb", "Mat. Finais Urbana (FNDE)")         %>%
  add_label("mat_fund_fin_rur", "Mat. Finais Rural (FNDE)")          %>%
  add_label("mat_fund_integ",   "Mat. Fund. Integral Total (FNDE)")  %>%
  add_label("mat_em_urb",       "Mat. EM Urbana (FNDE)")             %>%
  add_label("mat_em_rur",       "Mat. EM Rural (FNDE)")              %>%
  add_label("mat_em_prof",      "Mat. EM Profissionalizante (FNDE)") %>%
  add_label("mat_esp_frac",     "Mat. Especiais FUNDEB (FNDE)")      %>%
  add_label("mat_eja",          "Mat. EJA (FNDE)")                   %>%
  add_label("mat_ind",          "Mat. Indígena FUNDEB (FNDE)")

colnames(df_fnde)

# Quick check: coefficients should sum to ~1.0 within each UF
test <- df_fnde %>%
  group_by(uf) %>%
  summarise(check_coef_fb = sum(coef_est_fnde)) %>%
  ungroup()
rm(test)


### 5.4.1 Proportion of Iniciais vs. Finais from the Censo ─────────────────── ###
# Some municipalities report ONLY full-time (integral) EF in the FNDE sheets,
# making it impossible to split that enrolment using FNDE data alone.
# We compute the iniciais/finais split from the 2006 Censo and use it as an
# imputation denominator when allocating integral enrolments.

mat_cd <- mat_cd %>%
  mutate(
    prop_1_4_mun = ifelse(
      is.finite(mat_reg_iniciais / (mat_reg_iniciais + mat_reg_finais)),
      mat_reg_iniciais / (mat_reg_iniciais + mat_reg_finais), 0
    ),
    prop_5_8_mun = ifelse(
      is.finite(mat_reg_finais / (mat_reg_iniciais + mat_reg_finais)),
      mat_reg_finais   / (mat_reg_iniciais + mat_reg_finais), 0
    )
  ) %>%
  add_label("prop_1_4_mun", "Prop. de Mat. Iniciais (CENSO)") %>%
  add_label("prop_5_8_mun", "Prop. de Mat. Finais (CENSO)")


### 5.4.2 Special-Education and Indigenous Enrolments from the Censo ─────────── ###

# ── 5.4.2.1 Join special-ed counts and Censo proportions onto FNDE data ──── #
# Create a merge key from municipality name + UF abbreviation
df_fnde <- df_fnde %>% mutate(chave = paste(nome, uf, sep = "_"))
mat_cd  <- mat_cd  %>% mutate(chave = paste(nome, uf, sep = "_"))

# Diagnostic: print any unmatched rows between the two frames before merging
faltando_em_mat_cd  <- setdiff(df_fnde$chave, mat_cd$chave)
print(faltando_em_mat_cd)

faltando_em_df_fnde <- setdiff(mat_cd$chave, df_fnde$chave)
print(faltando_em_df_fnde)

rm(faltando_em_df_fnde, faltando_em_mat_cd)

df_fnde <- df_fnde %>%
  left_join(
    mat_cd %>% select(chave, codigo_ibge, mat_tot_esp, TA_esp, prop_1_4_mun, prop_5_8_mun),
    by = "chave"
  )


# ── 5.4.2.2 Indigenous and Quilombola Enrolments ──────────────────────────── #
# Filter to schools flagged as indigenous/quilombola and aggregate their
# enrolments by modality. Under FUNDEF these students would have been counted
# as regular EF, but FUNDEB assigns them a separate weight (FD_ind = 1.20).

mat_ind <- mat_2006 %>%
  filter(ind_quil == 1) %>%
  select(c(CODMUNIC:ncol(.))) %>%
  group_by(DEP, uf, nome = ifelse(DEP == "Estadual", "GOVERNO ESTADUAL", nome)) %>%
  summarise(
    codigo_ibge     = first(CODMUNIC),
    mat_reg_in_ind  = sum(mat_reg_in,  na.rm = TRUE),
    mat_reg_fin_ind = sum(mat_reg_fin, na.rm = TRUE),
    mat_esp_ind     = sum(mat_tot_esp, na.rm = TRUE),
    mat_em_ind      = sum(mat_tot_em,  na.rm = TRUE),
    mat_inf_ind     = sum(mat_tot_inf, na.rm = TRUE),
    mat_eja_ind     = sum(mat_tot_eja, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  mutate(chave = paste(nome, uf, sep = "_")) %>%
  mutate(
    codigo_ibge = ifelse(
      nome != "GOVERNO ESTADUAL", codigo_ibge,
      case_when(
        uf == "AC" ~ 12, uf == "AL" ~ 27, uf == "AP" ~ 16, uf == "AM" ~ 13,
        uf == "BA" ~ 29, uf == "CE" ~ 23, uf == "DF" ~ 53, uf == "ES" ~ 32,
        uf == "GO" ~ 52, uf == "MA" ~ 21, uf == "MT" ~ 51, uf == "MS" ~ 50,
        uf == "MG" ~ 31, uf == "PA" ~ 15, uf == "PB" ~ 25, uf == "PR" ~ 41,
        uf == "PE" ~ 26, uf == "PI" ~ 22, uf == "RJ" ~ 33, uf == "RN" ~ 24,
        uf == "RS" ~ 43, uf == "RO" ~ 11, uf == "RR" ~ 14, uf == "SC" ~ 42,
        uf == "SP" ~ 35, uf == "SE" ~ 28, uf == "TO" ~ 17, TRUE ~ NA_real_
      )
    )
  )

# Merge indigenous enrolments back into the main FNDE data frame,
# positioning each modality column adjacent to its non-indigenous counterpart.
# NA values are replaced with zero: municipalities with no indigenous schools
# contribute zero to the indigenous modality totals.
df_fnde <- left_join(
  df_fnde,
  mat_ind %>% select(c(chave, codigo_ibge, mat_reg_in_ind:mat_eja_ind)),
  by = c("codigo_ibge", "chave")
) %>%
  relocate(mat_reg_in_ind,  .after = mat_fund_in_rur) %>%
  relocate(mat_reg_fin_ind, .after = mat_fund_fin_rur) %>%
  relocate(mat_em_ind,      .after = mat_em_rur) %>%
  relocate(mat_inf_ind,     .after = mat_pre) %>%
  relocate(mat_eja_ind,     .after = mat_eja) %>%
  mutate(
    mat_reg_in_ind  = coalesce(mat_reg_in_ind,  0),
    mat_reg_fin_ind = coalesce(mat_reg_fin_ind, 0),
    mat_em_ind      = coalesce(mat_em_ind,      0),
    mat_inf_ind     = coalesce(mat_inf_ind,     0),
    mat_eja_ind     = coalesce(mat_eja_ind,     0)
  ) %>%
  add_label("mat_reg_in_ind",  "Mat. Indígena Iniciais (CENSO)")         %>%
  add_label("mat_reg_fin_ind", "Mat. Indígena Finais (CENSO)")           %>%
  add_label("mat_esp_ind",     "Mat. Indígena Especiais Total (CENSO)")  %>%
  add_label("mat_em_ind",      "Mat. Indígena EM (CENSO)")               %>%
  add_label("mat_inf_ind",     "Mat. Indígena Infantil (CENSO)")         %>%
  add_label("mat_eja_ind",     "Mat. Indígena EJA (CENSO)")

# Second coefficient-sum check after the indigenous join
test <- df_fnde %>%
  group_by(uf) %>%
  summarise(check_coef_fb = sum(coef_est_fnde)) %>%
  ungroup()
rm(test)


## 5.5 De Facto FUNDEF Simulation ─────────────────────────────────────────── ##
# Apply the FUNDEF distribution formula to 2006 enrolment data to obtain
# a simulated coefficient for each municipality (coef_simulado).
#
# FUNDEF formula:
#   coef_i = [FD1*(NA1/4_i + integral_i * prop_1/4) + FD2*(NA5/8_i + integral_i * prop_5/8 + NAe_i)]
#            / [FD1 * TA_1/4 + FD2 * (TA_5/8 + TA_e)]
#
# TA_* = state-level totals; NA_* = municipality-level counts.

# Step 1: Compute state-level totals (TA_*) for the FUNDEF denominator.
# Integral enrolments are split into iniciais/finais using the state proportions.
totais_estado2 <- df_fnde %>%
  group_by(uf) %>%
  summarise(
    mat_1_4  = sum(mat_fund_in_urb, na.rm = TRUE) + sum(mat_fund_in_rur, na.rm = TRUE)
    + sum(mat_reg_in_ind, na.rm = TRUE),
    mat_5_8  = sum(mat_fund_fin_urb, na.rm = TRUE) + sum(mat_fund_fin_rur, na.rm = TRUE)
    + sum(mat_reg_fin_ind, na.rm = TRUE),
    prop_1_4 = mat_1_4 / (mat_1_4 + mat_5_8),
    prop_5_8 = mat_5_8 / (mat_1_4 + mat_5_8),
    TA_1_4   = sum(mat_fund_in_urb, na.rm = TRUE) + sum(mat_fund_in_rur, na.rm = TRUE)
    + (sum(mat_fund_integ, na.rm = TRUE) * prop_1_4) + sum(mat_reg_in_ind, na.rm = TRUE),
    TA_5_8   = sum(mat_fund_fin_urb, na.rm = TRUE) + sum(mat_fund_fin_rur, na.rm = TRUE)
    + (sum(mat_fund_integ, na.rm = TRUE) * prop_5_8) + sum(mat_reg_fin_ind, na.rm = TRUE),
    .groups  = "drop"
  )

# FUNDEF weighting factors (only two, unlike the thirteen FUNDEB factors below)
FD1 <- 1.00   # grades 1–4
FD2 <- 1.05   # grades 5–8 + special education

# Step 2: Compute the simulated FUNDEF coefficient per municipality × network
simulacao <- df_fnde %>%
  left_join(totais_estado2, by = "uf") %>%
  mutate(
    mat_1_4_aux   = mat_fund_in_urb + mat_fund_in_rur + mat_reg_in_ind,
    mat_5_8_aux   = mat_fund_fin_urb + mat_fund_fin_rur + mat_reg_fin_ind,
    numerador     = FD1 * (mat_1_4_aux + mat_fund_integ * prop_1_4) +
      FD2 * (mat_5_8_aux + mat_fund_integ * prop_5_8 + mat_tot_esp),
    denominador   = FD1 * TA_1_4 + FD2 * (TA_5_8 + TA_esp),
    # The share of the state fund this municipality would receive under FUNDEF
    coef_simulado = numerador / denominador
  )

# Diagnostic: coefficients should sum to ~1.0 per UF
test <- simulacao %>%
  group_by(uf) %>%
  summarise(check = sum(coef_simulado, na.rm = TRUE))
rm(test)


# Step 3: Apply simulated coefficient to state fund totals.
# Read the transfer + score panel to get actual 2007 state fund aggregates.
df <- read.csv2(
  file.path(PATH_GIOVANNI, "Dados/painel_notas_transferencias_2000_2024.csv")
)

df_fundo <- df %>%
  select(codigo_ibge, ano, uf, nome, total_politica, total_politica_d) %>%
  distinct() %>%
  group_by(ano, uf) %>%
  summarise(
    total_fundo_real   = sum(total_politica,   na.rm = TRUE),
    total_fundo_d_real = sum(total_politica_d, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(ano == 2007) %>%
  add_label("total_fundo_real", "Valor efetivo do Fundo da UF (2007), considerando compl.")

# Load the pre-computed expected FUNDEF fund value for 2007
# (what the fund would have been if FUNDEF rules had continued).
# NOTE: expected_2007_fundef_value.RDS must be built separately.
df_simulation_2007 <- readRDS(
  file.path(PATH_TUFFY, "Dados/expected_2007_fundef_value.RDS")
)

df_fundo <- df_fundo %>%
  left_join(
    df_simulation_2007 %>% select(sig_uf, expected_fundef_2007_real),
    by = c("uf" = "sig_uf")
  )


# Step 4: Compute simulated receipts and the treatment measures.
simulacao <- simulacao %>%
  left_join(df_fundo, by = "uf") %>%
  relocate(chave, .after = "uf") %>%
  mutate(
    # Sensitivity check: simulate using actual 2007 FUNDEB fund total
    receita_simulada_old = coef_simulado * total_fundo_real,
    # PRIMARY SPECIFICATION: simulate using the expected FUNDEF fund for 2007
    receita_simulada     = coef_simulado * expected_fundef_2007_real,
    # Treatment measures: p.p. and % difference in distribution coefficient
    dif_coef_pp  = 100 * (coef_est_fnde - coef_simulado),
    dif_per_coef = 100 * ((coef_est_fnde - coef_simulado) / coef_simulado),
    # Total 2006 enrolment across all modalities
    tot_matri    = (mat_creche + mat_pre + mat_inf_ind +
                      mat_fund_in_urb + mat_fund_in_rur + mat_reg_in_ind +
                      mat_fund_fin_urb + mat_fund_fin_rur + mat_reg_fin_ind +
                      mat_fund_integ + mat_em_urb + mat_em_rur + mat_em_ind +
                      mat_em_prof + mat_eja + mat_eja_ind + mat_tot_esp),
    # Share in modalities not covered by FUNDEF (Infantil + EM):
    # municipalities with a high share gained more from FUNDEB → larger dosage
    shr_inf_em = 100 * ((mat_creche + mat_pre + mat_inf_ind +
                           mat_em_urb + mat_em_rur + mat_em_ind + mat_em_prof) / tot_matri),
    shr_inf    = 100 * ((mat_creche + mat_pre + mat_inf_ind) / tot_matri)
  ) %>%
  select(c(nome, uf, codigo_ibge, chave, total_fundo_real, coef_est_fnde, coef_simulado,
           receita_est_fnde, receita_simulada, expected_fundef_2007_real, everything())) %>%
  add_label("dif_coef_pp",          "Dif. (p.p.) do Coef. com o FUNDEB vs FUNDEF (2007)")         %>%
  add_label("receita_simulada",      "Expected FUNDEF value with simulated coeficient 2007 (main)") %>%
  add_label("dif_per_coef",          "Dif. (%) do Coef. com o FUNDEB vs FUNDEF (2007)")            %>%
  add_label("shr_inf_em",            "Share de estudantes Infantil + EM (2006)")                    %>%
  add_label("receita_simulada_old",  "Receita simulada caso a regra do FUNDEF se mantivesse (considerando compl.)") %>%
  add_label("coef_simulado",         "Coef. de Distrib. caso a regra do FUNDEF se mantivesse (considerando compl.)") %>%
  add_label("shr_inf",               "Share de estudantes Ensino Infantil (2006)")                  %>%
  add_label("tot_matri",             "Total de matriculados (2006)")

# Attach the actual FUNDEB receipt for 2007 as a comparison column
df_fundeb_munic <- df %>%
  filter(ano == 2007) %>%
  select(total_politica, codigo_ibge) %>%
  distinct()

simulacao <- simulacao %>%
  left_join(df_fundeb_munic, by = "codigo_ibge") %>%
  rename(receita_real = "total_politica") %>%
  select(c(codigo_ibge, nome, uf, coef_est_fnde, coef_simulado, dif_per_coef, dif_coef_pp,
           receita_est_fnde, receita_simulada, expected_fundef_2007_real, receita_real,
           shr_inf_em, shr_inf, everything())) %>%
  add_label("receita_real", "Receita recebida pelo FUNDEB (2007)")


## 5.6 VAA Simulation (FUNDEB 2007 Weights) ───────────────────────────────── ##
# Using the official FUNDEB 2007 weight factors (Art. 36, Law 11.494/2007),
# compute the Valor Aluno-Ano (VAA) — the per-weighted-student fund value —
# for both the actual FUNDEB allocation and the FUNDEF counterfactual.
# d_vaa = % difference between the two, which is an alternative treatment measure.

# Official FUNDEB 2007 weight factors by modality and location:
FDC    <- 0.8   # Creche
FDI    <- 0.9   # Pré-escola
FD1_u  <- 1.0   # EF inicial, urban
FD1_r  <- 1.05  # EF inicial, rural
FD2_u  <- 1.1   # EF final, urban
FD2_r  <- 1.15  # EF final, rural
FD1_int <- 1.25 # EF full-time (integral)
FD3_u  <- 1.2   # EM, urban
FD3_r  <- 1.25  # EM, rural
FD3_p  <- 1.3   # EM, vocational (profissionalizante)
FD_esp <- 1.2   # Special education (non-indigenous)
FD_ind <- 1.2   # Indigenous / quilombola schools
FD_eja <- 0.7   # EJA (adult education)
# Note: EM full-time (integral) weight not applied here as that
# disaggregation is unavailable in the 2006 Censo.

simulacao <- simulacao %>%
  # Compute weighted enrolments (matriculas ponderadas) for each modality
  mutate(
    mp_creche   = FDC    * mat_creche,
    mp_pre      = FDI    * mat_pre,
    mp_in_u     = FD1_u  * mat_fund_in_urb,
    mp_in_r     = FD1_r  * mat_fund_in_rur,
    mp_fin_u    = FD2_u  * mat_fund_fin_urb,
    mp_fin_r    = FD2_r  * mat_fund_fin_rur,
    mp_fund_int = FD1_int * mat_fund_integ,
    mp_em_u     = FD3_u  * mat_em_urb,
    mp_em_r     = FD3_r  * (mat_em_rur - mat_em_ind),   # indigenous EM counted separately
    mp_em_prof  = FD3_p  * mat_em_prof,
    mp_esp      = FD_esp * (mat_tot_esp - mat_esp_ind), # indigenous special-ed counted separately
    # All indigenous modalities aggregated under a single weight
    mp_ind      = FD_ind * (mat_inf_ind + mat_reg_in_ind + mat_reg_fin_ind +
                              mat_em_ind + mat_esp_ind + mat_eja_ind),
    mp_eja      = FD_eja * (mat_eja - mat_eja_ind)
  ) %>%
  # Total weighted enrolment = denominator for VAA
  mutate(mp = rowSums(across(c(mp_creche:mp_eja)), na.rm = TRUE)) %>%
  # VAA = transfer amount / total weighted enrolment
  mutate(
    VAA_1_4_simulado = receita_simulada / mp,   # counterfactual (FUNDEF)
    VAA_1_4_real     = receita_real     / mp    # actual (FUNDEB)
  ) %>%
  relocate(VAA_1_4_simulado, .after = receita_real) %>%
  relocate(VAA_1_4_real,     .after = VAA_1_4_simulado) %>%
  add_label("VAA_1_4_simulado", "Valor Aluno/Ano caso mantido o FUNDEF (pond. do FUNDEB 2007)") %>%
  add_label("VAA_1_4_real",     "Valor Aluno/Ano real (pond. do FUNDEB 2007)") %>%
  mutate(
    # % difference: positive means the municipality gained from FUNDEB
    d_vaa = 100 * ((VAA_1_4_real - VAA_1_4_simulado) / VAA_1_4_simulado)
  ) %>%
  relocate(d_vaa, .after = dif_per_coef) %>%
  add_label("d_vaa", "Dif. (%) do VAA simulado pro real")


### 5.6.1 Add Total 2006 Enrolment and Per-Student Spending Measures ──────── ###

simulacao <- simulacao %>%
  mutate(
    # Sum of all 2006 enrolment categories (used as the dosage denominator)
    total_alunos_2006   = mat_creche + mat_pre + mat_inf_ind +
      mat_fund_in_urb + mat_fund_in_rur + mat_reg_in_ind +
      mat_fund_fin_urb + mat_fund_fin_rur + mat_reg_fin_ind +
      mat_fund_integ + mat_em_urb + mat_em_rur + mat_em_ind +
      mat_em_prof + mat_eja + mat_eja_ind + mat_tot_esp,
    # Per-student transfer under actual FUNDEB and under the FUNDEF counterfactual
    rs_por_aluno_fundeb = receita_real     / total_alunos_2006,
    rs_por_aluno_sim    = receita_simulada / total_alunos_2006,
    # Monetary treatment intensity: BRL per student gained/lost in the transition
    dif_rs_aluno        = rs_por_aluno_fundeb - rs_por_aluno_sim
  )

saveRDS(simulacao, file.path(PATH_TUFFY, "Dados/simulacao_const.rds"))

# Clear all simulation intermediates before the regression assembly
rm(df_fundo, estaduais, lista_ufs, mat_2006_munic, mat_2006, mat_cd, mat_ind,
   totais_estado2, arquivos, FD_eja, FD_esp, FD_ind, FD1, FD1_int, FD1_r,
   FD1_u, FD2, FD2_r, FD2_u, FD3_p, FD3_r, FD3_u, FDC, FDI, file, pasta,
   df, df_fnde, df_fundeb_munic)

rm(list = ls())
gc()


# ============================================================================ #
# 6. REGRESSION DATA — Joining All Sources ----
# ============================================================================ #
# This section assembles the final panel used in all regressions by joining:
#   df_trn        — FUNDEF/FUNDEB transfers + SAEB scores (Section 3 export)
#   df_sim        — Counterfactual simulation (Section 5)
#   df_fib        — FINBRA education expenditures (Section 1.3)
#   pib / pib2    — Municipal GDP per capita (IBGE, two series)
#   df_pesosaeb   — SAEB enrolment weights (external file)
#   mat_mun_2006  — 2006 Censo municipal enrolment (Section 5.1)


# ── Load All Input Files ────────────────────────────────────────────────────
df_trn <- read.csv2(
  file.path(PATH_GIOVANNI, "Dados/painel_notas_transferencias_2000_2024.csv")
)

# SAEB enrolment weights (municipality × year).
# NOTE: pesos_saeb3.rds must be built externally and present before running.
df_pesosaeb <- readRDS(file.path(PATH_TUFFY, "Dados/pesos_saeb3.rds"))

# Counterfactual simulation results from Section 5
df_sim <- readRDS(file.path(PATH_TUFFY, "Dados/simulacao_const.rds"))

# FINBRA combined education expenditure panel from Section 1.3
df_fib <- read.csv2(
  file.path(
    PATH_GIOVANNI,
    "Dados/Gastos municipais/FINBRA/Despesas/FINBRA_EDU_05_21.csv"
  )
)


# ── IPCA Deflation Index (base year = 2007) ──────────────────────────────── #
# Section 2 deflated transfers to 2021 BRL. Here we rebuild the IPCA index
# normalised to 2007 = 1 because that is the regression base year
# (event time k = 0 corresponds to the FUNDEF → FUNDEB transition).
df_ipca <- read_excel(
  file.path(PATH_GIOVANNI, "Dados/IPCA_acumulado_ano.xlsx"),
  skip = 3
)
colnames(df_ipca) <- c("ano", "ipca")

df_ipca <- df_ipca %>%
  mutate(ano = as.numeric(ano)) %>%
  filter(ano %in% 2000:2021) %>%
  arrange(ano)

df_ipca <- df_ipca %>%
  mutate(
    ipca   = as.numeric(gsub(",", ".", ipca)),
    indice = cumprod(1 + as.numeric(ipca) / 100)
  ) %>%
  mutate(indice = indice / indice[ano == 2007])   # base year = 2007


## 6.1 SAEB Weights and GDP ──────────────────────────────────────────────────
# Rename weight columns to a cleaner format and join onto the transfer panel.
# Weights allow computing SAEB-weighted averages across municipalities.
df_pesosaeb <- df_pesosaeb %>%
  rename(peso_5 = ano_5, peso_9 = ano_9)

df_trn <- df_trn %>%
  left_join(df_pesosaeb,
            by = c("codigo_ibge" = "CO_MUNICIPIO", "ano" = "NU_ANO_CENSO"))

# Municipal GDP per capita — two IBGE series covering non-overlapping year ranges
pib <- read_excel(
  file.path(PATH_TUFFY, "Dados/PIB dos Municípios - base de dados 2002-2009.xls")
) %>%
  filter(Ano >= 2005) %>%
  select(1, 7, 8, 40)   # year, IBGE code, municipality name, GDP per capita

pib2 <- read_excel(
  file.path(PATH_TUFFY, "Dados/PIB dos Municípios - base de dados 2010-2021.xlsx")
) %>%
  select(1, 7, 8, 40)


## 6.2 Include Main Treatment and Control Variables ───────────────────────── #
# Join the simulation results onto the panel to obtain the treatment measures
# (dif_per_coef, dosage, aluno_dosage) and the 2007 counterfactual receipt.
# Also attach the 2007-base deflator to enable real-spending calculations.

# Network filter for SAEB observations: keep only the chosen network
# in biennial (odd) years; retain all rows in non-SAEB years.
rede_reg <- "Pública"   # change to "Estadual" or "Municipal" for sub-samples

temp <- df_trn %>%
  left_join(
    df_sim %>% select(c(codigo_ibge, coef_est_fnde:receita_real, dif_rs_aluno,
                        rs_por_aluno_fundeb, rs_por_aluno_sim, shr_inf,
                        tot_matri, total_alunos_2006, total_fundo_d_real)),
    by = "codigo_ibge"
  ) %>%
  filter(!is.na(coef_est_fnde)) %>%    # drop municipalities with no simulation data
  mutate(
    k  = ano - 2007,                   # event time: 0 = year of FUNDEB introduction
    uf = as.factor(uf)
  ) %>%
  left_join(
    df_ipca %>% rename(indice_ipca_07 = indice) %>% select(ano, indice_ipca_07),
    by = "ano"
  )

# Apply network filter: public network in SAEB years; all rows otherwise
temp <- temp %>%
  filter(
    case_when(
      ano >= 2005 & ano %% 2 != 0 ~ rede == rede_reg,
      TRUE ~ TRUE
    )
  ) %>%
  # Remove the 7th IBGE check digit so the code matches FINBRA and the Censo
  mutate(codigo_ibge = as.numeric(str_sub(as.character(codigo_ibge), 1, -2)))


## 6.3 Join FINBRA Education Expenditure Variables ─────────────────────────── #
# Merge real education spending from FINBRA. Reported zeros are treated as
# missing data (reporting gaps) rather than true zero expenditure.
# All spending values are deflated to constant 2007 BRL using indice_ipca_07.

temp <- temp %>%
  left_join(df_fib %>% select(-c(uf)), by = c("codigo_ibge", "ano")) %>%
  mutate(
    educacao           = ifelse(educacao           == 0, NA, educacao),
    ensino_fundamental = ifelse(ensino_fundamental == 0, NA, ensino_fundamental),
    ensino_medio       = ifelse(ensino_medio       == 0, NA, ensino_medio),
    educacao_infantil  = ifelse(educacao_infantil  == 0, NA, educacao_infantil)
  ) %>%
  mutate(
    # Deflate nominal spending to constant 2007 BRL
    real_des_edu  = educacao           / indice_ipca_07,
    real_des_fund = ensino_fundamental / indice_ipca_07,
    real_des_med  = ensino_medio       / indice_ipca_07,
    real_des_inf  = educacao_infantil  / indice_ipca_07,
    # Per-capita spending (population denominator from FINBRA)
    des_edu_pc  = real_des_edu  / populacao,
    des_fund_pc = real_des_fund / populacao,
    des_med_pc  = real_des_med  / populacao,
    des_inf_pc  = real_des_inf  / populacao
  ) %>%
  # For 2013+ keep only "Despesas Empenhadas" (committed expenditure),
  # the most comparable accounting category across years
  filter(ano < 2013 | (ano >= 2013 & coluna == "Despesas Empenhadas")) %>%
  relocate(despesas_totais, .after = "nome") %>%
  relocate(educacao,        .after = "despesas_totais") %>%
  relocate(populacao,       .after = "educacao") %>%
  relocate(des_fund_pc,     .after = "populacao") %>%
  relocate(des_med_pc,      .after = "populacao") %>%
  relocate(des_inf_pc,      .after = "populacao") %>%
  # Within each municipality, propagate the 2006 education spending value
  # to all years so the baseline spending control is always available
  group_by(codigo_ibge) %>%
  mutate(ed_spending_2006 = if_else(ano == 2006, educacao, NA_real_)) %>%
  fill(ed_spending_2006, .direction = "downup") %>%
  ungroup()


## 6.4 Join GDP Per Capita ─────────────────────────────────────────────────── #
colnames(pib)  <- c("ano", "codigo_ibge", "nom", "PIBpc")
colnames(pib2) <- c("ano", "codigo_ibge", "nom", "PIBpc")

# Stack both IBGE GDP series and remove the 7th check digit from municipality codes
pib <- bind_rows(pib, pib2) %>%
  mutate(codigo_ibge = as.numeric(str_sub(as.character(codigo_ibge), 1, -2)))

temp <- left_join(temp, pib, by = c("codigo_ibge", "ano")) %>%
  relocate(PIBpc, .after = "nome")

rm(pib2)


## 6.5 Join 2006 Censo Enrolment by Modality ─────────────────────────────── #
# Load the 2006 municipal enrolment counts (saved in Section 5.1.2) for the
# municipal network only, which is the primary recipient of FUNDEF transfers.
# These are used as the denominator in the aluno_dosage treatment variable.
mat_mun_2006 <- readRDS(file.path(PATH_TUFFY, "Dados/censo_2006_filtrado_mun.rds")) %>%
  filter(DEP == "Municipal") %>%
  mutate(
    mat_reg_fund = mat_reg_in + mat_reg_fin,
    codigo_ibge  = as.numeric(CODMUNIC) %/% 10   # strip check digit
  ) %>%
  group_by(codigo_ibge) %>%
  summarise(
    mat_fun = sum(mat_reg_fund, na.rm = TRUE),
    mat_inf = sum(mat_tot_inf,  na.rm = TRUE),
    mat_med = sum(mat_tot_em,   na.rm = TRUE),
    mat_esp = sum(mat_tot_esp,  na.rm = TRUE),
    mat_eja = sum(mat_tot_eja,  na.rm = TRUE)
  ) %>%
  mutate(total_alunos_2006 = mat_fun + mat_inf + mat_med + mat_esp + mat_eja) %>%
  ungroup()


## 6.6 Compute the Main Dosage Variables ──────────────────────────────────── #
# dosage      = (receita_real − receita_simulada) / receita_real
#   → fraction of actual 2007 FUNDEB receipt that exceeds the FUNDEF baseline
#   → positive: municipality gained from FUNDEB; negative: it lost
#   → preferred specification
#
# aluno_dosage = (receita_real − receita_simulada) / total_alunos_2006
#   → same treatment expressed in BRL per 2006 student (alternative scaling)

df_reg <- temp %>%
  filter(codigo_ibge > 10) %>%       # drop 2-digit state-level records
  select(-total_alunos_2006) %>%     # replace simulation count with Censo count
  left_join(mat_mun_2006, by = "codigo_ibge") %>%
  mutate(
    dif_rs_aluno_100 = dif_rs_aluno / 100,    # express treatment in hundreds of BRL per student
    dosage           = (receita_real - receita_simulada) / receita_real,          # preferred
    aluno_dosage     = (receita_real - receita_simulada) / total_alunos_2006      # alternative
  )

colnames(df_reg)

attr(df_reg$dosage,       "label") <- "Parcela da diferença de receita pelo FUNDEB (2007)"
attr(df_reg$aluno_dosage, "label") <- "Diferença de receita (2007) por aluno (2006)"

saveRDS(df_reg, file.path(PATH_TUFFY, "Dados/regdf.rds"))

gc()
rm(list = ls())


# ============================================================================ #
# 7. DATA CLEANING AND FLAGS ----
# ============================================================================ #
# Load the regression data frame and apply a series of cleaning steps:
#   (a) drop auxiliary columns no longer needed downstream
#   (b) join the richer annual school-census panel (censo_escolar_base_v2)
#       to get year-by-year enrolment counts (2005–2018)
#   (c) compute year-on-year growth rates for enrolment and education spending
#   (d) compute per-student spending ratios using annual enrolment
#   (e) re-compute aluno_dosage using the annual census total for 2006
#   (f) create binary flags for municipalities with extreme growth in any year
#       — used as exclusion criteria in robustness checks

data <- readRDS(file.path(PATH_TUFFY, "Dados/regdf.rds")) %>%
  # Drop columns that were intermediates or are superseded by annual census data
  select(-c(
    X.x, total_politica_d, total_coun_d,
    # Grade-specific approval rates (only cycle-level averages used in regressions)
    tx_aprovacao_iniciais, tx_aprovacao_1, tx_aprovacao_2, tx_aprovacao_3,
    tx_aprovacao_4, tx_aprovacao_5, tx_aprovacao_6, tx_aprovacao_7,
    tx_aprovacao_9, tx_aprovacao_em, tx_aprovacao_1em, tx_aprovacao_2em,
    tx_aprovacao_3em, tx_aprovacao_4em,
    codigo_ibge_n, coef_simulado, dif_per_coef, dif_coef_pp, ipca, X.y, coluna
  )) %>%
  # Tag the 2006-Censo enrolment columns with _aux so they are distinguishable
  # from the annual enrolment columns about to be joined from censo_escolar_base_v2
  rename(
    mat_fun_aux = mat_fun, mat_med_aux = mat_med,
    mat_inf_aux = mat_inf, mat_esp_aux = mat_esp,
    mat_eja_aux = mat_eja
  )


# ── Load Annual School Census Panel ─────────────────────────────────────── #
# censo_escolar_base_v2.rds is a pre-built panel of annual enrolment counts
# from the Censo Escolar (2005–2018), aggregated at municipality level.
# NOTE: this file must be built separately before running Section 7.

df_enroll <- readRDS(file.path(PATH_TUFFY, "Dados/censo_escolar_base_v2.rds")) %>%
  group_by(codmun, ano) %>%
  mutate(
    # Combine initial and final EF for both 9-year and 8-year tracks
    mat_reg_fun9 = ifelse(!is.na(reg_in_9) & !is.na(reg_fin_9), reg_in_9 + reg_fin_9, NA),
    mat_reg_fun8 = ifelse(!is.na(reg_in_8) & !is.na(reg_fin_8), reg_in_8 + reg_fin_8, NA)
  ) %>%
  summarise(
    mat_fun   = sum(ef_tot,            na.rm = TRUE),
    mat_med   = sum(em_tot,            na.rm = TRUE),
    mat_inf   = sum(day_tot + pre_tot, na.rm = TRUE),
    mat_esp   = sum(esp_tot,           na.rm = TRUE),
    mat_eja   = sum(eja_tot,           na.rm = TRUE),
    mat_total = mat_fun + mat_med + mat_inf + mat_eja + mat_esp,
    mat_fun9  = sum(mat_reg_fun9,      na.rm = TRUE),
    mat_fun8  = sum(mat_reg_fun8,      na.rm = TRUE),
    mat_ini9  = sum(reg_in_9,          na.rm = TRUE),
    mat_ini8  = sum(reg_in_8,          na.rm = TRUE),
    mat_fin9  = sum(reg_fin_9,         na.rm = TRUE),
    mat_fin8  = sum(reg_fin_8,         na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  # Remove IBGE check digit (7-digit → 6-digit code)
  mutate(codmun = codmun %/% 10) %>%
  rename(codigo_ibge = codmun)

# Merge annual enrolment into the main data frame
data <- data %>%
  left_join(df_enroll, by = c("ano", "codigo_ibge")) %>%
  group_by(codigo_ibge) %>%
  filter(ano > 2004 & ano < 2019) %>%   # restrict to the analysis window
  # Year-on-year growth rates (%) for enrolment and real education spending
  mutate(
    growth_enroll = ((mat_total - lag(mat_total)) / lag(mat_total)) * 100,
    growth_spend  = ((real_des_edu - lag(real_des_edu)) / lag(real_des_edu)) * 100
  ) %>%
  ungroup() %>%
  group_by(codigo_ibge, ano) %>%
  # Per-student spending rates using annual enrolment as denominator
  mutate(
    real_des_edu_pa = ifelse(mat_total != 0, real_des_edu  / mat_total, NA),
    real_des_inf_pa = ifelse(mat_inf   != 0, real_des_inf  / mat_inf,   NA),
    real_des_fun_pa = ifelse(mat_fun   != 0, real_des_fund / mat_fun,   NA),
    real_des_med_pa = ifelse(mat_med   != 0, real_des_med  / mat_med,   NA)
  ) %>%
  ungroup() %>%
  # Remove the 2006-Censo snapshot columns; annual panel is now the reference
  select(-c(mat_fun_aux, mat_med_aux, mat_inf_aux, mat_esp_aux, mat_eja_aux)) %>%
  group_by(codigo_ibge) %>%
  # Recompute aluno_dosage using the 2006 total from the annual census panel
  # (preferred final specification: more consistent with annual mat_total)
  mutate(
    aluno_dosage = (receita_real - receita_simulada) / mat_total[ano == 2006]
  ) %>%
  ungroup() %>%
  select(-c(tx_aprovacao_finais, tx_aprovacao_8, rs_por_aluno_fundeb,
            rs_por_aluno_sim, nom, nom_mun))


## 7.1 Outlier Flags ──────────────────────────────────────────────────────── ##
# For each municipality, flag whether any year in the panel shows an annual
# growth rate above/below a given threshold. Flags are binary: 1 if any year
# exceeds the threshold, 0 otherwise. They allow sensitivity analysis by
# sequentially excluding more extreme municipalities from the regression sample.

data <- data %>%
  group_by(codigo_ibge) %>%
  mutate(
    # Enrolment growth — increases
    flag_enroll15  = ifelse(any(growth_enroll >  15, na.rm = TRUE), 1, 0),
    flag_enroll20  = ifelse(any(growth_enroll >  20, na.rm = TRUE), 1, 0),
    flag_enroll25  = ifelse(any(growth_enroll >  25, na.rm = TRUE), 1, 0),
    flag_enroll30  = ifelse(any(growth_enroll >  30, na.rm = TRUE), 1, 0),
    flag_enroll40  = ifelse(any(growth_enroll >  40, na.rm = TRUE), 1, 0),
    # Enrolment growth — decreases
    flag_enrollm15 = ifelse(any(growth_enroll < -15, na.rm = TRUE), 1, 0),
    flag_enrollm20 = ifelse(any(growth_enroll < -20, na.rm = TRUE), 1, 0),
    flag_enrollm25 = ifelse(any(growth_enroll < -25, na.rm = TRUE), 1, 0),
    flag_enrollm30 = ifelse(any(growth_enroll < -30, na.rm = TRUE), 1, 0),
    flag_enrollm40 = ifelse(any(growth_enroll < -40, na.rm = TRUE), 1, 0),
    # Spending growth — increases
    flag_spend15   = ifelse(any(growth_spend >  15, na.rm = TRUE), 1, 0),
    flag_spend20   = ifelse(any(growth_spend >  20, na.rm = TRUE), 1, 0),
    flag_spend25   = ifelse(any(growth_spend >  25, na.rm = TRUE), 1, 0),
    flag_spend30   = ifelse(any(growth_spend >  30, na.rm = TRUE), 1, 0),
    flag_spend40   = ifelse(any(growth_spend >  40, na.rm = TRUE), 1, 0),
    flag_spend50   = ifelse(any(growth_spend >  50, na.rm = TRUE), 1, 0),
    flag_spend60   = ifelse(any(growth_spend >  60, na.rm = TRUE), 1, 0),
    flag_spend70   = ifelse(any(growth_spend >  70, na.rm = TRUE), 1, 0),
    flag_spend80   = ifelse(any(growth_spend >  80, na.rm = TRUE), 1, 0),
    # Spending growth — decreases
    flag_spendm15  = ifelse(any(growth_spend < -15, na.rm = TRUE), 1, 0),
    flag_spendm20  = ifelse(any(growth_spend < -20, na.rm = TRUE), 1, 0),
    flag_spendm25  = ifelse(any(growth_spend < -25, na.rm = TRUE), 1, 0),
    flag_spendm30  = ifelse(any(growth_spend < -30, na.rm = TRUE), 1, 0),
    flag_spendm40  = ifelse(any(growth_spend < -40, na.rm = TRUE), 1, 0),
    flag_spendm50  = ifelse(any(growth_spend < -50, na.rm = TRUE), 1, 0),
    flag_spendm60  = ifelse(any(growth_spend < -60, na.rm = TRUE), 1, 0),
    flag_spendm70  = ifelse(any(growth_spend < -70, na.rm = TRUE), 1, 0),
    flag_spendm80  = ifelse(any(growth_spend < -80, na.rm = TRUE), 1, 0)
  ) %>%
  ungroup()

# Summary of flag prevalence across the sample
summary(data %>% select(flag_enroll15, flag_enroll20, flag_enroll25, flag_enroll30,
                        flag_spend15, flag_spend20, flag_spend25, flag_spend30,
                        flag_spend40, flag_spend50, flag_spend60, flag_spend70,
                        flag_spend80))

test2 <- data %>% filter(flag_enroll15 == 0 & flag_enrollm15 == 0 & ano == 2007)


### 7.1.1 Visualise Flag Prevalence ──────────────────────────────────────── ###
# Bar chart showing the percentage of municipalities flagged at each threshold
# for enrolment (green) and spending (blue), separately for increases and decreases.

mean_long <- data %>%
  summarise(across(starts_with("flag_"), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "mean_value") %>%
  mutate(
    color  = ifelse(row_number() <= 10, "red", "darkblue"),
    growth = case_when(
      color == "red"      & row_number() ==  1 ~  15,
      color == "red"      & row_number() ==  2 ~  20,
      color == "red"      & row_number() ==  3 ~  25,
      color == "red"      & row_number() ==  4 ~  30,
      color == "red"      & row_number() ==  5 ~  40,
      color == "red"      & row_number() ==  6 ~ -15,
      color == "red"      & row_number() ==  7 ~ -20,
      color == "red"      & row_number() ==  8 ~ -25,
      color == "red"      & row_number() ==  9 ~ -30,
      color == "red"      & row_number() == 10 ~ -40,
      color == "darkblue" & row_number() == 11 ~  15,
      color == "darkblue" & row_number() == 12 ~  20,
      color == "darkblue" & row_number() == 13 ~  25,
      color == "darkblue" & row_number() == 14 ~  30,
      color == "darkblue" & row_number() == 15 ~  40,
      color == "darkblue" & row_number() == 16 ~  50,
      color == "darkblue" & row_number() == 17 ~  60,
      color == "darkblue" & row_number() == 18 ~  70,
      color == "darkblue" & row_number() == 19 ~  80,
      color == "darkblue" & row_number() == 20 ~ -15,
      color == "darkblue" & row_number() == 21 ~ -20,
      color == "darkblue" & row_number() == 22 ~ -25,
      color == "darkblue" & row_number() == 23 ~ -30,
      color == "darkblue" & row_number() == 24 ~ -40,
      color == "darkblue" & row_number() == 25 ~ -50,
      color == "darkblue" & row_number() == 26 ~ -60,
      color == "darkblue" & row_number() == 27 ~ -70,
      color == "darkblue" & row_number() == 28 ~ -80,
      TRUE ~ NA
    ),
    mean_value = round(100 * mean_value, digits = 0)
  )

mean_long

plot_df <- mean_long %>%
  mutate(
    group    = case_when(
      color == "red"      ~ "Matriculas",
      color == "darkblue" ~ "Despesas Educacionais",
      TRUE                ~ "Other"
    ),
    growth_f = factor(growth, levels = sort(unique(growth)))
  )

my_cols <- c("Matriculas" = "lightgreen", "Despesas Educacionais" = "steelblue1")
pd      <- position_dodge(width = 0.6)

p <- ggplot(plot_df, aes(x = growth_f, y = mean_value, fill = group)) +
  geom_col(data = filter(plot_df, group == "Despesas Educacionais"),
           width = 0.55, position = pd, alpha = 0.8,  color = "black") +
  geom_col(data = filter(plot_df, group == "Matriculas"),
           width = 0.55, position = pd, alpha = 0.95, color = "black") +
  geom_text(aes(label = mean_value),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 3.6, color = "black") +
  scale_fill_manual(values = my_cols, name = "Group") +
  labs(
    title = "Crescimento anual Municipal",
    x     = "Threshold de crescimento (%)",
    y     = "Total da amostra (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom"
  ) +
  scale_x_discrete(labels = levels(plot_df$growth_f))

# Add a vertical line separating negative from positive thresholds on the x-axis
unique_growth <- sort(unique(plot_df$growth))
pos_le_zero   <- sum(unique_growth <= 0)
x_vline_at    <- pos_le_zero + 0.5

p <- p + geom_vline(xintercept = x_vline_at, color = "black", linewidth = 0.8)
p

ggsave(
  filename = "dist_crescimento_2018.png",
  plot     = p,
  path     = file.path(PATH_TUFFY, "Resultados/v3/Figuras/ES/Robust"),
  width    = 600 / 96, height = 420 / 96, dpi = 110
)

rm(plot_df, mean_long, mean_df, od, p)


## 7.2 Save Final Dataset ──────────────────────────────────────────────────── ##
# regdf_flags.rds is the primary input for all regression scripts.
# It covers 2005–2018 and contains all outcome variables, treatment variables,
# controls, and the full set of outlier flags.
saveRDS(data, file.path(PATH_TUFFY, "Dados/regdf_flags.rds"))