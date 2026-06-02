# ---------------------------------------------------------------------------- #
# Data
# DataBase adjustment - Fundef Recreation
# Last edited by: Tuffy Licciardi Issa
# Date: 30/01/2025
# ---------------------------------------------------------------------------- #

#' I will try to recreate the Fundef transfer and total UF Fund amount with the
#' available data from FINBRA

# ---------------------------------------------------------------------------- #
# Libraries -----
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(readxl)
library(writexl)
library(lmtest)
library(fixest)
library(xtable)
library(data.table)
library(stargazer)
library(AER)
library(sf)
library(janitor)
library(geobr)
library(RColorBrewer)
library(ggnewscale)
library(cobalt)
library(did)
library(MatchIt)
library(fastDummies)
library(broom)
library(rdrobust)
library(knitr)
library(kableExtra)
library(scales)
library(httr2)
library(glue)
library(ggbreak)
library(ggtext)
library(jsonlite)
library(haven)


#Desativando a notação científica
options(scipen = 999)

# ---------------------------------------------------------------------------- #
# 1. Data -----
# ---------------------------------------------------------------------------- #

# library(httr2)
# library(jsonlite)
# library(dplyr)
# library(tidyr)
# 
# url <- "https://www.fnde.gov.br/olinda-ide/servico/DADOS_ABERTOS_SIOPE/versao/v1/odata/Receita_Siope(Ano_Consulta=@Ano_Consulta,Num_Peri=@Num_Peri,Sig_UF=@Sig_UF)?@Ano_Consulta=2006&@Num_Peri=1&@Sig_UF='AC'&$top=100000&$format=json"
# 
# x <- jsonlite::fromJSON(url)$value
# 
# mun_2006_ac <- as_tibble(x) %>%
#   filter(TIPO == "Municipal") %>%
#   pivot_wider(
#     id_cols = c(TIPO, NUM_ANO, NUM_PERI, COD_UF, SIG_UF, COD_MUNI, NOM_MUNI, NOM_COLU),
#     names_from = NOM_ITEM,
#     values_from = VAL_DECL,
#     values_fn = sum,
#     values_fill = 0
#   )



# ---------------------------------------------------------------------------- #
## 1.2 Opening Data ----
# ---------------------------------------------------------------------------- #

siope_06 <- read.csv("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/SIOPE/siope_AC_2006_P1.csv")



# ---------------------------------------------------------------------------- #
# 2. Contribution ----
# ---------------------------------------------------------------------------- #

# ── 1. Pivot and filter once ────────────────────────────────────────────────
test1_full <- siope_06 %>%
  mutate(NOM_MUNI = ifelse(NOM_MUNI == "null", "Estadual", NOM_MUNI)) %>%
  pivot_wider(
    id_cols     = c(TIPO, NUM_ANO, NUM_PERI, COD_UF, SIG_UF, COD_MUNI, NOM_MUNI, NOM_COLU),
    names_from  = NOM_ITEM,
    values_from = VAL_DECL,
    values_fn   = sum,
    values_fill = 0
  ) %>%
  clean_names() %>%
  filter(nom_colu == "Receitas Realizadas")

# ── 2. Municipal layer ───────────────────────────────────────────────────────
municipios <- test1_full %>%
  filter(tipo == "Municipal") %>%
  mutate(
    mun_contribution = deducao_do_fpm_para_o_fundef_15_percent +
      deducao_de_lc_87_96_para_o_fundef_15_percent +
      deducao_do_icms_para_o_fundef_15_percent +
      deducao_do_ipi_exportacao_para_o_fundef_15_percent
  ) %>%
  select(
    cod_uf, sig_uf, cod_muni, nom_muni,
    fundef_received  = transferencias_de_recursos_do_fundef,
    mun_contribution
  )

# ── 3. State layer — one row per UF ─────────────────────────────────────────
estado <- test1_full %>%
  filter(tipo == "Estadual") %>%
  mutate(
    state_contribution = deducao_do_icms_para_a_formacao_do_fundef_15_percent +
      deducao_da_cota_parte_do_fpe_para_formacao_do_fundef_15_percent +
      deducao_da_cota_parte_do_ipi_exportacao_para_formacao_do_fundef_15_percent
  ) %>%
  group_by(cod_uf) %>%
  summarise(
    state_contribution = sum(state_contribution, na.rm = TRUE),
    .groups = "drop"
  )

# ── 4. Join — state value broadcast to every municipality in the UF ─────────
fundef_06 <- municipios %>%
  left_join(estado, by = "cod_uf") %>%
  mutate(
    # Net position: positive = net gainer, negative = net contributor
    net_position = fundef_received - mun_contribution
  ) %>%
  arrange(sig_uf, nom_muni)

# ── 5. Quick sanity check ────────────────────────────────────────────────────
fundef_06 %>%
  group_by(sig_uf) %>%
  summarise(
    total_received       = sum(fundef_received, na.rm = T),
    total_mun_contrib    = sum(mun_contribution, na.rm = T),
    state_contrib        = first(state_contribution),
    total_pool_contrib   = total_mun_contrib + state_contrib,
    implied_fed_complement = total_received - total_pool_contrib
  )

# ── 6. Federal transfer ────────────────────────────────────────────────────
fundef_06 <- fundef_06 %>% 
  group_by(sig_uf) %>% 
  mutate(federal_transfer = sum(fundef_received, na.rm = T) 
         - sum(mun_contribution) - first(state_contribution) ) %>% 
  ungroup()