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

# # ---------------------------------------------------------------------------- #
# # 1. Data -----
# # ---------------------------------------------------------------------------- #
# 
# finbra_06 <- read_excel("Z:/Tuffy/Paper - Educ/Dados/FINBRA/2006_Receita.xlsx")
# 
# new_df <- finbra_06 %>% 
#   select(CD_UF, CD_MUN, UF, MUNICIPIO, #Municipality and state id
#          `Transf Multigovernamentais`, #Total Fundef Value
#          `Transf Multigov FUNDEF Comp`, `Transf Multigov FUNDEF`, `Outras Transf Multigov`,  #Amount received with Fundef
#          `Deduções Rec Corrente`) #Deduction to build the fund
# 
# test <- new_df %>% 
#   group_by(CD_UF, UF) %>% 
#   summarise(
#     total      = sum(`Transf Multigovernamentais`, na.rm = T),
#     gov_transf = sum(`Transf Multigov FUNDEF Comp`, na.rm = T),
#     tot_fundef = sum(`Transf Multigov FUNDEF`, na.rm = T),
#     other_fundef = sum(`Outras Transf Multigov`, na.rm = T),
#     tot_cost   = sum(`Deduções Rec Corrente`, na.rm = T),
#     
#     .groups = "drop"
#   ) %>% 
#   mutate(
#     total_fundef = gov_transf + tot_fundef,
#     state_cont = total_fundef - tot_cost
#   )
# 
# 
# 
# finbra_07 <- read_excel("Z:/Tuffy/Paper - Educ/Dados/FINBRA/2007_Receita.xlsx")
# 
# new_df2 <- finbra_07 %>% 
#   select(CD_UF, CD_MUN, UF, MUNICIPIO, #Municipality and state id
#          `Transf Multigov FUNDEB`, `Transf Multigov FUNDEB Comp`, #Amount received with Fundef
#          `Dedução Rec Tr União`, `Dedução Rec Tr Estado`) #Deduction to build the fund
# 
# 
# test <- new_df2 %>% 
#   group_by(CD_UF, UF) %>% 
#   mutate(cost = `Dedução Rec Tr União` + `Dedução Rec Tr Estado`) %>% 
#   summarise(
#     gov_transf = sum(`Transf Multigov FUNDEB Comp`, na.rm = T),
#     fundef = sum(`Transf Multigov FUNDEB`, na.rm = T),
#     tot_cost   = sum(cost, na.rm = T),
#     
#     .groups = "drop"
#   ) %>% 
#   mutate(
#     tot_fundef = gov_transf + fundef,
#     state_cont = tot_fundef - tot_cost
#   )
# 
# # ---------------------------------------------------------------------------- #
# # 2. SIOPE -----
# # ---------------------------------------------------------------------------- #
# 
 siope_06 <- read.csv("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/SIOPE/siope_AC_2006_P1.csv")

siope2 <- read.csv("Z:/Tuffy/Paper - Educ/Dados/SIOPE/siope_despesas_AC_2006_P1.csv")

# 
# test1 <- siope_06 %>% 
#   #filter(TIPO == "Estadual",
#          #NOM_COLU == "Receitas Realizadas") %>% 
#   mutate(
#     NOM_MUNI = ifelse(NOM_MUNI == "null", "Estadual", NOM_MUNI)
#   ) %>%
#   pivot_wider(
#     id_cols     = c(TIPO, NUM_ANO, NUM_PERI, COD_UF, SIG_UF, COD_MUNI, NOM_MUNI, NOM_COLU),
#     names_from  = NOM_ITEM,
#     values_from = VAL_DECL,
#     values_fn   = sum,      # agrega somando quando houver linhas duplicadas
#     values_fill = 0         # preenche NA por 0 (útil em receitas)
#   ) %>%
#   clean_names() %>% 
#   filter(nom_colu == "Receitas Realizadas") %>% 
#   mutate(
#     cost = rowSums(across(143:145), na.rm = TRUE) + rowSums(across(9:13), na.rm = TRUE)
#   )
# 
# 
# sum(test1$transferencias_de_recursos_do_fundef) - sum(test1$deducoes_da_receita_corrente) #- sum(test1$fundeg)
# 
# 
# sum(test1$transferencias_de_recursos_do_fundef) - sum(test1$cost)
# 
# 
# 
# 
# 
# 
# 
# 
# #FUNDEB test
# siope_07 <- read.csv("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/SIOPE/siope_AC_2007_P1.csv") 
# 
# temp <- siope_07 %>% 
#   filter(NOM_COLU == "Receitas Realizadas") %>% 
#   mutate(NOM_MUNI = ifelse(NOM_MUNI == 'null', "Estadual", NOM_MUNI)) %>% 
#   pivot_wider(
#     names_from  = NOM_ITEM,
#     values_from = VAL_DECL
#   )
# 
# temp <- siope_07 %>%
#   #filter(NOM_COLU == "Receitas Realizadas") #%>%
#   mutate(
#     NOM_MUNI = ifelse(NOM_MUNI == "null", "Estadual", NOM_MUNI)
#   ) %>%
#   pivot_wider(
#     id_cols     = c(TIPO, NUM_ANO, NUM_PERI, COD_UF, SIG_UF, COD_MUNI, NOM_MUNI, NOM_COLU),
#     names_from  = NOM_ITEM,
#     values_from = VAL_DECL,
#     values_fn   = sum,      # agrega somando quando houver linhas duplicadas
#     values_fill = 0         # preenche NA por 0 (útil em receitas)
#   ) %>%
#   clean_names() %>% 
#   mutate(
#     cost = deducao_da_receita_do_fpm_fundeb_e_redutor_financeiro_16_66_percent +
#       deducao_da_receita_para_formacao_do_fundeb_itr_6_66_percent +
#       deducao_de_receita_para_a_formacao_do_fundeb_icms_desoneracao_lei_complementar_87_96_16_66_percent +
#       deducao_de_receita_para_a_formacao_do_fundeb_icms_16_66_percent +
#       deducao_da_receita_para_a_formacao_do_fundeb_ipva_6_66_percent +
#       deducao_da_receita_para_a_formacao_do_fundeb_ipi_exportacao_16_66_percent +
#       #Estadual
#       deducao_da_receita_de_ipva_para_a_formacao_do_fundeb_6_66_percent +                                                                  
#       deducao_da_receita_de_itcd_para_a_formacao_do_fundeb_6_66_percent +                                                                
#       deducao_de_receita_de_icms_para_a_formacao_do_fundeb_16_66_percent +                                                               
#       deducao_da_cota_parte_do_fpe_para_formacao_do_fundeb_16_66_percent  +                                                              
#    deducao_de_receita_para_a_formacao_do_fundeb_ipi_exportacao_16_66_percent
#   ) 
# 
# #Receitas orçadas contem compl. gov
# 
# 
# sum(temp$transferencias_de_recursos_do_fundeb) - sum(temp$cost) + sum(temp$receita_de_remuneracao_de_depositos_bancarios_de_recursos_vinculados_fundeb) + sum(temp$transferencia_de_recursos_daun)
# 
# 
# 
# 
# 
# 
# 

#' Lets begin with the extraction of the values from the SIOPE in 2006. The first
#' exploration will focus on the AC UF data, which will allow me to decompose the
#' correct formula, and better understand the policy recreation. After testing this
#' code I will proceed to reproduce the Federal Transfer calculation for the remaining
#' Brazilian states.


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



