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
# 1. Students Data ----
# ---------------------------------------------------------------------------- #
## 1.1 First data filtering ----
# ---------------------------------------------------------------------------- #

censo <- read_delim("Z:/Arquivos IFB/Censo Escolar/Bases Agregadas/2005/microdados_educação_básica_2005/DADOS/CENSOESC_2005.CSV", delim = "|",
                             col_types = cols(.default = col_double(),
                                              CODFUNC = col_character(),
                                              MASCARA = col_character(),
                                              DEP     = col_character(),
                                              LOC = col_character(),
                                              UF = col_character(),
                                              SIGLA = col_character(),
                                              MUNIC = col_character(),
                                              ED_INDIG = col_character(),
                                              MAT_QUIL = col_character(),
                                              MAT_ETNI = col_character(),
                                              NIVELCRE = col_character(),
                                              NIVELPRE = col_character(),
                                              NIV_F1A4_8 = col_character(),
                                              NIV_F5A8_8 = col_character(),
                                              NIV_F9INI = col_character(),
                                              NIV_F9FIM = col_character(),
                                              NIVELMED = col_character(),
                                              NIVM_INT = col_character(),
                                              SUPL_AVA = col_character(),
                                              SUPL_SAVA = col_character(),
                                              EDPROFIS = col_character(),
                                              ESP_EXCL = col_character(),
                                              ESP_T_ES = col_character(),
                                              ENS_INCL = col_character(),
                                              ESC_ASSE = col_character(),
                                              AREA_QUIL = col_character(),
                                              ESP_S_RE = col_character(),
                                              ESP_A_IN = col_character(),
                                              ED_INDIG = col_character(),
                                              ED_IN_LM = col_character(),
                                              COD_ID_IND = col_character(),
                                              ED_IN_LP = col_character(),
                                              MAT_ETNI = col_character(),
                                              MAT_QUIL = col_character(),
                                              ESC_T_IN = col_character(),
                                              PRED_ESC = col_character(),
                                              TEMPLO = col_character(),
                                              DEF11C = col_double(),
                                              DEF11D = col_double(),
                                              DEF11E = col_double(),
                                              DEF11F = col_double(),
                                              NEF11C = col_double(),
                                              NEF11D = col_double(),
                                              NEF11E = col_double(),
                                              NEF11F = col_double(),
                                              DEF11G = col_double(),
                                              DEF11H = col_double(),
                                              DEF11I = col_double(),
                                              DEF11J = col_double(),
                                              NEF11G = col_double(),
                                              NEF11H = col_double(),
                                              NEF11I = col_double(),
                                              NEF11J = col_double()))


# ---------------------------------------------------------------------------- #
## 1.2 Per school data ----
# ---------------------------------------------------------------------------- #

mat_2005 <- censo %>%
  select(c(1:9,

           CODFUNC,
           ED_INDIG,
           MAT_QUIL,
           AREA_QUIL,
           ESC_ASSE,
           ED_IN_LM,
           ED_IN_LP,
           MAT_ETNI,
           ESC_T_IN,

           DEF11C:DEF11F, NEF11C:NEF11F, # EF iniciais (8 anos)
           DE9F11C:DE9F11G, NE9F11C:NE9F11G, # EF iniciais (9 anos)
           DEF11G:DEF11J, NEF11G:NEF11J, # EF finais (8 anos)
           DE9F11H:DE9F11N, NE9F11H:NE9F11N, # EF finais (9 anos)
           VEE1431:VEE1437, # Alunos de educação especial do EF por ano de nascimento

           #ED especial por série:
           VEE1619:VEE1691, VEE1719:VEE1791, VEE1819:VEE1891, VEE1919:VEE1991, # 1ºEF

           VEE1612:VEE1692, VEE1712:VEE1792, VEE1812:VEE1892, VEE1912:VEE1992, # 2ºEF,
           VEE1613:VEE1693, VEE1713:VEE1793, VEE1813:VEE1893, VEE1913:VEE1993, # 3ºEF
           VEE1614:VEE1694, VEE1714:VEE1794, VEE1814:VEE1894, VEE1914:VEE1994, # 4ºEF
           VEE1615:VEE1695, VEE1715:VEE1795, VEE1815:VEE1895, VEE1915:VEE1995, # 5ºEF
           VEE1616:VEE1696, VEE1716:VEE1796, VEE1816:VEE1896, VEE1916:VEE1996, # 6ºEF
           VEE1617:VEE1697, VEE1717:VEE1797, VEE1817:VEE1897, VEE1917:VEE1997, # 7ºEF
           VEE1618:VEE1698, VEE1718:VEE1798, VEE1818:VEE1898, VEE1918:VEE1998, # 8ºEF

           DEM118, DEM119, DEM11A, DEM11B, DEM11C, # Alunos do EM
           NEM118, NEM119, NEM11A, NEM11B, NEM11C,

           DPE119, NPE119, # Alunos de Creche
           DPE11D, NPE11D, # Alunos de Pré-Escola

           DES101F:DES101A, NES101F:NES101A # Alunos do EJA
  )) %>%

  mutate(COD_UF = as.numeric(str_sub(as.character(CODMUNIC, 1, 2)))) %>%
  mutate(reg_in = rowSums(across(c(DEF11C:NE9F11G)), na.rm = TRUE), # Contando EF de 8 e 9 anos!
         reg_fin = rowSums(across(c(DEF11G:NE9F11N)), na.rm = TRUE),
         esp_iniciais = rowSums(across(c(VEE1619:VEE1994)), na.rm = TRUE),
         esp_finais = rowSums(across(c(VEE1615:VEE1998)), na.rm = TRUE),
         esp_soma = esp_iniciais + esp_finais,
         esp_tot = rowSums(across(c(VEE1431:VEE1437)), na.rm = TRUE),
         em_tot = rowSums(across(c(DEM118:NEM11C)), na.rm = TRUE),
         creche = rowSums(across(c(DPE119, NPE119)), na.rm = TRUE),
         pre_escola = rowSums(across(c(DPE11D, NPE11D)), na.rm = TRUE),
         ed_inf_tot = rowSums(across(c(DPE119:NPE11D)), na.rm = TRUE),
         eja_tot = rowSums(across(c(DES101F:NES101A)), na.rm = TRUE),
         dif = esp_iniciais + esp_finais - esp_tot,
         esp_total_final = pmax(esp_soma, esp_tot)) %>%  # pega o maior valor entre a soma das deficiencias por série,
  # e da desagregação pelo ano de nascimento
  filter(!(DEP %in% c("Particular", "Federal")), # tira escolas federais e particulares
         CODFUNC == "Ativo") %>%  # tira escolas que não estão ativas
  mutate(CODMUNIC = as.numeric(str_c( #concatena
    str_sub(as.character(CODMUNIC), 1, 2), #pega os dois primeiros
    str_sub(as.character(CODMUNIC), -5) #pega os últimos 5
  ))) %>%
  mutate(ind_quil = ifelse(
    (coalesce(ED_INDIG, "n") == "s" |
       coalesce(MAT_QUIL, "n") == "s" |
       coalesce(AREA_QUIL, "n") == "s" |
       coalesce(ED_IN_LM, "n") == "s" |
       coalesce(ED_IN_LP, "n") == "s"),
    1, 0
  ))


# ---------------------------------------------------------------------------- #
### 1.2.1 Saving -----
# ---------------------------------------------------------------------------- #

# DF do censo já agregada por escola (é muito pesada para rodar toda vez):
write.csv2(mat_2005, file = "Z:/Tuffy/Paper - Educ/Dados/censo_2005_filtrado.csv")

mat_2005 <- read.csv2("Z:/Tuffy/Paper - Educ/Dados/censo_2005_filtrado.csv")

# ---------------------------------------------------------------------------- #
## 1.3 Studenst per Mun. ----
# ---------------------------------------------------------------------------- #

mat_2005_munic <- mat_2005 %>%
  group_by(CODMUNIC) %>%
  summarise(
    nome = first(MUNIC),
    no_uf = first(UF),
    uf = first(SIGLA),
    
    urb_ini = sum(if_else(LOC == "Urbana", reg_in, 0), na.rm = TRUE),
    urb_fim = sum(if_else(LOC == "Urbana", reg_fin, 0), na.rm = TRUE),
    rur_ini = sum(if_else(LOC == "Rural",  reg_in, 0), na.rm = TRUE),
    rur_fim_esp = sum(if_else(LOC == "Rural", reg_fin + esp_total_final, 0), na.rm = TRUE),
    
    .groups = "drop"
  )


# ---------------------------------------------------------------------------- #
### 1.3.1 Students UF (categories) ----
# ---------------------------------------------------------------------------- #

mat_uf <- mat_2005_munic %>% 
  group_by(uf) %>% 
    summarise(
      urb_ini = sum(urb_ini, na.rm = T),
      urb_fim = sum(urb_fim, na.rm = T),
      rur_ini = sum(rur_ini, na.rm = T),
      rur_fim_esp = sum(rur_fim_esp, na.rm = T)
      ) %>% 
  mutate(total = urb_ini + urb_fim + rur_ini + rur_fim_esp) %>% 
  ungroup()

#' mat_ac <- mat_2005_munic %>% 
#'   filter(uf == "AC") %>% 
#'   group_by(uf) %>% 
#'   summarise(
#'     urb_ini = sum(urb_ini, na.rm = T),
#'     urb_fim = sum(urb_fim, na.rm = T),
#'     rur_ini = sum(rur_ini, na.rm = T),
#'     rur_fim_esp = sum(rur_fim_esp, na.rm = T),
#'     
#'     .groups = "drop"
#'   )
#' 
#' #Calculating the value per student
#' mat_ac <- mat_ac %>% 
#'   mutate(
#'     urb_ini_rs = 1685.41*urb_ini,
#'     urb_fim_rs = 1719.12*urb_fim,
#'     rur_ini_rs = 1769.68*rur_ini,
#'     rur_fim_esp_rs = 1803.39*rur_fim_esp,
#'     
#'     fnd_tot = urb_ini_rs + urb_fim_rs + rur_ini_rs + rur_fim_esp_rs
#'   )
#' 
#' #' Found a 0.24% difference between the estipulated value and the actually received

# ---------------------------------------------------------------------------- #
# 2. SIOPE (2006) ----
# ---------------------------------------------------------------------------- #


# # ========================================================================= #
# # DIAGNOSTIC EXPORT - append to 1_8_fundef_remk.R
# # ========================================================================= #
# 
# library(jsonlite)
# 
# diagnostics <- list()
# 
# # ------------------------------------------------------------------------- #
# # 0) Re-run the pivot for three reference UFs:
# #    AC (small, easy to inspect), SP and RS (should have ~zero complement)
# # ------------------------------------------------------------------------- #
# 
# ref_ufs <- c("AC", "SP", "RS")
# 
# raw_pivots <- map(ref_ufs, function(uf) {
#   file_path <- paste0(
#     "Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/SIOPE/",
#     "siope_", uf, "_2006_P1.csv"
#   )
#   siope <- read.csv(file_path)
#   
#   siope %>%
#     mutate(NOM_MUNI = ifelse(NOM_MUNI == "null", "Estadual", NOM_MUNI)) %>%
#     pivot_wider(
#       id_cols = c(TIPO, NUM_ANO, NUM_PERI, COD_UF, SIG_UF,
#                   COD_MUNI, NOM_MUNI, NOM_COLU),
#       names_from = NOM_ITEM,
#       values_from = VAL_DECL,
#       values_fn = sum,
#       values_fill = 0
#     ) %>%
#     clean_names() %>%
#     filter(nom_colu == "Receitas Realizadas")
# })
# names(raw_pivots) <- ref_ufs
# 
# # ------------------------------------------------------------------------- #
# # 1) Check for "complementacao"/"uniao" columns
# # ------------------------------------------------------------------------- #
# 
# diagnostics$complement_columns <- map(raw_pivots, function(df) {
#   list(
#     complement_cols = names(df)[str_detect(names(df), "complement")],
#     uniao_cols      = names(df)[str_detect(names(df), "uniao")],
#     all_fundef_cols = names(df)[str_detect(names(df), "fundef")]
#   )
# })
# 
# # ------------------------------------------------------------------------- #
# # 2) Check NUM_PERI structure
# # ------------------------------------------------------------------------- #
# 
# diagnostics$num_peri_check <- map(raw_pivots, function(df) {
#   df %>% count(num_peri, tipo)
# })
# 
# # ------------------------------------------------------------------------- #
# # 3) Check ranges of deduction columns (sign issues)
# # ------------------------------------------------------------------------- #
# 
# diagnostics$deducao_ranges <- map(raw_pivots, function(df) {
#   ded_cols <- names(df)[str_detect(names(df), "deducao")]
#   df %>%
#     filter(tipo == "Municipal") %>%
#     select(all_of(ded_cols)) %>%
#     summarise(across(everything(), list(min = ~min(.x, na.rm=TRUE),
#                                         max = ~max(.x, na.rm=TRUE),
#                                         sum = ~sum(.x, na.rm=TRUE))))
# })
# 
# # ------------------------------------------------------------------------- #
# # 4) Municipality count vs expected, per UF
# # ------------------------------------------------------------------------- #
# 
# diagnostics$muni_counts <- map(raw_pivots, function(df) {
#   df %>% filter(tipo == "Municipal") %>% distinct(cod_muni) %>% nrow()
# })
# 
# # ------------------------------------------------------------------------- #
# # 5) Full UF-level summary table (re-run check_uf logic for ref UFs)
# # ------------------------------------------------------------------------- #
# 
# diagnostics$uf_summary <- map(raw_pivots, function(test1_full) {
#   
#   municipios <- test1_full %>%
#     filter(tipo == "Municipal") %>%
#     mutate(
#       mun_contribution =
#         deducao_do_fpm_para_o_fundef_15_percent +
#         deducao_de_lc_87_96_para_o_fundef_15_percent +
#         deducao_do_icms_para_o_fundef_15_percent +
#         deducao_do_ipi_exportacao_para_o_fundef_15_percent
#     )
#   
#   mun_uf <- municipios %>%
#     summarise(
#       mun_received     = sum(transferencias_de_recursos_do_fundef, na.rm = TRUE),
#       mun_contribution = sum(mun_contribution, na.rm = TRUE)
#     )
#   
#   estado <- test1_full %>%
#     filter(tipo == "Estadual") %>%
#     mutate(
#       state_contribution =
#         deducao_do_icms_para_a_formacao_do_fundef_15_percent +
#         deducao_da_cota_parte_do_fpe_para_formacao_do_fundef_15_percent +
#         deducao_da_cota_parte_do_ipi_exportacao_para_formacao_do_fundef_15_percent,
#       state_received = transferencias_de_recursos_do_fundef
#     ) %>%
#     summarise(
#       state_contribution = sum(state_contribution, na.rm = TRUE),
#       state_received     = sum(state_received, na.rm = TRUE)
#     )
#   
#   bind_cols(mun_uf, estado) %>%
#     mutate(
#       total_received     = mun_received + state_received,
#       total_contribution = mun_contribution + state_contribution,
#       federal_complement = total_received - total_contribution
#     )
# })
# 
# # ------------------------------------------------------------------------- #
# # 6) Raw row dump for AC (small enough to inspect fully)
# # ------------------------------------------------------------------------- #
# 
# diagnostics$ac_full_pivot <- raw_pivots$AC
# 
# # ------------------------------------------------------------------------- #
# # 7) Export everything to a single RDS file
# # ------------------------------------------------------------------------- #
# 
# output_path <- "C:/Users/tuffyli/Downloads/fundef_diagnostics.rds"
# saveRDS(diagnostics, output_path)
# 
# cat("Diagnostics saved to:", output_path, "\n")
# cat("File size:", file.size(output_path) / 1024, "KB\n")

# 
# 
# # ========================================================================= #
# # FUNDEF 2006 - UF loop with receipt-side correction
# # ========================================================================= #
# 
# library(tidyverse)
# library(janitor)
# library(purrr)
# 
# options(scipen = 999)
# 
# safe_sum_cols <- function(data, cols) {
#   cols_present <- intersect(cols, names(data))
#   if (length(cols_present) == 0) return(rep(0, nrow(data)))
#   rowSums(dplyr::select(data, all_of(cols_present)), na.rm = TRUE)
# }
# 
# safe_col <- function(data, col_name) {
#   if (col_name %in% names(data)) coalesce(data[[col_name]], 0) else rep(0, nrow(data))
# }
# 
# process_siope_uf <- function(uf, base_path) {
#   
#   file_path <- file.path(base_path, paste0("siope_", uf, "_2006_P1.csv"))
#   siope <- read.csv(file_path)
#   
#   test1_full <- siope %>%
#     mutate(NOM_MUNI = ifelse(NOM_MUNI == "null", "Estadual", NOM_MUNI)) %>%
#     pivot_wider(
#       id_cols     = c(TIPO, NUM_ANO, NUM_PERI, COD_UF, SIG_UF, COD_MUNI, NOM_MUNI, NOM_COLU),
#       names_from  = NOM_ITEM,
#       values_from = VAL_DECL,
#       values_fn   = sum,
#       values_fill = 0
#     ) %>%
#     clean_names() %>%
#     filter(nom_colu == "Receitas Realizadas")
#   
#   mun_contrib_cols <- c(
#     "deducao_do_fpm_para_o_fundef_15_percent",
#     "deducao_de_lc_87_96_para_o_fundef_15_percent",
#     "deducao_do_icms_para_o_fundef_15_percent",
#     "deducao_do_ipi_exportacao_para_o_fundef_15_percent"
#   )
#   
#   state_contrib_cols <- c(
#     "deducao_do_icms_para_a_formacao_do_fundef_15_percent",
#     "deducao_da_cota_parte_do_fpe_para_formacao_do_fundef_15_percent",
#     "deducao_da_cota_parte_do_ipi_exportacao_para_formacao_do_fundef_15_percent"
#   )
#   
#   municipios <- test1_full %>%
#     filter(tipo == "Municipal") %>%
#     mutate(
#       mun_contribution = safe_sum_cols(cur_data(), mun_contrib_cols),
#       received_1 = safe_col(cur_data(), "transferencias_de_recursos_do_fundef"),
#       received_2 = received_1 + safe_col(cur_data(), "transferencias_da_complementacao_da_uniao_ao_fundef"),
#       received_3 = received_2 + safe_col(cur_data(), "rec_de_remuneracao_de_depositos_bancarios_fundef")
#     ) %>%
#     select(
#       cod_uf, sig_uf, cod_muni, nom_muni,
#       mun_contribution,
#       received_1, received_2, received_3
#     )
#   
#   mun_uf <- municipios %>%
#     group_by(cod_uf, sig_uf) %>%
#     summarise(
#       mun_received_1   = sum(received_1, na.rm = TRUE),
#       mun_received_2   = sum(received_2, na.rm = TRUE),
#       mun_received_3   = sum(received_3, na.rm = TRUE),
#       mun_contribution = sum(mun_contribution, na.rm = TRUE),
#       .groups = "drop"
#     )
#   
#   estado <- test1_full %>%
#     filter(tipo == "Estadual") %>%
#     mutate(
#       state_contribution = safe_sum_cols(cur_data(), state_contrib_cols),
#       state_received_1 = safe_col(cur_data(), "transferencias_de_recursos_do_fundef"),
#       state_received_2 = state_received_1 + safe_col(cur_data(), "transferencias_da_complementacao_da_uniao_ao_fundef"),
#       state_received_3 = state_received_2 + safe_col(cur_data(), "rec_de_remuneracao_de_depositos_bancarios_fundef")
#     ) %>%
#     group_by(cod_uf, sig_uf) %>%
#     summarise(
#       state_contribution = sum(state_contribution, na.rm = TRUE),
#       state_received_1   = sum(state_received_1, na.rm = TRUE),
#       state_received_2   = sum(state_received_2, na.rm = TRUE),
#       state_received_3   = sum(state_received_3, na.rm = TRUE),
#       .groups = "drop"
#     )
#   
#   check_uf <- mun_uf %>%
#     left_join(estado, by = c("cod_uf", "sig_uf")) %>%
#     mutate(
#       total_received_1   = mun_received_1 + state_received_1,
#       total_received_2   = mun_received_2 + state_received_2,
#       total_received_3   = mun_received_3 + state_received_3,
#       total_contribution = mun_contribution + state_contribution,
#       federal_complement_1 = total_received_1 - total_contribution,
#       federal_complement_2 = total_received_2 - total_contribution,
#       federal_complement_3 = total_received_3 - total_contribution,
#       uf = sig_uf
#     ) %>%
#     arrange(sig_uf)
#   
#   check_uf
# }
# 
# ufs <- c(
#   "AC","AL","AP","AM","BA","CE","DF","ES","GO",
#   "MA","MT","MS","MG","PA","PB","PR","PE","PI",
#   "RJ","RN","RS","RO","RR","SC","SP","SE","TO"
# )
# 
# base_path <- "Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/SIOPE"
# 
# fundef_2006_uf <- map_dfr(
#   ufs,
#   process_siope_uf,
#   base_path = base_path
# )
# 
# fundef_2006_uf %>%
#   arrange(desc(abs(federal_complement_1)))
# 
# fundef_2006_uf %>%
#   arrange(desc(abs(federal_complement_2)))
# 
# fundef_2006_uf %>%
#   arrange(desc(abs(federal_complement_3)))
# 
# bad_ufs <- fundef_2006_uf %>%
#   filter(federal_complement_3 < -1e-6) %>%
#   select(cod_uf, sig_uf, federal_complement_3)
# 
# bad_ufs
# 
# inspect_uf <- function(uf) {
#   file_path <- paste0(
#     "Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/SIOPE/",
#     "siope_", uf, "_2006_P1.csv"
#   )
#   
#   siope <- read.csv(file_path)
#   
#   df <- siope %>%
#     mutate(NOM_MUNI = ifelse(NOM_MUNI == "null", "Estadual", NOM_MUNI)) %>%
#     pivot_wider(
#       id_cols = c(TIPO, NUM_ANO, NUM_PERI, COD_UF, SIG_UF,
#                   COD_MUNI, NOM_MUNI, NOM_COLU),
#       names_from = NOM_ITEM,
#       values_from = VAL_DECL,
#       values_fn = sum,
#       values_fill = 0
#     ) %>%
#     clean_names() %>%
#     filter(nom_colu == "Receitas Realizadas")
#   
#   tibble(
#     uf = uf,
#     receipt_cols = paste(names(df)[str_detect(names(df), "complement|uniao|deposit|fundef")], collapse = " | "),
#     deduction_cols = paste(names(df)[str_detect(names(df), "deducao")], collapse = " | ")
#   )
# }
# 
# purrr::map_dfr(bad_ufs$sig_uf, inspect_uf)





library(tidyverse)
library(janitor)
library(purrr)

options(scipen = 999)

# ------------------------------------------------------------------------- #
# Helpers
# ------------------------------------------------------------------------- #

safe_sum_cols <- function(data, cols) {
  cols_present <- intersect(cols, names(data))
  if (length(cols_present) == 0) return(rep(0, nrow(data)))
  rowSums(dplyr::select(data, all_of(cols_present)), na.rm = TRUE)
}

sum_matching_cols <- function(data, pattern) {
  cols <- names(data)[str_detect(names(data), pattern)]
  if (length(cols) == 0) return(rep(0, nrow(data)))
  rowSums(dplyr::select(data, all_of(cols)), na.rm = TRUE)
}

process_siope_uf <- function(uf, base_path) {
  
  file_path <- file.path(base_path, paste0("siope_", uf, "_2006_P1.csv"))
  siope <- read.csv(file_path)
  
  test1_full <- siope %>%
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
  
  # ----------------------------------------------------------------------- #
  # Contribution columns
  # ----------------------------------------------------------------------- #
  
  mun_contrib_cols <- c(
    "deducao_do_fpm_para_o_fundef_15_percent",
    "deducao_de_lc_87_96_para_o_fundef_15_percent",
    "deducao_do_icms_para_o_fundef_15_percent",
    "deducao_do_ipi_exportacao_para_o_fundef_15_percent"
  )
  
  state_contrib_cols <- c(
    "deducao_do_icms_para_a_formacao_do_fundef_15_percent",
    "deducao_da_cota_parte_do_fpe_para_formacao_do_fundef_15_percent",
    "deducao_da_cota_parte_do_ipi_exportacao_para_formacao_do_fundef_15_percent"
  )
  
  # ----------------------------------------------------------------------- #
  # Receipt-side columns: use patterns, not a single fixed name
  # ----------------------------------------------------------------------- #
  
  receipt_pattern <- paste(
    c(
      "^transferencias_de_recursos_do_fundef$",
      "^transferencias_da_complementacao_da_uniao_ao_fundef$",
      "^rec_de_remuneracao_de_depositos_bancarios_fundef$",
      "^remuneracao_de_depositos_bancarios$",
      "^remuneracao_de_depositos_bancarios_de_recursos_vinculados$",
      "^remuneracao_de_outros_depositos_bancarios$",
      "^rec_de_remuneracao_de_depositos_bancarios$"
    ),
    collapse = "|"
  )
  
  # ----------------------------------------------------------------------- #
  # Municipal layer
  # ----------------------------------------------------------------------- #
  
  municipios <- test1_full %>%
    filter(tipo == "Municipal") %>%
    mutate(
      mun_contribution = safe_sum_cols(pick(everything()), mun_contrib_cols),
      mun_received     = sum_matching_cols(pick(everything()), receipt_pattern)
    ) %>%
    select(
      cod_uf, sig_uf, cod_muni, nom_muni,
      mun_received,
      mun_contribution
    )
  
  mun_uf <- municipios %>%
    group_by(cod_uf, sig_uf) %>%
    summarise(
      mun_received     = sum(mun_received, na.rm = TRUE),
      mun_contribution = sum(mun_contribution, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ----------------------------------------------------------------------- #
  # State layer
  # ----------------------------------------------------------------------- #
  
  estado <- test1_full %>%
    filter(tipo == "Estadual") %>%
    mutate(
      state_contribution = safe_sum_cols(pick(everything()), state_contrib_cols),
      state_received     = sum_matching_cols(pick(everything()), receipt_pattern)
    ) %>%
    group_by(cod_uf, sig_uf) %>%
    summarise(
      state_contribution = sum(state_contribution, na.rm = TRUE),
      state_received     = sum(state_received, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ----------------------------------------------------------------------- #
  # UF check
  # ----------------------------------------------------------------------- #
  
  check_uf <- mun_uf %>%
    left_join(estado, by = c("cod_uf", "sig_uf")) %>%
    mutate(
      total_received     = mun_received + state_received,
      total_contribution = mun_contribution + state_contribution,
      federal_complement_inferred = total_received - total_contribution,
      federal_complement_problem = total_received < total_contribution,
      uf = sig_uf
    ) %>%
    arrange(sig_uf)
  
  check_uf
}



# ------------------------------------------------------------------------- #
# Run all UFs
# ------------------------------------------------------------------------- #

ufs <- c(
  "AC","AL","AP","AM","BA","CE","DF","ES","GO",
  "MA","MT","MS","MG","PA","PB","PR","PE","PI",
  "RJ","RN","RS","RO","RR","SC","SP","SE","TO"
)

base_path <- "Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/SIOPE"

fundef_2006_uf <- map_dfr(
  ufs,
  process_siope_uf,
  base_path = base_path
)

# ------------------------------------------------------------------------- #
# Inspect residuals
# ------------------------------------------------------------------------- #

fundef_2006_uf %>%
  arrange(federal_complement_inferred) %>%
  print(n = Inf)

fundef_2006_uf %>%
  filter(federal_complement_problem) %>%
  arrange(federal_complement_inferred)



# ========================================================================= #
# FUNDEF 2006 - reconstruct and validate UF-level accounting from SIOPE
# ========================================================================= #
#
# Objective of this block:
#   Check whether SIOPE 2006 can reproduce the accounting value of each
#   state-level FUNDEF fund before using the same logic to build the 2007
#   counterfactual.
#
# Accounting concepts used below:
#   1) Contribution side:
#      resources retained from the legal FUNDEF tax basket. These are the
#      municipal and state "deducao ... fundef 15 percent" SIOPE lines.
#
#   2) Federal complement:
#      explicit "transferencias_da_complementacao_da_uniao_ao_fundef". This is
#      federal money added to selected state funds. It is not inferred by forcing
#      residuals to zero; it is read directly when SIOPE reports it.
#
#   3) Receipt/distribution side:
#      "transferencias_de_recursos_do_fundef" is the amount received from the
#      fund by state and municipal governments. At the UF level, this should be
#      close to the resources available in the fund, but SIOPE reporting may
#      differ slightly across UFs.
#
#   4) Financial revenue:
#      "rec_de_remuneracao_de_depositos_bancarios_fundef" is income generated by
#      FUNDEF bank balances. It is part of resources available to education, but
#      it is not federal complement.
#
# Main validation checks:
#   A) Does the receipt side match the contribution side plus explicit federal
#      complement and financial revenue?
#   B) If not, how large is the residual as a share of the contribution side?
#   C) Are residuals concentrated in a few municipalities, or are they a state-
#      side reporting/accounting issue?

mun_contrib_cols <- c(
  "deducao_do_fpm_para_o_fundef_15_percent",
  "deducao_de_lc_87_96_para_o_fundef_15_percent",
  "deducao_do_icms_para_o_fundef_15_percent",
  "deducao_do_ipi_exportacao_para_o_fundef_15_percent"
)

state_contrib_cols <- c(
  "deducao_do_icms_para_a_formacao_do_fundef_15_percent",
  "deducao_da_cota_parte_do_fpe_para_formacao_do_fundef_15_percent",
  "deducao_da_cota_parte_do_ipi_exportacao_para_formacao_do_fundef_15_percent"
)

receipt_pattern <- paste(
  c(
    "^transferencias_de_recursos_do_fundef$",
    "^transferencias_da_complementacao_da_uniao_ao_fundef$"
  ),
  collapse = "|"
)

process_siope_uf_2006 <- function(uf, base_path) {
  
  file_path <- file.path(base_path, paste0("siope_", uf, "_2006_P1.csv"))
  siope <- read.csv(file_path)
  
  # Pivot SIOPE item labels into columns. Keep only "Receitas Realizadas",
  # because the reconstruction must use executed values, not budgeted values.
  test1_full <- siope %>%
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
  
  has_municipal <- any(test1_full$tipo == "Municipal")
  
  # ----------------------------------------------------------------------- #
  # DF (and any UF with no Municipal rows): combine everything into one row
  # ----------------------------------------------------------------------- #
  if (!has_municipal) {
    
    combined <- test1_full %>% filter(tipo == "Estadual")
    
    # DF has no municipal SIOPE layer comparable to other UFs. We still keep
    # both municipal-basket and state-basket deductions if SIOPE reports them.
    mun_contribution   <- sum(safe_sum_cols(combined, mun_contrib_cols))
    state_contribution <- sum(safe_sum_cols(combined, state_contrib_cols))
    total_received     <- sum(sum_matching_cols(combined, receipt_pattern))
    
    return(tibble(
      cod_uf             = combined$cod_uf[1],
      sig_uf             = uf,
      mun_received       = NA_real_,   # not meaningful for DF
      mun_contribution   = mun_contribution,
      state_contribution = state_contribution,
      state_received     = NA_real_,   # not meaningful for DF
      total_received     = total_received,
      total_contribution = mun_contribution + state_contribution,
      federal_complement_inferred = total_received - (mun_contribution + state_contribution),
      federal_complement_problem = total_received < (mun_contribution + state_contribution),
      uf                 = uf
    ))
  }
  
  # ----------------------------------------------------------------------- #
  # Normal UFs: sum municipal deductions/receipts and state deductions/receipts
  # separately. The split is useful because FUNDEF redistributes resources
  # inside each UF, often from the state government to municipal governments.
  # ----------------------------------------------------------------------- #
  municipios <- test1_full %>%
    filter(tipo == "Municipal") %>%
    mutate(
      mun_contribution = safe_sum_cols(pick(everything()), mun_contrib_cols),
      mun_received     = sum_matching_cols(pick(everything()), receipt_pattern)
    )
  
  mun_uf <- municipios %>%
    summarise(
      mun_received     = sum(mun_received, na.rm = TRUE),
      mun_contribution = sum(mun_contribution, na.rm = TRUE)
    )
  
  estado <- test1_full %>%
    filter(tipo == "Estadual") %>%
    mutate(
      state_contribution = safe_sum_cols(pick(everything()), state_contrib_cols),
      state_received     = sum_matching_cols(pick(everything()), receipt_pattern)
    ) %>%
    summarise(
      state_contribution = sum(state_contribution, na.rm = TRUE),
      state_received     = sum(state_received, na.rm = TRUE)
    )
  
  bind_cols(mun_uf, estado) %>%
    mutate(
      cod_uf             = test1_full$cod_uf[1],
      sig_uf             = uf,
      total_received     = mun_received + state_received,
      total_contribution = mun_contribution + state_contribution,
      # This is a raw residual, not the official federal complement. A negative
      # value indicates an accounting mismatch to inspect, not a value to clamp.
      federal_complement_inferred = total_received - total_contribution,
      federal_complement_problem = total_received < total_contribution,
      uf                 = uf
    )
}

ufs_2006 <- c(
  "AC","AL","AP","AM","BA","CE","DF","ES","GO",
  "MA","MT","MS","MG","PA","PB","PR","PE","PI",
  "RJ","RN","RS","RO","RR","SC","SP","SE","TO"
)

base_path <- "Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/SIOPE"

fundef_2006_uf <- map_dfr(ufs_2006, process_siope_uf_2006, base_path = base_path)

fundef_2006_uf %>% arrange(sig_uf) %>% print(n = Inf)

fundef_2006_diagnostics <- fundef_2006_uf %>%
  mutate(
    receipt_minus_contribution = total_received - total_contribution,
    receipt_contribution_ratio = total_received / total_contribution,
    negative_inferred_complement =
      federal_complement_inferred < -1e-6
  ) %>%
  arrange(receipt_minus_contribution)

fundef_2006_diagnostics %>%
  filter(negative_inferred_complement) %>%
  print(n = Inf)

inspect_siope_2006_columns <- function(uf, base_path) {
  
  file_path <- file.path(base_path, paste0("siope_", uf, "_2006_P1.csv"))
  
  siope_rr <- read.csv(file_path) %>%
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
  
  tibble(
    uf = uf,
    type = c("deduction", "receipt", "all_fundef"),
    columns = c(
      paste(names(siope_rr)[str_detect(names(siope_rr), "deducao")], collapse = " | "),
      paste(names(siope_rr)[str_detect(names(siope_rr), "transfer|complement|uniao|deposit|fundef")], collapse = " | "),
      paste(names(siope_rr)[str_detect(names(siope_rr), "fundef")], collapse = " | ")
    )
  )
}

inspect_siope_2006_components <- function(uf, base_path) {
  
  file_path <- file.path(base_path, paste0("siope_", uf, "_2006_P1.csv"))
  
  siope_rr <- read.csv(file_path) %>%
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
  
  component_cols <- c(
    mun_contrib_cols,
    state_contrib_cols,
    "transferencias_de_recursos_do_fundef",
    "transferencias_da_complementacao_da_uniao_ao_fundef",
    "rec_de_remuneracao_de_depositos_bancarios_fundef"
  )
  
  component_cols_present <- intersect(component_cols, names(siope_rr))
  
  siope_rr %>%
    group_by(tipo) %>%
    summarise(
      across(
        all_of(component_cols_present),
        ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    mutate(uf = uf, .before = 1)
}

inspect_siope_2006_periods <- function(uf, base_path) {
  
  file_path <- file.path(base_path, paste0("siope_", uf, "_2006_P1.csv"))
  
  read.csv(file_path) %>%
    mutate(NOM_MUNI = ifelse(NOM_MUNI == "null", "Estadual", NOM_MUNI)) %>%
    clean_names() %>%
    count(tipo, num_peri, nom_colu, name = "raw_rows") %>%
    mutate(uf = uf, .before = 1)
}

inspect_siope_2006_municipal_balance <- function(uf, base_path) {
  
  file_path <- file.path(base_path, paste0("siope_", uf, "_2006_P1.csv"))
  
  siope_rr <- read.csv(file_path) %>%
    mutate(NOM_MUNI = ifelse(NOM_MUNI == "null", "Estadual", NOM_MUNI)) %>%
    pivot_wider(
      id_cols     = c(TIPO, NUM_ANO, NUM_PERI, COD_UF, SIG_UF, COD_MUNI, NOM_MUNI, NOM_COLU),
      names_from  = NOM_ITEM,
      values_from = VAL_DECL,
      values_fn   = sum,
      values_fill = 0
    ) %>%
    clean_names() %>%
    filter(nom_colu == "Receitas Realizadas", tipo == "Municipal") %>%
    mutate(
      mun_contribution = safe_sum_cols(pick(everything()), mun_contrib_cols),
      transfer_fundef = safe_sum_cols(
        pick(everything()),
        "transferencias_de_recursos_do_fundef"
      ),
      complement_uniao = safe_sum_cols(
        pick(everything()),
        "transferencias_da_complementacao_da_uniao_ao_fundef"
      ),
      remun_bancaria_fundef = safe_sum_cols(
        pick(everything()),
        "rec_de_remuneracao_de_depositos_bancarios_fundef"
      ),
      mun_received_core = transfer_fundef + complement_uniao,
      mun_received_with_remun = mun_received_core + remun_bancaria_fundef,
      mun_gap_core = mun_received_core - mun_contribution,
      mun_gap_with_remun = mun_received_with_remun - mun_contribution,
      uf = uf
    ) %>%
    select(
      uf, cod_uf, sig_uf, cod_muni, nom_muni,
      mun_contribution,
      transfer_fundef,
      complement_uniao,
      remun_bancaria_fundef,
      mun_received_core,
      mun_received_with_remun,
      mun_gap_core,
      mun_gap_with_remun
    )
  
  siope_rr
}

fundef_2006_column_diagnostics <- map_dfr(
  ufs_2006,
  inspect_siope_2006_columns,
  base_path = base_path
)

fundef_2006_component_diagnostics <- map_dfr(
  ufs_2006,
  inspect_siope_2006_components,
  base_path = base_path
)

fundef_2006_final_diagnostics <- fundef_2006_component_diagnostics %>%
  mutate(
    # Rebuild the contribution side from the exact legal FUNDEF basket in SIOPE.
    mun_contribution_component =
      coalesce(deducao_do_fpm_para_o_fundef_15_percent, 0) +
      coalesce(deducao_de_lc_87_96_para_o_fundef_15_percent, 0) +
      coalesce(deducao_do_icms_para_o_fundef_15_percent, 0) +
      coalesce(deducao_do_ipi_exportacao_para_o_fundef_15_percent, 0),
    state_contribution_component =
      coalesce(deducao_do_icms_para_a_formacao_do_fundef_15_percent, 0) +
      coalesce(deducao_da_cota_parte_do_fpe_para_formacao_do_fundef_15_percent, 0) +
      coalesce(deducao_da_cota_parte_do_ipi_exportacao_para_formacao_do_fundef_15_percent, 0),
    contribution_component = if_else(
      tipo == "Municipal",
      mun_contribution_component,
      state_contribution_component
    ),
    # Keep these three receipt-side concepts separate:
    #   transfer_fundef: redistribution/payment from the FUNDEF account.
    #   complement_uniao: explicit federal complement, if reported.
    #   remun_bancaria_fundef: financial income from FUNDEF balances.
    transfer_fundef = coalesce(transferencias_de_recursos_do_fundef, 0),
    complement_uniao = coalesce(transferencias_da_complementacao_da_uniao_ao_fundef, 0),
    remun_bancaria_fundef = coalesce(rec_de_remuneracao_de_depositos_bancarios_fundef, 0),
    received_core = transfer_fundef + complement_uniao,
    received_with_remun = received_core + remun_bancaria_fundef
  ) %>%
  group_by(uf) %>%
  summarise(
    mun_contribution = sum(contribution_component[tipo == "Municipal"], na.rm = TRUE),
    state_contribution = sum(contribution_component[tipo == "Estadual"], na.rm = TRUE),
    total_contribution = sum(contribution_component, na.rm = TRUE),
    transfer_fundef = sum(transfer_fundef, na.rm = TRUE),
    complement_uniao = sum(complement_uniao, na.rm = TRUE),
    remun_bancaria_fundef = sum(remun_bancaria_fundef, na.rm = TRUE),
    total_received_core = sum(received_core, na.rm = TRUE),
    total_received_with_remun = sum(received_with_remun, na.rm = TRUE),
    
    # Raw distribution balance:
    #   positive values mean SIOPE receipts/distributions exceed deductions;
    #   negative values mean deductions exceed reported receipts.
    residual_core = total_received_core - total_contribution,
    residual_core_pct = residual_core / total_contribution,
    residual_with_remun = total_received_with_remun - total_contribution,
    residual_with_remun_pct = residual_with_remun / total_contribution,
    ratio_core = total_received_core / total_contribution,
    ratio_with_remun = total_received_with_remun / total_contribution,
    
    # Reconciliation check for SIOPE viability:
    #   If transfer_fundef excludes federal complement and financial revenue,
    #   then total available resources should be approximately:
    #     total_contribution + complement_uniao + remun_bancaria_fundef.
    #   We compare the observed receipt side against that expected amount.
    expected_resources_with_remun =
      total_contribution + complement_uniao + remun_bancaria_fundef,
    observed_resources_with_remun =
      transfer_fundef + complement_uniao + remun_bancaria_fundef,
    reconstruction_error =
      observed_resources_with_remun - expected_resources_with_remun,
    reconstruction_error_pct =
      reconstruction_error / expected_resources_with_remun,
    direct_balance_error =
      total_received_with_remun - total_contribution,
    direct_balance_error_pct =
      direct_balance_error / total_contribution,
    
    # This flag is intentionally descriptive rather than corrective. It tells
    # us whether the SIOPE reconstruction is close enough to carry forward with
    # caution. The 1 percent threshold is a transparent audit threshold, not a
    # legal rule.
    reconstruction_within_1pct =
      abs(reconstruction_error_pct) <= 0.01,
    direct_balance_within_1pct =
      abs(direct_balance_error_pct) <= 0.01,
    .groups = "drop"
  ) %>%
  arrange(reconstruction_error_pct)

fundef_2006_reconstruction_check <- fundef_2006_final_diagnostics %>%
  select(
    uf,
    total_contribution,
    transfer_fundef,
    complement_uniao,
    remun_bancaria_fundef,
    expected_resources_with_remun,
    observed_resources_with_remun,
    reconstruction_error,
    reconstruction_error_pct,
    reconstruction_within_1pct,
    direct_balance_error,
    direct_balance_error_pct,
    direct_balance_within_1pct,
    residual_with_remun,
    residual_with_remun_pct
  ) %>%
  arrange(reconstruction_error_pct)

fundef_2006_period_diagnostics <- map_dfr(
  ufs_2006,
  inspect_siope_2006_periods,
  base_path = base_path
)

ufs_2006_problem <- fundef_2006_diagnostics %>%
  filter(negative_inferred_complement) %>%
  pull(uf)

fundef_2006_problem_municipal_balance <- map_dfr(
  ufs_2006_problem,
  inspect_siope_2006_municipal_balance,
  base_path = base_path
) %>%
  arrange(uf, mun_gap_with_remun)

fundef_2006_column_diagnostics %>% print(n = Inf)

fundef_2006_problem_columns <- fundef_2006_diagnostics %>%
  filter(negative_inferred_complement) %>%
  select(uf) %>%
  left_join(fundef_2006_column_diagnostics, by = "uf")

fundef_2006_problem_columns %>% print(n = Inf)

local_output_path <- "C:/Users/tuffyli/Documents/GitHub/Fundef-Fundeb/outputs"
dir.create(local_output_path, showWarnings = FALSE, recursive = TRUE)

write.csv2(
  fundef_2006_diagnostics,
  file.path(local_output_path, "fundef_2006_diagnostics.csv"),
  row.names = FALSE
)

write.csv2(
  fundef_2006_column_diagnostics,
  file.path(local_output_path, "fundef_2006_column_diagnostics.csv"),
  row.names = FALSE
)

write.csv2(
  fundef_2006_problem_columns,
  file.path(local_output_path, "fundef_2006_problem_columns.csv"),
  row.names = FALSE
)

write.csv2(
  fundef_2006_component_diagnostics,
  file.path(local_output_path, "fundef_2006_component_diagnostics.csv"),
  row.names = FALSE
)

write.csv2(
  fundef_2006_final_diagnostics,
  file.path(local_output_path, "fundef_2006_final_diagnostics.csv"),
  row.names = FALSE
)

write.csv2(
  fundef_2006_reconstruction_check,
  file.path(local_output_path, "fundef_2006_reconstruction_check.csv"),
  row.names = FALSE
)

write.csv2(
  fundef_2006_period_diagnostics,
  file.path(local_output_path, "fundef_2006_period_diagnostics.csv"),
  row.names = FALSE
)

write.csv2(
  fundef_2006_problem_municipal_balance,
  file.path(local_output_path, "fundef_2006_problem_municipal_balance.csv"),
  row.names = FALSE
)




fundef_2006_uf <- fundef_2006_uf %>%
  left_join(
    fundef_2006_final_diagnostics %>%
      select(uf, remun_bancaria_fundef),
    by = c("sig_uf" = "uf")
  ) %>%
  mutate(
    total_received_with_remun =
      total_received + remun_bancaria_fundef,
    
    federal_complement_with_remun =
      total_received_with_remun - total_contribution
  )

fundef_2006_uf <- fundef_2006_uf %>% 
  select(sig_uf, cod_uf, everything(), -uf) %>% 
  mutate(prop_received = total)

rm(fundef_2006_column_diagnostics, fundef_2006_component_diagnostics, fundef_2006_diagnostics,
   fundef_2006_final_diagnostics, fundef_2006_period_diagnostics, fundef_2006_problem_columns,
   fundef_2006_problem_municipal_balance, fundef_2006_reconstruction_check, fundef_2006_summary)


# ---------------------------------------------------------------------------- #
# 3. SIOPE (2007): counterfactual FUNDEF using FUNDEB revenue data ----
# ---------------------------------------------------------------------------- #

# Goal:
#   Rebuild what each UF would have contributed to FUNDEF in 2007 if FUNDEB had
#   not replaced it. FUNDEF used 15% of the old basket:
#   FPM/FPE, ICMS, IPI-exportacao, and LC 87/96 (Lei Kandir).
#
# In 2007 SIOPE/FUNDEB files, use the gross "100 percent" revenue items wherever
# available, not the FUNDEB deductions. This avoids applying the FUNDEB transition
# rate to the counterfactual directly.

# ------------------------------------------------------------------------- #
## 3.1 Helpers ----
# ------------------------------------------------------------------------- #

sum_required_cols <- function(df, cols, context = "data") {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(
      "Missing required columns in ", context, ":\n",
      paste(missing, collapse = "\n"),
      call. = FALSE
    )
  }
  
  rowSums(dplyr::select(df, all_of(cols)), na.rm = TRUE)
}

resolve_one_col <- function(df, candidates, context) {
  present <- candidates[candidates %in% names(df)]
  
  if (length(present) == 1) {
    return(present)
  }
  
  if (length(present) > 1) {
    stop(
      "More than one candidate column matched for ", context, ":\n",
      paste(present, collapse = "\n"),
      call. = FALSE
    )
  }
  
  stop(
    "No candidate column matched for ", context, ". Tried:\n",
    paste(candidates, collapse = "\n"),
    call. = FALSE
  )
}

read_siope_receitas_realizadas <- function(file_path) {
  siope <- read.csv(file_path)
  
  if ("IDN_CLAS" %in% names(siope)) {
    siope <- siope %>% filter(IDN_CLAS == "RR")
  }
  
  siope %>%
    mutate(NOM_MUNI = ifelse(NOM_MUNI == "null", "Estadual", NOM_MUNI)) %>%
    pivot_wider(
      id_cols = c(
        TIPO, NUM_ANO, NUM_PERI,
        COD_UF, SIG_UF,
        COD_MUNI, NOM_MUNI,
        NOM_COLU
      ),
      names_from  = NOM_ITEM,
      values_from = VAL_DECL,
      values_fn   = sum,
      values_fill = 0
    ) %>%
    clean_names() %>%
    filter(nom_colu == "Receitas Realizadas")
}

# ------------------------------------------------------------------------- #
## 3.2 FUNDEF basket columns in 2007 SIOPE/FUNDEB files (corrected) ----
# ------------------------------------------------------------------------- #

municipal_basket_candidates <- list(
  fpm = c(
    "cota_parte_do_fundo_de_participacao_dos_municipios_fpm",
    "cota_parte_do_fundo_de_participacao_dos_municipios_fpm_100_percent"
  ),
  icms = c(
    "cota_parte_do_icms_100_percent",
    "cota_parte_do_icms"
  ),
  ipi_exportacao = c(
    "cota_parte_do_ipi_sobre_exportacao_100_percent",
    "cota_parte_do_ipi_sobre_exportacao"
  ),
  lc_87_96 = c(
    "transferencia_financeira_icms_desoneracao_l_c_no_87_96_100_percent",
    "transferencia_financeira_icms_desoneracao_l_c_87_96_100_percent",
    "transferencia_financeira_icms_desoneracao_lc_no_87_96_100_percent",
    "transferencia_financeira_icms_desoneracao_lc_87_96_100_percent"
  )
)

# State-side GROSS columns (still needed as a starting point, but we will
# subtract the municipal quota from icms / ipi_exportacao / lc_87_96 below)
state_basket_candidates <- list(
  fpe = c(
    "cota_parte_do_fundo_de_participacao_dos_estados_e_do_df_fpe_100_percent",
    "cota_parte_do_fundo_de_participacao_dos_estados_e_do_df_fpe"
  ),
  icms = c(
    "imposto_sobre_operacoes_relativas_a_circulacao_de_mercadorias_e_prestacao_de_servicos_icms_100_percent",
    "imposto_sobre_operacoes_relativas_a_circulacao_de_mercadorias_e_prestacao_de_servicos_icms"
  ),
  ipi_exportacao = c(
    "cota_parte_do_ipi_dos_estados_exportadores_ipi_exportacao_100_percent",
    "cota_parte_do_ipi_dos_estados_exportadores_ipi_exportacao"
  ),
  lc_87_96 = c(
    "transferencia_financeira_icms_desoneracao_l_c_no_87_96_100_percent",
    "transferencia_financeira_icms_desoneracao_l_c_87_96_100_percent",
    "transferencia_financeira_icms_desoneracao_lc_no_87_96_100_percent",
    "transferencia_financeira_icms_desoneracao_lc_87_96_100_percent"
  )
)

# Items where the state's "100%" gross figure already includes the
# municipal quota, and must therefore be netted out
shared_split_items <- c("icms", "ipi_exportacao", "lc_87_96")

# ------------------------------------------------------------------------- #
## 3.3 Rebuild 2007 counterfactual contributions by UF (corrected) ----
# ------------------------------------------------------------------------- #

process_siope_uf_2007 <- function(uf, base_path) {
  
  file_path <- file.path(base_path, paste0("siope_", uf, "_2007_P1.csv"))
  siope_rr <- read_siope_receitas_realizadas(file_path)
  
  municipal_rows <- siope_rr %>% filter(tipo == "Municipal")
  state_rows     <- siope_rr %>% filter(tipo == "Estadual")
  
  has_municipal <- nrow(municipal_rows) > 0
  
  if (has_municipal) {
    mun_base_cols <- resolve_basket_cols(
      municipal_rows, municipal_basket_candidates, paste0(uf, " municipal")
    )
    
    mun_cf <- municipal_rows %>%
      mutate(
        base_municipal = sum_required_cols(
          pick(everything()), mun_base_cols, paste0(uf, " municipal FUNDEF basket")
        ),
        contrib_municipal_cf = 0.15 * base_municipal
      ) %>%
      select(cod_uf, sig_uf, cod_muni, nom_muni, base_municipal, contrib_municipal_cf)
    
    mun_uf <- mun_cf %>%
      group_by(cod_uf, sig_uf) %>%
      summarise(
        mun_base       = sum(base_municipal, na.rm = TRUE),
        mun_contrib_cf = sum(contrib_municipal_cf, na.rm = TRUE),
        .groups = "drop"
      )
    
    mun_item_totals <- map_dfc(
      shared_split_items,
      function(item) {
        col <- resolve_one_col(
          municipal_rows, municipal_basket_candidates[[item]],
          paste0(uf, " municipal ", item)
        )
        tibble(!!paste0("mun_", item, "_total") := sum(municipal_rows[[col]], na.rm = TRUE))
      }
    )
  } else {
    mun_cf <- tibble()
    mun_uf <- tibble(cod_uf = state_rows$cod_uf[1], sig_uf = uf, mun_base = 0, mun_contrib_cf = 0)
    mun_item_totals <- tibble(
      mun_icms_total = 0, mun_ipi_exportacao_total = 0, mun_lc_87_96_total = 0
    )
  }
  
  state_gross <- state_rows %>%
    mutate(
      fpe_gross            = .data[[resolve_one_col(state_rows, state_basket_candidates$fpe, paste0(uf, " state fpe"))]],
      icms_gross           = .data[[resolve_one_col(state_rows, state_basket_candidates$icms, paste0(uf, " state icms"))]],
      ipi_exportacao_gross = .data[[resolve_one_col(state_rows, state_basket_candidates$ipi_exportacao, paste0(uf, " state ipi_exportacao"))]],
      lc_87_96_gross       = .data[[resolve_one_col(state_rows, state_basket_candidates$lc_87_96, paste0(uf, " state lc_87_96"))]]
    ) %>%
    summarise(
      fpe_gross            = sum(fpe_gross, na.rm = TRUE),
      icms_gross           = sum(icms_gross, na.rm = TRUE),
      ipi_exportacao_gross = sum(ipi_exportacao_gross, na.rm = TRUE),
      lc_87_96_gross       = sum(lc_87_96_gross, na.rm = TRUE)
    )
  
  state_retained <- state_gross %>%
    mutate(
      icms_retained           = icms_gross           - mun_item_totals$mun_icms_total,
      ipi_exportacao_retained = ipi_exportacao_gross - mun_item_totals$mun_ipi_exportacao_total,
      lc_87_96_retained       = lc_87_96_gross       - mun_item_totals$mun_lc_87_96_total,
      base_estadual = fpe_gross + icms_retained + ipi_exportacao_retained + lc_87_96_retained,
      contrib_estadual_cf = 0.15 * base_estadual
    )
  
  state_cf <- state_retained %>%
    transmute(cod_uf = state_rows$cod_uf[1], sig_uf = uf, base_estadual, contrib_estadual_cf)
  
  state_uf <- state_cf %>%
    group_by(cod_uf, sig_uf) %>%
    summarise(
      state_base       = sum(base_estadual, na.rm = TRUE),
      state_contrib_cf = sum(contrib_estadual_cf, na.rm = TRUE),
      .groups = "drop"
    )
  
  uf_cf <- mun_uf %>%
    full_join(state_uf, by = c("cod_uf", "sig_uf")) %>%
    mutate(
      across(c(mun_base, mun_contrib_cf, state_base, state_contrib_cf), ~ replace_na(.x, 0)),
      total_base_cf    = mun_base + state_base,
      total_contrib_cf = mun_contrib_cf + state_contrib_cf,
      uf = sig_uf
    ) %>%
    arrange(sig_uf)
  
  # sanity check must be BEFORE the return
  print(
    tibble(
      uf = uf,
      icms_gross = state_gross$icms_gross,
      mun_icms = mun_item_totals$mun_icms_total,
      icms_retained = state_retained$icms_retained,
      ipi_gross = state_gross$ipi_exportacao_gross,
      mun_ipi = mun_item_totals$mun_ipi_exportacao_total,
      ipi_retained = state_retained$ipi_exportacao_retained,
      lc87_gross = state_gross$lc_87_96_gross,
      mun_lc87 = mun_item_totals$mun_lc_87_96_total,
      lc87_retained = state_retained$lc_87_96_retained
    )
  )
  
  list(municipal = mun_cf, state = state_cf, uf = uf_cf)
}
# Opening all ufs

res_2007 <- map(ufs_2007, ~ process_siope_uf_2007(.x, base_path))

fundef_2007_municipal_cf <- map_dfr(res_2007, "municipal")
fundef_2007_state_cf     <- map_dfr(res_2007, "state")
fundef_2007_uf_cf        <- map_dfr(res_2007, "uf")

# ---------------------------------------------------------------------------- #
## 3.4 Deflate ----
# ---------------------------------------------------------------------------- #

#Para deflacionar os valores:
df_ipca <- read_excel("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/IPCA_acumulado_ano.xlsx", skip = 3)
colnames(df_ipca) = c("ano", "ipca")

df_ipca <- df_ipca %>% 
  mutate(ano = as.numeric(ano)) %>% 
  filter(ano %in% 2000:2021) %>%
  arrange(ano)


df_ipca <- df_ipca %>% 
  mutate(
    ipca = as.numeric(gsub(",",".",ipca)),
    indice = cumprod(1 + as.numeric(ipca)/100)) %>% 
  mutate(indice = indice / indice[ano == 2006])



# Sanity check for taxes growth
fundef_2007_uf_cf %>%
  mutate(total_contrib_cf = total_contrib_cf/df_ipca$indice[df_ipca$ano == 2007]) %>% 
  left_join(fundef_2006_uf %>% select(sig_uf, total_contribution), by = "sig_uf") %>%
  mutate(growth_2006_to_2007 = total_contrib_cf / total_contribution - 1) %>%
  select(sig_uf, total_contribution, total_contrib_cf, growth_2006_to_2007) %>%
  arrange(sig_uf) %>%
  print(n = Inf)


# ---------------------------------------------------------------------------- #
# 4. Per-Student Adjustment ----
# ---------------------------------------------------------------------------- #
## 4.1 Per-Student (2006) ----
# ---------------------------------------------------------------------------- #

fundef_2006 <- fundef_2006_uf %>% 
  left_join(mat_uf, by = c("sig_uf" = "uf"))


fundef_2006 <- fundef_2006 %>% 
  group_by(sig_uf) %>% 
  mutate(
    urb_per_stu = 
      total_received_with_remun /
      (urb_ini + 1.02*rur_ini + 1.05*urb_fim + 1.05*1.019*rur_fim_esp)
  )

# ---------------------------------------------------------------------------- #
## 4.2 Per-Student (2005) ----
# ---------------------------------------------------------------------------- #
### 4.2.1 Censo (2004) ----
# ---------------------------------------------------------------------------- #

censo <- read_delim("Z:/Arquivos IFB/Censo Escolar/Bases Agregadas/2004/microdados_educação_básica_2004/DADOS/CENSOESC_2004.CSV", delim = "|",
                    col_types = cols(.default = col_double(),
                                     CODFUNC = col_character(),
                                     MASCARA = col_character(),
                                     DEP     = col_character(),
                                     LOC = col_character(),
                                     UF = col_character(),
                                     SIGLA = col_character(),
                                     MUNIC = col_character(),
                                     ED_INDIG = col_character(),
                                     MAT_ETNI = col_character(),
                                     NIVELCRE = col_character(),
                                     NIVELPRE = col_character(),
                                     NIV_F1A4_8 = col_character(),
                                     NIV_F5A8_8 = col_character(),
                                     NIV_F9INI = col_character(),
                                     NIV_F9FIM = col_character(),
                                     NIVELMED = col_character(),
                                     SUPL_AVA = col_character(),
                                     SUPL_SAVA = col_character(),
                                     EDPROFIS = col_character(),
                                     ESP_EXCL = col_character(),
                                     ESP_T_ES = col_character(),
                                     AREA_QUIL = col_character(),
                                     ESP_S_RE = col_character(),
                                     ESP_A_IN = col_character(),
                                     ED_INDIG = col_character(),
                                     ED_IN_LM = col_character(),
                                     COD_ID_IND = col_character(),
                                     ED_IN_LP = col_character(),
                                     MAT_ETNI = col_character(),
                                     ESC_T_IN = col_character(),
                                     PRED_ESC = col_character(),
                                     TEMPLO = col_character(),
                                     DEF11C = col_double(),
                                     DEF11D = col_double(),
                                     DEF11E = col_double(),
                                     DEF11F = col_double(),
                                     NEF11C = col_double(),
                                     NEF11D = col_double(),
                                     NEF11E = col_double(),
                                     NEF11F = col_double(),
                                     DEF11G = col_double(),
                                     DEF11H = col_double(),
                                     DEF11I = col_double(),
                                     DEF11J = col_double(),
                                     NEF11G = col_double(),
                                     NEF11H = col_double(),
                                     NEF11I = col_double(),
                                     NEF11J = col_double()))



mat_2004 <- censo %>%
  select(c(1:9,
           
           CODFUNC,
           ED_INDIG,
           #MAT_QUIL,
           AREA_QUIL,
           #ESC_ASSE,
           ED_IN_LM,
           ED_IN_LP,
           #MAT_ETNI,
           ESC_T_IN,
           
           DEF11C:DEF11F, NEF11C:NEF11F, # EF iniciais (8 anos)
           DE9F11C:DE9F11G, NE9F11C:NE9F11G, # EF iniciais (9 anos)
           DEF11G:DEF11J, NEF11G:NEF11J, # EF finais (8 anos)
           DE9F11H:DE9F11N, NE9F11H:NE9F11N, # EF finais (9 anos)

           #ED especial por série:
           VEE1611:VEE1691, VEE1711:VEE1791, VEE1811:VEE1891, VEE1911:VEE1991, # 1ºEF
           
           VEE1612:VEE1692, VEE1712:VEE1792, VEE1812:VEE1892, VEE1912:VEE1992, # 2ºEF,
           VEE1613:VEE1693, VEE1713:VEE1793, VEE1813:VEE1893, VEE1913:VEE1993, # 3ºEF
           VEE1614:VEE1694, VEE1714:VEE1794, VEE1814:VEE1894, VEE1914:VEE1994, # 4ºEF
           VEE1615:VEE1695, VEE1715:VEE1795, VEE1815:VEE1895, VEE1915:VEE1995, # 5ºEF
           VEE1616:VEE1696, VEE1716:VEE1796, VEE1816:VEE1896, VEE1916:VEE1996, # 6ºEF
           VEE1617:VEE1697, VEE1717:VEE1797, VEE1817:VEE1897, VEE1917:VEE1997, # 7ºEF
           VEE1618:VEE1698, VEE1718:VEE1798, VEE1818:VEE1898, VEE1918:VEE1998 # 8ºEF
  )) %>%
  
  mutate(COD_UF = as.numeric(str_sub(as.character(CODMUNIC, 1, 2)))) %>%
  mutate(reg_in = rowSums(across(c(DEF11C:NE9F11G)), na.rm = TRUE), # Contando EF de 8 e 9 anos!
         reg_fin = rowSums(across(c(DEF11G:NE9F11N)), na.rm = TRUE),
         esp_iniciais = rowSums(across(c(VEE1611:VEE1994)), na.rm = TRUE),
         esp_finais = rowSums(across(c(VEE1615:VEE1998)), na.rm = TRUE),
         esp_soma = esp_iniciais + esp_finais
         ) %>%  # pega o maior valor entre a soma das deficiencias por série,
  # e da desagregação pelo ano de nascimento
  filter(!(DEP %in% c("Particular", "Federal")), # tira escolas federais e particulares
         CODFUNC == "Ativo") %>%  # tira escolas que não estão ativas
  mutate(CODMUNIC = as.numeric(str_c( #concatena
    str_sub(as.character(CODMUNIC), 1, 2), #pega os dois primeiros
    str_sub(as.character(CODMUNIC), -5) #pega os últimos 5
  )))


# ---------------------------------------------------------------------------- #
#### 4.2.1.1 Saving -----
# ---------------------------------------------------------------------------- #

# DF do censo já agregada por escola (é muito pesada para rodar toda vez):
write.csv2(mat_2004, file = "Z:/Tuffy/Paper - Educ/Dados/censo_2004_filtrado.csv")

mat_2004 <- read.csv2("Z:/Tuffy/Paper - Educ/Dados/censo_2004_filtrado.csv")


# ---------------------------------------------------------------------------- #
#### 4.2.1.2 Students per Mun. ----
# ---------------------------------------------------------------------------- #

mat_2004_munic <- mat_2004 %>%
  group_by(CODMUNIC) %>%
  summarise(
    nome = first(MUNIC),
    no_uf = first(UF),
    uf = first(SIGLA),
    
    urb_ini = sum(if_else(LOC == "Urbana", reg_in, 0), na.rm = TRUE),
    urb_fim = sum(if_else(LOC == "Urbana", reg_fin, 0), na.rm = TRUE),
    rur_ini = sum(if_else(LOC == "Rural",  reg_in, 0), na.rm = TRUE),
    rur_fim_esp = sum(if_else(LOC == "Rural", reg_fin + esp_soma, 0), na.rm = TRUE),
    
    .groups = "drop"
  )


# ---------------------------------------------------------------------------- #
#### 4.2.1.3 Students UF (categories) ----
# ---------------------------------------------------------------------------- #

mat_uf_2004 <- mat_2004_munic %>% 
  group_by(uf) %>% 
  summarise(
    urb_ini = sum(urb_ini, na.rm = T),
    urb_fim = sum(urb_fim, na.rm = T),
    rur_ini = sum(rur_ini, na.rm = T),
    rur_fim_esp = sum(rur_fim_esp, na.rm = T)
  ) %>% 
  mutate(total = urb_ini + urb_fim + rur_ini + rur_fim_esp) %>% 
  ungroup()



rm(mat_2004, mat_uf, mat_2004_munic, censo)


# ---------------------------------------------------------------------------- #
### 4.2.2 SIOPE (2005) ----
# ---------------------------------------------------------------------------- #
#
# Goal: extract total FUNDEF received per UF in 2005 to compute
#       per-student investment and replicate the 2005->2006 growth
#       rate into the 2006->2007 counterfactual.
#
# FUNDEF receipt candidates (2005 files may use slightly different labels):
#   - "transferencias_de_recursos_do_fundef"      (core redistribution)
#   - "transferencias_da_complementacao_da_uniao_ao_fundef" (federal complement)
#   - "rec_de_remuneracao_de_depositos_bancarios_fundef"    (bank interest)
#
# The function is self-detecting: it checks which receipt columns exist
# and warns on any unexpected structure, so you get a clear error message
# rather than a silent zero if the 2005 label differs.

# Receipt column candidates (ordered by specificity)
fundef_receipt_candidates_2005 <- c(
  "transferencias_de_recursos_do_fundef",
  "transferencias_da_complementacao_da_uniao_ao_fundef",
  "rec_de_remuneracao_de_depositos_bancarios_fundef"
)

read_siope_2005 <- function(uf, base_path) {
  
  file_path <- file.path(base_path, paste0("siope_", uf, "_2005_P1.csv"))
  
  if (!file.exists(file_path)) {
    warning("File not found: ", file_path)
    return(NULL)
  }
  
  siope <- read.csv(file_path)
  
  # ── Structural check: does this look like a standard SIOPE file? ──────────
  required_raw_cols <- c("TIPO", "NOM_MUNI", "NOM_ITEM", "VAL_DECL", "NOM_COLU")
  missing_raw <- setdiff(required_raw_cols, names(siope))
  if (length(missing_raw) > 0) {
    stop(
      "SIOPE 2005 file for ", uf, " is missing expected columns: ",
      paste(missing_raw, collapse = ", "),
      "\nActual columns: ", paste(names(siope), collapse = ", ")
    )
  }
  
  # ── Pivot (no IDN_CLAS filter: 2005 files likely don't have this field) ──
  siope_rr <- siope %>%
    mutate(NOM_MUNI = ifelse(NOM_MUNI == "null", "Estadual", NOM_MUNI)) %>%
    pivot_wider(
      id_cols     = c(TIPO, NUM_ANO, NUM_PERI, COD_UF, SIG_UF,
                      COD_MUNI, NOM_MUNI, NOM_COLU),
      names_from  = NOM_ITEM,
      values_from = VAL_DECL,
      values_fn   = sum,
      values_fill = 0
    ) %>%
    clean_names() %>%
    filter(nom_colu == "Receitas Realizadas")
  
  if (nrow(siope_rr) == 0) {
    warning(
      uf, ": pivot produced 0 rows after filtering nom_colu == 'Receitas Realizadas'. ",
      "Unique nom_colu values in raw file: ",
      paste(unique(siope$NOM_COLU), collapse = ", ")
    )
    return(NULL)
  }
  
  # ── Report which FUNDEF receipt columns are actually present ──────────────
  receipt_cols_found <- intersect(fundef_receipt_candidates_2005, names(siope_rr))
  receipt_cols_missing <- setdiff(fundef_receipt_candidates_2005, names(siope_rr))
  
  if (length(receipt_cols_found) == 0) {
    warning(
      uf, ": NO FUNDEF receipt columns found. Tried: ",
      paste(fundef_receipt_candidates_2005, collapse = ", "),
      "\nAll fundef-related columns in file: ",
      paste(names(siope_rr)[str_detect(names(siope_rr), "fundef")], collapse = ", ")
    )
    return(NULL)
  }
  
  if (length(receipt_cols_missing) > 0) {
    message(
      uf, ": some receipt columns absent (will be treated as 0): ",
      paste(receipt_cols_missing, collapse = ", ")
    )
  }
  
  # ── Sum FUNDEF received per entity (mun + state), then aggregate to UF ────
  siope_rr %>%
    mutate(
      fundef_received_core    = if ("transferencias_de_recursos_do_fundef"
                                    %in% names(.)) transferencias_de_recursos_do_fundef else 0,
      fundef_complement_uniao = if ("transferencias_da_complementacao_da_uniao_ao_fundef"
                                    %in% names(.)) transferencias_da_complementacao_da_uniao_ao_fundef else 0,
      fundef_remun_bancaria   = if ("rec_de_remuneracao_de_depositos_bancarios_fundef"
                                    %in% names(.)) rec_de_remuneracao_de_depositos_bancarios_fundef else 0,
      fundef_received_total   = fundef_received_core + fundef_complement_uniao + fundef_remun_bancaria
    ) %>%
    group_by(cod_uf, sig_uf) %>%
    summarise(
      fundef_received_core_2005    = sum(fundef_received_core,    na.rm = TRUE),
      fundef_complement_uniao_2005 = sum(fundef_complement_uniao, na.rm = TRUE),
      fundef_remun_bancaria_2005   = sum(fundef_remun_bancaria,   na.rm = TRUE),
      fundef_received_total_2005   = sum(fundef_received_total,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(uf = sig_uf)
}

# ── Run across all UFs ────────────────────────────────────────────────────────
ufs_2005 <- c(
  "AC","AL","AP","AM","BA","CE","DF","ES","GO",
  "MA","MT","MS","MG","PA","PB","PR","PE","PI",
  "RJ","RN","RS","RO","RR","SC","SP","SE","TO"
)

base_path_2005 <- "Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/SIOPE"
# Adjust if 2005 files live in a different subfolder

fundef_2005_uf <- map_dfr(
  ufs_2005,
  read_siope_2005,
  base_path = base_path_2005
)

fundef_2005_uf %>% arrange(sig_uf) %>% print(n = Inf)

fundef_2005_uf <- fundef_2005_uf %>%
  group_by(cod_uf, sig_uf) %>%
  summarise(
    mun_contribution   = sum(mun_contribution, na.rm = TRUE),
    state_contribution = sum(state_contribution, na.rm = TRUE),
    total_contribution  = mun_contribution + state_contribution,
    
    mun_received        = sum(mun_received, na.rm = TRUE),
    state_received      = sum(state_received, na.rm = TRUE),
    total_received      = mun_received + state_received,
    
    complement_uniao    = sum(fundef_complement_uniao_2005, na.rm = TRUE),
    remun_bancaria      = sum(fundef_remun_bancaria_2005, na.rm = TRUE),
    
    total_fund_resources = total_received + complement_uniao + remun_bancaria,
    balance_vs_contrib   = total_fund_resources - total_contribution,
    
    .groups = "drop"
  ) %>%
  arrange(sig_uf)


# ---------------------------------------------------------------------------- #
### 4.2.3 Combined ----
# ---------------------------------------------------------------------------- #


fundef_2005 <- fundef_2005_uf %>% 
  left_join(mat_uf_2004, by = c("sig_uf" = "uf")) %>% 
  select(-uf)

fundef_2005 <- fundef_2005 %>% 
  group_by(sig_uf) %>% 
  mutate(
    urb_per_stu = 
      fundef_received_total_2005 /
      (urb_ini + 1.02*rur_ini + 1.05*urb_fim + 1.05*1.019*rur_fim_esp),
    
    #deflate
    urb_per_stu_def = urb_per_stu/df_ipca$indice[df_ipca$ano == 2005]
  ) 


# ---------------------------------------------------------------------------- #
## 4.3 Combining the rates -----
# ---------------------------------------------------------------------------- #

fundef_2006 <- fundef_2006 %>% 
  left_join(fundef_2005 %>% select(urb_per_stu_def, sig_uf, cod_uf) %>%
              rename(urb_per_stu_2005 = urb_per_stu_def),
            by = c("sig_uf","cod_uf"))


rm(fundef_2005, fundef_2005_uf)
# ---------------------------------------------------------------------------- #
# 4. Combine 2006 FUNDEF and 2007 counterfactual FUNDEF ----
# ---------------------------------------------------------------------------- #

fundef_2006_summary <- fundef_2006_uf %>%
  transmute(
    cod_uf,
    sig_uf,
    uf = sig_uf,
    mun_contrib_2006 = mun_contribution,
    state_contrib_2006 = state_contribution,
    total_contrib_2006 = total_contribution,
    federal_transfer_2006_inferred = federal_complement_inferred,
    federal_transfer_2006_problem = federal_complement_problem,
    total_received_2006 = total_received,
    total_fundef_2006_distribution_side = total_received,
    total_fundef_2006_contribution_side = total_contribution
  )

fundef_cf_2006_2007 <- fundef_2007_uf_cf %>%
  transmute(
    cod_uf,
    sig_uf,
    uf,
    mun_contrib_2007_cf = mun_contrib_cf,
    state_contrib_2007_cf = state_contrib_cf,
    total_contrib_2007_cf = total_contrib_cf
  ) %>%
  left_join(fundef_2006_summary, by = c("cod_uf", "sig_uf", "uf")) %>%
  mutate(
    contrib_gap_2007_minus_2006 = total_contrib_2007_cf - total_contrib_2006,
    federal_transfer_2007_signed_gap = total_contrib_2006 - total_contrib_2007_cf,
    federal_transfer_2007_assumed = pmax(federal_transfer_2007_signed_gap, 0),
    total_fundef_2007_assumed = total_contrib_2007_cf + federal_transfer_2007_assumed
  ) %>%
  arrange(sig_uf)


# ---------------------------------------------------------------------------- #
# 5. Student-denominator calculations ----
# ---------------------------------------------------------------------------- #

students_2005_uf <- mat_uf %>%
  transmute(
    uf,
    fundef_students_2005 = total
  )

fundef_cf_2007_students <- fundef_cf_2006_2007 %>%
  left_join(students_2005_uf, by = "uf") %>%
  mutate(
    fundef_2007_per_student_using_2005_students =
      total_fundef_2007_assumed / fundef_students_2005
  )

# Optional next step: after building the true 2006 student denominator, create
# mat_uf_2006 with columns uf and total, then this block will add the adjusted
# total implied by 2006 enrollment.
if (exists("mat_uf_2006")) {
  students_2006_uf <- mat_uf_2006 %>%
    transmute(
      uf,
      fundef_students_2006 = total
    )
  
  fundef_cf_2007_students <- fundef_cf_2007_students %>%
    left_join(students_2006_uf, by = "uf") %>%
    mutate(
      fundef_2007_total_using_2006_students =
        fundef_2007_per_student_using_2005_students * fundef_students_2006,
      student_count_change_2006_vs_2005 =
        fundef_students_2006 / fundef_students_2005 - 1
    )
}

# ---------------------------------------------------------------------------- #
# 6. Deflation placeholder ----
# ---------------------------------------------------------------------------- #

# After adding the inflation data frame, join it here and deflate all monetary
# columns consistently. Expected minimum columns:
#   year, deflator_to_base_year
# where real_value = nominal_value * deflator_to_base_year.

if (exists("inflation_df")) {
  monetary_cols <- names(fundef_cf_2007_students)[
    str_detect(
      names(fundef_cf_2007_students),
      "contrib|transfer|fundef|per_student"
    )
  ]
  
  fundef_cf_2007_students_real <- fundef_cf_2007_students %>%
    mutate(year = 2007) %>%
    left_join(inflation_df, by = "year") %>%
    mutate(
      across(
        all_of(monetary_cols),
        ~ .x * deflator_to_base_year,
        .names = "{.col}_real"
      )
    )
}
