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
    mun_contribution =
      deducao_do_fpm_para_o_fundef_15_percent +
      deducao_de_lc_87_96_para_o_fundef_15_percent +
      deducao_do_icms_para_o_fundef_15_percent +
      deducao_do_ipi_exportacao_para_o_fundef_15_percent
  ) %>%
  select(
    cod_uf, sig_uf, cod_muni, nom_muni,
    fundef_received = transferencias_de_recursos_do_fundef,
    mun_contribution
  )

mun_uf <- municipios %>%
  group_by(cod_uf, sig_uf) %>%
  summarise(
    mun_received     = sum(fundef_received, na.rm = TRUE),
    mun_contribution = sum(mun_contribution, na.rm = TRUE),
    .groups = "drop"
  )

# ── 3. State layer ───────────────────────────────────────────────────────────
estado <- test1_full %>%
  filter(tipo == "Estadual") %>%
  mutate(
    state_contribution =
      deducao_do_icms_para_a_formacao_do_fundef_15_percent +
      deducao_da_cota_parte_do_fpe_para_formacao_do_fundef_15_percent +
      deducao_da_cota_parte_do_ipi_exportacao_para_formacao_do_fundef_15_percent,
    state_received = transferencias_de_recursos_do_fundef
  ) %>%
  group_by(cod_uf, sig_uf) %>%
  summarise(
    state_contribution = sum(state_contribution, na.rm = TRUE),
    state_received     = sum(state_received, na.rm = TRUE),
    .groups = "drop"
  )

# ── 4. UF-level sanity check ─────────────────────────────────────────────────
check_uf <- mun_uf %>%
  left_join(estado, by = c("cod_uf", "sig_uf")) %>%
  mutate(
    total_received     = mun_received + state_received,
    total_contribution = mun_contribution + state_contribution,
    federal_complement = total_received - total_contribution
  ) %>%
  arrange(sig_uf)

print(check_uf)

# ── 5. Municipality-level dataset with UF state values attached ─────────────
fundef_06 <- municipios %>%
  left_join(estado, by = c("cod_uf", "sig_uf")) %>%
  mutate(
    net_position = fundef_received - mun_contribution
  ) %>%
  arrange(sig_uf, nom_muni)

# ── 6. Optional: UF summary from the merged municipality table ───────────────
fundef_06 %>%
  group_by(cod_uf, sig_uf) %>%
  summarise(
    total_mun_received    = sum(fundef_received, na.rm = TRUE),
    total_mun_contrib     = sum(mun_contribution, na.rm = TRUE),
    state_contrib         = first(state_contribution),
    state_received        = first(state_received),
    total_received        = total_mun_received + state_received,
    total_contribution    = total_mun_contrib + state_contrib,
    federal_complement    = total_received - total_contribution,
    .groups = "drop"
  ) %>%
  arrange(sig_uf)


## 3.1) Censo Escolar de 2006 (deixar comentado):----

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

### 3.1.1) Agregação das matrículas de cada ciclo por ESCOLA: ----
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

# DF do censo já agregada por escola (é muito pesada para rodar toda vez):
write.csv2(mat_2005, file = "Z:/Tuffy/Paper - Educ/Dados/censo_2005_filtrado.csv")


# ---------------------------------------------------------------------------- #
# Agregado ----
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

mat_ac <- mat_2005_munic %>% 
  filter(uf == "AC") %>% 
  group_by(uf) %>% 
  summarise(
    urb_ini = sum(urb_ini, na.rm = T),
    urb_fim = sum(urb_fim, na.rm = T),
    rur_ini = sum(rur_ini, na.rm = T),
    rur_fim_esp = sum(rur_fim_esp, na.rm = T),
    
    .groups = "drop"
  )

#Calculating the value per student
mat_ac <- mat_ac %>% 
  mutate(
    urb_ini_rs = 1685.41*urb_ini,
    urb_fim_rs = 1719.12*urb_fim,
    rur_ini_rs = 1769.68*rur_ini,
    rur_fim_esp_rs = 1803.39*rur_fim_esp,
    
    fnd_tot = urb_ini_rs + urb_fim_rs + rur_ini_rs + rur_fim_esp_rs
  )

#' Found a 0.24% difference between the estipulated value and the actually received


# ---------------------------------------------------------------------------- #
# Start ----
# ---------------------------------------------------------------------------- #

# ------------------------------------------------------------------------- #
# UFs
# ------------------------------------------------------------------------- #

#Df será feito separadamente

ufs <- c(
  "AC","AL","AP","AM","BA","CE","ES","GO",
  "MA","MT","MS","MG","PA","PB","PR","PE","PI",
  "RJ","RN","RS","RO","RR","SC","SP","SE","TO"
)

# ------------------------------------------------------------------------- #
# Function
# ------------------------------------------------------------------------- #

process_siope_uf <- function(uf){
  
  file_path <- paste0(
    "Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/SIOPE/",
    "siope_", uf, "_2006_P1.csv"
  )
  
  siope <- read.csv(file_path)
  
  test1_full <- siope %>%
    mutate(
      NOM_MUNI = ifelse(NOM_MUNI == "null", "Estadual", NOM_MUNI)
    ) %>%
    pivot_wider(
      id_cols = c(
        TIPO, NUM_ANO, NUM_PERI,
        COD_UF, SIG_UF,
        COD_MUNI, NOM_MUNI,
        NOM_COLU
      ),
      names_from = NOM_ITEM,
      values_from = VAL_DECL,
      values_fn = sum,
      values_fill = 0
    ) %>%
    clean_names() %>%
    filter(nom_colu == "Receitas Realizadas")
  
  # ----------------------------------------------------------------------- #
  # Municipal contribution
  # ----------------------------------------------------------------------- #
  
  municipios <- test1_full %>%
    filter(tipo == "Municipal") %>%
    mutate(
      mun_contribution =
        deducao_do_fpm_para_o_fundef_15_percent +
        deducao_de_lc_87_96_para_o_fundef_15_percent +
        deducao_do_icms_para_o_fundef_15_percent +
        deducao_do_ipi_exportacao_para_o_fundef_15_percent
    ) %>%
    select(
      cod_uf,
      sig_uf,
      cod_muni,
      nom_muni,
      fundef_received = transferencias_de_recursos_do_fundef,
      mun_contribution
    )
  
  mun_uf <- municipios %>%
    group_by(cod_uf, sig_uf) %>%
    summarise(
      mun_received = sum(fundef_received, na.rm = TRUE),
      mun_contribution = sum(mun_contribution, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ----------------------------------------------------------------------- #
  # State contribution
  # ----------------------------------------------------------------------- #
  
  estado <- test1_full %>%
    filter(tipo == "Estadual") %>%
    mutate(
      state_contribution =
        deducao_do_icms_para_a_formacao_do_fundef_15_percent +
        deducao_da_cota_parte_do_fpe_para_formacao_do_fundef_15_percent +
        deducao_da_cota_parte_do_ipi_exportacao_para_formacao_do_fundef_15_percent,
      
      state_received =
        transferencias_de_recursos_do_fundef
    ) %>%
    group_by(cod_uf, sig_uf) %>%
    summarise(
      state_contribution =
        sum(state_contribution, na.rm = TRUE),
      
      state_received =
        sum(state_received, na.rm = TRUE),
      
      .groups = "drop"
    )
  
  # ----------------------------------------------------------------------- #
  # UF Summary
  # ----------------------------------------------------------------------- #
  
  check_uf <- mun_uf %>%
    left_join(
      estado,
      by = c("cod_uf", "sig_uf")
    ) %>%
    mutate(
      total_received =
        mun_received + state_received,
      
      total_contribution =
        mun_contribution + state_contribution,
      
      federal_complement =
        total_received - total_contribution
    )
  
  return(check_uf)
  
}

# ------------------------------------------------------------------------- #
# Run for all states
# ------------------------------------------------------------------------- #

fundef_2006_uf <- map_dfr(
  ufs,
  process_siope_uf
)

# ------------------------------------------------------------------------- #
# Results
# ------------------------------------------------------------------------- #

View(fundef_2006_uf)

fundef_2006_uf %>%
  arrange(desc(abs(federal_complement)))


# -------------------------------------------------------------------------
# 1) School census: municipality level
# -------------------------------------------------------------------------

mat_2005_munic <- mat_2005 %>%
  group_by(CODMUNIC) %>%
  summarise(
    nome = first(MUNIC),
    no_uf = first(UF),
    uf = first(SIGLA),
    
    urb_ini = sum(if_else(LOC == "Urbana", reg_in, 0), na.rm = TRUE),
    urb_fim = sum(if_else(LOC == "Urbana", reg_fin, 0), na.rm = TRUE),
    rur_ini = sum(if_else(LOC == "Rural", reg_in, 0), na.rm = TRUE),
    rur_fim_esp = sum(if_else(LOC == "Rural", reg_fin + esp_total_final, 0), na.rm = TRUE),
    
    .groups = "drop"
  )

# -------------------------------------------------------------------------
# 2) School census: UF level
# -------------------------------------------------------------------------

mat_uf <- mat_2005_munic %>%
  group_by(uf) %>%
  summarise(
    urb_ini = sum(urb_ini, na.rm = TRUE),
    urb_fim = sum(urb_fim, na.rm = TRUE),
    rur_ini = sum(rur_ini, na.rm = TRUE),
    rur_fim_esp = sum(rur_fim_esp, na.rm = TRUE),
    .groups = "drop"
  ) 

# -------------------------------------------------------------------------
# 3) Join census to FUNDEF UF dataframe
# -------------------------------------------------------------------------

fundef_uf_final <- fundef_2006_uf %>%
  left_join(mat_uf, by = c("sig_uf" = "uf"))
