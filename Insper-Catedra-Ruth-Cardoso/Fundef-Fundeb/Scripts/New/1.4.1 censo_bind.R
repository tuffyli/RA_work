# ---------------------------------------------------------------------------- #
# Data Combination
# DataBase adjustment
# Last edited by: Tuffy Licciardi Issa
# Date: 14/07/2025
# ---------------------------------------------------------------------------- #
#' Here I will combine the created school census municipal data with the dosage
#' variable. This will enable running the regression directly with this new data.


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
library(ggplot2)
library(readr)
library(foreign)
library(stringr)

#Desativando a notação científica
options(scipen = 999)

# ---------------------------------------------------------------------------- #
# 1. Main data ------------
# ---------------------------------------------------------------------------- #

main_df <- readRDS("Z:/Tuffy/Paper - Educ/Dados/final/regdf_flags.rds") %>% 
  mutate(cod_uf = codigo_ibge %/% 10000) %>% 
  group_by(cod_uf) %>%
  mutate(
    dosage_tercile = ntile(aluno_dosage, 3),
    #1. Lowest,
    #2. Middle,
    #3. Highest
    
    post_treat = ifelse(ano > 2006, 1, 0)
  ) %>%
  ungroup()

df_dosage <- main_df %>% 
  mutate(treat = ifelse(dosage_tercile != 2, 1, 0)) %>% 
  filter(ano == 2007) %>% 
  select(
    codigo_ibge, uf, aluno_dosage, treat, dosage_tercile
  )


df_pib <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/final_pib.rds") %>% 
  arrange(codigo_ibge, ano)


# ---------------------------------------------------------------------------- #
# 2. No municipal affiliation ----
# ---------------------------------------------------------------------------- #

no_fil <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/mun_prop_data.rds")

# ---------------------------------------------------------------------------- #
## 2.1 Combination -----
# ---------------------------------------------------------------------------- #

no_fil <- no_fil %>% 
  mutate(
    codigo_ibge = as.numeric(codmun) %/% 10,
    codmun = as.numeric(codmun)
  ) %>% 
  left_join(
    df_dosage,
    by = c("codigo_ibge")
  ) %>% 
  select(-codigo_ibge) %>% 
  left_join(
    df_pib %>% select(-nom),
    by = c("codmun" = "codigo_ibge", "ano" = "ano")
  )

# ---------------------------------------------------------------------------- #
## 2.2 Saving ----
# ---------------------------------------------------------------------------- #

saveRDS(no_fil, "Z:/Tuffy/Paper - Educ/Dados/final/mun_school_data.rds" )


# ---------------------------------------------------------------------------- #
# 3. With municipal affiliation ----
# ---------------------------------------------------------------------------- #

wi_fil <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/mun_prop_data_fil.rds")

# ---------------------------------------------------------------------------- #
## 3.1 Combination -----
# ---------------------------------------------------------------------------- #

wi_fil <- wi_fil %>% 
  mutate(
    codigo_ibge = as.numeric(codmun) %/% 10,
    codmun = as.numeric(codmun)
  ) %>% 
  left_join(
    df_dosage,
    by = c("codigo_ibge")
  ) %>% 
  select(-codigo_ibge) %>% 
  left_join(
    df_pib %>% select(-nom),
    by = c("codmun" = "codigo_ibge", "ano" = "ano")
  )

# ---------------------------------------------------------------------------- #
## 3.2 Saving ----
# ---------------------------------------------------------------------------- #

saveRDS(wi_fil, "Z:/Tuffy/Paper - Educ/Dados/final/mun_school_data_affiliations.rds" )

# ---------------------------------------------------------------------------- #
# 4. School Type main ----
# ---------------------------------------------------------------------------- #

df_type <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/mun_prop_data_school_type.rds")

# ---------------------------------------------------------------------------- #
## 4.1 Combination -----
# ---------------------------------------------------------------------------- #

df_type <- df_type %>% 
  mutate(
    codigo_ibge = as.numeric(codmun) %/% 10,
    codmun = as.numeric(codmun)
  ) %>% 
  left_join(
    df_dosage,
    by = c("codigo_ibge")
  ) %>% 
  select(-codigo_ibge) %>% 
  left_join(
    df_pib %>% select(-nom),
    by = c("codmun" = "codigo_ibge", "ano" = "ano")
  )


# ---- Saving ---- #

saveRDS(df_type, "Z:/Tuffy/Paper - Educ/Dados/intermediate/mun_prop_data_school_type_dosage.rds" )


# ---------------------------------------------------------------------------- #
# 5. School Type afl ----
# ---------------------------------------------------------------------------- #

df_type <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/mun_prop_data_school_type.rds")

# ---------------------------------------------------------------------------- #
## 5.1 Combination -----
# ---------------------------------------------------------------------------- #

df_type <- df_type %>% 
  mutate(
    codigo_ibge = as.numeric(codmun) %/% 10,
    codmun = as.numeric(codmun)
  ) %>% 
  left_join(
    df_dosage,
    by = c("codigo_ibge")
  ) %>% 
  select(-codigo_ibge) %>% 
  left_join(
    df_pib %>% select(-nom),
    by = c("codmun" = "codigo_ibge", "ano" = "ano")
  )


# ---- Saving ---- #

saveRDS(df_type, "Z:/Tuffy/Paper - Educ/Dados/intermediate/afl_mun_prop_data_school_type_dosage.rds" )
