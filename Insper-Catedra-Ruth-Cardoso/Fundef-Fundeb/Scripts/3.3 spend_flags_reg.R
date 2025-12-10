# ---------------------------------------------------------------------------- #
# Regressions - Spending + Flags
# Last edited by: Tuffy Licciardi Issa
# Date: 12/12/2025
# ---------------------------------------------------------------------------- #


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
library(grid)
library(patchwork)


#Deactivating scientific notation
options(scipen = 999)

# ---------------------------------------------------------------------------- #
# 1. Data ----
# ---------------------------------------------------------------------------- #


data <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf_flags.rds")



df_spend <- data %>% 
  filter(tipo == "Municipal") %>% 
  mutate(dif_rs_aluno_100 = dif_rs_aluno / 100,
         region = case_when(
           as.numeric(codigo_ibge) %/% 100000 == 1 ~ "Norte",        #North
           as.numeric(codigo_ibge) %/% 100000 == 2 ~ "Nordeste",     #Northeast
           as.numeric(codigo_ibge) %/% 100000 == 3 ~ "Sudeste",      #Southeast
           as.numeric(codigo_ibge) %/% 100000 == 4 ~ "Sul",          #South
           as.numeric(codigo_ibge) %/% 100000 == 5 ~ "Centro-Oeste", #Central-West
           TRUE ~ NA
         )
  ) %>%
  #Groups
  mutate(grupo = case_when(
    dosage > 0 ~ "Winner",   # net beneficiary
    dosage < 0 ~ "Loser",   # net contributer
    TRUE ~ NA_character_
  ),
  
  net_fundeb = (receita_real - receita_simulada)/real_des_edu[ano == 2007]
  ) %>% 
  filter(tipo == "Municipal") %>% 
  #Manually creating dummy values
  mutate(d05 = ifelse(ano == 2005, 1, 0),
         d06 = ifelse(ano == 2006, 1, 0),
         d07 = ifelse(ano == 2007, 1, 0),
         d08 = ifelse(ano == 2008, 1, 0),
         d09 = ifelse(ano == 2009, 1, 0),
         d10 = ifelse(ano == 2010, 1, 0),
         d11 = ifelse(ano == 2011, 1, 0),
         d12 = ifelse(ano == 2012, 1, 0),
         d13 = ifelse(ano == 2013, 1, 0),
         d14 = ifelse(ano == 2014, 1, 0),
         d15 = ifelse(ano == 2015, 1, 0),
         d16 = ifelse(ano == 2016, 1, 0),
         d17 = ifelse(ano == 2017, 1, 0),
         d18 = ifelse(ano == 2018, 1, 0),
         d19 = ifelse(ano == 2019, 1, 0),
         d20 = ifelse(ano == 2020, 1, 0),
         d21 = ifelse(ano == 2021, 1, 0))

# ---------------------------------------------------------------------------- #
# 2. Regressions ----
# ---------------------------------------------------------------------------- #
## 2.1. Reg Prof ----
# ---------------------------------------------------------------------------- #


est_10 <- feols(des_edu_pc ~ aluno_dosage : i(ano, ref = 2006)
                 | codigo_ibge + uf^ano,
                 data = df_spend %>% group_by (codigo_ibge) %>% 
                   filter(ano < 2011) %>% 
                   filter(all(between(growth_spend, -20, 70), na.rm = TRUE)) %>%
                   ungroup(),
                 vcov = "hetero")

est_prof <- feols(des_edu_pc ~ aluno_dosage : i(ano, ref = 2006)
                  | codigo_ibge + uf^ano,
                  data = df_spend %>% 
                    filter(ano < 2011) %>% 
                    filter(flag_spend70 == 0 & flag_spendm20 == 0),
                  vcov = "hetero")

etable(est_prof, est_10)


# ---------------------------------------------------------------------------- #
## 2.2 Exausting Filters -----
# ---------------------------------------------------------------------------- #

# for( i in c(60,70,80)) {
#   
#   #For threshold 60
#   if(i == 60){
#     
#     temp <- df_spend %>% 
#       filter(flag_spend60 == 0)
#     
#     } else if(i == 70){
#       
#       temp <- df_spend %>% 
#       filter(flag_spend70 == 0)
#       
#       } else {
#         
#         temp <- df_spend %>% 
#         filter(flag_spend80 == 0)
#         
#   } #ending the uper threshold loop
#     
#   
#   #Lower bound loop
#   for(j in c(20, 25, 30, 40)) {
#     
#     #lower20
#     if(j == 20){
#       
#       temp <- temp %>% 
#         filter(flag_spendm20 == 0)
#       
#     } else if(j == 25){ #Lower25
#       
#       temp <- temp %>%
#         filter(flag_spendm25 == 0)
#       
#     } else if(j == 30){ #Lower 30
#       
#       temp <- temp %>% 
#         filter(flag_spendm30 == 0)
#       
#     } else {#Lower 40
#       
#       temp <- temp %>% 
#         filter(flag_spendm40 == 0)
#       
#       }
#   } #ending lowebound loop
#   
#   
#   #Now for the regressions
#   
#   
#   
#   
# }



uppers <- c(60, 70, 80)
lowers <- c(20, 25, 30, 40)

my_formula <- des_edu_pc ~ aluno_dosage : i(ano, ref = 2006) + PIBpc | codigo_ibge + ano + uf^ano

for(i in uppers) {
  up_flag <- paste0("flag_spend", i)
  
  temp_i <- df_spend %>% filter(.data[[up_flag]] == 0 |
                                  dosage == 1)
  
  models_for_i <- vector("list", length(lowers))
  names(models_for_i) <- as.character(lowers)
  
  for(j in seq_along(lowers)) {
    jj <- lowers[j]
    # build lower-flag name e.g. "flag_spendm20"
    low_flag <- paste0("flag_spendm", jj)
    
    # always start from temp_i so filters don't accumulate across j
    temp_ij <- temp_i %>% filter(.data[[low_flag]] == 0 |
                                   dosage == 1)
    
    
    # run your model
    msg <- sprintf("Estimating i=%s j=%s on %d obs...", i, jj, nrow(temp_ij))
    message(msg)
    
    #Estimation
    m <- feols(my_formula, data = temp_ij, vcov = "hetero")
    
    # store in the slot corresponding to this j
    models_for_i[[j]] <- m
  }
  

  # produce a file name for each i
  out_file <- paste0("Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/reg_table_upper", i, ".tex")
  
  # Write a 4-column table for this i (only non-NULL models will appear)
  do.call(etable, c(models_valid,
                    list(file = out_file,
                         replace = TRUE,
                         vcov = "hetero",
                         tex = TRUE)))
  
  message("Wrote table for upper = ", i, " -> ", out_file)
}





