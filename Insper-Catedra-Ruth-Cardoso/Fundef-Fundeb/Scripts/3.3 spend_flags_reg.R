# ---------------------------------------------------------------------------- #
# Regressions - Spending + Flags
# Last edited by: Tuffy Licciardi Issa
# Date: 17/12/2025
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

#Here the dosage == 1 municipalities are loss through the filters

est_10 <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend %>% group_by (codigo_ibge) %>% 
                   filter(ano < 2011) %>% 
                   #filter(all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                   ungroup(),
                 vcov = "hetero")

est_prof <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_spend %>% 
                    filter(ano < 2011) %>% 
                    filter(dosage == 1 | flag_spend70 == 0 & flag_spendm20 == 0),
                  vcov = "hetero")

etable(est_10, est_prof
       )

etable( est_10, est_prof, #mod_fund, mod_med,
       vcov = "hetero",
       headers = list(":_:" = list("Sem Filtro" = 1,
                                   #"Fundamental" = 1, "Médio" = 1,
                                   "Filtro +70 -20" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/reg_prof.tex",
       replace = TRUE)

rm(est_prof, est_10)
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



uppers <- c(60, 70, 80
            )
lowers <- c(20, 25, 30, 40)

my_formula <- real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc | codigo_ibge + ano + uf^ano

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
  

  models_valid <- models_for_i[!vapply(models_for_i, is.null, logical(1))]
  
  # produce a file name for each i
  out_file <- paste0("Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/reg_table_upper", i, ".tex")
  
  #Write a 4-column table for this i (only non-NULL models will appear)
  do.call(etable, c(models_valid,
                    list(file = out_file,
                         headers = list(":_:" = list("-20%" = 1,
                                                     "-25%" = 1,
                                                     "-30%" = 1,
                                                     "-40%" = 1)),
                         replace = TRUE,
                         vcov = "hetero",
                         tex = TRUE)))

  message("Wrote table for upper = ", i, " -> ", out_file)
  
  
  rm( j, jj, up_flag, low_flag, msg)
}



# ---------------------------------------------------------------------------- #
## 2.3 Annual ----
# ---------------------------------------------------------------------------- #
### 2.3.1 Strict ----
# ---------------------------------------------------------------------------- #

#' Tabela 1 Coluna 1 = 60% e -20%

#data
temp <- df_spend %>% 
    filter(dosage == 1 | flag_spend70 == 0 & flag_spendm20 == 0) #Upper
  

  
#For each year 
est_10 <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = temp %>% filter(ano < 2011),
                vcov = "hetero")


est_14 <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = temp %>% filter(ano < 2015),
                  vcov = "hetero")


est_18 <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codigo_ibge + ano +  uf^ano,
                data = temp %>% filter(ano < 2019),
                vcov = "hetero")



#Saving the results
etable(est_10, est_14, est_18, #mod_fund, mod_med,
       vcov = "hetero",
       headers = list(":_:" = list("2010" = 1,
                                   "2014" = 1,
                                   "2018" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/reg_estrita_anos.tex",
       replace = TRUE)



rm(temp, est_10, est_14, est_18)

# ---------------------------------------------------------------------------- #
### 2.3.2 Broad ----
# ---------------------------------------------------------------------------- #

#Running the regressions
est_10 <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_spend %>% filter(ano < 2011) %>% group_by(codigo_ibge) %>% 
                  filter(dosage == 1 | 
                           all(growth_spend > -20 & growth_spend < 60, na.rm = TRUE)) %>% ungroup(),
                vcov = "hetero")


est_14 <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_spend %>% filter(ano < 2015) %>% group_by(codigo_ibge) %>% 
                  filter(dosage == 1 | 
                           all(growth_spend > -20 & growth_spend < 60, na.rm = TRUE)) %>% ungroup(),
                vcov = "hetero")


est_18 <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codigo_ibge + ano +  uf^ano,
                data = df_spend %>% filter(ano < 2019) %>% group_by(codigo_ibge) %>% 
                  filter(dosage == 1 | 
                           all(growth_spend > -20 & growth_spend < 60, na.rm = TRUE)) %>% ungroup(),
                vcov = "hetero")



#Saving the results
etable(est_10, est_14, est_18, #mod_fund, mod_med,
       vcov = "hetero",
       headers = list(":_:" = list("2010" = 1,
                                   "2014" = 1,
                                   "2018" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/reg_abrangente_anos.tex",
       replace = TRUE)


rm(est_10, est_14, est_18)

# ---------------------------------------------------------------------------- #
##3.3 Regression 2005 - 2018 -----
# ---------------------------------------------------------------------------- #

#Here the dosage == 1 municipalities are loss through the filters

est_10 <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_spend %>% group_by (codigo_ibge) %>% 
                  #filter(ano < 2011) %>% 
                  #filter(all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                  ungroup(),
                vcov = "hetero")

est_prof <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_spend %>% 
                    #filter(ano < 2011) %>% 
                    filter(dosage == 1 | flag_spend70 == 0 & flag_spendm20 == 0),
                  vcov = "hetero")

etable(est_10, est_prof
)

etable( est_10, est_prof, #mod_fund, mod_med,
        vcov = "hetero",
        headers = list(":_:" = list("Sem Filtro" = 1,
                                    #"Fundamental" = 1, "Médio" = 1,
                                    "Filtro +70 -20" = 1)),
        file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/reg_extended.tex",
        replace = TRUE)

rm(est_prof, est_10)


# ---------------------------------------------------------------------------- #
# 3. Enroll reg ----
# ---------------------------------------------------------------------------- #
## 3.1 Enrollment log ----
# ---------------------------------------------------------------------------- #

df_spend <- df_spend %>% 
  mutate( log_mat_total = log(mat_total),
          log_mat_fun   = log(mat_fun),
          log_mat_med   = log(mat_med),
          log_mat_inf   = log(mat_inf),
          log_mat_eja   = log(mat_eja),
          log_mat_esp   = log(mat_esp))


# ---------------------------------------------------------------------------- #
## 3.2 Regression ----
# ---------------------------------------------------------------------------- #
### 3.2.1 Cluster ----
# ---------------------------------------------------------------------------- #
#### 3.2.1.1 Strict -----
# ---------------------------------------------------------------------------- #


est_rest_tot_clu <- feols(mat_total ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_spend %>% 
                    filter(ano < 2011) %>% 
                    filter(dosage == 1 |
                             flag_spend70 == 0 & flag_spendm20 == 0),
                  vcov = ~codigo_ibge
                  )


est_rest_inf_clu <- feols(mat_inf ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = ~codigo_ibge
                      )



est_rest_fun_clu <- feols(mat_fun ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = ~codigo_ibge
                      )



est_rest_med_clu <- feols(mat_med ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = ~codigo_ibge
                      )


etable(est_rest_tot_clu, est_rest_inf_clu, est_rest_fun_clu, est_rest_med_clu
)

etable(est_rest_tot_clu, est_rest_inf_clu, est_rest_fun_clu, est_rest_med_clu, #mod_fund, mod_med,
       vcov = ~codigo_ibge,
       headers = list(":_:" = list("Total" = 1, "Infantil" = 1, "Fundamental" = 1,
                                   "Médio" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/reg_mat_rest_cluster.tex",
       replace = TRUE)


# #Extracting the standard deviations
# 
# models <- list(
#   Total       = est_rest_tot_clu,
#   Infantil    = est_rest_inf_clu,
#   Fundamental = est_rest_fun_clu,
#   Medio       = est_rest_med_clu
# )
# 
# se_df <- map_dfr(names(models), function(name) {
#   
#   m <- models[[name]]
#   Vc <- vcov(m, ~codigo_ibge)          # clustered vcov
#   se_clu <- sqrt(diag(Vc))             # cluster SEs
#   tibble(
#     term = names(se_clu),
#     se_cluster = as.numeric(se_clu),
#     model = name
#   )
# })


rm(est_rest_tot, est_rest_inf, est_rest_fun, est_rest_med,
   est_rest_fun_clu, est_rest_inf_clu, est_rest_med_clu, est_rest_tot_clu, se_df)
# ---------------------------------------------------------------------------- #
#### 3.2.1.2 Broad ----
# ---------------------------------------------------------------------------- #

est_abra_tot <- feols(mat_total ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_spend %>% group_by (codigo_ibge) %>% 
                    filter(ano < 2011) %>% 
                    filter(dosage == 1 |
                             all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                    ungroup(),
                  vcov = ~codigo_ibge)

est_abra_inf <- feols(mat_inf ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)


est_abra_fun <- feols(mat_fun ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)

est_abra_med <- feols(mat_med ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano +uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)


etable(est_abra_tot, est_abra_inf, est_abra_fun, est_abra_med
)

etable(est_abra_tot, est_abra_inf, est_abra_fun, est_abra_med, #mod_fund, mod_med,
       vcov = ~codigo_ibge,
       headers = list(":_:" = list("Total" = 1, "Infantil" = 1, "Fundamental" = 1,
                                   "Médio" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/reg_mat_abra_cluster.tex",
       replace = TRUE)


rm(est_abra_tot, est_abra_med, est_abra_fun, est_abra_inf)

# ---------------------------------------------------------------------------- #
### 3.2.2 Hetero ----
# ---------------------------------------------------------------------------- #
#### 3.2.2.1 Strict -----
# ---------------------------------------------------------------------------- #


est_rest_tot <- feols(mat_total ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")


est_rest_inf <- feols(mat_inf ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")



est_rest_fun <- feols(mat_fun ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")



est_rest_med <- feols(mat_med ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")


etable(est_rest_tot, est_rest_inf, est_rest_fun, est_rest_med
)

etable(est_rest_tot, est_rest_inf, est_rest_fun, est_rest_med, #mod_fund, mod_med,
       vcov = "hetero",
       headers = list(":_:" = list("Total" = 1, "Infantil" = 1, "Fundamental" = 1,
                                   "Médio" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/reg_mat_rest.tex",
       replace = TRUE)


rm(est_rest_tot, est_rest_inf, est_rest_fun, est_rest_med)


# ---------------------------------------------------------------------------- #
#### 3.2.2.2 Broad ----
# ---------------------------------------------------------------------------- #

est_abra_tot <- feols(mat_total ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")

est_abra_inf <- feols(mat_inf ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")


est_abra_fun <- feols(mat_fun ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")

est_abra_med <- feols(mat_med ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano +uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")


etable(est_abra_tot, est_abra_inf, est_abra_fun, est_abra_med
)

etable(est_abra_tot, est_abra_inf, est_abra_fun, est_abra_med, #mod_fund, mod_med,
       vcov = "hetero",
       headers = list(":_:" = list("Total" = 1, "Infantil" = 1, "Fundamental" = 1,
                                   "Médio" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/reg_mat_abra.tex",
       replace = TRUE)


rm(est_abra_tot, est_abra_med, est_abra_fun, est_abra_inf)




# ---------------------------------------------------------------------------- #
# 4. Other spendings ----
# ---------------------------------------------------------------------------- #

#Repeating teh estimations from the previous section but for the different courses

## 4.1 Regression ----
# ---------------------------------------------------------------------------- #
### 4.1.1 Cluster ----
# ---------------------------------------------------------------------------- #
#### 4.1.1.1 Strict -----
# ---------------------------------------------------------------------------- #


est_rest_tot <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = ~codigo_ibge)


est_rest_inf <- feols(real_des_inf_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = ~codigo_ibge)



est_rest_fun <- feols(real_des_fun_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = ~codigo_ibge)



est_rest_med <- feols(real_des_med_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = ~codigo_ibge)


etable(est_rest_tot, est_rest_inf, est_rest_fun, est_rest_med
)

etable(est_rest_tot, est_rest_inf, est_rest_fun, est_rest_med, #mod_fund, mod_med,
       vcov = ~codigo_ibge,
       headers = list(":_:" = list("Total" = 1, "Infantil" = 1, "Fundamental" = 1,
                                   "Médio" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/reg_other_spend_cluster_rest.tex",
       replace = TRUE)


rm(est_rest_tot, est_rest_inf, est_rest_fun, est_rest_med)
# ---------------------------------------------------------------------------- #
#### 4.1.1.2 Broad ----
# ---------------------------------------------------------------------------- #

est_abra_tot <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)

est_abra_inf <- feols(real_des_inf_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)


est_abra_fun <- feols(real_des_fun_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)

est_abra_med <- feols(real_des_med_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano +uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)


etable(est_abra_tot, est_abra_inf, est_abra_fun, est_abra_med
)

etable(est_abra_tot, est_abra_inf, est_abra_fun, est_abra_med, #mod_fund, mod_med,
       vcov = ~codigo_ibge,
       headers = list(":_:" = list("Total" = 1, "Infantil" = 1, "Fundamental" = 1,
                                   "Médio" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/reg_other_spend_cluster_abra.tex",
       replace = TRUE)


rm(est_abra_tot, est_abra_med, est_abra_fun, est_abra_inf)

# ---------------------------------------------------------------------------- #
### 4.1.2 Hetero ----
# ---------------------------------------------------------------------------- #
#### 4.1.2.1 Strict -----
# ---------------------------------------------------------------------------- #



est_rest_tot <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")


est_rest_inf <- feols(real_des_inf_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")



est_rest_fun <- feols(real_des_fun_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")



est_rest_med <- feols(real_des_med_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")


etable(est_rest_tot, est_rest_inf, est_rest_fun, est_rest_med
)

etable(est_rest_tot, est_rest_inf, est_rest_fun, est_rest_med, #mod_fund, mod_med,
       vcov = "hetero",
       headers = list(":_:" = list("Total" = 1, "Infantil" = 1, "Fundamental" = 1,
                                   "Médio" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/reg_other_spend_rest.tex",
       replace = TRUE)


rm(est_rest_tot, est_rest_inf, est_rest_fun, est_rest_med)
# ---------------------------------------------------------------------------- #
#### 4.1.2.2 Broad ----
# ---------------------------------------------------------------------------- #

est_abra_tot <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")

est_abra_inf <- feols(real_des_inf_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")


est_abra_fun <- feols(real_des_fun_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")

est_abra_med <- feols(real_des_med_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano +uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")


etable(est_abra_tot, est_abra_inf, est_abra_fun, est_abra_med
)

etable(est_abra_tot, est_abra_inf, est_abra_fun, est_abra_med, #mod_fund, mod_med,
       vcov = "hetero",
       headers = list(":_:" = list("Total" = 1, "Infantil" = 1, "Fundamental" = 1,
                                   "Médio" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/reg_other_spend_abra.tex",
       replace = TRUE)


rm(est_abra_tot, est_abra_med, est_abra_fun, est_abra_inf)



# ---------------------------------------------------------------------------- #
#5. ES Win vs. Lose ----
# ---------------------------------------------------------------------------- #
## 5.1 Plot Func ----
# ---------------------------------------------------------------------------- #

win_lose_plot <- function(est_obj, title = NULL, ylim = NULL) {
  # extract tidy data from fixest::etable or broom::tidy
  event_df <- broom::tidy(est_obj, conf.int = TRUE) %>% 
    mutate(
      ano = str_extract(term, "(?<=ano::)-?\\d+"),
      ano = as.numeric(ano),
      # Extract group (text after the last colon)
      grupo = str_extract(term, "[^:]+$"),
      grupo = as.factor(grupo))
  
  
  event_df <- event_df %>% 
    bind_rows(
      event_df %>%
        distinct(grupo) %>%
        mutate(
          term = paste0(as.character(grupo),"ano:2006"),     
          estimate = 0,
          std.error = 0,
          statistic = 0,
          p.value = 1,
          conf.low = 0,
          conf.high = 0,
          ano = 2006
        )
    ) %>% 
    filter(grupo != "PIBpc")
  
  ggplot(event_df, aes(x = ano, y = estimate, group = grupo)) +
    # shaded standard error area
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 2006, color = "black") +
    geom_point(aes(color = grupo), shape = 15, size = 2) +
    geom_line(aes(color = grupo)) +
    labs(
      title = title,
      x = "Ano"
    ) +
    coord_cartesian(ylim = ylim) +
    theme_classic() +
    theme(
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11)
    ) + 
    scale_x_continuous(breaks = seq(2005, 2018, 2))
  
}




# ---------------------------------------------------------------------------- #
## 5.2 Enrollment ----
# ---------------------------------------------------------------------------- #
### 5.2.1 Strict ----
# ---------------------------------------------------------------------------- #

est_rest_tot <- feols(mat_total ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")


est_rest_inf <- feols(mat_inf ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")



est_rest_fun <- feols(mat_fun ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")



est_rest_med <- feols(mat_med ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")



# ---------------------------------------------------------------------------- #
### 5.2.2 Broad ----
# ---------------------------------------------------------------------------- #


est_abra_tot <- feols(mat_total ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")

est_abra_inf <- feols(mat_inf ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data =df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")


est_abra_fun <- feols(mat_fun ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")

est_abra_med <- feols(mat_med ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano +uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")


# ---------------------------------------------------------------------------- #
### 5.2.3 No Filter ----
# ---------------------------------------------------------------------------- #


est_tot <- feols(mat_total ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend,
                      vcov = "hetero")

est_inf <- feols(mat_inf ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend,
                      vcov = "hetero")


est_fun <- feols(mat_fun ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend,
                      vcov = "hetero")

est_med <- feols(mat_med ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano +uf^ano,
                      data = df_spend,
                      vcov = "hetero")


# -------------------------------------------- #
### 5.2.4 Plot ----
# -------------------------------------------- #

#p_tot_r <- win_lose_plot(est_rest_tot, "Restrito")    
p_tot_a <- win_lose_plot(est_abra_tot, "Abrangente 70-20")
p_tot_f <- win_lose_plot(est_tot,      "Sem Filtro")

#p_inf_r <- win_lose_plot(est_rest_inf, "Restrito")    
p_inf_a <- win_lose_plot(est_abra_inf, "Abrangente 70-20")
p_inf_f <- win_lose_plot(est_inf,      "Sem Filtro")


#p_fun_r <- win_lose_plot(est_rest_fun, "Restrito")  
p_fun_a <- win_lose_plot(est_abra_fun, "Abrangente 70-20")         
p_fun_f <- win_lose_plot(est_fun,      "Sem Filtro")

#p_med_r <- win_lose_plot(est_rest_med, "Restrito")   
p_med_a <- win_lose_plot(est_abra_med, "Abrangente 70-20")
p_med_f <- win_lose_plot(est_med,      "Sem Filtro")



# helper that returns a tiny ggplot containing the vertical label text
label_row <- function(text, size_pt = 10) {
  ggplot() +
    theme_void() +
    annotate("text",
             x = 0.5, y = 0.5,
             label = text,
             angle = 90,                # rotate vertical
             size = size_pt,            # ggplot size (about pts)
             fontface = "bold",
             hjust = 0.5, vjust = 0.5) +
    theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1))
}


# Create the right column with vertical labels (one per row)
labels_col <- label_row("Total",     size_pt = 6) /   # top row
  label_row("Infantil", size_pt = 6) /   # middle row
  label_row("Fundamental", size_pt = 6) /   # middle row
  label_row("Médio", size_pt = 6) # bottom row

# put the 12 plots in the exact left-to-right, top-to-bottom order
plots <- list(
  p_tot_a, p_tot_f,
  p_inf_a, p_inf_f,
  p_fun_a, p_fun_f,
  p_med_a, p_med_f
)

# normalize per-plot margins so they don't force reflow
normalize_margin <- function(p) {
  p + theme(plot.margin = grid::unit(c(2,2,2,2), "pt"))
}
plots <- lapply(plots, normalize_margin)

# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 4) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Estimates from feols(...) with i(k, ref = -1)")

print(final)


ggsave(                                                #Saving image
  filename = paste0("win_lose_enroll_filter.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Filter/",
  width = 1300/96, height = 820/96, dpi = 300
)

 rm(p_fun_a, p_fun_r, p_inf_a, p_inf_r, p_med_a, p_med_r, p_tot_a, p_tot_r, final, grid_plot,
    p_fun_f, p_inf_f, p_med_f, p_tot_f,
    est_abra_fun, est_abra_inf, est_abra_med, est_abra_tot, est_rest_fun, est_rest_inf,
    est_rest_med, est_rest_tot, est_fun, est_inf, est_med, est_tot)

# ---------------------------------------------------------------------------- #
## 5.3 Spending ----
# ---------------------------------------------------------------------------- #
### 5.3.1 Strict ----
# ---------------------------------------------------------------------------- #

est_rest_tot <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")


est_rest_inf <- feols(real_des_inf_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")



est_rest_fun <- feols(real_des_fun_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")



est_rest_med <- feols(real_des_med_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 flag_spend70 == 0 & flag_spendm20 == 0),
                      vcov = "hetero")



# ---------------------------------------------------------------------------- #
### 5.3.2 Broad ----
# ---------------------------------------------------------------------------- #


est_abra_tot <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")

est_abra_inf <- feols(real_des_inf_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")


est_abra_fun <- feols(real_des_fun_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")

est_abra_med <- feols(real_des_med_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano +uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")

# ---------------------------------------------------------------------------- #
### 5.3.3 No Filter ----
# ---------------------------------------------------------------------------- #


est_tot <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend,
                      vcov = "hetero")

est_inf <- feols(real_des_inf_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend,
                      vcov = "hetero")


est_fun <- feols(real_des_fun_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend,
                      vcov = "hetero")

est_med <- feols(real_des_med_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano +uf^ano,
                      data = df_spend,
                      vcov = "hetero")


# -------------------------------------------- #
### 5.3.4 Plot ----
# -------------------------------------------- #

p_tot_r <- win_lose_plot(est_rest_tot, "Restrito")    
p_tot_a <- win_lose_plot(est_abra_tot, "Abrangente 70-20")
p_tot_f <- win_lose_plot(est_tot,      "Sem Filtro")

p_inf_r <- win_lose_plot(est_rest_inf, "Restrito")    
p_inf_a <- win_lose_plot(est_abra_inf, "Abrangente 70-20")      
p_inf_f <- win_lose_plot(est_inf,      "Sem Filtro")

p_fun_r <- win_lose_plot(est_rest_fun, "Restrito")  
p_fun_a <- win_lose_plot(est_abra_fun, "Abrangente 70-20")         
p_fun_f <- win_lose_plot(est_fun,      "Sem Filtro")

p_med_r <- win_lose_plot(est_rest_med, "Restrito")   
p_med_a <- win_lose_plot(est_abra_med, "Abrangente 70-20")
p_med_f <- win_lose_plot(est_med,      "Sem Filtro")



# Create the right column with vertical labels (one per row)
labels_col <- label_row("Total",     size_pt = 6) /   # top row
  label_row("Infantil", size_pt = 6) /   # middle row
  label_row("Fundamental", size_pt = 6) /   # middle row
  label_row("Médio", size_pt = 6) # bottom row

# put the 12 plots in the exact left-to-right, top-to-bottom order
plots <- list(
  p_tot_a, p_tot_f,
  p_inf_a, p_inf_f,
  p_fun_a, p_fun_f,
  p_med_a, p_med_f
)

plots <- lapply(plots, normalize_margin)

# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 4) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Estimates from feols(...) with i(k, ref = -1)")

print(final)


ggsave(                                                #Saving image
  filename = paste0("win_lose_spend_filter.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Filter/",
  width = 1300/96, height = 820/96, dpi = 300
)

rm(p_fun_a, p_fun_r, p_inf_a, p_inf_r, p_med_a, p_med_r, p_tot_a, p_tot_r, final, grid_plot,
   p_fun_f, p_inf_f, p_med_f, p_tot_f,
   est_abra_fun, est_abra_inf, est_abra_med, est_abra_tot, est_rest_fun, est_rest_inf,
   est_rest_med, est_rest_tot, est_fun, est_inf, est_med, est_tot, plots)

# ---------------------------------------------------------------------------- #
## 5.4 Spending Lvl----
# ---------------------------------------------------------------------------- #
### 5.4.1 Filter ----
# ---------------------------------------------------------------------------- #


est_abra_tot <- feols(real_des_edu ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")

est_abra_inf <- feols(real_des_inf ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")


est_abra_fun <- feols(real_des_fund ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")

est_abra_med <- feols(real_des_med ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano +uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = "hetero")

# ---------------------------------------------------------------------------- #
### 5.3.3 No Filter ----
# ---------------------------------------------------------------------------- #


est_tot <- feols(real_des_edu ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 vcov = "hetero")

est_inf <- feols(real_des_inf ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 vcov = "hetero")


est_fun <- feols(real_des_fund ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 vcov = "hetero")

est_med <- feols(real_des_med ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                 | codigo_ibge + ano +uf^ano,
                 data = df_spend,
                 vcov = "hetero")


# -------------------------------------------- #
### 5.3.4 Plot ----
# -------------------------------------------- #

p_tot_a <- win_lose_plot(est_abra_tot, "Abrangente 70-20")
p_tot_f <- win_lose_plot(est_tot,      "Sem Filtro")

p_inf_a <- win_lose_plot(est_abra_inf, "Abrangente 70-20")      
p_inf_f <- win_lose_plot(est_inf,      "Sem Filtro")

p_fun_a <- win_lose_plot(est_abra_fun, "Abrangente 70-20")         
p_fun_f <- win_lose_plot(est_fun,      "Sem Filtro")

p_med_a <- win_lose_plot(est_abra_med, "Abrangente 70-20")
p_med_f <- win_lose_plot(est_med,      "Sem Filtro")



# Create the right column with vertical labels (one per row)
labels_col <- label_row("Total",     size_pt = 6) /   # top row
  label_row("Infantil", size_pt = 6) /   # middle row
  label_row("Fundamental", size_pt = 6) /   # middle row
  label_row("Médio", size_pt = 6) # bottom row

# put the 12 plots in the exact left-to-right, top-to-bottom order
plots <- list(
  p_tot_a, p_tot_f,
  p_inf_a, p_inf_f,
  p_fun_a, p_fun_f,
  p_med_a, p_med_f
)

plots <- lapply(plots, normalize_margin)

# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 4) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Estimates from feols(...) with i(k, ref = -1)")

print(final)


ggsave(                                                #Saving image
  filename = paste0("win_lose_spend_lvl_filter.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Filter/",
  width = 1300/96, height = 820/96, dpi = 300
)

rm(p_fun_a, p_fun_r, p_inf_a, p_inf_r, p_med_a, p_med_r, p_tot_a, p_tot_r, final, grid_plot,
   p_fun_f, p_inf_f, p_med_f, p_tot_f,
   est_abra_fun, est_abra_inf, est_abra_med, est_abra_tot, est_rest_fun, est_rest_inf,
   est_rest_med, est_rest_tot, est_fun, est_inf, est_med, est_tot, plots, labels_col, normalize_margin, label_row, win_lose_plot)


# ---------------------------------------------------------------------------- #
# 6. Broad and Strict Muns ----
# ---------------------------------------------------------------------------- #
#' I will stablish a list collecting the unique municipalities codes present in
#' each filtering to better select them in the future.

#1) Strict dataframe
df_rest <- df_spend %>% 
  #filter(ano < 2011) %>% 
  filter(dosage == 1 |
           flag_spend70 == 0 & flag_spendm20 == 0)


#2) Broad dataframe
df_abra <- df_spend %>%
  group_by (codigo_ibge) %>% 
  #filter(ano < 2011) %>% 
  filter(dosage == 1 |
           all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
  ungroup()


#3) Creating list
filter_list <- list()



#4) Adding the results

filter_list[["rest"]] <- unique(df_rest$codigo_ibge) #strict

filter_list[["abra"]] <- unique(df_abra$codigo_ibge) #broad

rm(df_rest, df_abra)

#----------------------------------------------------------------------------- #
# 7. School Infra ----
# ---------------------------------------------------------------------------- #


#' In this section I will breakdown the effects for various variables associated
#' with the daycare and preschool system.
#' 
#' First I will define the graph function for the remaining estimations


# ---------------------------------------------------------------------------- #
## 7.1 Data (Mun Lvl) ----
# ---------------------------------------------------------------------------- #

#Starting with the scholl data
df_school <- readRDS("Z:/Tuffy/Paper - Educ/Dados/censo_escolar_base_v2.rds") %>% 
  mutate(codmun = as.character(codmun %/% 10),
         new_psc = ifelse(pre_tot > 0, 1, 0),
         new_day = ifelse(day_tot > 0, 1, 0)
  )

#We will calculate the municipal exposure, weighted by the enrollment numbers
df_school <- df_school %>% 
  group_by(codmun, ano) %>% 
  summarise(
    total_enroll = sum(enroll, na.rm = T),
    total_presch = sum(pre_tot, na.rm = T),
    total_daycar = sum(day_tot, na.rm = T),
    
    #numeral variables
    class     = total_enroll / sum(classroom, na.rm = T),
    n_employee = sum(employee, na.rm = T),                 #New contracts
    employee  = total_enroll / n_employee,
    schools   = n(),
    
    
    #School characteristics
    exp_troom = sum(enroll*teach_room, na.rm = T)/total_enroll,
    exp_lab   = sum(enroll*lab_dummy, na.rm = T) /total_enroll,
    exp_lib   = sum(enroll*lib_dummy, na.rm = T) /total_enroll,
    exp_play  = sum(enroll*play_area, na.rm = T) /total_enroll,
    exp_lunch = sum(enroll*lunch, na.rm = T)     /total_enroll,
    
    #Courses Student Exposure
    exp_psch  = sum((pre_tot + day_tot)*kinder, na.rm = T)    /total_enroll,
    exp_elem  = sum(ef_tot*elementary, na.rm = T)/total_enroll,
    exp_high  = sum(em_tot*high, na.rm = T)      /total_enroll,
    exp_inc   = sum(esp_tot*inclusion, na.rm = T) /total_enroll,
    
    #Childhood exp
    child_psch = sum(pre_tot*preschool, na.rm = T),
    child_dayc = sum(day_tot*daycare, na.rm = T),
    
    new_child_psch = sum(pre_tot, na.rm = T),
    new_child_dayc = sum(day_tot, na.rm = T),
    
    #Numb. Schools
    n_daycare = sum(daycare, na.rm = T),
    n_preschl = sum(preschool, na.rm = T),
    
    new_n_daycare = sum(new_day, na.rm = T),
    new_n_preschl = sum(new_psc, na.rm = T),
    
    .groups = "drop"
  ) %>% 
  mutate(k = ano - 2007)


#Main regression dataframe
df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf_flags.rds") %>% 
  select(codigo_ibge, ano, uf, nome, dosage, aluno_dosage, PIBpc,
         des_edu_pc, des_fund_pc, des_med_pc, des_inf_pc) %>% 
  mutate(across(c(codigo_ibge, uf), as.character))


#Combining both databases
df_comb <- df_school %>% 
  left_join(df_reg %>%
              filter(ano %in% c(2005:2018)) %>%
              mutate(codigo_ibge = as.character(codigo_ibge)),
            by = c("codmun" = "codigo_ibge", "ano" = "ano")) %>% 
  
  #Separating for Winner and Loser
  mutate(grupo = case_when(
    dosage > 0 ~ "Winner",
    dosage < 0 ~ "Loser",
    TRUE ~ NA
  ),
  grupo = factor(grupo))


# ---------------------------------------------------------------------------- #
## 7.2 W vs. L Breakdown ----
# ---------------------------------------------------------------------------- #

#'Continung with the data, I will repeat the last estimations, while applying the
#'diferenciation within [Winners] vs. [Losers] in the data.

win_lose_plot <- function(est_obj, title = NULL, ylim = NULL) {
  # extract tidy data from fixest::etable or broom::tidy
  event_df <- broom::tidy(est_obj, conf.int = TRUE) %>% 
    mutate(
      ano = str_extract(term, "(?<=ano::)-?\\d+"),
      ano = as.numeric(ano),
      # Extract group (text after the last colon)
      grupo = str_extract(term, "[^:]+$"),
      grupo = as.factor(grupo))
  
  
  event_df <- event_df %>% 
    bind_rows(
      event_df %>%
        distinct(grupo) %>%
        mutate(
          term = paste0(as.character(grupo),"ano:2006"),     
          estimate = 0,
          std.error = 0,
          statistic = 0,
          p.value = 1,
          conf.low = 0,
          conf.high = 0,
          ano = 2006
        )
    ) %>% 
    filter(grupo != "PIBpc")
  
  ggplot(event_df, aes(x = ano, y = estimate, group = grupo)) +
    # shaded standard error area
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 2006, color = "black") +
    geom_point(aes(color = grupo), shape = 15, size = 2) +
    geom_line(aes(color = grupo)) +
    labs(
      title = title,
      x = "Ano"
    ) +
    coord_cartesian(ylim = ylim) +
    theme_classic() +
    theme(
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11)
    ) + 
    scale_x_continuous(breaks = seq(2005, 2018, 2))
  
}

# ---------------------------------------------------------------------------- #
### 7.2.1 Infra reg ----
# ---------------------------------------------------------------------------- #
#### 7.2.1.1 Broad ----
# ---------------------------------------------------------------------------- #

#Classroom
est_class <- feols( class ~ aluno_dosage : i(ano, grupo, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = "hetero")
#Teacher room
est_troom <- feols( exp_troom ~ aluno_dosage : i(ano, grupo, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = "hetero")
#Laboratory
est_labs <- feols(exp_lab ~ aluno_dosage : i(ano, grupo, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                  vcov = "hetero")

#Library
est_libra <- feols(exp_lib ~ aluno_dosage : i(ano, grupo, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                   vcov = "hetero")

#Play Area
est_play <- feols(exp_play ~ aluno_dosage : i(ano, grupo, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                  vcov = "hetero")

#Lunch
est_lunch <- feols(exp_lunch ~ aluno_dosage : i(ano, grupo, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                   vcov = "hetero")

#Employee 
est_employ <- feols(employee ~ aluno_dosage : i(ano, grupo, ref = 2006)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = "hetero")


est_n_employ <- feols(n_employee ~ aluno_dosage : i(ano, grupo, ref = 2006)
                      + PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                      vcov = "hetero")

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ, est_n_employ)

# --------------------------------- #
##### 7.2.1.1.1 Graph ----
# --------------------------------- #

# 3) Apply helper to each ggiplot object
p_class <- win_lose_plot(est_class, "Aluno/Salas de aula")   #Classroom
p_troom <- win_lose_plot(est_troom, "Exp. Sala dos Prof.")  #Teacher's Room
p_labs  <- win_lose_plot(est_labs,  "Exp. Laboratórios")    #Laboratory
p_libra <- win_lose_plot(est_libra, "Exp. Biblioteca")      #Library
p_play  <- win_lose_plot(est_play,  "Exp. Quadras/Parque")  #Play Area
p_lunch <- win_lose_plot(est_lunch, "Exp. Merenda")         #Lunch
p_employ <- win_lose_plot(est_employ,"Aluno/Funcionários")   #Employee
p_nemploy <- win_lose_plot(est_n_employ, "N° Funcionários")
blank <- ggplot() + theme_void()

grid_plot <- ( p_troom + p_labs + p_libra) /
  (p_play + p_lunch + blank) /
  (p_class   + p_employ + p_nemploy)

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("winlose_school_infra_abra.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Filter/", #Saving directly to the report
  width = 1300/96, height = 720/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final, est_class, est_employ, est_labs, est_libra, est_lunch, est_play,
   est_troom)


# ---------------------------------------------------------------------------- #
#### 7.2.1.2 No Filter ----
# ---------------------------------------------------------------------------- #

#Classroom
est_class <- feols( class ~ aluno_dosage : i(ano, grupo, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Teacher room
est_troom <- feols( exp_troom ~ aluno_dosage : i(ano, grupo, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Laboratory
est_labs <- feols(exp_lab ~ aluno_dosage : i(ano, grupo, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Library
est_libra <- feols(exp_lib ~ aluno_dosage : i(ano, grupo, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Play Area
est_play <- feols(exp_play ~ aluno_dosage : i(ano, grupo, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Lunch
est_lunch <- feols(exp_lunch ~ aluno_dosage : i(ano, grupo, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Employee 
est_employ <- feols(employee ~ aluno_dosage : i(ano, grupo, ref = 2006)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")


est_n_employ <- feols(n_employee ~ aluno_dosage : i(ano, grupo, ref = 2006)
                      + PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb,
                      vcov = "hetero")

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ, est_n_employ)

# --------------------------------- #
##### 7.2.1.2.1 Graph ----
# --------------------------------- #

# 3) Apply helper to each ggiplot object
p_class <- win_lose_plot(est_class, "Aluno/Salas de aula")   #Classroom
p_troom <- win_lose_plot(est_troom, "Exp. Sala dos Prof.")  #Teacher's Room
p_labs  <- win_lose_plot(est_labs,  "Exp. Laboratórios")    #Laboratory
p_libra <- win_lose_plot(est_libra, "Exp. Biblioteca")      #Library
p_play  <- win_lose_plot(est_play,  "Exp. Quadras/Parque")  #Play Area
p_lunch <- win_lose_plot(est_lunch, "Exp. Merenda")         #Lunch
p_employ <- win_lose_plot(est_employ,"Aluno/Funcionários")   #Employee
p_nemploy <- win_lose_plot(est_n_employ, "N° Funcionários")
blank <- ggplot() + theme_void()

grid_plot <- ( p_troom + p_labs + p_libra) /
  (p_play + p_lunch + blank) /
  (p_class   + p_employ + p_nemploy)

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("winlose_school_infra_nofilter.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Filter/", #Saving directly to the report
  width = 1300/96, height = 720/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final, est_class, est_employ, est_labs, est_libra, est_lunch, est_play,
   est_troom)


# ---------------------------------------------------------------------------- #
## 7.3 Regression ----
# ---------------------------------------------------------------------------- #

event_plot <- function(est_obj, title = NULL, ylim = NULL) {
  # extract tidy data from fixest::etable or broom::tidy
  event_df <- broom::tidy(est_obj, conf.int = TRUE) %>% 
    mutate(
      ano = str_extract(term, "(?<=ano::)-?\\d+"),
      ano = as.numeric(ano)
      # Extract group (text after the last colon)
      )
  
  
  
  event_df <- event_df %>% 
    bind_rows(
      event_df %>%
        mutate(
          term = "ano:2006",     
          estimate = 0,
          std.error = 0,
          statistic = 0,
          p.value = 1,
          conf.low = 0,
          conf.high = 0,
          ano = 2006
        ))
  
  
  ggplot(event_df, aes(x = ano, y = estimate)) +
    # shaded standard error area
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                fill = "grey60", alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 2006, color = "black") +
    geom_point(shape = 15, size = 2, color = "black") +
    geom_line(color = "black") +
    labs(
      title = title,
      x = "Ano"
    ) +
    coord_cartesian(ylim = ylim) +
    theme_classic() +
    theme(
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11)
    ) + 
    scale_x_continuous(breaks = seq(2005, 2018, 2))
}

# ---------------------------------------------------------------------------- #
### 7.3.1 Infra reg ----
# ---------------------------------------------------------------------------- #
#### 7.3.1.1 Broad ----
# ---------------------------------------------------------------------------- #

#Classroom
est_class <- feols( class ~ aluno_dosage : i(ano, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = "hetero")
#Teacher room
est_troom <- feols( exp_troom ~ aluno_dosage : i(ano, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = "hetero")
#Laboratory
est_labs <- feols(exp_lab ~ aluno_dosage : i(ano, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                  vcov = "hetero")

#Library
est_libra <- feols(exp_lib ~ aluno_dosage : i(ano, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                   vcov = "hetero")

#Play Area
est_play <- feols(exp_play ~ aluno_dosage : i(ano, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                  vcov = "hetero")

#Lunch
est_lunch <- feols(exp_lunch ~ aluno_dosage : i(ano, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                   vcov = "hetero")

#Employee 
est_employ <- feols(employee ~ aluno_dosage : i(ano, ref = 2006)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = "hetero")


est_n_employ <- feols(n_employee ~ aluno_dosage : i(ano, ref = 2006)
                      + PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                      vcov = "hetero")

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ, est_n_employ)

# --------------------------------- #
##### 7.2.1.1.1 Graph ----
# --------------------------------- #

# 3) Apply helper to each ggiplot object
p_class <- event_plot(est_class, "Aluno/Salas de aula")   #Classroom
p_troom <- event_plot(est_troom, "Exp. Sala dos Prof.")  #Teacher's Room
p_labs  <- event_plot(est_labs,  "Exp. Laboratórios")    #Laboratory
p_libra <- event_plot(est_libra, "Exp. Biblioteca")      #Library
p_play  <- event_plot(est_play,  "Exp. Quadras/Parque")  #Play Area
p_lunch <- event_plot(est_lunch, "Exp. Merenda")         #Lunch
p_employ <- event_plot(est_employ,"Aluno/Funcionários")   #Employee
p_nemploy <- event_plot(est_n_employ, "N° Funcionários")
blank <- ggplot() + theme_void()

grid_plot <- ( p_troom + p_labs + p_libra) /
  (p_play + p_lunch + blank) /
  (p_class   + p_employ + p_nemploy)

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("school_infra_abra.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Filter/", #Saving directly to the report
  width = 1300/96, height = 720/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final, est_class, est_employ, est_labs, est_libra, est_lunch, est_play,
   est_troom)


# ---------------------------------------------------------------------------- #
#### 7.2.1.2 No Filter ----
# ---------------------------------------------------------------------------- #

#Classroom
est_class <- feols( class ~ aluno_dosage : i(ano, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Teacher room
est_troom <- feols( exp_troom ~ aluno_dosage : i(ano, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Laboratory
est_labs <- feols(exp_lab ~ aluno_dosage : i(ano, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Library
est_libra <- feols(exp_lib ~ aluno_dosage : i(ano, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Play Area
est_play <- feols(exp_play ~ aluno_dosage : i(ano, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Lunch
est_lunch <- feols(exp_lunch ~ aluno_dosage : i(ano, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Employee 
est_employ <- feols(employee ~ aluno_dosage : i(ano, ref = 2006)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")


est_n_employ <- feols(n_employee ~ aluno_dosage : i(ano, ref = 2006)
                      + PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb,
                      vcov = "hetero")

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ, est_n_employ)

# --------------------------------- #
##### 7.2.1.2.1 Graph ----
# --------------------------------- #

# 3) Apply helper to each ggiplot object
p_class <- event_plot(est_class, "Aluno/Salas de aula")   #Classroom
p_troom <- event_plot(est_troom, "Exp. Sala dos Prof.")  #Teacher's Room
p_labs  <- event_plot(est_labs,  "Exp. Laboratórios")    #Laboratory
p_libra <- event_plot(est_libra, "Exp. Biblioteca")      #Library
p_play  <- event_plot(est_play,  "Exp. Quadras/Parque")  #Play Area
p_lunch <- event_plot(est_lunch, "Exp. Merenda")         #Lunch
p_employ <- event_plot(est_employ,"Aluno/Funcionários")   #Employee
p_nemploy <- event_plot(est_n_employ, "N° Funcionários")
blank <- ggplot() + theme_void()

grid_plot <- ( p_troom + p_labs + p_libra) /
  (p_play + p_lunch + blank) /
  (p_class   + p_employ + p_nemploy)

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("school_infra_nofilter.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Filter/", #Saving directly to the report
  width = 1300/96, height = 720/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final, est_class, est_employ, est_labs, est_libra, est_lunch, est_play,
   est_troom)


rm(df_comb, df_reg, df_school, df_spend, est_n_employ, p_nemploy, data)
# ---------------------------------------------------------------------------- #
# 8. SAEB ----
# ---------------------------------------------------------------------------- #
## 8.1 Data ----
# ---------------------------------------------------------------------------- #
#Saeb dataframe
df_saeb <- readRDS( "Z:/Tuffy/Paper - Educ/Dados/saeb_nvl_aluno.rds") %>%
  mutate(codmun = ifelse(ano >= 2007, as.numeric(codmun) %/% 10, codmun),
         codmun = as.character(codmun)) %>%   #Arranging for the older municipal codes
  filter(codmun < 600000)



#Main regression dataframe
df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") %>% 
  select(codigo_ibge, ano, uf, nome, dif_coef_pp, dosage, aluno_dosage, PIBpc) %>% 
  mutate(across(c(codigo_ibge, uf), as.character))

# ---------------------------------------------------------------------------- #
### 8.1.1 Merge ----
# ---------------------------------------------------------------------------- #

#Combining both dataframes
df <- df_saeb %>% 
  left_join(df_reg %>%
              mutate(codigo_ibge = as.character(codigo_ibge)),
            by = c("codmun" = "codigo_ibge", "ano" = "ano")) %>% 
  mutate(
    sexo = as.numeric(sexo),
    raca = as.numeric(raca),
    mae_educ = as.numeric(mae_educ)
  ) %>% 
  mutate(age_exp = ifelse(treat_exp > 1, 1, treat_exp), #Based on Carrillo
         
         anos_exp = ano - 2007,
         anos_exp = ifelse(grade == 9, anos_exp - 2, anos_exp), #Adjsutment for the 9th grade
         
         ano_nasc = ano - (idade), #Birth year as reference
         
         peso = as.numeric(peso), #Weights
         
         age_late = case_when(
           grade == 5 & idade > 10 ~ 1, #Is older than the ideal age
           grade == 9 & idade > 14 ~ 1,  #Is older than the ideal age
           TRUE ~ 0
         ),
         age_dist = ifelse(idade_aux == 0, 1, 0), #Age distortion
         
         post_treat = ifelse(ano > 2007, 1, 0),
         
         nasc_post  = ifelse(ano_nasc >= 2002, 1, 0),
         
         rob_winner_dummy = ifelse(dosage > 0, 1, 0),
         
         treatment_expo = case_when( #Treatment exposition
           ano_nasc <= 2000 ~ 0,
           ano_nasc == 2001 ~ 1,
           ano_nasc == 2002 ~ 2,
           ano_nasc >= 2003 ~ 3,
           TRUE ~ NA
         ),
         
         #Separating the Brazilian Regions
         region = case_when(
           as.numeric(codmun) %/% 100000 == 1 ~ "Norte",        #North
           as.numeric(codmun) %/% 100000 == 2 ~ "Nordeste",     #Northeast
           as.numeric(codmun) %/% 100000 == 3 ~ "Sudeste",      #Southeast
           as.numeric(codmun) %/% 100000 == 4 ~ "Sul",          #South
           as.numeric(codmun) %/% 100000 == 5 ~ "Centro-Oeste", #Central-West
           TRUE ~ NA
         )
  ) %>% 
  mutate(grupo = case_when(
    dosage > 0 ~ "Winner",   # net beneficiary
    dosage < 0 ~ "Loser",   # net contributer
    TRUE ~ NA_character_),
    
    grupo = factor(grupo, levels = c("Loser", "Winner")) #Beneficiary dumm
  ) %>% 
  filter(as.numeric(ano_nasc) < 2008) %>% # This remove younger than expected people in 2017  -> ideal age = 10.
  mutate(ano_nasc = as.factor(ano_nasc))


# ---------------------------------------------------------------------------- #
## 8.2 Schools ----
# ---------------------------------------------------------------------------- #
### 8.2.1 Data ----
# ---------------------------------------------------------------------------- #

#Creating a dataframe for each school
df_mun_school <- df %>% 
  mutate(uf = as.numeric(codmun) %/% 10000) %>% 
  #Grouping by School
  group_by(codmun, cod_escola, grupo, ano, id_uf) %>% 
  summarise(students = n(),
            students_peso = sum(peso, na.rm = T),
            students_mpes = sum(peso_mt, na.rm = T),
            students_ppes = sum(peso_lp, na.rm= T),
            .groups = "drop") %>% 
  arrange(codmun, ano, id_uf) %>% 
  mutate_all(as.numeric) %>% 
  #Grouping by Municipality
  group_by(codmun, ano, grupo, id_uf) %>% 
  summarise(total_students = sum(students),
            total_students_peso = sum(students_peso),
            total_students_mpes = sum(students_mpes), #using the math weight as reference
            total_students_ppes = sum(students_ppes),
            schools = n(),
            .groups = "drop") %>% 
  arrange(codmun, ano, id_uf) %>% 
  #Filtering municipalities that appear every year
  group_by(codmun, ano) %>% 
  mutate(aux1 = 1) %>% 
  ungroup(ano) %>% 
  mutate(aux2 = sum(aux1, na.rm = T)) %>% 
  filter(aux2 == 7) %>% #Appears in every year
  select(-c(aux1, aux2))



#Filters
filter_saeb <- list()
filter_saeb[["in_saeb"]] <- as.character(unique(df_mun_school$codmun)) #List of municipalities present in all SAEB
filter_saeb[["win"]]     <- unique((df %>% filter(grupo == "Winner"))$codmun)


df_mun_school <- df_mun_school %>% 
  mutate(grupo = ifelse(as.character(codmun %in% filter_saeb[["win"]]), "Winner", "Loser"))


# ---------------------------------------------------------------------------- #
### 8.2.2 Regression ----
# ---------------------------------------------------------------------------- #
#### 8.2.2.1 Combined ----
# ---------------------------------------------------------------------------- #

est_scho <- feols(schools ~ i(ano, ref = 2007), #FE
                  data = df_mun_school, #Only 5h grade
                  vcov = "hetero")



est_stu <- feols(total_students_mpes ~ i(ano, ref = 2007), #FE
                 data = df_mun_school, #Only 5h grade
                 vcov = "hetero")



#Filtered regressions
est_schoa <- feols(schools ~ i(ano, ref = 2007), #FE
                   data = df_mun_school %>% filter(codmun %in% filter_list[["abra"]]), #Only 5h grade
                   vcov = "hetero")



est_stua <- feols(total_students_mpes ~ i(ano, ref = 2007), #FE
                  data = df_mun_school %>% filter(codmun %in% filter_list[["abra"]]), #Only 5h grade
                  vcov = "hetero")

etable(est_scho, est_schoa, est_stu, est_stua,
       vcov = "hetero",
       headers = list(":_:" = list("N° Escolas" = 1, "N° Escolas - C/F",
                                   #"Fundamental" = 1, "Médio" = 1,
                                   "N° Alunos" = 1, "N° Alunos - C/F")),
       drop = "Constant",
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/saeb_breakdown.tex",
       replace = TRUE)

# ---------------------------------------------------------------------------- #
#### 8.2.2.2 Win Lose ----
# ---------------------------------------------------------------------------- #
est_scho <- feols(schools ~ i(ano, grupo, ref = 2007), #FE
                  data = df_mun_school, #Only 5h grade
                  vcov = "hetero")



est_stu <- feols(total_students_mpes ~ i(ano, grupo, ref = 2007), #FE
                 data = df_mun_school, #Only 5h grade
                 vcov = "hetero")



#Filtered regressions
est_schoa <- feols(schools ~ i(ano, grupo, ref = 2007), #FE
                  data = df_mun_school %>% filter(codmun %in% filter_list[["abra"]]), #Only 5h grade
                  vcov = "hetero")



est_stua <- feols(total_students_mpes ~ i(ano, grupo, ref = 2007), #FE
                 data = df_mun_school %>% filter(codmun %in% filter_list[["abra"]]), #Only 5h grade
                 vcov = "hetero")

etable(est_scho, est_stu, est_schoa, est_stua)

# ---------------------------------------------------------------------------- #
#### 8.2.2.3 Graph ----
# ---------------------------------------------------------------------------- #

win_lose_plot <- function(est_obj, title = NULL, ylim = NULL) {
  # extract tidy data from fixest::etable or broom::tidy
  event_df <- broom::tidy(est_obj, conf.int = TRUE) %>% 
    mutate(
      ano = str_extract(term, "(?<=ano::)-?\\d+"),
      ano = as.numeric(ano),
      # Extract group (text after the last colon)
      grupo = str_extract(term, "[^:]+$"),
      grupo = as.factor(grupo))
  
  
  event_df <- event_df %>% 
    bind_rows(
      event_df %>%
        distinct(grupo) %>%
        mutate(
          term = paste0(as.character(grupo),"ano:2007"),     
          estimate = 0,
          std.error = 0,
          statistic = 0,
          p.value = 1,
          conf.low = 0,
          conf.high = 0,
          ano = 2007
        )
    ) %>% 
    filter(grupo != "PIBpc",
           grupo != "(Intercept)")
  
  ggplot(event_df, aes(x = ano, y = estimate, group = grupo)) +
    # shaded standard error area
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 2007, color = "black") +
    geom_point(aes(color = grupo), shape = 15, size = 2) +
    geom_line(aes(color = grupo)) +
    labs(
      title = title,
      x = "Ano"
    ) +
    coord_cartesian(ylim = ylim) +
    theme_classic() +
    theme(
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11)
    ) + 
    scale_x_continuous(breaks = seq(2005, 2018, 2))
  
}


p_school <- win_lose_plot(est_scho, "Escolas")   #No Filter
p_studen <- win_lose_plot(est_stu, "Alunos")  #No filter
p_schoola <- win_lose_plot(est_schoa, "Escolas")
p_studena <- win_lose_plot(est_stua, "Alunos")  

# helper that returns a tiny ggplot containing the vertical label text
label_row <- function(text, size_pt = 10) {
  ggplot() +
    theme_void() +
    annotate("text",
             x = 0.5, y = 0.5,
             label = text,
             angle = 90,                # rotate vertical
             size = size_pt,            # ggplot size (about pts)
             fontface = "bold",
             hjust = 0.5, vjust = 0.5) +
    theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1))
}


# Create the right column with vertical labels (one per row)
labels_col <- label_row("Abrangente",     size_pt = 6) /   # top row
  label_row("Sem Filtro", size_pt = 6) 

# put the 12 plots in the exact left-to-right, top-to-bottom order
plots <- list(
  p_schoola, p_studena, p_school, p_studen
)

# normalize per-plot margins so they don't force reflow
normalize_margin <- function(p) {
  p + theme(plot.margin = grid::unit(c(2,2,2,2), "pt"))
}
plots <- lapply(plots, normalize_margin)

# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 2) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Estimates from feols(...) with i(k, ref = -1)")

print(final)




ggsave( #Saving image
  filename = paste0("winlose_saeb_breakdown.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Filter/", #Saving directly to the report
  width = 1000/96, height = 620/96, dpi = 300
)

rm(final, grid_plot, plots, p_school, p_schoola, p_studen, p_studena, 
   est_scho, est_schoa, est_stu, est_stua, label_row, label_col, normalize_margin)

# ---------------------------------------------------------------------------- #
## 8.3 Grade ----
# ---------------------------------------------------------------------------- #
### 8.3.1 Regressions ----
# ---------------------------------------------------------------------------- #
#### 8.3.1.1 No Filter ----
# ---------------------------------------------------------------------------- #
#Weights dataframes
df_w5 <- df %>% filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]) %>% select(peso, peso_mt, peso_lp)  #5th grade
df_w9 <- df %>% filter(grade == 9) %>% select(peso, peso_mt, peso_lp)  #9th grade


df_w2 <- df %>% 
  filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]) %>% 
  filter(codmun %in% filter_list[["abra"]]) %>% select(peso, peso_mt, peso_lp)


main_mat_f <- feols(as.numeric(profic_mat) ~ aluno_dosage : i(ano, grupo, ref = 2007)
                  + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]), #Only 5h grade
                  weights = df_w5$peso_mt,
                  vcov = "hetero")

main_pot_f <- feols(as.numeric(profic_port) ~ aluno_dosage : i(ano, grupo, ref = 2007)
                  + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]), #Only 5h grade
                  weights = df_w5$peso_lp,
                  vcov = "hetero")


etable(main_mat_f, main_pot_f)

# ---------------------------------------------------------------------------- #
#### 8.3.1.3 Broad ----
# ---------------------------------------------------------------------------- #

main_mat_a <- feols(as.numeric(profic_mat) ~ aluno_dosage : i(ano, grupo, ref = 2007)
                    + PIBpc #Controls
                    | codmun + ano + uf^ano, #FE
                    data = df %>% filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]) %>% 
                      filter(codmun %in% filter_list[["abra"]]), #Only 5h grade
                    weights = df_w2$peso_mt,
                    vcov = "hetero")

main_pot_a <- feols(as.numeric(profic_port) ~ aluno_dosage : i(ano, grupo, ref = 2007)
                    + PIBpc #Controls
                    | codmun + ano + uf^ano, #FE
                    data = df %>% filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]) %>% 
                      filter(codmun %in% filter_list[["abra"]]), #Only 5h grade
                    weights = df_w2$peso_lp,
                    vcov = "hetero")


etable(main_mat_a, main_pot_a)

# ---------------------------------------------------------------------------- #
#### 8.3.1.2 Graph ----
# ---------------------------------------------------------------------------- #

win_lose_plot <- function(est_obj, title = NULL, ylim = NULL) {
  # extract tidy data from fixest::etable or broom::tidy
  event_df <- broom::tidy(est_obj, conf.int = TRUE) %>% 
    mutate(
      ano = str_extract(term, "(?<=ano::)-?\\d+"),
      ano = as.numeric(ano),
      # Extract group (text after the last colon)
      grupo = str_extract(term, "[^:]+$"),
      grupo = as.factor(grupo))
  
  
  event_df <- event_df %>% 
    bind_rows(
      event_df %>%
        distinct(grupo) %>%
        mutate(
          term = paste0(as.character(grupo),"ano:2007"),     
          estimate = 0,
          std.error = 0,
          statistic = 0,
          p.value = 1,
          conf.low = 0,
          conf.high = 0,
          ano = 2007
        )
    ) %>% 
    filter(grupo != "PIBpc",
           grupo != "(Intercept)")
  
  ggplot(event_df, aes(x = ano, y = estimate, group = grupo)) +
    # shaded standard error area
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 2007, color = "black") +
    geom_point(aes(color = grupo), shape = 15, size = 2) +
    geom_line(aes(color = grupo)) +
    labs(
      title = title,
      x = "Ano"
    ) +
    coord_cartesian(ylim = ylim) +
    theme_classic() +
    theme(
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11)
    ) + 
    scale_x_continuous(breaks = seq(2005, 2018, 2))
  
}


p_mat_f <- win_lose_plot(main_mat_f, "Matemática")   #Classroom
p_pot_f <- win_lose_plot(main_pot_f, "Português")  #Teacher's Room

p_mat_a <- win_lose_plot(main_mat_a, "Matemática")   #Classroom
p_pot_a <- win_lose_plot(main_pot_a, "Português")  #Teacher's Room


# helper that returns a tiny ggplot containing the vertical label text
label_row <- function(text, size_pt = 10) {
  ggplot() +
    theme_void() +
    annotate("text",
             x = 0.5, y = 0.5,
             label = text,
             angle = 90,                # rotate vertical
             size = size_pt,            # ggplot size (about pts)
             fontface = "bold",
             hjust = 0.5, vjust = 0.5) +
    theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1))
}


# Create the right column with vertical labels (one per row)
labels_col <- label_row("Abrangente",     size_pt = 6) /   # top row
  label_row("Sem Filtro", size_pt = 6) 

# put the 12 plots in the exact left-to-right, top-to-bottom order
plots <- list(
p_mat_a, p_pot_a, p_mat_f, p_pot_f
)

# normalize per-plot margins so they don't force reflow
normalize_margin <- function(p) {
  p + theme(plot.margin = grid::unit(c(2,2,2,2), "pt"))
}
plots <- lapply(plots, normalize_margin)

# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 2) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Estimates from feols(...) with i(k, ref = -1)")

print(final)

ggsave( #Saving image
  filename = paste0("winlose_saeb_notas.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Filter/", #Saving directly to the report
  width = 1000/96, height = 620/96, dpi = 300
)

rm(plots, labels_col, p_mat_a, p_mat_f, p_pot_a, p_pot_f, main_mat_a, final,
   main_mat_f, main_pot_a, main_pot_f, df_w2, df_w5, df_w9)

#----------------------------------------------------------------------------- #
### 8.3.2 Regression Comb ----
# ---------------------------------------------------------------------------- #


#Weights dataframes
df_w5 <- df %>% filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]) %>% select(peso, peso_mt, peso_lp)  #5th grade
df_w9 <- df %>% filter(grade == 9) %>% select(peso, peso_mt, peso_lp)  #9th grade


df_w2 <- df %>% 
  filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]) %>% 
  filter(codmun %in% filter_list[["abra"]]) %>% select(peso, peso_mt, peso_lp)


main_mat_f <- feols(as.numeric(profic_mat) ~ aluno_dosage : i(ano, ref = 2007)
                    + PIBpc #Controls
                    | codmun + ano + uf^ano, #FE
                    data = df %>% filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]), #Only 5h grade
                    weights = df_w5$peso_mt,
                    vcov = "hetero")

main_pot_f <- feols(as.numeric(profic_port) ~ aluno_dosage : i(ano, ref = 2007)
                    + PIBpc #Controls
                    | codmun + ano + uf^ano, #FE
                    data = df %>% filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]), #Only 5h grade
                    weights = df_w5$peso_lp,
                    vcov = "hetero")


etable(main_mat_f, main_pot_f)



main_mat_a <- feols(as.numeric(profic_mat) ~ aluno_dosage : i(ano, ref = 2007)
                    + PIBpc #Controls
                    | codmun + ano + uf^ano, #FE
                    data = df %>% filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]) %>% 
                      filter(codmun %in% filter_list[["abra"]]), #Only 5h grade
                    weights = df_w2$peso_mt,
                    vcov = "hetero")

main_pot_a <- feols(as.numeric(profic_port) ~ aluno_dosage : i(ano, ref = 2007)
                    + PIBpc #Controls
                    | codmun + ano + uf^ano, #FE
                    data = df %>% filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]) %>% 
                      filter(codmun %in% filter_list[["abra"]]), #Only 5h grade
                    weights = df_w2$peso_lp,
                    vcov = "hetero")


etable(main_mat_a, main_pot_a)



etable(main_mat_f, main_mat_a, main_pot_f, main_pot_a,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,
                                   "Matemática - C/F" = 1, "Português" = 1,
                                   "Português - C/F" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Spend/saeb_grades.tex",
       replace = TRUE)








