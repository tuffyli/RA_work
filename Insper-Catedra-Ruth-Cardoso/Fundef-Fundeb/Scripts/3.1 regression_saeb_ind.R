# ---------------------------------------------------------------------------- #
# Regressions
# Last edited by: Tuffy Licciardi Issa
# Date: 05/11/2025
# ---------------------------------------------------------------------------- #

#' *************************************************************************** #
#' The main objective of this code is to recreate the specifications utilized in
#' the reference work of Rocha and others. Thus, regarding the SAEB exam scores 
#' I will estimate the effects on the stacked cross-section of students acording
#' to each exposure to treatment measurement.
#' ********************************************************************** #



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


#Deactivating scientific notation
options(scipen = 999)

# ---------------------------------------------------------------------------- #
# 1. Data ----
# ---------------------------------------------------------------------------- #

# ------------------- #
## 1.1 Extraction ----
#Saeb dataframe
df_saeb <- readRDS( "Z:/Tuffy/Paper - Educ/Dados/saeb_nvl_aluno.rds") %>%
mutate(codmun = ifelse(ano >= 2007, as.numeric(codmun) %/% 10, codmun),
       codmun = as.character(codmun)) %>%   #Arranging for the older municipal codes
filter(codmun < 600000)



#Main regression dataframe
df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") %>% 
  select(codigo_ibge, ano, uf, nome, dif_coef_pp, dosage, aluno_dosage, PIBpc) %>% 
  mutate(across(c(codigo_ibge, uf), as.character))
  
# ------------------ #
# 1.2 Merge ----

#Combining both dataframes
df <- df_saeb %>% 
  left_join(df_reg %>%
              filter(ano %in% c(2005:2017)) %>%
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
         
         
         peso = as.numeric(peso), #Weights
         
         age_late = case_when(
           grade == 5 & idade > 10 ~ 1, #Is older than the ideal age
           grade == 9 & idade > 14 ~ 1,  #Is older than the ideal age
           TRUE ~ 0
         ),
         age_dist = ifelse(idade_aux == 0, 1, 0), #Age distortion
         
         post_treat = ifelse(ano > 2007, 1, 0)
         )



# ---------------------------------------------------------------------------- #
# 2. Main Regression ----
# ---------------------------------------------------------------------------- #
## 2.1 Dosage ----
# ------------------ #
### 2.1.1 Age exp ----

main_mat <- feols(as.numeric(profic_mat) ~ dosage : age_exp : i(ano, ref = 2007)
                              + sexo + raca + mae_educ + PIBpc + idade #Controls
                              | codmun + ano + uf^ano, #FE
                              data = df %>% filter(grade == 5), #Only 5h grade
                              #weights = df_saeb$peso_5,
                              vcov = "hetero")

main_pot <- feols(as.numeric(profic_port) ~ dosage : age_exp : i(ano, ref = 2007)
                  + sexo + raca + mae_educ + PIBpc + idade #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

etable(main_mat, main_pot)


etable(main_mat, main_pot,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage/nvl_individuo_age_exp_agregado.tex", replace = TRUE)



# ----------------- #
### 2.1.2 Years exp ----

main_mat <- feols(as.numeric(profic_mat) ~ dosage : i(anos_exp, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df %>% filter(grade == 5) %>% select(peso),
                  vcov = "hetero")

main_pot <- feols(as.numeric(profic_port) ~ dosage : i(anos_exp, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df %>% filter(grade == 5) %>% select(peso),
                  vcov = "hetero")


sec_mat <- feols(as.numeric(profic_mat) ~ dosage : i(anos_exp, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc 
                 | codmun + ano + uf^ano,
                 data = df %>% filter(grade == 9),
                 #weights = df$peso,
                 vcov = "hetero")



sec_pot <- feols(as.numeric(profic_port) ~ dosage : i(anos_exp, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc 
                 | codmun + ano + uf^ano,
                 data = df %>% filter(grade == 9),
                 #weights = df$peso,
                 vcov = "hetero")

etable(main_mat, main_pot,
  sec_mat, sec_pot)



etable(main_mat, main_pot,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage/nvl_individuo_anos_exp.tex", replace = TRUE)


rm(main_mat, main_pot, sec_mat, sec_pot)

# -------------------------------- #
## 2.2 Aluno Dosage ----
# -------------------------------- #

# ------------------ #
### 2.2.1 Age exp ----

main_mat <- feols(as.numeric(profic_mat) ~ aluno_dosage : age_exp
                  + sexo + raca + mae_educ + PIBpc   #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

main_pot <- feols(as.numeric(profic_port) ~ aluno_dosage : age_exp
                  + sexo + raca + mae_educ + PIBpc  #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

etable(main_mat, main_pot)


etable(main_mat, main_pot,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/nvl_individuo_age_exp_agregado.tex", replace = TRUE)



# ----------------- #
### 2.2.2 Years exp ----

main_mat <- feols(as.numeric(profic_mat) ~ aluno_dosage : i(anos_exp, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df %>% filter(grade == 5) %>% select(peso),
                  vcov = "hetero")

main_pot <- feols(as.numeric(profic_port) ~ aluno_dosage : i(anos_exp, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df %>% filter(grade == 5) %>% select(peso),
                  vcov = "hetero")


sec_mat <- feols(as.numeric(profic_mat) ~ aluno_dosage : i(anos_exp, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc 
                 | codmun + ano + uf^ano,
                 data = df %>% filter(grade == 9),
                 #weights = df$peso,
                 vcov = "hetero")



sec_pot <- feols(as.numeric(profic_port) ~ aluno_dosage : i(anos_exp, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc 
                 | codmun + ano + uf^ano,
                 data = df %>% filter(grade == 9),
                 #weights = df$peso,
                 vcov = "hetero")

etable(main_mat, main_pot,
       sec_mat, sec_pot)



etable(main_mat, main_pot,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/nvl_individuo_anos_exp.tex", replace = TRUE)


rm(main_mat, main_pot, sec_mat, sec_pot)

# ---------------------------------------------------------------------------- #
# 3. Age vs. Grade Distortion ----
# ---------------------------------------------------------------------------- #

#' Here I will investigate the effect of the FUNDEB policy in the grade-age dis-
#' tortion dummy variable. Where the ideal age-grade equals 0, while the distor-
#' tion is equal 1

# ---------------- #
## 3.1 Dosage ----

main_dist <- feols(as.numeric(age_late) ~ dosage : i(anos_exp, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df %>% filter(grade == 5) %>% select(peso),
                  vcov = "hetero")


sec_dist <- feols(as.numeric(age_late) ~ dosage : i(anos_exp, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc 
                 | codmun + ano + uf^ano,
                 data = df %>% filter(grade == 9),
                 #weights = df$peso,
                 vcov = "hetero")

etable(main_dist, sec_dist,
       vcov = "hetero",
       headers = list(":_:" = list("5°Ano" = 1,"9°Ano" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage/dist_individuo_anos_exp.tex", replace = TRUE)

# ---------------- #
## 3.2 Aluno Dosage ----

main_dist <- feols(as.numeric(age_late) ~ aluno_dosage : i(anos_exp, ref = 0)
                   + sexo + raca + mae_educ + idade + PIBpc #Controls
                   | codmun + ano + uf^ano, #FE
                   data = df %>% filter(grade == 5), #Only 5h grade
                   #weights = df %>% filter(grade == 5) %>% select(peso),
                   vcov = "hetero")


sec_dist <- feols(as.numeric(age_late) ~ aluno_dosage : i(anos_exp, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc 
                  | codmun + ano + uf^ano,
                  data = df %>% filter(grade == 9),
                  #weights = df$peso,
                  vcov = "hetero")


etable(main_dist, sec_dist)
etable(main_dist, sec_dist,
       vcov = "hetero",
       headers = list(":_:" = list("5°Ano" = 1,"9°Ano" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/dist_individuo_anos_exp.tex", replace = TRUE)



rm(main_dist, sec_dist)
# ---------------------------------------------------------------------------- #
# 4. Above vs. Below ----
# ---------------------------------------------------------------------------- #
#' Here I will create an auxiliar dummy indicating if the municipality is placed
#' into the group of beneficiary or contributer

df <- df %>% 
  mutate(grupo = case_when(
    dosage > 0 ~ "Above",   # net beneficiary
    dosage < 0 ~ "Below",   # net contributer
    TRUE ~ NA_character_
    ),
    grupo = factor(grupo, levels = c("Below", "Above"))) #Beneficiary dummy

## 4.1 Ref. Specification ----
#' This specification is more similar to the Rudi Rocha reference paper, regar-
#' ding the health spending policy change in Brazil. In a similar way from the
#' authors, I will divide the group into Above and Below, meaning the ones that
#' received more from the policy change and the ones that presented losses.

### 4.1.1 Dosage ----

mat_dos <- feols(as.numeric(profic_mat) ~ dosage : i(anos_exp, grupo, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df %>% filter(grade == 5) %>% select(peso),
                  vcov = "hetero")

por_dos <- feols(as.numeric(profic_port) ~ dosage : i(anos_exp, grupo, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df %>% filter(grade == 5) %>% select(peso),
                  vcov = "hetero")

etable(mat_dos,por_dos)
etable(mat_dos, por_dos,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage/rudi_individuo_abbe.tex", replace = TRUE)

#### 4.1.1.1 Graph ----

model_list <- list(
  mat  = mat_dos,
  por = por_dos
)

for (model_name in names(model_list)) {
  
  
  current_model <- model_list[[model_name]]
  
  temp <- broom::tidy(current_model, conf.int = TRUE) %>%
    # Keep only terms starting with "dosageanos_exp::"
    filter(str_detect(term, "^dosage:anos_exp::")) %>%
    mutate(
      # Extract time_exposure number after "dosageanos_exp::"
      time_exposure = str_extract(term, "(?<=dosage:anos_exp::)-?\\d+"),
      time_exposure = as.numeric(time_exposure),
      # Extract group name (Above/Below)
      grupo = str_extract(term, "(?<=grupo::)\\w+"),
      grupo = as.factor(grupo)
    )
  
  
  
  
  temp_edu <- temp %>%
    distinct(grupo) %>%             # one row per group (Above / Below)
    mutate(
      term = "time_to_treat:0",     # or another label that fits your pattern
      estimate = 0,
      std.error = 0,
      statistic = 0,
      p.value = 1,
      conf.low = 0,
      conf.high = 0,
      time_exposure = 0
    )
  
  # combine with your main dataframe
  temp <- bind_rows(temp, temp_edu) %>%
    arrange(grupo, time_exposure) %>% 
    bind_rows(
      temp_edu %>%
        distinct(grupo) %>%
        mutate(
          time_exposure = 0,
          estimate = 0,
          conf.low = 0,
          conf.high = 0
        )
    ) %>%
    arrange(grupo, time_exposure)
  
  
  p <- ggplot(temp, aes(x = time_exposure, y = estimate, group = grupo)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = grupo), shape = 15, size = 2) +
    geom_line(aes(color = grupo)) +
    #facet_wrap(~ grupo, ncol = 1) +     # <--- separate plot per group
    labs(x = "Years of Exposure", y = "Nota",
         color = NULL, fill = NULL) +
    theme_classic() +
    theme(
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11),
      legend.position = c(0.95, 0.95),
      legend.justification = c("right", "top") # anchor legend box
    ) + # anchor legend box
    scale_x_continuous(
      breaks = seq(min(temp$time_exposure, na.rm = TRUE),
                   max(temp$time_exposure, na.rm = TRUE),
                   by = 2)
    )
  
  
  p <- p +
    theme(
      legend.background = element_blank(),
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11),
      legend.position = c(0.94, 0.17),
      legend.justification = c("right", "top") # anchor legend box
    ) + # anchor legend box
    scale_x_continuous(
      breaks = seq(min(temp$time_exposure, na.rm = TRUE),
                   max(temp$time_exposure, na.rm = TRUE),
                   by = 2))
  
  
  print(p)
  
  ggsave(
    filename = paste0("grafico_", model_name, "_Rudi_dosage.png"),
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Outro/",
    width = 600/96, height = 420/96, dpi = 110
  )
  rm(temp_edu, temp, model_name, current_model, p)
}
rm(model_list)

### 4.1.2 Aluno Dosage ----

mat_dos <- feols(as.numeric(profic_mat) ~ aluno_dosage : i(anos_exp, grupo, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc #Controls
                 | codmun + ano + uf^ano, #FE
                 data = df %>% filter(grade == 5), #Only 5h grade
                 #weights = df %>% filter(grade == 5) %>% select(peso),
                 vcov = "hetero")

por_dos <- feols(as.numeric(profic_port) ~ aluno_dosage : i(anos_exp, grupo, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc #Controls
                 | codmun + ano + uf^ano, #FE
                 data = df %>% filter(grade == 5), #Only 5h grade
                 #weights = df %>% filter(grade == 5) %>% select(peso),
                 vcov = "hetero")

etable(mat_dos,por_dos)
etable(mat_dos, por_dos,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/rudi_aluno_individuo_abbe.tex", replace = TRUE)




#### 4.1.2.1 Graph ----

model_list <- list(
  mat  = mat_dos,
  por = por_dos
)

for (model_name in names(model_list)) {
  
  
  current_model <- model_list[[model_name]]
  
  temp <- broom::tidy(current_model, conf.int = TRUE) %>%
    # Keep only terms starting with "dosageanos_exp::"
    filter(str_detect(term, "^aluno_dosage:anos_exp::")) %>%
    mutate(
      # Extract time_exposure number after "dosageanos_exp::"
      time_exposure = str_extract(term, "(?<=aluno_dosage:anos_exp::)-?\\d+"),
      time_exposure = as.numeric(time_exposure),
      # Extract group name (Above/Below)
      grupo = str_extract(term, "(?<=grupo::)\\w+"),
      grupo = as.factor(grupo)
    )
  
  
  
  
  temp_edu <- temp %>%
    distinct(grupo) %>%             # one row per group (Above / Below)
    mutate(
      term = "time_to_treat:0",     # or another label that fits your pattern
      estimate = 0,
      std.error = 0,
      statistic = 0,
      p.value = 1,
      conf.low = 0,
      conf.high = 0,
      time_exposure = 0
    )
  
  # combine with your main dataframe
  temp <- bind_rows(temp, temp_edu) %>%
    arrange(grupo, time_exposure) %>% 
    bind_rows(
      temp_edu %>%
        distinct(grupo) %>%
        mutate(
          time_exposure = 0,
          estimate = 0,
          conf.low = 0,
          conf.high = 0
        )
    ) %>%
    arrange(grupo, time_exposure)
  
  
  p <- ggplot(temp, aes(x = time_exposure, y = estimate, group = grupo)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = grupo), shape = 15, size = 2) +
    geom_line(aes(color = grupo)) +
    #facet_wrap(~ grupo, ncol = 1) +     # <--- separate plot per group
    labs(x = "Years of Exposure", y = "Nota",
         color = NULL, fill = NULL) +
    theme_classic() +
    theme(
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11),
      legend.position = c(0.95, 0.95),
      legend.justification = c("right", "top") # anchor legend box
    ) + # anchor legend box
    scale_x_continuous(
      breaks = seq(min(temp$time_exposure, na.rm = TRUE),
                   max(temp$time_exposure, na.rm = TRUE),
                   by = 2)
    )
  
  
  p <- p +
    theme(
      legend.background = element_blank(),
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11),
      legend.position = c(0.94, 0.17),
      legend.justification = c("right", "top") # anchor legend box
    ) + # anchor legend box
    scale_x_continuous(
      breaks = seq(min(temp$time_exposure, na.rm = TRUE),
                   max(temp$time_exposure, na.rm = TRUE),
                   by = 2))
  
  
  print(p)
  
  ggsave(
    filename = paste0("grafico_", model_name, "_Rudi_dosage.png"),
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Outro/",
    width = 600/96, height = 420/96, dpi = 110
  )
  rm(temp_edu, temp, model_name, current_model, p)
}
rm(model_list, mat_dos, por_dos)



# ---------------------- #
## 4.2 Roberto Ref ----
# ---------------------- #
#' This specification follows the estimation model from FUNDEF impact in educa-
#' tion study.

### 4.2.1 Main Regression ----



mat_dos <- feols(as.numeric(profic_mat) ~   i(anos_exp, grupo, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc #Controls
                 | codmun + ano + uf^ano, #FE
                 data = df %>% filter(grade == 5), #Only 5h grade
                 #weights = df %>% filter(grade == 5) %>% select(peso),
                 vcov = "hetero")

por_dos <- feols(as.numeric(profic_port) ~  i(anos_exp, grupo, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc #Controls
                 | codmun + ano + uf^ano, #FE
                 data = df %>% filter(grade == 5), #Only 5h grade
                 #weights = df %>% filter(grade == 5) %>% select(peso),
                 vcov = "hetero")

etable(mat_dos,por_dos)

etable(mat_dos, por_dos,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage/roberto_individuo_abbe.tex", replace = TRUE)


#### 4.2.1.1 Graph ----


model_list <- list(
  mat  = mat_dos,
  por = por_dos
)

for (model_name in names(model_list)) {
  
  
  current_model <- model_list[[model_name]]
    
  temp <- broom::tidy(current_model, conf.int = TRUE) %>%
    # Keep only rows starting with "anos_exp::"
    filter(str_detect(term, "^anos_exp::")) %>%
    mutate(
      # Extract numeric value after "anos_exp::"
      time_exposure = str_extract(term, "(?<=anos_exp::)-?\\d+"),
      time_exposure = as.numeric(time_exposure),
      # Extract group name (Above/Below)
      grupo = str_extract(term, "(?<=grupo::)\\w+"),
      grupo = as.factor(grupo)
    )
    
    
    
    
    temp_edu <- temp %>%
      distinct(grupo) %>%             # one row per group (Above / Below)
      mutate(
        term = "time_to_treat:0",     # or another label that fits your pattern
        estimate = 0,
        std.error = 0,
        statistic = 0,
        p.value = 1,
        conf.low = 0,
        conf.high = 0,
        time_exposure = 0
      )
    
    # combine with your main dataframe
    temp <- bind_rows(temp, temp_edu) %>%
      arrange(grupo, time_exposure) %>% 
      bind_rows(
        temp_edu %>%
          distinct(grupo) %>%
          mutate(
            time_exposure = 0,
            estimate = 0,
            conf.low = 0,
            conf.high = 0
          )
      ) %>%
      arrange(grupo, time_exposure)
    
    
    p <- ggplot(temp, aes(x = time_exposure, y = estimate, group = grupo)) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
      geom_vline(xintercept = 0, color = "black") +
      geom_point(aes(color = grupo), shape = 15, size = 2) +
      geom_line(aes(color = grupo)) +
      #facet_wrap(~ grupo, ncol = 1) +     # <--- separate plot per group
      labs(x = "Years of Exposure", y = "Nota",
           color = NULL, fill = NULL) +
      theme_classic() +
      theme(
        axis.line = element_line(color = "grey70"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 11),
        legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top") # anchor legend box
      ) + # anchor legend box
      scale_x_continuous(
        breaks = seq(min(temp$time_exposure, na.rm = TRUE),
                     max(temp$time_exposure, na.rm = TRUE),
                     by = 2)
      )
    
    
    p <- p +
      theme(
        legend.background = element_blank(),
        axis.line = element_line(color = "grey70"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 11),
        legend.position = c(0.14, 0.17),
        legend.justification = c("right", "top") # anchor legend box
      ) + # anchor legend box
      scale_x_continuous(
        breaks = seq(min(temp$time_exposure, na.rm = TRUE),
                     max(temp$time_exposure, na.rm = TRUE),
                     by = 2))
    
    
    print(p)
    
    ggsave(
      filename = paste0("grafico_", model_name, "_Roberto.png"),
      plot = p,
      path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Outro/",
      width = 600/96, height = 420/96, dpi = 110
    )
    rm(temp_edu, temp, model_name, p)
}

