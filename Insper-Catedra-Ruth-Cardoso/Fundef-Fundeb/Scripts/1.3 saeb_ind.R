# ---------------------------------------------------------------------------- #
# SAEB data in the individual level
# Last edited by: Tuffy Licciardi Issa
# Date: 04/11/2025
# ---------------------------------------------------------------------------- #

#' Objective:
#' Here I will extract all individual level data from SAEB database.

# ---------------------------------------------------------------------------- #
# Libraries ----
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

# ---------------------------------------------------------------------------- #
# 1. Loading Data ----
# ---------------------------------------------------------------------------- #
## 1.1 Data Paths ----

paths_list <- c(
  #2005
  "Z:/Arquivos IFB/Prova Brasil/2005/base_PB2005_4serie_cod.dta",
  "Z:/Arquivos IFB/Prova Brasil/2005/base_PB2005_8serie.dta",
  #2007
  "Z:/Arquivos IFB/Prova Brasil/2007/DADOS/TS_ALUNO.dta",
  "Z:/Arquivos IFB/Prova Brasil/2007/DADOS/TS_QUEST_ALUNO.dta",
  #2009
  "Z:/Arquivos IFB/Prova Brasil/2009/DADOS/TS_ALUNO.dta",
  "Z:/Arquivos IFB/Prova Brasil/2009/DADOS/TS_QUEST_ALUNO.dta",
  #2011
  "Z:/Arquivos IFB/SAEB/microdados_saeb_2011/Dados/TS_QUEST_ALUNO.dta",
  "Z:/Arquivos IFB/SAEB/microdados_saeb_2011/Dados/TS_RESULTADO_ALUNO.dta",
  #2013
  "Z:/Arquivos IFB/SAEB/microdados_aneb_prova_brasil_2013/DADOS/ts_aluno_5ef.dta",
  "Z:/Arquivos IFB/SAEB/microdados_aneb_prova_brasil_2013/DADOS/ts_aluno_9ef.dta",
  #2015
  "Z:/Arquivos IFB/SAEB/microdados_saeb_2015/DADOS/TS_ALUNO_5EF.dta",
  "Z:/Arquivos IFB/SAEB/microdados_saeb_2015/DADOS/TS_ALUNO_9EF.dta",
  #2017
  "Z:/Arquivos IFB/SAEB/microdados_saeb_2017/DADOS/ts_aluno_5ef.dta",
  "Z:/Arquivos IFB/SAEB/microdados_saeb_2017/DADOS/ts_aluno_9ef.dta" #,
  # #2019
  # "Z:/Arquivos IFB/SAEB/microdados_saeb_2019/DADOS/ts_aluno_5ef.dta",
  # "Z:/Arquivos IFB/SAEB/microdados_saeb_2019/DADOS/ts_aluno_9ef.dta"
)

# --------------------- #
## 1.2 Opening Loop ----
# --------------------- #

for (idx in seq(1, length(paths_list), by = 2)) {
  
  ini <- Sys.time()
  
  # year index (1..8)
  pair_index <- (idx + 1) / 2        # 1 for first pair, 2 for second pair...
  year <- 2005 + 2 * (pair_index - 1)
  
  path_a <- paths_list[idx]
  path_b <- paths_list[idx + 1]
  
  message("Processing year ", year, ":")
  message(" - path A: ", path_a)
  message(" - path B: ", path_b)
  

  df1 <- read_dta(path_a)  
  df2 <- read_dta(path_b)
  ###1.2.1 2005 ----
  if( year == 2005) {
    
    #Extracting the variables
    df1 <- df1 %>% 
      select(municipio_codmunic, id_uf, municipio, nome_esc, cod_escola,
             serie, disc, aluno, profic_mat, profic_port, peso, dep_admin,
             q1, q2, q4, q20) %>% 
      filter(dep_admin == 2) %>%  #Only Municipal Schools
      mutate(
        grade = 5
      ) %>% 
      rename(
        sexo = q1,
        raca = q2, 
        idade = q4,
        mae_educ = q20
      ) %>% 
      mutate(
        idade = as.character(idade),
        #Organizing age data and creating the exposition variable
        idade = case_when(
          idade == "A" ~ 8,
          idade == "B" ~ 9,
          idade == "C" ~ 10,
          idade == "D" ~ 11, 
          idade == "E" ~ 12,
          idade == "F" ~ 13,
          idade == "G" ~ 14,
          idade == "H" ~ 15,
          TRUE ~ NA
        ),
        idade_aux = ifelse(idade == 10, 1, 0), #Ideal age in 4th/5th grade
        
        treat_exp = 0, #Treatment exposition
      
        codmun = paste0(substr(municipio_codmunic, 1, 2),
                        substr(municipio_codmunic, 8, 11))
        
        )
    
    df2 <- df2 %>% 
      select(municipio_codmunic, id_uf, municipio, nome_esc, cod_escola,
             serie, disc, aluno, profic_mat, profic_port, peso, dep_admin,
             q1, q2, q4, q20) %>%
      filter(dep_admin == 2) %>% #Selecting only municipal schools
      mutate(
        grade = 5
      ) %>% 
      rename(
        sexo = q1,
        raca = q2, 
        idade = q4,
        mae_educ = q20
      ) %>% 
      mutate(
        #Organizing age data and creating the exposition variable
        idade = case_when(
          idade == "A" ~ year - 1986,
          idade == "B" ~ year - 1987,
          idade == "C" ~ year - 1988,
          idade == "D" ~ year - 1989, 
          idade == "E" ~ year - 1990,
          idade == "F" ~ year - 1991,
          idade == "G" ~ year - 1992,
          idade == "H" ~ year - 1993,
          TRUE ~ NA
        ),
        idade_aux = ifelse(idade == 14, 1, 0), #Ideal age in 8th/9th grade
        
        treat_exp = 0, #Treatment exposition
      
        codmun = NA
        )
    
    df_bind <- rbind(df1, df2) %>% 
      mutate(ano = year) %>% 
      filter(!is.na(idade)) %>%
      group_by(municipio, id_uf) %>% 
      mutate(
        codmun = ifelse(serie == 8, codmun[serie == 4], codmun)
      ) %>% 
      ungroup() %>% 
      select(-c(municipio, municipio_codmunic, nome_esc, disc, dep_admin)) %>% 
      mutate( #Other control variables
        
        cod_escola = as.character(cod_escola),
        sexo = as.character(sexo),
        raca = as.character(raca),
        mae_educ = as.character(mae_educ),
        
        sexo = ifelse(sexo == "A" ,1 ,0), #Male = 1
        raca = ifelse(raca %in% c("A","D"), 1, 0), #White or Asian = 1
        mae_educ = ifelse(mae_educ %in% c("D", "E"), 1, 0) #Completed HighSchool = 1
        
      )
  
    
    ##1.2.2 2007 ----
    #For other data formats
  } else if(year == 2007){
      
    df_bind <- df2 %>% 
      left_join(df1 %>% #Grades dataset
                  select(id_aluno,id_serie, PK_COD_ENTIDADE, NU_THETAT_L, NU_THETAT_M) %>% 
                  rename(profic_mat = NU_THETAT_M,
                         profic_port = NU_THETAT_L),
                by = c("id_aluno", "id_serie")
                ) %>% 
      filter(id_dependencia_adm == 3) %>% #Municipal Schools
      
      select(id_serie, id_municipio, id_uf,q_al1, q_al2, q_al4, q_al19, profic_mat,
             profic_port, id_aluno, PK_COD_ENTIDADE) %>% 
      rename(
        cod_escola = PK_COD_ENTIDADE,
        sexo = q_al1,
        raca = q_al2, 
        idade = q_al4,
        mae_educ = q_al19,
        serie = id_serie,
        codmun = id_municipio,
        aluno = id_aluno
      ) %>% 
      mutate(
        #Organizing age data and creating the exposition variable
        idade = case_when(
          
          # 4th
          idade == "A" & serie == 4 ~ 8,
          idade == "B" & serie == 4 ~ 9,
          idade == "C" & serie == 4 ~ 10,
          idade == "D" & serie == 4 ~ 11, 
          idade == "E" & serie == 4 ~ 12,
          idade == "F" & serie == 4 ~ 13,
          idade == "G" & serie == 4 ~ 14,
          idade == "H" & serie == 4 ~ 15,
          
          # 8th
          idade == "A" & serie == 8 ~ year - 1994,
          idade == "B" & serie == 8 ~ year - 1993,
          idade == "C" & serie == 8 ~ year - 1992,
          idade == "D" & serie == 8 ~ year - 1991, 
          idade == "E" & serie == 8 ~ year - 1990,
          idade == "F" & serie == 8 ~ year - 1989,
          idade == "G" & serie == 8 ~ year - 1988,
          idade == "H" & serie == 8 ~ year - 1987,
          TRUE ~ NA
        ),
        idade_aux = ifelse(idade == 14 & serie == 8 |
                             idade == 10 & serie == 4, 1, 0), #Ideal age in 8th/9th grade
        
        grade = ifelse(serie == 8, 9, 5),
        
        treat_exp = 0 #Treatment exposition
      ) %>% 
      filter(!is.na(idade)) %>% 
      mutate(ano = year,
             peso = 1) %>%
      
      mutate( #Other control variables
        
        sexo = as.character(sexo),
        raca = as.character(raca),
        mae_educ = as.character(mae_educ),
        
        sexo = ifelse(sexo == "A", 1 ,0), #Male = 1
        raca = ifelse(raca %in% c("A","D"), 1, 0), #White or Asian = 1
        mae_educ = ifelse(mae_educ %in% c("D", "E"), 1, 0) #Completed HighSchool = 1
        
      )
      
  ###1.2.3 2009 ----
  } else if(year == 2009){
      
    
    #Extracting the questions
    
    # find maximum length (number of questions)
    max_q <- max(nchar(df2$TX_RESPOSTAS), na.rm = TRUE)
    
    # create one column per question by extracting the i-th character
    for(i in seq_len(max_q)) {
      df2[[paste0("Q", i)]] <- substr(df2$TX_RESPOSTAS, i, i)
    }
    
    # optional: treat '*' or '.' as NA (no answer)
    df2[df2 == "*" | df2 == "."] <- NA
    
    # view results (first few Q columns)
    head(df2[, c("TX_RESPOSTAS", paste0("Q", 1:10))])
    
    
    rm(i, max_q)
    
    
    #Combined data
    df_bind <- df2 %>% 
      left_join(df1 %>% #Grades dataset
                  select(ID_ALUNO,ID_SERIE, NU_THETAT_L, NU_THETAT_M, PK_COD_ENTIDADE) %>% 
                  rename(profic_mat = NU_THETAT_M,
                         profic_port = NU_THETAT_L),
                by = c("ID_ALUNO", "ID_SERIE")
      ) %>% 
      filter(ID_DEPENDENCIA_ADM == 3)  %>% #Municipal Schools
      
      select(ID_ALUNO,ID_SERIE, COD_MUNICIPIO, SIGLA_UF ,Q1, Q2, Q4, Q19, profic_mat,
             profic_port, PK_COD_ENTIDADE) %>% 
      rename(
        cod_escola = PK_COD_ENTIDADE,
        sexo = Q1,
        raca = Q2, 
        idade = Q4,
        mae_educ = Q19,
        serie = ID_SERIE,
        codmun = COD_MUNICIPIO,
        id_uf = SIGLA_UF,
        aluno = ID_ALUNO
      ) %>% 
      mutate(
        idade = as.character(idade),
        #Organizing age data and creating the exposition variable
        idade = case_when(
          
          # 4th
          idade == "A" & serie == 5 ~ 8,
          idade == "B" & serie == 5 ~ 9,
          idade == "C" & serie == 5 ~ 10,
          idade == "D" & serie == 5 ~ 11, 
          idade == "E" & serie == 5 ~ 12,
          idade == "F" & serie == 5 ~ 13,
          idade == "G" & serie == 5 ~ 14,
          idade == "H" & serie == 5 ~ 15,
          
          # 8th
          idade == "A" & serie == 9 ~ year - 1994,
          idade == "B" & serie == 9 ~ year - 1993,
          idade == "C" & serie == 9 ~ year - 1992,
          idade == "D" & serie == 9 ~ year - 1991, 
          idade == "E" & serie == 9 ~ year - 1990,
          idade == "F" & serie == 9 ~ year - 1989,
          idade == "G" & serie == 9 ~ year - 1988,
          idade == "H" & serie == 9 ~ year - 1987,
          TRUE ~ NA
        ),
        idade_aux = ifelse(idade == 14 & serie == 9 |
                             idade == 10 & serie == 5, 1, 0), #Ideal age in 8th/9th grade
        
        grade = ifelse(serie == 9, 9, 5),
        
        treat_exp = ifelse(grade == 5, (year - 2007)/idade,
                           (year - 2009)/(idade - year + 2007)) #Treatment exposition
      ) %>% 
      filter(!is.na(idade)) %>% 
      mutate(ano = year,
             peso = 1) %>%
      
      mutate( #Other control variables
        
        sexo = as.character(sexo),
        raca = as.character(raca),
        mae_educ = as.character(mae_educ),
        
        sexo = ifelse(sexo == "A", 1 ,0), #Male = 1
        raca = ifelse(raca %in% c("A","D"), 1, 0), #White or Asian = 1
        mae_educ = ifelse(mae_educ %in% c("D", "E"), 1, 0) , #Completed HighSchool = 1
        id_uf = as.numeric(codmun) %/% 100000
      )
    
    df_bind <- df_bind %>% 
      select(colnames(df_final))
    
    
    #### 1.2.4 2011 ----
  } else if(year == 2011) {
      
    df_bind <- df1 %>% 
      filter(id_dependencia_adm == 3) %>% #Municipal Schools
      left_join(df2 %>%
                  select(id_municipio, id_aluno, id_serie, proficiencia_lp_saeb, proficiencia_mt_saeb),
                by = c("id_municipio", "id_aluno", "id_serie")) %>% 
      select(id_aluno,id_serie, id_municipio, id_uf,  tx_resp_q001, tx_resp_q002,
             tx_resp_q004, tx_resp_q019, proficiencia_lp_saeb, proficiencia_mt_saeb,
             id_escola) %>% 
      rename(
        cod_escola = id_escola,
        profic_port = proficiencia_lp_saeb,
        profic_mat = proficiencia_mt_saeb,
        sexo = tx_resp_q001,
        raca = tx_resp_q002, 
        idade = tx_resp_q004,
        mae_educ = tx_resp_q019,
        serie = id_serie,
        codmun = id_municipio,
        aluno = id_aluno
      ) %>% 
      mutate(
        idade = as.character(idade),
        #Organizing age data and creating the exposition variable
        idade = case_when(
          
          # 4th
          idade == "A" & serie == 5 ~ 8,
          idade == "B" & serie == 5 ~ 9,
          idade == "C" & serie == 5 ~ 10,
          idade == "D" & serie == 5 ~ 11, 
          idade == "E" & serie == 5 ~ 12,
          idade == "F" & serie == 5 ~ 13,
          idade == "G" & serie == 5 ~ 14,
          idade == "H" & serie == 5 ~ 15,
          
          # 8th
          idade == "A" & serie == 9 ~ year - 1999,
          idade == "B" & serie == 9 ~ year - 1998,
          idade == "C" & serie == 9 ~ year - 1997,
          idade == "D" & serie == 9 ~ year - 1996, 
          idade == "E" & serie == 9 ~ year - 1995,
          idade == "F" & serie == 9 ~ year - 1994,
          idade == "G" & serie == 9 ~ year - 1993,
          idade == "H" & serie == 9 ~ year - 1992,
          TRUE ~ NA
        ),
        idade_aux = ifelse(idade == 14 & serie == 9 |
                             idade == 10 & serie == 5, 1, 0), #Ideal age in 8th/9th grade
        
        grade = ifelse(serie == 9, 9, 5),
        
        #The following specificantion accompanies the cohort
        treat_exp = ifelse(grade == 5, (year - 2007)/idade,
                           (year - 2009)/(idade - year + 2007)) #Treatment exposition
      ) %>% 
      filter(!is.na(idade)) %>% 
      mutate(ano = year,
             peso = 1) %>%
      
      mutate( #Other control variables
        
        sexo = as.character(sexo),
        raca = as.character(raca),
        mae_educ = as.character(mae_educ),
        
        sexo = ifelse(sexo == "A", 1 ,0), #Male = 1
        raca = ifelse(raca %in% c("A","D"), 1, 0), #White or Asian = 1
        mae_educ = ifelse(mae_educ %in% c("E", "F"), 1, 0) #Completed HighSchool = 1
        
      )
    
    ### 1.2.5 2013 - 2017 ----
  } else if(year %in% c(2013:2017)) {
      
    df1 <- df1 %>% 
      filter(id_dependencia_adm == 3) %>% #Municipal Schools
      select(id_aluno,id_serie, id_municipio, id_uf, tx_resp_q001, tx_resp_q002,
             tx_resp_q004, tx_resp_q019, proficiencia_lp_saeb, proficiencia_mt_saeb,
             peso_aluno_lp, peso_aluno_mt, id_escola) 
    
    
    df2 <- df2 %>% 
      filter(id_dependencia_adm == 3) %>% #Municipal Schools
      select(id_aluno,id_serie, id_municipio, id_uf,  tx_resp_q001, tx_resp_q002,
             tx_resp_q004, tx_resp_q019, proficiencia_lp_saeb, proficiencia_mt_saeb,
             peso_aluno_lp, peso_aluno_mt, id_escola) 
    
    
    
    #Binding both datasets
    df_bind <- rbind(df1,df2) %>% 
      rename(
        cod_escola = id_escola,
        profic_port = proficiencia_lp_saeb,
        profic_mat = proficiencia_mt_saeb,
        sexo = tx_resp_q001,
        raca = tx_resp_q002, 
        idade = tx_resp_q004,
        mae_educ = tx_resp_q019,
        serie = id_serie,
        codmun = id_municipio,
        aluno = id_aluno
      ) 
      
      
      
      
    #Since for each year the reference year changes for the 9th grade I will add
    # the command of age extraction for each different year
    
    
    if( year == 2013){
    
      df_bind <- df_bind %>% 
        mutate(
          idade = as.character(idade),
          #Organizing age data and creating the exposition variable
          idade = case_when(
          
            # 8th
            idade == "A" & serie == 9 ~ year - 2001,
            idade == "B" & serie == 9 ~ year - 2000,
            idade == "C" & serie == 9 ~ year - 1999,
            idade == "D" & serie == 9 ~ year - 1998, 
            idade == "E" & serie == 9 ~ year - 1997,
            idade == "F" & serie == 9 ~ year - 1996,
            idade == "G" & serie == 9 ~ year - 1995,
            idade == "H" & serie == 9 ~ year - 1994,
            
            # 4th
            idade == "A" & serie == 5 ~ 8,
            idade == "B" & serie == 5 ~ 9,
            idade == "C" & serie == 5 ~ 10,
            idade == "D" & serie == 5 ~ 11, 
            idade == "E" & serie == 5 ~ 12,
            idade == "F" & serie == 5 ~ 13,
            idade == "G" & serie == 5 ~ 14,
            idade == "H" & serie == 5 ~ 15,
            
            TRUE ~ NA
          ),
          
  
          
          idade_aux = ifelse(idade == 14 & serie == 9 |
                               idade == 10 & serie == 5, 1, 0), #Ideal age in 8th/9th grade
          
          grade = ifelse(serie == 9, 9, 5),
          peso = mean(peso_aluno_lp + peso_aluno_mt, na.rm = T),
          treat_exp = ifelse(grade == 5, (year - 2007)/idade,
                             (year - 2009)/(idade - year + 2007)) #Treatment exposition
        )
      
      #Year of 2015
    } else if (year == 2015) {
      
      df_bind <- df_bind %>% 
        mutate(
          idade = as.character(idade),
          #Organizing age data and creating the exposition variable
          idade = case_when(
            
            # 8th
            idade == "A" & serie == 9 ~ year - 2003,
            idade == "B" & serie == 9 ~ year - 2002,
            idade == "C" & serie == 9 ~ year - 2001,
            idade == "D" & serie == 9 ~ year - 2000, 
            idade == "E" & serie == 9 ~ year - 1999,
            idade == "F" & serie == 9 ~ year - 1998,
            idade == "G" & serie == 9 ~ year - 1997,
            idade == "H" & serie == 9 ~ year - 1996,
            
            # 4th
            idade == "A" & serie == 5 ~ 8,
            idade == "B" & serie == 5 ~ 9,
            idade == "C" & serie == 5 ~ 10,
            idade == "D" & serie == 5 ~ 11, 
            idade == "E" & serie == 5 ~ 12,
            idade == "F" & serie == 5 ~ 13,
            idade == "G" & serie == 5 ~ 14,
            idade == "H" & serie == 5 ~ 15,
            
            TRUE ~ NA
          ),
          
          
          
          idade_aux = ifelse(idade == 14 & serie == 9 |
                               idade == 10 & serie == 5, 1, 0), #Ideal age in 8th/9th grade
          
          grade = ifelse(serie == 9, 9, 5),
          peso = mean(peso_aluno_lp + peso_aluno_mt, na.rm = T),
          treat_exp = ifelse(grade == 5, (year - 2007)/idade,
                             (year - 2009)/(idade - year + 2007)) #Treatment exposition
        )
      
      
     #Now for the year of 2017 
    } else {
      
      
      df_bind <- df_bind %>% 
        mutate(
          idade = as.character(idade),
          #Organizing age data and creating the exposition variable
          idade = case_when(
            
            # 8th
            idade == "A" & serie == 9 ~ year - 2005,
            idade == "B" & serie == 9 ~ year - 2004,
            idade == "C" & serie == 9 ~ year - 2003,
            idade == "D" & serie == 9 ~ year - 2002, 
            idade == "E" & serie == 9 ~ year - 2001,
            idade == "F" & serie == 9 ~ year - 2000,
            idade == "G" & serie == 9 ~ year - 1999,
            idade == "H" & serie == 9 ~ year - 1998,
            
            # 4th
            idade == "A" & serie == 5 ~ 8,
            idade == "B" & serie == 5 ~ 9,
            idade == "C" & serie == 5 ~ 10,
            idade == "D" & serie == 5 ~ 11, 
            idade == "E" & serie == 5 ~ 12,
            idade == "F" & serie == 5 ~ 13,
            idade == "G" & serie == 5 ~ 14,
            idade == "H" & serie == 5 ~ 15,
            
            TRUE ~ NA
          ),
          
          
          
          idade_aux = ifelse(idade == 14 & serie == 9 |
                               idade == 10 & serie == 5, 1, 0), #Ideal age in 8th/9th grade
          
          grade = ifelse(serie == 9, 9, 5),
          peso = mean(peso_aluno_lp + peso_aluno_mt, na.rm = T),
          treat_exp = ifelse(grade == 5, (year - 2007)/idade,
                             (year - 2009)/(idade - year + 2007)) #Treatment exposition
        )
      
      
      
    } 
    
    
    
    
    
    
    #Final filtering and data preparation
    df_bind <- df_bind %>% 
      select(-c(peso_aluno_mt, peso_aluno_lp)) %>% 
      filter(!is.na(idade)) %>% 
      mutate(ano = year) %>%
      
      mutate( #Other control variables
        
        sexo = as.character(sexo),
        raca = as.character(raca),
        mae_educ = as.character(mae_educ),
        
        sexo = ifelse(sexo == "A", 1 ,0), #Male = 1
        raca = ifelse(raca %in% c("A","D"), 1, 0), #White or Asian = 1
        mae_educ = ifelse(mae_educ %in% c("E", "F"), 1, 0) #Completed HighSchool = 1
        
      )
    

  } 
  
  
  if (year == 2005){
    
    df_final <- df_bind

  } else {
    
    df_final <- rbind(df_final, df_bind) %>% 
      mutate(treat_exp = ifelse(treat_exp > 1, 1, treat_exp))
  
  }
  
  
  
  #Total waiting time per dataset
  fim <- Sys.time()
  
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  
  message("---------------------------------------------")
  message("Total Time: ",mins," mins and ", secs, " s")
  message("---------------------------------------------")
  
  
  
  
  rm(df_bind, idx, ini, fim, delta, mins, secs, path_a, path_b, year, df1, df2, pair_index)
}



#Saving final dataset
saveRDS(df_final, "Z:/Tuffy/Paper - Educ/Dados/saeb_nvl_aluno.rds")


