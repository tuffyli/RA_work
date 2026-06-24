# ---------------------------------------------------------------------------- #
# SAEB data in the individual level
# Last edited by: Tuffy Licciardi Issa
# Date: 23/10/2025
# ---------------------------------------------------------------------------- #

#' Objective:
#' Here I will extract the municipality weights through Brazil's Censo Escolar.
#' Thus, having the total enrollment for each municipality by year.

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

#1. Censo Escolar ----

path_list <- c("Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2005/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2006/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2007.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2008.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2009.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2010.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2011.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2012.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2013.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2014.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2015.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2016.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2017.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2018.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2019.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2020.rds",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2021/2021/dados/microdados_ed_basica_2021.csv"
               )

#Extraction and Weightening Loop
for(i in c(2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)){
  
 gc()
  message("Ano: ",i)
  
  ini <- Sys.time()
  
  j <- i - 2004

  
  if (i <= 2006){
    
    temp <- read_dta(path_list[j]) %>% 
      select(c(1:9,
               DEF11F, NEF11F, #4 série
               DEF11J, NEF11J, #8 série
               
               DE9F11F, NE9F11F,#5 ano
               DE9F11N, NE9F11N #9 ano
               )
             )
    
    temp_al <- temp %>% 
      filter(!(DEP %in% c("Particular", "Federal")), # Removes Federal and Private Schools
                      CODFUNC == "Ativo") %>%  # Removes deactivated schools
      mutate(CODMUNIC = as.numeric(str_c( #concatenates
        str_sub(as.character(CODMUNIC), 1, 2), #only two firsts
        str_sub(as.character(CODMUNIC), -5))),
        
        DEF11F = ifelse(is.na(DEF11F), 0, DEF11F),
        NEF11F = ifelse(is.na(NEF11F), 0, NEF11F),
        DE9F11F = ifelse(is.na(DE9F11F), 0, DE9F11F),
        NE9F11F = ifelse(is.na(NE9F11F), 0, NE9F11F),
        
        DEF11J = ifelse(is.na(DEF11J), 0, DEF11J),
        NEF11J = ifelse(is.na(NEF11J), 0, NEF11J),
        DE9F11N = ifelse(is.na(DE9F11N), 0, DE9F11N),
        NE9F11N = ifelse(is.na(NE9F11N), 0, NE9F11N)
        ) %>%
      group_by(CODMUNIC, DEP, ANO) %>% 
      summarise(
        ano_5 = sum(DEF11F + NEF11F + DE9F11F + NE9F11F, na.rm = T),
        ano_9 = sum(DEF11J + NEF11J + DE9F11N + NE9F11N, na.rm = T),
        .groups = "drop"
      )
  
    } else if(i %in% c(2007:2019)) {
    
    temp <- readRDS(path_list[j]) %>% 
      select(co_uf,
             co_municipio,
             nu_ano_censo,
             tp_etapa_ensino,
             tp_dependencia,
             co_entidade
             ) %>% 
      filter(tp_dependencia %in% c(2,3),
             tp_etapa_ensino %in% c(41, 18, 11, 7))
      
    message("Base aberta para o ano: ", i)
    
    
    temp_al <- temp %>% 
          rename(CODMUNIC = co_municipio,
                 ANO = nu_ano_censo) %>%
          mutate(
            fund5 = ifelse(tp_etapa_ensino %in% c(18,7), 1, 0),
            fund9 = ifelse(tp_etapa_ensino %in% c(41, 11), 1, 0),

                 DEP = case_when(
                   tp_dependencia == 2 ~ "Estadual",
                   tp_dependencia == 3 ~ "Municipal",
                   TRUE ~ NA)
            ) %>%
          group_by(CODMUNIC, DEP, ANO) %>%
          summarise(
            ano_5 = sum(fund5, na.rm = T),
            ano_9 = sum(fund9, na.rm = T)
          )
    
    temp_ent <- temp %>% 
      mutate(
        DEP = case_when(
          tp_dependencia == 2 ~ "Estadual",
          tp_dependencia == 3 ~ "Municipal",
          TRUE ~ NA
        )
      ) %>% 
      group_by(co_municipio,co_entidade, DEP) %>%
      summarise(
        ano = i
      )

      message("Terminado a agregação em: ", i)
    
  
    } else if(i == 2021) {
      
      temp <- read.csv2(path_list[j]) %>% 
        select(NU_ANO_CENSO,
               CO_UF,
               CO_MUNICIPIO,
               CO_ENTIDADE,
               TP_DEPENDENCIA) %>% 
        filter(TP_DEPENDENCIA %in% c(2,3))
      
      temp_ent <- temp %>% 
        mutate(
          DEP = case_when(
            TP_DEPENDENCIA == 2 ~ "Estadual",
            TP_DEPENDENCIA == 3 ~ "Municipal",
            TRUE ~ NA
          )
        ) %>% 
        rename(
          co_municipio = CO_MUNICIPIO,
          co_entidade = CO_ENTIDADE
        ) %>% 
        group_by(co_municipio,co_entidade, DEP) %>%
        summarise(
          ano = i
        )
      
      message("Terminado a agregação em: ", i)
      
    }
  
  #Salvando
  if(i == 2005){
    data <- temp_al
    
    message("Base final criada com Sucesso")
  } else if(i == 2006) {
    data <- rbind(data, temp_al) %>% 
      arrange(CODMUNIC,ANO,DEP)
    
    message("Base unida com sucesso")
    
  } else if(i == 2007){
    
    data <- rbind(data, temp_al) %>% 
      arrange(CODMUNIC,ANO,DEP)
    
    data_ent <- temp_ent
    
    message("Base unida com sucesso")
    
  } else if(i == 2021){
    
    
    data_ent <- rbind(data_ent, temp_ent)
    message("Base unida com sucesso")
    
    
  } else {
    data <- rbind(data, temp_al) %>% 
      arrange(CODMUNIC,ANO,DEP)
    
    data_ent <- rbind(data_ent, temp_ent)
    
    message("Base unida com sucesso")
  }
  
  fim <- Sys.time()
  
  
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  
  message("---------------------------------------------")
  message("Tempo Decorrido: ",mins," mins e ", secs, " s")
  message("---------------------------------------------")
  
  rm(temp_al, temp_ent, delta, ini, fim, temp, mins, secs, i, j)
  }
rm(path_list)


#2. Mat. Aprovados por Ano -----

library(readxl)

path <- "Z:/Giovanni Zanetti/Progressão Continuada - Estudo/Alunos por série - 2007 a 2023/88.matriculas_serie_ano.xlsx"

sheets <- excel_sheets(path)
print(sheets)   # just to see what’s inside

# read all sheets into a list (one data.frame per sheet)
bases <- lapply(sheets, function(s) {
  read_xlsx(path, sheet = s, skip = 6)
})
names(bases) <- sheets  # name each element by its sheet

bases[[1]]        #2007


#3. Filtro Escola Municipal ----

for(s in unique(data_ent$ano)) {
  
  n <- s - 2006
  
  temp <- bases[[n]]
  
  temp <- temp %>% 
    select(NU_ANO_CENSO, CO_UF, CO_MUNICIPIO, CO_ENTIDADE, qtmataprov5ano, qtmataprov9ano)
  
  temp <- temp %>% 
    left_join( data_ent %>%
                 ungroup() %>% filter(ano == s),
               by = c("CO_MUNICIPIO" = "co_municipio", "CO_ENTIDADE" = "co_entidade"))
  
  temp_filtered <- temp %>% 
    filter(DEP == "Municipal")
  
  temp_filtered <- temp_filtered %>% 
    group_by(CO_MUNICIPIO, NU_ANO_CENSO) %>% 
    summarise(
      ano_5 = sum(qtmataprov5ano, na.rm = T),
      ano_9 = sum(qtmataprov9ano, na.rm = T)
    )
  
  
  if (n == 1) {
    
    data_matr <- temp_filtered
  
    } else {
    
    data_matr <- rbind(data_matr, temp_filtered) %>% 
      arrange(CO_MUNICIPIO, NU_ANO_CENSO)
    
  }

  
  rm(temp, s, temp_filtered, n)
  }


# ---------------------------------------------------------------------------- #
# 4. DATA FINAL ----
# ---------------------------------------------------------------------------- #

data_ap <- data %>% 
  ungroup() %>% 
  filter(DEP == "Municipal",
         ANO == 2005 ) %>% 
  select(CODMUNIC, ANO, ano_5, ano_9) %>% 
  rename(CO_MUNICIPIO = CODMUNIC,
         NU_ANO_CENSO = ANO)


final <- rbind(data_ap, data_matr) %>% 
  arrange(CO_MUNICIPIO, NU_ANO_CENSO)
  

#Salvando
saveRDS(final, "Z:/Tuffy/Paper - Educ/Dados/pesos_saeb.rds" )

#2 version

data_ap2 <- data %>% 
  ungroup() %>% 
  filter(DEP == "Municipal",
         ANO %in% c(2005,2007) ) %>% 
  select(CODMUNIC, ANO, ano_5, ano_9) %>% 
  rename(CO_MUNICIPIO = CODMUNIC,
         NU_ANO_CENSO = ANO)



final2 <- rbind(data_ap2, data_matr %>% filter(NU_ANO_CENSO != 2007)) %>% 
  arrange(CO_MUNICIPIO, NU_ANO_CENSO)


#Saving
saveRDS(final2, "Z:/Tuffy/Paper - Educ/Dados/pesos_saeb2.rds" )

#3 version

data_ap3 <- data %>% 
  ungroup() %>% 
  filter(DEP == "Municipal") %>% 
  select(CODMUNIC, ANO, ano_5, ano_9) %>% 
  rename(CO_MUNICIPIO = CODMUNIC,
         NU_ANO_CENSO = ANO)



final3 <- rbind(data_ap3, data_matr %>% filter(NU_ANO_CENSO == 2021)) %>% 
  arrange(CO_MUNICIPIO, NU_ANO_CENSO)


#Saving
saveRDS(final3, "Z:/Tuffy/Paper - Educ/Dados/pesos_saeb3.rds" )
