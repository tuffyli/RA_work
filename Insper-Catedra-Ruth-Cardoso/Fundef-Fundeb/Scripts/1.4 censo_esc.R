# ---------------------------------------------------------------------------- #
# Data Extraction
# DataBase adjustment
# Last edited by: Tuffy Licciardi Issa
# Date: 11/11/2025
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
library(readr)

#Desativando a notação científica
options(scipen = 999)

# ---------------------------------------------------------------------------- #
#  School data ----
# ---------------------------------------------------------------------------- #

#' ***************************************************************************
#' In this sectio I will extract information regarding school characteristics from
#' Brazil's School Census. With the data collect in this code I plan to investi-
#' gate the violation of the pretrends in the SAEB regressions. Hopefully it is
#' possible to uncover the improvent in SAEB scores.
#' ***************************************************************************


path_list <- c("Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2005/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2006/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2007/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2008/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2009/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2010/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2011/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2012/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2013/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2014/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2015/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2016/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2017/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2018/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2019/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2020/DADOS/escolas.CSV",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2021/2021/dados/microdados_ed_basica_2021.csv"
)

#Loop for creating a database regarding each schools characteristics
#1. Loop -----
for(i in c(2005:2018)){
  
  gc()
  message("Year: ",i)
  
  ini <- Sys.time()
  
  j <- i - 2004 #path index
  
  ##1.1 2005-2006 years ----
  if (i <= 2006){
    
    temp <- read_dta(path_list[j]) %>%
      select(c(1:24,
               SAL_DE_P, #Teacher's room
               PERMANEN, #Classroom
               LAB_INFO, #Computer Lab
               LAB_CIEN, #Science Lab
               LAB_OUTR, #Other Lab
               BIBLIO,   #Library
               FUNCION,  #Employee
               PROFESS,  #Teachers
               MERE_ESC, #Lunch
               AUX_CRECHE, #Auxiliar at Kindergarden
               AUX_PRE,  #Auxiliar at Kindergarden
               228:241,  #Teacher's especiality
               PARQ_INF, #Playground
               QUAD_DES, #Court
               QUAD_COB  #Covered court
               
              
               )
      ) %>% 
      filter(DEP == "Municipal", # Removes Federal and Private Schools
             CODFUNC == "Ativo") %>%  # Removes deactivated schools
      mutate(
        CODMUNIC = as.numeric(str_c( #concatenates
          str_sub(as.character(CODMUNIC), 1, 2), #only first two strings
          str_sub(as.character(CODMUNIC), -5))),
        
        #School characteristics variables
        classroom = as.numeric(PERMANEN),
        teach_room = ifelse(SAL_DE_P == "s", 1, 0),
        lab_dummy = ifelse(LAB_INFO == "s" | LAB_CIEN == "s" | LAB_OUTR == "s",
                           1, 0),
        lib_dummy = ifelse(BIBLIO == "s", 1, 0),
        play_area = ifelse(PARQ_INF == "s"| QUAD_DES == "s" | QUAD_COB == "s",
                           1, 0),
        lunch = ifelse(MERE_ESC == "s", 1, 0),
        employee = as.numeric(FUNCION),
        teacher = as.numeric(PROFESS),
        
        #Education levels
        kinder = ifelse(NIVELCRE == "s" | NIVELPRE == "s" , 1, 0),
        elementary = ifelse(NIV_F1A4_8 == "s" | NIV_F5A8_8 == "s" | NIV_F9FIM == "s" |
                              NIV_F9INI == "s", 1, 0),
        
        daycare = ifelse(NIVELCRE == "s", 1, 0),
        preschool = ifelse(NIVELPRE == "s", 1, 0),
        
        
        high = ifelse(NIVELMED == "s", 1, 0),
        inclusion = ifelse(ESP_EXCL == "s" | ESP_T_ES == "s" | ENS_INCL == "s",
                           1, 0)
        
        # eja = ifelse(SUPL_AVA == "s" |
        #                SUPL_SAVA == "s", 1, 0),

        

      ) %>% 
      select(c(1:9,53:66)) %>% 
        rename(
          ano = ANO,
          school = MASCARA,
          codmun = CODMUNIC
        ) %>% 
        select(-c(UF, SIGLA, DEP, LOC, CODFUNC, MUNIC, teacher)) %>% 
        mutate( uf = codmun %/% 100000)
      

    # 1.2 2007-2014 years ----
  } else if(i %in% c(2007:2014)) {
    
    temp <- read_dta(path_list[j]) %>%
      rename_all(tolower) 
    
          if (i %in% c(2011:2014)){ #Valid for this years specific variables constructions
            temp <- temp %>% 
              mutate(id_quadra_esportes = ifelse(id_quadra_esportes_coberta == 1 |
                                                   id_quadra_esportes_descoberta == 1,
                                                 1, 0)) %>% 
              filter(desc_situacao_funcionamento == 1)
            
            #For 2008-2010 the sports court code remains the same as 2007
          } else if (i %in% c(2008:2010)) { #2008 is the first year to present the change in notation
            
            temp <- temp %>%
              filter(desc_situacao_funcionamento == 1)
            
          } else {
            temp <- temp %>% #Exclusive for 2007
              filter(desc_situacao_funcionamento == "EM ATIVIDADE") #Active Schools
          } 
    
    temp <- temp %>% 
      select(fk_cod_estado,
             fk_cod_municipio,
             ano_censo,
             id_dependencia_adm,
             pk_cod_entidade,
             
             #School characteristics
             id_sala_professor,          #Teacher's room
             id_laboratorio_ciencias,    #Science Lab
             id_laboratorio_informatica, #Computer Lab
             id_quadra_esportes,         #Court
             id_parque_infantil,         #Playground
             id_biblioteca,              #Library
             num_salas_existentes,       #Classrooms
             num_salas_utilizadas,
             num_funcionarios,           #Employees
             id_alimentacao,             #Lunch
             
             #Education levels
             id_reg_infantil_creche,     #Kindergarden
             id_reg_infantil_preescola,  #Kindergarden
             id_reg_fund_8_anos,         #Elementary
             id_reg_fund_9_anos,         #Elementary
             
             id_reg_medio_integrado,
             id_reg_medio_medio,
             id_reg_medio_normal,        #Highschool
             id_reg_medio_prof,
             id_mod_ens_esp 
      ) %>% 
      filter(id_dependencia_adm == 3)
    
    message("Opened database for year: ", i)
    
    
    temp <- temp %>% 
      mutate(
        
        #School characteristics variables
        classroom = ifelse(!is.na(num_salas_existentes),
                           as.numeric(num_salas_existentes), NA), 
        
        teach_room = as.numeric(id_sala_professor),
        
        lab_dummy = ifelse(id_laboratorio_ciencias == 1 |
                             id_laboratorio_informatica == 1, 1, 0),
        
        lib_dummy = as.numeric(id_biblioteca),
        
        play_area = ifelse(id_parque_infantil == 1 |
                             id_quadra_esportes == 1, 1, 0),
        
        lunch = as.numeric(id_alimentacao),
        
        employee = ifelse(!is.na(num_funcionarios),
                          as.numeric(num_funcionarios), NA),
        
        #teacher = as.numeric(PROFESS), #Not present in this data frame.
        
        #Education levels
        kinder = ifelse(id_reg_infantil_creche == 1 |
                          id_reg_infantil_preescola == 1, 1, 0),
        
        daycare = ifelse(id_reg_infantil_creche == 1, 1, 0),
        preschool = ifelse(id_reg_infantil_preescola == 1, 1, 0),
        
        elementary = ifelse(id_reg_fund_8_anos == 1 | id_reg_fund_9_anos == 1, 1, 0),
        
        high = ifelse(id_reg_medio_integrado == 1 | id_reg_medio_medio == 1 |
                        id_reg_medio_normal == 1, 1, 0),
        
        inclusion = ifelse(id_mod_ens_esp == 1, 1, 0)
      ) %>% 
      rename(
        uf = fk_cod_estado,
        codmun = fk_cod_municipio,
        ano = ano_censo,
        school = pk_cod_entidade
      ) %>% 
      select( c(uf, codmun, ano, school, 25:37) ) #Final datafilter
    
    
    
    
    ###1.2.1 2008
    
    
    
    message("Finishing data preparation for: ", i)
    
    
    
    
  } else if(i %in% c(2015:2020)) {
    ## 1.3 2015 - 2020 year ----

    temp <- read_dta(path_list[j]) %>% 
      rename_all(tolower) %>%
      filter(tp_situacao_funcionamento == 1) 
    
    ### 1.3.1 2015 - 2017 ----
       if(i %in% c(2015:2017)) {
          
          temp <- temp %>% 
            select(nu_ano_censo,
                   co_uf,
                   co_municipio,
                   co_entidade,
                   tp_dependencia,
                   
                   in_sala_professor,           #Teacher's room
                   in_laboratorio_ciencias,     #Science Lab
                   in_laboratorio_informatica, #Computer Lab
                   in_quadra_esportes,          #Court
                   in_parque_infantil,          #Playground
                   in_biblioteca,               #Libary
                   nu_salas_existentes,         #Classrooms
                   nu_salas_utilizadas,
                   nu_funcionarios,             #Employee
                   in_alimentacao,              #Lunch
                   
                   in_comum_creche,             #Kindergarden
                   in_comum_pre,
                   in_comum_fund_ai,            #Elementary
                   in_comum_fund_af,
                   in_comum_medio_medio,        #Highschool
                   in_comum_medio_integrado,
                   in_comum_medio_normal,
                   in_especial_exclusiva        #Special
                   
            ) %>% 
            filter(tp_dependencia == 3) %>% 
            mutate(
              
              #School characteristics variables
              classroom = ifelse(!is.na(nu_salas_existentes),
                                 as.numeric(nu_salas_existentes), NA), 
              
              teach_room = as.numeric(in_sala_professor),
              
              lab_dummy = ifelse(in_laboratorio_informatica == 1 |
                                   in_laboratorio_ciencias == 1, 1, 0),
              
              lib_dummy = as.numeric(in_biblioteca),
              
              play_area = ifelse(in_parque_infantil == 1 |
                                   in_quadra_esportes == 1, 1, 0),
              
              lunch = as.numeric(in_alimentacao),
              
              employee = ifelse(!is.na(nu_funcionarios),
                                as.numeric(nu_funcionarios), NA),
              
              #teacher = as.numeric(PROFESS), #Not present in this data frame.
              
              #Education levels
              kinder = ifelse(in_comum_creche == 1 |
                                in_comum_pre == 1, 1, 0),
              
              daycare = ifelse(in_comum_creche == 1, 1, 0),
              preschool = ifelse(in_comum_pre == 1, 1, 0),
              
              elementary = ifelse(in_comum_fund_ai == 1 | in_comum_fund_af == 1, 1, 0),
              
              high = ifelse(in_comum_medio_medio == 1 | in_comum_medio_integrado == 1 |
                              in_comum_medio_normal == 1, 1, 0),
              
              inclusion = ifelse(in_especial_exclusiva == 1, 1, 0)
            ) %>% 
            rename(
              uf = co_uf,
              codmun = co_municipio,
              ano = nu_ano_censo,
              school = co_entidade
            ) %>% 
            select( c(uf, codmun, ano, school, 24:36) ) #Final datafilter'
          
          
        } else if(i %in% c(2018:2020)) {
      ### 1.3.2 2018 ----
      
          if(i == 2018) {
                temp <- temp %>% 
                  select(nu_ano_censo,
                         co_uf,
                         co_municipio,
                         co_entidade,
                         tp_dependencia,
                         
                         in_sala_professor,           #Teacher's room
                         in_laboratorio_ciencias,     #Science Lab
                         in_laboratorio_informatica, #Computer Lab
                         in_quadra_esportes,          #Court
                         in_parque_infantil,          #Playground
                         in_biblioteca,               #Libary
                         qt_salas_existentes,         #Classrooms
                         qt_salas_utilizadas,
                         qt_funcionarios,             #Employee
                         in_alimentacao,              #Lunch
                         
                         in_comum_creche,             #Kindergarden
                         in_comum_pre,
                         in_comum_fund_ai,            #Elementary
                         in_comum_fund_af,
                         in_comum_medio_medio,        #Highschool
                         in_comum_medio_integrado,
                         in_comum_medio_normal,
                         in_especial_exclusiva        #Special
                         
                  ) %>% 
                  filter(tp_dependencia == 3) %>% 
                  mutate(
                    
                    #School characteristics variables
                    classroom = ifelse(!is.na(qt_salas_existentes),
                                       as.numeric(qt_salas_existentes), NA), 
                    
                    teach_room = as.numeric(in_sala_professor),
                    
                    lab_dummy = ifelse(in_laboratorio_informatica == 1 |
                                         in_laboratorio_ciencias == 1, 1, 0),
                    
                    lib_dummy = as.numeric(in_biblioteca),
                    
                    play_area = ifelse(in_parque_infantil == 1 |
                                         in_quadra_esportes == 1, 1, 0),
                    
                    lunch = as.numeric(in_alimentacao),
                    
                    employee = ifelse(!is.na(qt_funcionarios),
                                      as.numeric(qt_funcionarios), NA),
                    
                    #teacher = as.numeric(PROFESS), #Not present in this data frame.
                    
                    #Education levels
                    kinder = ifelse(in_comum_creche == 1 |
                                      in_comum_pre == 1, 1, 0),
                    
                    daycare = ifelse(in_comum_creche == 1, 1, 0),
                    preschool = ifelse(in_comum_pre == 1, 1, 0),
                    
                    elementary = ifelse(in_comum_fund_ai == 1 | in_comum_fund_af == 1, 1, 0),
                    
                    high = ifelse(in_comum_medio_medio == 1 | in_comum_medio_integrado == 1 |
                                    in_comum_medio_normal == 1, 1, 0),
                    
                    inclusion = ifelse(in_especial_exclusiva == 1, 1, 0)
                  ) %>% 
                  rename(
                    uf = co_uf,
                    codmun = co_municipio,
                    ano = nu_ano_censo,
                    school = co_entidade
                  ) %>% 
                  select( c(uf, codmun, ano, school, 24:36) ) #Final datafilter'
                
                } else if(i %in% c(2019:2020)){
        ### 1.3.3 2019 - 2020 ----
          
                  temp <- temp %>% 
                    select(nu_ano_censo,
                           co_uf,
                           co_municipio,
                           co_entidade,
                           tp_dependencia,
                           
                           in_sala_professor,           #Teacher's room
                           in_laboratorio_ciencias,     #Science Lab
                           in_laboratorio_informatica, #Computer Lab
                           in_quadra_esportes,          #Court
                           in_parque_infantil,          #Playground
                           in_biblioteca,               #Libary
                           qt_salas_utilizadas,         #Classrooms
                           151:163,                     #Employee
                           in_alimentacao,              #Lunch
                           
                           in_comum_creche,             #Kindergarden
                           in_comum_pre,
                           in_comum_fund_ai,            #Elementary
                           in_comum_fund_af,
                           in_comum_medio_medio,        #Highschool
                           in_comum_medio_integrado,
                           in_comum_medio_normal,
                           in_especial_exclusiva        #Special
                           
                    ) %>% 
                    filter(tp_dependencia == 3) %>% 
                    mutate(
                      
                      #School characteristics variables
                      classroom = ifelse(!is.na(qt_salas_utilizadas),
                                         as.numeric(qt_salas_utilizadas), NA), 
                      
                      teach_room = as.numeric(in_sala_professor),
                      
                      lab_dummy = ifelse(in_laboratorio_informatica == 1 |
                                           in_laboratorio_ciencias == 1, 1, 0),
                      
                      lib_dummy = as.numeric(in_biblioteca),
                      
                      play_area = ifelse(in_parque_infantil == 1 |
                                           in_quadra_esportes == 1, 1, 0),
                      
                      lunch = as.numeric(in_alimentacao),
                      
                      
                      #teacher = as.numeric(PROFESS), #Not present in this data frame.
                      
                      #Education levels
                      kinder = ifelse(in_comum_creche == 1 |
                                        in_comum_pre == 1, 1, 0),
                      
                      daycare = ifelse(in_comum_creche == 1, 1, 0),
                      preschool = ifelse(in_comum_pre == 1, 1, 0),
                      
                      
                      elementary = ifelse(in_comum_fund_ai == 1 | in_comum_fund_af == 1, 1, 0),
                      
                      high = ifelse(in_comum_medio_medio == 1 | in_comum_medio_integrado == 1 |
                                      in_comum_medio_normal == 1, 1, 0),
                      
                      inclusion = ifelse(in_especial_exclusiva == 1, 1, 0)
                    ) %>% 
                    rename(
                      uf = co_uf,
                      codmun = co_municipio,
                      ano = nu_ano_censo,
                      school = co_entidade
                    ) %>% 
                    mutate(employee = rowSums(across(
                      c(qt_prof_administrativo, qt_prof_servicos_gerais,
                        qt_prof_bilbiotecario, qt_prof_saude,
                        qt_prof_coordenador, qt_prof_fonoaudiologo,
                        qt_prof_nutricionista, qt_prof_psicologo,
                        qt_prof_alimentacao, qt_prof_pedagogia,
                        qt_prof_secretario, qt_prof_seguranca,
                        qt_prof_minitores),
                      na.rm = TRUE
                    ))) %>%
                    select( c(uf, codmun, ano, school, 24:36) )
                  } else {
    ## 1.4 2021 ----
    temp <- read.csv2(path_list[j]) %>% 
      test <- temp %>%  select(NU_ANO_CENSO,
                               CO_UF,
                               CO_MUNICIPIO,
                               CO_ENTIDADE,
                               TP_DEPENDENCIA,
                               
                               IN_SALA_PROFESSOR,           #Teacher's room
                               IN_LABORATORIO_CIENCIAS,     #Science Lab
                               IN_LABORATORIO_INFORMATICA, #Computer Lab
                               IN_QUADRA_ESPORTES,          #Court
                               IN_PARQUE_INFANTIL,          #Playground
                               IN_BIBLIOTECA,               #Libary
                               QT_SALAS_EXISTENTES,         #Classrooms
                               QT_SALAS_UTILIZADAS,
                               QT_FUNCIONARIOS,             #Employee
                               IN_ALIMENTACAO,              #Lunch
                               
                               IN_INF,                      #Kindergarden
                               IN_FUND,                     #Elementary
                               IN_MED,                      #Highschool
                               IN_ESP                       #Special
                               
      ) %>% 
        filter(TP_DEPENDENCIA == 3) %>% 
        mutate(
          
          #School characteristics variables
          classroom = ifelse(!is.na(QT_SALAS_UTILIZADAS),
                             as.numeric(QT_SALAS_UTILIZADAS), NA), 
          
          teach_room = as.numeric(IN_SALA_PROFESSOR),
          
          lab_dummy = ifelse(IN_LABORATORIO_INFORMATICA == 1 |
                               IN_LABORATORIO_CIENCIAS == 1, 1, 0),
          
          lib_dummy = as.numeric(IN_BIBLIOTECA),
          
          play_area = ifelse(IN_PARQUE_INFANTIL == 1 |
                               IN_QUADRA_ESPORTES == 1, 1, 0),
          
          lunch = as.numeric(IN_ALIMENTACAO),
          
          employee = ifelse(!is.na(QT_FUNCIONARIOS),
                            as.numeric(QT_FUNCIONARIOS), NA),
          
          #teacher = as.numeric(PROFESS), #Not present in this data frame.
          
          #Education levels
          kinder = as.numeric(IN_INF),
          
          elementary = as.numeric(IN_FUND),
          
          high = as.numeric(IN_MED),
          
          inclusion = as.numeric(IN_ESP)
        ) %>% 
        rename(
          uf = CO_UF,
          codmun = CO_MUNICIPIO,
          ano = NU_ANO_CENSO,
          school = CO_ENTIDADE
        ) %>% 
        select( c(uf, codmun, ano, school, 20:30) ) #Final datafilter
      
      
                  } 
          }
      message("Finishing data preparation for ", i)
  }
  
  #Binding itno a single dataframe
  if(i == 2005){
    data <- temp
    
    message("Successfully created reference dataframe")
  } else {
    data <- rbind(data, temp) %>% 
      arrange(codmun,ano)
    
    message("Successfull binding")
    message("Total years in final data frame: ", paste(unique(data$ano), collapse = ", "))
  }
  
  fim <- Sys.time()
  
  
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  
  message("---------------------------------------------")
  message("Total time elapsed: ",mins," mins e ", secs, " s")
  message("---------------------------------------------")
  
  rm(temp, delta, ini, fim, temp, mins, secs, i, j)
}
rm(path_list)

# 2. Saving data ----
saveRDS(data, "Z:/Tuffy/Paper - Educ/Dados/censo_escolar_base.rds")

rm(data)

# ---------------------------------------------------------------------------- #
# 3. Extracting total enrollments per school ----
# ---------------------------------------------------------------------------- #

path_list <- c("Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2005/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2006/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2007.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2008.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2009.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2010.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2011.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2012.rds",                          "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2013.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2014.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2015.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2016.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2017.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2018.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2019.rds",
               "Z:/Arquivos IFB/Censo Escolar/Situação do Aluno/Dados RDS/ts_censo_basico_situacao_2020.rds",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2021/2021/dados/microdados_ed_basica_2021.csv"
               )

for (i in c(2005:2018)) {
  
  
  gc()
  message("Year: ",i)
  
  ini <- Sys.time()
  
  j <- i - 2004 #path index
  
  if (i <= 2006){
    ## 3.1 2005 - 2006 ----
    
    temp <- read_dta(path_list[j]) %>%
      select(c(1:24,
               SAL_DE_P, #Teacher's room
               PERMANEN, #Classroom
               LAB_INFO, #Computer Lab
               LAB_CIEN, #Science Lab
               LAB_OUTR, #Other Lab
               BIBLIO,   #Library
               FUNCION,  #Employee
               PROFESS,  #Teachers
               MERE_ESC, #Lunch
               AUX_CRECHE, #Auxiliar at Kindergarden
               AUX_PRE,  #Auxiliar at Kindergarden
               228:241,  #Teacher's especiality
               PARQ_INF, #Playground
               QUAD_DES, #Court
               QUAD_COB,  #Covered court

               #For the Enrollments
               DEF11C:DEF11F, NEF11C:NEF11F,      # EF iniciais (8 anos)
               DE9F11C:DE9F11G, NE9F11C:NE9F11G,  # EF iniciais (9 anos)
               DEF11G:DEF11J, NEF11G:NEF11J,      # EF finais (8 anos)
               DE9F11H:DE9F11N, NE9F11H:NE9F11N,  # EF finais (9 anos)
               VEE1431:VEE1437, # Alunos de educação especial do EF por ano de nascimento

               #ED especial por série:
               VEE1619:VEE1691, VEE1719:VEE1791, VEE1819:VEE1891, VEE1919:VEE1991, # 1ºEF
               VEE1612:VEE1692, VEE1712:VEE1792, VEE1812:VEE1892, VEE1912:VEE1992, # 2ºEF
               VEE1613:VEE1693, VEE1713:VEE1793, VEE1813:VEE1893, VEE1913:VEE1993, # 3ºEF
               VEE1614:VEE1694, VEE1714:VEE1794, VEE1814:VEE1894, VEE1914:VEE1994, # 4ºEF
               VEE1615:VEE1695, VEE1715:VEE1795, VEE1815:VEE1895, VEE1915:VEE1995, # 5ºEF
               VEE1616:VEE1696, VEE1716:VEE1796, VEE1816:VEE1896, VEE1916:VEE1996, # 6ºEF
               VEE1617:VEE1697, VEE1717:VEE1797, VEE1817:VEE1897, VEE1917:VEE1997, # 7ºEF
               VEE1618:VEE1698, VEE1718:VEE1798, VEE1818:VEE1898, VEE1918:VEE1998, # 8ºEF

               DEM118, DEM119, DEM11A, DEM11B, DEM11C, #Highschool
               NEM118, NEM119, NEM11A, NEM11B, NEM11C,
               DPE119, NPE119,                         #Preschool (N) - Daycare
               DPE11D, NPE11D,                         #          (D) - Preschool
               DES101F:DES101A, NES101F:NES101A        #EJA
      )
      ) %>%
      filter(DEP == "Municipal", # Removes Federal and Private Schools
             CODFUNC == "Ativo") %>%  # Removes deactivated schools
      mutate(
        CODMUNIC = as.numeric(str_c( #concatenates
          str_sub(as.character(CODMUNIC), 1, 2), #only first two strings
          str_sub(as.character(CODMUNIC), -5))),

        #School characteristics variables
        classroom = as.numeric(PERMANEN),
        teach_room = ifelse(SAL_DE_P == "s", 1, 0),
        lab_dummy = ifelse(LAB_INFO == "s" | LAB_CIEN == "s" | LAB_OUTR == "s",
                           1, 0),
        lib_dummy = ifelse(BIBLIO == "s", 1, 0),
        play_area = ifelse(PARQ_INF == "s"| QUAD_DES == "s" | QUAD_COB == "s",
                           1, 0),
        lunch = ifelse(MERE_ESC == "s", 1, 0),
        employee = as.numeric(FUNCION),
        teacher = as.numeric(PROFESS),

        #Education levels
        kinder = ifelse(NIVELCRE == "s" | NIVELPRE == "s" , 1, 0),
        elementary = ifelse(NIV_F1A4_8 == "s" | NIV_F5A8_8 == "s" | NIV_F9FIM == "s" |
                              NIV_F9INI == "s", 1, 0),
        high = ifelse(NIVELMED == "s", 1, 0),
        inclusion = ifelse(ESP_EXCL == "s" | ESP_T_ES == "s" | ENS_INCL == "s",
                           1, 0),

        eja = ifelse(SUPL_AVA == "s" |
                       SUPL_SAVA == "s", 1, 0),

        reg_in = rowSums(across(c(DEF11C:NE9F11G)), na.rm = TRUE), # Contando EF de 8 e 9 anos!
        reg_fin = rowSums(across(c(DEF11G:NE9F11N)), na.rm = TRUE),

        ef_tot = reg_in + reg_fin,
        esp_tot = rowSums(across(c(VEE1431:VEE1437)), na.rm = TRUE),
        em_tot = rowSums(across(c(DEM118:NEM11C)), na.rm = TRUE),
        ed_inf_tot = rowSums(across(c(DPE119:NPE11D)), na.rm = TRUE),
        eja_tot = rowSums(across(c(DES101F:NES101A)), na.rm = TRUE),
        day_tot = rowSums(across(c(NPE119,NPE11D)), na.rm = TRUE),
        pre_tot = rowSums(across(c(DPE119,DPE11D)), na.rm = TRUE)


      ) %>%
      select(c(1:9,53:65, ef_tot:pre_tot)) %>%
      rename(
        ano = ANO,
        school = MASCARA,
        codmun = CODMUNIC
      ) %>%
      select(ano, school, codmun, ef_tot:pre_tot) %>%
      mutate( uf = codmun %/% 100000) 
    
    
  } else {
    ##3.2 2007 - 2018 ----  
    
    
    
    if(i == 2018){
     
      temp <- readRDS(path_list[j]) %>%
        rename_all(tolower) %>% 
        select(co_uf,
               co_municipio,
               nu_ano_censo,
               tp_etapa_ensino,
               tp_dependencia,
               co_entidade,
               in_necessidade_especial
        ) %>% 
        filter(tp_dependencia == 3) %>% 
        rename(in_especial_exclusiva = in_necessidade_especial) #This is a abandoned variable
      
      gc()
      
      message("Openned data base for: ", i)
      
    } else {
    
    
    temp <- readRDS(path_list[j]) %>%
      rename_all(tolower) %>% 
      select(co_uf,
             co_municipio,
             nu_ano_censo,
             tp_etapa_ensino,
             tp_dependencia,
             co_entidade,
             in_especial_exclusiva
      ) %>% 
      filter(tp_dependencia == 3)
        
    message("Openned data base for: ", i)
    
    }
      
      #Summarise for each school year...
      
      temp <- temp %>% 
        mutate(
          tp_etapa_ensino = as.numeric(tp_etapa_ensino),
          
          inf = ifelse(tp_etapa_ensino %in% c(1,2), 1, 0),    #Preschool
          ef = ifelse(tp_etapa_ensino %in% c(4:21,41), 1 ,0), #Elementary
          em = ifelse(tp_etapa_ensino %in% c(25:38), 1, 0),   #HighSchooç
          eja = ifelse(tp_etapa_ensino %in% c(65:74), 1, 0),  #EJA
          day = ifelse(tp_etapa_ensino == 1, 1, 0),           #Daycare
          pre = ifelse(tp_etapa_ensino == 2, 1, 0)            #Preschool
          
        ) %>% 
        group_by(co_uf, co_municipio, co_entidade, nu_ano_censo) %>% 
        summarise(
          ef_tot = sum(ef, na.rm = T),
          esp_tot = sum(as.numeric(in_especial_exclusiva), 1, 0),
          em_tot = sum(em, na.rm = T),
          ed_inf_tot = sum(inf, na.rm = T),
          eja_tot = sum(eja, na.rm = T),
          day_tot = sum(day, na.rm = T),
          pre_tot = sum(pre, na.rm = T),
        
          .groups = "drop"
          ) %>% 
        rename(
          ano = nu_ano_censo,
          school = co_entidade, 
          codmun = co_municipio,
          uf = co_uf
        ) %>% 
        mutate(codmun = as.numeric(codmun))
    }
  

  
  # ------------------------------------------------------------ #
  #Binding itno a single dataframe
  if(i == 2005){
    data <- temp
    
    message("Successfully created reference dataframe")
  } else {
    data <- rbind(data, temp) %>% 
      arrange(codmun,ano)
    
    message("Successfull binding")
    message("Total years in final data frame: ", paste(unique(data$ano), collapse = ", "))
  }
  
  fim <- Sys.time()
  
  
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  
  message("---------------------------------------------")
  message("Total time elapsed: ",mins," mins e ", secs, " s")
  message("---------------------------------------------")
  
  rm(temp, delta, ini, fim, temp, mins, secs, i, j)
  
  gc()
  
  }

saveRDS(data,"Z:/Tuffy/Paper - Educ/Dados/matriculas_por_escola.rds")


#4. Merging the two datasets ----

df_school <- readRDS("Z:/Tuffy/Paper - Educ/Dados/censo_escolar_base.rds")

df_combined <- df_school %>% 
  left_join(data,
            by = c("uf" = "uf", "ano" = "ano", "codmun" = "codmun", "school" = "school")) %>% 
  group_by(school, ano) %>% 
  mutate(
    enroll = ef_tot + em_tot + ed_inf_tot + eja_tot #We dont count the discarded variable of esp
  ) %>% 
  ungroup()

saveRDS(df_combined, "Z:/Tuffy/Paper - Educ/Dados/censo_escolar_base_v2.rds")
rm(df_school, data)


rm(list = ls())
gc

