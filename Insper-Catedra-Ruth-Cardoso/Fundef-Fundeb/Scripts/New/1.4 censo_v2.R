# ---------------------------------------------------------------------------- #
# Data Extraction
# DataBase adjustment
# Last edited by: Tuffy Licciardi Issa
# Date: 14/07/2025
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
#  School data ----
# ---------------------------------------------------------------------------- #

#' ***************************************************************************
#' In this sectio I will extract information regarding school characteristics from
#' Brazil's School Census. With the data collect in this code I plan to investi-
#' gate the violation of the pretrends in the SAEB regressions. Hopefully it is
#' possible to uncover the improvent in SAEB scores.
#' ***************************************************************************


path_list <- c("Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar1998/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar1999/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2000/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2001/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2002/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2003/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2004/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2005/DADOS/DADOS_CENSOESC.dta",
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
# ---------------------------------------------------------------------------- #
#1. Loop School Infra + teachers -----
# ---------------------------------------------------------------------------- #
for(i in c(1998:2018)){
  
  gc()
  message("Year: ",i)
  
  ini <- Sys.time()
  
  j <- i - 1997 #path index
  
  #Total running time:
  if (j == 1){
    main_time <- Sys.time()
  }

  # ------------------------------------------------------------------------ #
  ## 1.1 1998 - 2000 ----
  # ------------------------------------------------------------------------ #
  
  if (i < 2001 ) {

    
    temp <- read_dta(path_list[j])
    
    # ---------------------------------------------------------------------- #
    ## 1.1.1 1998 ----
    # ---------------------------------------------------------------------- #
    if (i < 2000) {

      
      
        if( i == 1998) {
          temp <- temp %>% 
            select(c(1:24,
                     PERMANEN, #Classroom
                     LAB_INFO, #Computer Lab
                     LAB_CIEN, #Science Lab
                     LAB_OUTR, #Other Lab
                     BIBLIO,   #Library
                     FUNCION,  #Employee
                     PROFESS,  #Teachers
                     MERENDA, #Lunch
                     228:241,  #Teacher's especiality
                     PARQ_INF, #Playground
                     QUADRA, #Court
                     
                     # SANI_PRE, #Adapt elementary toilet
                     # FRALD,    #Dipper changers
                     
                     ENER_INE, #Non-existing energy
                     ENER_PUB, #Pub. Infra. energy
                     AGUA_INE, #Non-existing water
                     AGUA_PUB, #Pub. Infra. water
                     ESG_INEX,  #Non-existing sewage
                     ESG_PUB,  #Pub. Infra. water
                     
                     # -- Teacher Education -- #
                     VDG1C1:VDG1C3, #N° Teachers (Day, Kinder, Middle)
                     
                     VDG125, VDG126, VDG127, VDG135, VDG136, VDG137, #Pre with college
                     VDG145, VDG146, VDG147, #Pre (alfabetização)
                     VDG155, VDG156, VDG157, VDG165, VDG166, VDG167 #Fund(8)
            )
            ) %>% 
            filter(CODFUNC == "Ativo")  # Removes deactivated schools
          
          
          temp <- temp %>% 
            mutate(
              
              #School characteristics variables
              classroom = as.numeric(PERMANEN),
              
              teach_room = NA,
              
              lab_dummy = ifelse(LAB_INFO == "s" | LAB_CIEN == "s" | LAB_OUTR == "s",
                                 1, 0),
              
              lib_dummy = ifelse(BIBLIO == "s", 1, 0),
              
              play_area = ifelse(PARQ_INF == "s"| QUADRA == "s",
                                 1, 0),
              
              lunch = ifelse(MERENDA == "s", 1, 0),
              
              employee = as.numeric(FUNCION),
              
              # diaper = ifelse(FRALD == "s", 1, 0), #Diaper changing room
              # 
              # toilet_adapt = ifelse(SANI_PRE == "s", 1, 0), #Toilet adapted for Preschool
              
              # --- Teachers --- #
              
              teacher = as.numeric(PROFESS),
              # aux_cre = as.numeric(AUX_CRECHE),
              # aux_pre = as.numeric(AUX_PRE),
              
              t_fund = as.numeric(VDG1C3), #N° at Middle
              
              t_pree = rowSums(
                data.frame(
                  as.numeric(VDG1C1),
                  as.numeric(VDG1C2)
                ),
                na.rm = TRUE
              ), #N° at Preschool
              
              t_crec = NA, #N° at Daycare
              
              ## Education
              t_crec_educ = NA, #College in Daycare
              
              t_pree_educ = rowSums(
                across(c(
                  VDG125, VDG126, VDG127,
                  VDG135, VDG136, VDG137,
                  VDG145, VDG146, VDG147
                )),
                na.rm = TRUE
              ), #College in Preschool
              
              t_fund_educ = rowSums(
                across(c(
                  VDG155, VDG156, VDG157,  #Fund(8)
                  VDG165, VDG166, VDG167   #Fund(8)
                )),
                na.rm = TRUE
              ), #Middle (8 and 9 years)#Middle (8 and 9 years)
              
              
              # --- Infra --- #
              
              water_non = ifelse(AGUA_INE == "s", 1, 0), #Non-access to Water
              water_dummy = ifelse(AGUA_PUB == "s", 1, 0), #Public Water system
              
              sewage_non = ifelse(ESG_INEX == "s", 1, 0), #Non-access to sewage
              sewage_dummy = ifelse(ESG_PUB == "s", 1, 0), #Public sewage system
              
              energy_non = ifelse(ENER_INE == "s", 1, 0), #Non-access to eletric energy
              energy_dummy = ifelse(ENER_PUB == "s", 1, 0), #Public energy
              
              affil_mun  = NA,
              affiliated = NA #No information on affiliation
              
            ) %>% 
            select(c(1:9,classroom:affiliated)) %>% 
            rename(
              ano = ANO,
              dep_adm = DEP,
              school = MASCARA,
              codmunic = CODMUNIC
            ) %>% 
            mutate(
              codmun = str_c(str_sub(codmunic, 1,2),
                             str_sub(codmunic, 8,12))) %>% 
            select(-c(UF, SIGLA, LOC, CODFUNC, MUNIC, codmunic)) %>% 
            mutate( uf = as.numeric(codmun) %/% 100000)
        
        } else {
          # ------------------------------------------------------------------ #
          ## 1.1.2 1999 ----
          # ------------------------------------------------------------------ #
          
          temp <- temp %>% 
            select(c(1:24,
                     SAL_DE_P, #Teacher's room
                     PERMANEN, #Classroom
                     LAB_INFO, #Computer Lab
                     LAB_CIEN, #Science Lab
                     LAB_OUTR, #Other Lab
                     BIBLIO,   #Library
                     FUNCION,  #Employee
                     PROFESS,  #Teachers
                     MERENDA, #Lunch
                     228:241,  #Teacher's especiality
                     PARQ_INF, #Playground
                     QUADRA, #Court
                     
                     # SANI_PRE, #Adapt elementary toilet
                     # FRALD,    #Dipper changers
                     
                     ENER_INE, #Non-existing energy
                     ENER_PUB, #Pub. Infra. energy
                     AGUA_INE, #Non-existing water
                     AGUA_PUB, #Pub. Infra. water
                     ESG_INEX,  #Non-existing sewage
                     ESG_PUB,  #Pub. Infra. water
                     
                     # -- Teacher Education -- #
                     VDG1CA:VDG1C3, #N° Teachers (Day, Kinder, Middle)
                     
                     VDG1E5, VDG1E6, VDG1E7, VDG1F5, VDG1F6, VDG1F7, #Creche with College
                     VDG125, VDG126, VDG127, VDG135, VDG136, VDG137, #Pre with college
                     VDG145, VDG146, VDG147, #Pre (alfabetização)
                     VDG155, VDG156, VDG157, VDG165, VDG166, VDG167 #Fund(8)
            )
            ) %>% 
            filter(CODFUNC == "Ativo")  # Removes deactivated schools
          
          
          temp <- temp %>% 
            mutate(
              
              #School characteristics variables
              classroom = as.numeric(PERMANEN),
              
              teach_room = ifelse(SAL_DE_P == "s", 1, 0),
              
              lab_dummy = ifelse(LAB_INFO == "s" | LAB_CIEN == "s" | LAB_OUTR == "s",
                                 1, 0),
              
              lib_dummy = ifelse(BIBLIO == "s", 1, 0),
              
              play_area = ifelse(PARQ_INF == "s"| QUADRA == "s",
                                 1, 0),
              
              lunch = ifelse(MERENDA == "s", 1, 0),
              
              employee = as.numeric(FUNCION),
              
              # diaper = ifelse(FRALD == "s", 1, 0), #Diaper changing room
              # 
              # toilet_adapt = ifelse(SANI_PRE == "s", 1, 0), #Toilet adapted for Preschool
              
              # --- Teachers --- #
              
              teacher = as.numeric(PROFESS),
              # aux_cre = as.numeric(AUX_CRECHE),
              # aux_pre = as.numeric(AUX_PRE),
              
              t_fund  = as.numeric(VDG1C3), #N° at Middle
              t_pree = rowSums(
                data.frame(
                  as.numeric(VDG1C1),
                  as.numeric(VDG1C2)
                ),
                na.rm = TRUE
              ), #N° at Preschool
              t_crec  = as.numeric(VDG1CA), #N³ at Daycare
              
              ## Education
              t_crec_educ = rowSums(
                across(c(
                  VDG1E5, VDG1E6, VDG1E7,
                  VDG1F5, VDG1F6, VDG1F7
                )),
                na.rm = TRUE
              ), #College in Daycare
              
              t_pree_educ = rowSums(
                across(c(
                  VDG125, VDG126, VDG127,
                  VDG135, VDG136, VDG137,
                  VDG145, VDG146, VDG147
                )),
                na.rm = TRUE
              ), #College in Preschool
              
              t_fund_educ = rowSums(
                across(c(
                  VDG155, VDG156, VDG157,  #Fund(8)
                  VDG165, VDG166, VDG167   #Fund(8)
                )),
                na.rm = TRUE
              ), #Middle (8 and 9 years)                        #Middle (8 and 9 years)
              
              
              # --- Infra --- #
              
              water_non = ifelse(AGUA_INE == "s", 1, 0), #Non-access to Water
              water_dummy = ifelse(AGUA_PUB == "s", 1, 0), #Public Water system
              
              sewage_non = ifelse(ESG_INEX == "s", 1, 0), #Non-access to sewage
              sewage_dummy = ifelse(ESG_PUB == "s", 1, 0), #Public sewage system
              
              energy_non = ifelse(ENER_INE == "s", 1, 0), #Non-access to eletric energy
              energy_dummy = ifelse(ENER_PUB == "s", 1, 0), #Public energy
              
              affil_mun  = NA,
              affiliated = NA #No information on affiliation
              
            ) %>% 
            select(c(1:9,classroom:affiliated)) %>% 
            rename(
              ano = ANO,
              dep_adm = DEP,
              school = MASCARA,
              codmunic = CODMUNIC
            ) %>% 
            mutate(
              codmun = str_c(str_sub(codmunic, 1,2),
                             str_sub(codmunic, 8,12))) %>% 
            select(-c(UF, SIGLA, LOC, CODFUNC, MUNIC, codmunic)) %>% 
            mutate( uf = as.numeric(codmun) %/% 100000)
          
        }
      
        
    } else {
      # ------------------------------------------ #
      ### 1.2 2000 ----
      # ------------------------------------------ #
      
      temp <- temp %>% 
        select(c(1:24,
                 SAL_DE_P, #Teacher's room
                 PERMANEN, #Classroom
                 LAB_INFO, #Computer Lab
                 LAB_CIEN, #Science Lab
                 LAB_OUTR, #Other Lab
                 BIBLIOTE,   #Library
                 FUNCION,  #Employee
                 PROFESS,  #Teachers
                 MERENDA, #Lunch
                 228:241,  #Teacher's especiality
                 PARQ_INF, #Playground
                 QUADRA, #Court
  
                 # SANI_PRE, #Adapt elementary toilet
                 # FRALD,    #Dipper changers
                 
                 ENER_INE, #Non-existing energy
                 ENER_PUB, #Pub. Infra. energy
                 AGUA_INE, #Non-existing water
                 AGUA_PUB, #Pub. Infra. water
                 ESG_INEX,  #Non-existing sewage
                 ESG_PUB,  #Pub. Infra. water
                 
                 # -- Teacher Education -- #
                 VDG1CA:VDG1C3, #N° Teachers (Day, Kinder, Middle)
                 
                 VDG1E5, VDG1E6, VDG1E7, VDG1F5, VDG1F6, VDG1F7, #Creche with College
                 VDG125, VDG126, VDG127, VDG135, VDG136, VDG137, #Pre with college
                 VDG145, VDG146, VDG147, #Pre (alfabetização)
                 VDG155, VDG156, VDG157, VDG165, VDG166, VDG167 #Fund(8)
        )
        ) %>% 
        filter(CODFUNC == "Ativo")  # Removes deactivated schools
      
      temp <- temp %>% 
        mutate(
          #School characteristics variables
          classroom = as.numeric(PERMANEN),
          
          teach_room = ifelse(SAL_DE_P == "s", 1, 0),
          
          lab_dummy = ifelse(LAB_INFO == "s" | LAB_CIEN == "s" | LAB_OUTR == "s",
                             1, 0),
          
          lib_dummy = ifelse(BIBLIOTE == "s", 1, 0),
          
          play_area = ifelse(PARQ_INF == "s"| QUADRA == "s",
                             1, 0),
          
          lunch = ifelse(MERENDA == "s", 1, 0),
          
          employee = as.numeric(FUNCION),
          
          # diaper = ifelse(FRALD == "s", 1, 0), #Diaper changing room
          # 
          # toilet_adapt = ifelse(SANI_PRE == "s", 1, 0), #Toilet adapted for Preschool
          
          # --- Teachers --- #
          
          teacher = as.numeric(PROFESS),
          # aux_cre = as.numeric(AUX_CRECHE),
          # aux_pre = as.numeric(AUX_PRE),
          
          t_fund  = as.numeric(VDG1C3), #N° at Middle
          t_pree = rowSums(
            data.frame(
              as.numeric(VDG1C1),
              as.numeric(VDG1C2)
            ),
            na.rm = TRUE
          ), # N° at Preschool
          t_crec  = as.numeric(VDG1CA), #N³ at Daycare
          
          ## Education
          t_crec_educ = rowSums(
            across(c(
              VDG1E5, VDG1E6, VDG1E7,
              VDG1F5, VDG1F6, VDG1F7
            )),
            na.rm = TRUE
          ), #College in Daycare
          
          t_pree_educ = rowSums(
            across(c(
              VDG125, VDG126, VDG127,
              VDG135, VDG136, VDG137,
              VDG145, VDG146, VDG147
            )),
            na.rm = TRUE
          ), #College in Preschool
          
          t_fund_educ = rowSums(
            across(c(
              VDG155, VDG156, VDG157,  #Fund(8)
              VDG165, VDG166, VDG167   #Fund(8)
            )),
            na.rm = TRUE
          ), #Middle (8 and 9 years)                        
          
          
          # --- Infra --- #
          
          water_non = ifelse(AGUA_INE == "s", 1, 0), #Non-access to Water
          water_dummy = ifelse(AGUA_PUB == "s", 1, 0), #Public Water system
          
          sewage_non = ifelse(ESG_INEX == "s", 1, 0), #Non-access to sewage
          sewage_dummy = ifelse(ESG_PUB == "s", 1, 0), #Public sewage system
          
          energy_non = ifelse(ENER_INE == "s", 1, 0), #Non-access to eletric energy
          energy_dummy = ifelse(ENER_PUB == "s", 1, 0), #Public energy
          
          affil_mun  = NA,
          affiliated = NA #No information on affiliation
          
        ) %>% 
        select(c(1:9,classroom:affiliated)) %>% 
        rename(
          ano = ANO,
          dep_adm = DEP,
          school = MASCARA,
          codmunic = CODMUNIC
        ) %>% 
        mutate(
          codmun = str_c(str_sub(codmunic, 1,2),
                         str_sub(codmunic, 8,12))) %>% 
        select(-c(UF, SIGLA, LOC, CODFUNC, MUNIC, codmunic)) %>% 
        mutate( uf = as.numeric(codmun) %/% 100000) 
      
      }
    
    
    
  } else if (i %in% c(2001:2003)) {
    # ------------------------------------------------------------------------ #
    ## 1.3 2001 - 2003 ----
    # ------------------------------------------------------------------------ #
    
    temp <- read_dta(path_list[j]) %>% 
      select(c(1:24,
               SAL_DE_P, #Teacher's room
               PERMANEN, #Classroom
               LAB_INFO, #Computer Lab
               LAB_CIEN, #Science Lab
               LAB_OUTR, #Other Lab
               BIBLIOTE,   #Library
               FUNCION,  #Employee
               PROFESS,  #Teachers
               MERE_ESC, #Lunch
               228:241,  #Teacher's especiality
               PARQ_INF, #Playground
               QUAD_DES, #Court
               QUAD_COB, #Covered court

               # SANI_PRE, #Adapt elementary toilet
               # FRALD,    #Dipper changers
               
               ENER_INE, #Non-existing energy
               ENER_PUB, #Pub. Infra. energy
               AGUA_INE, #Non-existing water
               AGUA_PUB, #Pub. Infra. water
               ESG_INEX,  #Non-existing sewage
               ESG_PUB,  #Pub. Infra. water
               
               # -- Teacher Education -- #
               VDG1CA:VDG1C3, #N° Teachers (Day, Kinder, Middle)
               
               VDG1E5, VDG1E6, VDG1E7, VDG1F5, VDG1F6, VDG1F7, #Creche with College
               VDG125, VDG126, VDG127, VDG135, VDG136, VDG137, #Pre with college
               VDG145, VDG146, VDG147, #Pre (alfabetização)
               VDG155, VDG156, VDG157, VDG165, VDG166, VDG167 #Fund(8)
               )
             ) %>% 
      filter(CODFUNC == "Ativo")  # Removes deactivated schools
    
    temp <- temp %>% 
      mutate(
        #School characteristics variables
        classroom = as.numeric(PERMANEN),
        
        teach_room = ifelse(SAL_DE_P == "s", 1, 0),
        
        lab_dummy = ifelse(LAB_INFO == "s" | LAB_CIEN == "s" | LAB_OUTR == "s",
                           1, 0),
        
        lib_dummy = ifelse(BIBLIOTE == "s", 1, 0),
        
        play_area = ifelse(PARQ_INF == "s"| QUAD_DES == "s" |
                           QUAD_COB == "s",
                           1, 0),
        
        lunch = ifelse(MERE_ESC == "s", 1, 0),
        
        employee = as.numeric(FUNCION),
        
        # diaper = ifelse(FRALD == "s", 1, 0), #Diaper changing room
        # 
        # toilet_adapt = ifelse(SANI_PRE == "s", 1, 0), #Toilet adapted for Preschool
        
        # --- Teachers --- #
        
        teacher = as.numeric(PROFESS),
        # aux_cre = as.numeric(AUX_CRECHE),
        # aux_pre = as.numeric(AUX_PRE),
        
        t_fund  = as.numeric(VDG1C3), #N° at Middle
        t_pree = rowSums(
          sapply(across(c(VDG1C1, VDG1C2)), as.numeric),
          na.rm = TRUE
        ), # N° at Preschool        
        t_crec  = as.numeric(VDG1CA), #N³ at Daycare
        
        ## Education
        t_crec_educ = rowSums(
          across(c(
            VDG1E5, VDG1E6, VDG1E7,
            VDG1F5, VDG1F6, VDG1F7
          )),
          na.rm = TRUE
        ), #College in Daycare
        
        t_pree_educ = rowSums(
          across(c(
            VDG125, VDG126, VDG127,
            VDG135, VDG136, VDG137,
            VDG145, VDG146, VDG147
          )),
          na.rm = TRUE
        ), #College in Preschool
        
        t_fund_educ = rowSums(
          across(c(
            VDG155, VDG156, VDG157,
            VDG165, VDG166, VDG167 #Fund(8)
          )),
          na.rm = TRUE
        ), #Middle (8 and 9 years)#Middle (8 and 9 years)
        
        
        # --- Infra --- #
        
        water_non = ifelse(AGUA_INE == "s", 1, 0), #Non-access to Water
        water_dummy = ifelse(AGUA_PUB == "s", 1, 0), #Public Water system
        
        sewage_non = ifelse(ESG_INEX == "s", 1, 0), #Non-access to sewage
        sewage_dummy = ifelse(ESG_PUB == "s", 1, 0), #Public sewage system
        
        energy_non = ifelse(ENER_INE == "s", 1, 0), #Non-access to eletric energy
        energy_dummy = ifelse(ENER_PUB == "s", 1, 0), #Public energy
        
        affil_mun  = NA,
        affiliated = NA #No information on affiliation
        
      ) %>% 
      select(c(1:9,classroom:affiliated)) %>% 
      rename(
        ano = ANO,
        dep_adm = DEP,
        school = MASCARA,
        codmunic = CODMUNIC
      ) %>% 
      mutate(
        codmun = str_c(str_sub(codmunic, 1,2),
                       str_sub(codmunic, 8,12))) %>% 
      select(-c(UF, SIGLA, LOC, CODFUNC, MUNIC, codmunic)) %>% 
      mutate( uf = as.numeric(codmun) %/% 100000)
    
    
  } else if (i %in% c(2004:2006)) {
    # ------------------------------------------------------------------------ #
    ##1.4 2004-2006 years ----
    # ------------------------------------------------------------------------ #
    
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
               QUAD_COB, #Covered court
               GIN_ESP,  #Sport Ginasium
               
               # SANI_PRE, #Adapt elementary toilet
               # FRALD,    #Dipper changers
               
               ENER_INE, #Non-existing energy
               ENER_PUB, #Pub. Infra. energy
               AGUA_INE, #Non-existing water
               AGUA_PUB, #Pub. Infra. water
               ESG_INEX,  #Non-existing sewage
               ESG_PUB,  #Pub. Infra. water
               
               # -- Teacher Education -- #
               VDG1CA:VDG1C3, #N° Teachers (Day, Kinder, Middle)
               
               VDG1E5, VDG1E6, VDG1E7, VDG1F5, VDG1F6, VDG1F7, #Creche with College
               VDG125, VDG126, VDG127, VDG135, VDG136, VDG137, #Pre with college
               VDG155, VDG156, VDG157, VDG165, VDG166, VDG167, #Fund(8)
               VDG1L5, VDG1L6, VDG1L7, VDG1M5, VDG1M6, VDG1M7  #Fund(9)
               
               )
      ) %>% 
      filter(CODFUNC == "Ativo") %>%  # Removes deactivated schools
        mutate(
          codmun = str_c(str_sub(CODMUNIC, 1,2), #Formats municipality code
                         str_sub(CODMUNIC, 8,12))
          )
      
      
    temp <- temp %>% 
      mutate(
        
        #School characteristics variables
        classroom = as.numeric(PERMANEN),
        
        teach_room = ifelse(SAL_DE_P == "s", 1, 0),
        
        lab_dummy = ifelse(LAB_INFO == "s" | LAB_CIEN == "s" | LAB_OUTR == "s",
                           1, 0),
        
        lib_dummy = ifelse(BIBLIO == "s", 1, 0),
        
        play_area = ifelse(PARQ_INF == "s"| QUAD_DES == "s" |
                             QUAD_COB == "s" | GIN_ESP == "s",
                           1, 0),
        
        lunch = ifelse(MERE_ESC == "s", 1, 0),
        
        employee = as.numeric(FUNCION),
        
        # diaper = ifelse(FRALD == "s", 1, 0), #Diaper changing room
        # 
        # toilet_adapt = ifelse(SANI_PRE == "s", 1, 0), #Toilet adapted for Preschool
        
        # --- Teachers --- #
        
        teacher = as.numeric(PROFESS),
        # aux_cre = as.numeric(AUX_CRECHE),
        # aux_pre = as.numeric(AUX_PRE),
        
        t_fund  = as.numeric(VDG1C3), #N° at Middle
        t_pree  = as.numeric(VDG1C1), #N° at Preschool
        t_crec  = as.numeric(VDG1CA), #N³ at Daycare
        
        # --- Teacher Education --- #
        t_crec_educ = rowSums(
          across(c(
            VDG1E5, VDG1E6, VDG1E7,  # Daycare
            VDG1F5, VDG1F6, VDG1F7   # Daycare
          )),
          na.rm = TRUE
        ), #College in Daycare
        
        t_pree_educ = rowSums(
          across(c(
            VDG125, VDG126, VDG127,  # Preschool
            VDG135, VDG136, VDG137   # Preschool
          )),
          na.rm = TRUE
        ), #College in Preschool
        
        t_fund_educ = rowSums(
          across(c(
            VDG155, VDG156, VDG157,  # Fund(8)
            VDG165, VDG166, VDG167,  # Fund(8)
            
            VDG1L5, VDG1L6, VDG1L7,  # Fund(9)
            VDG1M5, VDG1M6, VDG1M7   # Fund(9)
          )),
          na.rm = TRUE
        ), #College in Elementary School
        
        # --- Infra --- #
        
        water_non = ifelse(AGUA_INE == "s", 1, 0), #Non-access to Water
        water_dummy = ifelse(AGUA_PUB == "s", 1, 0), #Public Water system
        
        sewage_non = ifelse(ESG_INEX == "s", 1, 0), #Non-access to sewage
        sewage_dummy = ifelse(ESG_PUB == "s", 1, 0), #Public sewage system
        
        energy_non = ifelse(ENER_INE == "s", 1, 0), #Non-access to eletric energy
        energy_dummy = ifelse(ENER_PUB == "s", 1, 0), #Public energy
        
        affil_mun  = NA,
        affiliated = NA #No information on affiliation
        
      ) %>% 
      select(c(1:9, codmun, classroom:affiliated)) %>% 
      rename(
        ano = ANO,
        dep_adm = DEP,
        school = MASCARA
        ) %>%
      select(-c(UF, SIGLA, LOC, CODFUNC, MUNIC, CODMUNIC)) %>% 
      mutate( uf = as.numeric(codmun) %/% 100000)
    

  } else if(i %in% c(2007:2014)) {
    # ------------------------------------------------------------------------ #
    ## 1.5 2007-2014 years ----
    # ------------------------------------------------------------------------ #
    
    
    temp <- read_dta(path_list[j]) %>%
      rename_all(tolower) 
    
    message("Opened database for year: ", i)
    
    
    #For teachers data
    regioes <- c("CO", "NORDESTE", "NORTE", "SUDESTE", "SUL")
    
    pasta_dados <- dirname(path_list[j])   # pega .../2007/DADOS
    
    
    if (i %in% c(2011:2014)){ #Valid for this years specific variables constructions
      # ---------------------------------------------------------------------- #
      ### 1.5.1 2011 - 2014 ----
      # ---------------------------------------------------------------------- #
      
      temp <- temp %>% 
        mutate(id_quadra_esportes = ifelse(id_quadra_esportes_coberta == 1 |
                                             id_quadra_esportes_descoberta == 1,
                                           1, 0)) %>% 
        filter(desc_situacao_funcionamento == 1)
      
      
      # --- Teacher DF --- #
      
      docentes_ano <- purrr::map_dfr(regioes, function(reg) {
        arq <- file.path(pasta_dados, paste0("DOCENTES_", reg, ".dta"))
        
        haven::read_dta(arq) %>%
          dplyr::rename_all(tolower) %>%
          dplyr::mutate(regiao = reg, ano = i)
      })
      
      message("Teacher data extraction completed")
      
      docentes_ano <- docentes_ano %>% 
        select(fk_cod_escolaridade,
               fk_cod_etapa_ensino,
               pk_cod_entidade,
               fk_cod_municipio,
               id_situacao_curso_1,
               id_situacao_curso_2,
               id_situacao_curso_3)
      
      # Grouping by school
      
      docentes_school <- docentes_ano %>% 
        group_by(pk_cod_entidade, fk_cod_municipio) %>% 
        summarise(
          teacher    = n(),
          
          t_fund     = sum(fk_cod_etapa_ensino %in% c(4:24, 41), na.rm = T),
          t_pree     = sum(fk_cod_etapa_ensino == 2, na.rm = T),
          t_crec     = sum(fk_cod_etapa_ensino == 1, na.rm = T),
          
          # - Education - #
          t_crec_educ = sum(fk_cod_etapa_ensino == 1 & fk_cod_escolaridade %in% c(6,7) &
                              (id_situacao_curso_1 == 1 | id_situacao_curso_2 == 1 |
                                 id_situacao_curso_3 == 1),
                            na.rm = T), #College in Daycare
          
          t_pree_educ = sum(fk_cod_etapa_ensino == 2 & fk_cod_escolaridade %in% c(6,7) &
                              (id_situacao_curso_1 == 1 | id_situacao_curso_2 == 1 |
                                 id_situacao_curso_3 == 1),
                            na.rm = T), #College in Preschool
          
          t_fund_educ = sum(fk_cod_etapa_ensino %in% c(4:24, 41) & fk_cod_escolaridade %in% c(6,7) &
                              (id_situacao_curso_1 == 1 | id_situacao_curso_2 == 1 |
                                 id_situacao_curso_3 == 1),
                            na.rm = T),
          .groups = "drop"
        )
      
      rm(docentes_ano)
      
      # -- Main DF -- #
      
      temp <- temp %>% 
        select(fk_cod_estado,
               fk_cod_municipio,
               ano_censo,
               id_dependencia_adm,
               pk_cod_entidade,
               id_conveniada_pp,               #Affiliation
               id_tipo_convenio_poder_publico, #Aff. type
               
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
               id_mod_ens_esp,
               
               # -- Infra -- #
               
               id_agua_inexistente,
               id_agua_rede_publica,
               id_energia_inexistente,
               id_energia_rede_publica,
               id_esgoto_inexistente,
               id_esgoto_rede_publica
        )
      
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
          
          # --- Infra --- #
          
          water_non = as.numeric(id_agua_inexistente), #Non-access to Water
          water_dummy = as.numeric(id_agua_rede_publica), #Public Water system
          
          sewage_non = as.numeric(id_esgoto_inexistente), #Non-access to sewage
          sewage_dummy = as.numeric(id_esgoto_rede_publica), #Public sewage system
          
          energy_non = as.numeric(id_energia_inexistente), #Non-access to eletric energy
          energy_dummy = as.numeric(id_energia_rede_publica), #Public energy
          
          affil_mun  = ifelse(id_tipo_convenio_poder_publico %in% c(2,3), 1, 0),
          affiliated = as.numeric(id_conveniada_pp) #No information on affiliation
        ) %>% 
        left_join(docentes_school, 
                  by = c("fk_cod_municipio", "pk_cod_entidade")) %>% 
        rename(
          uf = fk_cod_estado,
          codmun = fk_cod_municipio,
          dep_adm = id_dependencia_adm,
          ano = ano_censo,
          school = pk_cod_entidade
        ) %>% 
        select(1:5, classroom:t_fund_educ)
      
      
      rm(docentes_school)
      
      #For 2008-2010 the sports court code remains the same as 2007
    } else if (i %in% c(2008:2010)) { #2008 is the first year to present the change in notation
      # ---------------------------------------------------------------------- #
      ### 1.5.2 2008 - 2010 ----
      # ---------------------------------------------------------------------- #
      
      temp <- temp %>%
        filter(desc_situacao_funcionamento == 1)
      
      # --- Teacher DF --- #
      
      docentes_ano <- purrr::map_dfr(regioes, function(reg) {
        arq <- file.path(pasta_dados, paste0("DOCENTES_", reg, ".dta"))
        
        haven::read_dta(arq) %>%
          dplyr::rename_all(tolower) %>%
          dplyr::mutate(regiao = reg, ano = i)
      })
      
      message("Teacher data extraction completed")
      
      docentes_ano <- docentes_ano %>% 
        select(fk_cod_escolaridade,
               fk_cod_etapa_ensino,
               pk_cod_entidade,
               fk_cod_municipio)
      
      # Grouping by school
      
      docentes_school <- docentes_ano %>% 
        group_by(pk_cod_entidade, fk_cod_municipio) %>% 
        summarise(
          teacher    = n(),
          
          t_fund     = sum(fk_cod_etapa_ensino %in% c(4:24, 41), na.rm = T),
          t_pree     = sum(fk_cod_etapa_ensino == 2, na.rm = T),
          t_crec     = sum(fk_cod_etapa_ensino == 1, na.rm = T),
          
          # - Education - #
          t_crec_educ = sum(fk_cod_etapa_ensino == 1 & fk_cod_escolaridade %in% c(6,7),
                            na.rm = T), #College in Daycare
          
          t_pree_educ = sum(fk_cod_etapa_ensino == 2 & fk_cod_escolaridade %in% c(6,7),
                            na.rm = T), #College in Preschool
          
          t_fund_educ = sum(fk_cod_etapa_ensino %in% c(4:24, 41) & fk_cod_escolaridade %in% c(6,7),
                            na.rm = T),
          .groups = "drop" 
        )
      
      rm(docentes_ano)
      
      # -- Main DF -- #
      
      temp <- temp %>% 
        select(fk_cod_estado,
               fk_cod_municipio,
               ano_censo,
               id_dependencia_adm,
               pk_cod_entidade,
               id_conveniada_pp,               #Affiliation
               id_tipo_convenio_poder_publico, #Aff. type
               
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
               id_mod_ens_esp,
               
               # -- Infra -- #
               
               id_agua_inexistente,
               id_agua_rede_publica,
               id_energia_inexistente,
               id_energia_rede_publica,
               id_esgoto_inexistente,
               id_esgoto_rede_publica
        )
      
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
          
          # --- Infra --- #
          
          water_non = as.numeric(id_agua_inexistente), #Non-access to Water
          water_dummy = as.numeric(id_agua_rede_publica), #Public Water system
          
          sewage_non = as.numeric(id_esgoto_inexistente), #Non-access to sewage
          sewage_dummy = as.numeric(id_esgoto_rede_publica), #Public sewage system
          
          energy_non = as.numeric(id_energia_inexistente), #Non-access to eletric energy
          energy_dummy = as.numeric(id_energia_rede_publica), #Public energy
          
          affil_mun  = ifelse(id_tipo_convenio_poder_publico %in% c(2,3), 1, 0),
          affiliated = as.numeric(id_conveniada_pp) #No information on affiliation
        ) %>% 
        left_join(docentes_school, 
                  by = c("fk_cod_municipio", "pk_cod_entidade")) %>% 
        rename(
          uf = fk_cod_estado,
          codmun = fk_cod_municipio,
          dep_adm = id_dependencia_adm,
          ano = ano_censo,
          school = pk_cod_entidade
        ) %>% 
        select(1:5, classroom:t_fund_educ)
      
      rm(docentes_school)
      
    } else {
      # ---------------------------------------------------------------------- #
      ### 1.5.3 2007 ----
      # ---------------------------------------------------------------------- #
      
      temp <- temp %>% #Exclusive for 2007
        filter(desc_situacao_funcionamento == "EM ATIVIDADE") #Active Schools
      
      # --- Teacher DF --- #
      
      docentes_ano <- purrr::map_dfr(regioes, function(reg) {
        arq <- file.path(pasta_dados, paste0("DOCENTES_", reg, ".dta"))
        
        haven::read_dta(arq) %>%
          dplyr::rename_all(tolower) %>%
          dplyr::mutate(regiao = reg, ano = i)
      })
      
      message("Teacher data extraction completed")
      
      docentes_ano <- docentes_ano %>% 
        select(fk_cod_escolaridade,
               fk_cod_etapa_ensino,
               pk_cod_entidade,
               fk_cod_municipio)
      
    # Grouping by school
      
      docentes_school <- docentes_ano %>% 
        group_by(pk_cod_entidade, fk_cod_municipio) %>% 
        summarise(
          teacher    = n(),
          
          t_fund     = sum(fk_cod_etapa_ensino %in% c(4:24, 41), na.rm = T),
          t_pree     = sum(fk_cod_etapa_ensino == 2, na.rm = T),
          t_crec     = sum(fk_cod_etapa_ensino == 1, na.rm = T),
          
          # - Education - #
          t_crec_educ = sum(fk_cod_etapa_ensino == 1 & fk_cod_escolaridade %in% c(6,7),
                            na.rm = T), #College in Daycare
          
          t_pree_educ = sum(fk_cod_etapa_ensino == 2 & fk_cod_escolaridade %in% c(6,7),
                            na.rm = T), #College in Preschool
          
          t_fund_educ = sum(fk_cod_etapa_ensino %in% c(4:24, 41) & fk_cod_escolaridade %in% c(6,7),
                            na.rm = T),
          .groups = "drop" 
          )
      
      rm(docentes_ano)
      
      # -- Main DF -- #
      
      temp <- temp %>% 
        select(fk_cod_estado,
               fk_cod_municipio,
               ano_censo,
               id_dependencia_adm,
               pk_cod_entidade,
               id_conveniada_pp,           #Affiliation
               
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
               id_mod_ens_esp,
               
               # -- Infra -- #
               
               id_agua_inexistente,
               id_agua_rede_publica,
               id_energia_inexistente,
               id_energia_rede_publica,
               id_esgoto_inexistente,
               id_esgoto_rede_publica
        )
      
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
          
          # --- Infra --- #
          
          water_non = as.numeric(id_agua_inexistente), #Non-access to Water
          water_dummy = as.numeric(id_agua_rede_publica), #Public Water system
          
          sewage_non = as.numeric(id_esgoto_inexistente), #Non-access to sewage
          sewage_dummy = as.numeric(id_esgoto_rede_publica), #Public sewage system
          
          energy_non = as.numeric(id_energia_inexistente), #Non-access to eletric energy
          energy_dummy = as.numeric(id_energia_rede_publica), #Public energy
          
          affil_mun  = NA,
          affiliated = as.numeric(id_conveniada_pp) #No information on affiliation
        ) %>% 
        left_join(docentes_school, 
                  by = c("fk_cod_municipio", "pk_cod_entidade")) %>% 
        rename(
          uf = fk_cod_estado,
          codmun = fk_cod_municipio,
          dep_adm = id_dependencia_adm,
          ano = ano_censo,
          school = pk_cod_entidade
        ) %>% 
        select(1:5, classroom:t_fund_educ)
      
      
    } 
    
    

    
    
    message("Finishing data preparation for: ", i)
    
    
    
    rm(regioes, pasta_dados)
    
  } else if(i %in% c(2015:2020)) {
    ## 1.6 2015 - 2020 year ----
    
    temp <- read_dta(path_list[j]) %>% 
      rename_all(tolower) %>%
      filter(tp_situacao_funcionamento == 1) 
    
    
    # --- Teacher DF --- #
    
    #For teachers data
    regioes <- c("CO", "NORDESTE", "NORTE", "SUDESTE", "SUL")
    
    pasta_dados <- dirname(path_list[j])   # pega .../2007/DADOS
    
    
    
    docentes_ano <- purrr::map_dfr(regioes, function(reg) {
      arq <- file.path(pasta_dados, paste0("DOCENTES_", reg, ".dta"))
      
      haven::read_dta(arq) %>%
        dplyr::rename_all(tolower) %>%
        dplyr::mutate(regiao = reg, ano = i)
    })
    
    message("Teacher data extraction completed")
    
    docentes_ano <- docentes_ano %>% 
      select(tp_escolaridade,
             tp_etapa_ensino,
             co_entidade,
             co_municipio)
    
    # Grouping by school
    
    docentes_school <- docentes_ano %>% 
      group_by(co_entidade, co_municipio) %>% 
      summarise(
        teacher    = n(),
        
        t_fund     = sum(tp_etapa_ensino %in% c(4:24, 41), na.rm = T),
        t_pree     = sum(tp_etapa_ensino == 2, na.rm = T),
        t_crec     = sum(tp_etapa_ensino == 1, na.rm = T),
        
        # - Education - #
        t_crec_educ = sum(tp_etapa_ensino == 1 & tp_escolaridade == 4,
                          na.rm = T), #College in Daycare
        
        t_pree_educ = sum(tp_etapa_ensino == 2 & tp_escolaridade == 4,
                          na.rm = T), #College in Preschool
        
        t_fund_educ = sum(tp_etapa_ensino %in% c(4:24, 41) & tp_escolaridade == 4,
                          na.rm = T),
        .groups = "drop"
      )
    
    rm(docentes_ano)
    
    # ------------------------------------------------------------------------ #
    ### 1.6.1 2015 - 2017 ----
    # ------------------------------------------------------------------------ #
    if(i %in% c(2015:2017)) {
      
      temp <- temp %>% 
        select(nu_ano_censo,
               co_uf,
               co_municipio,
               co_entidade,
               tp_dependencia,
               in_conveniada_pp, #affiliation
               tp_convenio_poder_publico, #Type of affiliation
               
               
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
               in_especial_exclusiva,        #Special
               
               # --- Infra --- #
               in_agua_rede_publica,
               in_agua_inexistente,
               in_energia_rede_publica,
               in_energia_inexistente,
               in_esgoto_rede_publica,
               in_esgoto_inexistente
               
        ) %>% 
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
          
          # --- Infra --- #
          
          water_non = as.numeric(in_agua_inexistente), #Non-access to Water
          water_dummy = as.numeric(in_agua_rede_publica), #Public Water system
          
          sewage_non = as.numeric(in_esgoto_inexistente), #Non-access to sewage
          sewage_dummy = as.numeric(in_esgoto_rede_publica), #Public sewage system
          
          energy_non = as.numeric(in_energia_inexistente), #Non-access to eletric energy
          energy_dummy = as.numeric(in_energia_rede_publica), #Public energy
          
          affil_mun  = ifelse(tp_convenio_poder_publico %in% c(1,3), 1, 0), #2015 onwards changed the name
          affiliated = as.numeric(in_conveniada_pp) #No information on affiliation
          
          
          
        ) %>% 
        left_join(docentes_school,
                  by = c("co_municipio", "co_entidade")) %>% 
        rename(
          uf = co_uf,
          codmun = co_municipio,
          dep_adm = tp_dependencia,
          ano = nu_ano_censo,
          school = co_entidade
        ) %>% 
        select( c(uf, codmun, ano, school, dep_adm, classroom:t_fund_educ)) #Final datafilter'
      
      rm(docentes_school)
      
    } else if(i %in% c(2018:2020)) {
      # ---------------------------------------------------------------------- #
      ### 1.6.2 2018 ----
      # ---------------------------------------------------------------------- #
      
      if(i == 2018) {
        temp <- temp %>% 
          select(nu_ano_censo,
                 co_uf,
                 co_municipio,
                 co_entidade,
                 tp_dependencia,
                 in_conveniada_pp, #affiliation
                 tp_convenio_poder_publico, #Type of affiliation
                 
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
                 in_especial_exclusiva,        #Special
                 
                 # --- Infra --- #
                 in_agua_rede_publica,
                 in_agua_inexistente,
                 in_energia_rede_publica,
                 in_energia_inexistente,
                 in_esgoto_rede_publica,
                 in_esgoto_inexistente
          ) %>% 
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
    
            
            # --- Infra --- #
            
            water_non = as.numeric(in_agua_inexistente), #Non-access to Water
            water_dummy = as.numeric(in_agua_rede_publica), #Public Water system
            
            sewage_non = as.numeric(in_esgoto_inexistente), #Non-access to sewage
            sewage_dummy = as.numeric(in_esgoto_rede_publica), #Public sewage system
            
            energy_non = as.numeric(in_energia_inexistente), #Non-access to eletric energy
            energy_dummy = as.numeric(in_energia_rede_publica), #Public energy
            
            affil_mun  = ifelse(tp_convenio_poder_publico %in% c(1,3), 1, 0), #2015 onwards changed the name
            affiliated = as.numeric(in_conveniada_pp) #No information on affiliation
            
            
          ) %>% 
          left_join(docentes_school,
                    by = c("co_municipio", "co_entidade")) %>% 
          rename(
            uf = co_uf,
            codmun = co_municipio,
            dep_adm = tp_dependencia,
            ano = nu_ano_censo,
            school = co_entidade
          ) %>% 
          select( c(uf, codmun, ano, dep_adm, school, classroom:t_fund_educ) ) #Final datafilter'
        
      } else if(i %in% c(2019:2020)){
        # -------------------------------------------------------------------- #
        ### 1.6.3 2019 - 2020 ----
        # -------------------------------------------------------------------- #
        
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
        # -------------------------------------------------------------------- #
        ## 1.7 2021 ----
        # -------------------------------------------------------------------- #
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
    
    rm(docentes_school)
    }
  

  #Binding itno a single dataframe
  if(j == 1){
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
  
  
  # Total Loop time
  if (j == 21) {
    
    fim <- Sys.time()
    
    
    delta <- difftime(fim, main_time, units = "secs")
    mins <- floor(as.numeric(delta) / 60)
    secs <- round(as.numeric(delta) %% 60)
    
    message("------------------------------------------------------------------")
    message("Total Loop time: ",mins," mins and ", secs, " s")
    message("------------------------------------------------------------------")
    
    rm(main_time)
    
  }
  
  rm(temp, delta, ini, fim, temp, mins, secs, i, j)
}

rm(path_list)

# ---------------------------------------------------------------------------- #
# 2. Saving data ----
# ---------------------------------------------------------------------------- #

saveRDS(data, "Z:/Tuffy/Paper - Educ/Dados/intermediate/censo_escolar_base.rds")

rm(data)

# ---------------------------------------------------------------------------- #
# 3. Extracting total enrollments per school ----
# ---------------------------------------------------------------------------- #

path_list <- c("Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar1998/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar1999/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2000/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2001/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2002/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2003/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2004/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2005/DADOS/DADOS_CENSOESC.dta",
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

for (i in c(1998:2018)) {
  
  
  gc()
  message("Year: ",i)
  
  ini <- Sys.time()
  
  j <- i - 1997 #path index
  
  
  #Total running time:
  if (j == 1){
    main_time <- Sys.time()
  }
  
  
  if (i < 2005) {
  ## 3.1 1998 - 2004 ----
    
    ### 3.1.1 1998 - 2002 ----
    if (i < 2003) {
        ##### 3.1.1.1 1998 ----
        if ( i == 1998 ) {
          temp <- read_dta(path_list[j])
          
          temp <- temp %>% 
            select(c(1:24,
                     
                     # --- Enrollments --- #
                     DEF11C:DEF11F, NEF11C:NEF11F,      # EF iniciais (8 anos)
                     DEF11G:DEF11J, NEF11G:NEF11J,      # EF finais (8 anos)
                     VEE1431:VEE1435,                   # Alunos de educação especial do EF por ano de nascimento
                     
                     
                     DEM118, DEM119, DEM11A, DEM11B, DEM11C, #Highschool
                     NEM118, NEM119, NEM11A, NEM11B, NEM11C,
                     DPE119, NPE119,                         #(N) - Daycare
                     DPE11D, NPE11D,                         #(D) - Preschool
                     VES731:VES756,                         #EJA
                     
                     # --- Teachers --- #
                     
                     # --- Infra --- #
            )) %>%
            filter(CODFUNC == "Ativo") %>%  # Removes deactivated schools
            mutate(

              
              reg_in = rowSums(across(c(DEF11C:DEF11F, NEF11C:NEF11F)), na.rm = TRUE), # Contando EF de 8 e 9 anos!
              reg_fin = rowSums(across(c(DEF11G:DEF11J, NEF11G:NEF11J)), na.rm = TRUE),
              
              ef_tot = reg_in + reg_fin,
              em_tot = rowSums(across(c(DEM118:NEM11C)), na.rm = TRUE),
              ed_inf_tot = rowSums(across(c(DPE119:NPE11D)), na.rm = TRUE),
              eja_tot = rowSums(across(c(VES731:VES756)), na.rm = TRUE),
              day_tot = rowSums(across(c(NPE119,NPE11D)), na.rm = TRUE),
              pre_tot = rowSums(across(c(DPE119,DPE11D)), na.rm = TRUE),
              esp1 = rowSums(across(c(VEE1431:VEE1435)), na.rm = TRUE),
              
              esp_tot = pmax(esp1), #extracts maximum value between both groups
              
              reg_in_9 = 0, #in 9 anos
              reg_in_8 = rowSums(across(c(DEF11C:DEF11F, NEF11C:NEF11F)), na.rm = TRUE),# in 8 anos
              
              reg_fin_9 = 0, #fin 9 anos
              reg_fin_8 = rowSums(across(c(DEF11G:DEF11J, NEF11G:NEF11J)), na.rm = TRUE)# in 8 anos
              
            ) %>%
            select(c(1:9,DEP,53:65, ef_tot:reg_fin_8)) 
            
        } else if (i == 1999) {
          ### 3.1.1.2 1999 ----
          
          temp <- read_dta(path_list[j])
          
          temp <- temp %>% 
            select(c(1:24,
                     
                     #For the Enrollments
                     DEF11C:DEF11F, NEF11C:NEF11F,      # EF iniciais (8 anos)
                     DEF11G:DEF11J, NEF11G:NEF11J,      # EF finais (8 anos)
                     VEE1431:VEE1435,                   # Alunos de educação especial do EF por ano de nascimento
                     
                     
                     DEM118, DEM119, DEM11A, DEM11B, DEM11C, #Highschool
                     NEM118, NEM119, NEM11A, NEM11B, NEM11C,
                     DPE119, NPE119,                         #          (N) - Daycare
                     DPE11D, NPE11D,                         #          (D) - Preschool
                     VES761:VES7C6                         #EJA
            )) %>%
            filter(CODFUNC == "Ativo") %>%  # Removes deactivated schools
            mutate(
              
              
              reg_in = rowSums(across(c(DEF11C:DEF11F, NEF11C:NEF11F)), na.rm = TRUE), # Contando EF de 8 e 9 anos!
              reg_fin = rowSums(across(c(DEF11G:DEF11J, NEF11G:NEF11J)), na.rm = TRUE),
              
              ef_tot = reg_in + reg_fin,
              em_tot = rowSums(across(c(DEM118:NEM11C)), na.rm = TRUE),
              ed_inf_tot = rowSums(across(c(DPE119:NPE11D)), na.rm = TRUE),
              eja_tot = rowSums(across(c(VES761:VES7C6)), na.rm = TRUE),
              day_tot = rowSums(across(c(NPE119,NPE11D)), na.rm = TRUE),
              pre_tot = rowSums(across(c(DPE119,DPE11D)), na.rm = TRUE),
              esp1 = rowSums(across(c(VEE1431:VEE1435)), na.rm = TRUE),
              
              esp_tot = pmax(esp1), #extracts maximum value between both groups
              
              reg_in_9 = 0, #in 9 anos
              reg_in_8 = rowSums(across(c(DEF11C:DEF11F, NEF11C:NEF11F)), na.rm = TRUE),# in 8 anos
              
              reg_fin_9 = 0, #fin 9 anos
              reg_fin_8 = rowSums(across(c(DEF11G:DEF11J, NEF11G:NEF11J)), na.rm = TRUE)# in 8 anos
              
            ) %>%
            select(c(1:9,DEP,53:65, ef_tot:reg_fin_8))
          
        
      } else { 
          #### 3.1.1.2 2000 - 2002 ----
              
          
          temp <- read_dta(path_list[j]) #%>% 
          
          temp <- temp %>% select(c(1:24,
                   
                   #For the Enrollments
                   DEF11C:DEF11F, NEF11C:NEF11F,      # EF iniciais (8 anos)
                   DEF11G:DEF11J, NEF11G:NEF11J,      # EF finais (8 anos)
                   VEE1431:VEE1437,                   # Alunos de educação especial do EF por ano de nascimento
                   
                   
                   DEM118, DEM119, DEM11A, DEM11B, DEM11C, #Highschool
                   NEM118, NEM119, NEM11A, NEM11B, NEM11C,
                   DPE119, NPE119,                         #          (N) - Daycare
                   DPE11D, NPE11D,                         #          (D) - Preschool
                   VES761:VES813                        #EJA
          )) %>%
            filter(CODFUNC == "Ativo") %>%  # Removes deactivated schools
            mutate(
             
              
              reg_in = rowSums(across(c(DEF11C:DEF11F, NEF11C:NEF11F)), na.rm = TRUE), # Contando EF de 8 e 9 anos!
              reg_fin = rowSums(across(c(DEF11G:DEF11J, NEF11G:NEF11J)), na.rm = TRUE),
              
              ef_tot = reg_in + reg_fin,
              em_tot = rowSums(across(c(DEM118:NEM11C)), na.rm = TRUE),
              ed_inf_tot = rowSums(across(c(DPE119:NPE11D)), na.rm = TRUE),
              eja_tot = rowSums(across(c(VES761:VES813)), na.rm = TRUE),
              day_tot = rowSums(across(c(NPE119,NPE11D)), na.rm = TRUE),
              pre_tot = rowSums(across(c(DPE119,DPE11D)), na.rm = TRUE),
              esp1 = rowSums(across(c(VEE1431:VEE1437)), na.rm = TRUE),
              
              esp_tot = pmax(esp1), #extracts maximum value between both groups
              
              reg_in_9 = 0, #in 9 anos
              reg_in_8 = rowSums(across(c(DEF11C:DEF11F, NEF11C:NEF11F)), na.rm = TRUE),# in 8 anos
              
              reg_fin_9 = 0, #fin 9 anos
              reg_fin_8 = rowSums(across(c(DEF11G:DEF11J, NEF11G:NEF11J)), na.rm = TRUE)# in 8 anos
              
            ) %>%
            select(c(1:9,DEP, 53:65, ef_tot:reg_fin_8)) 
          
            }
            
      # Final adjusted base
      
            temp <- temp %>%
              rename(
                ano = ANO,
                school = MASCARA,
                codmunic = CODMUNIC,
                dep_adm = DEP
              ) %>%
              select(ano, school, codmunic, dep_adm, ef_tot:pre_tot, esp_tot, reg_in_9:reg_fin_8) %>%
              mutate(
                codmun = str_c(str_sub(codmunic, 1,2),
                               str_sub(codmunic, 8,12)),
                
                uf = as.numeric(codmun) %/% 100000
              ) %>% 
              select(-codmunic) 
            

            # ---- Comparable Area (1998 - 1999) ---- #
            if (i < 2000){
              
              #Openning comparable area
              
              amcs <- read_dta("Z:/Tuffy/Paper - Brasil/amcs.dta")
              
              
              rm(amcs)
            }
      
      
      } else if (i == 2003) {
        #### 3.1.2 2003 ----
        
      temp <- read_dta(path_list[j]) 
      
      temp <- temp %>% 
        select(c(1:24,
                 
                 #For the Enrollments
                 DEF11C:DEF11F, NEF11C:NEF11F,      # EF iniciais (8 anos)
                 VEF1711:VEF1715,                   # EF iniciais (9 anos)
                 DEF11G:DEF11J, NEF11G:NEF11J,      # EF finais (8 anos)
                 VEF1716:VEF1719,                   # EF finais (9 anos)
                 VEE1431:VEE1437,                   # Alunos de educação especial do EF por ano de nascimento
                 
                 
                 DEM118, DEM119, DEM11A, DEM11B, DEM11C, #Highschool
                 NEM118, NEM119, NEM11A, NEM11B, NEM11C,
                 DPE119, NPE119,                         #          (N) - Daycare
                 DPE11D, NPE11D,                         #          (D) - Preschool
                 DES1017:NES101A                         #EJA
        )) %>%
        filter(CODFUNC == "Ativo") %>%  # Removes deactivated schools
        mutate(
          reg_in = rowSums(across(c(DEF11C:VEF1715)), na.rm = TRUE), # Contando EF de 8 e 9 anos!
          reg_fin = rowSums(across(c(DEF11G:VEF1719)), na.rm = TRUE),
          
          ef_tot = reg_in + reg_fin,
          em_tot = rowSums(across(c(DEM118:NEM11C)), na.rm = TRUE),
          ed_inf_tot = rowSums(across(c(DPE119:NPE11D)), na.rm = TRUE),
          eja_tot = rowSums(across(c(DES1017:NES101A)), na.rm = TRUE),
          day_tot = rowSums(across(c(NPE119,NPE11D)), na.rm = TRUE),
          pre_tot = rowSums(across(c(DPE119,DPE11D)), na.rm = TRUE),
          esp1 = rowSums(across(c(VEE1431:VEE1437)), na.rm = TRUE),
          
          esp_tot = pmax(esp1), #extracts maximum value between both groups
          
          reg_in_9 = rowSums(across(c(VEF1711:VEF1715)), na.rm = TRUE), #in 9 anos
          reg_in_8 = rowSums(across(c(DEF11C:DEF11F, NEF11C:NEF11F)), na.rm = TRUE),# in 8 anos
          
          reg_fin_9 = rowSums(across(c(VEF1716:VEF1719)), na.rm = TRUE), #fin 9 anos
          reg_fin_8 = rowSums(across(c(DEF11G:DEF11J, NEF11G:NEF11J)), na.rm = TRUE)# in 8 anos
          
        ) %>%
        select(c(1:9,DEP,53:65, ef_tot:reg_fin_8)) %>%
        rename(
          ano = ANO,
          school = MASCARA,
          codmunic = CODMUNIC,
          dep_adm = DEP
        ) %>%
        select(ano, school, codmunic, dep_adm, ef_tot:pre_tot, esp_tot, reg_in_9:reg_fin_8) %>%
        mutate(
          codmun = str_c(str_sub(codmunic, 1,2),
                         str_sub(codmunic, 8,12)),
          
          uf = as.numeric(codmun) %/% 100000
        ) %>% 
        select(-codmunic) 
      
    } else { 
      ### 3.1.3 2004----
      
      temp <- read_dta(path_list[j]) 
      
      #%>% 
        temp <- temp %>% 
          select(c(1:24,
                 
                 #For the Enrollments
                 DEF11C:DEF11F, NEF11C:NEF11F,      # EF iniciais (8 anos)
                 DE9F11C:DE9F11G, NE9F11C:NE9F11G,  # EF iniciais (9 anos)
                 DEF11G:DEF11J, NEF11G:NEF11J,      # EF finais (8 anos)
                 DE9F11H:DE9F11N, NE9F11H:NE9F11N,  # EF finais (9 anos)
                 VEE1431:VEE1437,                   # Alunos de educação especial do EF por ano de nascimento
                 
                 
                 DEM118, DEM119, DEM11A, DEM11B, DEM11C, #Highschool
                 NEM118, NEM119, NEM11A, NEM11B, NEM11C,
                 DPE119, NPE119,                         #          (N) - Daycare
                 DPE11D, NPE11D,                         #          (D) - Preschool
                 VES762:VES7C4                         #EJA
        )) %>%
        filter(CODFUNC == "Ativo") %>%  # Removes deactivated schools
        mutate(
          
          reg_in = rowSums(across(c(DEF11C:NE9F11G)), na.rm = TRUE), # Contando EF de 8 e 9 anos!
          reg_fin = rowSums(across(c(DEF11G:NE9F11N)), na.rm = TRUE),
          
          ef_tot = reg_in + reg_fin,
          em_tot = rowSums(across(c(DEM118:NEM11C)), na.rm = TRUE),
          ed_inf_tot = rowSums(across(c(DPE119:NPE11D)), na.rm = TRUE),
          eja_tot = rowSums(across(c(VES762:VES7C4)), na.rm = TRUE),
          day_tot = rowSums(across(c(NPE119,NPE11D)), na.rm = TRUE),
          pre_tot = rowSums(across(c(DPE119,DPE11D)), na.rm = TRUE),
          esp1 = rowSums(across(c(VEE1431:VEE1437)), na.rm = TRUE),
  
          esp_tot = pmax(esp1), #extracts maximum value between both groups
          
          reg_in_9 = rowSums(across(c(DE9F11C:DE9F11G, NE9F11C:NE9F11G)), na.rm = TRUE), #in 9 anos
          reg_in_8 = rowSums(across(c(DEF11C:DEF11F, NEF11C:NEF11F)), na.rm = TRUE),# in 8 anos
          
          reg_fin_9 = rowSums(across(c(DE9F11H:DE9F11N, NE9F11H:NE9F11N)), na.rm = TRUE), #fin 9 anos
          reg_fin_8 = rowSums(across(c(DEF11G:DEF11J, NEF11G:NEF11J)), na.rm = TRUE)# in 8 anos
          
        ) %>%
        select(c(1:9,DEP,53:65, ef_tot:reg_fin_8)) %>%
        rename(
          ano = ANO,
          school = MASCARA,
          codmunic = CODMUNIC,
          dep_adm = DEP
        ) %>%
        select(ano, school, codmunic, dep_adm, ef_tot:pre_tot, esp_tot, reg_in_9:reg_fin_8) %>%
        mutate(
          codmun = str_c(str_sub(codmunic, 1,2),
                         str_sub(codmunic, 8,12)),
          
          uf = as.numeric(codmun) %/% 100000
          ) %>% 
          select(-codmunic)
      
      } 
      
      }  else if (i %in% c(2005,2006)) {
    ## 3.2 2005 - 2006 ----
    
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
               VEE1431:VEE1437,                   # Alunos de educação especial do EF por ano de nascimento
               
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
      )) %>%
      filter(CODFUNC == "Ativo") %>%  # Removes deactivated schools
      mutate(
        
        codmun = str_c(str_sub(CODMUNIC, 1,2),
                         str_sub(CODMUNIC, 8,12)),
        
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
        em_tot = rowSums(across(c(DEM118:NEM11C)), na.rm = TRUE),
        ed_inf_tot = rowSums(across(c(DPE119:NPE11D)), na.rm = TRUE),
        eja_tot = rowSums(across(c(DES101F:NES101A)), na.rm = TRUE),
        day_tot = rowSums(across(c(NPE119,NPE11D)), na.rm = TRUE),
        pre_tot = rowSums(across(c(DPE119,DPE11D)), na.rm = TRUE),
        esp1 = rowSums(across(c(VEE1431:VEE1437)), na.rm = TRUE),
        
        esp_iniciais = rowSums(across(c(VEE1619:VEE1994)), na.rm = TRUE),
        
        
        esp_finais = rowSums(across(c(VEE1615:VEE1998)), na.rm = TRUE),
        
        
        esp_soma = esp_iniciais + esp_finais,
        esp_tot = pmax(esp_soma, esp1), #extracts maximum value between both groups
        
        reg_in_9 = rowSums(across(c(DE9F11C:DE9F11G, NE9F11C:NE9F11G)), na.rm = TRUE), #in 9 anos
        reg_in_8 = rowSums(across(c(DEF11C:DEF11F, NEF11C:NEF11F)), na.rm = TRUE),# in 8 anos
        
        reg_fin_9 = rowSums(across(c(DE9F11H:DE9F11N, NE9F11H:NE9F11N)), na.rm = TRUE), #fin 9 anos
        reg_fin_8 = rowSums(across(c(DEF11G:DEF11J, NEF11G:NEF11J)), na.rm = TRUE)# in 8 anos
        
      ) %>%
      select(c(1:9, codmun, DEP, 53:65, ef_tot:reg_fin_8)) %>%
      rename(
        ano = ANO,
        school = MASCARA,
        dep_adm = DEP
      ) %>%
      select(ano, school, codmun, dep_adm, ef_tot:pre_tot, esp_tot, reg_in_9:reg_fin_8) %>%
      mutate( uf = as.numeric(codmun) %/% 100000) 
    
    
  } else {
    ##3.3 2007 - 2018 ----  
    
    
    
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
        ) 
      
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
        pre = ifelse(tp_etapa_ensino == 2, 1, 0),            #Preschool
        
        reg_in_8 = ifelse(tp_etapa_ensino %in% c(4:7), 1, 0), #Initial 8y
        reg_in_9 = ifelse(tp_etapa_ensino %in% c(14:18), 1, 0), #Initial 9y
        
        reg_fin_8 = ifelse(tp_etapa_ensino %in% c(8:11), 1, 0), #End 8y
        reg_fin_9 = ifelse(tp_etapa_ensino %in% c(19,20,21,41), 1, 0) #End 9y
        
      ) %>% 
      group_by(co_uf, co_municipio, co_entidade, nu_ano_censo, tp_dependencia) %>% 
      summarise(
        ef_tot = sum(ef, na.rm = T),
        esp_tot = sum(as.numeric(in_especial_exclusiva), 1, 0),
        em_tot = sum(em, na.rm = T),
        ed_inf_tot = sum(inf, na.rm = T),
        eja_tot = sum(eja, na.rm = T),
        day_tot = sum(day, na.rm = T),
        pre_tot = sum(pre, na.rm = T),
        
        reg_in_9 = sum(reg_in_9, na.rm = T),
        reg_in_8 = sum(reg_in_8, na.rm = T),
        reg_fin_8 = sum(reg_fin_8, na.rm = T),
        reg_fin_9 = sum(reg_fin_9, na.rm = T),
        

        
        .groups = "drop"
      ) %>% 
      rename(
        dep_adm = tp_dependencia,
        ano = nu_ano_censo,
        school = co_entidade, 
        codmun = co_municipio,
        uf = co_uf
      ) %>% 
      mutate(codmun = as.numeric(codmun))
    }
  
  
  # ------------------------------------------------------------ #
  #Binding itno a single dataframe
  
  if(j == 1){
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
  
  
  # Total Loop time
  if (j == 21) {
    
    fim <- Sys.time()
    
    
    delta <- difftime(fim, main_time, units = "secs")
    mins <- floor(as.numeric(delta) / 60)
    secs <- round(as.numeric(delta) %% 60)
    
    message("------------------------------------------------------------------")
    message("Total Loop time: ",mins," mins and ", secs, " s")
    message("------------------------------------------------------------------")
    
    rm(main_time)
    
  }
  
  rm(temp, delta, ini, fim, temp, mins, secs, i, j)
  
  gc()
  
  }

saveRDS(data,"Z:/Tuffy/Paper - Educ/Dados/intermediate/matriculas_por_escola.rds")

rm(data)


# ---------------------------------------------------------------------------- #
#4. Merging the two datasets ----
# ---------------------------------------------------------------------------- #

data <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/matriculas_por_escola.rds")

df_school <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/censo_escolar_base.rds")

df_combined <- df_school %>% 
  left_join(data %>% select(-dep_adm),
            by = c("uf" = "uf", "ano" = "ano", "codmun" = "codmun", "school" = "school")) #%>% 
  

df_combined <- df_combined %>% 
  mutate(
    enroll = rowSums(
      across(c(ef_tot, em_tot, ed_inf_tot, eja_tot)),
      na.rm = TRUE) #We dont count the discarded variable of esp
  ) %>% 
  mutate(
    dep_adm = case_when(
      
      dep_adm == 1 ~ "Federal",    #Federal
      
      dep_adm == 2 ~ "Estadual",   #State
      
      dep_adm == 3 ~ "Municipal",  #Municipality
      
      dep_adm == 4 ~ "Particular", #Private
      
      TRUE ~ dep_adm
      
    )
  )

saveRDS(df_combined, "Z:/Tuffy/Paper - Educ/Dados/intermediate/censo_escolar_base_v2.rds")
rm(df_school, data)


rm(list = ls())
gc()

# ---------------------------------------------------------------------------- #
# 5. Population and age ----
# ---------------------------------------------------------------------------- #
#' Now we will extratc the municipal age data on population to create a variable
#' associated with the enrollment of students outside of the school. This enables
#' the investigation of the movement of children joining the public school system.


base_path <- "Z:/Tuffy/Paper - Educ/Dados/datasus"

# opening the database

popbr <- map_dfr(sprintf("%02d", 0:18), function(yy) {
  file <- file.path(base_path, paste0("POPSBR", yy), paste0("pop", yy, ".dbf"))
  df <- foreign::read.dbf(file, as.is = TRUE)
  df$arquivo_ano <- as.integer(paste0("20", yy))
  df
})


#First filter variable
popbr <- popbr %>% 
  rename_all(tolower) %>%
  mutate(
    age_inf  = ifelse(as.numeric(idade) <= 5, 1*pop, 0),
    age_fund = ifelse(as.numeric(idade) %in% c(6:14), 1*pop, 0)
  )

#Municipal dataframe

pop_mun <- popbr %>% 
  group_by(cod_mun, ano) %>% 
  summarise(
    total_population = sum(pop, na.rm = T),
    age_inf          = sum(age_inf, na.rm = T),
    age_fund         = sum(age_fund, na.rm = T),
    
    .groups = "drop"
  )

saveRDS(pop_mun, "Z:/Tuffy/Paper - Educ/Dados/intermediate/pop_age.rds")

# ---------------------------------------------------------------------------- #
## 5.1 Enrollment proportion ----
# ---------------------------------------------------------------------------- #

pop_mun <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/pop_age.rds")

df_enroll <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/censo_escolar_base_v2.rds")

# ---------------------------------------------------------------------------- #
### 5.1.1. Variables ----
# ---------------------------------------------------------------------------- #

# Municipal enrollment
df_lvl_mun <- df_enroll %>% 
  ungroup() %>% 
  mutate(is_mun = dep_adm == "Municipal") %>%
  group_by(codmun, ano) %>% 
  summarise(
    
    # -------- Enrollment totals -------- #
    inf_enroll = sum(
      rowSums(across(c(pre_tot, day_tot)), na.rm = TRUE),
      na.rm = TRUE
    ),
    
    fun_enroll = sum(
      rowSums(across(c(reg_in_9, reg_in_8, reg_fin_9, reg_fin_8)), na.rm = TRUE),
      na.rm = TRUE
    ),
    
    total_enroll = sum(coalesce(enroll, 0), na.rm = TRUE),
    
    total_pub_enroll = sum(
      if_else(is_mun, coalesce(enroll, 0), 0),
      na.rm = TRUE
    ),
    
    inf_pub_enroll = sum(
      if_else(
        is_mun,
        rowSums(across(c(pre_tot, day_tot)), na.rm = TRUE),
        0,
        missing = 0
      ),
      na.rm = TRUE
    ),
    
    cre_enroll = sum(coalesce(day_tot, 0), na.rm = TRUE),
    pre_enroll = sum(coalesce(pre_tot, 0), na.rm = TRUE),
    
    cre_pub_enroll = sum(
      if_else(is_mun, coalesce(day_tot, 0), 0, missing = 0),
      na.rm = TRUE
    ),
    
    pre_pub_enroll = sum(
      if_else(is_mun, coalesce(pre_tot, 0), 0, missing = 0),
      na.rm = TRUE
    ),
    
    fun_pub_enroll = sum(
      if_else(
        is_mun,
        rowSums(across(c(reg_in_9, reg_in_8, reg_fin_9, reg_fin_8)), na.rm = TRUE),
        0,
        missing = 0
      ),
      na.rm = TRUE
    ),
    
    # -------- Number of schools -------- #
    n_pub_creche = n_distinct(school[coalesce(day_tot, 0) > 0 & is_mun]),
    
    n_pub_presco = n_distinct(school[coalesce(pre_tot, 0) > 0 & is_mun]),
    
    n_pub_fundam = n_distinct(
      school[
        (coalesce(reg_in_8, 0) > 0 | coalesce(reg_fin_8, 0) > 0 |
           coalesce(reg_in_9, 0) > 0 | coalesce(reg_fin_9, 0) > 0) & is_mun
      ]
    ),
    
    # -------- Municipal infra / resources -------- #
    pub_classroom = sum(if_else(is_mun, coalesce(enroll, 0) * 
                                  coalesce(classroom, 0), 0), na.rm = TRUE),
    
    pub_teachroom = sum(if_else(is_mun, coalesce(enroll, 0) * 
                                  coalesce(teach_room, 0), 0), na.rm = TRUE),
    
    pub_labs      = sum(if_else(is_mun, coalesce(enroll, 0) * 
                                  coalesce(lab_dummy, 0), 0), na.rm = TRUE),
    
    pub_library   = sum(if_else(is_mun, coalesce(enroll, 0) * 
                                  coalesce(lib_dummy, 0), 0), na.rm = TRUE),
    
    pub_playarea  = sum(if_else(is_mun, coalesce(enroll, 0) * 
                                  coalesce(play_area, 0), 0), na.rm = TRUE),
    
    pub_lunch     = sum(if_else(is_mun, coalesce(enroll, 0) * 
                                  coalesce(lunch, 0), 0), na.rm = TRUE),
    
    
    pub_no_water  = sum(if_else(is_mun, coalesce(enroll, 0) * 
                                  coalesce(water_non, 0), 0), na.rm = TRUE),
    
    pub_water_dum = sum(if_else(is_mun, coalesce(enroll, 0) * 
                                  coalesce(water_dummy, 0), 0), na.rm = TRUE),
    
    
    pub_no_sewage  = sum(if_else(is_mun, coalesce(enroll, 0) * 
                                   coalesce(sewage_non, 0), 0), na.rm = TRUE),
    
    pub_sewage_dum = sum(if_else(is_mun, coalesce(enroll, 0) * 
                                   coalesce(sewage_dummy, 0), 0), na.rm = TRUE),
    
    
    pub_no_energy  = sum(if_else(is_mun, coalesce(enroll, 0) * 
                                   coalesce(energy_non, 0), 0), na.rm = TRUE),
    
    pub_energy_dum = sum(if_else(is_mun, coalesce(enroll, 0) * 
                                   coalesce(energy_dummy, 0), 0), na.rm = TRUE),
    
    
    pub_employee = sum(if_else(is_mun, coalesce(enroll, 0) *
                                 coalesce(employee, 0), 0), na.rm = TRUE),
    
    # -------- Teacher totals / education -------- #
    pub_t_fun    = sum(if_else(is_mun, coalesce(fun_pub_enroll, 0) *
                                 coalesce(t_fund, 0), 0), na.rm = TRUE),
    
    pub_t_pre    = sum(if_else(is_mun, coalesce(pre_pub_enroll, 0) *
                                  coalesce(t_pree, 0), 0), na.rm = TRUE),
    
    pub_t_cre    = sum(if_else(is_mun, coalesce(cre_pub_enroll, 0) * 
                                  coalesce(t_crec, 0), 0), na.rm = TRUE),
    
    
    pub_t_fun_edu = sum(if_else(is_mun, coalesce(fun_pub_enroll, 0) * 
                                  coalesce(t_fund_educ, 0), 0), na.rm = TRUE),
    
    pub_t_pre_edu = sum(if_else(is_mun, coalesce(pre_pub_enroll, 0) * 
                                  coalesce(t_pree_educ, 0), 0), na.rm = TRUE),
    
    pub_t_cre_edu = sum(if_else(is_mun, coalesce(cre_pub_enroll, 0) * 
                                  coalesce(t_crec_educ, 0), 0), na.rm = TRUE),
    
    # --- Municipal level variables --- #
    
    n_t_fun = sum(
      if_else(is_mun, coalesce(t_fund, 0), 0),
      na.rm = TRUE
    ),
    
    n_t_pre = sum(
      if_else(is_mun, coalesce(t_pree, 0), 0),
      na.rm = TRUE
    ),
    
    n_t_cre = sum(
      if_else(is_mun, coalesce(t_crec, 0), 0),
      na.rm = TRUE
    ),
    
    n_t_cre_edu = sum(
      if_else(is_mun, coalesce(t_crec_educ, 0), 0),
      na.rm = TRUE
    ),
    
    n_t_pre_edu = sum(
      if_else(is_mun, coalesce(t_pree_educ, 0), 0),
      na.rm = TRUE
    ),
    
    n_t_fun_edu = sum(
      if_else(is_mun, coalesce(t_fund_educ, 0), 0),
      na.rm = TRUE
    ),
    
    n_employee = sum(
      if_else(is_mun, coalesce(employee, 0), 0),
      na.rm = TRUE
    ),
    
    
    # -------- Total exposure -------- #
    
    tot_classroom = sum(coalesce(enroll, 0) * coalesce(classroom, 0), na.rm = TRUE),
    
    tot_teachroom = sum(coalesce(enroll, 0) * coalesce(teach_room, 0), na.rm = TRUE),
    
    tot_labs = sum(coalesce(enroll, 0) * coalesce(lab_dummy, 0), na.rm = TRUE),
    
    tot_library = sum(coalesce(enroll, 0) * coalesce(lib_dummy, 0), na.rm = TRUE),
    
    tot_playarea = sum(coalesce(enroll, 0) * coalesce(play_area, 0), na.rm = TRUE),
    
    tot_lunch = sum(coalesce(enroll, 0) * coalesce(lunch, 0), na.rm = TRUE),
    
    tot_no_water = sum(coalesce(enroll, 0) * coalesce(water_non, 0), na.rm = TRUE),
    
    tot_water_dum = sum(coalesce(enroll, 0) * coalesce(water_dummy, 0), na.rm = TRUE),
    
    tot_no_sewage = sum(coalesce(enroll, 0) * coalesce(sewage_non, 0), na.rm = TRUE),
    
    tot_sewage_dum = sum(coalesce(enroll, 0) * coalesce(sewage_dummy, 0), na.rm = TRUE),
    
    tot_no_energy = sum(coalesce(enroll, 0) * coalesce(energy_non, 0), na.rm = TRUE),
    
    tot_energy_dum = sum(coalesce(enroll, 0) * coalesce(energy_dummy, 0), na.rm = TRUE),
    
    tot_employee = sum(coalesce(enroll, 0) * coalesce(employee, 0), na.rm = TRUE),
    
    tot_t_fund = sum(coalesce(fun_enroll, 0) * coalesce(t_fund, 0), na.rm = TRUE),
    
    tot_t_pree = sum(coalesce(pre_enroll, 0) * coalesce(t_pree, 0), na.rm = TRUE),
    
    tot_t_crec = sum(coalesce(cre_enroll, 0) * coalesce(t_crec, 0), na.rm = TRUE),
    
    tot_t_fun_edu = sum(coalesce(fun_enroll, 0) * coalesce(t_fund_educ, 0), na.rm = TRUE),
    
    tot_t_pre_edu = sum(coalesce(pre_enroll, 0) * coalesce(t_pree_educ, 0), na.rm = TRUE),
    
    tot_t_cre_edu = sum(coalesce(cre_enroll, 0) * coalesce(t_crec_educ, 0), na.rm = TRUE),
    
    .groups = "drop"
  )


# Combining both databases
# ---------------------------------------------------------------------------- #
### 5.1.2 Age data ----
# ---------------------------------------------------------------------------- #

df_lvl_mun <- df_lvl_mun %>% 
  left_join(pop_mun %>% mutate(ano = as.numeric(ano)), by = c("codmun" = "cod_mun", "ano"))


# ---------------------------------------------------------------------------- #
### 5.1.3 Prop. data ----
# ---------------------------------------------------------------------------- #

df_prop_mun <- df_lvl_mun %>% 
  mutate(
    
    # -------- Enrollment rates -------- #
    
    prop_inf = if_else(
      !is.na(age_inf) & age_inf > 0,
      inf_enroll / age_inf,
      NA_real_
    ),
    
    prop_fun = if_else(
      !is.na(age_fund) & age_fund > 0,
      fun_enroll / age_fund,
      NA_real_
    ),
    
    prop_pub_inf = if_else(
      !is.na(age_inf) & age_inf > 0,
      inf_pub_enroll / age_inf,
      NA_real_
    ),
    
    prop_pub_fun = if_else(
      !is.na(age_fund) & age_fund > 0,
      fun_pub_enroll / age_fund,
      NA_real_
    ),
    
    
    # -------- Public share of total exposure -------- #
    exp_classroom = if_else(!is.na(tot_classroom) & tot_classroom > 0,
                            pub_classroom / tot_classroom,
                            NA_real_),
    
    exp_teachroom = if_else(!is.na(tot_teachroom) & tot_teachroom > 0,
                            pub_teachroom / tot_teachroom,
                            NA_real_),
    
    exp_labs = if_else(!is.na(tot_labs) & tot_labs > 0,
                       pub_labs / tot_labs,
                       NA_real_),
    
    exp_library = if_else(!is.na(tot_library) & tot_library > 0,
                          pub_library / tot_library,
                          NA_real_),
    
    exp_playarea = if_else(!is.na(tot_playarea) & tot_playarea > 0,
                           pub_playarea / tot_playarea,
                           NA_real_),
    
    exp_lunch = if_else(!is.na(tot_lunch) & tot_lunch > 0,
                        pub_lunch / tot_lunch,
                        NA_real_),
    
    exp_no_water = if_else(!is.na(tot_no_water) & tot_no_water > 0,
                           pub_no_water / tot_no_water,
                           NA_real_),
    
    exp_water = if_else(!is.na(tot_water_dum) & tot_water_dum > 0,
                        pub_water_dum / tot_water_dum,
                        NA_real_),
    
    exp_no_sewage = if_else(!is.na(tot_no_sewage) & tot_no_sewage > 0,
                            pub_no_sewage / tot_no_sewage,
                            NA_real_),
    
    exp_sewage = if_else(!is.na(tot_sewage_dum) & tot_sewage_dum > 0,
                         pub_sewage_dum / tot_sewage_dum,
                         NA_real_),
    
    exp_no_energy = if_else(!is.na(tot_no_energy) & tot_no_energy > 0,
                            pub_no_energy / tot_no_energy,
                            NA_real_),
    
    exp_energy = if_else(!is.na(tot_energy_dum) & tot_energy_dum > 0,
                         pub_energy_dum / tot_energy_dum,
                         NA_real_),
    
    exp_employee = if_else(!is.na(tot_employee) & tot_employee > 0,
                           pub_employee / tot_employee,
                           NA_real_),
    
    exp_t_fun = if_else(!is.na(tot_t_fund) & tot_t_fund > 0,
                        pub_t_fun / tot_t_fund,
                        NA_real_),
    
    exp_t_pre = if_else(!is.na(tot_t_pree) & tot_t_pree > 0,
                        pub_t_pre / tot_t_pree,
                        NA_real_),
    
    exp_t_cre = if_else(!is.na(tot_t_crec) & tot_t_crec > 0,
                        pub_t_cre / tot_t_crec,
                        NA_real_),
    
    exp_t_fun_edu = if_else(!is.na(tot_t_fun_edu) & tot_t_fun_edu > 0,
                            pub_t_fun_edu / tot_t_fun_edu,
                            NA_real_),
    
    exp_t_pre_edu = if_else(!is.na(tot_t_pre_edu) & tot_t_pre_edu > 0,
                            pub_t_pre_edu / tot_t_pre_edu,
                            NA_real_),
    
    exp_t_cre_edu = if_else(!is.na(tot_t_cre_edu) & tot_t_cre_edu > 0,
                            pub_t_cre_edu / tot_t_cre_edu,
                            NA_real_)
  ) %>% 
  select(
    -c(pub_classroom:pub_t_cre_edu, #Removing the non-more needed variables
       tot_classroom:tot_t_cre_edu)
  )
  
  
# ---- Saving ---- #

saveRDS(df_prop_mun, "Z:/Tuffy/Paper - Educ/Dados/intermediate/mun_prop_data.rds")



# ---------------------------------------------------------------------------- #
## 5.2 With Affiliation -----
# ---------------------------------------------------------------------------- #
### 5.2.1 Variables -----
# ---------------------------------------------------------------------------- #

# Municipal enrollment
df_lvl_mun <- df_enroll %>% 
  ungroup() %>% 
  mutate(is_mun = dep_adm == "Municipal",
         is_afl = (affiliated == 1 & affil_mun == 1)) %>%
  group_by(codmun, ano) %>% 
  summarise(
        
        # -------- Enrollment totals -------- #
        inf_enroll = sum(
          rowSums(across(c(pre_tot, day_tot)), na.rm = TRUE),
          na.rm = TRUE
        ),
        
        fun_enroll = sum(
          rowSums(across(c(reg_in_9, reg_in_8, reg_fin_9, reg_fin_8)), na.rm = TRUE),
          na.rm = TRUE
        ),
        
        total_enroll = sum(coalesce(enroll, 0), na.rm = TRUE),
        
        total_pub_enroll = sum(
          if_else(is_mun | is_afl, coalesce(enroll, 0), 0),
          na.rm = TRUE
        ),
        
        inf_pub_enroll = sum(
          if_else(
            is_mun | is_afl,
            rowSums(across(c(pre_tot, day_tot)), na.rm = TRUE),
            0,
            missing = 0
          ),
          na.rm = TRUE
        ),
        
        cre_enroll = sum(coalesce(day_tot, 0), na.rm = TRUE),
        pre_enroll = sum(coalesce(pre_tot, 0), na.rm = TRUE),
        
        cre_pub_enroll = sum(
          if_else(is_mun | is_afl, coalesce(day_tot, 0), 0, missing = 0),
          na.rm = TRUE
        ),
        
        pre_pub_enroll = sum(
          if_else(is_mun | is_afl, coalesce(pre_tot, 0), 0, missing = 0),
          na.rm = TRUE
        ),
        
        fun_pub_enroll = sum(
          if_else(
            is_mun | is_afl,
            rowSums(across(c(reg_in_9, reg_in_8, reg_fin_9, reg_fin_8)), na.rm = TRUE),
            0,
            missing = 0
          ),
          na.rm = TRUE
        ),
        
        # -------- Number of schools -------- #
        n_pub_creche = n_distinct(school[coalesce(day_tot, 0) > 0 & is_mun | is_afl]),
        
        n_pub_presco = n_distinct(school[coalesce(pre_tot, 0) > 0 & is_mun | is_afl]),
        
        n_pub_fundam = n_distinct(
          school[
            (coalesce(reg_in_8, 0) > 0 | coalesce(reg_fin_8, 0) > 0 |
               coalesce(reg_in_9, 0) > 0 | coalesce(reg_fin_9, 0) > 0) & is_mun | is_afl
          ]
        ),
        
        # -------- Municipal infra / resources -------- #
        pub_classroom = sum(if_else(is_mun | is_afl, coalesce(enroll, 0) * 
                                      coalesce(classroom, 0), 0), na.rm = TRUE),
        
        pub_teachroom = sum(if_else(is_mun | is_afl, coalesce(enroll, 0) * 
                                      coalesce(teach_room, 0), 0), na.rm = TRUE),
        
        pub_labs      = sum(if_else(is_mun | is_afl, coalesce(enroll, 0) * 
                                      coalesce(lab_dummy, 0), 0), na.rm = TRUE),
        
        pub_library   = sum(if_else(is_mun | is_afl, coalesce(enroll, 0) * 
                                      coalesce(lib_dummy, 0), 0), na.rm = TRUE),
        
        pub_playarea  = sum(if_else(is_mun | is_afl, coalesce(enroll, 0) * 
                                      coalesce(play_area, 0), 0), na.rm = TRUE),
        
        pub_lunch     = sum(if_else(is_mun | is_afl, coalesce(enroll, 0) * 
                                      coalesce(lunch, 0), 0), na.rm = TRUE),
        
        
        pub_no_water  = sum(if_else(is_mun | is_afl, coalesce(enroll, 0) * 
                                      coalesce(water_non, 0), 0), na.rm = TRUE),
        
        pub_water_dum = sum(if_else(is_mun | is_afl, coalesce(enroll, 0) * 
                                      coalesce(water_dummy, 0), 0), na.rm = TRUE),
        
        
        pub_no_sewage  = sum(if_else(is_mun | is_afl, coalesce(enroll, 0) * 
                                       coalesce(sewage_non, 0), 0), na.rm = TRUE),
        
        pub_sewage_dum = sum(if_else(is_mun | is_afl, coalesce(enroll, 0) * 
                                       coalesce(sewage_dummy, 0), 0), na.rm = TRUE),
        
        
        pub_no_energy  = sum(if_else(is_mun | is_afl, coalesce(enroll, 0) * 
                                       coalesce(energy_non, 0), 0), na.rm = TRUE),
        
        pub_energy_dum = sum(if_else(is_mun | is_afl, coalesce(enroll, 0) * 
                                       coalesce(energy_dummy, 0), 0), na.rm = TRUE),
        
        
        pub_employee = sum(if_else(is_mun | is_afl, coalesce(enroll, 0) *
                                     coalesce(employee, 0), 0), na.rm = TRUE),
        
        # -------- Teacher totals / education -------- #
        pub_t_fun    = sum(if_else(is_mun | is_afl, coalesce(fun_pub_enroll, 0) *
                                     coalesce(t_fund, 0), 0), na.rm = TRUE),
        
        pub_t_pre    = sum(if_else(is_mun | is_afl, coalesce(pre_pub_enroll, 0) *
                                     coalesce(t_pree, 0), 0), na.rm = TRUE),
        
        pub_t_cre    = sum(if_else(is_mun | is_afl, coalesce(cre_pub_enroll, 0) * 
                                     coalesce(t_crec, 0), 0), na.rm = TRUE),
        
        
        pub_t_fun_edu = sum(if_else(is_mun | is_afl, coalesce(fun_pub_enroll, 0) * 
                                      coalesce(t_fund_educ, 0), 0), na.rm = TRUE),
        
        pub_t_pre_edu = sum(if_else(is_mun | is_afl, coalesce(pre_pub_enroll, 0) * 
                                      coalesce(t_pree_educ, 0), 0), na.rm = TRUE),
        
        pub_t_cre_edu = sum(if_else(is_mun | is_afl, coalesce(cre_pub_enroll, 0) * 
                                      coalesce(t_crec_educ, 0), 0), na.rm = TRUE),
        
        # --- Municipal level variables --- #
        
        n_t_fun = sum(
          if_else(is_mun | is_afl, coalesce(t_fund, 0), 0),
          na.rm = TRUE
        ),
        
        n_t_pre = sum(
          if_else(is_mun | is_afl, coalesce(t_pree, 0), 0),
          na.rm = TRUE
        ),
        
        n_t_cre = sum(
          if_else(is_mun | is_afl, coalesce(t_crec, 0), 0),
          na.rm = TRUE
        ),
        
        n_t_cre_edu = sum(
          if_else(is_mun | is_afl, coalesce(t_crec_educ, 0), 0),
          na.rm = TRUE
        ),
        
        n_t_pre_edu = sum(
          if_else(is_mun | is_afl, coalesce(t_pree_educ, 0), 0),
          na.rm = TRUE
        ),
        
        n_t_fun_edu = sum(
          if_else(is_mun | is_afl, coalesce(t_fund_educ, 0), 0),
          na.rm = TRUE
        ),
        
        n_employee = sum(
          if_else(is_mun | is_afl, coalesce(employee, 0), 0),
          na.rm = TRUE
        ),
        
        
        # -------- Total exposure -------- #
        
        tot_classroom = sum(coalesce(enroll, 0) * coalesce(classroom, 0), na.rm = TRUE),
        
        tot_teachroom = sum(coalesce(enroll, 0) * coalesce(teach_room, 0), na.rm = TRUE),
        
        tot_labs = sum(coalesce(enroll, 0) * coalesce(lab_dummy, 0), na.rm = TRUE),
        
        tot_library = sum(coalesce(enroll, 0) * coalesce(lib_dummy, 0), na.rm = TRUE),
        
        tot_playarea = sum(coalesce(enroll, 0) * coalesce(play_area, 0), na.rm = TRUE),
        
        tot_lunch = sum(coalesce(enroll, 0) * coalesce(lunch, 0), na.rm = TRUE),
        
        tot_no_water = sum(coalesce(enroll, 0) * coalesce(water_non, 0), na.rm = TRUE),
        
        tot_water_dum = sum(coalesce(enroll, 0) * coalesce(water_dummy, 0), na.rm = TRUE),
        
        tot_no_sewage = sum(coalesce(enroll, 0) * coalesce(sewage_non, 0), na.rm = TRUE),
        
        tot_sewage_dum = sum(coalesce(enroll, 0) * coalesce(sewage_dummy, 0), na.rm = TRUE),
        
        tot_no_energy = sum(coalesce(enroll, 0) * coalesce(energy_non, 0), na.rm = TRUE),
        
        tot_energy_dum = sum(coalesce(enroll, 0) * coalesce(energy_dummy, 0), na.rm = TRUE),
        
        tot_employee = sum(coalesce(enroll, 0) * coalesce(employee, 0), na.rm = TRUE),
        
        tot_t_fund = sum(coalesce(fun_enroll, 0) * coalesce(t_fund, 0), na.rm = TRUE),
        
        tot_t_pree = sum(coalesce(pre_enroll, 0) * coalesce(t_pree, 0), na.rm = TRUE),
        
        tot_t_crec = sum(coalesce(cre_enroll, 0) * coalesce(t_crec, 0), na.rm = TRUE),
        
        tot_t_fun_edu = sum(coalesce(fun_enroll, 0) * coalesce(t_fund_educ, 0), na.rm = TRUE),
        
        tot_t_pre_edu = sum(coalesce(pre_enroll, 0) * coalesce(t_pree_educ, 0), na.rm = TRUE),
        
        tot_t_cre_edu = sum(coalesce(cre_enroll, 0) * coalesce(t_crec_educ, 0), na.rm = TRUE),
        
        .groups = "drop"
      )
    
    
# Combining both databases
# ---------------------------------------------------------------------------- #
### 5.2.2 Age data ----
# ---------------------------------------------------------------------------- #
    
df_lvl_mun <- df_lvl_mun %>% 
  left_join(pop_mun %>% mutate(ano = as.numeric(ano)), by = c("codmun" = "cod_mun", "ano"))
    
    
# ---------------------------------------------------------------------------- #
### 5.2.3 Prop. data ----
# ---------------------------------------------------------------------------- #
    
df_prop_mun <- df_lvl_mun %>% 
  mutate(
        
        # -------- Enrollment rates -------- #
        
        prop_inf = if_else(
          !is.na(age_inf) & age_inf > 0,
          inf_enroll / age_inf,
          NA_real_
        ),
        
        prop_fun = if_else(
          !is.na(age_fund) & age_fund > 0,
          fun_enroll / age_fund,
          NA_real_
        ),
        
        prop_pub_inf = if_else(
          !is.na(age_inf) & age_inf > 0,
          inf_pub_enroll / age_inf,
          NA_real_
        ),
        
        prop_pub_fun = if_else(
          !is.na(age_fund) & age_fund > 0,
          fun_pub_enroll / age_fund,
          NA_real_
        ),
        
        
        # -------- Public share of total exposure -------- #
        exp_classroom = if_else(!is.na(tot_classroom) & tot_classroom > 0,
                                pub_classroom / tot_classroom,
                                NA_real_),
        
        exp_teachroom = if_else(!is.na(tot_teachroom) & tot_teachroom > 0,
                                pub_teachroom / tot_teachroom,
                                NA_real_),
        
        exp_labs = if_else(!is.na(tot_labs) & tot_labs > 0,
                           pub_labs / tot_labs,
                           NA_real_),
        
        exp_library = if_else(!is.na(tot_library) & tot_library > 0,
                              pub_library / tot_library,
                              NA_real_),
        
        exp_playarea = if_else(!is.na(tot_playarea) & tot_playarea > 0,
                               pub_playarea / tot_playarea,
                               NA_real_),
        
        exp_lunch = if_else(!is.na(tot_lunch) & tot_lunch > 0,
                            pub_lunch / tot_lunch,
                            NA_real_),
        
        exp_no_water = if_else(!is.na(tot_no_water) & tot_no_water > 0,
                               pub_no_water / tot_no_water,
                               NA_real_),
        
        exp_water = if_else(!is.na(tot_water_dum) & tot_water_dum > 0,
                            pub_water_dum / tot_water_dum,
                            NA_real_),
        
        exp_no_sewage = if_else(!is.na(tot_no_sewage) & tot_no_sewage > 0,
                                pub_no_sewage / tot_no_sewage,
                                NA_real_),
        
        exp_sewage = if_else(!is.na(tot_sewage_dum) & tot_sewage_dum > 0,
                             pub_sewage_dum / tot_sewage_dum,
                             NA_real_),
        
        exp_no_energy = if_else(!is.na(tot_no_energy) & tot_no_energy > 0,
                                pub_no_energy / tot_no_energy,
                                NA_real_),
        
        exp_energy = if_else(!is.na(tot_energy_dum) & tot_energy_dum > 0,
                             pub_energy_dum / tot_energy_dum,
                             NA_real_),
        
        exp_employee = if_else(!is.na(tot_employee) & tot_employee > 0,
                               pub_employee / tot_employee,
                               NA_real_),
        
        exp_t_fun = if_else(!is.na(tot_t_fund) & tot_t_fund > 0,
                            pub_t_fun / tot_t_fund,
                            NA_real_),
        
        exp_t_pre = if_else(!is.na(tot_t_pree) & tot_t_pree > 0,
                            pub_t_pre / tot_t_pree,
                            NA_real_),
        
        exp_t_cre = if_else(!is.na(tot_t_crec) & tot_t_crec > 0,
                            pub_t_cre / tot_t_crec,
                            NA_real_),
        
        exp_t_fun_edu = if_else(!is.na(tot_t_fun_edu) & tot_t_fun_edu > 0,
                                pub_t_fun_edu / tot_t_fun_edu,
                                NA_real_),
        
        exp_t_pre_edu = if_else(!is.na(tot_t_pre_edu) & tot_t_pre_edu > 0,
                                pub_t_pre_edu / tot_t_pre_edu,
                                NA_real_),
        
        exp_t_cre_edu = if_else(!is.na(tot_t_cre_edu) & tot_t_cre_edu > 0,
                                pub_t_cre_edu / tot_t_cre_edu,
                                NA_real_)
      ) %>% 
  select(
    -c(pub_classroom:pub_t_cre_edu, #Removing the non-more needed variables
       tot_classroom:tot_t_cre_edu)
      )
    
    
    # ---- Saving ---- #
    
saveRDS(df_prop_mun, "Z:/Tuffy/Paper - Educ/Dados/intermediate/mun_prop_data_fil.rds")

# ---------------------------------------------------------------------------- #
## 5.3 School type Infra ----
# ---------------------------------------------------------------------------- #

pop_mun <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/pop_age.rds")

df_enroll <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/censo_escolar_base_v2.rds")

# ---------------------------------------------------------------------------- #
### 5.3.1. Variables ----
# ---------------------------------------------------------------------------- #

# Municipal enrollment
df_lvl_mun <- df_enroll %>% 
  ungroup() %>% 
  mutate(is_mun = dep_adm == "Municipal") %>%
  group_by(codmun, ano) %>% 
  summarise(
    
    # -------- Enrollment totals -------- #
    inf_enroll = sum(
      rowSums(across(c(pre_tot, day_tot)), na.rm = TRUE),
      na.rm = TRUE
    ),
    
    fun_enroll = sum(
      rowSums(across(c(reg_in_9, reg_in_8, reg_fin_9, reg_fin_8)), na.rm = TRUE),
      na.rm = TRUE
    ),
    
    total_enroll = sum(coalesce(enroll, 0), na.rm = TRUE),
    
    total_pub_enroll = sum(
      if_else(is_mun, coalesce(enroll, 0), 0),
      na.rm = TRUE
    ),
    
    inf_pub_enroll = sum(
      if_else(
        is_mun,
        rowSums(across(c(pre_tot, day_tot)), na.rm = TRUE),
        0,
        missing = 0
      ),
      na.rm = TRUE
    ),
    
    cre_enroll = sum(coalesce(day_tot, 0), na.rm = TRUE),
    pre_enroll = sum(coalesce(pre_tot, 0), na.rm = TRUE),
    
    cre_pub_enroll = sum(
      if_else(is_mun, coalesce(day_tot, 0), 0, missing = 0),
      na.rm = TRUE
    ),
    
    pre_pub_enroll = sum(
      if_else(is_mun, coalesce(pre_tot, 0), 0, missing = 0),
      na.rm = TRUE
    ),
    
    fun_pub_enroll = sum(
      if_else(
        is_mun,
        rowSums(across(c(reg_in_9, reg_in_8, reg_fin_9, reg_fin_8)), na.rm = TRUE),
        0,
        missing = 0
      ),
      na.rm = TRUE
    ),
    
    # -------- Creche exposure -------- #
    
    cre_tot_classroom = sum(
      coalesce(day_tot, 0) * coalesce(classroom, 0),
      na.rm = TRUE
    ),
    
    cre_tot_teachroom = sum(
      coalesce(day_tot, 0) * coalesce(teach_room, 0),
      na.rm = TRUE
    ),
    
    cre_tot_labs = sum(
      coalesce(day_tot, 0) * coalesce(lab_dummy, 0),
      na.rm = TRUE
    ),
    
    cre_tot_library = sum(
      coalesce(day_tot, 0) * coalesce(lib_dummy, 0),
      na.rm = TRUE
    ),
    
    cre_tot_playarea = sum(
      coalesce(day_tot, 0) * coalesce(play_area, 0),
      na.rm = TRUE
    ),
    
    cre_tot_lunch = sum(
      coalesce(day_tot, 0) * coalesce(lunch, 0),
      na.rm = TRUE
    ),
    
    cre_tot_no_water = sum(
      coalesce(day_tot, 0) * coalesce(water_non, 0),
      na.rm = TRUE
    ),
    
    cre_tot_water_dum = sum(
      coalesce(day_tot, 0) * coalesce(water_dummy, 0),
      na.rm = TRUE
    ),
    
    cre_tot_no_sewage = sum(
      coalesce(day_tot, 0) * coalesce(sewage_non, 0),
      na.rm = TRUE
    ),
    
    cre_tot_sewage_dum = sum(
      coalesce(day_tot, 0) * coalesce(sewage_dummy, 0),
      na.rm = TRUE
    ),
    
    cre_tot_no_energy = sum(
      coalesce(day_tot, 0) * coalesce(energy_non, 0),
      na.rm = TRUE
    ),
    
    cre_tot_energy_dum = sum(
      coalesce(day_tot, 0) * coalesce(energy_dummy, 0),
      na.rm = TRUE
    ),
    
    cre_tot_employee = sum(
      coalesce(day_tot, 0) * coalesce(employee, 0),
      na.rm = TRUE
    ),
    
    cre_pub_classroom = sum(
      if_else(is_mun, coalesce(day_tot, 0) * coalesce(classroom, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_teachroom = sum(
      if_else(is_mun, coalesce(day_tot, 0) * coalesce(teach_room, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_labs = sum(
      if_else(is_mun, coalesce(day_tot, 0) * coalesce(lab_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_library = sum(
      if_else(is_mun, coalesce(day_tot, 0) * coalesce(lib_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_playarea = sum(
      if_else(is_mun, coalesce(day_tot, 0) * coalesce(play_area, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_lunch = sum(
      if_else(is_mun, coalesce(day_tot, 0) * coalesce(lunch, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_no_water = sum(
      if_else(is_mun, coalesce(day_tot, 0) * coalesce(water_non, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_water_dum = sum(
      if_else(is_mun, coalesce(day_tot, 0) * coalesce(water_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_no_sewage = sum(
      if_else(is_mun, coalesce(day_tot, 0) * coalesce(sewage_non, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_sewage_dum = sum(
      if_else(is_mun, coalesce(day_tot, 0) * coalesce(sewage_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_no_energy = sum(
      if_else(is_mun, coalesce(day_tot, 0) * coalesce(energy_non, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_energy_dum = sum(
      if_else(is_mun, coalesce(day_tot, 0) * coalesce(energy_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_employee = sum(
      if_else(is_mun, coalesce(day_tot, 0) * coalesce(employee, 0), 0),
      na.rm = TRUE
    ),
    
    
    # -------- Preschool exposure -------- #
    
    pre_tot_classroom = sum(
      coalesce(pre_tot, 0) * coalesce(classroom, 0),
      na.rm = TRUE
    ),
    
    pre_tot_teachroom = sum(
      coalesce(pre_tot, 0) * coalesce(teach_room, 0),
      na.rm = TRUE
    ),
    
    pre_tot_labs = sum(
      coalesce(pre_tot, 0) * coalesce(lab_dummy, 0),
      na.rm = TRUE
    ),
    
    pre_tot_library = sum(
      coalesce(pre_tot, 0) * coalesce(lib_dummy, 0),
      na.rm = TRUE
    ),
    
    pre_tot_playarea = sum(
      coalesce(pre_tot, 0) * coalesce(play_area, 0),
      na.rm = TRUE
    ),
    
    pre_tot_lunch = sum(
      coalesce(pre_tot, 0) * coalesce(lunch, 0),
      na.rm = TRUE
    ),
    
    pre_tot_no_water = sum(
      coalesce(pre_tot, 0) * coalesce(water_non, 0),
      na.rm = TRUE
    ),
    
    pre_tot_water_dum = sum(
      coalesce(pre_tot, 0) * coalesce(water_dummy, 0),
      na.rm = TRUE
    ),
    
    pre_tot_no_sewage = sum(
      coalesce(pre_tot, 0) * coalesce(sewage_non, 0),
      na.rm = TRUE
    ),
    
    pre_tot_sewage_dum = sum(
      coalesce(pre_tot, 0) * coalesce(sewage_dummy, 0),
      na.rm = TRUE
    ),
    
    pre_tot_no_energy = sum(
      coalesce(pre_tot, 0) * coalesce(energy_non, 0),
      na.rm = TRUE
    ),
    
    pre_tot_energy_dum = sum(
      coalesce(pre_tot, 0) * coalesce(energy_dummy, 0),
      na.rm = TRUE
    ),
    
    pre_tot_employee = sum(
      coalesce(pre_tot, 0) * coalesce(employee, 0),
      na.rm = TRUE
    ),
    
    pre_pub_classroom = sum(
      if_else(is_mun, coalesce(pre_tot, 0) * coalesce(classroom, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_teachroom = sum(
      if_else(is_mun, coalesce(pre_tot, 0) * coalesce(teach_room, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_labs = sum(
      if_else(is_mun, coalesce(pre_tot, 0) * coalesce(lab_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_library = sum(
      if_else(is_mun, coalesce(pre_tot, 0) * coalesce(lib_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_playarea = sum(
      if_else(is_mun, coalesce(pre_tot, 0) * coalesce(play_area, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_lunch = sum(
      if_else(is_mun, coalesce(pre_tot, 0) * coalesce(lunch, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_no_water = sum(
      if_else(is_mun, coalesce(pre_tot, 0) * coalesce(water_non, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_water_dum = sum(
      if_else(is_mun, coalesce(pre_tot, 0) * coalesce(water_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_no_sewage = sum(
      if_else(is_mun, coalesce(pre_tot, 0) * coalesce(sewage_non, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_sewage_dum = sum(
      if_else(is_mun, coalesce(pre_tot, 0) * coalesce(sewage_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_no_energy = sum(
      if_else(is_mun, coalesce(pre_tot, 0) * coalesce(energy_non, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_energy_dum = sum(
      if_else(is_mun, coalesce(pre_tot, 0) * coalesce(energy_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_employee = sum(
      if_else(is_mun, coalesce(pre_tot, 0) * coalesce(employee, 0), 0),
      na.rm = TRUE
    ),
    
    # -------- Fundamental exposure -------- #
    
    fun_tot_classroom = sum(
      coalesce(fun_enroll, 0) * coalesce(classroom, 0),
      na.rm = TRUE
    ),
    
    fun_tot_teachroom = sum(
      coalesce(fun_enroll, 0) * coalesce(teach_room, 0),
      na.rm = TRUE
    ),
    
    fun_tot_labs = sum(
      coalesce(fun_enroll, 0) * coalesce(lab_dummy, 0),
      na.rm = TRUE
    ),
    
    fun_tot_library = sum(
      coalesce(fun_enroll, 0) * coalesce(lib_dummy, 0),
      na.rm = TRUE
    ),
    
    fun_tot_playarea = sum(
      coalesce(fun_enroll, 0) * coalesce(play_area, 0),
      na.rm = TRUE
    ),
    
    fun_tot_lunch = sum(
      coalesce(fun_enroll, 0) * coalesce(lunch, 0),
      na.rm = TRUE
    ),
    
    fun_tot_no_water = sum(
      coalesce(fun_enroll, 0) * coalesce(water_non, 0),
      na.rm = TRUE
    ),
    
    fun_tot_water_dum = sum(
      coalesce(fun_enroll, 0) * coalesce(water_dummy, 0),
      na.rm = TRUE
    ),
    
    fun_tot_no_sewage = sum(
      coalesce(fun_enroll, 0) * coalesce(sewage_non, 0),
      na.rm = TRUE
    ),
    
    fun_tot_sewage_dum = sum(
      coalesce(fun_enroll, 0) * coalesce(sewage_dummy, 0),
      na.rm = TRUE
    ),
    
    fun_tot_no_energy = sum(
      coalesce(fun_enroll, 0) * coalesce(energy_non, 0),
      na.rm = TRUE
    ),
    
    fun_tot_energy_dum = sum(
      coalesce(fun_enroll, 0) * coalesce(energy_dummy, 0),
      na.rm = TRUE
    ),
    
    fun_tot_employee = sum(
      coalesce(fun_enroll, 0) * coalesce(employee, 0),
      na.rm = TRUE
    ),
    
    fun_pub_classroom = sum(
      if_else(is_mun, coalesce(fun_enroll, 0) * coalesce(classroom, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_teachroom = sum(
      if_else(is_mun, coalesce(fun_enroll, 0) * coalesce(teach_room, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_labs = sum(
      if_else(is_mun, coalesce(fun_enroll, 0) * coalesce(lab_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_library = sum(
      if_else(is_mun, coalesce(fun_enroll, 0) * coalesce(lib_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_playarea = sum(
      if_else(is_mun, coalesce(fun_enroll, 0) * coalesce(play_area, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_lunch = sum(
      if_else(is_mun, coalesce(fun_enroll, 0) * coalesce(lunch, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_no_water = sum(
      if_else(is_mun, coalesce(fun_enroll, 0) * coalesce(water_non, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_water_dum = sum(
      if_else(is_mun, coalesce(fun_enroll, 0) * coalesce(water_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_no_sewage = sum(
      if_else(is_mun, coalesce(fun_enroll, 0) * coalesce(sewage_non, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_sewage_dum = sum(
      if_else(is_mun, coalesce(fun_enroll, 0) * coalesce(sewage_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_no_energy = sum(
      if_else(is_mun, coalesce(fun_enroll, 0) * coalesce(energy_non, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_energy_dum = sum(
      if_else(is_mun, coalesce(fun_enroll, 0) * coalesce(energy_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_employee = sum(
      if_else(is_mun, coalesce(fun_enroll, 0) * coalesce(employee, 0), 0),
      na.rm = TRUE
    ),
    
    
    .groups = "drop"
  )


# Combining both databases
# ---------------------------------------------------------------------------- #
### 5.3.2 Age data ----
# ---------------------------------------------------------------------------- #

df_lvl_mun <- df_lvl_mun %>% 
  left_join(pop_mun %>% mutate(ano = as.numeric(ano)), by = c("codmun" = "cod_mun", "ano"))


# ---------------------------------------------------------------------------- #
### 5.3.3 Prop. data ----
# ---------------------------------------------------------------------------- #

df_prop_mun <- df_lvl_mun %>% 
  mutate(
    # -------- CREC -------- #
    cre_exp_classroom = if_else(!is.na(cre_tot_classroom) & cre_tot_classroom > 0,
                                cre_pub_classroom / cre_tot_classroom, NA_real_),
    
    cre_exp_teachroom = if_else(!is.na(cre_tot_teachroom) & cre_tot_teachroom > 0,
                                cre_pub_teachroom / cre_tot_teachroom, NA_real_),
    
    cre_exp_labs = if_else(!is.na(cre_tot_labs) & cre_tot_labs > 0,
                           cre_pub_labs / cre_tot_labs, NA_real_),
    
    cre_exp_library = if_else(!is.na(cre_tot_library) & cre_tot_library > 0,
                              cre_pub_library / cre_tot_library, NA_real_),
    
    cre_exp_playarea = if_else(!is.na(cre_tot_playarea) & cre_tot_playarea > 0,
                               cre_pub_playarea / cre_tot_playarea, NA_real_),
    
    cre_exp_lunch = if_else(!is.na(cre_tot_lunch) & cre_tot_lunch > 0,
                            cre_pub_lunch / cre_tot_lunch, NA_real_),
    
    cre_exp_no_water = if_else(!is.na(cre_tot_no_water) & cre_tot_no_water > 0,
                               cre_pub_no_water / cre_tot_no_water, NA_real_),
    
    cre_exp_water_dum = if_else(!is.na(cre_tot_water_dum) & cre_tot_water_dum > 0,
                                cre_pub_water_dum / cre_tot_water_dum, NA_real_),
    
    cre_exp_no_sewage = if_else(!is.na(cre_tot_no_sewage) & cre_tot_no_sewage > 0,
                                cre_pub_no_sewage / cre_tot_no_sewage, NA_real_),
    
    cre_exp_sewage_dum = if_else(!is.na(cre_tot_sewage_dum) & cre_tot_sewage_dum > 0,
                                 cre_pub_sewage_dum / cre_tot_sewage_dum, NA_real_),
    
    cre_exp_no_energy = if_else(!is.na(cre_tot_no_energy) & cre_tot_no_energy > 0,
                                cre_pub_no_energy / cre_tot_no_energy, NA_real_),
    
    cre_exp_energy_dum = if_else(!is.na(cre_tot_energy_dum) & cre_tot_energy_dum > 0,
                                 cre_pub_energy_dum / cre_tot_energy_dum, NA_real_),
    
    cre_exp_employee = if_else(!is.na(cre_tot_employee) & cre_tot_employee > 0,
                               cre_pub_employee / cre_tot_employee, NA_real_),
    # -------- PREE -------- #
    
    pre_exp_classroom = if_else(!is.na(pre_tot_classroom) & pre_tot_classroom > 0,
                                pre_pub_classroom / pre_tot_classroom, NA_real_),
    
    pre_exp_teachroom = if_else(!is.na(pre_tot_teachroom) & pre_tot_teachroom > 0,
                                pre_pub_teachroom / pre_tot_teachroom, NA_real_),
    
    pre_exp_labs = if_else(!is.na(pre_tot_labs) & pre_tot_labs > 0,
                           pre_pub_labs / pre_tot_labs, NA_real_),
    
    pre_exp_library = if_else(!is.na(pre_tot_library) & pre_tot_library > 0,
                              pre_pub_library / pre_tot_library, NA_real_),
    
    pre_exp_playarea = if_else(!is.na(pre_tot_playarea) & pre_tot_playarea > 0,
                               pre_pub_playarea / pre_tot_playarea, NA_real_),
    
    pre_exp_lunch = if_else(!is.na(pre_tot_lunch) & pre_tot_lunch > 0,
                            pre_pub_lunch / pre_tot_lunch, NA_real_),
    
    pre_exp_no_water = if_else(!is.na(pre_tot_no_water) & pre_tot_no_water > 0,
                               pre_pub_no_water / pre_tot_no_water, NA_real_),
    
    pre_exp_water_dum = if_else(!is.na(pre_tot_water_dum) & pre_tot_water_dum > 0,
                                pre_pub_water_dum / pre_tot_water_dum, NA_real_),
    
    pre_exp_no_sewage = if_else(!is.na(pre_tot_no_sewage) & pre_tot_no_sewage > 0,
                                pre_pub_no_sewage / pre_tot_no_sewage, NA_real_),
    
    pre_exp_sewage_dum = if_else(!is.na(pre_tot_sewage_dum) & pre_tot_sewage_dum > 0,
                                 pre_pub_sewage_dum / pre_tot_sewage_dum, NA_real_),
    
    pre_exp_no_energy = if_else(!is.na(pre_tot_no_energy) & pre_tot_no_energy > 0,
                                pre_pub_no_energy / pre_tot_no_energy, NA_real_),
    
    pre_exp_energy_dum = if_else(!is.na(pre_tot_energy_dum) & pre_tot_energy_dum > 0,
                                 pre_pub_energy_dum / pre_tot_energy_dum, NA_real_),
    
    pre_exp_employee = if_else(!is.na(pre_tot_employee) & pre_tot_employee > 0,
                               pre_pub_employee / pre_tot_employee, NA_real_),
    
    # -------- FUND -------- #
    fun_exp_classroom = if_else(!is.na(fun_tot_classroom) & fun_tot_classroom > 0,
                                fun_pub_classroom / fun_tot_classroom, NA_real_),
    
    fun_exp_teachroom = if_else(!is.na(fun_tot_teachroom) & fun_tot_teachroom > 0,
                                fun_pub_teachroom / fun_tot_teachroom, NA_real_),
    
    fun_exp_labs = if_else(!is.na(fun_tot_labs) & fun_tot_labs > 0,
                           fun_pub_labs / fun_tot_labs, NA_real_),
    
    fun_exp_library = if_else(!is.na(fun_tot_library) & fun_tot_library > 0,
                              fun_pub_library / fun_tot_library, NA_real_),
    
    fun_exp_playarea = if_else(!is.na(fun_tot_playarea) & fun_tot_playarea > 0,
                               fun_pub_playarea / fun_tot_playarea, NA_real_),
    
    fun_exp_lunch = if_else(!is.na(fun_tot_lunch) & fun_tot_lunch > 0,
                            fun_pub_lunch / fun_tot_lunch, NA_real_),
    
    fun_exp_no_water = if_else(!is.na(fun_tot_no_water) & fun_tot_no_water > 0,
                               fun_pub_no_water / fun_tot_no_water, NA_real_),
    
    fun_exp_water_dum = if_else(!is.na(fun_tot_water_dum) & fun_tot_water_dum > 0,
                                fun_pub_water_dum / fun_tot_water_dum, NA_real_),
    
    fun_exp_no_sewage = if_else(!is.na(fun_tot_no_sewage) & fun_tot_no_sewage > 0,
                                fun_pub_no_sewage / fun_tot_no_sewage, NA_real_),
    
    fun_exp_sewage_dum = if_else(!is.na(fun_tot_sewage_dum) & fun_tot_sewage_dum > 0,
                                 fun_pub_sewage_dum / fun_tot_sewage_dum, NA_real_),
    
    fun_exp_no_energy = if_else(!is.na(fun_tot_no_energy) & fun_tot_no_energy > 0,
                                fun_pub_no_energy / fun_tot_no_energy, NA_real_),
    
    fun_exp_energy_dum = if_else(!is.na(fun_tot_energy_dum) & fun_tot_energy_dum > 0,
                                 fun_pub_energy_dum / fun_tot_energy_dum, NA_real_),
    
    fun_exp_employee = if_else(!is.na(fun_tot_employee) & fun_tot_employee > 0,
                               fun_pub_employee / fun_tot_employee, NA_real_)
  ) %>% 
  select(
    -c(cre_tot_classroom:fun_pub_employee #Removing the non-more needed variables
       )
  )


# ---- Saving ---- #

saveRDS(df_prop_mun, "Z:/Tuffy/Paper - Educ/Dados/intermediate/mun_prop_data_school_type.rds")



# ---------------------------------------------------------------------------- #
## 5.4 School type Infra with Affil ----
# ---------------------------------------------------------------------------- #

pop_mun <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/pop_age.rds")

df_enroll <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/censo_escolar_base_v2.rds")

# ---------------------------------------------------------------------------- #
### 5.4.1. Variables ----
# ---------------------------------------------------------------------------- #

# Municipal enrollment
df_lvl_mun <- df_enroll %>% 
  ungroup() %>% 
  mutate(is_mun = dep_adm == "Municipal",
         is_afl = (affiliated == 1 & affil_mun == 1)) %>%
  group_by(codmun, ano) %>% 
  summarise(
    
    # -------- Enrollment totals -------- #
    inf_enroll = sum(
      rowSums(across(c(pre_tot, day_tot)), na.rm = TRUE),
      na.rm = TRUE
    ),
    
    fun_enroll = sum(
      rowSums(across(c(reg_in_9, reg_in_8, reg_fin_9, reg_fin_8)), na.rm = TRUE),
      na.rm = TRUE
    ),
    
    total_enroll = sum(coalesce(enroll, 0), na.rm = TRUE),
    
    total_pub_enroll = sum(
      if_else(is_mun | is_afl, coalesce(enroll, 0), 0),
      na.rm = TRUE
    ),
    
    inf_pub_enroll = sum(
      if_else(
        is_mun | is_afl,
        rowSums(across(c(pre_tot, day_tot)), na.rm = TRUE),
        0,
        missing = 0
      ),
      na.rm = TRUE
    ),
    
    cre_enroll = sum(coalesce(day_tot, 0), na.rm = TRUE),
    pre_enroll = sum(coalesce(pre_tot, 0), na.rm = TRUE),
    
    cre_pub_enroll = sum(
      if_else(is_mun | is_afl, coalesce(day_tot, 0), 0, missing = 0),
      na.rm = TRUE
    ),
    
    pre_pub_enroll = sum(
      if_else(is_mun | is_afl, coalesce(pre_tot, 0), 0, missing = 0),
      na.rm = TRUE
    ),
    
    fun_pub_enroll = sum(
      if_else(
        is_mun | is_afl,
        rowSums(across(c(reg_in_9, reg_in_8, reg_fin_9, reg_fin_8)), na.rm = TRUE),
        0,
        missing = 0
      ),
      na.rm = TRUE
    ),
    
    # -------- Creche exposure -------- #
    
    cre_tot_classroom = sum(
      coalesce(day_tot, 0) * coalesce(classroom, 0),
      na.rm = TRUE
    ),
    
    cre_tot_teachroom = sum(
      coalesce(day_tot, 0) * coalesce(teach_room, 0),
      na.rm = TRUE
    ),
    
    cre_tot_labs = sum(
      coalesce(day_tot, 0) * coalesce(lab_dummy, 0),
      na.rm = TRUE
    ),
    
    cre_tot_library = sum(
      coalesce(day_tot, 0) * coalesce(lib_dummy, 0),
      na.rm = TRUE
    ),
    
    cre_tot_playarea = sum(
      coalesce(day_tot, 0) * coalesce(play_area, 0),
      na.rm = TRUE
    ),
    
    cre_tot_lunch = sum(
      coalesce(day_tot, 0) * coalesce(lunch, 0),
      na.rm = TRUE
    ),
    
    cre_tot_no_water = sum(
      coalesce(day_tot, 0) * coalesce(water_non, 0),
      na.rm = TRUE
    ),
    
    cre_tot_water_dum = sum(
      coalesce(day_tot, 0) * coalesce(water_dummy, 0),
      na.rm = TRUE
    ),
    
    cre_tot_no_sewage = sum(
      coalesce(day_tot, 0) * coalesce(sewage_non, 0),
      na.rm = TRUE
    ),
    
    cre_tot_sewage_dum = sum(
      coalesce(day_tot, 0) * coalesce(sewage_dummy, 0),
      na.rm = TRUE
    ),
    
    cre_tot_no_energy = sum(
      coalesce(day_tot, 0) * coalesce(energy_non, 0),
      na.rm = TRUE
    ),
    
    cre_tot_energy_dum = sum(
      coalesce(day_tot, 0) * coalesce(energy_dummy, 0),
      na.rm = TRUE
    ),
    
    cre_tot_employee = sum(
      coalesce(day_tot, 0) * coalesce(employee, 0),
      na.rm = TRUE
    ),
    
    cre_pub_classroom = sum(
      if_else(is_mun | is_afl, coalesce(day_tot, 0) * coalesce(classroom, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_teachroom = sum(
      if_else(is_mun | is_afl, coalesce(day_tot, 0) * coalesce(teach_room, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_labs = sum(
      if_else(is_mun | is_afl, coalesce(day_tot, 0) * coalesce(lab_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_library = sum(
      if_else(is_mun | is_afl, coalesce(day_tot, 0) * coalesce(lib_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_playarea = sum(
      if_else(is_mun | is_afl, coalesce(day_tot, 0) * coalesce(play_area, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_lunch = sum(
      if_else(is_mun | is_afl, coalesce(day_tot, 0) * coalesce(lunch, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_no_water = sum(
      if_else(is_mun | is_afl, coalesce(day_tot, 0) * coalesce(water_non, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_water_dum = sum(
      if_else(is_mun | is_afl, coalesce(day_tot, 0) * coalesce(water_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_no_sewage = sum(
      if_else(is_mun | is_afl, coalesce(day_tot, 0) * coalesce(sewage_non, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_sewage_dum = sum(
      if_else(is_mun | is_afl, coalesce(day_tot, 0) * coalesce(sewage_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_no_energy = sum(
      if_else(is_mun | is_afl, coalesce(day_tot, 0) * coalesce(energy_non, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_energy_dum = sum(
      if_else(is_mun | is_afl, coalesce(day_tot, 0) * coalesce(energy_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    cre_pub_employee = sum(
      if_else(is_mun | is_afl, coalesce(day_tot, 0) * coalesce(employee, 0), 0),
      na.rm = TRUE
    ),
    
    
    # -------- Preschool exposure -------- #
    
    pre_tot_classroom = sum(
      coalesce(pre_tot, 0) * coalesce(classroom, 0),
      na.rm = TRUE
    ),
    
    pre_tot_teachroom = sum(
      coalesce(pre_tot, 0) * coalesce(teach_room, 0),
      na.rm = TRUE
    ),
    
    pre_tot_labs = sum(
      coalesce(pre_tot, 0) * coalesce(lab_dummy, 0),
      na.rm = TRUE
    ),
    
    pre_tot_library = sum(
      coalesce(pre_tot, 0) * coalesce(lib_dummy, 0),
      na.rm = TRUE
    ),
    
    pre_tot_playarea = sum(
      coalesce(pre_tot, 0) * coalesce(play_area, 0),
      na.rm = TRUE
    ),
    
    pre_tot_lunch = sum(
      coalesce(pre_tot, 0) * coalesce(lunch, 0),
      na.rm = TRUE
    ),
    
    pre_tot_no_water = sum(
      coalesce(pre_tot, 0) * coalesce(water_non, 0),
      na.rm = TRUE
    ),
    
    pre_tot_water_dum = sum(
      coalesce(pre_tot, 0) * coalesce(water_dummy, 0),
      na.rm = TRUE
    ),
    
    pre_tot_no_sewage = sum(
      coalesce(pre_tot, 0) * coalesce(sewage_non, 0),
      na.rm = TRUE
    ),
    
    pre_tot_sewage_dum = sum(
      coalesce(pre_tot, 0) * coalesce(sewage_dummy, 0),
      na.rm = TRUE
    ),
    
    pre_tot_no_energy = sum(
      coalesce(pre_tot, 0) * coalesce(energy_non, 0),
      na.rm = TRUE
    ),
    
    pre_tot_energy_dum = sum(
      coalesce(pre_tot, 0) * coalesce(energy_dummy, 0),
      na.rm = TRUE
    ),
    
    pre_tot_employee = sum(
      coalesce(pre_tot, 0) * coalesce(employee, 0),
      na.rm = TRUE
    ),
    
    pre_pub_classroom = sum(
      if_else(is_mun | is_afl, coalesce(pre_tot, 0) * coalesce(classroom, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_teachroom = sum(
      if_else(is_mun | is_afl, coalesce(pre_tot, 0) * coalesce(teach_room, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_labs = sum(
      if_else(is_mun | is_afl, coalesce(pre_tot, 0) * coalesce(lab_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_library = sum(
      if_else(is_mun | is_afl, coalesce(pre_tot, 0) * coalesce(lib_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_playarea = sum(
      if_else(is_mun | is_afl, coalesce(pre_tot, 0) * coalesce(play_area, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_lunch = sum(
      if_else(is_mun | is_afl, coalesce(pre_tot, 0) * coalesce(lunch, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_no_water = sum(
      if_else(is_mun | is_afl, coalesce(pre_tot, 0) * coalesce(water_non, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_water_dum = sum(
      if_else(is_mun | is_afl, coalesce(pre_tot, 0) * coalesce(water_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_no_sewage = sum(
      if_else(is_mun | is_afl, coalesce(pre_tot, 0) * coalesce(sewage_non, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_sewage_dum = sum(
      if_else(is_mun | is_afl, coalesce(pre_tot, 0) * coalesce(sewage_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_no_energy = sum(
      if_else(is_mun | is_afl, coalesce(pre_tot, 0) * coalesce(energy_non, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_energy_dum = sum(
      if_else(is_mun | is_afl, coalesce(pre_tot, 0) * coalesce(energy_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    pre_pub_employee = sum(
      if_else(is_mun | is_afl, coalesce(pre_tot, 0) * coalesce(employee, 0), 0),
      na.rm = TRUE
    ),
    
    # -------- Fundamental exposure -------- #
    
    fun_tot_classroom = sum(
      coalesce(fun_enroll, 0) * coalesce(classroom, 0),
      na.rm = TRUE
    ),
    
    fun_tot_teachroom = sum(
      coalesce(fun_enroll, 0) * coalesce(teach_room, 0),
      na.rm = TRUE
    ),
    
    fun_tot_labs = sum(
      coalesce(fun_enroll, 0) * coalesce(lab_dummy, 0),
      na.rm = TRUE
    ),
    
    fun_tot_library = sum(
      coalesce(fun_enroll, 0) * coalesce(lib_dummy, 0),
      na.rm = TRUE
    ),
    
    fun_tot_playarea = sum(
      coalesce(fun_enroll, 0) * coalesce(play_area, 0),
      na.rm = TRUE
    ),
    
    fun_tot_lunch = sum(
      coalesce(fun_enroll, 0) * coalesce(lunch, 0),
      na.rm = TRUE
    ),
    
    fun_tot_no_water = sum(
      coalesce(fun_enroll, 0) * coalesce(water_non, 0),
      na.rm = TRUE
    ),
    
    fun_tot_water_dum = sum(
      coalesce(fun_enroll, 0) * coalesce(water_dummy, 0),
      na.rm = TRUE
    ),
    
    fun_tot_no_sewage = sum(
      coalesce(fun_enroll, 0) * coalesce(sewage_non, 0),
      na.rm = TRUE
    ),
    
    fun_tot_sewage_dum = sum(
      coalesce(fun_enroll, 0) * coalesce(sewage_dummy, 0),
      na.rm = TRUE
    ),
    
    fun_tot_no_energy = sum(
      coalesce(fun_enroll, 0) * coalesce(energy_non, 0),
      na.rm = TRUE
    ),
    
    fun_tot_energy_dum = sum(
      coalesce(fun_enroll, 0) * coalesce(energy_dummy, 0),
      na.rm = TRUE
    ),
    
    fun_tot_employee = sum(
      coalesce(fun_enroll, 0) * coalesce(employee, 0),
      na.rm = TRUE
    ),
    
    fun_pub_classroom = sum(
      if_else(is_mun | is_afl, coalesce(fun_enroll, 0) * coalesce(classroom, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_teachroom = sum(
      if_else(is_mun | is_afl, coalesce(fun_enroll, 0) * coalesce(teach_room, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_labs = sum(
      if_else(is_mun | is_afl, coalesce(fun_enroll, 0) * coalesce(lab_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_library = sum(
      if_else(is_mun | is_afl, coalesce(fun_enroll, 0) * coalesce(lib_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_playarea = sum(
      if_else(is_mun | is_afl, coalesce(fun_enroll, 0) * coalesce(play_area, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_lunch = sum(
      if_else(is_mun | is_afl, coalesce(fun_enroll, 0) * coalesce(lunch, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_no_water = sum(
      if_else(is_mun | is_afl, coalesce(fun_enroll, 0) * coalesce(water_non, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_water_dum = sum(
      if_else(is_mun | is_afl, coalesce(fun_enroll, 0) * coalesce(water_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_no_sewage = sum(
      if_else(is_mun | is_afl, coalesce(fun_enroll, 0) * coalesce(sewage_non, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_sewage_dum = sum(
      if_else(is_mun | is_afl, coalesce(fun_enroll, 0) * coalesce(sewage_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_no_energy = sum(
      if_else(is_mun | is_afl, coalesce(fun_enroll, 0) * coalesce(energy_non, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_energy_dum = sum(
      if_else(is_mun | is_afl, coalesce(fun_enroll, 0) * coalesce(energy_dummy, 0), 0),
      na.rm = TRUE
    ),
    
    fun_pub_employee = sum(
      if_else(is_mun | is_afl, coalesce(fun_enroll, 0) * coalesce(employee, 0), 0),
      na.rm = TRUE
    ),
    
    
    .groups = "drop"
  )


# Combining both databases
# ---------------------------------------------------------------------------- #
### 5.4.2 Age data ----
# ---------------------------------------------------------------------------- #

df_lvl_mun <- df_lvl_mun %>% 
  left_join(pop_mun %>% mutate(ano = as.numeric(ano)), by = c("codmun" = "cod_mun", "ano"))


# ---------------------------------------------------------------------------- #
### 5.4.3 Prop. data ----
# ---------------------------------------------------------------------------- #

df_prop_mun <- df_lvl_mun %>% 
  mutate(
    # -------- CREC -------- #
    cre_exp_classroom = if_else(!is.na(cre_tot_classroom) & cre_tot_classroom > 0,
                                cre_pub_classroom / cre_tot_classroom, NA_real_),
    
    cre_exp_teachroom = if_else(!is.na(cre_tot_teachroom) & cre_tot_teachroom > 0,
                                cre_pub_teachroom / cre_tot_teachroom, NA_real_),
    
    cre_exp_labs = if_else(!is.na(cre_tot_labs) & cre_tot_labs > 0,
                           cre_pub_labs / cre_tot_labs, NA_real_),
    
    cre_exp_library = if_else(!is.na(cre_tot_library) & cre_tot_library > 0,
                              cre_pub_library / cre_tot_library, NA_real_),
    
    cre_exp_playarea = if_else(!is.na(cre_tot_playarea) & cre_tot_playarea > 0,
                               cre_pub_playarea / cre_tot_playarea, NA_real_),
    
    cre_exp_lunch = if_else(!is.na(cre_tot_lunch) & cre_tot_lunch > 0,
                            cre_pub_lunch / cre_tot_lunch, NA_real_),
    
    cre_exp_no_water = if_else(!is.na(cre_tot_no_water) & cre_tot_no_water > 0,
                               cre_pub_no_water / cre_tot_no_water, NA_real_),
    
    cre_exp_water_dum = if_else(!is.na(cre_tot_water_dum) & cre_tot_water_dum > 0,
                                cre_pub_water_dum / cre_tot_water_dum, NA_real_),
    
    cre_exp_no_sewage = if_else(!is.na(cre_tot_no_sewage) & cre_tot_no_sewage > 0,
                                cre_pub_no_sewage / cre_tot_no_sewage, NA_real_),
    
    cre_exp_sewage_dum = if_else(!is.na(cre_tot_sewage_dum) & cre_tot_sewage_dum > 0,
                                 cre_pub_sewage_dum / cre_tot_sewage_dum, NA_real_),
    
    cre_exp_no_energy = if_else(!is.na(cre_tot_no_energy) & cre_tot_no_energy > 0,
                                cre_pub_no_energy / cre_tot_no_energy, NA_real_),
    
    cre_exp_energy_dum = if_else(!is.na(cre_tot_energy_dum) & cre_tot_energy_dum > 0,
                                 cre_pub_energy_dum / cre_tot_energy_dum, NA_real_),
    
    cre_exp_employee = if_else(!is.na(cre_tot_employee) & cre_tot_employee > 0,
                               cre_pub_employee / cre_tot_employee, NA_real_),
    # -------- PREE -------- #
    
    pre_exp_classroom = if_else(!is.na(pre_tot_classroom) & pre_tot_classroom > 0,
                                pre_pub_classroom / pre_tot_classroom, NA_real_),
    
    pre_exp_teachroom = if_else(!is.na(pre_tot_teachroom) & pre_tot_teachroom > 0,
                                pre_pub_teachroom / pre_tot_teachroom, NA_real_),
    
    pre_exp_labs = if_else(!is.na(pre_tot_labs) & pre_tot_labs > 0,
                           pre_pub_labs / pre_tot_labs, NA_real_),
    
    pre_exp_library = if_else(!is.na(pre_tot_library) & pre_tot_library > 0,
                              pre_pub_library / pre_tot_library, NA_real_),
    
    pre_exp_playarea = if_else(!is.na(pre_tot_playarea) & pre_tot_playarea > 0,
                               pre_pub_playarea / pre_tot_playarea, NA_real_),
    
    pre_exp_lunch = if_else(!is.na(pre_tot_lunch) & pre_tot_lunch > 0,
                            pre_pub_lunch / pre_tot_lunch, NA_real_),
    
    pre_exp_no_water = if_else(!is.na(pre_tot_no_water) & pre_tot_no_water > 0,
                               pre_pub_no_water / pre_tot_no_water, NA_real_),
    
    pre_exp_water_dum = if_else(!is.na(pre_tot_water_dum) & pre_tot_water_dum > 0,
                                pre_pub_water_dum / pre_tot_water_dum, NA_real_),
    
    pre_exp_no_sewage = if_else(!is.na(pre_tot_no_sewage) & pre_tot_no_sewage > 0,
                                pre_pub_no_sewage / pre_tot_no_sewage, NA_real_),
    
    pre_exp_sewage_dum = if_else(!is.na(pre_tot_sewage_dum) & pre_tot_sewage_dum > 0,
                                 pre_pub_sewage_dum / pre_tot_sewage_dum, NA_real_),
    
    pre_exp_no_energy = if_else(!is.na(pre_tot_no_energy) & pre_tot_no_energy > 0,
                                pre_pub_no_energy / pre_tot_no_energy, NA_real_),
    
    pre_exp_energy_dum = if_else(!is.na(pre_tot_energy_dum) & pre_tot_energy_dum > 0,
                                 pre_pub_energy_dum / pre_tot_energy_dum, NA_real_),
    
    pre_exp_employee = if_else(!is.na(pre_tot_employee) & pre_tot_employee > 0,
                               pre_pub_employee / pre_tot_employee, NA_real_),
    
    # -------- FUND -------- #
    fun_exp_classroom = if_else(!is.na(fun_tot_classroom) & fun_tot_classroom > 0,
                                fun_pub_classroom / fun_tot_classroom, NA_real_),
    
    fun_exp_teachroom = if_else(!is.na(fun_tot_teachroom) & fun_tot_teachroom > 0,
                                fun_pub_teachroom / fun_tot_teachroom, NA_real_),
    
    fun_exp_labs = if_else(!is.na(fun_tot_labs) & fun_tot_labs > 0,
                           fun_pub_labs / fun_tot_labs, NA_real_),
    
    fun_exp_library = if_else(!is.na(fun_tot_library) & fun_tot_library > 0,
                              fun_pub_library / fun_tot_library, NA_real_),
    
    fun_exp_playarea = if_else(!is.na(fun_tot_playarea) & fun_tot_playarea > 0,
                               fun_pub_playarea / fun_tot_playarea, NA_real_),
    
    fun_exp_lunch = if_else(!is.na(fun_tot_lunch) & fun_tot_lunch > 0,
                            fun_pub_lunch / fun_tot_lunch, NA_real_),
    
    fun_exp_no_water = if_else(!is.na(fun_tot_no_water) & fun_tot_no_water > 0,
                               fun_pub_no_water / fun_tot_no_water, NA_real_),
    
    fun_exp_water_dum = if_else(!is.na(fun_tot_water_dum) & fun_tot_water_dum > 0,
                                fun_pub_water_dum / fun_tot_water_dum, NA_real_),
    
    fun_exp_no_sewage = if_else(!is.na(fun_tot_no_sewage) & fun_tot_no_sewage > 0,
                                fun_pub_no_sewage / fun_tot_no_sewage, NA_real_),
    
    fun_exp_sewage_dum = if_else(!is.na(fun_tot_sewage_dum) & fun_tot_sewage_dum > 0,
                                 fun_pub_sewage_dum / fun_tot_sewage_dum, NA_real_),
    
    fun_exp_no_energy = if_else(!is.na(fun_tot_no_energy) & fun_tot_no_energy > 0,
                                fun_pub_no_energy / fun_tot_no_energy, NA_real_),
    
    fun_exp_energy_dum = if_else(!is.na(fun_tot_energy_dum) & fun_tot_energy_dum > 0,
                                 fun_pub_energy_dum / fun_tot_energy_dum, NA_real_),
    
    fun_exp_employee = if_else(!is.na(fun_tot_employee) & fun_tot_employee > 0,
                               fun_pub_employee / fun_tot_employee, NA_real_)
  ) %>% 
  select(
    -c(cre_tot_classroom:fun_pub_employee #Removing the non-more needed variables
    )
  )


# ---- Saving ---- #

saveRDS(df_prop_mun, "Z:/Tuffy/Paper - Educ/Dados/intermediate/afl_mun_prop_data_school_type.rds")














    


    
    
    

