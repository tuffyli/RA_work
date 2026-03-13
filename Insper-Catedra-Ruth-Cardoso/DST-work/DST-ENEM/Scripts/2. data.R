# ---------------------------------------------------------------------------- #
# Data Description
# DataBase adjustment
# Last edited by: Tuffy Licciardi Issa
# Date: 11/03/2026
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Libraries -----
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(data.table)
library(sf)
library(haven)
library(labelled)
library(rdrobust)
library(fastDummies)
library(janitor)
library(xtable)
library(viridis)
library(readstata13)
library(stringr)
library(RColorBrewer)
library(fixest)
library(rddensity)
library(tidyr)
library(stringi)
library(readxl)
library(knitr)
library(stargazer)



# ---------------------------------------------------------------------------- #
# Construção das bases de dados
# ---------------------------------------------------------------------------- #

# Data de criação: 14/11/2023
# Criado por: Bruno Komatsu

# Última modificação: 14/11/2023
# Modificado por: Bruno Komatsu

# Descrição: 
# Cria as bases de dados do ENEM

#------------------------------------------------------------------------------#
# 0. Organização do Script ----
#------------------------------------------------------------------------------#

# 1. Dados geográficos
# 2. Dados do ENEM

#------------------------------------------------------------------------------#
# 1. Dados geográficos ----
# Construção da variável de distância à fronteira do HV
#------------------------------------------------------------------------------#

# Base de municípios
mun_hv <- st_read(dsn = sh_path, layer = "BRMUE250GC_SIR") %>% 
  rename(co_municipio = 2) %>% 
  mutate(uf = as.integer(co_municipio) %/% 100000,
         uf = factor(uf, labels = c('RO','AC','AM','RR','PA','AP','TO',
                                    'MA','PI','CE','RN','PB','PE','AL','SE','BA',
                                    'MG','ES','RJ','SP',
                                    'PR','SC','RS',
                                    'MS','MT','GO','DF'))) %>% 
  mutate(hv = ifelse(as.integer(co_municipio) %/% 100000 >= 30, 1, 0))

# Polígono de áreas sem HV
mun_hv_0 <- mun_hv %>% 
  filter(hv == 0) %>% 
  st_union() %>% 
  st_sf()

line_hv_0 <- mun_hv_0 %>% 
  st_cast("MULTILINESTRING")

# Polígono de áreas com HV
mun_hv_1 <- mun_hv %>% 
  filter(hv == 1) %>% 
  st_union() %>% 
  st_sf()

line_hv_1 <- mun_hv_1 %>% 
  st_cast("MULTILINESTRING")

# Linha da fronteira do HV
line <- st_intersection(x = line_hv_0, y = line_hv_1)

# Shapes de UFs para construir linhas de segmentos da fronteira
uflist <- c("MT","RO","AM","PA","TO","GO","BA","MG","ES")
for(i in 1:length(uflist)) {
  
  temp <- mun_hv %>% 
    filter(uf == uflist[i]) %>% 
    st_union() %>% 
    st_sf()
  
  assign(
    paste0("mun_",tolower(uflist[i])),
    st_cast(temp,"MULTILINESTRING")
  )
  
  rm(i, temp)
  
}
rm(uflist)

seg1 <- st_intersection(x = mun_mt, y = mun_ro)
seg2 <- st_intersection(x = mun_mt, y = mun_am)
seg3 <- st_intersection(x = mun_mt, y = mun_pa)
seg4 <- st_intersection(x = mun_mt, y = mun_to)
seg5 <- st_intersection(x = mun_go, y = mun_to)
seg6 <- st_intersection(x = mun_go, y = mun_ba)
seg7 <- st_intersection(x = mun_mg, y = mun_ba)
seg8 <- st_intersection(x = mun_es, y = mun_ba)

ggplot() +
  geom_sf(data = seg1, color = RColorBrewer::brewer.pal(n = 8,name = "RdBu")[1]) +
  geom_sf(data = seg2, color = RColorBrewer::brewer.pal(n = 8,name = "RdBu")[2]) +
  geom_sf(data = seg3, color = RColorBrewer::brewer.pal(n = 8,name = "RdBu")[3]) +
  geom_sf(data = seg4, color = RColorBrewer::brewer.pal(n = 8,name = "RdBu")[4]) +
  geom_sf(data = seg5, color = RColorBrewer::brewer.pal(n = 8,name = "RdBu")[5]) +
  geom_sf(data = seg6, color = RColorBrewer::brewer.pal(n = 8,name = "RdBu")[6]) +
  geom_sf(data = seg7, color = RColorBrewer::brewer.pal(n = 8,name = "RdBu")[7]) +
  geom_sf(data = seg8, color = RColorBrewer::brewer.pal(n = 8,name = "RdBu")[8])

# Obtendo distância do centróide à fronteira do HV
mun_hv <- mun_hv %>% 
  mutate(centroid = st_centroid(geometry),
         dist_hv_border = ifelse(hv==1,
                                 st_distance(centroid, line_hv_0),
                                 st_distance(centroid, line_hv_1)),
         co_municipio = as.integer(co_municipio),
         uf = as.character(uf),
         dist_seg1 = st_distance(centroid, seg1),
         dist_seg2 = st_distance(centroid, seg2),
         dist_seg3 = st_distance(centroid, seg3),
         dist_seg4 = st_distance(centroid, seg4),
         dist_seg5 = st_distance(centroid, seg5),
         dist_seg6 = st_distance(centroid, seg6),
         dist_seg7 = st_distance(centroid, seg7),
         dist_seg8 = st_distance(centroid, seg8)) %>%
  rowwise() %>%
  mutate(
    dist_seg = min(dist_seg1,dist_seg2,dist_seg3,dist_seg4,dist_seg5,dist_seg6,dist_seg7,dist_seg8),
    seg = case_when(
      dist_seg == dist_seg1 ~ 1,
      dist_seg == dist_seg2 ~ 1,
      dist_seg == dist_seg3 ~ 2,
      dist_seg == dist_seg4 ~ 3,
      dist_seg == dist_seg5 ~ 4,
      dist_seg == dist_seg6 ~ 5,
      dist_seg == dist_seg7 ~ 6,
      dist_seg == dist_seg8 ~ 7
    )
  ) %>%
  select(
    -dist_seg1,
    -dist_seg2,
    -dist_seg3,
    -dist_seg4,
    -dist_seg5,
    -dist_seg6,
    -dist_seg7,
    -dist_seg8,
    -dist_seg
  )

saveRDS(mun_hv, file = "Bases de dados/revisao/mun_hv.RDS")
saveRDS(line, file = "Bases de dados/revisao/line.RDS")
rm(
  mun_hv_0,
  line_hv_0,
  mun_hv_1,
  line_hv_1,
  seg1,
  seg2,
  seg3,
  seg4,
  seg5,
  seg5,
  seg6,
  seg7,
  seg8,
  mun_am,mun_ba,mun_es,mun_go,mun_mg,mun_mt,mun_po,mun_ro,mun_to
)


# Mapa
# ggplot(mun_hv %>% mutate(a = ifelse(hv==1,
#                                     st_distance(st_cast(geometry, to = "MULTILINESTRING"), line_hv_0),
#                                     st_distance(st_cast(geometry, to = "MULTILINESTRING"), line_hv_1))), aes(color=a,fill=a)) +
#   geom_sf() +
#   scale_fill_viridis_c(option = "D") +
#   scale_color_viridis_c(option = "D")




#---------------------------------------------------------------------------- #
# 2. No Age Filter (Main) ----
# --------------------------------------------------------------------------- #

#' The functions here listed will be utilized in further data treatment.

# ---------------------------------------------------------------------------- #
##2.1 Functions ---------
# ---------------------------------------------------------------------------- #

#Padronizes the scores
pad <- function(x){
  x = (x - mean(x, na.rm = T)) / 
    sd(x, na.rm = T)
  return(x)
}

# Function to calculate the weighted standard errors, according to formula on:
#https://medium.com/@OttoYu/weighted-mean-and-standard-devation-computation-f1dbacdf49b6



weighted.sd <- function(x,w,na.rm){
  
  # Verifying whether x and w have the same length
  if(length(x) == length(w)){
    
    # removing NA
    if(!missing(na.rm)){
      if(na.rm == T){
        temp <- !is.na(x) & !is.na(w)
        x <- x[temp]
        w <- w[temp]
        rm(temp)
      } 
    }
    
    # Calculating SD
    x = ((w %*% (x - weighted.mean(x, w = w))^2) / (sum(w)))^(1/2)
    
    return(x)
  }
}


#Weighted padronization
wpad <- function(x,w){
  x = (x - weighted.mean(x,w = w, na.rm = T))/as.vector(weighted.sd(x, w = w, na.rm = T))
  return(x)
}


#Data organization
org_data <- function(base,ano,vlist,cnames) {
  
  # Organização da base de dados
  base <- base %>% relocate(vlist)
  
  colnames(base) <- cnames
  
  # Pessoas no domicílio ------------------------------------------------------#
  # [English: Peoples within each householf]
  
  if (ano == 2009) {
    # Modificações somente de 2009
    # [English: 2009 exclusive modifications]
    
    base <- base %>%
      mutate(
        # 2009
        pessoas_dom = case_when(
          pessoas_dom == 'F' ~ 'A',
          # 1 pessoa
          
          pessoas_dom %in% c('A', 'B', 'C') ~ 'B',
          # 2 a 4 pessoas
          
          pessoas_dom %in% c('D', 'E') ~ 'C',
          # 5 ou mais pessoas
          
          .default = 'D'
        ),
      )
  } else if (ano == 2010) {
    # Modificações somente de 2010
    
    base <- base %>%
      mutate(
        # 2010
        pessoas_dom = case_when(
          pessoas_dom == 'E' ~ 'A',
          pessoas_dom == 'A' ~ 'B',
          pessoas_dom %in% c('B', 'C', 'D') ~ 'C',
          .default = 'D'
        )
      )
    
  } else if (ano == 2011) {
    
    # Modificações somente de 2011
    base <- base %>%
      mutate(
        pessoas_dom = as.integer(pessoas_dom),
        pessoas_dom = case_when(
          pessoas_dom == 1 ~ 'A',
          pessoas_dom >= 2 & pessoas_dom <= 4 ~ 'B',
          pessoas_dom >= 5 & pessoas_dom <= 20 ~ 'C',
          .default = 'D'
        )
      )
    
  } else if (ano >= 2012) {
    
    # Modificações somente de 2012 a 2019
    # base <- base %>%
    #   mutate(
    #     pessoas_dom = case_when(
    #       pessoas_dom == 1 ~ 'A',
    #       pessoas_dom >= 2 & pessoas_dom <= 4 ~ 'B',
    #       pessoas_dom >= 5 & pessoas_dom <= 20 ~ 'C',
    #       .default = 'D'
    #     )
    #   )
  }  
  
  # Renda domiciliar ----------------------------------------------------------#
  
  if (ano %in% c(2009,2010)) {
    
    # Modificações somente de 2009 e 2010
    base <- base %>%
      mutate(
        # 2009, 2010
        renda_dom = case_when(
          renda_dom == 'H' ~ 'A',
          # Nenhuma
          renda_dom == 'A' ~ 'B',
          # Até 1 SM
          renda_dom %in% c('B', 'C', 'D') ~ 'C',
          # >1 a 10 SM
          renda_dom %in% c('E', 'F', 'G') ~ 'D',
          # >10 a 50 SM
          .default = 'E'
        )
      )
  } else if (ano == 2011) {
    
    # Modificações somente de 2011
    base <- base %>%
      mutate(
        renda_dom = case_when(
          renda_dom == 'A' ~ 'A',
          renda_dom == 'B' ~ 'B',
          renda_dom %in% c('C', 'D', 'E', 'F', 'G') ~ 'C',
          renda_dom %in% c('H', 'I', 'J', 'K') ~ 'D',
          .default = 'E'
        )
      )
  } else if (ano >= 2012) {
    
    # Modificações a partir de 2012
    base <- base %>%
      mutate(
        renda_dom = case_when(
          renda_dom == 'A' ~ 'A',
          renda_dom == 'B' ~ 'B',
          renda_dom %in% c('C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M') ~ 'C',
          renda_dom %in% c('N', 'O', 'P', 'Q') ~ 'D',
          .default = 'E'
        )
      )
  }
  
  # Raça ----------------------------------------------------------------------#
  
  if(ano == 2009) {
    
    # Modificações somente de 2009
    base <- base %>%
      mutate(
        # 2009
        raca = case_when(
          raca %in% c('', '*', '.') ~ 'A',
          # inválido
          raca == 'A' ~ 'B',
          # branca
          raca == 'C' ~ 'C',
          # preta
          raca == 'B' ~ 'D',
          # parda
          raca == 'D' ~ 'E',
          # amarela
          raca == 'E' ~ 'F',
          # indígena
          .default = 'G'
        )
      )
  } else if (ano %in% c(2010, 2011, 2012, 2013, 2014, 2017, 2018, 2019)) {
    
    # Modificações somente de 2010 a 2014 e de 2017 a 2019
    base <- base %>%
      mutate(
        # 2010, 2011, 2012, 2013, 2014, 2017, 2018, 2019
        raca = case_when(
          raca == 0 ~ 'A',
          raca == 1 ~ 'B',
          raca == 2 ~ 'C',
          raca == 3 ~ 'D',
          raca == 4 ~ 'E',
          raca == 5 ~ 'F',
          .default = 'G'
        )
        
      )
    
  } else if(ano %in% c(2015,2016)) {
    
    # Modificações somente de 2015 e 2016
    base <- base %>%
      mutate(
        
        # 2015, 2016
        raca = case_when(
          raca %in% c(0,6) ~ 'A',
          raca == 1 ~ 'B',
          raca == 2 ~ 'C',
          raca == 3 ~ 'D',
          raca == 4 ~ 'E',
          raca == 5 ~ 'F',
          .default = 'G')
        
      )
    
  }
  
  # Escolaridade do pai -------------------------------------------------------#
  
  if (ano == 2010) {
    
    # Modificações somente de 2010
    base <- base %>%
      mutate(
        esc_pai = case_when(
          esc_pai == "H" ~ "A",
          esc_pai == "A" ~ "B",
          esc_pai == "B" ~ "C",
          esc_pai == "C" ~ "D",
          esc_pai == "D" ~ "E",
          esc_pai %in% c("E", "F", "G") ~ "F",
          esc_pai == "I" ~ "G",
          .default = "H"
        ),
        
        esc_mae = case_when(
          esc_mae == "H" ~ "A",
          esc_mae == "A" ~ "B",
          esc_mae == "B" ~ "C",
          esc_mae == "C" ~ "D",
          esc_mae == "D" ~ "E",
          esc_mae %in% c("E", "F", "G") ~ "F",
          esc_mae == "I" ~ "G",
          .default = "H"
        )
      )
  } else if (ano == 2009 | (ano >= 2009 & ano <= 2014)) {
    
    # Modificações somente de 2009 e 2011 a 2014
    base <- base %>%
      mutate(
        esc_pai = case_when(
          esc_pai == "A" ~ "A",
          # Não estudou
          esc_pai == "B" ~ "B",
          # EF 1º ciclo completo
          esc_pai %in% c("C", "D") ~ "C",
          # EF 2º ciclo completo
          esc_pai %in% c("E", "F") ~ "D",
          # EM completo
          esc_pai == "G" ~ "E",
          # ES completo
          esc_pai == "H" ~ "F",
          # Pós-graduação
          esc_pai == "I" ~ "G",
          # Não sabe
          .default = "H"
          # missing
        ),
        
        esc_mae = case_when(
          esc_mae == "A" ~ "A",
          # Não estudou
          esc_mae == "B" ~ "B",
          # EF 1º ciclo completo
          esc_mae %in% c("C", "D") ~ "C",
          # EF 2º ciclo completo
          esc_mae %in% c("E", "F") ~ "D",
          # EM completo
          esc_mae == "G" ~ "E",
          # ES completo
          esc_mae == "H" ~ "F",
          # Pós-graduação
          esc_mae == "I" ~ "G",
          # Não sabe
          .default = "H"
          # missing
        )
        
      )
  } else if (ano >= 2015 & ano <= 2019) {
    
    # Modificações somente de 2015 a 2019
    base <- base %>%
      mutate(
        esc_pai = case_when(
          esc_pai %in% c("A", "B") ~ "A",
          esc_pai == "C" ~ "B",
          esc_pai == "D" ~ "C",
          esc_pai == "E" ~ "D",
          esc_pai == "F" ~ "E",
          esc_pai == "G" ~ "F",
          esc_pai == "H" ~ "G",
          .default = "H"
        ),
        esc_mae = case_when(
          esc_mae %in% c("A", "B") ~ "A",
          esc_mae == "C" ~ "B",
          esc_mae == "D" ~ "C",
          esc_mae == "E" ~ "D",
          esc_mae == "F" ~ "E",
          esc_mae == "G" ~ "F",
          esc_mae == "H" ~ "G",
          .default = "H"
        ) 
        
        
        
      )
  } 
  
  # Dados Residenciais --------------------------------------------------------#
  
  if(ano < 2015) {
    
    base <- base %>% 
      mutate(
        #Domestica
        empr_dom = case_when(
          empr_dom == "D" ~ 0, #Sem doméstica
          empr_dom %in% c("A", "B", "C") ~ 1,
          .default        = NA  #Com uma ou mais
        ),
        
        #Banheiros
        n_banheiro = case_when(
          n_banheiro == "D" ~ 0,
          n_banheiro == "A" ~ 1,
          n_banheiro == "B" ~ 2,
          n_banheiro == "C" ~ 3, # 3 ou mais
          .default          = NA,
        ),
        
        #Quartos
        n_quartos = NA,
        
        #Carros
        n_carros = case_when(
          n_carros == "A" ~ 1,
          n_carros == "B" ~ 2,
          n_carros == "C" ~ 3,
          n_carros == "D" ~ 0,
          .default        = NA
        ),
        
        #Geladeira
        n_geladeira = case_when(
          n_geladeira == "A" ~ 1,
          n_geladeira == "B" ~ 2,
          n_geladeira == "C" ~ 3,
          n_geladeira == "D" ~ 0,
          .default           = NA
        ),
        
        #Celular
        n_celular = case_when(
          n_celular == "A" ~ 1,
          n_celular == "B" ~ 2,
          n_celular == "C" ~ 3,
          n_celular == "D" ~ 0,
          .default         = NA
        ),
        
        #PC
        pc = case_when(
          pc == "A" ~ 1,
          pc == "B" ~ 2,
          pc == "C" ~ 3,
          pc == "D" ~ 0,
          .default  = NA
        ),
        
        internet = case_when(
          internet == "D" ~ 0,
          internet %in% c("A", "B", "C") ~ 1,
          .default      = NA
        )
      )
    } else if (ano %in% c(2015:2019)) { #Anos in 2015-2019
    
    base <- base %>% 
      mutate(
        #Domestica
        empr_dom = case_when(
          empr_dom == "A" ~ 0, #Sem doméstica
          empr_dom %in% c("B", "C", "D") ~ 1,
          .default        = NA  #Com uma ou mais
        ),
        
        #Banheiros
        n_banheiro = case_when(
          n_banheiro == "A" ~ 0,
          n_banheiro == "B" ~ 1,
          n_banheiro == "C" ~ 2,
          n_banheiro == "D" ~ 3, 
          n_banheiro == "E" ~ 4, #4 ou mais
          .default          = NA
        ),
        
        #Quartos
        n_quartos = case_when(
          n_quartos == "A" ~ 0,
          n_quartos == "B" ~ 1,
          n_quartos == "C" ~ 2,
          n_quartos == "D" ~ 3, 
          n_quartos == "E" ~ 4, #4 ou mais
          .default         = NA
        ),
        
        #Carros
        n_carros = case_when(
          n_carros == "A" ~ 0,
          n_carros == "B" ~ 1,
          n_carros == "C" ~ 2,
          n_carros == "D" ~ 3, 
          n_carros == "E" ~ 4, #4 ou mais
          .default        = NA
        ),
        
        #Geladeira
        n_geladeira = case_when(
          n_geladeira == "A" ~ 0,
          n_geladeira == "B" ~ 1,
          n_geladeira == "C" ~ 2,
          n_geladeira == "D" ~ 3, 
          n_geladeira == "E" ~ 4, #4 ou mais
          .default           = NA
        ),
        
        #Celular
        n_celular = case_when(
          n_celular == "A" ~ 0,
          n_celular == "B" ~ 1,
          n_celular == "C" ~ 2,
          n_celular == "D" ~ 3, 
          n_celular == "E" ~ 4, #4 ou mais
          .default         = NA
        ),
        
        #PC
        pc = case_when(
          pc == "A" ~ 0,
          pc == "B" ~ 1,
          pc == "C" ~ 2,
          pc == "D" ~ 3, 
          pc == "E" ~ 4, #4 ou mais
          .default  = NA
        ),
        
        internet = case_when(
          internet == "B" ~ 0,
          internet == "A" ~ 1,
          .default        = NA
        )
      )
    
  }
  
  # Abstenção em redação ------------------------------------------------------#
  
  if (ano %in% c(2009,2011)) {
    
    # Modificações somente de 2009
    base <- base %>%
      mutate(
        abs_rd = case_when(
          status_rd == "B" ~ 0,
          # Entregou a redação em branco
          status_rd == "F" ~ 1,
          # Faltou à prova
          status_rd == "N" ~ 0,
          # Redação anulada
          status_rd == "P" ~ 0,
          # Presente à prova
          .default = NA
        )
      )
  } else if (ano == 2010) {
    
    # Modificações somente de 2010
    base <- base %>%
      mutate(
        abs_rd = case_when(
          status_rd == "B" ~ 0,
          # Entregou a redação em branco
          status_rd == "D" ~ 0,
          # Desconsiderada
          status_rd == "F" ~ 1,
          # Faltou à prova
          status_rd == "N" ~ 0,
          # Redação anulada
          status_rd == "P" ~ 0,
          # Presente à prova
          .default = NA
        )
      )
  } else if (ano == 2012) {
    
    # Modificações somente de 2012
    base <- base %>%
      mutate(
        abs_rd = case_when(
          status_rd == "B" ~ 0,
          # Entregou a redação em branco
          status_rd == "F" ~ 1,
          # Faltou à prova
          status_rd == "N" ~ 0,
          # Redação anulada
          status_rd == "P" ~ 0,
          # Presente à prova
          status_rd == "T" ~ 0,
          # Fuga ao tema
          status_rd == "I" ~ 0,
          # Texto insuficiente
          status_rd == "A" ~ 0,
          # Não atende ao tipo textual
          status_rd == "H" ~ 0,
          # Anulada - fere aos direitos humanos
          status_rd == "C" ~ 0,
          # Cópia de texto motivador
          .default = NA
        )
      )
    
  } else if (ano == 2013) {
    
    # Modificações somente de 2013 
    base <- base %>%
      mutate(
        # Não tem a opção 8
        abs_rd = case_when(
          status_rd == 1 ~ 0,
          # Entregou a redação em branco
          status_rd == 6 ~ 1,
          # Faltou à prova
          status_rd == 2 ~ 0,
          # Redação anulada
          status_rd == 7 ~ 0,
          # Presente à prova
          status_rd == 3 ~ 0,
          # Fuga ao tema
          status_rd == 5 ~ 0,
          # Texto insuficiente
          status_rd == 4 ~ 0,
          # Não atende ao tipo textual
          status_rd == 9 ~ 0,
          # Anulada - fere aos direitos humanos
          status_rd == 10 ~ 0,
          # Cópia de texto motivador
          .default = NA
        )
      )
    
  } else if (ano == 2014) {
    
    # Modificações somente de 2014 
    base <- base %>%
      mutate(
        # Não tem a opção 8
        abs_rd = case_when(
          status_rd == 1 ~ 0,
          # Entregou a redação em branco
          status_rd == 6 ~ 1,
          # Faltou à prova
          status_rd == 2 ~ 0,
          # Redação anulada
          status_rd == 1 ~ 0,
          # Presente à prova
          status_rd == 3 ~ 0,
          # Fuga ao tema
          status_rd == 5 ~ 0,
          # Texto insuficiente
          status_rd == 4 ~ 0,
          # Não atende ao tipo textual
          status_rd == 9 ~ 0,
          # Anulada - fere aos direitos humanos
          status_rd == 10 ~ 0,
          # Cópia de texto motivador
          status_rd == 11 ~ 0,
          # Parte do texto desconectada com o tema proposto
          .default = NA
        )
      )
    
  } else if (ano == 2015) {
    
    # Modificações somente de 2015 
    # base <- base %>%
    #   mutate(
    #     # Não tem a opção de ausência
    #     abs_rd = case_when(
    #       status_rd == 4 ~ 0,
    #       # Entregou a redação em branco
    #       status_rd == 2 ~ 0,
    #       # Redação anulada
    #       status_rd == 1 ~ 0,
    #       # Presente à prova
    #       status_rd == 6 ~ 0,
    #       # Fuga ao tema
    #       status_rd == 8 ~ 0,
    #       # Texto insuficiente
    #       status_rd == 7 ~ 0,
    #       # Não atende ao tipo textual
    #       status_rd == 5 ~ 0,
    #       # Anulada - fere aos direitos humanos
    #       status_rd == 3 ~ 0,
    #       # Cópia de texto motivador
    #       status_rd == 9 ~ 0,
    #       # Parte do texto desconectada com o tema proposto
    #       status_rd == 98 ~ 0,
    #       # Não atende ao item 2.2.5 do edital: Dispor de documentos comprobatórios da condição que motiva a solicitação de atendimento ESPECIALIZADO e/ou ESPECÍFICO.
    #       .default = 1
    #     )
    #   )
    
    # Não tem a opção de ausência
    base <- base %>%
      mutate(
        abs_rd = ifelse(pres_lc == 0 & pres_mt == 0, 1, 0)
      )
    
  } else if (ano == 2016) {
    
    # # Modificações somente de 2016 
    # base <- base %>%
    #   mutate(
    #     # Não tem a opção de ausência
    #     abs_rd = case_when(
    #       status_rd == 4 ~ 0,
    #       # Entregou a redação em branco
    #       status_rd == 2 ~ 0,
    #       # Redação anulada
    #       status_rd == 1 ~ 0,
    #       # Presente à prova
    #       status_rd == 6 ~ 0,
    #       # Fuga ao tema
    #       status_rd == 8 ~ 0,
    #       # Texto insuficiente
    #       status_rd == 7 ~ 0,
    #       # Não atende ao tipo textual
    #       status_rd == 5 ~ 0,
    #       # Anulada - fere aos direitos humanos
    #       status_rd == 3 ~ 0,
    #       # Cópia de texto motivador
    #       status_rd == 9 ~ 0,
    #       # Parte do texto desconectada com o tema proposto
    #       .default = 1
    #     )
    #   )
    
    # Não tem a opção de ausência
    base <- base %>%
      mutate(
        abs_rd = ifelse(pres_lc == 0 & pres_mt == 0, 1, 0)
      )
    
  } else if(ano %in% c(2017, 2018, 2019)) {
    
    # # Modificações somente de 2017 a 2019 
    # base <- base %>%
    #   mutate(
    #     
    #     # 2017, 2018, 2019 - não tem a opção 5 e nem a opção de ausência
    #     abs_rd = case_when(
    #       status_rd == 4 ~ 0,                      # Entregou a redação em branco
    #       status_rd == 2 ~ 0,                      # Redação anulada
    #       status_rd == 1 ~ 0,                      # Presente à prova
    #       status_rd == 6 ~ 0,                      # Fuga ao tema
    #       status_rd == 8 ~ 0,                      # Texto insuficiente
    #       status_rd == 7 ~ 0,                      # Não atende ao tipo textual
    #       status_rd == 3 ~ 0,                     # Cópia de texto motivador
    #       status_rd == 9 ~ 0,                     # Parte do texto desconectada com o tema proposto
    #       .default = 1
    #     )
    #     
    #   )
    
    # Não tem a opção de ausência
    base <- base %>%
      mutate(
        abs_rd = ifelse(pres_lc == 0 & pres_ch == 0, 1, 0)
      )
  }
  
  
  # Abstenção nas provas objetivas --------------------------------------------#
  
  # Todos os anos    
  base <- base %>%
    mutate(
      
      abs_cn = case_when(
        pres_cn == 0 ~ 1,                          # Faltou à prova
        pres_cn == 1 ~ 0,                          # Presente na prova
        pres_cn == 2 ~ 0,                          # Eliminado na prova
        .default = NA
      ),
      abs_ch = case_when(
        pres_ch == 0 ~ 1,                          # Faltou à prova
        pres_ch == 1 ~ 0,                          # Presente na prova
        pres_ch == 2 ~ 0,                          # Eliminado na prova
        .default = NA
      ),
      abs_lc = case_when(
        pres_lc == 0 ~ 1,                          # Faltou à prova
        pres_lc == 1 ~ 0,                          # Presente na prova
        pres_lc == 2 ~ 0,                          # Eliminado na prova
        .default = NA
      ),
      abs_mt = case_when(
        pres_mt == 0 ~ 1,                          # Faltou à prova
        pres_mt == 1 ~ 0,                          # Presente na prova
        pres_mt == 2 ~ 0,                          # Eliminado na prova
        .default = NA
      )
      
    )
  
  # Sexo ----------------------------------------------------------------------#
  if(ano <= 2010 | ano >= 2013){
    
    base <- base %>%
      mutate(
        mas = ifelse(sexo == "M", 0, 1)
      )
    
  } else if (ano %in% c(2011,2012)) {
    
    base <- base %>%
      mutate(
        mas = sexo
      )
  }
  
  
  
  
  # Filtros e novas variáveis -------------------------------------------------#
  
  # Filtro geral
  base <- base %>%
    filter(
      #conclusao == 2,
      #idade > 18
    ) %>%
    mutate(hv = ifelse(mun_prova %/% 100000 > 30, 1, 0)) %>%
    mutate(
      urb = ifelse(urb_rur == 1, 1, 0),
      priv = ifelse(dep_adm == 4, 1, 0),
      media = (cn + ch + lc + mt + rd) / 5,
      ano = ano
    ) %>%
    relocate(media, .after = rd) %>%
    relocate(ano, .before = idade) %>%
    relocate(mun_escola, .after = mun_prova)
  
  return(base)
  
}


filtros <- function(base,filtro) {
  
  # Filtro para base de regressões de notas - sem missing e valores inválidos
  if (filtro == "notas") {
    
    base <- base %>%
      filter(
        !is.na(cn) &
          !is.na(ch) &
          !is.na(lc) &
          !is.na(mt) &
          !is.na(rd) &   
          !is.na(dep_adm) &
          # !is.na(urb_rur) &
          # !is.na(sexo) &
          # !is.na(raca) &
          # !is.na(pessoas_dom) &
          # !is.na(renda_dom) &
          # !is.na(esc_pai) &
          # esc_pai != "H" &
          # raca != "G" &
          # renda_dom != "E" &
          # pessoas_dom != "D" &
          !is.na(mun_escola) &!is.na(func_esc)
      )
    
  } else if(filtro == "abs") {
    
    # Filtro para base de regressões de abstenções
    if("treineiro" %in% colnames(enem)){
      
      base_a <- base %>%
        select(-c(cn,ch,lc,mt,rd,sexo,raca,pessoas_dom,renda_dom,
                  esc_pai,mun_escola,func_esc,rd1,rd2,rd3,rd4,rd5,co_cn,co_ch,co_lc,
                  co_mt,pres_cn,pres_ch,pres_lc,pres_mt,status_rd,treineiro))
      
    } else {
      
      base_a <- base %>%
        select(-c(cn,ch,lc,mt,rd,sexo,raca,pessoas_dom,renda_dom,
                  esc_pai,mun_escola,func_esc,rd1,rd2,rd3,rd4,rd5,co_cn,co_ch,co_lc,
                  co_mt,pres_cn,pres_ch,pres_lc,pres_mt,status_rd))
      
    }
    
    
  }
  
  return(base)
  
}

# ---------------------------------------------------------------------------- #
###2.1.1 Labels ----
# ---------------------------------------------------------------------------- #

dlist <- c("CN","CH","LC","MT")
vlist2013 <- c(
  "NU_INSCRICAO",
  paste0(rep("TX_RESPOSTAS_", times= length(dlist)), dlist),
  paste0(rep("ID_PROVA_", times= length(dlist)), dlist),
  paste0(rep("GABARITO_", times= length(dlist)), dlist),
  "TP_LINGUA"
) %>% tolower()
vlist2015 <- c(
  "NU_INSCRICAO",
  paste0(rep("TX_RESPOSTAS_", times= length(dlist)), dlist),
  paste0(rep("CO_PROVA_", times= length(dlist)), dlist),
  paste0(rep("TX_GABARITO_", times= length(dlist)), dlist),
  "TP_LINGUA"
) %>% tolower()
vlist2016 <- c(
  "NU_INSCRICAO",
  paste0(rep("TX_RESPOSTAS_", times= length(dlist)), dlist),
  paste0(rep("CO_PROVA_", times= length(dlist)), dlist),
  paste0(rep("TX_GABARITO_", times= length(dlist)), dlist),
  "TP_LINGUA"
) 
vnames <- c(
  "id_enem",
  paste0(rep("tx_respostas_", times = length(dlist)),tolower(dlist)),
  paste0(rep("co_prova_", times = length(dlist)),tolower(dlist)),
  paste0(rep("tx_gabarito_", times = length(dlist)),tolower(dlist)),
  "lingua"
)
vlist_item_df <- data.frame(
  vl2013 = vlist2013,
  vl2014 = vlist2013,
  vl2015 = vlist2015,
  vl2016 = vlist2016,
  vl2017 = vlist2016,
  vl2018 = vlist2016,
  vl2019 = vlist2016,
  vnames = vnames
)
rm(vlist2013,vlist2015,vlist2016,vnames,dlist)

# ---------------------------------------------------------------------------- #
## 2.2 Begin -----
### 2.2.1 Loop test year ----
# ---------------------------------------------------------------------------- #
for (ano in 2009:2019) {
  
  temp <-
    fread(
      paste0(
        "Z:/Arquivos IFB/Enem/Parâmetros/itens_prova_",
        ano,
        ".csv"
      )
    ) %>% 
    rename_all(tolower) %>%
    mutate(ano = ano) %>%
    filter(in_item_aban == 0) %>%
    select(-in_item_aban, -tx_motivo_aban)
  
  
  if(ano == 2009) {
    itens_prova <- temp
  } else {
    itens_prova <- itens_prova %>% bind_rows(temp)
  }
  
  rm(temp)
  rm(ano)
}

itens_prova <- itens_prova %>%
  group_by(sg_area, co_prova) %>%
  mutate(
    pos_min = min(co_posicao),
    co_posicao = co_posicao - pos_min + 1
  ) %>%
  ungroup() %>%
  filter(is.na(tp_lingua)) %>%
  select(-pos_min,-tp_lingua,in_item_adaptado)


setDT(itens_prova)
medians <-
  itens_prova[, .(
    a_md = median(nu_param_a, na.rm = T),
    b_md = median(nu_param_b, na.rm = T),
    c_md = median(nu_param_c, na.rm = T)
  ), by = .(sg_area, ano, co_prova)]

itens_medias <-
  itens_prova[, .(
    a_md = mean(nu_param_a, na.rm = T),
    b_md = mean(nu_param_b, na.rm = T),
    c_md = mean(nu_param_c, na.rm = T)
  ), by = .(sg_area, ano)]

# CHECKLIST:
# 2009  OK
# 2010  OK
# 2011  OK
# 2012  OK
# 2013  OK
# 2014  OK
# 2015  OK
# 2016  OK
# 2017  OK
# 2018  OK
# 2019  OK

## a) 2009 ---

# Variáveis correspondentes:
# i.   Q15: qtd pessoas no domicílio
# ii.  Q21: renda domiciliar
# iii. Q3:  cor/raça
# iv.  Q17: escolaridade do pai

# Leitura da base de dados


# ---------------------------------------------------------------------------- #
#### 2.2.1.1 Var. List ----
# ---------------------------------------------------------------------------- #
# Base de dados com listas de variáveis e labels
vlist_df <- data.frame(
  vl2013 = c(
    "nu_inscricao",
    "idade",
    "st_conclusao",
    "id_dependencia_adm_esc",
    "id_localizacao_esc",
    "cod_municipio_prova",
    "cod_municipio_residencia",
    "uf_prova",
    "nota_cn",
    "nota_ch",
    "nota_lc",
    "nota_mt",
    "nu_nota_redacao",
    "q004",
    "q003",
    "tp_sexo",
    "tp_cor_raca",
    "sit_func_esc",
    "q001",
    "q002",
    "q005",
    "q006",
    "q020", #Empregada dom.
    "q021", #Banheiro
    "q009", #Quartos NA, in reality radio
    "q011", #Carro
    "q013", #Geladeira
    "q016", #Celular
    "q010", #PC
    "q017", #Internet
    "cod_municipio_esc",
    "nu_nota_comp1",
    "nu_nota_comp2",
    "nu_nota_comp3",
    "nu_nota_comp4",
    "nu_nota_comp5",
    "id_prova_cn",
    "id_prova_ch",
    "id_prova_lc",
    "id_prova_mt",
    "in_presenca_cn",
    "in_presenca_ch",
    "in_presenca_lc",
    "in_presenca_mt",
    "in_status_redacao",
    ""
  ),
  vl2014 = c("nu_inscricao",
             "idade",
             "st_conclusao",
             "id_dependencia_adm_esc",
             "id_localizacao_esc",
             "cod_municipio_prova",
             "cod_municipio_residencia",
             "uf_prova",
             "nota_cn",
             "nota_ch",
             "nota_lc",
             "nota_mt",
             "nu_nota_redacao",
             "q004",
             "q003",
             "tp_sexo",
             "tp_cor_raca",
             "sit_func_esc",
             "q001",
             "q002",
             "q005",
             "q006",
             "q020", #Empregada dom.
             "q021", #Banheiro
             "q009", #Quartos NA, in reality radio
             "q011", #Carro
             "q013", #Geladeira
             "q016", #Celular
             "q010", #PC
             "q017", #Internet
             "cod_municipio_esc",
             "nu_nota_comp1",
             "nu_nota_comp2",
             "nu_nota_comp3",
             "nu_nota_comp4",
             "nu_nota_comp5",
             "id_prova_cn",
             "id_prova_ch",
             "id_prova_lc",
             "id_prova_mt",
             "in_presenca_cn",
             "in_presenca_ch",
             "in_presenca_lc",
             "in_presenca_mt",
             "in_status_redacao",
             ""),
  vl2015 = c(
    "nu_inscricao",
    "nu_idade",
    "tp_st_conclusao",
    "tp_dependencia_adm_esc",
    "tp_localizacao_esc",
    "co_municipio_prova",
    "co_municipio_residencia",
    "sg_uf_prova",
    "nu_nota_cn",
    "nu_nota_ch",
    "nu_nota_lc",
    "nu_nota_mt",
    "nu_nota_redacao",
    "q005",
    "q006",
    "tp_sexo",
    "tp_cor_raca",
    "tp_sit_func_esc",
    "q001",
    "q002",
    "q003",
    "q004",
    "q007", #Empregada dom.
    "q008", #Banheiro
    "q009", #Quartos
    "q010", #Carro
    "q012", #Geladeira
    "q022", #Celular
    "q024", #PC
    "q025", #Internet
    "co_municipio_esc",
    "nu_nota_comp1",
    "nu_nota_comp2",
    "nu_nota_comp3",
    "nu_nota_comp4",
    "nu_nota_comp5",
    "co_prova_cn",
    "co_prova_ch",
    "co_prova_lc",
    "co_prova_mt",
    "tp_presenca_cn",
    "tp_presenca_ch",
    "tp_presenca_lc",
    "tp_presenca_mt",
    "tp_status_redacao",
    "in_treineiro"
  ),
  vl2016 = c(
    "NU_INSCRICAO",
    "NU_IDADE",
    "TP_ST_CONCLUSAO",
    "TP_DEPENDENCIA_ADM_ESC",
    "TP_LOCALIZACAO_ESC",
    "CO_MUNICIPIO_PROVA",
    "CO_MUNICIPIO_RESIDENCIA",
    "SG_UF_PROVA",
    "NU_NOTA_CN",
    "NU_NOTA_CH",
    "NU_NOTA_LC",
    "NU_NOTA_MT",
    "NU_NOTA_REDACAO",
    "Q005",
    "Q006",
    "TP_SEXO",
    "TP_COR_RACA",
    "TP_SIT_FUNC_ESC",
    "Q001",
    "Q002",
    "Q003",
    "Q004",
    "Q007", #Empregada dom.
    "Q008", #Banheiro
    "Q009", #Quartos
    "Q010", #Carro
    "Q012", #Geladeira
    "Q022", #Celular
    "Q024", #PC
    "Q025", #Internet
    "CO_MUNICIPIO_ESC",
    "NU_NOTA_COMP1",
    "NU_NOTA_COMP2",
    "NU_NOTA_COMP3",
    "NU_NOTA_COMP4",
    "NU_NOTA_COMP5",
    "CO_PROVA_CN",
    "CO_PROVA_CH",
    "CO_PROVA_LC",
    "CO_PROVA_MT",
    "TP_PRESENCA_CN",
    "TP_PRESENCA_CH",
    "TP_PRESENCA_LC",
    "TP_PRESENCA_MT",
    "TP_STATUS_REDACAO",
    "IN_TREINEIRO"
  ),
  vl2017 = c(
    "NU_INSCRICAO",
    "NU_IDADE",
    "TP_ST_CONCLUSAO",
    "TP_DEPENDENCIA_ADM_ESC",
    "TP_LOCALIZACAO_ESC",
    "CO_MUNICIPIO_PROVA",
    "CO_MUNICIPIO_RESIDENCIA",
    "SG_UF_PROVA",
    "NU_NOTA_CN",
    "NU_NOTA_CH",
    "NU_NOTA_LC",
    "NU_NOTA_MT",
    "NU_NOTA_REDACAO",
    "Q005",
    "Q006",
    "TP_SEXO",
    "TP_COR_RACA",
    "TP_SIT_FUNC_ESC",
    "Q001",
    "Q002",
    "Q003",
    "Q004",
    "Q007", #Empregada dom.
    "Q008", #Banheiro
    "Q009", #Quartos
    "Q010", #Carro
    "Q012", #Geladeira
    "Q022", #Celular
    "Q024", #PC
    "Q025", #Internet
    "CO_MUNICIPIO_ESC",
    "NU_NOTA_COMP1",
    "NU_NOTA_COMP2",
    "NU_NOTA_COMP3",
    "NU_NOTA_COMP4",
    "NU_NOTA_COMP5",
    "CO_PROVA_CN",
    "CO_PROVA_CH",
    "CO_PROVA_LC",
    "CO_PROVA_MT",
    "TP_PRESENCA_CN",
    "TP_PRESENCA_CH",
    "TP_PRESENCA_LC",
    "TP_PRESENCA_MT",
    "TP_STATUS_REDACAO",
    "IN_TREINEIRO"
  ),
  vl2018 = c(
    "NU_INSCRICAO",
    "NU_IDADE",
    "TP_ST_CONCLUSAO",
    "TP_DEPENDENCIA_ADM_ESC",
    "TP_LOCALIZACAO_ESC",
    "CO_MUNICIPIO_PROVA",
    "CO_MUNICIPIO_RESIDENCIA",
    "SG_UF_PROVA",
    "NU_NOTA_CN",
    "NU_NOTA_CH",
    "NU_NOTA_LC",
    "NU_NOTA_MT",
    "NU_NOTA_REDACAO",
    "Q005",
    "Q006",
    "TP_SEXO",
    "TP_COR_RACA",
    "TP_SIT_FUNC_ESC",
    "Q001",
    "Q002",
    "Q003",
    "Q004",
    "Q007", #Empregada dom.
    "Q008", #Banheiro
    "Q009", #Quartos
    "Q010", #Carro
    "Q012", #Geladeira
    "Q022", #Celular
    "Q024", #PC
    "Q025", #Internet
    "CO_MUNICIPIO_ESC",
    "NU_NOTA_COMP1",
    "NU_NOTA_COMP2",
    "NU_NOTA_COMP3",
    "NU_NOTA_COMP4",
    "NU_NOTA_COMP5",
    "CO_PROVA_CN",
    "CO_PROVA_CH",
    "CO_PROVA_LC",
    "CO_PROVA_MT",
    "TP_PRESENCA_CN",
    "TP_PRESENCA_CH",
    "TP_PRESENCA_LC",
    "TP_PRESENCA_MT",
    "TP_STATUS_REDACAO",
    "IN_TREINEIRO"
  ),
  vl2019 = c(
    "NU_INSCRICAO",
    "NU_IDADE",
    "TP_ST_CONCLUSAO",
    "TP_DEPENDENCIA_ADM_ESC",
    "TP_LOCALIZACAO_ESC",
    "CO_MUNICIPIO_PROVA",
    "CO_MUNICIPIO_RESIDENCIA",
    "SG_UF_PROVA",
    "NU_NOTA_CN",
    "NU_NOTA_CH",
    "NU_NOTA_LC",
    "NU_NOTA_MT",
    "NU_NOTA_REDACAO",
    "Q005",
    "Q006",
    "TP_SEXO",
    "TP_COR_RACA",
    "TP_SIT_FUNC_ESC",
    "Q001",
    "Q002",
    "Q003",
    "Q004",
    "Q007", #Empregada dom.
    "Q008", #Banheiro
    "Q009", #Quartos
    "Q010", #Carro
    "Q012", #Geladeira
    "Q022", #Celular
    "Q024", #PC
    "Q025", #Internet
    "CO_MUNICIPIO_ESC",
    "NU_NOTA_COMP1",
    "NU_NOTA_COMP2",
    "NU_NOTA_COMP3",
    "NU_NOTA_COMP4",
    "NU_NOTA_COMP5",
    "CO_PROVA_CN",
    "CO_PROVA_CH",
    "CO_PROVA_LC",
    "CO_PROVA_MT",
    "TP_PRESENCA_CN",
    "TP_PRESENCA_CH",
    "TP_PRESENCA_LC",
    "TP_PRESENCA_MT",
    "TP_STATUS_REDACAO",
    "IN_TREINEIRO"
  ),
  vnames = c("id_enem",
             "idade",
             "conclusao",
             "dep_adm",
             "urb_rur",
             "mun_prova",
             "mun_res",
             "uf",
             "cn",
             "ch",
             "lc",
             "mt",
             "rd",
             "pessoas_dom",
             "renda_dom",
             "sexo",
             "raca",
             "func_esc",
             "esc_pai",
             "esc_mae",
             "emp_pai",
             "emp_mae",
             "empr_dom",
             "n_banheiro",
             "n_quartos",
             "n_carros",
             "n_geladeira",
             "n_celular",
             "pc",
             "internet",
             "mun_escola",
             "rd1",
             "rd2",
             "rd3",
             "rd4",
             "rd5",
             "co_cn",
             "co_ch",
             "co_lc",
             "co_mt",
             "pres_cn",
             "pres_ch",
             "pres_lc",
             "pres_mt",
             "status_rd",
             "treineiro"
  )
  
)



flist <- c(
  # "Z:/Arquivos IFB/Enem/2009/Dados/MICRODADOS_ENEM_2009.csv",
  # "Z:/Arquivos IFB/Enem/2010/Dados/MICRODADOS_ENEM_2010.csv",
  # "Z:/Arquivos IFB/Enem/2011/DADOS/MICRODADOS_ENEM_2011.dta",
  # "Z:/Arquivos IFB/Enem/2012/DADOS/MICRODADOS_ENEM_2012.dta",
  "Z:/Arquivos IFB/Enem/2013/microdados_enem2013/DADOS/MICRODADOS_ENEM_2013.dta",
  "Z:/Arquivos IFB/Enem/2014/microdados_enem2014/DADOS/MICRODADOS_ENEM_2014.dta",
  "Z:/Arquivos IFB/Enem/2015/DADOS/MICRODADOS_ENEM_2015.dta",
  "Z:/Arquivos IFB/Enem/2016/Microdados_enem_2016/DADOS/microdados_enem_2016.csv",
  "Z:/Arquivos IFB/Enem/2017/Microdados Enem 2017/DADOS/MICRODADOS_ENEM_2017.csv",
  "Z:/Arquivos IFB/Enem/2018/DADOS/MICRODADOS_ENEM_2018.csv",
  "Z:/Arquivos IFB/Enem/2019/DADOS/MICRODADOS_ENEM_2019.csv"
)

# ---------------------------------------------------------------------------- #
### 2.2.2 Loop grades + absence ----
# ---------------------------------------------------------------------------- #

for(i in 2013:2019){
  
  message(paste0("Ano: ",i))
  ini <- Sys.time()
  
  j <- i - 2012
  delta <- ifelse(i <= 2014,1,0)
  vlist <- vlist_df[1:(nrow(vlist_df) - delta),j]
  cnames <- vlist_df[1:(nrow(vlist_df) - delta),ncol(vlist_df)]
  rm(delta)
  
  if (i <= 2010 | i >= 2016){
    
    enem <-
      fread(file = flist[j],
            sep = ";",
            select = vlist)
    
  } else if(i >= 2011 & i <= 2015){
    enem <- read_dta(file = flist[j], col_select = vlist)
  }
  
  message("Opened database. Now organizing...")
  
  enem <-
    org_data(
      base = enem,
      ano = i,
      vlist = vlist,
      cnames = cnames
    )
  
  message("Saving data")
  enem_notas <- filtros(base = enem, filtro = "notas")
  enem_abs <- filtros(base = enem, filtro = "abs")
  
  saveRDS(enem_notas, file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/enem_notas_",i,"_v4.RDS"))
  message("Saved scores data")
  saveRDS(enem_abs, file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/enem_abs_",i,"_v4.RDS"))
  
  message("Saved absent data. Finished for year: ",i)
  
  fim <- Sys.time()
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  hours <- round(mins /60)
  
  message("---------------------------------------------")
  message("Elapsed time: ", hours, " hours, ",mins," mins, and ", secs, " s")
  message("---------------------------------------------")
  
  rm(enem,i,j,cnames,vlist,enem_notas,enem_abs, ini,delta, fim, mins, secs, hours)
}

#------------------------------------------------------------------------------#
# Base de itens do ENEM
#------------------------------------------------------------------------------#
### 2.2.3 Loop Topics ----
# ---------------------------------------------------------------------------- #
# Lista de disciplinas
dlist <- c("cn","ch","lc","mt")

# Loop nos anos
for(ano in 2013:2019){
  
  message(paste0("Ano: ",ano))
  ini <- Sys.time()
  
  # Base de IDs para selecionar alunos
  enem <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/enem_notas_",ano,"_v4.RDS")) 
  id <- enem %>% select(id_enem)
  rm(enem)
  
  # Especificações da base de dados
  j <- ano - 2012
  delta <- ifelse(ano == 2019, 1, 0)
  vlist <- vlist_item_df[1:(nrow(vlist_item_df) - delta),j]
  cnames <- vlist_item_df[1:(nrow(vlist_item_df) - delta),ncol(vlist_item_df)]
  
  # Bases de dados em CSV e em DTA, dependendo do ano
  print(paste0("Abrindo a base: ",ano))
  if (ano <= 2010 | ano >= 2016){
    
    item <-
      fread(file = flist[j],
            sep = ";",
            select = vlist)
    
  } else if(ano >= 2011 & ano <= 2015){
    item <- read_dta(file = flist[j], col_select = vlist)
  }
  
  fim <- Sys.time()
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  hours <- round(mins /60)
  
  message("---------------------------------------------")
  message("Elapsed time for data opening: ",hours, " hours, ",mins," mins, and ", secs, " s")
  message("---------------------------------------------")
  rm(fim, delta, mins, secs, hours)
  
  # Organização da base de dados
  item <- item %>% relocate(all_of(vlist))
  colnames(item) <- cnames
  rm(vlist,cnames,delta)
  
  # Reorganizando a base (reshape)
  if (ano == 2009){
    vlist <- "id_enem"
  } else {
    vlist <- c("id_enem","lingua")
  }
  
  item <- item %>%
    right_join(id, by = "id_enem") %>%
    setDT() %>%
    melt(
      id.vars = vlist,
      measure.vars = list(
        paste0("tx_respostas_", dlist),
        paste0("co_prova_", dlist),
        paste0("tx_gabarito_", dlist)
      ),
      value.name = c("tx_respostas", "co_prova", "tx_gabarito")
    ) %>%
    mutate(
      sg_area = case_when(
        variable == "1" ~ toupper(dlist[1]),
        variable == "2" ~ toupper(dlist[2]),
        variable == "3" ~ toupper(dlist[3]),
        variable == "4" ~ toupper(dlist[4])
      )
    )
  
  rm(id,vlist)
  
  # Loop nas áreas
  for(i in 1:length(dlist)){
    
    print(paste0("Área: ",dlist[i]))
    delta_t <- Sys.time() - ini
    print(delta_t)
    rm(delta_t)
    
    # Selecionando área para DF local
    temp <- item[sg_area == toupper(dlist[i]),]
    
    # Excluindo área do DF geral de itens
    item <- item[sg_area != toupper(dlist[i]),]
    
    # Língua estrangeira
    if(dlist[i] == "lc" & ano >= 2010) {
      
      if(ano == 2011) {
        temp <-
          temp[, 
               tx_gabarito := substr(tx_gabarito, 6, 45)]
      } else {
        temp <-
          temp[, 
               tx_gabarito := substr(tx_gabarito, 11, 50)]
      }
      
      
      temp <-
        temp[, 
             tx_respostas := substr(tx_respostas, 6, 45)]
      
      temp <- temp[, lingua := NULL]
      print("ok")
    }
    
    temp <- temp[, nc := nchar(tx_respostas)]
    nc <- temp %>% summarise(max = max(nc)) %>% as.numeric()
    
    temp <- temp[, paste0("resp", c(1:nc)) := tstrsplit(tx_respostas, "", fixed = TRUE)]
    temp <- temp[, paste0("gab", c(1:nc)) := tstrsplit(tx_gabarito, "", fixed = TRUE)]
    temp <- temp[, c("tx_respostas","tx_gabarito") := NULL]
    
    # Reorganizando o Df local de itens
    temp <- temp %>%
      melt(id.vars = c("id_enem","co_prova","sg_area"),
           measure.vars = list(
             paste0("resp", c(1:nc)),
             paste0("gab", c(1:nc))
           ),
           value.name = c("tx_respostas","tx_gabarito")
      ) %>%
      mutate(co_posicao = as.numeric(as.character(variable))) %>%
      select(-variable)
    
    rm(nc)
    
    # Dummies de acerto, resposta em branco e dupla resposta
    temp <- temp[, acerto := fifelse(tx_respostas == tx_gabarito, 1, 0)]
    temp <- temp[, branco := fifelse(tx_respostas == ".", 1, 0)]
    temp <- temp[, dupla := fifelse(tx_respostas == "*", 1, 0)]
    
    # Variável de ano
    temp <- temp[, ano := ano]
    
    print(paste0("Juntando bases de parâmetros"))
    delta_t <- Sys.time() - ini
    print(delta_t)
    rm(delta_t)
    
    # Juntando base de itens com base de medianas de parâmetros
    temp <- temp %>% merge(medians, by = c("sg_area","ano","co_prova"))
    
    # Juntando base de itens com base de parâmetros
    temp <- temp %>% merge(itens_prova, by = c("ano","co_posicao","co_prova","sg_area","tx_gabarito"))
    #temp %>% setorder(cols = "id_enem","sg_area","co_posicao") %>% slice(1:100) %>% View()
    
    # Excluindo itens sem parâmetros
    #temp <- temp[!is.na(nu_param_a),]
    
    # Posições mínima e máxima
    temp <- temp[, c("pmin","pmax") := 
                   .(min(co_posicao),
                     max(co_posicao))]
    
    #temp %>% setorder(cols = "id_enem","sg_area") %>% slice(1:100) %>% View()
    
    # Construindo dummies de acertos
    # vnames <- c(paste0("acertos_",rep(c("a","b","c"), each = 2),c("l","h")),
    #             paste0("acertos_",rep(c("ini","fim"), times = 2),rep(c("5","10"), each = 2)))
    
    temp <- temp[, c(paste0("acerto_",rep(c("a","b","c"), each = 2),c("l","h")),
                     paste0("acerto_",rep(c("ini","fim"), times = 2),rep(c("5","10"), each = 2))) :=
                   .(
                     fifelse(nu_param_a < a_md, acerto, NA_real_),
                     fifelse(nu_param_a >= a_md, acerto, NA_real_),
                     fifelse(nu_param_b < b_md, acerto, NA_real_),
                     fifelse(nu_param_b >= b_md, acerto, NA_real_),
                     fifelse(nu_param_c < c_md, acerto, NA_real_),
                     fifelse(nu_param_c >= c_md, acerto, NA_real_),
                     fifelse(co_posicao <= pmin + 4, acerto, NA_real_),
                     fifelse(co_posicao >= pmax - 4, acerto, NA_real_),
                     fifelse(co_posicao <= pmin + 9, acerto, NA_real_),
                     fifelse(co_posicao >= pmax - 9, acerto, NA_real_)
                   )]
    # rm(vnames)
    
    # Seleção de variáveis
    idvlist <- c("id_enem","sg_area","co_prova")
    vlist <- c(
      "acerto",
      "acerto_al",
      "acerto_ah",
      "acerto_bl",
      "acerto_bh",
      "acerto_cl",
      "acerto_ch",
      "acerto_ini5",
      "acerto_fim5",
      "acerto_ini10",
      "acerto_fim10",
      "branco",
      "dupla"
    )
    # temp <- temp[, .SD, .SDcols = c(idvlist,vlist)]
    # temp <- temp %>% select(idvlist,vlist)
    
    # Número de observações
    # temp_obs <- temp[, lapply(.SD, function(x) sum(!is.na(x))),
    #              by = .(id_enem, sg_area),
    #              .SDcols = vlist]
    
    # colnames(temp_obs) <- paste0("n_",vlist)
    
    print("Agregação")
    
    temp <- temp[, lapply(.SD, mean, na.rm = T),
                 by = .(id_enem, sg_area),
                 .SDcols = vlist]
    
    # temp <- temp %>% merge(temp_obs, by = idvlist)
    
    rm(idvlist, vlist)
    #temp2 %>% setorder(cols = "id_enem","sg_area") %>% slice(1:100) %>% View()
    
    
    # Armazenado base de dados
    if(i == 1){
      enem_ace <- temp
    } else {
      enem_ace <- enem_ace %>% bind_rows(temp)
    }
    rm(i, temp)
    gc()
    
  } # fim do loop nas disciplinas (i)
  
  message("----------------------------")
  message("Ended topic loop for: ", item," in ",ano)
  
  fim <- Sys.time()
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  hours <- round(mins /60)
  message("Elapsed time: ",hours, " hours, ",mins," mins, and ", secs, " s")
  message("---------------------------------------------")
  
  rm(fim, delta, mins, secs, hours)
  
  saveRDS(enem_ace, file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/enem_ace_",ano,"_v4.RDS"))
  rm(enem_ace, ini,item,j)
  
  rm(ano)
  
} # fim do loop nos anos (ano)





# ---------------------------------------------------------------------------- #
## 2.3 Joined data ----
# ---------------------------------------------------------------------------- #

# PIB
pib <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/pib.RDS") %>%
  mutate(codmun = as.integer(codmun)) %>%
  rename(mun_prova = codmun)

# Base de distâncias
mun_hv <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/mun_hv.RDS")

# Coordenadas
coordenadas <- mun_hv$centroid %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(
    lon = X,
    lat = Y
  )

mun_hv <- mun_hv %>%
  bind_cols(coordenadas) %>%
  st_drop_geometry() %>%
  select(co_municipio, lon, lat, dist_hv_border, seg)
rm(coordenadas)

# Bases de dados de todos os anos
for(ano in 2013:2019){
  
  ini <- Sys.time()
  message("Starting for: ", ano)
  
  temp <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/enem_notas_",ano,"_v4.RDS")) %>%
    filtros(filtro = "notas")
  temp2 <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/enem_ace_",ano,"_v4.RDS"))%>%
    mutate(
      sg_area = tolower(sg_area)
    ) %>%
    dcast(id_enem ~ sg_area,
          value.var = c(
            "acerto",
            paste0(
              "acerto_",
              rep(c("a","b","c"), each = 2), rep(c("l","h"), times = 3)
            ),
            paste0(
              "acerto_",
              rep(c("ini","fim"), each = 2), rep(c("5","10"), times = 2)
            ),
            "branco","dupla"
          )
    )
  
  temp <- temp %>% merge(temp2, by = "id_enem", all.y = F,all.x = F)
  rm(temp2)
  
  
  
  message("Creating the variables")
  temp <- temp %>%
    select(-sexo,-status_rd) %>%
    left_join(mun_hv,
              by = c(
                "mun_prova" = "co_municipio"
              ))  %>%
    mutate(
      dist_hv_border = ifelse(hv == 1, dist_hv_border,-dist_hv_border),
      dia_1 = case_when(
        ano >= 2009 & ano <= 2016 ~ (cn + mt) / 2,
        ano >= 2017 & ano <= 2019 ~ (lc + rd + ch) /3
      ),
      dia_2 = case_when(
        ano >= 2009 & ano <= 2016 ~ (lc + rd + ch) / 3,
        ano >= 2017 & ano <= 2019 ~ (cn + mt) /2
      ),
      uf_prova = mun_prova %/% 10^5,
      id180 = ifelse(idade != 18 & priv == 0, 1, NA),
      id181 = ifelse(idade == 18 & priv == 0, 1, NA),
      dom50 = ifelse(pessoas_dom != "C" & priv == 0, 1, NA),
      dom51 = ifelse(pessoas_dom == "C" & priv == 0, 1, NA),
      ppi0 = ifelse(!(raca %in% c("C","D","F")) & priv == 0, 1, NA),
      ppi1 = ifelse(raca %in% c("C","D","F") & priv == 0, 1, NA),
      escp0 = case_when(
        esc_pai %in% c("D","E","F") & priv == 0 ~ 1,
        esc_pai %in% c("A","B","C") & priv == 0 ~ NA,
        .default = NA
      ),
      escp1 = case_when(
        esc_pai %in% c("D","E","F") & priv == 0 ~ NA,
        esc_pai %in% c("A","B","C") & priv == 0 ~ 1,
        .default = NA
      ),
      renda10 = ifelse(!(renda_dom %in% c("A","B")) & priv == 0,1,NA),
      renda11 = ifelse(renda_dom %in% c("A","B") & priv == 0,1,NA),
      fem0 = ifelse(mas == 1 & priv == 0, 1, NA),
      fem1 = ifelse(mas == 0 & priv == 0, 1, NA),
      priv0 = ifelse(priv == 0, 1, NA),
      priv1 = ifelse(priv == 1, 1, NA),
      seg15 = ifelse(seg %in% c(1:5) & priv == 0, 1, NA),
      seg67 = ifelse(seg %in% c(6:7) & priv == 0, 1, NA),
      total = 1,
      nonmig1 = ifelse(
        !is.na(mun_prova) &
          !is.na(mun_res) &
          !is.na(mun_escola) &
          mun_prova == mun_res & mun_res == mun_escola & priv == 0,
        1,
        NA
      ),
      nonmig2 = ifelse(
        !is.na(mun_prova) &
          !is.na(mun_res) &
          !is.na(mun_escola) &
          mun_prova == mun_res & mun_res != mun_escola & priv == 0,
        1,
        NA),
      nonmig3 = ifelse(
        !is.na(mun_prova) &
          !is.na(mun_res) &
          !is.na(mun_escola) &
          mun_prova != mun_res & mun_res == mun_escola & priv == 0,
        1,
        NA),
      nonmig4 = ifelse(
        !is.na(mun_prova) &
          !is.na(mun_res) &
          !is.na(mun_escola) &
          mun_prova == mun_escola & mun_res != mun_escola & priv == 0,
        1,
        NA),
      id18 = ifelse(idade == 18, 1, 0),
      dom5 = ifelse(pessoas_dom == "C", 1, 0),
      ppi = ifelse(raca %in% c("C","D","F"), 1, 0),
      escp = case_when(
        esc_pai %in% c("D","E","F") ~ 1,
        esc_pai %in% c("A","B","C") ~ 0,
        .default = NA
      ),
      renda1 = ifelse(renda_dom %in% c("A","B"),1,0),
      fem = ifelse(mas == 0, 1, 0),
      rd0 = ifelse(rd == 0, 1, 0),
      rd1 = ifelse(rd0 == 1,NA,rd1),
      rd2 = ifelse(rd0 == 1,NA,rd2),
      rd3 = ifelse(rd0 == 1,NA,rd3),
      rd4 = ifelse(rd0 == 1,NA,rd4),
      rd5 = ifelse(rd0 == 1,NA,rd5),
      branco_ch = branco_ch * 100,
      branco_cn = branco_cn * 100,
      branco_lc = branco_lc * 100,
      branco_mt = branco_mt * 100,
      dupla_ch = dupla_ch * 100,
      dupla_cn = dupla_cn * 100,
      dupla_lc = dupla_lc * 100,
      dupla_mt = dupla_mt * 100,
      acerto_ch = acerto_ch * 100,
      acerto_cn = acerto_cn * 100,
      acerto_lc = acerto_lc * 100,
      acerto_mt = acerto_mt * 100,
      acerto_al_ch = acerto_al_ch * 100,
      acerto_al_cn = acerto_al_cn * 100,
      acerto_al_lc = acerto_al_lc * 100,
      acerto_al_mt = acerto_al_mt * 100,
      acerto_ah_ch = acerto_ah_ch * 100,
      acerto_ah_cn = acerto_ah_cn * 100,
      acerto_ah_lc = acerto_ah_lc * 100,
      acerto_ah_mt = acerto_ah_mt * 100,
      acerto_bl_ch = acerto_bl_ch * 100,
      acerto_bl_cn = acerto_bl_cn * 100,
      acerto_bl_lc = acerto_bl_lc * 100,
      acerto_bl_mt = acerto_bl_mt * 100,
      acerto_bh_ch = acerto_bh_ch * 100,
      acerto_bh_cn = acerto_bh_cn * 100,
      acerto_bh_lc = acerto_bh_lc * 100,
      acerto_bh_mt = acerto_bh_mt * 100,
      acerto_cl_ch = acerto_cl_ch * 100,
      acerto_cl_cn = acerto_cl_cn * 100,
      acerto_cl_lc = acerto_cl_lc * 100,
      acerto_cl_mt = acerto_cl_mt * 100,
      acerto_ch_ch = acerto_ch_ch * 100,
      acerto_ch_cn = acerto_ch_cn * 100,
      acerto_ch_lc = acerto_ch_lc * 100,
      acerto_ch_mt = acerto_ch_mt * 100,
      acerto = (acerto_ch + acerto_cn + acerto_lc + acerto_mt) / 4,
      acerto_pal = (acerto_al_ch + acerto_al_cn + acerto_al_lc + acerto_al_mt) / 4,
      acerto_pah = (acerto_ah_ch + acerto_ah_cn + acerto_ah_lc + acerto_ah_mt) / 4,
      acerto_pbl = (acerto_bl_ch + acerto_bl_cn + acerto_bl_lc + acerto_bl_mt) / 4,
      acerto_pbh = (acerto_bh_ch + acerto_bh_cn + acerto_bh_lc + acerto_bh_mt) / 4,
      acerto_pcl = (acerto_cl_ch + acerto_cl_cn + acerto_cl_lc + acerto_cl_mt) / 4,
      acerto_pch = (acerto_ch_ch + acerto_ch_cn + acerto_ch_lc + acerto_ch_mt) / 4,
      acerto_ini5_cn = acerto_ini5_cn * 100,
      acerto_ini5_ch = acerto_ini5_ch * 100,
      acerto_ini5_lc = acerto_ini5_lc * 100,
      acerto_ini5_mt = acerto_ini5_mt * 100,
      acerto_ini10_cn = acerto_ini10_cn * 100,
      acerto_ini10_ch = acerto_ini10_ch * 100,
      acerto_ini10_lc = acerto_ini10_lc * 100,
      acerto_ini10_mt = acerto_ini10_mt * 100,
      acerto_fim5_cn = acerto_fim5_cn * 100,
      acerto_fim5_ch = acerto_fim5_ch * 100,
      acerto_fim5_lc = acerto_fim5_lc * 100,
      acerto_fim5_mt = acerto_fim5_mt * 100,
      acerto_fim10_cn = acerto_fim10_cn * 100,
      acerto_fim10_ch = acerto_fim10_ch * 100,
      acerto_fim10_lc = acerto_fim10_lc * 100,
      acerto_fim10_mt = acerto_fim10_mt * 100,
      acerto_ini5_d1 = case_when(
        ano == 2009 ~ acerto_ini5_cn,
        ano >= 2010 & ano <= 2016 ~ acerto_ini5_ch,
        ano >= 2017 & ano <= 2019 ~ acerto_ini5_lc,
        .default = NA
      ),
      acerto_ini10_d1 = case_when(
        ano == 2009 ~ acerto_ini10_cn,
        ano >= 2010 & ano <= 2016 ~ acerto_ini10_ch,
        ano >= 2017 & ano <= 2019 ~ acerto_ini10_lc,
        .default = NA
      ),
      acerto_fim5_d1 = case_when(
        ano == 2009 ~ acerto_fim5_ch,
        ano >= 2010 & ano <= 2016 ~ acerto_fim5_cn,
        ano >= 2017 & ano <= 2019 ~ acerto_fim5_ch,
        .default = NA
      ),
      acerto_fim10_d1 = case_when(
        ano == 2009 ~ acerto_fim10_ch,
        ano >= 2010 & ano <= 2016 ~ acerto_fim5_cn,
        ano >= 2017 & ano <= 2019 ~ acerto_fim5_ch,
        .default = NA
      ),
      acerto_ini5_d2 = case_when(
        ano == 2009 ~ acerto_ini5_lc,
        ano >= 2010 & ano <= 2016 ~ acerto_ini5_lc,
        ano >= 2017 & ano <= 2019 ~ acerto_ini5_cn,
        .default = NA
      ),
      acerto_ini10_d2 = case_when(
        ano == 2009 ~ acerto_ini10_lc,
        ano >= 2010 & ano <= 2016 ~ acerto_ini10_lc,
        ano >= 2017 & ano <= 2019 ~ acerto_ini10_cn,
        .default = NA
      ),
      acerto_fim5_d2 = case_when(
        ano == 2009 ~ acerto_fim5_mt,
        ano >= 2010 & ano <= 2016 ~ acerto_fim5_mt,
        ano >= 2017 & ano <= 2019 ~ acerto_fim5_mt,
        .default = NA
      ),
      acerto_fim10_d2 = case_when(
        ano == 2009 ~ acerto_fim10_mt,
        ano >= 2010 & ano <= 2016 ~ acerto_fim10_mt,
        ano >= 2017 & ano <= 2019 ~ acerto_fim10_mt,
        .default = NA
      ),
      acerto_ini5 = (acerto_ini5_cn + acerto_ini5_ch + acerto_ini5_lc + acerto_ini5_mt)/4,
      acerto_fim5 = (acerto_fim5_cn + acerto_fim5_ch + acerto_fim5_lc + acerto_fim5_mt)/4,
      d_acerto_5 = acerto_fim5 - acerto_ini5,
      d_acerto_5_d1 = acerto_fim5_d1 - acerto_ini5_d1,
      d_acerto_5_d2 = acerto_fim5_d2 - acerto_ini5_d2,
      d_acerto_10_d1 = acerto_fim10_d1 - acerto_ini10_d1,
      d_acerto_10_d2 = acerto_fim10_d2 - acerto_ini10_d2,
      d_pb_ch = acerto_bl_ch - acerto_bh_ch,
      d_pb_cn = acerto_bl_cn - acerto_bh_cn,
      d_pb_lc = acerto_bl_lc - acerto_bh_lc,
      d_pb_mt = acerto_bl_mt - acerto_bh_mt,
      d_pb = acerto_pbl - acerto_pbh,
      d_acerto_5ch = acerto_fim5_ch - acerto_ini5_ch,
      d_acerto_5cn = acerto_fim5_cn - acerto_ini5_cn,
      d_acerto_5lc = acerto_fim5_lc - acerto_ini5_lc,
      d_acerto_5mt = acerto_fim5_mt - acerto_ini5_mt
    ) %>%
    merge(pib, by = c("mun_prova", "ano"))
  
  setDT(temp)
  vpad <- temp[, .(
    media_p = pad(media),
    cn_p = pad(cn),
    ch_p = pad(ch),
    lc_p = pad(lc),
    mt_p = pad(mt),
    rd_p = pad(rd),
    dia_1_p = pad(dia_1),
    dia_2_p = pad(dia_2),
    rd1_p = pad(rd1),
    rd2_p = pad(rd2),
    rd3_p = pad(rd3),
    rd4_p = pad(rd4),
    rd5_p = pad(rd5)
  )]
  
  temp <- temp %>% bind_cols(vpad)
  rm(vpad)
  
  base_nota <- temp %>% setDT()
  
  rm(temp)
  
  message("Saving main scores data base...")
  saveRDS(base_nota, file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_",ano,".RDS"))
  rm(base_nota)
  
  
  message("Saving absent students data base...")
  # Base de abstenções
  base_abs <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/enem_abs_",ano,"_v4.RDS")) %>%
    select(-sexo,-status_rd) %>%
    left_join(mun_hv,
              by = c(
                "mun_prova" = "co_municipio"
              )) %>%
    mutate(
      priv = ifelse(dep_adm == 4, 1, 0),
      priv0 = ifelse(priv == 0, 1, NA)
    ) %>%
    select(
      -conclusao,
      -cn,
      -ch,
      -lc,
      -mt,
      -rd,
      -media,
      -rd1,
      -rd2,
      -rd3,
      -rd4,
      -rd5,
      -co_cn,
      -co_ch,
      -co_lc,
      -co_mt,
      -pres_cn,
      -pres_ch,
      -pres_lc,
      -pres_mt,
      -dep_adm
    ) %>%
    mutate(
      abs = ifelse(
        abs_rd == 1 &
          abs_cn == 1 &
          abs_ch == 1 &
          abs_lc == 1 &
          abs_mt == 1,
        1,
        0),
      uf_prova = mun_prova %/% 10^5,
      dist_hv_border = ifelse(hv == 1, dist_hv_border,-dist_hv_border)
    )
  
  saveRDS(base_abs, file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_abs_",ano,".RDS"))
  rm(base_abs)
  rm(ano)
  
  fim <- Sys.time()
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  hours <- round(mins /60)
  
  message("---------------------------------------------")
  message("Elapsed time: ",hours, " hours, ",mins," mins, and ", secs, " s")
  message("---------------------------------------------")
  
  rm(ini, fim, delta, mins, secs, hours)
}

rm(mun_hv,pib)


# ---------------------------------------------------------------------------- #
# 3. SAEB ----
# ---------------------------------------------------------------------------- #
## 3.1 Paths ----
# ---------------------------------------------------------------------------- #
saeb17_path <- "Z:/Arquivos IFB/SAEB/microdados_saeb_2017/DADOS"
saeb19_path <- "Z:/Arquivos IFB/SAEB/microdados_saeb_2019/DADOS"

# ---------------------------------------------------------------------------- #
## 3.2 Mun----
# ---------------------------------------------------------------------------- #
mun_hv <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/mun_hv.RDS")

# Coordenadas
coordenadas <- mun_hv$centroid %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(
    lon = X,
    lat = Y
  )

mun_hv <- mun_hv %>%
  bind_cols(coordenadas) %>%
  st_drop_geometry() %>%
  select(co_municipio, lon, lat, dist_hv_border, seg)
rm(coordenadas)

# ---------------------------------------------------------------------------- #
## 3.3 2017 ----
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
### 3.3.1 5th grade MS -----
# ---------------------------------------------------------------------------- #
saeb5 <- read_dta("Z:/Arquivos IFB/SAEB/microdados_saeb_2017/DADOS/ts_aluno_5ef.dta")

saeb5 <- saeb5 %>% 
    select(
      id_dependencia_adm,
      id_localizacao,
      id_municipio,
      in_prova_brasil,
      proficiencia_lp_saeb,
      proficiencia_mt_saeb,
      peso_aluno_lp,
      peso_aluno_mt,
      in_proficiencia
    ) %>%
  rename_all(tolower) %>% 
  rename(dep_adm = id_dependencia_adm,
         urb_rur = id_localizacao,
         mun_prova = id_municipio,
         lp = proficiencia_lp_saeb,
         mt = proficiencia_mt_saeb
  ) %>% 
  mutate(
    urb = ifelse(urb_rur == 1, 1, 0),
    priv = ifelse(dep_adm == 4, 1, 0),
    ano = 2017,
    serie = 5
  ) %>% 
  filter(
    !is.na(lp) & !is.na(mt) &
      !is.na(peso_aluno_lp) & !is.na(peso_aluno_mt) &
      !is.na(dep_adm) & !is.na(urb_rur) &
      !is.na(mun_prova) &
      in_prova_brasil == 1 &
      in_proficiencia == 1
  ) %>%
  setDT()


saeb5 <- saeb5[priv == 0, .(
  lp = weighted.mean(x = lp, w = peso_aluno_lp),
  mt = weighted.mean(x = mt, w = peso_aluno_mt),
  lp_peso = sum(peso_aluno_lp),
  mt_peso = sum(peso_aluno_mt)
), by = c("mun_prova","ano","serie")]

# ---------------------------------------------------------------------------- #
### 3.3.2 9th grade MS -------
# ---------------------------------------------------------------------------- #

saeb9 <- read_dta("Z:/Arquivos IFB/SAEB/microdados_saeb_2017/DADOS/ts_aluno_9ef.dta")

saeb9 <- saeb9 %>% 
  select(
    id_dependencia_adm,
    id_localizacao,
    id_municipio,
    in_prova_brasil,
    proficiencia_lp_saeb,
    proficiencia_mt_saeb,
    peso_aluno_lp,
    peso_aluno_mt,
    in_proficiencia
  ) %>%
  rename_all(tolower) %>% 
  rename(dep_adm = id_dependencia_adm,
         urb_rur = id_localizacao,
         mun_prova = id_municipio,
         lp = proficiencia_lp_saeb,
         mt = proficiencia_mt_saeb
  ) %>% 
  mutate(
    urb = ifelse(urb_rur == 1, 1, 0),
    priv = ifelse(dep_adm == 4, 1, 0),
    ano = 2017,
    serie = 9
  ) %>% 
  filter(
    !is.na(lp) & !is.na(mt) &
      !is.na(peso_aluno_lp) & !is.na(peso_aluno_mt) &
      !is.na(dep_adm) & !is.na(urb_rur) &
      !is.na(mun_prova) &
      in_prova_brasil == 1 &
      in_proficiencia == 1
  ) %>%
  setDT()


saeb9 <- saeb9[priv == 0, .(
  lp = weighted.mean(x = lp, w = peso_aluno_lp),
  mt = weighted.mean(x = mt, w = peso_aluno_mt),
  lp_peso = sum(peso_aluno_lp),
  mt_peso = sum(peso_aluno_mt)
), by = c("mun_prova","ano","serie")]

# ---------------------------------------------------------------------------- #
### 3.3.3 Senior HS -------
# ---------------------------------------------------------------------------- #

saeb3 <- read_dta("Z:/Arquivos IFB/SAEB/microdados_saeb_2017/DADOS/ts_aluno_3em_esc.dta")

saeb3 <- saeb3 %>% 
  select(
    id_dependencia_adm,
    id_localizacao,
    id_municipio,
    in_prova_brasil,
    proficiencia_lp_saeb,
    proficiencia_mt_saeb,
    peso_aluno_lp,
    peso_aluno_mt,
    in_proficiencia
  ) %>%
  rename_all(tolower) %>% 
  rename(dep_adm = id_dependencia_adm,
         urb_rur = id_localizacao,
         mun_prova = id_municipio,
         lp = proficiencia_lp_saeb,
         mt = proficiencia_mt_saeb
  ) %>% 
  mutate(
    urb = ifelse(urb_rur == 1, 1, 0),
    priv = ifelse(dep_adm == 4, 1, 0),
    ano = 2017,
    serie = 3
  ) %>% 
  filter(
    !is.na(lp) & !is.na(mt) &
      !is.na(peso_aluno_lp) & !is.na(peso_aluno_mt) &
      !is.na(dep_adm) & !is.na(urb_rur) &
      !is.na(mun_prova) &
      in_prova_brasil == 1 &
      in_proficiencia == 1
  ) %>%
  setDT()


saeb3 <- saeb3[priv == 0, .(
  lp = weighted.mean(x = lp, w = peso_aluno_lp),
  mt = weighted.mean(x = mt, w = peso_aluno_mt),
  lp_peso = sum(peso_aluno_lp),
  mt_peso = sum(peso_aluno_mt)
), by = c("mun_prova","ano","serie")]

saeb_17 <- rbind(saeb3, saeb5, saeb9) %>% 
  arrange(mun_prova, ano)

# ---------------------------------------------------------------------------- #
## 3.4 2019 -----
### 3.4.1 5th grade MS ----
# ---------------------------------------------------------------------------- #
saeb5 <- read_dta("Z:/Arquivos IFB/SAEB/microdados_saeb_2019/DADOS/ts_aluno_5ef.dta")

saeb5 <- saeb5 %>% 
  select(
    id_dependencia_adm,
    id_localizacao,
    id_municipio,
    proficiencia_lp_saeb,
    proficiencia_mt_saeb,
    peso_aluno_lp,
    peso_aluno_mt
  ) %>%
  rename_all(tolower) %>% 
  rename(dep_adm = id_dependencia_adm,
         urb_rur = id_localizacao,
         mun_prova = id_municipio,
         lp = proficiencia_lp_saeb,
         mt = proficiencia_mt_saeb
  ) %>% 
  mutate(
    urb = ifelse(urb_rur == 1, 1, 0),
    priv = ifelse(dep_adm == 4, 1, 0),
    ano = 2019,
    serie = 5
  ) %>% 
  filter(
    !is.na(lp) & !is.na(mt) &
      !is.na(peso_aluno_lp) & !is.na(peso_aluno_mt) &
      !is.na(dep_adm) & !is.na(urb_rur) &
      !is.na(mun_prova) 
  ) %>%
  setDT()


saeb5 <- saeb5[priv == 0, .(
  lp = weighted.mean(x = lp, w = peso_aluno_lp),
  mt = weighted.mean(x = mt, w = peso_aluno_mt),
  lp_peso = sum(peso_aluno_lp),
  mt_peso = sum(peso_aluno_mt)
), by = c("mun_prova","ano","serie")]

# ---------------------------------------------------------------------------- #
### 3.4.2 9th grade MS -------
# ---------------------------------------------------------------------------- #

saeb9 <- read_dta("Z:/Arquivos IFB/SAEB/microdados_saeb_2019/DADOS/ts_aluno_9ef.dta")

saeb9 <- saeb9 %>% 
  select(
    id_dependencia_adm,
    id_localizacao,
    id_municipio,
    proficiencia_lp_saeb,
    proficiencia_mt_saeb,
    peso_aluno_lp,
    peso_aluno_mt
  ) %>%
  rename_all(tolower) %>% 
  rename(dep_adm = id_dependencia_adm,
         urb_rur = id_localizacao,
         mun_prova = id_municipio,
         lp = proficiencia_lp_saeb,
         mt = proficiencia_mt_saeb
  ) %>% 
  mutate(
    urb = ifelse(urb_rur == 1, 1, 0),
    priv = ifelse(dep_adm == 4, 1, 0),
    ano = 2019,
    serie = 9
  ) %>% 
  filter(
    !is.na(lp) & !is.na(mt) &
      !is.na(peso_aluno_lp) & !is.na(peso_aluno_mt) &
      !is.na(dep_adm) & !is.na(urb_rur) &
      !is.na(mun_prova) 
  ) %>%
  setDT()


saeb9 <- saeb9[priv == 0, .(
  lp = weighted.mean(x = lp, w = peso_aluno_lp),
  mt = weighted.mean(x = mt, w = peso_aluno_mt),
  lp_peso = sum(peso_aluno_lp),
  mt_peso = sum(peso_aluno_mt)
), by = c("mun_prova","ano","serie")]

# ---------------------------------------------------------------------------- #
### 3.4.3 Senior HS -------
# ---------------------------------------------------------------------------- #

saeb3 <- read_dta("Z:/Arquivos IFB/SAEB/microdados_saeb_2019/DADOS/ts_aluno_34em.dta")

saeb3 <- saeb3 %>% 
  select(
    id_dependencia_adm,
    id_localizacao,
    id_municipio,
    proficiencia_lp_saeb,
    proficiencia_mt_saeb,
    peso_aluno_lp,
    peso_aluno_mt
  ) %>%
  rename_all(tolower) %>% 
  rename(dep_adm = id_dependencia_adm,
         urb_rur = id_localizacao,
         mun_prova = id_municipio,
         lp = proficiencia_lp_saeb,
         mt = proficiencia_mt_saeb
  ) %>% 
  mutate(
    urb = ifelse(urb_rur == 1, 1, 0),
    priv = ifelse(dep_adm == 4, 1, 0),
    ano = 2019,
    serie = 3
  ) %>% 
  filter(
    !is.na(lp) & !is.na(mt) &
      !is.na(peso_aluno_lp) & !is.na(peso_aluno_mt) &
      !is.na(dep_adm) & !is.na(urb_rur) &
      !is.na(mun_prova)
  ) %>%
  setDT()


saeb3 <- saeb3[priv == 0, .(
  lp = weighted.mean(x = lp, w = peso_aluno_lp),
  mt = weighted.mean(x = mt, w = peso_aluno_mt),
  lp_peso = sum(peso_aluno_lp),
  mt_peso = sum(peso_aluno_mt)
), by = c("mun_prova","ano","serie")]

saeb_19 <- rbind(saeb3, saeb5, saeb9) %>% 
  arrange(mun_prova, ano)

# ---------------------------------------------------------------------------- #

saeb <- rbind(saeb_17, saeb_19)

saveRDS(saeb, file = paste0("Z:/Tuffy/Paper - HV/Bases/saeb_total.RDS"))
rm(saeb, saeb_17, saeb_19, saeb3, saeb5, saeb9)



rm(list = ls())
gc()
# ---------------------------------------------------------------------------- #
#4. INPE Data ----
# ---------------------------------------------------------------------------- #
## 4.1 (2018) ----
# ---------------------------------------------------------------------------- #

dtb <- read_xls("Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/inpe/DTB_BRASIL_MUNICIPIO.xls") %>%
  mutate(
    nomemun = stri_trans_general(Nome_Município, "Latin-ASCII"),
    nomemun = gsub(" ","",x = nomemun),
    nomemun = gsub("-","",x = nomemun),
    nomemun = gsub("'","",x = nomemun),
    nomemun = toupper(nomemun),
    nomeuf = stri_trans_general(Nome_UF, "Latin-ASCII"),
    nomeuf = gsub(" ","",x = nomeuf),
    nomeuf = gsub("-","",x = nomeuf),
    nomeuf = gsub("'","",x = nomeuf),
    nomeuf = toupper(nomeuf),
    codmun = `Código Município Completo`
  ) %>%
  select(nomemun, nomeuf, codmun)

# Dados do INPE-Queimadas
inpe <- fread(file = "Z:/Tuffy/Paper - HV/Bases/inpe/dados_sisam-2018/task_9045.dados_sisam.2018.csv")

inpe <- inpe %>%
  select(
    municipio_nome,
    uf_nome,
    vento_velocidade_ms,
    temperatura_c,
    precipitacao_mmdia,
    umidade_relativa_percentual,
    datahora,
    pm25_ugm3,
    o3_ppb
  ) %>%
  mutate(
    dia = day(ymd_hms(datahora)),
    mes = month(ymd_hms(datahora)),
    hora = hour(ymd_hms(datahora))
  ) %>%
  filter(mes == 11 & dia %in% c(4,11) & hora == 12) %>% #Dia e hora do ENEM# 2018
  select(-hora)

inpe <- inpe %>%
  mutate(
    nomemun = stri_trans_general(municipio_nome, "Latin-ASCII"),
    nomemun = gsub(" ","",x = nomemun),
    nomemun = gsub("-","",x = nomemun),
    nomemun = gsub("'","",x = nomemun),
    nomeuf = stri_trans_general(uf_nome, "Latin-ASCII"),
    nomeuf = gsub(" ","",x = nomeuf),
    nomeuf = gsub("-","",x = nomeuf),
    nomeuf = gsub("'","",x = nomeuf)
  ) %>% 
  inner_join(dtb, by = c("nomemun","nomeuf")) %>%
  rename(
    vento = vento_velocidade_ms,
    temp = temperatura_c,
    prec = precipitacao_mmdia,
    umid = umidade_relativa_percentual,
    pm25 = pm25_ugm3,
    o3 = o3_ppb
  ) %>%
  select(-nomeuf,-nomemun,-municipio_nome,-uf_nome,-mes) %>%
  pivot_wider(id_cols = codmun,names_from = dia,values_from = c(vento,temp,prec,umid,pm25,o3))

rm(dtb)

saveRDS(inpe, "Z:/Tuffy/Paper - HV/Bases/inpe/mun/inpe_mun_2018.rds")

# ---------------------------------------------------------------------------- #
## 4.2 (2019) ----
# ---------------------------------------------------------------------------- #
dtb <- read_xls("Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/inpe/DTB_BRASIL_MUNICIPIO.xls") %>%
  mutate(
    nomemun = stri_trans_general(Nome_Município, "Latin-ASCII"),
    nomemun = gsub(" ","",x = nomemun),
    nomemun = gsub("-","",x = nomemun),
    nomemun = gsub("'","",x = nomemun),
    nomemun = toupper(nomemun),
    nomeuf = stri_trans_general(Nome_UF, "Latin-ASCII"),
    nomeuf = gsub(" ","",x = nomeuf),
    nomeuf = gsub("-","",x = nomeuf),
    nomeuf = gsub("'","",x = nomeuf),
    nomeuf = toupper(nomeuf),
    codmun = `Código Município Completo`
  ) %>%
  select(nomemun, nomeuf, codmun)

# Dados do INPE-Queimadas
inpe <- fread(file = "Z:/Tuffy/Paper - HV/Bases/inpe/dados_sisam-2019/task_9045.dados_sisam.2019.csv")

inpe <- inpe %>%
  select(
    municipio_nome,
    uf_nome,
    vento_velocidade_ms,
    temperatura_c,
    precipitacao_mmdia,
    umidade_relativa_percentual,
    datahora,
    pm25_ugm3,
    o3_ppb
  ) %>%
  mutate(
    dia = day(ymd_hms(datahora)),
    mes = month(ymd_hms(datahora)),
    hora = hour(ymd_hms(datahora))
  ) %>%
  filter(mes == 11 & dia %in% c(3,10) & hora == 12) %>% #Dia e hora do ENEM# 2018
  select(-hora)

inpe <- inpe %>%
  mutate(
    nomemun = stri_trans_general(municipio_nome, "Latin-ASCII"),
    nomemun = gsub(" ","",x = nomemun),
    nomemun = gsub("-","",x = nomemun),
    nomemun = gsub("'","",x = nomemun),
    nomeuf = stri_trans_general(uf_nome, "Latin-ASCII"),
    nomeuf = gsub(" ","",x = nomeuf),
    nomeuf = gsub("-","",x = nomeuf),
    nomeuf = gsub("'","",x = nomeuf)
  ) %>% 
  inner_join(dtb, by = c("nomemun","nomeuf")) %>%
  rename(
    vento = vento_velocidade_ms,
    temp = temperatura_c,
    prec = precipitacao_mmdia,
    umid = umidade_relativa_percentual,
    pm25 = pm25_ugm3,
    o3 = o3_ppb
  ) %>%
  select(-nomeuf,-nomemun,-municipio_nome,-uf_nome,-mes) %>%
  pivot_wider(id_cols = codmun,names_from = dia,values_from = c(vento,temp,prec,umid,pm25,o3))

rm(dtb)
saveRDS(inpe, "Z:/Tuffy/Paper - HV/Bases/inpe/mun/inpe_mun_2019.rds")

rm(list = ls())
gc()

# ---------------------------------------------------------------------------- #
# 5.Mock (all obs)------------
# ---------------------------------------------------------------------------- #


#----------------------------------------------------------------------------- #
## 5.1. Construção Bases Novas ----
# ---------------------------------------------------------------------------- #
###5.1.1 Func ---------
# ---------------------------------------------------------------------------- #

pad <- function(x){
  x = (x - mean(x, na.rm = T)) / 
    sd(x, na.rm = T)
  return(x)
}

# Function to calculate the weighted standard errors, according to formula on:
#https://medium.com/@OttoYu/weighted-mean-and-standard-devation-computation-f1dbacdf49b6

weighted.sd <- function(x,w,na.rm){
  
  # Verifying whether x and w have the same length
  if(length(x) == length(w)){
    
    # removing NA
    if(!missing(na.rm)){
      if(na.rm == T){
        temp <- !is.na(x) & !is.na(w)
        x <- x[temp]
        w <- w[temp]
        rm(temp)
      } 
    }
    
    # Calculating SD
    x = ((w %*% (x - weighted.mean(x, w = w))^2) / (sum(w)))^(1/2)
    
    return(x)
  }
}

wpad <- function(x,w){
  x = (x - weighted.mean(x,w = w, na.rm = T))/as.vector(weighted.sd(x, w = w, na.rm = T))
  return(x)
}



org_data <- function(base,ano,vlist,cnames) {
  
  # Organização da base de dados
  base <- base %>% relocate(vlist)
  
  colnames(base) <- cnames
  
  # Pessoas no domicílio ------------------------------------------------------#
  
  if (ano == 2009) {
    # Modificações somente de 2009
    
    base <- base %>%
      mutate(
        # 2009
        pessoas_dom = case_when(
          pessoas_dom == 'F' ~ 'A',
          # 1 pessoa
          pessoas_dom %in% c('A', 'B', 'C') ~ 'B',
          # 2 a 4 pessoas
          pessoas_dom %in% c('D', 'E') ~ 'C',
          # 5 ou mais pessoas
          .default = 'D'
        ),
      )
  } else if (ano == 2010) {
    # Modificações somente de 2010
    
    base <- base %>%
      mutate(
        # 2010
        pessoas_dom = case_when(
          pessoas_dom == 'E' ~ 'A',
          pessoas_dom == 'A' ~ 'B',
          pessoas_dom %in% c('B', 'C', 'D') ~ 'C',
          .default = 'D'
        )
      )
    
  } else if (ano == 2011) {
    
    # Modificações somente de 2011
    base <- base %>%
      mutate(
        pessoas_dom = as.integer(pessoas_dom),
        pessoas_dom = case_when(
          pessoas_dom == 1 ~ 'A',
          pessoas_dom >= 2 & pessoas_dom <= 4 ~ 'B',
          pessoas_dom >= 5 & pessoas_dom <= 20 ~ 'C',
          .default = 'D'
        )
      )
    
  } else if (ano >= 2012) {
    
    # Modificações somente de 2012 a 2019
    base <- base %>%
      mutate(
        npessoas_dom = as.numeric(pessoas_dom)
        pessoas_dom = case_when(
          pessoas_dom == 1 ~ 'A',
          pessoas_dom >= 2 & pessoas_dom <= 4 ~ 'B',
          pessoas_dom >= 5 & pessoas_dom <= 20 ~ 'C',
          .default = 'D'
        )
      )
  }  
  
  # Renda domiciliar ----------------------------------------------------------#
  
  if (ano %in% c(2009,2010)) {
    
    # Modificações somente de 2009 e 2010
    base <- base %>%
      mutate(
        # 2009, 2010
        renda_dom = case_when(
          renda_dom == 'H' ~ 'A',
          # Nenhuma
          renda_dom == 'A' ~ 'B',
          # Até 1 SM
          renda_dom %in% c('B', 'C', 'D') ~ 'C',
          # >1 a 10 SM
          renda_dom %in% c('E', 'F', 'G') ~ 'D',
          # >10 a 50 SM
          .default = 'E'
        )
      )
  } else if (ano == 2011) {
    
    # Modificações somente de 2011
    base <- base %>%
      mutate(
        renda_dom = case_when(
          renda_dom == 'A' ~ 'A',
          renda_dom == 'B' ~ 'B',
          renda_dom %in% c('C', 'D', 'E', 'F', 'G') ~ 'C',
          renda_dom %in% c('H', 'I', 'J', 'K') ~ 'D',
          .default = 'E'
        )
      )
  } else if (ano >= 2012) {
    
    # Modificações a partir de 2012
    base <- base %>%
      mutate(
        renda_dom = case_when(
          renda_dom == 'A' ~ 'A',
          renda_dom == 'B' ~ 'B',
          renda_dom %in% c('C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M') ~ 'C',
          renda_dom %in% c('N', 'O', 'P', 'Q') ~ 'D',
          .default = 'E'
        )
      )
  }
  
  # Raça ----------------------------------------------------------------------#
  
  if(ano == 2009) {
    
    # Modificações somente de 2009
    base <- base %>%
      mutate(
        # 2009
        raca = case_when(
          raca %in% c('', '*', '.') ~ 'A',
          # inválido
          raca == 'A' ~ 'B',
          # branca
          raca == 'C' ~ 'C',
          # preta
          raca == 'B' ~ 'D',
          # parda
          raca == 'D' ~ 'E',
          # amarela
          raca == 'E' ~ 'F',
          # indígena
          .default = 'G'
        )
      )
  } else if (ano %in% c(2010, 2011, 2012, 2013, 2014, 2017, 2018, 2019)) {
    
    # Modificações somente de 2010 a 2014 e de 2017 a 2019
    base <- base %>%
      mutate(
        # 2010, 2011, 2012, 2013, 2014, 2017, 2018, 2019
        raca = case_when(
          raca == 0 ~ 'A',
          raca == 1 ~ 'B',
          raca == 2 ~ 'C',
          raca == 3 ~ 'D',
          raca == 4 ~ 'E',
          raca == 5 ~ 'F',
          .default = 'G'
        )
        
      )
    
  } else if(ano %in% c(2015,2016)) {
    
    # Modificações somente de 2015 e 2016
    base <- base %>%
      mutate(
        
        # 2015, 2016
        raca = case_when(
          raca %in% c(0,6) ~ 'A',
          raca == 1 ~ 'B',
          raca == 2 ~ 'C',
          raca == 3 ~ 'D',
          raca == 4 ~ 'E',
          raca == 5 ~ 'F',
          .default = 'G')
        
      )
    
  }
  
  # Escolaridade do pai -------------------------------------------------------#
  
  if (ano == 2010) {
    
    # Modificações somente de 2010
    base <- base %>%
      mutate(
        esc_pai = case_when(
          esc_pai == "H" ~ "A",
          esc_pai == "A" ~ "B",
          esc_pai == "B" ~ "C",
          esc_pai == "C" ~ "D",
          esc_pai == "D" ~ "E",
          esc_pai %in% c("E", "F", "G") ~ "F",
          esc_pai == "I" ~ "G",
          .default = "H"
        ),
        
        esc_mae = case_when(
          esc_mae == "H" ~ "A",
          esc_mae == "A" ~ "B",
          esc_mae == "B" ~ "C",
          esc_mae == "C" ~ "D",
          esc_mae == "D" ~ "E",
          esc_mae %in% c("E", "F", "G") ~ "F",
          esc_mae == "I" ~ "G",
          .default = "H"
        )
      )
  } else if (ano == 2009 | (ano >= 2009 & ano <= 2014)) {
    
    # Modificações somente de 2009 e 2011 a 2014
    base <- base %>%
      mutate(
        esc_pai = case_when(
          esc_pai == "A" ~ "A",
          # Não estudou
          esc_pai == "B" ~ "B",
          # EF 1º ciclo completo
          esc_pai %in% c("C", "D") ~ "C",
          # EF 2º ciclo completo
          esc_pai %in% c("E", "F") ~ "D",
          # EM completo
          esc_pai == "G" ~ "E",
          # ES completo
          esc_pai == "H" ~ "F",
          # Pós-graduação
          esc_pai == "I" ~ "G",
          # Não sabe
          .default = "H"
          # missing
        ),
        
        esc_mae = case_when(
          esc_mae == "A" ~ "A",
          # Não estudou
          esc_mae == "B" ~ "B",
          # EF 1º ciclo completo
          esc_mae %in% c("C", "D") ~ "C",
          # EF 2º ciclo completo
          esc_mae %in% c("E", "F") ~ "D",
          # EM completo
          esc_mae == "G" ~ "E",
          # ES completo
          esc_mae == "H" ~ "F",
          # Pós-graduação
          esc_mae == "I" ~ "G",
          # Não sabe
          .default = "H"
          # missing
        )
        
      )
  } else if (ano >= 2015 & ano <= 2019) {
    
    # Modificações somente de 2015 a 2019
    base <- base %>%
      mutate(
        esc_pai = case_when(
          esc_pai %in% c("A", "B") ~ "A",
          esc_pai == "C" ~ "B",
          esc_pai == "D" ~ "C",
          esc_pai == "E" ~ "D",
          esc_pai == "F" ~ "E",
          esc_pai == "G" ~ "F",
          esc_pai == "H" ~ "G",
          .default = "H"
        ),
        esc_mae = case_when(
          esc_mae %in% c("A", "B") ~ "A",
          esc_mae == "C" ~ "B",
          esc_mae == "D" ~ "C",
          esc_mae == "E" ~ "D",
          esc_mae == "F" ~ "E",
          esc_mae == "G" ~ "F",
          esc_mae == "H" ~ "G",
          .default = "H"
        ) 
        
        
        
      )
  } 
  
  # Abstenção em redação ------------------------------------------------------#
  
  if (ano %in% c(2009,2011)) {
    
    # Modificações somente de 2009
    base <- base %>%
      mutate(
        abs_rd = case_when(
          status_rd == "B" ~ 0,
          # Entregou a redação em branco
          status_rd == "F" ~ 1,
          # Faltou à prova
          status_rd == "N" ~ 0,
          # Redação anulada
          status_rd == "P" ~ 0,
          # Presente à prova
          .default = NA
        )
      )
  } else if (ano == 2010) {
    
    # Modificações somente de 2010
    base <- base %>%
      mutate(
        abs_rd = case_when(
          status_rd == "B" ~ 0,
          # Entregou a redação em branco
          status_rd == "D" ~ 0,
          # Desconsiderada
          status_rd == "F" ~ 1,
          # Faltou à prova
          status_rd == "N" ~ 0,
          # Redação anulada
          status_rd == "P" ~ 0,
          # Presente à prova
          .default = NA
        )
      )
  } else if (ano == 2012) {
    
    # Modificações somente de 2012
    base <- base %>%
      mutate(
        abs_rd = case_when(
          status_rd == "B" ~ 0,
          # Entregou a redação em branco
          status_rd == "F" ~ 1,
          # Faltou à prova
          status_rd == "N" ~ 0,
          # Redação anulada
          status_rd == "P" ~ 0,
          # Presente à prova
          status_rd == "T" ~ 0,
          # Fuga ao tema
          status_rd == "I" ~ 0,
          # Texto insuficiente
          status_rd == "A" ~ 0,
          # Não atende ao tipo textual
          status_rd == "H" ~ 0,
          # Anulada - fere aos direitos humanos
          status_rd == "C" ~ 0,
          # Cópia de texto motivador
          .default = NA
        )
      )
    
  } else if (ano == 2013) {
    
    # Modificações somente de 2013 
    base <- base %>%
      mutate(
        # Não tem a opção 8
        abs_rd = case_when(
          status_rd == 1 ~ 0,
          # Entregou a redação em branco
          status_rd == 6 ~ 1,
          # Faltou à prova
          status_rd == 2 ~ 0,
          # Redação anulada
          status_rd == 7 ~ 0,
          # Presente à prova
          status_rd == 3 ~ 0,
          # Fuga ao tema
          status_rd == 5 ~ 0,
          # Texto insuficiente
          status_rd == 4 ~ 0,
          # Não atende ao tipo textual
          status_rd == 9 ~ 0,
          # Anulada - fere aos direitos humanos
          status_rd == 10 ~ 0,
          # Cópia de texto motivador
          .default = NA
        )
      )
    
  } else if (ano == 2014) {
    
    # Modificações somente de 2014 
    base <- base %>%
      mutate(
        # Não tem a opção 8
        abs_rd = case_when(
          status_rd == 1 ~ 0,
          # Entregou a redação em branco
          status_rd == 6 ~ 1,
          # Faltou à prova
          status_rd == 2 ~ 0,
          # Redação anulada
          status_rd == 1 ~ 0,
          # Presente à prova
          status_rd == 3 ~ 0,
          # Fuga ao tema
          status_rd == 5 ~ 0,
          # Texto insuficiente
          status_rd == 4 ~ 0,
          # Não atende ao tipo textual
          status_rd == 9 ~ 0,
          # Anulada - fere aos direitos humanos
          status_rd == 10 ~ 0,
          # Cópia de texto motivador
          status_rd == 11 ~ 0,
          # Parte do texto desconectada com o tema proposto
          .default = NA
        )
      )
    
  } else if (ano == 2015) {
    
    # Modificações somente de 2015 
    # base <- base %>%
    #   mutate(
    #     # Não tem a opção de ausência
    #     abs_rd = case_when(
    #       status_rd == 4 ~ 0,
    #       # Entregou a redação em branco
    #       status_rd == 2 ~ 0,
    #       # Redação anulada
    #       status_rd == 1 ~ 0,
    #       # Presente à prova
    #       status_rd == 6 ~ 0,
    #       # Fuga ao tema
    #       status_rd == 8 ~ 0,
    #       # Texto insuficiente
    #       status_rd == 7 ~ 0,
    #       # Não atende ao tipo textual
    #       status_rd == 5 ~ 0,
    #       # Anulada - fere aos direitos humanos
    #       status_rd == 3 ~ 0,
    #       # Cópia de texto motivador
    #       status_rd == 9 ~ 0,
    #       # Parte do texto desconectada com o tema proposto
    #       status_rd == 98 ~ 0,
    #       # Não atende ao item 2.2.5 do edital: Dispor de documentos comprobatórios da condição que motiva a solicitação de atendimento ESPECIALIZADO e/ou ESPECÍFICO.
    #       .default = 1
    #     )
    #   )
    
    # Não tem a opção de ausência
    base <- base %>%
      mutate(
        abs_rd = ifelse(pres_lc == 0 & pres_mt == 0, 1, 0)
      )
    
  } else if (ano == 2016) {
    
    # # Modificações somente de 2016 
    # base <- base %>%
    #   mutate(
    #     # Não tem a opção de ausência
    #     abs_rd = case_when(
    #       status_rd == 4 ~ 0,
    #       # Entregou a redação em branco
    #       status_rd == 2 ~ 0,
    #       # Redação anulada
    #       status_rd == 1 ~ 0,
    #       # Presente à prova
    #       status_rd == 6 ~ 0,
    #       # Fuga ao tema
    #       status_rd == 8 ~ 0,
    #       # Texto insuficiente
    #       status_rd == 7 ~ 0,
    #       # Não atende ao tipo textual
    #       status_rd == 5 ~ 0,
    #       # Anulada - fere aos direitos humanos
    #       status_rd == 3 ~ 0,
    #       # Cópia de texto motivador
    #       status_rd == 9 ~ 0,
    #       # Parte do texto desconectada com o tema proposto
    #       .default = 1
    #     )
    #   )
    
    # Não tem a opção de ausência
    base <- base %>%
      mutate(
        abs_rd = ifelse(pres_lc == 0 & pres_mt == 0, 1, 0)
      )
    
  } else if(ano %in% c(2017, 2018, 2019)) {
    
    # # Modificações somente de 2017 a 2019 
    # base <- base %>%
    #   mutate(
    #     
    #     # 2017, 2018, 2019 - não tem a opção 5 e nem a opção de ausência
    #     abs_rd = case_when(
    #       status_rd == 4 ~ 0,                      # Entregou a redação em branco
    #       status_rd == 2 ~ 0,                      # Redação anulada
    #       status_rd == 1 ~ 0,                      # Presente à prova
    #       status_rd == 6 ~ 0,                      # Fuga ao tema
    #       status_rd == 8 ~ 0,                      # Texto insuficiente
    #       status_rd == 7 ~ 0,                      # Não atende ao tipo textual
    #       status_rd == 3 ~ 0,                     # Cópia de texto motivador
    #       status_rd == 9 ~ 0,                     # Parte do texto desconectada com o tema proposto
    #       .default = 1
    #     )
    #     
    #   )
    
    # Não tem a opção de ausência
    base <- base %>%
      mutate(
        abs_rd = ifelse(pres_lc == 0 & pres_ch == 0, 1, 0)
      )
  }
  
  
  # Abstenção nas provas objetivas --------------------------------------------#
  
  # Todos os anos    
  base <- base %>%
    mutate(
      
      abs_cn = case_when(
        pres_cn == 0 ~ 1,                          # Faltou à prova
        pres_cn == 1 ~ 0,                          # Presente na prova
        pres_cn == 2 ~ 0,                          # Eliminado na prova
        .default = NA
      ),
      abs_ch = case_when(
        pres_ch == 0 ~ 1,                          # Faltou à prova
        pres_ch == 1 ~ 0,                          # Presente na prova
        pres_ch == 2 ~ 0,                          # Eliminado na prova
        .default = NA
      ),
      abs_lc = case_when(
        pres_lc == 0 ~ 1,                          # Faltou à prova
        pres_lc == 1 ~ 0,                          # Presente na prova
        pres_lc == 2 ~ 0,                          # Eliminado na prova
        .default = NA
      ),
      abs_mt = case_when(
        pres_mt == 0 ~ 1,                          # Faltou à prova
        pres_mt == 1 ~ 0,                          # Presente na prova
        pres_mt == 2 ~ 0,                          # Eliminado na prova
        .default = NA
      )
      
    )
  
  # Sexo ----------------------------------------------------------------------#
  if(ano <= 2010 | ano >= 2013){
    
    base <- base %>%
      mutate(
        mas = ifelse(sexo == "M", 0, 1)
      )
    
  } else if (ano %in% c(2011,2012)) {
    
    base <- base %>%
      mutate(
        mas = sexo
      )
  }
  
  
  
  
  # Filtros e novas variáveis -------------------------------------------------#
  
  # Filtro geral
  base <- base %>%
    
    mutate(hv = ifelse(mun_prova %/% 100000 > 30, 1, 0)) %>%
    mutate(
      urb = ifelse(urb_rur == 1, 1, 0),
      priv = ifelse(dep_adm == 4, 1, 0),
      media = (cn + ch + lc + mt + rd) / 5,
      ano = ano
    ) %>%
    relocate(media, .after = rd) %>%
    relocate(ano, .before = idade) %>%
    relocate(mun_escola, .after = mun_prova)
  
  return(base)
  
}


filtros <- function(base,filtro) {
  
  # Filtro para base de regressões de notas - sem missing e valores inválidos
  if (filtro == "notas") {
    
    base <- base %>%
      filter(
        !is.na(cn) &
          !is.na(ch) &
          !is.na(lc) &
          !is.na(mt) &
          !is.na(rd) &   
          #!is.na(dep_adm) &
          #!is.na(urb_rur) &
          !is.na(sexo) &
          !is.na(raca) &
          !is.na(pessoas_dom) &
          !is.na(renda_dom) &
          !is.na(esc_pai) &
          esc_pai != "H" &
          raca != "G" &
          renda_dom != "E" &
          pessoas_dom != "D" #&
        #!is.na(mun_escola) &
        #!is.na(func_esc)
      )
    
  } else if(filtro == "abs") {
    
    # Filtro para base de regressões de abstenções
    if("treineiro" %in% colnames(enem)){
      
      base_a <- base %>%
        select(-c(cn,ch,lc,mt,rd,sexo,raca,pessoas_dom,renda_dom,
                  esc_pai,mun_escola,func_esc,rd1,rd2,rd3,rd4,rd5,co_cn,co_ch,co_lc,
                  co_mt,pres_cn,pres_ch,pres_lc,pres_mt,status_rd,treineiro))
      
    } else {
      
      base_a <- base %>%
        select(-c(cn,ch,lc,mt,rd,sexo,raca,pessoas_dom,renda_dom,
                  esc_pai,mun_escola,func_esc,rd1,rd2,rd3,rd4,rd5,co_cn,co_ch,co_lc,
                  co_mt,pres_cn,pres_ch,pres_lc,pres_mt,status_rd))
      
    }
    
    
  }
  
  return(base)
  
}

###5.1.2 Labels ----

dlist <- c("CN","CH","LC","MT")

vlist2016 <- c(
  "NU_INSCRICAO",
  paste0(rep("TX_RESPOSTAS_", times= length(dlist)), dlist),
  paste0(rep("CO_PROVA_", times= length(dlist)), dlist),
  paste0(rep("TX_GABARITO_", times= length(dlist)), dlist),
  "TP_LINGUA"
) 
vnames <- c(
  "id_enem",
  paste0(rep("tx_respostas_", times = length(dlist)),tolower(dlist)),
  paste0(rep("co_prova_", times = length(dlist)),tolower(dlist)),
  paste0(rep("tx_gabarito_", times = length(dlist)),tolower(dlist)),
  "lingua"
)
vlist_item_df <- data.frame(

  vl2018 = vlist2016,
  vl2019 = vlist2016,
  vnames = vnames
)
rm(vlist2009,vlist2010,vlist2011,vlist2012,vlist2013,vlist2015,vlist2016,vnames,dlist)


##5.2 inicio -----
# Loop no ano da prova
for (ano in 2009:2019) {
  
  temp <-
    fread(
      paste0(
        "Z:/Arquivos IFB/Enem/Parâmetros/itens_prova_",
        ano,
        ".csv"
      )
    ) %>% 
    rename_all(tolower) %>%
    mutate(ano = ano) %>%
    filter(in_item_aban == 0) %>%
    select(-in_item_aban, -tx_motivo_aban)
  
  
  if(ano == 2009) {
    itens_prova <- temp
  } else {
    itens_prova <- itens_prova %>% bind_rows(temp)
  }
  
  rm(temp)
  rm(ano)
}

itens_prova <- itens_prova %>%
  group_by(sg_area, co_prova) %>%
  mutate(
    pos_min = min(co_posicao),
    co_posicao = co_posicao - pos_min + 1
  ) %>%
  ungroup() %>%
  filter(is.na(tp_lingua)) %>%
  select(-pos_min,-tp_lingua,in_item_adaptado)


setDT(itens_prova)
medians <-
  itens_prova[, .(
    a_md = median(nu_param_a, na.rm = T),
    b_md = median(nu_param_b, na.rm = T),
    c_md = median(nu_param_c, na.rm = T)
  ), by = .(sg_area, ano, co_prova)]

itens_medias <-
  itens_prova[, .(
    a_md = mean(nu_param_a, na.rm = T),
    b_md = mean(nu_param_b, na.rm = T),
    c_md = mean(nu_param_c, na.rm = T)
  ), by = .(sg_area, ano)]

# CHECKLIST:
# 2009  OK
# 2010  OK
# 2011  OK
# 2012  OK
# 2013  OK
# 2014  OK
# 2015  OK
# 2016  OK
# 2017  OK
# 2018  OK
# 2019  OK

## a) 2009 ---

# Variáveis correspondentes:
# i.   Q15: qtd pessoas no domicílio
# ii.  Q21: renda domiciliar
# iii. Q3:  cor/raça
# iv.  Q17: escolaridade do pai

# Leitura da base de dados




# Base de dados com listas de variáveis e labels
vlist_df <- data.frame(
  vl2018 = c(
    "NU_INSCRICAO",
    "NU_IDADE",
    "TP_ST_CONCLUSAO",
    "TP_DEPENDENCIA_ADM_ESC",
    "TP_LOCALIZACAO_ESC",
    "CO_MUNICIPIO_PROVA",
    "CO_MUNICIPIO_RESIDENCIA",
    "SG_UF_PROVA",
    "NU_NOTA_CN",
    "NU_NOTA_CH",
    "NU_NOTA_LC",
    "NU_NOTA_MT",
    "NU_NOTA_REDACAO",
    "Q005",
    "Q006",
    "TP_SEXO",
    "TP_COR_RACA",
    "TP_SIT_FUNC_ESC",
    "Q001",
    "Q002",
    "Q003",
    "Q004",
    "CO_MUNICIPIO_ESC",
    "NU_NOTA_COMP1",
    "NU_NOTA_COMP2",
    "NU_NOTA_COMP3",
    "NU_NOTA_COMP4",
    "NU_NOTA_COMP5",
    "CO_PROVA_CN",
    "CO_PROVA_CH",
    "CO_PROVA_LC",
    "CO_PROVA_MT",
    "TP_PRESENCA_CN",
    "TP_PRESENCA_CH",
    "TP_PRESENCA_LC",
    "TP_PRESENCA_MT",
    "TP_STATUS_REDACAO",
    "IN_TREINEIRO"
  ),
  vl2019 = c(
    "NU_INSCRICAO",
    "NU_IDADE",
    "TP_ST_CONCLUSAO",
    "TP_DEPENDENCIA_ADM_ESC",
    "TP_LOCALIZACAO_ESC",
    "CO_MUNICIPIO_PROVA",
    "CO_MUNICIPIO_RESIDENCIA",
    "SG_UF_PROVA",
    "NU_NOTA_CN",
    "NU_NOTA_CH",
    "NU_NOTA_LC",
    "NU_NOTA_MT",
    "NU_NOTA_REDACAO",
    "Q005",
    "Q006",
    "TP_SEXO",
    "TP_COR_RACA",
    "TP_SIT_FUNC_ESC",
    "Q001",
    "Q002",
    "Q003",
    "Q004",
    "CO_MUNICIPIO_ESC",
    "NU_NOTA_COMP1",
    "NU_NOTA_COMP2",
    "NU_NOTA_COMP3",
    "NU_NOTA_COMP4",
    "NU_NOTA_COMP5",
    "CO_PROVA_CN",
    "CO_PROVA_CH",
    "CO_PROVA_LC",
    "CO_PROVA_MT",
    "TP_PRESENCA_CN",
    "TP_PRESENCA_CH",
    "TP_PRESENCA_LC",
    "TP_PRESENCA_MT",
    "TP_STATUS_REDACAO",
    "IN_TREINEIRO"
  ),
  vnames = c("id_enem",
             "idade",
             "conclusao",
             "dep_adm",
             "urb_rur",
             "mun_prova",
             "mun_res",
             "uf",
             "cn",
             "ch",
             "lc",
             "mt",
             "rd",
             "pessoas_dom",
             "renda_dom",
             "sexo",
             "raca",
             "func_esc",
             "esc_pai",
             "esc_mae",
             "emp_pai",
             "emp_mae",
             "mun_escola",
             "rd1",
             "rd2",
             "rd3",
             "rd4",
             "rd5",
             "co_cn",
             "co_ch",
             "co_lc",
             "co_mt",
             "pres_cn",
             "pres_ch",
             "pres_lc",
             "pres_mt",
             "status_rd",
             "treineiro"
  )
  
)


## 5.3 Regs de org -----



flist <- c(
  "Z:/Arquivos IFB/Enem/2018/DADOS/MICRODADOS_ENEM_2018.csv",
  "Z:/Arquivos IFB/Enem/2019/DADOS/MICRODADOS_ENEM_2019.csv"
)

for(i in 2018:2019){
  
  print(paste0("Ano: ",i))
  ini <- Sys.time()
  
  j <- i - 2013
  delta <- ifelse(i <= 2014,1,0)
  vlist <- vlist_df[1:(nrow(vlist_df) - delta),j]
  cnames <- vlist_df[1:(nrow(vlist_df) - delta),ncol(vlist_df)]
  rm(delta)
  
  if (i <= 2010 | i >= 2016){
    
    enem <-
      fread(file = flist[j],
            sep = ";",
            select = vlist)
    
  } else if(i >= 2011 & i <= 2015){
    enem <- read_dta(file = flist[j], col_select = vlist)
  }
  
  print(summary(enem$conclusao))
  
  enem <-
    org_data(
      base = enem,
      ano = i,
      vlist = vlist,
      cnames = cnames
    )
  
  print(summary(enem$conclusao))
  
  enem_notas <- filtros(base = enem, filtro = "notas")
  print(summary(enem_notas$conclusao))
  
  enem_abs <- filtros(base = enem, filtro = "abs")
  
  saveRDS(enem_notas, file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/enem_notas_",i,"_v4.RDS"))
  saveRDS(enem_abs, file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/enem_abs_",i,"_v4.RDS"))
  
  delta_t <- Sys.time() - ini
  print(delta_t)
  rm(enem,i,j,cnames,vlist,enem_notas,enem_abs, ini,delta_t)
}

#------------------------------------------------------------------------------#
# Base de itens do ENEM
#------------------------------------------------------------------------------#

# Lista de disciplinas
dlist <- c("cn","ch","lc","mt")

# Loop nos anos
gc()
for(ano in 2018:2019){
  
  print(paste0("Ano: ",ano))
  ini <- Sys.time()
  
  # Base de IDs para selecionar alunos
  enem <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/enem_notas_",ano,"_v4.RDS")) 
  id <- enem %>% select(id_enem)
  rm(enem)
  
  # Especificações da base de dados
  j <- ano - 2013
  delta <- ifelse(ano == 2009, 1, 0)
  vlist <- vlist_item_df[1:(nrow(vlist_item_df) - delta),j]
  cnames <- vlist_item_df[1:(nrow(vlist_item_df) - delta),ncol(vlist_item_df)]
  
  # Bases de dados em CSV e em DTA, dependendo do ano
  print(paste0("Abrindo a base: ",ano))
  if (ano <= 2010 | ano >= 2016){
    
    item <-
      fread(file = flist[j],
            sep = ";",
            select = vlist)
    
  } else if(ano >= 2011 & ano <= 2015){
    item <- read_dta(file = flist[j], col_select = vlist)
  }
  
  delta_t <- Sys.time() - ini
  print(delta_t)
  rm(delta_t)
  gc()
  
  # Organização da base de dados
  item <- item %>% relocate(all_of(vlist))
  colnames(item) <- cnames
  rm(vlist,cnames,delta)
  
  # Reorganizando a base (reshape)
  if (ano == 2009){
    vlist <- "id_enem"
  } else {
    vlist <- c("id_enem","lingua")
  }
  
  item <- item %>%
    right_join(id, by = "id_enem") %>%
    setDT() %>%
    melt(
      id.vars = vlist,
      measure.vars = list(
        paste0("tx_respostas_", dlist),
        paste0("co_prova_", dlist),
        paste0("tx_gabarito_", dlist)
      ),
      value.name = c("tx_respostas", "co_prova", "tx_gabarito")
    ) %>%
    mutate(
      sg_area = case_when(
        variable == "1" ~ toupper(dlist[1]),
        variable == "2" ~ toupper(dlist[2]),
        variable == "3" ~ toupper(dlist[3]),
        variable == "4" ~ toupper(dlist[4])
      )
    )
  
  rm(id,vlist)
  gc()
  # Loop nas áreas
  for(i in 1:length(dlist)){
    
    print(paste0("Área: ",dlist[i]))
    delta_t <- Sys.time() - ini
    print(delta_t)
    rm(delta_t)
    
    # Selecionando área para DF local
    temp <- item[sg_area == toupper(dlist[i]),]
    
    # Excluindo área do DF geral de itens
    item <- item[sg_area != toupper(dlist[i]),]
    
    # Língua estrangeira
    if(dlist[i] == "lc" & ano >= 2010) {
      
      if(ano == 2011) {
        temp <-
          temp[, 
               tx_gabarito := substr(tx_gabarito, 6, 45)]
      } else {
        temp <-
          temp[, 
               tx_gabarito := substr(tx_gabarito, 11, 50)]
      }
      
      
      temp <-
        temp[, 
             tx_respostas := substr(tx_respostas, 6, 45)]
      
      temp <- temp[, lingua := NULL]
      print("ok")
    }
    
    temp <- temp[, nc := nchar(tx_respostas)]
    nc <- temp %>% summarise(max = max(nc)) %>% as.numeric()
    
    temp <- temp[, paste0("resp", c(1:nc)) := tstrsplit(tx_respostas, "", fixed = TRUE)]
    temp <- temp[, paste0("gab", c(1:nc)) := tstrsplit(tx_gabarito, "", fixed = TRUE)]
    temp <- temp[, c("tx_respostas","tx_gabarito") := NULL]
    
    # Reorganizando o Df local de itens
    temp <- temp %>%
      melt(id.vars = c("id_enem","co_prova","sg_area"),
           measure.vars = list(
             paste0("resp", c(1:nc)),
             paste0("gab", c(1:nc))
           ),
           value.name = c("tx_respostas","tx_gabarito")
      ) %>%
      mutate(co_posicao = as.numeric(as.character(variable))) %>%
      select(-variable)
    
    rm(nc)
    
    # Dummies de acerto, resposta em branco e dupla resposta
    temp <- temp[, acerto := fifelse(tx_respostas == tx_gabarito, 1, 0)]
    temp <- temp[, branco := fifelse(tx_respostas == ".", 1, 0)]
    temp <- temp[, dupla := fifelse(tx_respostas == "*", 1, 0)]
    
    # Variável de ano
    temp <- temp[, ano := ano]
    
    print(paste0("Juntando bases de parâmetros"))
    delta_t <- Sys.time() - ini
    print(delta_t)
    rm(delta_t)
    
    # Juntando base de itens com base de medianas de parâmetros
    temp <- temp %>% merge(medians, by = c("sg_area","ano","co_prova"))
    
    # Juntando base de itens com base de parâmetros
    temp <- temp %>% merge(itens_prova, by = c("ano","co_posicao","co_prova","sg_area","tx_gabarito"))
    #temp %>% setorder(cols = "id_enem","sg_area","co_posicao") %>% slice(1:100) %>% View()
    
    # Excluindo itens sem parâmetros
    #temp <- temp[!is.na(nu_param_a),]
    
    # Posições mínima e máxima
    temp <- temp[, c("pmin","pmax") := 
                   .(min(co_posicao),
                     max(co_posicao))]
    
    #temp %>% setorder(cols = "id_enem","sg_area") %>% slice(1:100) %>% View()
    
    # Construindo dummies de acertos
    # vnames <- c(paste0("acertos_",rep(c("a","b","c"), each = 2),c("l","h")),
    #             paste0("acertos_",rep(c("ini","fim"), times = 2),rep(c("5","10"), each = 2)))
    
    temp <- temp[, c(paste0("acerto_",rep(c("a","b","c"), each = 2),c("l","h")),
                     paste0("acerto_",rep(c("ini","fim"), times = 2),rep(c("5","10"), each = 2))) :=
                   .(
                     fifelse(nu_param_a < a_md, acerto, NA_real_),
                     fifelse(nu_param_a >= a_md, acerto, NA_real_),
                     fifelse(nu_param_b < b_md, acerto, NA_real_),
                     fifelse(nu_param_b >= b_md, acerto, NA_real_),
                     fifelse(nu_param_c < c_md, acerto, NA_real_),
                     fifelse(nu_param_c >= c_md, acerto, NA_real_),
                     fifelse(co_posicao <= pmin + 4, acerto, NA_real_),
                     fifelse(co_posicao >= pmax - 4, acerto, NA_real_),
                     fifelse(co_posicao <= pmin + 9, acerto, NA_real_),
                     fifelse(co_posicao >= pmax - 9, acerto, NA_real_)
                   )]
    # rm(vnames)
    
    # Seleção de variáveis
    idvlist <- c("id_enem","sg_area","co_prova")
    vlist <- c(
      "acerto",
      "acerto_al",
      "acerto_ah",
      "acerto_bl",
      "acerto_bh",
      "acerto_cl",
      "acerto_ch",
      "acerto_ini5",
      "acerto_fim5",
      "acerto_ini10",
      "acerto_fim10",
      "branco",
      "dupla"
    )
    
    gc()
    # temp <- temp[, .SD, .SDcols = c(idvlist,vlist)]
    # temp <- temp %>% select(idvlist,vlist)
    
    # Número de observações
    # temp_obs <- temp[, lapply(.SD, function(x) sum(!is.na(x))),
    #              by = .(id_enem, sg_area),
    #              .SDcols = vlist]
    
    # colnames(temp_obs) <- paste0("n_",vlist)
    
    print("Agregação")
    
    temp <- temp[, lapply(.SD, mean, na.rm = T),
                 by = .(id_enem, sg_area),
                 .SDcols = vlist]
    
    # temp <- temp %>% merge(temp_obs, by = idvlist)
    
    rm(idvlist, vlist)
    #temp2 %>% setorder(cols = "id_enem","sg_area") %>% slice(1:100) %>% View()
    
    
    # Armazenado base de dados
    if(i == 1){
      enem_ace <- temp
    } else {
      enem_ace <- enem_ace %>% bind_rows(temp)
    }
    rm(i, temp)
    gc()
    
  } # fim do loop nas disciplinas (i)
  
  print("Fim do loop nas disciplinas")
  delta_t <- Sys.time() - ini
  print(delta_t)
  saveRDS(enem_ace, file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/enem_ace_",ano,"_v4.RDS"))
  rm(enem_ace, ini,delta_t,item,j)
  
  rm(ano)
  
} # fim do loop nos anos (ano)





# ---------------------------------------------------------------------------- #
## 5.4 Unindo todas as bases ----
# ---------------------------------------------------------------------------- #

# PIB
pib <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/pib.RDS") %>%
  mutate(codmun = as.integer(codmun)) %>%
  rename(mun_prova = codmun)

# Base de distâncias
mun_hv <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/mun_hv.RDS")

# Coordenadas
coordenadas <- mun_hv$centroid %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(
    lon = X,
    lat = Y
  )

mun_hv <- mun_hv %>%
  bind_cols(coordenadas) %>%
  st_drop_geometry() %>%
  select(co_municipio, lon, lat, dist_hv_border, seg)
rm(coordenadas)

# Bases de dados de todos os anos
for(ano in 2018:2019){
  
  print(ano)
  
  temp <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/enem_notas_",ano,"_v4.RDS")) %>%
    filtros(filtro = "notas")
  temp2 <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/enem_ace_",ano,"_v4.RDS"))%>%
    mutate(
      sg_area = tolower(sg_area)
    ) %>%
    dcast(id_enem ~ sg_area,
          value.var = c(
            "acerto",
            paste0(
              "acerto_",
              rep(c("a","b","c"), each = 2), rep(c("l","h"), times = 3)
            ),
            paste0(
              "acerto_",
              rep(c("ini","fim"), each = 2), rep(c("5","10"), times = 2)
            ),
            "branco","dupla"
          )
    )
  
  temp <- temp %>% merge(temp2, by = "id_enem", all.y = F,all.x = F)
  rm(temp2)
  
  temp <- temp %>%
    select(-sexo,-status_rd) %>%
    left_join(mun_hv,
              by = c(
                "mun_prova" = "co_municipio"
              ))  %>%
    mutate(
      dist_hv_border = ifelse(hv == 1, dist_hv_border,-dist_hv_border),
      dia_1 = case_when(
        ano >= 2009 & ano <= 2016 ~ (cn + mt) / 2,
        ano >= 2017 & ano <= 2019 ~ (lc + rd + ch) /3
      ),
      dia_2 = case_when(
        ano >= 2009 & ano <= 2016 ~ (lc + rd + ch) / 3,
        ano >= 2017 & ano <= 2019 ~ (cn + mt) /2
      ),
      uf_prova = mun_prova %/% 10^5,
      id180 = ifelse(idade != 18 & priv == 0, 1, NA),
      id181 = ifelse(idade == 18 & priv == 0, 1, NA),
      dom50 = ifelse(pessoas_dom != "C" & priv == 0, 1, NA),
      dom51 = ifelse(pessoas_dom == "C" & priv == 0, 1, NA),
      ppi0 = ifelse(!(raca %in% c("C","D","F")) & priv == 0, 1, NA),
      ppi1 = ifelse(raca %in% c("C","D","F") & priv == 0, 1, NA),
      escp0 = case_when(
        esc_pai %in% c("D","E","F") & priv == 0 ~ 1,
        esc_pai %in% c("A","B","C") & priv == 0 ~ NA,
        .default = NA
      ),
      escp1 = case_when(
        esc_pai %in% c("D","E","F") & priv == 0 ~ NA,
        esc_pai %in% c("A","B","C") & priv == 0 ~ 1,
        .default = NA
      ),
      renda10 = ifelse(!(renda_dom %in% c("A","B")) & priv == 0,1,NA),
      renda11 = ifelse(renda_dom %in% c("A","B") & priv == 0,1,NA),
      fem0 = ifelse(mas == 1 & priv == 0, 1, NA),
      fem1 = ifelse(mas == 0 & priv == 0, 1, NA),
      priv0 = ifelse(priv == 0, 1, NA),
      priv1 = ifelse(priv == 1, 1, NA),
      seg15 = ifelse(seg %in% c(1:5) & priv == 0, 1, NA),
      seg67 = ifelse(seg %in% c(6:7) & priv == 0, 1, NA),
      total = 1,
      nonmig1 = ifelse(
        !is.na(mun_prova) &
          !is.na(mun_res) &
          !is.na(mun_escola) &
          mun_prova == mun_res & mun_res == mun_escola & priv == 0,
        1,
        NA
      ),
      nonmig2 = ifelse(
        !is.na(mun_prova) &
          !is.na(mun_res) &
          !is.na(mun_escola) &
          mun_prova == mun_res & mun_res != mun_escola & priv == 0,
        1,
        NA),
      nonmig3 = ifelse(
        !is.na(mun_prova) &
          !is.na(mun_res) &
          !is.na(mun_escola) &
          mun_prova != mun_res & mun_res == mun_escola & priv == 0,
        1,
        NA),
      nonmig4 = ifelse(
        !is.na(mun_prova) &
          !is.na(mun_res) &
          !is.na(mun_escola) &
          mun_prova == mun_escola & mun_res != mun_escola & priv == 0,
        1,
        NA),
      id18 = ifelse(idade == 18, 1, 0),
      dom5 = ifelse(pessoas_dom == "C", 1, 0),
      ppi = ifelse(raca %in% c("C","D","F"), 1, 0),
      escp = case_when(
        esc_pai %in% c("D","E","F") ~ 0,
        esc_pai %in% c("A","B","C") ~ 1,
        .default = NA
      ),
      renda1 = ifelse(renda_dom %in% c("A","B"),1,0),
      fem = ifelse(mas == 0, 1, 0),
      rd0 = ifelse(rd == 0, 1, 0),
      rd1 = ifelse(rd0 == 1,NA,rd1),
      rd2 = ifelse(rd0 == 1,NA,rd2),
      rd3 = ifelse(rd0 == 1,NA,rd3),
      rd4 = ifelse(rd0 == 1,NA,rd4),
      rd5 = ifelse(rd0 == 1,NA,rd5),
      branco_ch = branco_ch * 100,
      branco_cn = branco_cn * 100,
      branco_lc = branco_lc * 100,
      branco_mt = branco_mt * 100,
      dupla_ch = dupla_ch * 100,
      dupla_cn = dupla_cn * 100,
      dupla_lc = dupla_lc * 100,
      dupla_mt = dupla_mt * 100,
      acerto_ch = acerto_ch * 100,
      acerto_cn = acerto_cn * 100,
      acerto_lc = acerto_lc * 100,
      acerto_mt = acerto_mt * 100,
      acerto_al_ch = acerto_al_ch * 100,
      acerto_al_cn = acerto_al_cn * 100,
      acerto_al_lc = acerto_al_lc * 100,
      acerto_al_mt = acerto_al_mt * 100,
      acerto_ah_ch = acerto_ah_ch * 100,
      acerto_ah_cn = acerto_ah_cn * 100,
      acerto_ah_lc = acerto_ah_lc * 100,
      acerto_ah_mt = acerto_ah_mt * 100,
      acerto_bl_ch = acerto_bl_ch * 100,
      acerto_bl_cn = acerto_bl_cn * 100,
      acerto_bl_lc = acerto_bl_lc * 100,
      acerto_bl_mt = acerto_bl_mt * 100,
      acerto_bh_ch = acerto_bh_ch * 100,
      acerto_bh_cn = acerto_bh_cn * 100,
      acerto_bh_lc = acerto_bh_lc * 100,
      acerto_bh_mt = acerto_bh_mt * 100,
      acerto_cl_ch = acerto_cl_ch * 100,
      acerto_cl_cn = acerto_cl_cn * 100,
      acerto_cl_lc = acerto_cl_lc * 100,
      acerto_cl_mt = acerto_cl_mt * 100,
      acerto_ch_ch = acerto_ch_ch * 100,
      acerto_ch_cn = acerto_ch_cn * 100,
      acerto_ch_lc = acerto_ch_lc * 100,
      acerto_ch_mt = acerto_ch_mt * 100,
      acerto = (acerto_ch + acerto_cn + acerto_lc + acerto_mt) / 4,
      acerto_pal = (acerto_al_ch + acerto_al_cn + acerto_al_lc + acerto_al_mt) / 4,
      acerto_pah = (acerto_ah_ch + acerto_ah_cn + acerto_ah_lc + acerto_ah_mt) / 4,
      acerto_pbl = (acerto_bl_ch + acerto_bl_cn + acerto_bl_lc + acerto_bl_mt) / 4,
      acerto_pbh = (acerto_bh_ch + acerto_bh_cn + acerto_bh_lc + acerto_bh_mt) / 4,
      acerto_pcl = (acerto_cl_ch + acerto_cl_cn + acerto_cl_lc + acerto_cl_mt) / 4,
      acerto_pch = (acerto_ch_ch + acerto_ch_cn + acerto_ch_lc + acerto_ch_mt) / 4,
      acerto_ini5_cn = acerto_ini5_cn * 100,
      acerto_ini5_ch = acerto_ini5_ch * 100,
      acerto_ini5_lc = acerto_ini5_lc * 100,
      acerto_ini5_mt = acerto_ini5_mt * 100,
      acerto_ini10_cn = acerto_ini10_cn * 100,
      acerto_ini10_ch = acerto_ini10_ch * 100,
      acerto_ini10_lc = acerto_ini10_lc * 100,
      acerto_ini10_mt = acerto_ini10_mt * 100,
      acerto_fim5_cn = acerto_fim5_cn * 100,
      acerto_fim5_ch = acerto_fim5_ch * 100,
      acerto_fim5_lc = acerto_fim5_lc * 100,
      acerto_fim5_mt = acerto_fim5_mt * 100,
      acerto_fim10_cn = acerto_fim10_cn * 100,
      acerto_fim10_ch = acerto_fim10_ch * 100,
      acerto_fim10_lc = acerto_fim10_lc * 100,
      acerto_fim10_mt = acerto_fim10_mt * 100,
      acerto_ini5_d1 = case_when(
        ano == 2009 ~ acerto_ini5_cn,
        ano >= 2010 & ano <= 2016 ~ acerto_ini5_ch,
        ano >= 2017 & ano <= 2019 ~ acerto_ini5_lc,
        .default = NA
      ),
      acerto_ini10_d1 = case_when(
        ano == 2009 ~ acerto_ini10_cn,
        ano >= 2010 & ano <= 2016 ~ acerto_ini10_ch,
        ano >= 2017 & ano <= 2019 ~ acerto_ini10_lc,
        .default = NA
      ),
      acerto_fim5_d1 = case_when(
        ano == 2009 ~ acerto_fim5_ch,
        ano >= 2010 & ano <= 2016 ~ acerto_fim5_cn,
        ano >= 2017 & ano <= 2019 ~ acerto_fim5_ch,
        .default = NA
      ),
      acerto_fim10_d1 = case_when(
        ano == 2009 ~ acerto_fim10_ch,
        ano >= 2010 & ano <= 2016 ~ acerto_fim5_cn,
        ano >= 2017 & ano <= 2019 ~ acerto_fim5_ch,
        .default = NA
      ),
      acerto_ini5_d2 = case_when(
        ano == 2009 ~ acerto_ini5_lc,
        ano >= 2010 & ano <= 2016 ~ acerto_ini5_lc,
        ano >= 2017 & ano <= 2019 ~ acerto_ini5_cn,
        .default = NA
      ),
      acerto_ini10_d2 = case_when(
        ano == 2009 ~ acerto_ini10_lc,
        ano >= 2010 & ano <= 2016 ~ acerto_ini10_lc,
        ano >= 2017 & ano <= 2019 ~ acerto_ini10_cn,
        .default = NA
      ),
      acerto_fim5_d2 = case_when(
        ano == 2009 ~ acerto_fim5_mt,
        ano >= 2010 & ano <= 2016 ~ acerto_fim5_mt,
        ano >= 2017 & ano <= 2019 ~ acerto_fim5_mt,
        .default = NA
      ),
      acerto_fim10_d2 = case_when(
        ano == 2009 ~ acerto_fim10_mt,
        ano >= 2010 & ano <= 2016 ~ acerto_fim10_mt,
        ano >= 2017 & ano <= 2019 ~ acerto_fim10_mt,
        .default = NA
      ),
      acerto_ini5 = (acerto_ini5_cn + acerto_ini5_ch + acerto_ini5_lc + acerto_ini5_mt)/4,
      acerto_fim5 = (acerto_fim5_cn + acerto_fim5_ch + acerto_fim5_lc + acerto_fim5_mt)/4,
      d_acerto_5 = acerto_fim5 - acerto_ini5,
      d_acerto_5_d1 = acerto_fim5_d1 - acerto_ini5_d1,
      d_acerto_5_d2 = acerto_fim5_d2 - acerto_ini5_d2,
      d_acerto_10_d1 = acerto_fim10_d1 - acerto_ini10_d1,
      d_acerto_10_d2 = acerto_fim10_d2 - acerto_ini10_d2,
      d_pb_ch = acerto_bl_ch - acerto_bh_ch,
      d_pb_cn = acerto_bl_cn - acerto_bh_cn,
      d_pb_lc = acerto_bl_lc - acerto_bh_lc,
      d_pb_mt = acerto_bl_mt - acerto_bh_mt,
      d_pb = acerto_pbl - acerto_pbh,
      d_acerto_5ch = acerto_fim5_ch - acerto_ini5_ch,
      d_acerto_5cn = acerto_fim5_cn - acerto_ini5_cn,
      d_acerto_5lc = acerto_fim5_lc - acerto_ini5_lc,
      d_acerto_5mt = acerto_fim5_mt - acerto_ini5_mt
    ) %>%
    merge(pib, by = c("mun_prova", "ano"))
  
  setDT(temp)
  vpad <- temp[, .(
    media_p = pad(media),
    cn_p = pad(cn),
    ch_p = pad(ch),
    lc_p = pad(lc),
    mt_p = pad(mt),
    rd_p = pad(rd),
    dia_1_p = pad(dia_1),
    dia_2_p = pad(dia_2),
    rd1_p = pad(rd1),
    rd2_p = pad(rd2),
    rd3_p = pad(rd3),
    rd4_p = pad(rd4),
    rd5_p = pad(rd5)
  )]
  
  temp <- temp %>% bind_cols(vpad)
  rm(vpad)
  
  base_nota <- temp %>% setDT()
  
  rm(temp)
  
  
  gc()
  

  saveRDS(base_nota, file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/base_nota_",ano,".RDS"))
  rm(base_nota)
  
  # Base de abstenções
  base_abs <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/enem_abs_",ano,"_v4.RDS")) %>%
    select(-sexo,-status_rd) %>%
    left_join(mun_hv,
              by = c(
                "mun_prova" = "co_municipio"
              )) %>%
    mutate(
      priv = ifelse(dep_adm == 4, 1, 0),
      priv0 = ifelse(priv == 0, 1, NA)
    ) %>%
    select(
      -conclusao,
      -cn,
      -ch,
      -lc,
      -mt,
      -rd,
      -media,
      -rd1,
      -rd2,
      -rd3,
      -rd4,
      -rd5,
      -co_cn,
      -co_ch,
      -co_lc,
      -co_mt,
      -pres_cn,
      -pres_ch,
      -pres_lc,
      -pres_mt
      ,
      -dep_adm
    ) %>%
    mutate(
      abs = ifelse(
        abs_rd == 1 &
          abs_cn == 1 &
          abs_ch == 1 &
          abs_lc == 1 &
          abs_mt == 1,
        1,
        0),
      uf_prova = mun_prova %/% 10^5,
      dist_hv_border = ifelse(hv == 1, dist_hv_border,-dist_hv_border)
    )
  
  
  
  
  base_abs <- base_abs %>% 
    
    filter(
      !is.na(abs_rd) & !is.na(abs_mt) & !is.na(abs_lc) & !is.na(abs_cn) & !is.na(abs_ch)
    ) %>% 
    
    mutate(
      
      in_d1 = ifelse(abs_rd == 0 & abs_ch == 0 & abs_lc == 0, 1, 0),
      
      in_d2 = ifelse(abs_mt == 0 & abs_cn == 0, 1, 0)
    ) %>% 
    mutate(
      first_n_last = ifelse(in_d1 == 1 & in_d2 == 0, 1, 0),
      last_n_first = ifelse(in_d1 == 0 & in_d2 == 1, 1, 0)
    )
  
  
  base_ab <- base_abs[,.(media_abs = mean(abs, na.rm = T),
                         in_d1_nd2 = mean(first_n_last, na.rm = T),
                         in_d2_nd1 = mean(last_n_first, na.rm = T),
                         obs = .N),
                      by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] %>% 
    filter(as.numeric(ano) %in% c(2018,2019)) %>% 
    arrange(mun_prova,ano)
  
  
  
  
  
  
  saveRDS(base_ab, file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/base_abs_mun_",ano,".RDS"))
  rm(base_abs)
  rm(ano)
  
}

rm(mun_hv,pib)