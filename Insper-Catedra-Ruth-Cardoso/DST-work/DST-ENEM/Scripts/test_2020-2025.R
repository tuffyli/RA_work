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
library(rdrobust)
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
library(fixest)

#test


vlist <- c("")

enem <- fread(file = "Z:/Arquivos IFB/Enem/2020/DADOS/MICRODADOS_ENEM_2020.csv",
              sep = ";") 

enem <- enem %>% 
  rename_with(tolower) %>% 
  filter(tp_st_conclusao == 2) %>% 
  filter(tp_escola == 3) %>%  #Publica
  filter(tp_presenca_mt == 1 & tp_presenca_lc == 1 & tp_presenca_ch == 1 &
           tp_presenca_cn == 1 & tp_status_redacao != 2) %>% 
  mutate(media = (nu_nota_mt + nu_nota_lc + nu_nota_ch + nu_nota_mt + nu_nota_redacao)/5 )


enem_ag <- enem %>% 
  group_by(co_municipio_prova) %>% 
  summarise(
    media = mean(media, na.rm = T),
    obs = n(),
    .groups = "drop"
  )


mun_hv <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/mun_hv.RDS")
  
mun_hv <- mun_hv %>% 
  mutate(
    dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)
  )

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

enem_ag <- enem_ag %>% 
  left_join(mun_hv %>% select(co_municipio, seg, lat, lon, dist_hv_border), by = c("co_municipio_prova" = "co_municipio") )



load("Z:/Tuffy/Paper - HV/Resultados/bandwidths_2019_2018_NF.RData")


enem_ag <- enem_ag %>% 
  filter(abs(dist_hv_border) <= bw_main_a) #selecionando apenas os municípios dentro da janela ótima

c_rlist <- list()

#Com controles no ano de referência
ef <- dummy_cols(enem_ag$seg) #EF de segmento
ef <- ef %>% select(-1,-2)

c_rlist[[as.character(paste0("C|TC"))]] <- rdrobust( #Resultado armazenado na lista
  y = enem_ag$media, #nota media no município
  x = enem_ag$dist_hv_border, #RV de distância
  c = 0, #Ponto de corte
  p =1,
  h = bw_main_a, #Limitando para a banda ótima de ref.
  b = bw_bias_a, #Limitando para a banda ótima de ref.
  cluster = enem_ag$seg, #cluster no segmento
  weights = enem_ag$obs, #peso nas amostras
  vce = "hc0", 
  covs = cbind( #covariadas
    ef,
    enem_ag$lat,
    enem_ag$lon
  )
)

t10cc <- data.frame(
  coef = do.call(rbind,lapply(c_rlist, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(c_rlist, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(c_rlist, FUN = function(x){x$pv[3]})),
  n = do.call(rbind, lapply(c_rlist, FUN = function(x){x$N_h})),
  t = do.call(rbind, lapply(c_rlist, FUN = function(x){x$N}))
) %>% 
  mutate(
    N = n.1 + n.2,
    Tot = t.1 + t.2,
    ano = 2013:2019,
    ymin = coef - 1.96 * se,
    ymax = coef + 1.96 * se
  ) %>% 
  select(-c(n.1, n.2))

t10cc
