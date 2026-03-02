# ---------------------------------------------------------------------------- #
# Regressions
# Main estimations and Robustness
# Last edited by: Tuffy Licciardi Issa
# Date: 02/03/2026
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Libraries -----
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# 1. Início ----
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

# ---------------------------------------------------------------------------- #
## 1.0 Data -----
# ---------------------------------------------------------------------------- #


base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2019.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2018.RDS"))) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2017.RDS"))) %>%
  setDT() %>% 
  filter(conclusao == 2)

#Exam start time dummies
base <- base %>% 
  mutate(
    h13 = case_when( #Prova iniciando as 13h
      ano %in% c(2017:2019) &
        uf %in% c("GO", "DF", "MG", "ES", "RJ", "SP", "SC", "PR", "RS") ~ 1,
      
      ano == 2019 &
        uf %in% c("GO", "DF", "MG", "ES", "RJ", "SP", "SC", "PR", "RS", "BA", "SE",
                  "AL", "PE", "PB", "RN", "CE","PI", "MA", "TO", "PA", "AP") ~ 1,
      
      TRUE ~ 0
    ),
    
    h12 = case_when( #Prova iniciando as 12h
      ano %in% c(2017, 2018) &
        uf %in% c("BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI", "MA", "TO", "MT",
                  "MS", "PA", "AP") ~ 1,
      
      ano == 2019 &
        uf %in% c("MT", "MS", "RO", "RR") | uf == "AM" & !mun_prova %in%
        c(1300201, 1300607, 1300706, 1301407, 1301506, 1301654, 1301803, 1301951,
          1302306, 1302405, 1303502, 1303908, 1304062) ~ 1,
      
      TRUE ~ 0
    ),
    
    h11 = case_when( #Prova iniciando as 12h
      ano %in% c(2017, 2018) &
        uf %in% c("RO", "RR") | uf == "AM" & !mun_prova %in% 
        c(1300201, 1300607, 1300706, 1301407, 1301506, 1301654, 1301803, 1301951,
          1302306, 1302405, 1303502, 1303908, 1304062) ~ 1,
      
      ano == 2019 &
        uf == "AC" | mun_prova %in% c(1300201, 1300607, 1300706, 1301407, 1301506,
                                      1301654, 1301803, 1301951, 1302306, 1302405,
                                      1303502, 1303908, 1304062) ~ 1,
      
      TRUE ~ 0
    ),
    
    h10 = case_when( #Prova iniciando as 12h
      ano %in% c(2017, 2018) &
        uf == "AC" | mun_prova %in% c(1300201, 1300607, 1300706, 1301407, 1301506,
                                      1301654, 1301803, 1301951, 1302306, 1302405,
                                      1303502, 1303908, 1304062) ~ 1,
      
      TRUE ~ 0
    ),
    
    
    new_dist = dist_hv_border/100000)



summary(base %>% select(idade, conclusao, esc_mae))

test <- base %>% 
  filter(priv0 == 1)

length(unique(test$id_enem))
rm(test)


base <- base %>% 
  mutate(
    
    escm = case_when(
      esc_mae %in% c("D","E","F") ~ 1, #With high school
      esc_mae %in% c("A","B","C") ~ 0,
      .default = NA),
    
    escp = case_when(
      esc_pai %in% c("D","E","F") ~ 1, #With High schooç
      esc_pai %in% c("A","B","C") ~ 0,
      .default = NA
    ),
    
    mae_trab_man = case_when(
      emp_mae %in% c("A","B","C") ~ 1,
      emp_mae %in% c("D","E") ~ 0,
      .default = NA
    ),
    
    pai_trab_man = case_when(
      emp_pai %in% c("A","B","C") ~ 1,
      emp_pai %in% c("D","E") ~ 0,
      .default = NA
    )
  ) %>% 
  select(-c(co_cn, co_ch, co_lc, co_mt,
            pres_cn, pres_ch, pres_lc, pres_mt,
                  abs_rd, abs_cn, abs_ch, abs_lc, abs_mt,
            mas, urb, priv,
            acerto_ch, acerto_lc, acerto_mt, acerto_cn,
            acerto_ini5_ch, acerto_ini5_cn, acerto_ini5_lc, acerto_ini5_mt,
            acerto_ini10_ch, acerto_ini10_cn, acerto_ini10_lc, acerto_ini10_mt,
            acerto_fim5_ch, acerto_fim5_cn, acerto_fim5_lc, acerto_fim5_mt,
            acerto_fim10_ch, acerto_fim10_cn, acerto_fim10_lc, acerto_fim10_mt,
            branco_ch, branco_cn, branco_lc, branco_mt, dupla_mt,
            dupla_ch, dupla_cn, dupla_lc, total,
            
            #Only param. B captures difficulty
            acerto_pal, acerto_pah, acerto_pcl, acerto_pch,
            
            acerto_ini5_d1, acerto_ini10_d1, acerto_fim5_d1, acerto_fim10_d1,
            acerto_ini5_d2, acerto_ini10_d2, acerto_fim5_d2, acerto_fim10_d2,
            141:165, #acertos
            167:179 #padronizadas
            )) 


# ---------------------------------------------------------------------------- #
### 1.0.1 INPE ----
# ---------------------------------------------------------------------------- #

inpe18 <- readRDS("Z:/Tuffy/Paper - HV/Bases/inpe/mun/inpe_mun_2018.rds")
inpe19 <- readRDS("Z:/Tuffy/Paper - HV/Bases/inpe/mun/inpe_mun_2019.rds")

base <- base %>% 
  mutate(
    temp_d1 = case_when(
      ano == 2019 ~ inpe19$temp_3[match(mun_prova, inpe19$codmun)],
      ano == 2018 ~ inpe18$temp_4[match(mun_prova, inpe18$codmun)],
      TRUE ~ NA
    ),
    
    temp_d2 = case_when(
      ano == 2019 ~ inpe19$temp_10[match(mun_prova, inpe19$codmun)],
      ano == 2018 ~ inpe18$temp_11[match(mun_prova, inpe18$codmun)],
      TRUE ~ NA
    ),
    
    umid_d1 = case_when(
      ano == 2019 ~ inpe19$umid_3[match(mun_prova, inpe19$codmun)],
      ano == 2018 ~ inpe18$umid_4[match(mun_prova, inpe18$codmun)],
      TRUE ~ NA
    ),
    
    umid_d2 = case_when(
      ano == 2019 ~ inpe19$umid_10[match(mun_prova, inpe19$codmun)],
      ano == 2018 ~ inpe18$umid_11[match(mun_prova, inpe18$codmun)],
      TRUE ~ NA
    )
  )

rm(inpe18, inpe19)
# ---------------------------------------------------------------------------- #
### 1.0.2 Dist ----
# ---------------------------------------------------------------------------- #

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
  select(co_municipio, lon, lat, dist_hv_border, seg, hv)
rm(coordenadas)


mun_hv <- mun_hv %>%
  mutate(dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>% 
  select(co_municipio, dist_hv_border, lat, lon, seg) 

#Res. Dist.
base <- base %>% 
  left_join(mun_hv %>% rename(dist_hv_res = dist_hv_border) %>% #Coordinates of Residency 
              rename(lat_res = lat, 
                     lon_res = lon,
                     seg_res = seg)
            , by = c("mun_res" = "co_municipio")) #%>% 

#School Dist.
base <- base %>% 
  mutate(dist_hv_esc = case_when(
    nonmig1 == 1 ~ dist_hv_border,
    nonmig3 == 1 ~ dist_hv_res,
    nonmig4 == 1 ~ dist_hv_border,
    TRUE ~ mun_hv$dist_hv_border[match(mun_escola, mun_hv$co_municipio)]),
    
    lat_esc = case_when(
      nonmig1 == 1 ~ lat,
      nonmig3 == 1 ~ lat_res,
      nonmig4 == 1 ~ lat,
      TRUE ~ mun_hv$lat[match(mun_escola, mun_hv$co_municipio)]),
    
    lon_esc = case_when(
      nonmig1 == 1 ~ lon,
      nonmig3 == 1 ~ lon_res,
      nonmig4 == 1 ~ lon,
      TRUE ~ mun_hv$lon[match(mun_escola, mun_hv$co_municipio)]),
    
    seg_esc = case_when(
      nonmig1 == 1 ~ seg,
      nonmig3 == 1 ~ seg_res,
      nonmig4 == 1 ~ seg,
      TRUE ~ mun_hv$seg[match(mun_escola, mun_hv$co_municipio)]),
    
  )

rm(mun_hv)

# ---------------------------------------------------------------------------- #
## 1.1 Base agregada ----
# ---------------------------------------------------------------------------- #
base_a <- base[priv0 == 1,.(media = mean(media, na.rm = T),
                            idade = mean(id18, na.rm = T),
                            temp_d1 = mean(temp_d1, na.rm = T),
                            h13 = first(h13),
                            h12 = first(h12),
                            h11 = first(h11),
                            h10 = first(h10),
                            obs = .N),
               by = .(mun_prova,ano,dist_hv_border,new_dist,seg,lat,lon)] %>% 
  filter(as.numeric(ano) %in% c(2018,2019)) %>% 
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2018, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    d.media = media - v2_nota,
    
    v1idade = ifelse(ano == 2018, idade, NA),
    v2idade = max(v1idade, na.rm = T),
    didade = idade - v2idade,
    
    v1temp = ifelse(ano == 2018, temp_d1, NA),
    v2temp = max(v1temp, na.rm = T),
    dtemp  = temp_d1 - v2temp
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_nota, v2_nota, v1idade, v2idade, v1temp, v2temp)) %>% 
  group_by(mun_prova) %>% 
  mutate(
    d.h13 = h13[ano == 2019] + h13[ano == 2018], #2 = Manteve, 1 mudou, 0 nunca
    d.h12 = h12[ano == 2019] + h12[ano == 2018],
    d.h11 = h11[ano == 2019] + h11[ano == 2018],
    d.h10 = h10[ano == 2019] + h10[ano == 2018]
  )

### 1.1.1 Controles ----
ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
ef <- ef %>% select(-1,-2)

list <- list()

## A. Sem Filtro de Idade ----
list[[as.character(paste0(2019,"-",2018,"C|NF"))]] <- rdrobust(
  y = base_a$d.media[base_a$ano == 2019],
  x = base_a$dist_hv_border[base_a$ano == 2018],
  c = 0,
  cluster = base_a$seg[base_a$ano == 2018],
  weights = base_a$obs[base_a$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_a$lat[base_a$ano == 2018],
    base_a$lon[base_a$ano == 2018]
  )
)




# ---------------------------------------------------------------------------- #
#Extração da banda ótima
bw_main_a  <- list[["2019-2018C|NF"]]$bws[1]
bw_bias_a  <- list[["2019-2018C|NF"]]$bws[2]

#Salvando a banda principal
save(bw_main_a, bw_bias_a,
     file = "Z:/Tuffy/Paper - HV/Resultados/bandwidths_2019_2018_NF.RData")
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
### 1.1.2 Fuso ----
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2019,"-",2018,"|fuso"))]] <- rdrobust(
  y = base_a$d.media[base_a$ano == 2019],
  x = base_a$dist_hv_border[base_a$ano == 2018],
  c = 0,
  cluster = base_a$seg[base_a$ano == 2018],
  weights = base_a$obs[base_a$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_a$lat[base_a$ano == 2018],
    base_a$lon[base_a$ano == 2018],
    base_a$h13[base_a$ano == 2019],
    base_a$h12[base_a$ano == 2019],
    base_a$h11[base_a$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
### 1.1.3 Fuso + C ----
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2019,"-",2018,"|fuso+C"))]] <- rdrobust(
  y = base_a$d.media[base_a$ano == 2019],
  x = base_a$dist_hv_border[base_a$ano == 2018],
  c = 0,
  cluster = base_a$seg[base_a$ano == 2018],
  weights = base_a$obs[base_a$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_a$lat[base_a$ano == 2018],
    base_a$lon[base_a$ano == 2018],
    base_a$dtemp[base_a$ano == 2019],
    base_a$didade[base_a$ano == 2019],
    base_a$h13[base_a$ano == 2019],
    base_a$h12[base_a$ano == 2019],
    base_a$h11[base_a$ano == 2019]
  )
)


# ---------------------------------------------------------------------------- #
### 1.1.4 Pol ----
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2019,"-",2018,"|pol"))]] <- rdrobust(
  y = base_a$d.media[base_a$ano == 2019],
  x = base_a$dist_hv_border[base_a$ano == 2018],
  c = 0,
  p = 2, 
  cluster = base_a$seg[base_a$ano == 2018],
  weights = base_a$obs[base_a$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_a$lat[base_a$ano == 2018],
    base_a$lon[base_a$ano == 2018]
  )
)

# ---------------------------------------------------------------------------- #
### 1.1.5 Pol + Fuso ----
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2019,"-",2018,"|pol+fuso"))]] <- rdrobust(
  y = base_a$d.media[base_a$ano == 2019],
  x = base_a$dist_hv_border[base_a$ano == 2018],
  c = 0,
  p = 2, 
  cluster = base_a$seg[base_a$ano == 2018],
  weights = base_a$obs[base_a$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_a$lat[base_a$ano == 2018],
    base_a$lon[base_a$ano == 2018],
    base_a$h13[base_a$ano == 2019],
    base_a$h12[base_a$ano == 2019],
    base_a$h11[base_a$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
### 1.1.6 Pol + Fuso + Cont ----
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2019,"-",2018,"|pol+fuso+C"))]] <- rdrobust(
  y = base_a$d.media[base_a$ano == 2019],
  x = base_a$dist_hv_border[base_a$ano == 2018],
  c = 0,
  p = 2, 
  cluster = base_a$seg[base_a$ano == 2018],
  weights = base_a$obs[base_a$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_a$lat[base_a$ano == 2018],
    base_a$lon[base_a$ano == 2018],
    base_a$dtemp[base_a$ano == 2019],
    base_a$didade[base_a$ano == 2019],
    base_a$h13[base_a$ano == 2019],
    base_a$h12[base_a$ano == 2019],
    base_a$h11[base_a$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
### 1.7.1 Tabela  ----
# ---------------------------------------------------------------------------- #

t10 <- data.frame(
  coef = do.call(rbind,lapply(list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(list, FUN = function(x){x$pv[3]})),
  n = do.call(rbind,lapply(list, FUN = function(x){x$N_h}))
)
print(t10)


t10 <- t10 %>%
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "**", 
                         ifelse(pv < 0.05, "*", 
                                ifelse(pv < 0.1, "", "")
                         ))),
    se =  paste0("(", formatC(x = se, digits = 2, format = "f"), ")"),
    N = paste0("[N = ", n.1 + n.2, "]")
  ) %>%
  select( coef, se, N ) %>%
  setDT()



names <- c("2019 - 2018",
           " "," ")

result <- data.frame(
  var = names,
  con = rep(NA, times = length(names)),
  fus = rep(NA, times = length(names)),
  mor = rep(NA, times = length(names)),
  po2 = rep(NA, times = length(names)),
  pfu = rep(NA, times = length(names)),
  pfc = rep(NA, times = length(names))
)

# #TC Segmentos
# result$seg[1] <- t10$coef[[2]]
# result$seg[2] <- t10$se[[2]]
# result$seg[3] <- t10$N[[2]]

#TC Controles
result$con[1] <- t10$coef[[1]]
result$con[2] <- t10$se[[1]]
result$con[3] <- t10$N[[1]]

#TC Fus
result$fus[1] <- t10$coef[[2]]
result$fus[2] <- t10$se[[2]]
result$fus[3] <- t10$N[[2]]

#Mais Cont
result$mor[1] <- t10$coef[[3]]
result$mor[2] <- t10$se[[3]]
result$mor[3] <- t10$N[[3]]


#TC Pol 2° Grau
result$po2[1] <- t10$coef[[4]]
result$po2[2] <- t10$se[[4]]
result$po2[3] <- t10$N[[4]]

#TC Pol 2° Grau + Fuso
result$pfu[1] <- t10$coef[[5]]
result$pfu[2] <- t10$se[[5]]
result$pfu[3] <- t10$N[[5]]

#TC Pol 2° Grau + Fuso + C
result$pfc[1] <- t10$coef[[6]]
result$pfc[2] <- t10$se[[6]]
result$pfc[3] <- t10$N[[6]]



colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)","(6)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcccccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/DIFF_Principal_TC_v2.tex")
rm(ef, list, result, t10, latex_table)

# ---------------------------------------------------------------------------- #
## 1.2 Dists -----
# ---------------------------------------------------------------------------- #

# Storing results
rlist <- list()



# ---------------------------------------------------------------------------- #
### 1.2.1 2019-2018 ----
#### 1.2.1.1 Data ----
# ---------------------------------------------------------------------------- #

# ---------- #
#Res
# ---------- #
base_res <- base[priv0 == 1,.(media = mean(media, na.rm = T),
                            obs = .N),
               by = .(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res)] %>% 
  filter(as.numeric(ano) %in% c(2018,2019)) %>% 
  arrange(mun_res,ano) %>%
  group_by(mun_res) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2018, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    d.media = media - v2_nota
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_nota, v2_nota)) 

# --------- #
# School 
# --------- #

#Res
base_esc <- base[priv0 == 1,.(media = mean(media, na.rm = T),
                              obs = .N),
                 by = .(mun_escola,ano,dist_hv_esc,seg_esc,lat_esc,lon_esc)] %>% 
  filter(as.numeric(ano) %in% c(2018,2019)) %>% 
  arrange(mun_escola,ano) %>%
  group_by(mun_escola) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2018, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    d.media = media - v2_nota
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_nota, v2_nota))


# ---------------------------------------------------------------------------- #
#### 1.2.1.2 Regression ----
# ---------------------------------------------------------------------------- #

# ----------- #
# --- Res ---
# ----------- #

#Controls
ef <- dummy_cols(base_res$seg_res[base_res$ano == 2018])
ef <- ef %>% select(-1,-2)

#With Bandwidth
rlist[[as.character(paste0(2019,"-",2018,"res|bwa"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2019],
  x = base_res$dist_hv_res[base_res$ano == 2018],
  c = 0,
  h = bw_main_a,
  b = bw_bias_a,
  cluster = base_res$seg_res[base_res$ano == 2018],
  weights = base_res$obs[base_res$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_res$lat_res[base_res$ano == 2018],
    base_res$lon_res[base_res$ano == 2018]
  )
)


#without
rlist[[as.character(paste0(2019,"-",2018,"res|0"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2019],
  x = base_res$dist_hv_res[base_res$ano == 2018],
  c = 0,
  cluster = base_res$seg_res[base_res$ano == 2018],
  weights = base_res$obs[base_res$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_res$lat_res[base_res$ano == 2018],
    base_res$lon_res[base_res$ano == 2018]
  )
)

# ----------- #
# --- Esc ---
# ----------- #

#Controls
ef <- dummy_cols(base_esc$seg_esc[base_esc$ano == 2018])
ef <- ef %>% select(-1,-2)

#With Bandwidth
rlist[[as.character(paste0(2019,"-",2018,"esc|bwa"))]] <- rdrobust(
  y = base_esc$d.media[base_esc$ano == 2019],
  x = base_esc$dist_hv_esc[base_esc$ano == 2018],
  c = 0,
  h = bw_main_a,
  b = bw_bias_a,
  cluster = base_esc$seg_esc[base_esc$ano == 2018],
  weights = base_esc$obs[base_esc$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_esc$lat_esc[base_esc$ano == 2018],
    base_esc$lon_esc[base_esc$ano == 2018]
  )
)


#without
rlist[[as.character(paste0(2019,"-",2018,"esc|0"))]] <- rdrobust(
  y = base_esc$d.media[base_esc$ano == 2019],
  x = base_esc$dist_hv_esc[base_esc$ano == 2018],
  c = 0,
  cluster = base_esc$seg_esc[base_esc$ano == 2018],
  weights = base_esc$obs[base_esc$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_esc$lat_esc[base_esc$ano == 2018],
    base_esc$lon_esc[base_esc$ano == 2018]
  )
)


# ---------------------------------------------------------------------------- #
### 1.2.2 2018-2017 ----
#### 1.2.2.1 Data ----
# ---------------------------------------------------------------------------- #

# ---------- #
#Res
# ---------- #
base_res <- base[priv0 == 1,.(media = mean(media, na.rm = T),
                              obs = .N),
                 by = .(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res)] %>% 
  filter(as.numeric(ano) %in% c(2017,2018)) %>% 
  arrange(mun_res,ano) %>%
  group_by(mun_res) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2017, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    d.media = media - v2_nota
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_nota, v2_nota)) 

# --------- #
# School 
# --------- #

#Res
base_esc <- base[priv0 == 1,.(media = mean(media, na.rm = T),
                              obs = .N),
                 by = .(mun_escola,ano,dist_hv_esc,seg_esc,lat_esc,lon_esc)] %>% 
  filter(as.numeric(ano) %in% c(2017,2018)) %>% 
  arrange(mun_escola,ano) %>%
  group_by(mun_escola) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2017, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    d.media = media - v2_nota
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_nota, v2_nota))


# ---------------------------------------------------------------------------- #
#### 1.2.2.2 Regression ----
# ---------------------------------------------------------------------------- #

# ----------- #
# --- Res ---
# ----------- #

#Controls
ef <- dummy_cols(base_res$seg_res[base_res$ano == 2017])
ef <- ef %>% select(-1,-2)

#With Bandwidth
rlist[[as.character(paste0(2018,"-",2017,"res|bwa"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2018],
  x = base_res$dist_hv_res[base_res$ano == 2017],
  c = 0,
  h = bw_main_a,
  b = bw_bias_a,
  cluster = base_res$seg_res[base_res$ano == 2017],
  weights = base_res$obs[base_res$ano == 2017],
  vce = "hc0",
  covs = cbind(
    ef,
    base_res$lat_res[base_res$ano == 2017],
    base_res$lon_res[base_res$ano == 2017]
  )
)


#without
rlist[[as.character(paste0(2018,"-",2017,"res|0"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2018],
  x = base_res$dist_hv_res[base_res$ano == 2017],
  c = 0,
  cluster = base_res$seg_res[base_res$ano == 2017],
  weights = base_res$obs[base_res$ano == 2017],
  vce = "hc0",
  covs = cbind(
    ef,
    base_res$lat_res[base_res$ano == 2017],
    base_res$lon_res[base_res$ano == 2017]
  )
)

# ----------- #
# --- Esc ---
# ----------- #

#Controls
ef <- dummy_cols(base_esc$seg_esc[base_esc$ano == 2017])
ef <- ef %>% select(-1,-2)

#With Bandwidth
rlist[[as.character(paste0(2018,"-",2017,"esc|bwa"))]] <- rdrobust(
  y = base_esc$d.media[base_esc$ano == 2018],
  x = base_esc$dist_hv_esc[base_esc$ano == 2017],
  c = 0,
  h = bw_main_a,
  b = bw_bias_a,
  cluster = base_esc$seg_esc[base_esc$ano == 2017],
  weights = base_esc$obs[base_esc$ano == 2017],
  vce = "hc0",
  covs = cbind(
    ef,
    base_esc$lat_esc[base_esc$ano == 2017],
    base_esc$lon_esc[base_esc$ano == 2017]
  )
)


#without
rlist[[as.character(paste0(2018,"-",2017,"esc|0"))]] <- rdrobust(
  y = base_esc$d.media[base_esc$ano == 2018],
  x = base_esc$dist_hv_esc[base_esc$ano == 2017],
  c = 0,
  cluster = base_esc$seg_esc[base_esc$ano == 2017],
  weights = base_esc$obs[base_esc$ano == 2017],
  vce = "hc0",
  covs = cbind(
    ef,
    base_esc$lat_esc[base_esc$ano == 2017],
    base_esc$lon_esc[base_esc$ano == 2017]
  )
)

# ---------------------------------------------------------------------------- #
### 1.2.3 Table ----
# ---------------------------------------------------------------------------- #

t10 <- data.frame(
  coef = do.call(rbind,lapply(rlist, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(rlist, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(rlist, FUN = function(x){x$pv[3]})),
  n = do.call(rbind,lapply(rlist, FUN = function(x){x$N_h}))
)
print(t10)


t10 <- t10 %>%
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "**", 
                         ifelse(pv < 0.05, "*", 
                                ifelse(pv < 0.1, "", "")
                         ))),
    se =  paste0("(", formatC(x = se, digits = 2, format = "f"), ")"),
    N = paste0("[N = ", n.1 + n.2, "]")
  ) %>%
  select( coef, se, N ) %>%
  setDT()



names2 <- c("Residency",
           " "," ",
           "School",
           " ", " ")

result <- data.frame(
  var = names2,
  b07 = rep(NA, times = length(names2)),
  b09 = rep(NA, times = length(names2)),
  bw7 = rep(NA, times = length(names2)),
  bw9 = rep(NA, times = length(names2))
)



# ---------------- #
#Building result table
# ---------------- #

#BW Res.
result$bw7[1] <- t10$coef[[5]] #18-17
result$bw7[2] <- t10$se[[5]]
result$bw7[3] <- t10$N[[5]]

result$bw9[1] <- t10$coef[[1]] #19-18
result$bw9[2] <- t10$se[[1]]
result$bw9[3] <- t10$N[[1]]

#BW Esc.
result$bw7[4] <- t10$coef[[7]] #18-17
result$bw7[5] <- t10$se[[7]]
result$bw7[6] <- t10$N[[7]]

result$bw9[4] <- t10$coef[[3]] #19-18
result$bw9[5] <- t10$se[[3]]
result$bw9[6] <- t10$N[[3]]

#0 Res.
result$b07[1] <- t10$coef[[6]] #18-17
result$b07[2] <- t10$se[[6]]
result$b07[3] <- t10$N[[6]]

result$b09[1] <- t10$coef[[2]] #19-18
result$b09[2] <- t10$se[[2]]
result$b09[3] <- t10$N[[2]]

#0 Esc.
result$b07[4] <- t10$coef[[8]] #18-17
result$b07[5] <- t10$se[[8]]
result$b07[6] <- t10$N[[8]]

result$b09[4] <- t10$coef[[4]] #19-18
result$b09[5] <- t10$se[[4]]
result$b09[6] <- t10$N[[4]]

# ------------------ #
#### 1.2.3.1 Saving to latex ----
# ----------------- #

colnames(result) <- c("", "(1)", "(2)","(3)", "(4)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/DIFF_dists_res_esc.tex")
rm(ef, rlist, result, t10, latex_table, base_res, base_esc)

# ---------------------------------------------------------------------------- #
##1.3 Comparação Young vs. Old ----
# # ---------------------------------------------------------------------------- #

#Age filter dummy
base <- base %>%
  mutate(
    old = ifelse(
      idade > 18 & conclusao == 2, 1,
      ifelse( idade %in% c(17, 18), 0, NA))
  )


# ------------------------ #
#Reg.
# ------------------------ #
rlist <- list()

for (j in c(0:1)){
  
  base_y <- base %>%
    filter( old == j)
  
  base_a <- base_y[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
                   by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] %>%
    filter(as.numeric(ano) %in% c(2018,2019)) %>%
    arrange(mun_prova,ano) %>%
    group_by(mun_prova) %>%
    mutate(
      dup1 = 1,
      dup2 = sum(dup1),
      v1_nota = ifelse(ano == 2018, media, NA),
      v2_nota = max(v1_nota, na.rm = T),
      d.media = media - v2_nota
    ) %>%
    ungroup() %>%
    filter(dup2 == 2) %>%
    select(-c(dup2, dup1, v1_nota, v2_nota))
  
  rm(base_y)
  
  
  
  ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  # 
  #   rlist[[as.character(paste0("old =",j,"|NC"))]] <- rdrobust(
  #     y = base_a$d.media[base_a$ano == 2019],
  #     x = base_a$dist_hv_border[base_a$ano == 2018],
  #     c = 0,
  #     cluster = base_a$seg[base_a$ano == 2018],
  #     weights = base_a$obs[base_a$ano == 2018],
  #     vce = "hc0"
  #     # ,
  #     # covs = cbind(
  #     #   ef,
  #     #   base_a$lat[base_a$ano == 2018],
  #     #   base_a$lon[base_a$ano == 2018]
  #     # )
  #   )
  
  
  
  rlist[[as.character(paste0("old =",j,"|C"))]] <- rdrobust(
    y = base_a$d.media[base_a$ano == 2019],
    x = base_a$dist_hv_border[base_a$ano == 2018],
    c = 0,
    cluster = base_a$seg[base_a$ano == 2018],
    weights = base_a$obs[base_a$ano == 2018],
    vce = "hc0",
    covs = cbind(
      ef,
      base_a$lat[base_a$ano == 2018],
      base_a$lon[base_a$ano == 2018]
    )
  )
  
  
  rlist[[as.character(paste0("old =",j,"|BW"))]] <- rdrobust(
    y = base_a$d.media[base_a$ano == 2019],
    x = base_a$dist_hv_border[base_a$ano == 2018],
    c = 0,
    cluster = base_a$seg[base_a$ano == 2018],
    weights = base_a$obs[base_a$ano == 2018],
    vce = "hc0",
    h = bw_main_a,
    b = bw_bias_a,
    covs = cbind(
      ef,
      base_a$lat[base_a$ano == 2018],
      base_a$lon[base_a$ano == 2018]
    )
  )
  
  
}
rm(j, ef)

tab <- data.frame(
  coef = do.call(rbind,lapply(rlist, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(rlist, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(rlist, FUN = function(x){x$pv[3]})),
  n = do.call(rbind, lapply(rlist, FUN = function(x){x$N_h}))
)


tab <- tab %>%
  mutate(
    N = n.1 + n.2,
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "**",
                         ifelse(pv < 0.05, "*",
                                ifelse(pv < 0.1, "", "")
                         ))),
    se = paste0("(", formatC(x = se, digits = 2, format = "f"), ")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ",formatC(x = N, digits = 0, format = "f"),"]"),
    
    esp = 1,
    id = 1
  )

names <- c(
  "Younger",
  " "," ",
  "Older",
  " "," "
)

result <- data.frame(
  var = names,
  #nc = rep(NA, times = length(names)),
  cc= rep(NA, times = length(names)),
  bw = rep(NA, times = length(names))
  
)


# result$nc[1] <- tab$coef[[1]]
# result$nc[2] <- tab$se[[1]]
# result$nc[3] <- tab$N[[1]]
result$cc[1] <- tab$coef[[1]]
result$cc[2] <- tab$se[[1]]
result$cc[3] <- tab$N[[1]]
result$bw[1] <- tab$coef[[2]]
result$bw[2] <- tab$se[[2]]
result$bw[3] <- tab$N[[2]]

# result$nc[4] <- tab$coef[[4]]
# result$nc[5] <- tab$se[[4]]
# result$nc[6] <- tab$N[[4]]
result$cc[4] <- tab$coef[[3]]
result$cc[5] <- tab$se[[3]]
result$cc[6] <- tab$N[[3]]
result$bw[4] <- tab$coef[[4]]
result$bw[5] <- tab$se[[4]]
result$bw[6] <- tab$N[[4]]



colnames(result) <- c("", "(1)", "(2)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/OLDER_vs_young_v1.tex")

rm(base_a, names, result, rlist, tab, latex_table)


# ---------------------------------------------------------------------------- #
#2. Gráficos + Reg ----
## 2.1 Principal ----
# ---------------------------------------------------------------------------- #

base_a <- base[priv0 == 1,.(media = mean(media, na.rm = T),
                            obs = .N),
               by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] %>% 
  filter(as.numeric(ano) %in% c(2018,2019)) %>% 
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2018, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    d.media = media - v2_nota 
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_nota, v2_nota))

# ---------------------------------------------------------------------------- #
### 2.1.1 19-18 ----
# ---------------------------------------------------------------------------- #
fig <- list()

temp <- base_a %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main_a ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.media)#,
    #subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(ano == 2019) %>% 
  select(d.media) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(ano == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(ano == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(ano == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(ano == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              #h = bw_main_a,
              nbins = 35,
              #b = bw_bias_a,
              weights = temp$obs[temp$ano == 2018],
              #subset = temp$subset == 1,
              hide = T,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-160*10^5,160*10^5,4*10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linewidth = 1) + 
  scale_color_brewer(palette = "Set1") + 
  # geom_segment(aes(x = x_l_sta,
  #                  xend = x_l_end,
  #                  y = y_l_sta,
  #                  yend = y_l_end),
  #              size = 1.5, color = "#E41A4C") +
  # geom_segment(aes(x = x_r_sta,
  #                  xend = x_r_end,
  #                  y = y_r_sta,
  #                  yend = y_r_end),
  #              size = 1.5, color = "#377EB8") +
  labs(x = "Distance to DST Border (km)",
       y = "Overall ENEM Score") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-20,20) + 
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20))


fig_gg


ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/RDD_Principal_1918_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/RDD_Principal_1918_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)

# ---------------------------------------------------------------------------- #
#### 2.1.1.1 Lowess (19 e 18) ----
# ---------------------------------------------------------------------------- #

base_a <- base_a %>% 
  mutate(trat = ifelse(dist_hv_border > 0, 1, 0))

years <- c(2019)
cutoff <- 0.5
span_val <- 0.5   # loess span ~ Stata bw

for (y in years) {
  options(scipen = 999)
  d <- subset(base_a, ano == y)
  d$dist_km <- d$dist_hv_border/1000
  
  # compute lowess for treated and control (if they exist)
  d1 <- subset(d, trat == 1)
  d0 <- subset(d, trat == 0)
  
  lw1 <- if (nrow(d1) > 1) stats::lowess(d1$dist_km, d1$d.media, f = span_val) else NULL
  lw0 <- if (nrow(d0) > 1) stats::lowess(d0$dist_km, d0$d.media, f = span_val) else NULL
  
  png_filename <- paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/lowess/media_rdd", y, "_base_lowess.png")
  png(png_filename, width = 1200, height = 800, res = 150)
  par(mar = c(5,5,4,2) + 0.1)
  
  
  # Scatter
  plot(d$dist_km, d$d.media,
       pch = 20, cex = 0.6,
       xlab = "Distance to DST Border (km)", ylab = "ENEM Score",
       #main = paste("Lowess —", y),
       xlim = range(d$dist_km, na.rm = TRUE),
       ylim = range(d$d.media, na.rm = TRUE),
       xaxt = "n")
  
  # vertical cutoff line
  abline(v = cutoff, lty = 2, col = "darkgray", lwd = 3)
  
  x_range <- range(d$dist_km, na.rm = TRUE)
  ticks <- seq(floor(x_range[1] / 400) * 400,
               ceiling(x_range[2] / 400) * 400,
               by = 400)
  
  axis(1,
       at = ticks,
       labels = format(ticks, scientific = FALSE),
       las = 2)
  
  
  
  # add lowess lines (use different colors)
  if (!is.null(lw1)) lines(lw1, col = "red", lwd = 2)
  if (!is.null(lw0)) lines(lw0, col = "red", lwd = 2)
  
  dev.off()
  message("Wrote: ", png_filename)
}

rm(d, d0, d1, lw0, lw1, cutoff, png_filename, x_range, ticks, years, span_val,)


# ---------------------------------------------------------------------------- #
####2.1.1.2 CMOGRAM -----
# ---------------------------------------------------------------------------- #
###### 2.1.1.2.1 Data -----
# ---------------------------------------------------------------------------- #


df_cmo <- base[priv0 == 1,.(media = mean(media, na.rm = T),
                            media_dia1 = mean(dia_1, na.rm = T),
                            media_dia2 = mean(dia_2, na.rm = T),
                            media_rd = mean(rd, na.rm = T),
                            media_cn = mean(cn, na.rm = T),
                            media_lc = mean(lc, na.rm = T),
                            media_ch = mean(ch, na.rm = T),
                            media_mt = mean(mt, na.rm = T),
                            
                            #Dificulty
                            mediabl = mean(acerto_pbl, na.rm = T),
                            mediabh = mean(acerto_pbh, na.rm = T),
                            obs = .N),
               by = .(mun_prova,ano,dist_hv_border)] %>% 
  filter(as.numeric(ano) %in% c(2018,2019)) %>% 
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2018, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    dmedia = media - v2_nota,
    
    
    v1_rd = ifelse(ano == 2018, media_rd, NA),
    v2_rd = max(v1_rd, na.rm = T),
    dmedia_rd = media_rd - v2_rd,
    
    #Ciências Naturais
    v1_cn = ifelse(ano == 2018, media_cn, NA),
    v2_cn = max(v1_cn, na.rm = T),
    dmedia_cn = media_cn - v2_cn,
    
    #Ciências Humanas
    v1_ch = ifelse(ano == 2018, media_ch, NA),
    v2_ch = max(v1_ch, na.rm = T),
    dmedia_ch = media_ch - v2_ch,
    
    #Lingua Portuguesa
    v1_lc = ifelse(ano == 2018, media_lc, NA),
    v2_lc = max(v1_lc, na.rm = T),
    dmedia_lc = media_lc - v2_lc,
    
    #Matematica
    v1_mt = ifelse(ano == 2018, media_mt, NA),
    v2_mt = max(v1_mt, na.rm = T),
    dmedia_mt = media_mt - v2_mt,
    
    #dia1_ 2019 REF
    v1_d1 = ifelse(ano == 2018, media_dia1, NA), 
    v2_d1 = max(v1_d1, na.rm = T),
    dmedia_d1 = media_dia1 - v2_d1,
    
    v1_d2 = ifelse(ano == 2018, media_dia2, NA), #Note que inverte
    v2_d2 = max(v1_d2, na.rm = T),
    dmedia_d2 = media_dia2 - v2_d2,
    
    #Dificulty
    v1_pbl = ifelse(ano == 2018, mediabl, NA),
    v2_pbl = max(v1_pbl, na.rm = T),
    d.mediabl = mediabl - v2_pbl,
    
    v1_pbh = ifelse(ano == 2018, mediabh, NA),
    v2_pbh = max(v1_pbh, na.rm = T),
    d.mediabh = mediabh - v2_pbh
    
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  group_by(mun_prova) %>% 
  mutate(
    #Weights ref
    obs_r = obs[ano == 2018]
  ) %>% 
  ungroup() %>% 
  select(-c(dup2, dup1, v1_nota, v2_nota,
            v1_rd,v2_rd,v1_cn, v2_cn, v1_ch,v2_ch,v1_lc,v2_lc,v1_mt,v2_mt,
            v1_d1, v2_d1, v2_d2, v1_d2, v1_pbl, v2_pbl, v1_pbh, v2_pbh))







# ---------------------------------------------------------------------------- #
# ###### 2.1.1.2.2 Loop ---- #
# # ---------------------------------------------------------------------------- #
# 
# #For the loop
# cols <- c("dmedia", "dmedia_rd", "dmedia_lc", "dmedia_ch", "dmedia_cn", "dmedia_mt",
#           "dmedia_d1", "dmedia_d2", "d.mediabl", "d.mediabh")
# 
# ylabel <- c("Average ENEM Score",
#             "Average Writing Score",
#             "Average Language Score",
#             "Average Human S. Score",
#             "Average Natural S. Score",
#             "Average Math Score",
#             "Average ENEM Score (Day 1)",
#             "Average ENEM Score (Day 2)",
#             "Average Easier Questions Score",
#             "Average More Difficult \n Questions Score")
# 
# 
# for (j in seq_along(cols)) {
#   
#   yvar     <- cols[j]
#   y_lab    <- ylabel[j]
#   
#   options(scipen = 999)
#   # -----------------------------#
#   # PREPARE DATA
#   # -----------------------------#
#   d <- df_cmo %>%
#     filter(ano == 2019) %>%
#     mutate(x_km = dist_hv_border / 1000,
#            xc = x_km ) %>%
#     filter(!is.na(.data[[yvar]]), !is.na(xc))
#   
#   d <- d %>% filter(!is.na(obs_r))
#   
#   
#   # -----------------------------#
#   # RDPLOT (fixed-width)
#   # -----------------------------#
#   rd_out <- rdplot(
#     y = d[[yvar]],
#     x = d$xc,
#     weights = d$obs_r,   #2018 weights
#     c = 0
#   )
#   
#   #extract bins
#   bins <- rd_out$vars_bins
#   
#   # create side variable: left (<0) and right (>=0)
#   bins$side <- ifelse(bins$rdplot_mean_x < 0, "left", "right")
#   bins$side <- factor(bins$side, levels = c("left", "right"))
#   
#   #Temporary base
#   df18 <- df_cmo %>% filter(ano == 2019)
#   
#   # Make a treatment indicator like in your Stata: dist > 0 treated
#   df18 <- df18 %>% mutate(trat = ifelse(dist_hv_border > 0, 1, 0))
#   
#   
#   # -----------------------------#
#   # LOESS FITS ON BINNED MEANS
#   # -----------------------------#
#   bins_left  <- subset(bins, rdplot_mean_x < 0)
#   bins_right <- subset(bins, rdplot_mean_x >= 0)
#   
# 
#   # ----------------------------- #
#   # Plot 
#   # ----------------------------- #
#   bins$side <- factor(bins$side, levels = c("left","right"))
#   
#   bins$side <- factor(bins$side, levels = c("left", "right"))
#   
#   # Use rdplot_mean_* names for points and ensure colors for fill and border match Set1
#   base_p <- ggplot() +
#     # binned points colored by side (shape 21 uses fill + colour)
#     geom_point(data = bins,
#                aes(x = rdplot_mean_x, y = rdplot_mean_y, fill = side, colour = side),
#                alpha = 0.8, size = 2.5) +
#     scale_fill_brewer(palette = "Set1") + 
#     theme_minimal()
# 
#   
#   if(yvar == "d.mediabh") {
#     base_p <- base_p +
#       coord_cartesian(ylim = c(-5, 5))
#   } else if (yvar == "d.mediabl") {
#     base_p <- base_p +
#       coord_cartesian(ylim = c(0,10))
#   }
#   
#   
#     # ------------------------ #
#     #No weights
#     # ------------------------ #
#   p_low_loess <- base_p +
#     geom_smooth(
#       data = filter(df18, dist_hv_border <= 0),
#       aes(x = dist_hv_border / 1000, y = .data[[yvar]]),
#       method = "loess", se = FALSE, color = "#E41A4C", inherit.aes = FALSE
#     ) +
#     geom_smooth(
#       data = filter(df18, dist_hv_border > 0),
#       aes(x = dist_hv_border / 1000, y = .data[[yvar]]),
#       method = "loess", se = FALSE, color = "#377EB8", inherit.aes = FALSE
#     ) +
#     geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40", size = 0.9) +
#     labs(x = "Distance to DST Border (km)", y = y_lab) +
#     theme_minimal(base_size = 18) +
#     theme(legend.position = "none",
#           axis.title.x = element_text(size = 20),
#           axis.title.y = element_text(size = 20),
#           axis.text.x = element_text(size = 15,angle = 90,hjust = 1, vjust = 0.5),
#           axis.text.y = element_text(size = 15))
#   
#   
#   
#   # ------------------------ #
#   #With weights
#   # ------------------------ #
#   p_low_loess_w <- base_p +
#     geom_smooth(
#       data = filter(df18, dist_hv_border <= 0),
#       aes(x = dist_hv_border / 1000, y = .data[[yvar]],
#           weight = df_cmo$obs_r[df_cmo$ano == 2018 & df_cmo$dist_hv_border <= 0]),
#       method = "loess", se = FALSE, color = "#E41A4C", inherit.aes = FALSE
#     ) +
#     geom_smooth(
#       data = filter(df18, dist_hv_border > 0),
#       aes(x = dist_hv_border / 1000, y = .data[[yvar]],
#       weight = df_cmo$obs_r[df_cmo$ano == 2018 & df_cmo$dist_hv_border > 0]),
#       method = "loess", se = FALSE, color = "#377EB8", inherit.aes = FALSE
#     ) +
#     geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40", size = 0.9) +
#     labs(x = "Distance to DST Border (km)", y = y_lab) +
#     theme_minimal(base_size = 18) +
#     theme(legend.position = "none",
#           axis.title.x = element_text(size = 20),
#           axis.title.y = element_text(size = 20),
#           axis.text.x = element_text(size = 15,angle = 90,hjust = 1, vjust = 0.5),
#           axis.text.y = element_text(size = 15))
#   
#   
#   print(p_low_loess)
#   print(p_low_loess_w)
#   # optionally save:
#   ggsave(paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/cmogram/cmogram_low_binned_", 2019, "_", yvar, ".png"),
#          p_low_loess, width = 10, height = 6, dpi = 200)
#   ggsave(paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/cmogram/weight_cmogram_low_binned_", 2019, "_", yvar, ".png"),
#          p_low_loess_w, width = 10, height = 6, dpi = 200)
#   
#   
#   #Lowess test
#   if(j == 1) {
#     
#     my_span <- 0.66
#     
#     
#     p_low_lowess <- base_p +
#       geom_smooth(
#         data = filter(df18, dist_hv_border <= 0),
#         aes(x = dist_hv_border / 1000, y = .data[[yvar]]),  # include weight if desired
#         method = "loess",
#         method.args = list(span = my_span, degree = 1, family = "symmetric"),
#         se = FALSE,
#         color = "#E41A4C",
#         inherit.aes = FALSE
#       ) +
#       
#       # right side (x > 0)
#       geom_smooth(
#         data = filter(df18, dist_hv_border > 0),
#         aes(x = dist_hv_border / 1000, y = .data[[yvar]]),
#         method = "loess",
#         method.args = list(span = my_span, degree = 1, family = "symmetric"),
#         se = FALSE,
#         color = "#377EB8",
#         inherit.aes = FALSE
#       ) +
#       geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40", size = 0.9) +
#       labs(x = "Distance to DST Border (km)", y = y_lab) +
#       theme_minimal(base_size = 18) +
#       theme(legend.position = "none",
#             axis.title.x = element_text(size = 20),
#             axis.title.y = element_text(size = 20),
#             axis.text.x = element_text(size = 15,angle = 90,hjust = 1, vjust = 0.5),
#             axis.text.y = element_text(size = 15))
#     
#     
#     print(p_low_lowess)
#     
#     # Running variable in km
#     df18 <- df18 %>%
#       mutate(x_km = dist_hv_border / 1000)
#     
#     # ----------------------------- #
#     # LEFT SIDE (x <= 0) 
#     # ----------------------------- #
#     df_left <- df18 %>% filter(x_km <= 0)
#     
#     lw_left <- lowess(
#       x = df_left$x_km,
#       y = df_left[[yvar]],
#       f = 0.8
#     )
#     
#     # ----------------------------- #
#     # RIGHT SIDE (x > 0)
#     # ----------------------------- #
#     df_right <- df18 %>% filter(x_km > 0)
#     
#     lw_right <- lowess(
#       x = df_right$x_km,
#       y = df_right[[yvar]],
#       f = 0.8
#     )
#     
#     # ----------------------------- #
#     # Plot
#     # ----------------------------- #
#     p_low_lowess_real <- base_p +
#       geom_line(
#         inherit.aes = FALSE,
#         data = data.frame(x = lw_left$x, y = lw_left$y),
#         aes(x = x, y = y),
#         color = "#E41A4C"
#       ) +
#       
#       geom_line(
#         inherit.aes = FALSE,
#         data = data.frame(x = lw_right$x, y = lw_right$y),
#         aes(x = x, y = y),
#         color = "#377EB8"
#       ) +
#       
#       geom_vline(xintercept = 0, linetype = "dashed") +
#       labs(x = "Distance to DST Border (km)", y = y_lab) +
#       theme_minimal(base_size = 18) +
#       theme(legend.position = "none",
#             axis.title.x = element_text(size = 20),
#             axis.title.y = element_text(size = 20),
#             axis.text.x = element_text(size = 15,angle = 90,hjust = 1, vjust = 0.5),
#             axis.text.y = element_text(size = 15))
#     
#     print(p_low_lowess_real)
#     
#     ggsave(paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/cmogram/lowess_", 2019, "_", yvar, ".png"),
#            p_low_lowess, width = 10, height = 6, dpi = 200)
#     ggsave(paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/cmogram/lowess_real_", 2019, "_", yvar, ".png"),
#            p_low_lowess_real, width = 10, height = 6, dpi = 200)
#     
#     rm(p_low_lowess, p_low_lowess_real, lw_right, df_right, lw_left, df_left, my_span)
#     
#   }
#   
#   message("Finshed for: ", yvar)
#   
#   rm(yvar, ylab, d, j, rd_out, bins, df18, bins_left, bins_right, base_p, p_low_loess_w, p_low_loess)
#    
# }
# 
# rm(cols, ylabel)
# 
# # for (j in seq_along(cols)) {
# #   
# #   yvar     <- cols[j]
# #   y_lab    <- ylabel[j]
# #   
# #   options(scipen = 999)
# #   # -----------------------------#
# #   # PREPARE DATA
# #   # -----------------------------#
# #   d <- df_cmo %>%
# #     filter(ano == 2019) %>%
# #     mutate(x_km = dist_hv_border / 1000,
# #            xc = x_km ) %>%
# #     filter(!is.na(.data[[yvar]]), !is.na(xc))
# #   
# #   d <- d %>% filter(!is.na(obs_r))
# # 
# #   
# #   # -----------------------------#
# #   # RDPLOT (fixed-width)
# #   # -----------------------------#
# #   rd_out <- rdplot(
# #     y = d[[yvar]],
# #     x = d$xc,
# #     weights = d$obs_r,   #2018 weights
# #     c = 0
# #   )
# #   
# #   #extract bins
# #   bins <- rd_out$vars_bins
# #   
# #   bins <- rd_out$vars_bins
# #   # create side variable: left (<0) and right (>=0)
# #   bins$side <- ifelse(bins$rdplot_mean_x < 0, "left", "right")
# #   bins$side <- factor(bins$side, levels = c("left", "right"))
# #   
# #   
# #   # -----------------------------#
# #   # LOESS FITS ON BINNED MEANS
# #   # -----------------------------#
# #   bins_left  <- subset(bins, rdplot_mean_x < 0)
# #   bins_right <- subset(bins, rdplot_mean_x >= 0)
# #   
# #   fit_left  <- loess(rdplot_mean_y ~ rdplot_mean_x,
# #                      data = bins_left,
# #                      weights = bins_left$rdplot_N,
# #                      span = 0.6)
# #   
# #   fit_right <- loess(rdplot_mean_y ~ rdplot_mean_x,
# #                      data = bins_right,
# #                      weights = bins_right$rdplot_N,
# #                      span = 0.6)
# #   
# #   
# #   # left grid
# #   if (nrow(bins_left) >= 0) {
# #     xg_left <- seq(min(bins_left$rdplot_mean_x, na.rm = TRUE),
# #                    max(bins_left$rdplot_mean_x, na.rm = TRUE),
# #                    length.out = 200)
# #     yg_left <- tryCatch(predict(fit_left, newdata = data.frame(rdplot_mean_x = xg_left)),
# #                         error = function(e) rep(NA_real_, length(xg_left)))
# #     # try direct prediction at 0; fallback to last predicted y (closest-to-zero on left)
# #     y0_left_try <- tryCatch(as.numeric(predict(fit_left, newdata = data.frame(rdplot_mean_x = 0))),
# #                             error = function(e) NA_real_)
# #     if (is.na(y0_left_try)) {
# #       # choose nearest grid value (the one with largest x because left xg <= 0)
# #       idx_closest <- which.max(xg_left)
# #       y0_left <- yg_left[idx_closest]
# #     } else y0_left <- y0_left_try
# #     # ensure 0 included at the end
# #     if (!any(abs(xg_left - 0) < .Machine$double.eps^0.5)) {
# #       xg_left <- c(xg_left, 0)
# #       yg_left <- c(yg_left, y0_left)
# #     } else {
# #       yg_left[which.min(abs(xg_left - 0))] <- y0_left
# #     }
# #     grid_left <- data.frame(x = xg_left, y = yg_left) %>% arrange(x)
# #   } else {
# #     grid_left <- data.frame(x = numeric(0), y = numeric(0))
# #     y0_left <- NA_real_
# #   }
# #   
# #   # right grid
# #   if (nrow(bins_right) >= 0) {
# #     xg_right <- seq(min(bins_right$rdplot_mean_x, na.rm = TRUE),
# #                     max(bins_right$rdplot_mean_x, na.rm = TRUE),
# #                     length.out = 200)
# #     yg_right <- tryCatch(predict(fit_right, newdata = data.frame(rdplot_mean_x = xg_right)),
# #                          error = function(e) rep(NA_real_, length(xg_right)))
# #     # try direct prediction at 0; fallback to first predicted y (closest-to-zero on right)
# #     y0_right_try <- tryCatch(as.numeric(predict(fit_right, newdata = data.frame(rdplot_mean_x = 0))),
# #                              error = function(e) NA_real_)
# #     if (is.na(y0_right_try)) {
# #       idx_closest <- which.min(xg_right)  # smallest x in right grid (closest to 0)
# #       y0_right <- yg_right[idx_closest]
# #     } else y0_right <- y0_right_try
# #     # ensure 0 included at the start
# #     if (!any(abs(xg_right - 0) < .Machine$double.eps^0.5)) {
# #       xg_right <- c(0, xg_right)
# #       yg_right <- c(y0_right, yg_right)
# #     } else {
# #       yg_right[which.min(abs(xg_right - 0))] <- y0_right
# #     }
# #     grid_right <- data.frame(x = xg_right, y = yg_right) %>% arrange(x)
# #   } else {
# #     grid_right <- data.frame(x = numeric(0), y = numeric(0))
# #     y0_right <- NA_real_
# #   }
# #   # ----------------------------- #
# #   # Plot 
# #   # ----------------------------- #
# #   bins$side <- factor(bins$side, levels = c("left","right"))
# #   
# #   bins$side <- factor(bins$side, levels = c("left", "right"))
# #   
# #   # Use rdplot_mean_* names for points and ensure colors for fill and border match Set1
# #   p <- ggplot() +
# #     # binned points colored by side (shape 21 uses fill + colour)
# #     geom_point(data = bins,
# #                aes(x = rdplot_mean_x, y = rdplot_mean_y, fill = side, colour = side),
# #                alpha = 0.8, size = 2.5) +
# #     scale_fill_brewer(palette = "Set1") +
# # 
# #     # left loess line (only if grid_left exists and has rows)
# #     { if (exists("grid_left") && nrow(grid_left) > 0)
# #       geom_line(data = grid_left, aes(x = x, y = y), colour = "#E41A4C", size = 1) } +
# #     
# #     # right loess line (only if grid_right exists and has rows)
# #     { if (exists("grid_right") && nrow(grid_right) > 0)
# #       geom_line(data = grid_right, aes(x = x, y = y), colour = "#377EB8", size = 1) } +
# #     
# #     # optional points where lines meet the cutoff (if available)
# #     { if (exists("y0_left") && !is.na(y0_left))  geom_point(aes(x = 0, y = y0_left), colour = "#E41A4C", size = 1) } +
# #     { if (exists("y0_right") && !is.na(y0_right)) geom_point(aes(x = 0, y = y0_right), colour = "#377EB8", size = 1) } +
# #     
# #     geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40", size = 0.9) +
# #     labs(x = "Distance to DST Border (km)", y = y_lab) +
# #     theme_minimal(base_size = 15) +
# #     theme(legend.position = "none")
# #   
# #   print(p)
# #   # optionally save:
# #   ggsave(paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/cmogram/cmogram_low_binned_", 2019, "_", yvar, ".png"), p, width = 10, height = 6, dpi = 200)
# #   
# #   message("Finished for: ", yvar)
# #   
# # }
# # 
# 
# # 
# # 
# # 
# # # -----------------------------#
# # # PARAMETERS
# # # -----------------------------#
# # year <- 2019
# # xvar <- "dist_hv_border"   # running var in metres
# # wvar <- "obs_r"              # weight variable name in df_cmo; set to NULL if none
# # 
# # bin_width_km <- 50
# # span <- 0.6
# # min_obs_per_bin <- 1
# # 
# # # whether to weight the loess on binned means by the bin-level sum of weights
# # weight_bins <- TRUE
# # 
# # # whether to compute binned means as weighted means (if wvar provided)
# # weighted_binned_means <- TRUE
# # 
# # # optionally compute & plot a weighted loess on raw obs (not usually used for cmogram)
# # plot_raw_weighted_loess <- FALSE
# # 
# # # colours
# # col_left <- "red"
# # col_right <- "red"
# # point_colors <- c(left = "#1f78b4", right = "#e31a1c")
# # 
# # 
# # #For the loop
# # cols <- c("dmedia", "dmedia_rd", "dmedia_lc", "dmedia_ch", "dmedia_cn", "dmedia_mt",
# #           "dmedia_d1", "dmedia_d2")
# # 
# # ylabel <- c("Average ENEM Score",
# #             "Average Writing Score",
# #             "Average Language Score",
# #             "Average Human S. Score",
# #             "Average Natural S. Score",
# #             "Average Math Score",
# #             "Average ENEM Score (Day 1)",
# #             "Average ENEM Score (Day 2)")
# # 
# # 
# # 
# # for (j in seq_along(cols)) {
# #   
# #   yvar     <- cols[j]
# #   y_lab    <- ylabel[j]
# #   
# #   options(scipen = 999)
# #     # -----------------------------#
# #     # PREPARE DATA
# #     # -----------------------------#
# #     d <- df_cmo %>%
# #       filter(ano == year) %>%
# #       mutate(x_km = .data[[xvar]] / 1000,
# #              xc = x_km - 0) %>%
# #       filter(!is.na(.data[[yvar]]), !is.na(xc))
# #     
# #     # ensure weight exists (if requested)
# #     if (!is.null(wvar)) {
# #       if (! (wvar %in% names(d)) ) stop("Weight variable '", wvar, "' not found in the data.")
# #       # Replace NA weights with zero or remove observations with NA weights as you prefer:
# #       d <- d %>% filter(!is.na(.data[[wvar]]))
# #     }
# #     
# #     # -----------------------------#
# #     # BINNING (fixed-width)
# #     # -----------------------------#
# #     bin_w <- bin_width_km
# #     breaks <- seq(floor(min(d$xc, na.rm=TRUE) / bin_w) * bin_w,
# #                   ceiling(max(d$xc, na.rm=TRUE) / bin_w) * bin_w,
# #                   by = bin_w)
# #     d <- d %>% mutate(bin = cut(xc, breaks = breaks, include.lowest = TRUE, right = FALSE))
# #     
# #     # compute binned stats; use weighted mean if wvar given and weighted_binned_means = TRUE
# #     binned <- d %>%
# #       group_by(bin) %>%
# #       summarise(
# #         n_bin = n(),
# #         w_bin = if (!is.null(wvar)) sum(.data[[wvar]], na.rm = TRUE) else NA_real_,
# #         x_bin = if (!is.null(wvar) && weighted_binned_means) {
# #           # x position of bin = weighted mean of x (weighted by w)
# #           if (sum(.data[[wvar]], na.rm = TRUE) >= 0) weighted.mean(xc, .data[[wvar]], na.rm = TRUE) else mean(xc, na.rm = TRUE)
# #         } else {
# #           mean(xc, na.rm = TRUE)    # unweighted mean
# #         },
# #         y_bin = if (!is.null(wvar) && weighted_binned_means) {
# #           if (sum(.data[[wvar]], na.rm = TRUE) >= 0) weighted.mean(.data[[yvar]], .data[[wvar]], na.rm = TRUE) else mean(.data[[yvar]], na.rm = TRUE)
# #         } else {
# #           mean(.data[[yvar]], na.rm = TRUE)
# #         },
# #         .groups = "drop"
# #       ) %>%
# #       filter(n_bin >= min_obs_per_bin) %>%
# #       arrange(x_bin) %>%
# #       mutate(side = ifelse(x_bin <= 0, "left", "right"))
# #     
# #     # -----------------------------#
# #     # LOESS FITS ON BINNED MEANS
# #     # -----------------------------#
# #     # choose weights for loess on binned means: either equal (NULL) or w_bin
# #     w_for_loess_bins <- if (weight_bins && !all(is.na(binned$w_bin))) binned$w_bin else NULL
# #     
# #     fit_left <- NULL; fit_right <- NULL
# #     if (any(binned$side == "left")) {
# #       left_df <- filter(binned, side == "left")
# #       if (!is.null(w_for_loess_bins)) {
# #         fit_left <- loess(y_bin ~ x_bin, data = left_df, weights = left_df$w_bin, span = span)
# #       } else {
# #         fit_left <- loess(y_bin ~ x_bin, data = left_df, span = span)
# #       }
# #     }
# #     if (any(binned$side == "right")) {
# #       right_df <- filter(binned, side == "right")
# #       if (!is.null(w_for_loess_bins)) {
# #         fit_right <- loess(y_bin ~ x_bin, data = right_df, weights = right_df$w_bin, span = span)
# #       } else {
# #         fit_right <- loess(y_bin ~ x_bin, data = right_df, span = span)
# #       }
# #     }
# #     
# #     # ----------------------------- #
# #     # PREDICTION GRIDS 
# #     # ----------------------------- #
# #     safe_predict_grid <- function(fit, side_df, to_zero = TRUE, ngrid = 250) {
# #       if (is.null(fit) || nrow(side_df) < 1) return(data.frame(x = numeric(0), yhat = numeric(0)))
# #       x_min <- min(side_df$x_bin, na.rm = TRUE)
# #       x_max <- max(side_df$x_bin, na.rm = TRUE)
# #       # create grid spanning that side's bins; ensure grid touches 0 if to_zero = TRUE
# #       if (side_df$x_bin[1] < 0 && x_max <= 0) { # left side
# #         xg <- seq(x_min, x_max, length.out = ngrid)
# #       } else if (side_df$x_bin[1] >= 0 && x_min >= 0) { # right side
# #         xg <- seq(x_min, x_max, length.out = ngrid)
# #       } else {
# #         xg <- seq(x_min, x_max, length.out = ngrid)
# #       }
# #       yg <- tryCatch(predict(fit, newdata = data.frame(x_bin = xg)), error = function(e) rep(NA_real_, length(xg)))
# #       # try to get value at 0; if predict(0) fails, use closest grid value to 0
# #       y0_try <- tryCatch(as.numeric(predict(fit, newdata = data.frame(x_bin = 0))), error = function(e) NA_real_)
# #       if (is.na(y0_try)) {
# #         # choose closest grid value to 0 (either last left or first right)
# #         idx_closest <- which.min(abs(xg - 0))
# #         y0 <- yg[idx_closest]
# #       } else y0 <- y0_try
# #       # ensure 0 included
# #       if (!any(abs(xg - 0) < .Machine$double.eps^0.5)) {
# #         if (min(xg) > 0) { xg <- c(0, xg); yg <- c(y0, yg) }
# #         else { xg <- c(xg, 0); yg <- c(yg, y0) }
# #       } else {
# #         # replace grid value at exactly 0 with y0 to avoid tiny numeric mismatch
# #         xg[which.min(abs(xg - 0))] <- 0
# #         yg[which.min(abs(xg - 0))] <- y0
# #       }
# #       data.frame(x = xg, yhat = yg) %>% arrange(x)
# #     }
# #     
# #     grid_left <- if (!is.null(fit_left)) safe_predict_grid(fit_left, filter(binned, side == "left")) else data.frame()
# #     grid_right <- if (!is.null(fit_right)) safe_predict_grid(fit_right, filter(binned, side == "right")) else data.frame()
# #     
# #     # ----------------------------- #
# #     # OPTIONAL: raw-data weighted loess (if you want to draw it)
# #     # ----------------------------- #
# #     raw_loess_df <- data.frame()
# #     if (plot_raw_weighted_loess) {
# #       if (!is.null(wvar)) {
# #         fit_raw <- loess(formula = as.formula(paste0(yvar, " ~ xc")), data = d, weights = d[[wvar]], span = span)
# #       } else {
# #         fit_raw <- loess(formula = as.formula(paste0(yvar, " ~ xc")), data = d, span = span)
# #       }
# #       xg_raw <- seq(min(d$xc, na.rm = TRUE), max(d$xc, na.rm = TRUE), length.out = 400)
# #       raw_loess_df <- data.frame(x = xg_raw, y = predict(fit_raw, newdata = data.frame(xc = xg_raw)))
# #     }
# #     
# #     # ----------------------------- #
# #     # PLOT: binned points (colored by side) + weighted loess over binned means
# #     # ----------------------------- #
# #     p <- ggplot() +
# #       # optionally: weighted raw loess (thin background line)
# #       { if (plot_raw_weighted_loess && nrow(raw_loess_df)>0) geom_line(data = raw_loess_df, aes(x=x, y=y), color = "grey60", size = 0.6) } +
# #       
# #       # binned points (colored by side)
# #       geom_point(data = binned, aes(x = x_bin, y = y_bin, fill = side, colour = side),
# #                  size = 3, shape = 21, stroke = 0.7) +
# #       scale_color_brewer(palette = "Set1") +
# #       
# #       
# #       # left & right loess lines fitted on binned means using weights if requested
# #       { if (nrow(grid_left) >= 0)  geom_line(data = grid_left,  aes(x = x, y = yhat), colour = "#E41A4C", size = 1) } +
# #       { if (nrow(grid_right) >= 0) geom_line(data = grid_right, aes(x = x, y = yhat), colour = "#377EB8", size = 1) } +
# #       
# #       geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40", size = 0.9) +
# #       labs(x = "Distance to DST Border (km)", y = y_lab) +
# #       theme_minimal(base_size = 15) +
# #       theme(legend.position = "none")
# #     
# #     print(p)
# #     # optionally save:
# #     ggsave(paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/cmogram/cmogram_low_binned_", year, "_", yvar, ".png"), p, width = 10, height = 6, dpi = 200)
# # 
# #     message("Finished for: ", yvar)
# # }
# 
# # ---------------------------------------------------------------------------- #
##### 2.1.1.2.2 Filters -----
# ---------------------------------------------------------------------------- #

#Filtering the data for every specification:

vars <- c("escm", "time13")

#Creating the timezone variable
base <- base %>% 
  group_by(mun_prova) %>% 
  mutate( time13 = case_when(
    
    uf %in% c("GO", "DF", "MG", "ES", "RJ", "SP", "SC", "PR", "RS", "BA", "SE",
              "AL", "PE", "PB", "RN", "CE","PI", "MA", "TO", "PA", "AP") ~ 1,
    
    uf %in% c("MT", "MS", "RO", "RR") | uf == "AM" &
      !mun_prova %in%
      c(1300201, 1300607, 1300706, 1301407, 1301506, 1301654, 1301803, 1301951,
        1302306, 1302405, 1303502, 1303908, 1304062) ~ 0,
    
    T ~ NA
  )) %>% ungroup()

###### --------------------- Mother E. ---------------------------------------- #

#With High School
df_escm1 <- base %>%
        filter(priv0 == 1, escm == 1) %>%
        group_by(mun_prova, ano, dist_hv_border) %>%
        summarise(media = mean(media, na.rm = TRUE), .groups = "drop") %>%
        filter(as.numeric(ano) %in% c(2018, 2019)) %>%
        arrange(mun_prova, ano) %>%
        group_by(mun_prova) %>%
        mutate(
          dup1 = 1,
          dup2 = sum(dup1),
          v1_nota = ifelse(ano == 2018, media, NA),
          v2_nota = max(v1_nota, na.rm = T),
          dmedia_escm1 = media - v2_nota
        ) %>%
        ungroup() %>%
        filter(dup2 == 2) %>%
        select(-c(dup2, dup1, v1_nota, v2_nota))

#With OUT High School
df_escm0 <- base %>%
  filter(priv0 == 1, escm == 0) %>%
  group_by(mun_prova, ano, dist_hv_border) %>%
  summarise(media = mean(media, na.rm = TRUE), .groups = "drop") %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_prova, ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2018, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    dmedia_escm0 = media - v2_nota
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup2, dup1, v1_nota, v2_nota))

###### --------------------- Timezone ---------------------------------------- #

#UTC -3
df_time1 <- base %>%
  filter(priv0 == 1, time13 == 1) %>%
  group_by(mun_prova, ano, dist_hv_border) %>%
  summarise(media = mean(media, na.rm = TRUE), .groups = "drop") %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_prova, ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2018, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    dmedia_bra1 = media - v2_nota
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup2, dup1, v1_nota, v2_nota))

#UTC -4
df_time0 <- base %>%
  filter(priv0 == 1, time13 == 0) %>%
  group_by(mun_prova, ano, dist_hv_border) %>%
  summarise(media = mean(media, na.rm = TRUE), .groups = "drop") %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_prova, ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2018, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    dmedia_bra0 = media - v2_nota
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup2, dup1, v1_nota, v2_nota))

# ---------------------------------------------------------------------------- #
##### 2.1.1.2.3 Export -----
# ---------------------------------------------------------------------------- #


df_cmo <- df_cmo %>% 
  filter(ano == 2019) %>% 
  left_join(df_escm0 %>% filter(ano == 2019) %>% select(mun_prova, dmedia_escm0),
            by = c("mun_prova")) %>%
  left_join(df_escm1 %>% filter(ano == 2019) %>% select(mun_prova, dmedia_escm1),
            by = c("mun_prova")) %>%
  left_join(df_time1 %>% filter(ano == 2019) %>% select(mun_prova, dmedia_bra1),
            by = c("mun_prova")) %>%
  left_join(df_time0 %>% filter(ano == 2019) %>% select(mun_prova, dmedia_bra0),
            by = c("mun_prova"))
  


rm(df_escm0, df_escm1, df_time0, df_time1)

# ---------------------------------------------------------------------------- #
### 2.1.2 19-17 ----
# ---------------------------------------------------------------------------- #

base_temp <- base

##### 2.1.2.1 opt bw -----
base_b <- base_temp[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
                    by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] %>%
  filter(as.numeric(ano) %in% c(2017,2019)) %>%
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2017, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    d.media = media - v2_nota
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup2, dup1, v1_nota, v2_nota))


ef <- dummy_cols(base_b$seg[base_b$ano == 2017])
ef <- ef %>% select(-1,-2)

list <- list()

list[[as.character(paste0(2019,"-",2017,"C|NF"))]] <- rdrobust(
  y = base_b$d.media[base_b$ano == 2019],
  x = base_b$dist_hv_border[base_b$ano == 2017],
  c = 0,
  cluster = base_b$seg[base_b$ano == 2017],
  weights = base_b$obs[base_b$ano == 2017],
  vce = "hc0",
  covs = cbind(
    ef,
    base_b$lat[base_b$ano == 2017],
    base_b$lon[base_b$ano == 2017]
  )
)

# ---------------------------------------------------------------------------- #
#Extração da banda ótima
bw_main_b  <- list[["2019-2017C|NF"]]$bws[1]
bw_bias_b  <- list[["2019-2017C|NF"]]$bws[2]
# ---------------------------------------------------------------------------- #

rm(list, ef)
##### 2.1.2.2. graph -----
fig <- list()

temp <- base_b %>%
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main_b ~ 1,
    .default = 0
  )
  ) %>%
  filter(
    !is.na(d.media),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(ano == 2019) %>%
  select(d.media) %>%
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(ano == 2017) %>%
  select(dist_hv_border)

# Clusters
clu <- temp %>%
  filter(ano == 2017) %>%
  select(seg)

# Latitude
latv <- temp %>%
  filter(ano == 2017) %>%
  select(lat)

# Longitude
lonv <- temp %>%
  filter(ano == 2017) %>%
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main_b,
              weights = temp$obs[temp$ano == 2017],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-4*10^5,4*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  geom_segment(aes(x = x_l_sta,
                   xend = x_l_end,
                   y = y_l_sta,
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") +
  geom_segment(aes(x = x_r_sta,
                   xend = x_r_end,
                   y = y_r_sta,
                   yend = y_r_end),
               size = 1.5, color = "#377EB8") +
  labs(x = "Distance to DST Border (km)",
       y = "Overall ENEM \n average") +
  theme_bw() +
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-5,25) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/RDD_Principal_1917_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/RDD_Principal_1917_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)



# ---------------------------------------------------------------------------- #
### 2.1.3 18-17 ----
# ---------------------------------------------------------------------------- #


##### 2.1.3.1 opt bw -----
base_c <- base_temp[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
                    by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] %>%
  filter(as.numeric(ano) %in% c(2017,2018)) %>%
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2017, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    d.media = media - v2_nota
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup2, dup1, v1_nota, v2_nota))


ef <- dummy_cols(base_c$seg[base_c$ano == 2017])
ef <- ef %>% select(-1,-2)

list <- list()

list[[as.character(paste0(2018,"-",2017,"C|NF"))]] <- rdrobust(
  y = base_c$d.media[base_c$ano == 2018],
  x = base_c$dist_hv_border[base_c$ano == 2017],
  c = 0,
  cluster = base_c$seg[base_c$ano == 2017],
  weights = base_c$obs[base_c$ano == 2017],
  vce = "hc0",
  covs = cbind(
    ef,
    base_c$lat[base_c$ano == 2017],
    base_c$lon[base_c$ano == 2017]
  )
)

# ---------------------------------------------------------------------------- #
#Extração da banda ótima
bw_main_c  <- list[["2018-2017C|NF"]]$bws[1]
bw_bias_c  <- list[["2018-2017C|NF"]]$bws[2]
# ---------------------------------------------------------------------------- #

rm(list, ef)
##### 2.1.2.2. graph -----
fig <- list()

temp <- base_c %>%
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main_c ~ 1,
    .default = 0
  )
  ) %>%
  filter(
    !is.na(d.media)#,
    #subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(ano == 2018) %>%
  select(d.media) %>%
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(ano == 2017) %>%
  select(dist_hv_border)

# Clusters
clu <- temp %>%
  filter(ano == 2017) %>%
  select(seg)

# Latitude
latv <- temp %>%
  filter(ano == 2017) %>%
  select(lat)

# Longitude
lonv <- temp %>%
  filter(ano == 2017) %>%
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              #h = bw_main_c,
              nbins = 35,
              weights = temp$obs[temp$ano == 2017],
              #subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-160*10^5,160*10^5,4*10^5)

# Gráfico

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linewidth = 1) + 
  scale_color_brewer(palette = "Set1") + 
  # geom_segment(aes(x = x_l_sta,
  #                  xend = x_l_end,
  #                  y = y_l_sta,
  #                  yend = y_l_end),
  #              size = 1.5, color = "#E41A4C") +
  # geom_segment(aes(x = x_r_sta,
  #                  xend = x_r_end,
  #                  y = y_r_sta,
  #                  yend = y_r_end),
  #              size = 1.5, color = "#377EB8") +
  labs(x = "Distance to DST Border (km)",
       y = "Overall ENEM Score") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  #ylim(-20,20) + 
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/RDD_Principal_1817_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/RDD_Principal_1817_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)

# ---------------------------------------------------------------------------- #
#### 2.1.2.3 Lowess (2017) ----
# ---------------------------------------------------------------------------- #

base_c <- base_c %>% 
  mutate(trat = ifelse(dist_hv_border > 0, 1, 0))

years <- c(2018)
cutoff <- 0.5
span_val <- 0.5   # loess span ~ Stata bw

for (y in years) {
  options(scipen = 999)
  d <- subset(base_c, ano == y)
  d$dist_km <- d$dist_hv_border/1000
  
  # compute lowess for treated and control (if they exist)
  d1 <- subset(d, trat == 1)
  d0 <- subset(d, trat == 0)
  
  lw1 <- if (nrow(d1) > 1) stats::lowess(d1$dist_km, d1$d.media, f = span_val) else NULL
  lw0 <- if (nrow(d0) > 1) stats::lowess(d0$dist_km, d0$d.media, f = span_val) else NULL
  
  png_filename <- paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/lowess/media_rdd", y, "_base_lowess.png")
  png(png_filename, width = 1200, height = 800, res = 150)
  par(mar = c(5,5,4,2) + 0.1)
  
  
  # Scatter
  plot(d$dist_km, d$d.media,
       pch = 20, cex = 0.6,
       xlab = "Distance to DST Border (km)", ylab = "ENEM Score",
       #main = paste("Lowess —", y),
       xlim = range(d$dist_km, na.rm = TRUE),
       ylim = range(d$d.media, na.rm = TRUE),
       xaxt = "n")
  
  # vertical cutoff line
  abline(v = cutoff, lty = 2, col = "darkgray", lwd = 3)
  
  x_range <- range(d$dist_km, na.rm = TRUE)
  ticks <- seq(floor(x_range[1] / 400) * 400,
               ceiling(x_range[2] / 400) * 400,
               by = 400)
  
  axis(1,
       at = ticks,
       labels = format(ticks, scientific = FALSE),
       las = 2)
  
  
  
  # add lowess lines (use different colors)
  if (!is.null(lw1)) lines(lw1, col = "red", lwd = 2)
  if (!is.null(lw0)) lines(lw0, col = "red", lwd = 2)
  
  dev.off()
  message("Wrote: ", png_filename)
}

rm(d, d0, d1, lw0, lw1, cutoff, png_filename, x_range, ticks, years, span_val, y)

# ---------------------------------------------------------------------------- #
#### 2.1.2.4 CMOGRAM -----
# ---------------------------------------------------------------------------- #
###### 2.1.2.4.1 Stata Export ----
# ---------------------------------------------------------------------------- #

df_cmo <- df_cmo %>% 
  left_join(base_c %>% filter(ano == 2018) %>% select(mun_prova, d.media) %>% 
              rename(dmedia_2018 = d.media),
            by = c("mun_prova"))


#Final data adjustments

df_cmo <- df_cmo %>% 
  rename(dmedia_easy = d.mediabl,
         dmedia_hard = d.mediabh) %>% 
  mutate(dist_km = dist_hv_border/1000) %>% 
  select(-c(dist_hv_border,obs, obs_r,
            media, media_dia1, media_dia2, media_rd, media_cn, media_lc, media_ch, media_mt,
            mediabl, mediabh, ano))

# -------------- #
# Labels --- #
# -------------- #

attr(df_cmo$dmedia, "label") <- "Nota 2019-2018"
attr(df_cmo$dmedia_rd, "label") <- "Nota Redação 2019-2018"
attr(df_cmo$dmedia_cn, "label") <- "Nota C. Naturais 2019-2018"
attr(df_cmo$dmedia_ch, "label") <- "Nota C. Humanas 2019-2018"
attr(df_cmo$dmedia_lc, "label") <- "Nota Linguagem 2019-2018"
attr(df_cmo$dmedia_mt, "label") <- "Nota Matemática 2019-2018"
attr(df_cmo$dmedia_d1, "label") <- "Nota 1° Dia 2019-2018"
attr(df_cmo$dmedia_d2, "label") <- "Nota 2° Dia 2019-2018"
attr(df_cmo$dmedia_easy, "label") <- "Nota Q. Fácil 2019-2018"
attr(df_cmo$dmedia_hard, "label") <- "Nota Q. Difícil 2019-2018"
attr(df_cmo$dmedia_escm0, "label") <- "Nota Mãe SEM EM 2019-2018"
attr(df_cmo$dmedia_escm1, "label") <- "Nota Mãe COM EM 2019-2018"
attr(df_cmo$dmedia_bra1, "label") <- "Nota UTC-3 (Brasília) 2019-2018"
attr(df_cmo$dmedia_bra0, "label") <- "Nota UTC-4 2019-2018"
attr(df_cmo$dmedia_2018, "label") <- "Nota 2018-2017"
attr(df_cmo$dist_km, "label") <- "Distância em km"

library(haven)


write_dta(df_cmo, "C:/Users/tuffyli/OneDrive - Insper/Dados_HV/dados_cmogram.dta" )



rm(df_cmo)
# ---------------------------------------------------------------------------- #
## 2.2 Dist. Res. -----
# ---------------------------------------------------------------------------- #







# ---------------------------------------------------------------------------- #
## 2.3 Bins ----
# ---------------------------------------------------------------------------- #
### A. 1918 ----
# ---------------------------------------------------------------------------- #
#### 2.3.1 POL 1 ------
# ---------------------------------------------------------------------------- #
plist <- list()

bins <- c(5, 10, 25)

for(j in bins) {
  
  temp <- base_a %>%
    mutate(subset = case_when(
      abs(dist_hv_border) < bw_main_a ~ 1,
      .default = 0
    )
    ) %>%
    filter(
      !is.na(d.media),
      subset == 1
    )
  
  # Dependent variable
  yv <- temp %>%
    filter(ano == 2019) %>%
    select(d.media) %>%
    rename(vd = 1)
  
  #45696
  # Running variable
  xv <- temp %>%
    filter(ano == 2018) %>%
    select(dist_hv_border)
  
  # Clusters
  clu <- temp %>%
    filter(ano == 2018) %>%
    select(seg)
  
  # Latitude
  latv <- temp %>%
    filter(ano == 2018) %>%
    select(lat)
  
  # Longitude
  lonv <- temp %>%
    filter(ano == 2018) %>%
    select(lon)
  
  ef <- dummy_cols(clu$seg)
  ef <- ef %>% select(-1,-2)
  
  # Estimando parâmetros do gráfico
  plist[[as.character(paste0(j))]] <- rdplot(y = yv$vd,
                                             x = xv$dist_hv_border,
                                             c = 0,
                                             p = 1,
                                             #binselect = "esmv",
                                             kernel = "triangular",
                                             h = bw_main_a,
                                             weights = temp$obs[temp$ano == 2018],
                                             subset = temp$subset == 1,
                                             hide = T,
                                             masspoints= "adjust",
                                             covs = cbind(ef,latv,lonv),
                                             nbins = c(j,j)
  )
  
  rm(yv, xv, clu, latv, lonv, ef, temp)
}

fig_loop <- names(plist)

for (i in fig_loop) {
  
  fig <- plist[[as.character(i)]]
  
  
  # df_poly <- fig$vars_poly %>%
  #   rename(x = rdplot_x, y = rdplot_y)
  # 
  # # split left / right around cutoff x == 0
  # df_left  <- df_poly %>% filter(x < 0)
  # df_right <- df_poly %>% filter(x >= 0)
  # 
  # # fit quadratic if enough points (need >= 3)
  # fit_left  <- if (nrow(df_left)  >= 3) lm(y ~ poly(x, 2, raw = TRUE), data = df_left)  else NULL
  # fit_right <- if (nrow(df_right) >= 3) lm(y ~ poly(x, 2, raw = TRUE), data = df_right) else NULL
  # 
  # # prediction grids: use your x_l_sta / x_l_end / x_r_sta / x_r_end as range endpoints
  # grid_left  <- if (!is.null(fit_left))  tibble(x = seq(x_l_sta, x_l_end, length.out = 200))  else tibble()
  # grid_right <- if (!is.null(fit_right)) tibble(x = seq(x_r_sta, x_r_end, length.out = 200)) else tibble()
  # 
  # if (nrow(grid_left)  > 0) grid_left  <- grid_left  %>% mutate(y = predict(fit_left,  newdata = grid_left))
  # if (nrow(grid_right) > 0) grid_right <- grid_right %>% mutate(y = predict(fit_right, newdata = grid_right))
  # 
  
  
  # Vetores e valores auxiliares
  fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
  fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1
  
  x_r_sta <- 0
  x_r_end <- max(fig$vars_poly$rdplot_x)
  y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                    min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                    max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
  y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]
  
  x_l_sta <- min(fig$vars_poly$rdplot_x)
  x_l_end <- 0
  
  y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
  y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                    max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                    min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
  
  xtips <- seq(-10*10^5,10*10^5,10^5)
  
  # Gráfico
  fig_gg <- ggplot() +
    geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
               alpha = 0.8, size = 2, show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_brewer(palette = "Set1") +
    geom_segment(aes(x = x_l_sta,
                     xend = x_l_end,
                     y = y_l_sta,
                     yend = y_l_end),
                 size = 1, color = "#E41A4C") +
    geom_segment(aes(x = x_r_sta,
                     xend = x_r_end,
                     y = y_r_sta,
                     yend = y_r_end),
                 size = 1, color = "#377EB8") +
    # left side
    # geom_smooth(
    #   data = df_left,
    #   aes(x = x, y = y),
    #   method = "lm",
    #   formula = y ~ poly(x, 2),
    #   se = FALSE,
    #   linewidth = 1,
    #   color = "#E41A4C"
    # ) +
    # # right side
    # geom_smooth(
    #   data = df_right,
    #   aes(x = x, y = y),
    #   method = "lm",
    #   formula = y ~ poly(x, 2),
    #   se = FALSE,
    #   linewidth = 1,
    #   color = "#377EB8"
    # )
    labs(x = "Distance to DST Border (km)",
         y = "Overall ENEM Score") +
    theme_bw() +
    scale_x_continuous(breaks = xtips,
                       labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
    ylim(-15,15) + 
    theme(axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20,angle = 90,hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 20))
  
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/1_bins_",i,"_pol1_1918_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 9, width = 13)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/1_bins_",i,"_pol1_1918_v1.pdf"),plot = fig_gg, device = "pdf",height = 9, width = 13)
  
  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)
  
}


#### 2.3.2 POL 2 ------
plist <- list()

bins <- c(5, 10, 25)

for(j in bins) {
  
  temp <- base_a %>%
    mutate(subset = case_when(
      abs(dist_hv_border) < bw_main_p ~ 1,
      .default = 0
    )
    ) %>%
    filter(
      !is.na(d.media),
      subset == 1
    )
  
  # Dependent variable
  yv <- temp %>%
    filter(ano == 2019) %>%
    select(d.media) %>%
    rename(vd = 1)
  
  #45696
  # Running variable
  xv <- temp %>%
    filter(ano == 2018) %>%
    select(dist_hv_border)
  
  # Clusters
  clu <- temp %>%
    filter(ano == 2018) %>%
    select(seg)
  
  # Latitude
  latv <- temp %>%
    filter(ano == 2018) %>%
    select(lat)
  
  # Longitude
  lonv <- temp %>%
    filter(ano == 2018) %>%
    select(lon)
  
  ef <- dummy_cols(clu$seg)
  ef <- ef %>% select(-1,-2)
  
  # Estimando parâmetros do gráfico
  plist[[as.character(paste0(j))]] <- rdplot(y = yv$vd,
                                             x = xv$dist_hv_border,
                                             c = 0,
                                             p = 2,
                                             #binselect = "esmv",
                                             kernel = "triangular",
                                             h = bw_main_p,
                                             weights = temp$obs[temp$ano == 2018],
                                             subset = temp$subset == 1,
                                             hide = T,
                                             masspoints= "adjust",
                                             covs = cbind(ef,latv,lonv),
                                             nbins = c(j,j)
  )
  
  rm(yv, xv, clu, latv, lonv, ef, temp)
}

fig_loop <- names(plist)

for (i in fig_loop) {
  
  fig <- plist[[as.character(i)]]
  
  # Vetores e valores auxiliares
  fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
  fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1
  
  
  fig$vars_poly <- fig$vars_poly %>%
    mutate(x = rdplot_x, y = rdplot_y)
  
  
  x_r_sta <- 0
  x_r_end <- fig$vars_poly$rdplot_x
  y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                    min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                    max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
  y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]
  
  x_l_sta <- min(fig$vars_poly$rdplot_x)
  x_l_end <- 0
  
  y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
  y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                    max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                    min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
  
  xtips <- seq(-10*10^5,10*10^5,10^5)
  
  
  vars_poly_left <- fig$vars_poly %>%
    filter(rdplot_x < 0,
           rdplot_x >= min(fig$vars_bins$rdplot_mean_x, na.rm = TRUE))
  
  vars_poly_right <- fig$vars_poly %>%
    filter(rdplot_x > 0,
           rdplot_x <= max(fig$vars_bins$rdplot_mean_x, na.rm = TRUE))
  
  # Gráfico
  fig_gg <- ggplot() +
    geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
               alpha = 1, size = 2, show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_brewer(palette = "Set1") +
    geom_line(data = vars_poly_right, aes(x = rdplot_x, y = rdplot_y, color = factor(hv)), size = 1, show.legend = F)+
    geom_line(data = vars_poly_left, aes(x = rdplot_x, y = rdplot_y, color = factor(hv)), size = 1, show.legend = F) +
    
    labs(x = "Distance to DST Border (km)",
         y = "Overall ENEM Score") +
    theme_bw() +
    scale_x_continuous(breaks = xtips,
                       labels = (xtips /1000 ) %>% formatC(digits = 0, format = "f")) +
    ylim(-15,15) + 
    theme(axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20,angle = 90,hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 20))
  
  
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/1_bins_",i,"_pol2_1918_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 9, width = 13)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/1_bins_",i,"_pol2_1918_v1.pdf"),plot = fig_gg, device = "pdf",height = 9, width = 13)
  
  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips, vars_poly_left, vars_poly_right)
  
}

rm(bins, j, plist, fig_loop)
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
### B. 1917 ----
#### 2.3.1 POL 1 ------
# ---------------------------------------------------------------------------- #
plist <- list()

bins <- c(5, 10, 25)

for(j in bins) {
  
  temp <- base_b %>%
    mutate(subset = case_when(
      abs(dist_hv_border) < bw_main_b ~ 1,
      .default = 0
    )
    ) %>%
    filter(
      !is.na(d.media),
      subset == 1
    )
  
  # Dependent variable
  yv <- temp %>%
    filter(ano == 2019) %>%
    select(d.media) %>%
    rename(vd = 1)
  
  #45696
  # Running variable
  xv <- temp %>%
    filter(ano == 2017) %>%
    select(dist_hv_border)
  
  # Clusters
  clu <- temp %>%
    filter(ano == 2017) %>%
    select(seg)
  
  # Latitude
  latv <- temp %>%
    filter(ano == 2017) %>%
    select(lat)
  
  # Longitude
  lonv <- temp %>%
    filter(ano == 2017) %>%
    select(lon)
  
  ef <- dummy_cols(clu$seg)
  ef <- ef %>% select(-1,-2)
  
  # Estimando parâmetros do gráfico
  plist[[as.character(paste0(j))]] <- rdplot(y = yv$vd,
                                             x = xv$dist_hv_border,
                                             c = 0,
                                             p = 1,
                                             #binselect = "esmv",
                                             kernel = "triangular",
                                             h = bw_main_b,
                                             weights = temp$obs[temp$ano == 2017],
                                             subset = temp$subset == 1,
                                             hide = T,
                                             masspoints= "adjust",
                                             covs = cbind(ef,latv,lonv),
                                             nbins = c(j,j)
  )
  
  rm(yv, xv, clu, latv, lonv, ef, temp)
}

fig_loop <- names(plist)

for (i in fig_loop) {
  
  fig <- plist[[as.character(i)]]
  
  # Vetores e valores auxiliares
  fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
  fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1
  
  x_r_sta <- 0
  x_r_end <- max(fig$vars_poly$rdplot_x)
  y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                    min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                    max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
  y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]
  
  x_l_sta <- min(fig$vars_poly$rdplot_x)
  x_l_end <- 0
  
  y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
  y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                    max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                    min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
  
  xtips <- seq(-10*10^5,10*10^5,10^5)
  
  # Gráfico
  fig_gg <- ggplot() +
    geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
               alpha = 0.8, size = 2, show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_brewer(palette = "Set1") +
    geom_segment(aes(x = x_l_sta,
                     xend = x_l_end,
                     y = y_l_sta,
                     yend = y_l_end),
                 size = 1, color = "#E41A4C") +
    geom_segment(aes(x = x_r_sta,
                     xend = x_r_end,
                     y = y_r_sta,
                     yend = y_r_end),
                 size = 1, color = "#377EB8") +
    labs(x = "Distance to DST Border (km)",
         y = "Overall ENEM \n average") +
    theme_bw() +
    scale_x_continuous(breaks = xtips,
                       labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
    ylim(-5,25) + 
    theme(axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 35),
          axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 30))
  
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/2_bins_",i,"_pol1_1917_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 9, width = 13)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/2_bins_",i,"_pol1_1917_v1.pdf"),plot = fig_gg, device = "pdf",height = 9, width = 13)
  
  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)
  
}


#### 2.3.2 POL 2 ------
plist <- list()

bins <- c(5, 10, 25)

for(j in bins) {
  
  temp <- base_b %>%
    mutate(subset = case_when(
      abs(dist_hv_border) < bw_main_p ~ 1,
      .default = 0
    )
    ) %>%
    filter(
      !is.na(d.media),
      subset == 1
    )
  
  # Dependent variable
  yv <- temp %>%
    filter(ano == 2019) %>%
    select(d.media) %>%
    rename(vd = 1)
  
  #45696
  # Running variable
  xv <- temp %>%
    filter(ano == 2017) %>%
    select(dist_hv_border)
  
  # Clusters
  clu <- temp %>%
    filter(ano == 2017) %>%
    select(seg)
  
  # Latitude
  latv <- temp %>%
    filter(ano == 2017) %>%
    select(lat)
  
  # Longitude
  lonv <- temp %>%
    filter(ano == 2017) %>%
    select(lon)
  
  ef <- dummy_cols(clu$seg)
  ef <- ef %>% select(-1,-2)
  
  # Estimando parâmetros do gráfico
  plist[[as.character(paste0(j))]] <- rdplot(y = yv$vd,
                                             x = xv$dist_hv_border,
                                             c = 0,
                                             p = 2,
                                             #binselect = "esmv",
                                             kernel = "triangular",
                                             h = bw_main_p,
                                             weights = temp$obs[temp$ano == 2017],
                                             subset = temp$subset == 1,
                                             hide = T,
                                             masspoints= "adjust",
                                             covs = cbind(ef,latv,lonv),
                                             nbins = c(j,j)
  )
  
  rm(yv, xv, clu, latv, lonv, ef, temp)
}

fig_loop <- names(plist)

for (i in fig_loop) {
  
  fig <- plist[[as.character(i)]]
  
  # Vetores e valores auxiliares
  fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
  fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1
  
  
  fig$vars_poly <- fig$vars_poly %>%
    mutate(x = rdplot_x, y = rdplot_y)
  
  
  x_r_sta <- 0
  x_r_end <- fig$vars_poly$rdplot_x
  y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                    min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                    max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
  y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]
  
  x_l_sta <- min(fig$vars_poly$rdplot_x)
  x_l_end <- 0
  
  y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
  y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                    max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                    min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
  
  xtips <- seq(-10*10^5,10*10^5,10^5)
  
  
  vars_poly_left <- fig$vars_poly %>%
    filter(rdplot_x < 0,
           rdplot_x >= min(fig$vars_bins$rdplot_mean_x, na.rm = TRUE))
  
  vars_poly_right <- fig$vars_poly %>%
    filter(rdplot_x > 0,
           rdplot_x <= max(fig$vars_bins$rdplot_mean_x, na.rm = TRUE))
  
  # Gráfico
  fig_gg <- ggplot() +
    geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
               alpha = 1, size = 2, show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_brewer(palette = "Set1") +
    geom_line(data = vars_poly_right, aes(x = rdplot_x, y = rdplot_y, color = factor(hv)), size = 1, show.legend = F)+
    geom_line(data = vars_poly_left, aes(x = rdplot_x, y = rdplot_y, color = factor(hv)), size = 1, show.legend = F) +
    
    labs(x = "Distance to DST Border (km)",
         y = "Overall ENEM \n average") +
    theme_bw() +
    scale_x_continuous(breaks = xtips,
                       labels = (xtips /1000 ) %>% formatC(digits = 0, format = "f")) +
    ylim(-5,25) + 
    theme(axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20,angle = 90,hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 20))
  
  
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/2_bins_",i,"_pol4_1917_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 9, width = 13)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/2_bins_",i,"_pol4_1917_v1.pdf"),plot = fig_gg, device = "pdf",height = 9, width = 13)
  
  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips, vars_poly_left, vars_poly_right)
  
}

rm(bins, j, plist, fig_loop)
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #

### C. 1817 ----
#### 2.3.1 POL 1 ------
plist <- list()

bins <- c(5, 10, 25)

for(j in bins) {
  
  temp <- base_c %>%
    mutate(subset = case_when(
      abs(dist_hv_border) < bw_main_c ~ 1,
      .default = 0
    )
    ) %>%
    filter(
      !is.na(d.media),
      subset == 1
    )
  
  # Dependent variable
  yv <- temp %>%
    filter(ano == 2018) %>%
    select(d.media) %>%
    rename(vd = 1)
  
  #45696
  # Running variable
  xv <- temp %>%
    filter(ano == 2017) %>%
    select(dist_hv_border)
  
  # Clusters
  clu <- temp %>%
    filter(ano == 2017) %>%
    select(seg)
  
  # Latitude
  latv <- temp %>%
    filter(ano == 2017) %>%
    select(lat)
  
  # Longitude
  lonv <- temp %>%
    filter(ano == 2017) %>%
    select(lon)
  
  ef <- dummy_cols(clu$seg)
  ef <- ef %>% select(-1,-2)
  
  # Estimando parâmetros do gráfico
  plist[[as.character(paste0(j))]] <- rdplot(y = yv$vd,
                                             x = xv$dist_hv_border,
                                             c = 0,
                                             p = 1,
                                             #binselect = "esmv",
                                             kernel = "triangular",
                                             h = bw_main_c,
                                             weights = temp$obs[temp$ano == 2017],
                                             subset = temp$subset == 1,
                                             hide = T,
                                             masspoints= "adjust",
                                             covs = cbind(ef,latv,lonv),
                                             nbins = c(j,j)
  )
  
  rm(yv, xv, clu, latv, lonv, ef, temp)
}

fig_loop <- names(plist)

for (i in fig_loop) {
  
  fig <- plist[[as.character(i)]]
  
  # Vetores e valores auxiliares
  fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
  fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1
  
  x_r_sta <- 0
  x_r_end <- max(fig$vars_poly$rdplot_x)
  y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                    min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                    max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
  y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]
  
  x_l_sta <- min(fig$vars_poly$rdplot_x)
  x_l_end <- 0
  
  y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
  y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                    max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                    min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
  
  xtips <- seq(-10*10^5,10*10^5,10^5)
  
  # Gráfico
  fig_gg <- ggplot() +
    geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
               alpha = 0.8, size = 2, show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_brewer(palette = "Set1") +
    geom_segment(aes(x = x_l_sta,
                     xend = x_l_end,
                     y = y_l_sta,
                     yend = y_l_end),
                 size = 1, color = "#E41A4C") +
    geom_segment(aes(x = x_r_sta,
                     xend = x_r_end,
                     y = y_r_sta,
                     yend = y_r_end),
                 size = 1, color = "#377EB8") +
    labs(x = "Distance to DST Border (km)",
         y = "Overall ENEM Score") +
    theme_bw() +
    scale_x_continuous(breaks = xtips,
                       labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
    ylim(-5,25) + 
    theme(axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20,angle = 90,hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 20))
  
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/3_bins_",i,"_pol1_1817_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 9, width = 13)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/3_bins_",i,"_pol1_1817_v1.pdf"),plot = fig_gg, device = "pdf",height = 9, width = 13)
  
  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)
  
}


#### 2.3.2 POL 2 ------
plist <- list()

bins <- c(5, 10, 25)

for(j in bins) {
  
  temp <- base_c %>%
    mutate(subset = case_when(
      abs(dist_hv_border) < bw_main_p ~ 1,
      .default = 0
    )
    ) %>%
    filter(
      !is.na(d.media),
      subset == 1
    )
  
  # Dependent variable
  yv <- temp %>%
    filter(ano == 2018) %>%
    select(d.media) %>%
    rename(vd = 1)
  
  #45696
  # Running variable
  xv <- temp %>%
    filter(ano == 2017) %>%
    select(dist_hv_border)
  
  # Clusters
  clu <- temp %>%
    filter(ano == 2017) %>%
    select(seg)
  
  # Latitude
  latv <- temp %>%
    filter(ano == 2017) %>%
    select(lat)
  
  # Longitude
  lonv <- temp %>%
    filter(ano == 2017) %>%
    select(lon)
  
  ef <- dummy_cols(clu$seg)
  ef <- ef %>% select(-1,-2)
  
  # Estimando parâmetros do gráfico
  plist[[as.character(paste0(j))]] <- rdplot(y = yv$vd,
                                             x = xv$dist_hv_border,
                                             c = 0,
                                             p = 2,
                                             #binselect = "esmv",
                                             kernel = "triangular",
                                             h = bw_main_p,
                                             weights = temp$obs[temp$ano == 2017],
                                             subset = temp$subset == 1,
                                             hide = T,
                                             masspoints= "adjust",
                                             covs = cbind(ef,latv,lonv),
                                             nbins = c(j,j)
  )
  
  rm(yv, xv, clu, latv, lonv, ef, temp)
}

fig_loop <- names(plist)

for (i in fig_loop) {
  
  fig <- plist[[as.character(i)]]
  
  # Vetores e valores auxiliares
  fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
  fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1
  
  
  fig$vars_poly <- fig$vars_poly %>%
    mutate(x = rdplot_x, y = rdplot_y)
  
  
  x_r_sta <- 0
  x_r_end <- fig$vars_poly$rdplot_x
  y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                    min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                    max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
  y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]
  
  x_l_sta <- min(fig$vars_poly$rdplot_x)
  x_l_end <- 0
  
  y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
  y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2],
                    max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                    min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
  
  xtips <- seq(-10*10^5,10*10^5,10^5)
  
  
  vars_poly_left <- fig$vars_poly %>%
    filter(rdplot_x < 0,
           rdplot_x >= min(fig$vars_bins$rdplot_mean_x, na.rm = TRUE))
  
  vars_poly_right <- fig$vars_poly %>%
    filter(rdplot_x > 0,
           rdplot_x <= max(fig$vars_bins$rdplot_mean_x, na.rm = TRUE))
  
  # Gráfico
  fig_gg <- ggplot() +
    geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
               alpha = 1, size = 2, show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_brewer(palette = "Set1") +
    geom_line(data = vars_poly_right, aes(x = rdplot_x, y = rdplot_y, color = factor(hv)), size = 1, show.legend = F)+
    geom_line(data = vars_poly_left, aes(x = rdplot_x, y = rdplot_y, color = factor(hv)), size = 1, show.legend = F) +
    
    labs(x = "Distance to DST Border (km)",
         y = "Overall ENEM Score") +
    theme_bw() +
    scale_x_continuous(breaks = xtips,
                       labels = (xtips /1000 ) %>% formatC(digits = 0, format = "f")) +
    ylim(-5,25) + 
    theme(axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20,angle = 90,hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 20))
  
  
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/3_bins_",i,"_pol2_1817_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 9, width = 13)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/3_bins_",i,"_pol2_1817_v1.pdf"),plot = fig_gg, device = "pdf",height = 9, width = 13)
  
  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips, vars_poly_left, vars_poly_right)
  
}

rm(bins, j, plist, fig_loop, base_c, base_b, bw_bias_b, bw_main_b,
   bw_main_c, bw_bias_c)

rm(base_temp)

#Final adjustment for the remaining
base <- base %>% filter(ano != 2017)

# ---------------------------------------------------------------------------- #
# 3. Matérias----
# ---------------------------------------------------------------------------- #
#Bases


# ---------------- #
# A
# ---------------- #
base_a <- base[priv0 == 1,.(media_rd = mean(rd, na.rm = T),
                            media_cn = mean(cn, na.rm = T),
                            media_lc = mean(lc, na.rm = T),
                            media_ch = mean(ch, na.rm = T),
                            media_mt = mean(mt, na.rm = T),obs = .N),
               by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] 

#Calculando as diferenças

#Base A
base_a <- base_a %>%
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    
    #Redação
    v1_rd = ifelse(ano == 2018, media_rd, NA),
    v2_rd = max(v1_rd, na.rm = T),
    d.media_rd = media_rd - v2_rd,
    
    #Ciências Naturais
    v1_cn = ifelse(ano == 2018, media_cn, NA),
    v2_cn = max(v1_cn, na.rm = T),
    d.media_cn = media_cn - v2_cn,
    
    #Ciências Humanas
    v1_ch = ifelse(ano == 2018, media_ch, NA),
    v2_ch = max(v1_ch, na.rm = T),
    d.media_ch = media_ch - v2_ch,
    
    #Lingua Portuguesa
    v1_lc = ifelse(ano == 2018, media_lc, NA),
    v2_lc = max(v1_lc, na.rm = T),
    d.media_lc = media_lc - v2_lc,
    
    #Matematica
    v1_mt = ifelse(ano == 2018, media_mt, NA),
    v2_mt = max(v1_mt, na.rm = T),
    d.media_mt = media_mt - v2_mt
    
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup1,dup2,v1_rd,v2_rd,
            v1_cn, v2_cn, v1_ch,v2_ch,v1_lc,v2_lc,v1_mt,v2_mt))




#Calculando as diferenças





## 3.1. Reg ----

p_list <- list()

d_list <- c("d.media_rd", "d.media_lc", "d.media_ch", "d.media_cn", "d.media_mt")

### A. TC ----
for (i in d_list){
  
  #Com Controles
  
  ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  p_list[[as.character(paste0("wc_",i,"|TC"))]] <-
    rdrobust(
      y = base_a[[i]][base_a$ano == 2019],
      x = base_a$dist_hv_border[base_a$ano == 2018],
      c = 0,
      p = 1,
      h = bw_main_a,
      b = bw_bias_a,
      cluster = base_a$seg[base_a$ano == 2018],
      weights = base_a$obs[base_a$ano == 2018],
      vce = "hc0",
      covs = cbind(
        ef, 
        base_a$lat[base_a$ano == 2018], 
        base_a$lon[base_a$ano == 2018]
      )
    )
  
  
}



rm(ef, i)

## 3.2 Tabela ----
notas_tab <- data.frame(
  coef = do.call(rbind,lapply(p_list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(p_list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(p_list, FUN = function(x){x$pv[3]})),
  n.1 = do.call(rbind, lapply(p_list, FUN = function(x){x$N_h[1]})),
  n.2 = do.call(rbind, lapply(p_list, FUN = function(x){x$N_h[2]}))
  
)


notas_tab <- notas_tab %>% 
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "**", 
                         ifelse(pv < 0.05, "*", 
                                ifelse(pv < 0.1, "", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]"),
    esp = 1:5,
    id = 1
  ) %>%
  select(-c(pv))

names <- c("2019 - 2018",
           " "," ")

result <- data.frame(
  var = names,
  rd = rep(NA, times = length(names)),
  lc = rep(NA, times = length(names)),
  ch = rep(NA, times = length(names)),
  
  cn = rep(NA, times = length(names)),
  mt = rep(NA, times = length(names))
)

#A
result$cn[1] <- notas_tab$coef[[4]]
result$cn[2] <- notas_tab$se[[4]]
result$cn[3] <- notas_tab$N[[4]]

result$mt[1] <- notas_tab$coef[[5]]
result$mt[2] <- notas_tab$se[[5]]
result$mt[3] <- notas_tab$N[[5]]

result$rd[1] <- notas_tab$coef[[1]]
result$rd[2] <- notas_tab$se[[1]]
result$rd[3] <- notas_tab$N[[1]]

result$lc[1] <- notas_tab$coef[[2]]
result$lc[2] <- notas_tab$se[[2]]
result$lc[3] <- notas_tab$N[[2]]

result$ch[1] <- notas_tab$coef[[3]]
result$ch[2] <- notas_tab$se[[3]]
result$ch[3] <- notas_tab$N[[3]]




colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Materias_v1.tex")

# ---------------------------------------------------------------------------- #
## 3.3 Gráfico red (19-18) ----
# ---------------------------------------------------------------------------- #

fig <- list()

temp <- base_a %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main_a ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.media_rd)#,
    #subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(ano == 2019) %>% 
  select(d.media_rd) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(ano == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(ano == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(ano == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(ano == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p =1,
              #binselect = "esmv",
              kernel = "triangular",
              #h = bw_main_a,
              nbins = 35,
              #b = bw_bias_a,
              weights = temp$obs[temp$ano == 2018],
              #subset = temp$subset == 1,
              hide = T,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-160*10^5,160*10^5,4*10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linewidth = 1) + 
  scale_color_brewer(palette = "Set1") + 
  labs(x = "Distance to DST Border (km)",
       y = "Average Writing Score") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(40,105) + 
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/RDD_Redacao.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/RDD_Redacao.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)




rm(notas_tab, p_list, result, d_list, latex_table)



# ---------------------------------------------------------------------------- #
## 3.4 Gráfico red (18-17) -----
# ---------------------------------------------------------------------------- #
### 3.4.1 Dados ----
# ---------------------------------------------------------------------------- #


#Agregando os dados para 2018 e 2017
base_c <- base_temp[priv0 == 1,.(media_rd = mean(rd, na.rm = T),
                                 media_cn = mean(cn, na.rm = T),
                                 media_lc = mean(lc, na.rm = T),
                                 media_ch = mean(ch, na.rm = T),
                                 media_mt = mean(mt, na.rm = T),obs = .N),
                    by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)]  %>%
  filter(as.numeric(ano) %in% c(2017,2018))


#Base C
base_c <- base_c %>%
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    
    #Redação
    v1_rd = ifelse(ano == 2017, media_rd, NA),
    v2_rd = max(v1_rd, na.rm = T),
    d.media_rd = media_rd - v2_rd,
    
    #Ciências Naturais
    v1_cn = ifelse(ano == 2017, media_cn, NA),
    v2_cn = max(v1_cn, na.rm = T),
    d.media_cn = media_cn - v2_cn,
    
    #Ciências Humanas
    v1_ch = ifelse(ano == 2017, media_ch, NA),
    v2_ch = max(v1_ch, na.rm = T),
    d.media_ch = media_ch - v2_ch,
    
    #Lingua Portuguesa
    v1_lc = ifelse(ano == 2017, media_lc, NA),
    v2_lc = max(v1_lc, na.rm = T),
    d.media_lc = media_lc - v2_lc,
    
    #Matematica
    v1_mt = ifelse(ano == 2017, media_mt, NA),
    v2_mt = max(v1_mt, na.rm = T),
    d.media_mt = media_mt - v2_mt
    
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup1,dup2,v1_rd,v2_rd,
            v1_cn, v2_cn, v1_ch,v2_ch,v1_lc,v2_lc,v1_mt,v2_mt))


# --------------------------------------------------------------------------- #
### 3.4.2 Gráfico ----
# --------------------------------------------------------------------------- #


fig <- list()

temp <- base_c %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main_a ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.media_rd)#,
    #subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(ano == 2018) %>% 
  select(d.media_rd) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(ano == 2017) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(ano == 2017) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(ano == 2017) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(ano == 2017) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p =1,
              #binselect = "esmv",
              kernel = "triangular",
              #h = bw_main_a,
              nbins = 35,
              #b = bw_bias_a,
              weights = temp$obs[temp$ano == 2017],
              #subset = temp$subset == 1,
              hide = T,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-160*10^5,160*10^5,4*10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linewidth = 1) + 
  #geom_vline(xintercept = bw_main_a, linediwdth = 0.75, alpha = 0.3) +
  scale_color_brewer(palette = "Set1") + 
  labs(x = "Distance to DST Border (km)",
       y = "Average Writing Score") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-50, 10) + 
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/RDD_Redacao_17.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/RDD_Redacao_17.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)




rm(notas_tab, p_list, result, d_list, latex_table, base_c)



# ---------------------------------------------------------------------------- #
# 4. Redação ----
# ---------------------------------------------------------------------------- #

# Agregados dos critérios
base_a <- base[priv0 == 1,.(media_rd1 = mean(rd1, na.rm = T),
                            media_rd2 = mean(rd2, na.rm = T),
                            media_rd3 = mean(rd3, na.rm = T),
                            media_rd4 = mean(rd4, na.rm = T),
                            media_rd5 = mean(rd5, na.rm = T),
                            obs = .N),
               by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] 


#Calculando as diferenças
base_a <- base_a %>%
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    
    #Redação - 1
    v1_rd1 = ifelse(ano == 2018, media_rd1, NA),
    v2_rd1 = max(v1_rd1, na.rm = T),
    d.media_rd1 = media_rd1 - v2_rd1,
    
    #Redação - 2
    v1_rd2 = ifelse(ano == 2018, media_rd2, NA),
    v2_rd2 = max(v1_rd2, na.rm = T),
    d.media_rd2 = media_rd2 - v2_rd2,
    
    #Redação - 3
    v1_rd3 = ifelse(ano == 2018, media_rd3, NA),
    v2_rd3 = max(v1_rd3, na.rm = T),
    d.media_rd3 = media_rd3 - v2_rd3,
    
    #Redação - 4
    v1_rd4 = ifelse(ano == 2018, media_rd4, NA),
    v2_rd4 = max(v1_rd4, na.rm = T),
    d.media_rd4 = media_rd4 - v2_rd4,
    
    #Redação - 5
    v1_rd5 = ifelse(ano == 2018, media_rd5, NA),
    v2_rd5 = max(v1_rd5, na.rm = T),
    d.media_rd5 = media_rd5 - v2_rd5,
    
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup1,dup2,v1_rd1,v2_rd1,v1_rd2,v2_rd2,v1_rd3,v2_rd3,v1_rd4,v2_rd4,v1_rd5,v2_rd5))

# ---------------------------------------------------------------------------- #
##4.1 Reg ----

p_list <- list()

d_list <- c("d.media_rd1", "d.media_rd2", "d.media_rd3", "d.media_rd4", "d.media_rd5")


### A. TC ----
for (i in d_list){
  
  #Com Controles
  
  ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  p_list[[as.character(paste0("Wc_",i,"|TC"))]] <-
    rdrobust(
      y = base_a[[i]][base_a$ano == 2019],
      x = base_a$dist_hv_border[base_a$ano == 2018],
      c = 0,
      h = bw_main_a,
      b = bw_bias_a,
      cluster = base_a$seg[base_a$ano == 2018],
      weights = base_a$obs[base_a$ano == 2018],
      vce = "hc0",
      covs = cbind(
        ef, 
        base_a$lat[base_a$ano == 2018], 
        base_a$lon[base_a$ano == 2018]
      )
    )
  
  
}
rm(ef,i)



##4.2 Tab ----


red_tab <- data.frame(
  coef = do.call(rbind,lapply(p_list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(p_list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(p_list, FUN = function(x){x$pv[3]})),
  n.1 = do.call(rbind,lapply(p_list, FUN = function(x){x$N_h[1]})),
  n.2 = do.call(rbind,lapply(p_list, FUN = function(x){x$N_h[2]}))
)


red_tab <- red_tab %>% 
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "**", 
                         ifelse(pv < 0.05, "*", 
                                ifelse(pv < 0.1, "", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]"),
    esp = 1:5,
    id = 1
  ) %>%
  select(-c(pv))





row <- c(
  "Proficiency in formal written language",
  " "," ",
  "Comprehension of essay theme",
  " "," ",
  "Organization and structure of arguments",
  " ", " ",
  "Use of linguistic mechanisms",
  " ", " ",
  "Proposal of intervention",
  " ", " ")

result <- data.frame(
  var = row,
  ba = rep(NA, times = length(row))
)




# BA
result$ba[1] <- red_tab$coef[[1]]
result$ba[2] <- red_tab$se[[1]]
result$ba[3] <- red_tab$N[[1]]
result$ba[4] <- red_tab$coef[[2]]
result$ba[5] <- red_tab$se[[2]]
result$ba[6] <- red_tab$N[[2]]
result$ba[7] <- red_tab$coef[[3]]
result$ba[8] <- red_tab$se[[3]]
result$ba[9] <- red_tab$N[[3]]
result$ba[10] <- red_tab$coef[[4]]
result$ba[11] <- red_tab$se[[4]]
result$ba[12] <- red_tab$N[[4]]
result$ba[13] <- red_tab$coef[[5]]
result$ba[14] <- red_tab$se[[5]]
result$ba[15] <- red_tab$N[[5]]





colnames(result) <- c("", "(1)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lc",
  linesep = ""
)

writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Redacao_v1.tex")


rm(p_list, red_tab, result, d_list, latex_table, row)


# ---------------------------------------------------------------------------- #
# 5. Dificuldade ----
# ---------------------------------------------------------------------------- #
gc()
# Agregados dos critérios
base_a <- base[priv0 == 1,.(media_ac_bl_ch = mean(acerto_bl_ch, na.rm = T),
                            media_ac_bh_ch = mean(acerto_bh_ch, na.rm = T),
                            media_ac_bl_cn = mean(acerto_bl_cn, na.rm = T),
                            media_ac_bh_cn = mean(acerto_bh_cn, na.rm = T),
                            media_ac_bl_lc = mean(acerto_bl_lc, na.rm = T),
                            media_ac_bh_lc = mean(acerto_bh_lc, na.rm = T),
                            media_ac_bl_mt = mean(acerto_bl_mt, na.rm = T),
                            media_ac_bh_mt = mean(acerto_bh_mt, na.rm = T),
                            mediabl = mean(acerto_pbl, na.rm = T),
                            mediabh = mean(acerto_pbh, na.rm = T),
                            obs = .N),
               by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)]


#Calculando as diferenças
base_a <- base_a %>%
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1)) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  group_by(mun_prova) %>%
  mutate(
    
    #TOTAL
    
    v1_pbl = ifelse(ano == 2018, mediabl, NA),
    v2_pbl = max(v1_pbl, na.rm = T),
    d.mediabl = mediabl - v2_pbl,
    
    v1_pbh = ifelse(ano == 2018, mediabh, NA),
    v2_pbh = max(v1_pbh, na.rm = T),
    d.mediabh = mediabh - v2_pbh,
    
    # Ciências Humanas
    ## Facil
    v1_bl_ch = ifelse(ano == 2018, media_ac_bl_ch, NA),
    v2_bl_ch = max(v1_bl_ch, na.rm = T),
    d.media_bl_ch = media_ac_bl_ch - v2_bl_ch,
    
    ## Dificil
    v1_bh_ch = ifelse(ano == 2018, media_ac_bh_ch, NA),
    v2_bh_ch = max(v1_bh_ch, na.rm = T),
    d.media_bh_ch = media_ac_bh_ch - v2_bh_ch,
    
    
    # Ciências Naturais
    ## Facil
    v1_bl_cn = ifelse(ano == 2018, media_ac_bl_cn, NA),
    v2_bl_cn = max(v1_bl_cn, na.rm = T),
    d.media_bl_cn = media_ac_bl_cn - v2_bl_cn,
    
    ## Dificil
    v1_bh_cn = ifelse(ano == 2018, media_ac_bh_cn, NA),
    v2_bh_cn = max(v1_bh_cn, na.rm = T),
    d.media_bh_cn = media_ac_bh_cn - v2_bh_cn,
    
    
    # Lingua
    ## Facil
    v1_bl_lc = ifelse(ano == 2018, media_ac_bl_lc, NA),
    v2_bl_lc = max(v1_bl_lc, na.rm = T),
    d.media_bl_lc = media_ac_bl_lc - v2_bl_lc,
    
    ## Dificil
    v1_bh_lc = ifelse(ano == 2018, media_ac_bh_lc, NA),
    v2_bh_lc = max(v1_bh_lc, na.rm = T),
    d.media_bh_lc = media_ac_bh_lc - v2_bh_lc,
    
    
    # Matematica
    ## Facil
    v1_bl_mt = ifelse(ano == 2018, media_ac_bl_mt, NA),
    v2_bl_mt = max(v1_bl_mt, na.rm = T),
    d.media_bl_mt = media_ac_bl_mt - v2_bl_mt,
    
    ## Dificil
    v1_bh_mt = ifelse(ano == 2018, media_ac_bh_mt, NA),
    v2_bh_mt = max(v1_bh_mt, na.rm = T),
    d.media_bh_mt = media_ac_bh_mt - v2_bh_mt
    
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup1,dup2,
            v1_bl_ch,v2_bl_ch, v1_bh_ch, v2_bh_ch,
            v1_bl_cn,v2_bl_cn, v1_bh_cn, v2_bh_cn,
            v1_bl_lc,v2_bl_lc, v1_bh_lc, v2_bh_lc,
            v1_bl_mt,v2_bl_mt, v1_bh_mt, v2_bh_mt,
            v1_pbh, v1_pbl, v2_pbl, v2_pbh))

# ---------------------------------------------------------------------------- #
##5.1 Reg ----
# ---------------------------------------------------------------------------- #
rp_list <- list()

d_list <- c("d.mediabl", "d.mediabh", "d.media_bl_lc", "d.media_bh_lc",
            "d.media_bl_ch", "d.media_bh_ch", "d.media_bl_cn", "d.media_bh_cn",
            "d.media_bl_mt", "d.media_bh_mt")

for (i in d_list){
  
  #Com Controles
  
  ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  rp_list[[as.character(paste0("cc_",i,"|TC"))]] <-
    rdrobust(
      y = base_a[[i]][base_a$ano == 2019],
      x = base_a$dist_hv_border[base_a$ano == 2018],
      c = 0,
      h = bw_main_a,
      b = bw_bias_a,
      cluster = base_a$seg[base_a$ano == 2018],
      weights = base_a$obs[base_a$ano == 2018],
      vce = "hc0",
      covs = cbind(
        ef,
        base_a$lat[base_a$ano == 2018],
        base_a$lon[base_a$ano == 2018]
      )
    )
  
  
}
rm(ef,i)


# ---------------------------------------------------------------------------- #
###5.1.1 Dif (Easy - Hard)----
# ---------------------------------------------------------------------------- #
base_a <- base_a %>%
  group_by(mun_prova,
           ano) %>%
  mutate(
    dif_avg = d.mediabl - d.mediabh,
    dif_lc = d.media_bl_lc - d.media_bh_lc,
    dif_ch = d.media_bl_ch - d.media_bh_ch,
    dif_cn = d.media_bl_cn - d.media_bh_cn,
    dif_mt = d.media_bl_mt - d.media_bh_mt
  )

new_list <- c("dif_avg", "dif_lc", "dif_ch", "dif_cn", "dif_mt")
dif_list <- list()

for (i in new_list){
  
  
  
  
  
  #Com Controles
  
  ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  dif_list[[as.character(paste0("cc_",i,"|TC"))]] <-
    rdrobust(
      y = base_a[[i]][base_a$ano == 2019],
      x = base_a$dist_hv_border[base_a$ano == 2018],
      c = 0,
      h = bw_main_a,
      b = bw_bias_a,
      cluster = base_a$seg[base_a$ano == 2018],
      weights = base_a$obs[base_a$ano == 2018],
      vce = "hc0",
      covs = cbind(
        ef,
        base_a$lat[base_a$ano == 2018],
        base_a$lon[base_a$ano == 2018]
      )
    )
  
  
}
rm(ef,i, new_list)


# ---------------------------------------------------------------------------- #
##5.2 Tab ----
# ---------------------------------------------------------------------------- #


mat_nota <- data.frame(
  coef = do.call(rbind,lapply(rp_list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(rp_list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(rp_list, FUN = function(x){x$pv[3]})),
  n = do.call(rbind,lapply(rp_list, FUN = function(x){x$N_h}))
)





mat_nota <- mat_nota %>%
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "**",
                         ifelse(pv < 0.05, "*",
                                ifelse(pv < 0.1, "", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]"),
    #esp = 1:20,
    id = 1
  ) %>%
  select(-c(pv))



# ---------------------------------------------------------------------------- #
### 5.2.1 DIF TAB ----
#

dif_tab <- data.frame(
  coef = do.call(rbind,lapply(dif_list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(dif_list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(dif_list, FUN = function(x){x$pv[3]})),
  n = do.call(rbind,lapply(dif_list, FUN = function(x){x$N_h}))
)


dif_tab <- dif_tab %>%
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "**",
                         ifelse(pv < 0.05, "*",
                                ifelse(pv < 0.1, "", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]"),
    #esp = 1:10,
    id = 1
  ) %>%
  select(-c(pv))



row1 <- c("Average",
          " ", " ",
          "Language",
          " ", " ",
          "Human S.", 
          " ", " ",
          "Natural S.",
          " ", " ",
          "Math",
          " ", " ")



result <- data.frame(
  var = row1,
  
  ea = rep(NA, times = length(row1)),
  md = rep(NA, times = length(row1)),
  df = rep(NA, times = length(row1))
)

####### TC ---- #
#AVG
result$ea[1] <- mat_nota$coef[[1]]
result$ea[2] <- mat_nota$se[[1]]
result$ea[3] <- mat_nota$N[[1]]

result$md[1] <- mat_nota$coef[[2]]
result$md[2] <- mat_nota$se[[2]]
result$md[3] <- mat_nota$N[[2]]

result$df[1] <- dif_tab$coef[[1]]
result$df[2] <- dif_tab$se[[1]]
result$df[3] <- dif_tab$N[[1]]


#Lang
result$ea[4] <- mat_nota$coef[[3]]
result$ea[5] <- mat_nota$se[[3]]
result$ea[6] <- mat_nota$N[[3]]

result$md[4] <- mat_nota$coef[[4]]
result$md[5] <- mat_nota$se[[4]]
result$md[6] <- mat_nota$N[[4]]

result$df[4] <- dif_tab$coef[[2]]
result$df[5] <- dif_tab$se[[2]]
result$df[6] <- dif_tab$N[[2]]


#Cien Humanas
result$ea[7] <- mat_nota$coef[[5]]
result$ea[8] <- mat_nota$se[[5]]
result$ea[9] <- mat_nota$N[[5]]

result$md[7] <- mat_nota$coef[[6]]
result$md[8] <- mat_nota$se[[6]]
result$md[9] <- mat_nota$N[[6]]

result$df[7] <- dif_tab$coef[[3]]
result$df[8] <- dif_tab$se[[3]]
result$df[9] <- dif_tab$N[[3]]



#Cien Nat
result$ea[10] <- mat_nota$coef[[7]]
result$ea[11] <- mat_nota$se[[7]]
result$ea[12] <- mat_nota$N[[7]]

result$md[10] <- mat_nota$coef[[8]]
result$md[11] <- mat_nota$se[[8]]
result$md[12] <- mat_nota$N[[8]]

result$df[10] <- dif_tab$coef[[4]]
result$df[11] <- dif_tab$se[[4]]
result$df[12] <- dif_tab$N[[4]]



#MT
result$ea[13] <- mat_nota$coef[[9]]
result$ea[14] <- mat_nota$se[[9]]
result$ea[15] <- mat_nota$N[[9]]

result$md[13] <- mat_nota$coef[[10]]
result$md[14] <- mat_nota$se[[10]]
result$md[15] <- mat_nota$N[[10]]

result$df[13] <- dif_tab$coef[[5]]
result$df[14] <- dif_tab$se[[5]]
result$df[15] <- dif_tab$N[[5]]


colnames(result) <- c(" ", "(1)", "(2)", "(3)")


latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccccc",
  linesep = ""
)



writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Dificuldade_v1.tex")

rm(dif_list, dif_tab, mat_nota, result, rp_list, latex_table, d_list, row1)

# ---------------------------------------------------------------------------- #
## 5.3 Mother Schooling -----
# ---------------------------------------------------------------------------- #
### 5.3.1 Regression ----
# ---------------------------------------------------------------------------- #
load(file = "Z:/Tuffy/Paper - HV/Resultados/bandwidths_2019_2018_p2.RData")
load(file = "Z:/Tuffy/Paper - HV/Resultados/bandwidths_2019_2018_NF.RData")


result <- list()

for (i in c(0:1)) {
  
  temp <- base %>% 
    filter(escm == i)
  
  temp_ag <- temp[priv0 == 1,.(
                              mediabl = mean(acerto_pbl, na.rm = T),
                              mediabh = mean(acerto_pbh, na.rm = T),
                              obs = .N),
                 by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)]
  
  #Base adjustments
  temp_ag <- temp_ag %>%
    arrange(mun_prova,ano) %>%
    group_by(mun_prova) %>%
    mutate(
      dup1 = 1,
      dup2 = sum(dup1)) %>%
    ungroup() %>%
    filter(dup2 == 2) %>%
    group_by(mun_prova) %>%
    mutate(
      
      #TOTAL
      
      v1_pbl = ifelse(ano == 2018, mediabl, NA),
      v2_pbl = max(v1_pbl, na.rm = T),
      d.mediabl = mediabl - v2_pbl,
      
      v1_pbh = ifelse(ano == 2018, mediabh, NA),
      v2_pbh = max(v1_pbh, na.rm = T),
      d.mediabh = mediabh - v2_pbh
    ) %>%
    ungroup() %>%
    filter(dup2 == 2) %>%
    select(-c(dup1,dup2,
              v1_pbh, v1_pbl, v2_pbl, v2_pbh)) %>% 
    group_by(mun_prova, ano) %>%
    mutate(
      dif_avg = d.mediabl - d.mediabh
    ) %>% ungroup()
  
  
  
  
  
  for(v in c("d.mediabl", "d.mediabh", "dif_avg")) {
    
    
    
    
    #Com Controles
    
    ef <- dummy_cols(temp_ag$seg[temp_ag$ano == 2018])
    ef <- ef %>% select(-1,-2)
    
    
    result[[as.character(paste0(i,"_",v))]] <-
      rdrobust(
        y = temp_ag[[v]][temp_ag$ano == 2019],
        x = temp_ag$dist_hv_border[temp_ag$ano == 2018],
        c = 0,
        h = bw_main_a,
        b = bw_bias_a,
        cluster = temp_ag$seg[temp_ag$ano == 2018],
        weights = temp_ag$obs[temp_ag$ano == 2018],
        vce = "hc0",
        covs = cbind(
          ef,
          temp_ag$lat[temp_ag$ano == 2018],
          temp_ag$lon[temp_ag$ano == 2018]
        )
      )
    
    
  }
  rm(ef,v, temp, temp_ag)
}

# --------------------------------------------------------------------------- #
### 5.3.2 Table -----
# ---------------------------------------------------------------------------- #


mat_nota <- data.frame(
  coef = do.call(rbind,lapply(result, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(result, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(result, FUN = function(x){x$pv[3]})),
  n = do.call(rbind,lapply(result, FUN = function(x){x$N_h}))
)





mat_nota <- mat_nota %>%
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "**",
                         ifelse(pv < 0.05, "*",
                                ifelse(pv < 0.1, "", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]"),
    #esp = 1:20,
    id = 1
  ) %>%
  select(-c(pv))


# ---------------------------------------------------------------------------- #
### 5.3.3 Latex ----
# ---------------------------------------------------------------------------- #

row3 <- c("Mother without High School",
          " ", " ",
          "Mother with High School",
          " ", " "
          )

final <- data.frame(
  var = row3,
  
  ea = rep(NA, times = length(row3)),
  md = rep(NA, times = length(row3)),
  df = rep(NA, times = length(row3))
)

#AVG
final$ea[1] <- mat_nota$coef[[1]]
final$ea[2] <- mat_nota$se[[1]]
final$ea[3] <- mat_nota$N[[1]]

final$md[1] <- mat_nota$coef[[2]]
final$md[2] <- mat_nota$se[[2]]
final$md[3] <- mat_nota$N[[2]]

final$df[1] <- mat_nota$coef[[3]]
final$df[2] <- mat_nota$se[[3]]
final$df[3] <- mat_nota$N[[3]]


#Lang
final$ea[4] <- mat_nota$coef[[4]]
final$ea[5] <- mat_nota$se[[4]]
final$ea[6] <- mat_nota$N[[4]]

final$md[4] <- mat_nota$coef[[5]]
final$md[5] <- mat_nota$se[[5]]
final$md[6] <- mat_nota$N[[5]]

final$df[4] <- mat_nota$coef[[6]]
final$df[5] <- mat_nota$se[[6]]
final$df[6] <- mat_nota$N[[6]]


colnames(final) <- c(" ", "(1)", "(2)", "(3)")


latex_table <- knitr::kable(
  final,
  format = "latex",
  booktabs = TRUE,
  align = "lccccc",
  linesep = ""
)



writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Dificuldade_mae.tex")

rm( mat_nota, result, final, latex_table, d_list, row3)


# 
# # ---------------------------------------------------------------------------- #
# # 6. Ini vs. Fim ----
# # ---------------------------------------------------------------------------- #
# 
# # Agregados dos critérios
# base_a <- base[priv0 == 1,.(media_ac_ini5_ch = mean(acerto_ini5_ch, na.rm = T),
#                             media_ac_fim5_ch = mean(acerto_fim5_ch, na.rm = T),
#                             media_ac_ini5_cn = mean(acerto_ini5_cn, na.rm = T),
#                             media_ac_fim5_cn = mean(acerto_fim5_cn, na.rm = T),
#                             media_ac_ini5_lc = mean(acerto_ini5_lc, na.rm = T),
#                             media_ac_fim5_lc = mean(acerto_fim5_lc, na.rm = T),
#                             media_ac_ini5_mt = mean(acerto_ini5_mt, na.rm = T),
#                             media_ac_fim5_mt = mean(acerto_fim5_mt, na.rm = T),
#                             media_ini5 = mean(acerto_ini5, na.rm = T),
#                             media_fim5 = mean(acerto_fim5, na.rm = T),
#                             obs = .N),
#                by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] 
# 
# 
# #Calculando as diferenças
# base_a <- base_a %>%
#   arrange(mun_prova,ano) %>%
#   group_by(mun_prova) %>%
#   mutate(
#     dup1 = 1,
#     dup2 = sum(dup1),
#     
#     
#     #TOTAL
#     
#     v1_ini5 = ifelse(ano == 2018, media_ini5, NA),
#     v2_ini5 = max(v1_ini5, na.rm = T),
#     d.media_ini5 = media_ini5 - v2_ini5,
#     
#     v1_fim5 = ifelse(ano == 2018, media_fim5, NA),
#     v2_fim5 = max(v1_fim5, na.rm = T),
#     d.media_fim5 = media_fim5 - v2_fim5,
#     
#     # Ciências Humanas
#     ## Inicio
#     v1_ini5_ch = ifelse(ano == 2018, media_ac_ini5_ch, NA),
#     v2_ini5_ch = max(v1_ini5_ch, na.rm = T),
#     d.media_ini5_ch = media_ac_ini5_ch - v2_ini5_ch,
#     
#     ## Fim
#     v1_fim5_ch = ifelse(ano == 2018, media_ac_fim5_ch, NA),
#     v2_fim5_ch = max(v1_fim5_ch, na.rm = T),
#     d.media_fim5_ch = media_ac_fim5_ch - v2_fim5_ch,
#     
#     
#     # Ciências Naturais
#     ## Inicio
#     v1_ini5_cn = ifelse(ano == 2018, media_ac_ini5_cn, NA),
#     v2_ini5_cn = max(v1_ini5_cn, na.rm = T),
#     d.media_ini5_cn = media_ac_ini5_cn - v2_ini5_cn,
#     
#     ## Fim
#     v1_fim5_cn = ifelse(ano == 2018, media_ac_fim5_cn, NA),
#     v2_fim5_cn = max(v1_fim5_cn, na.rm = T),
#     d.media_fim5_cn = media_ac_fim5_cn - v2_fim5_cn,
#     
#     
#     # Lingua
#     ## Inicio
#     v1_ini5_lc = ifelse(ano == 2018, media_ac_ini5_lc, NA),
#     v2_ini5_lc = max(v1_ini5_lc, na.rm = T),
#     d.media_ini5_lc = media_ac_ini5_lc - v2_ini5_lc,
#     
#     ## Fim
#     v1_fim5_lc = ifelse(ano == 2018, media_ac_fim5_lc, NA),
#     v2_fim5_lc = max(v1_fim5_lc, na.rm = T),
#     d.media_fim5_lc = media_ac_fim5_lc - v2_fim5_lc,
#     
#     
#     # Matematica
#     ## Inicio
#     v1_ini5_mt = ifelse(ano == 2018, media_ac_ini5_mt, NA),
#     v2_ini5_mt = max(v1_ini5_mt, na.rm = T),
#     d.media_ini5_mt = media_ac_ini5_mt - v2_ini5_mt,
#     
#     ## Fim
#     v1_fim5_mt = ifelse(ano == 2018, media_ac_fim5_mt, NA),
#     v2_fim5_mt = max(v1_fim5_mt, na.rm = T),
#     d.media_fim5_mt = media_ac_fim5_mt - v2_fim5_mt
#     
#   ) %>%
#   ungroup() %>%
#   filter(dup2 == 2,
#          mun_prova != 1400159) %>% #Municipio com 15 obs totais (2019 + 2019)
#   #Possui VALORES NA
#   select(-c(dup1,dup2,
#             v1_ini5_ch,v2_ini5_ch, v1_fim5_ch, v2_fim5_ch,
#             v1_ini5_cn,v2_ini5_cn, v1_fim5_cn, v2_fim5_cn,
#             v1_ini5_lc,v2_ini5_lc, v1_fim5_lc, v2_fim5_lc,
#             v1_ini5_mt,v2_ini5_mt, v1_fim5_mt, v2_fim5_mt,
#             v1_ini5, v1_fim5, v2_ini5, v2_fim5))
# 
# 
# ##6.1 Reg ----
# 
# rp_list <- list()
# 
# d_list <- c("d.media_ini5", "d.media_fim5", "d.media_ini5_lc", "d.media_fim5_lc",
#             "d.media_ini5_ch", "d.media_fim5_ch", "d.media_ini5_cn", "d.media_fim5_cn",
#             "d.media_ini5_mt", "d.media_fim5_mt"
# )
# 
# 
# for (i in d_list){
#   
#   
#   #Com Controles
#   
#   ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
#   ef <- ef %>% select(-1,-2)
#   
#   
#   rp_list[[as.character(paste0("cc_",i))]] <-
#     rdrobust(
#       y = base_a[[i]][base_a$ano == 2019],
#       x = base_a$dist_hv_border[base_a$ano == 2018],
#       c = 0,
#       h = bw_main,
#       b = bw_bias,
#       cluster = base_a$seg[base_a$ano == 2018],
#       weights = base_a$obs[base_a$ano == 2018],
#       vce = "hc0",
#       covs = cbind(
#         ef, 
#         base_a$lat[base_a$ano == 2018], 
#         base_a$lon[base_a$ano == 2018]
#       )
#     )
#   
#   
# }
# rm(ef,i)
# 
# 
# 
# 
# ##DIF----
# 
# base_a <- base_a %>% 
#   group_by(mun_prova,
#            ano) %>% 
#   mutate(
#     dif_avg = d.media_ini5 - d.media_fim5,
#     dif_lc = d.media_ini5_lc - d.media_fim5_lc,
#     dif_ch = d.media_ini5_ch - d.media_fim5_ch,
#     dif_cn = d.media_ini5_cn - d.media_fim5_cn,
#     dif_mt = d.media_ini5_mt - d.media_fim5_mt
#   )
# 
# new_list <- c("dif_avg", "dif_lc", "dif_ch", "dif_cn", "dif_mt")
# dif_list <- list()
# 
# for (i in new_list){
#   
#   
#   
#   
#   
#   #Com Controles
#   
#   ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
#   ef <- ef %>% select(-1,-2)
#   
#   
#   dif_list[[as.character(paste0("cc_",i))]] <-
#     rdrobust(
#       y = base_a[[i]][base_a$ano == 2019],
#       x = base_a$dist_hv_border[base_a$ano == 2018],
#       c = 0,
#       h = bw_main,
#       b = bw_bias,
#       cluster = base_a$seg[base_a$ano == 2018],
#       weights = base_a$obs[base_a$ano == 2018],
#       vce = "hc0",
#       covs = cbind(
#         ef, 
#         base_a$lat[base_a$ano == 2018], 
#         base_a$lon[base_a$ano == 2018]
#       )
#     )
#   
#   
# }
# 
# rm(ef,i, new_list)
# 
# 
# 
# 
# ##6.2 Tab ----
# 
# 
# ini_fim_tab <- data.frame(
#   coef = do.call(rbind,lapply(rp_list, FUN = function(x){x$coef[3]})),
#   se = do.call(rbind,lapply(rp_list, FUN = function(x){x$se[3]})),
#   pv = do.call(rbind,lapply(rp_list, FUN = function(x){x$pv[3]})),
#   n = do.call(rbind, lapply(rp_list, FUN = function(x){x$N_h}))
# ) %>% 
#   mutate(
#     N = n.1 + n.2
#   )
# 
# 
# 
# ini_fim_tab <- ini_fim_tab %>% 
#   mutate(
#     coef = paste0(formatC(x = coef, digits = 2, format = "f"),
#                   ifelse(pv < 0.01, "**", 
#                          ifelse(pv < 0.05, "*", 
#                                 ifelse(pv < 0.1, "", "")
#                          ))),
#     se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
#     pv = formatC(x = pv, digits = 3, format = "f"),
#     N = paste0("[N = ", n.1 + n.2, "]"),
#     esp = 1:10,
#     id = 1
#   ) %>%
#   select(-c(pv))
# 
# 
# ## DIF TAB ----
# 
# dif_tab <- data.frame(
#   coef = do.call(rbind,lapply(dif_list, FUN = function(x){x$coef[3]})),
#   se = do.call(rbind,lapply(dif_list, FUN = function(x){x$se[3]})),
#   pv = do.call(rbind,lapply(dif_list, FUN = function(x){x$pv[3]})),
#   n = do.call(rbind,lapply(dif_list, FUN = function(x){x$N_h}))
# ) %>% 
#   mutate(
#     N = n.1 + n.2
#   )
# 
# 
# 
# dif_tab <- dif_tab %>% 
#   mutate(
#     coef = paste0(formatC(x = coef, digits = 2, format = "f"),
#                   ifelse(pv < 0.01, "**", 
#                          ifelse(pv < 0.05, "*", 
#                                 ifelse(pv < 0.1, "", "")
#                          ))),
#     se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
#     pv = formatC(x = pv, digits = 3, format = "f"),
#     N = paste0("[N = ", n.1 + n.2, "]"),
#     esp = 1:5,
#     id = 1
#   ) %>%
#   select(-c(pv))
# 
# 
# 
# row1 <- c("First 5 items",
#           " "," ",
#           "Last 5 items",
#           " ", " ",
#           "Difference",
#           " ", " ")
# 
# 
# 
# 
# result <- data.frame(
#   var = row1,
#   
#   avg = rep(NA, times = length(names)),
#   lc = rep(NA, times = length(names)),
#   ch = rep(NA, times = length(names)),
#   cn = rep(NA, times = length(names)),
#   mt = rep(NA, times = length(names))
# )
# 
# 
# #AVG
# result$avg[1] <- ini_fim_tab$coef[[1]]
# result$avg[2] <- ini_fim_tab$se[[1]]
# result$avg[3] <- ini_fim_tab$N[[1]]
# result$avg[4] <- ini_fim_tab$coef[[2]]
# result$avg[5] <- ini_fim_tab$se[[2]]
# result$avg[6] <- ini_fim_tab$N[[2]]
# 
# result$avg[7] <- dif_tab$coef[[1]]
# result$avg[8] <- dif_tab$se[[1]]
# result$avg[9] <- dif_tab$N[[1]]
# 
# #Lang
# result$lc[1] <- ini_fim_tab$coef[[3]]
# result$lc[2] <- ini_fim_tab$se[[3]]
# result$lc[3] <- ini_fim_tab$N[[3]]
# result$lc[4] <- ini_fim_tab$coef[[4]]
# result$lc[5] <- ini_fim_tab$se[[4]]
# result$lc[6] <- ini_fim_tab$N[[4]]
# 
# result$lc[7] <- dif_tab$coef[[2]]
# result$lc[8] <- dif_tab$se[[2]]
# result$lc[9] <- dif_tab$N[[2]]
# 
# 
# #Cien Humanas
# result$ch[1] <- ini_fim_tab$coef[[5]]
# result$ch[2] <- ini_fim_tab$se[[5]]
# result$ch[3] <- ini_fim_tab$N[[5]]
# result$ch[4] <- ini_fim_tab$coef[[6]]
# result$ch[5] <- ini_fim_tab$se[[6]]
# result$ch[6] <- ini_fim_tab$N[[6]]
# 
# result$ch[7] <- dif_tab$coef[[3]]
# result$ch[8] <- dif_tab$se[[3]]
# result$ch[9] <- dif_tab$N[[3]]
# 
# 
# #Cien Nat
# result$cn[1] <- ini_fim_tab$coef[[7]]
# result$cn[2] <- ini_fim_tab$se[[7]]
# result$cn[3] <- ini_fim_tab$N[[7]]
# result$cn[4] <- ini_fim_tab$coef[[8]]
# result$cn[5] <- ini_fim_tab$se[[8]]
# result$cn[6] <- ini_fim_tab$N[[8]]
# 
# result$cn[7] <- dif_tab$coef[[4]]
# result$cn[8] <- dif_tab$se[[4]]
# result$cn[9] <- dif_tab$N[[4]]
# 
# 
# #MT
# result$mt[1] <- ini_fim_tab$coef[[9]]
# result$mt[2] <- ini_fim_tab$se[[9]]
# result$mt[3] <- ini_fim_tab$N[[9]]
# result$mt[4] <- ini_fim_tab$coef[[10]]
# result$mt[5] <- ini_fim_tab$se[[10]]
# result$mt[6] <- ini_fim_tab$N[[10]]
# 
# result$mt[7] <- dif_tab$coef[[5]]
# result$mt[8] <- dif_tab$se[[5]]
# result$mt[9] <- dif_tab$N[[5]]
# 
# 
# 
# 
# colnames(result) <- c(" ", "(1)", "(2)", "(3)", "(4)", "(5)")
# 
# 
# latex_table <- knitr::kable(
#   result,
#   format = "latex",
#   booktabs = TRUE,
#   align = "lccccc",
#   linesep = ""
# )
# 
# 
# writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/inicio_fim_v1.tex")
# 
# 
# 
# rm(dif_list, dif_tab, ini_fim_tab, result, rp_list, d_list, latex_table, row1)
# 
# 
# 
# # ---------------------------------------------------------------------------- #
# 7. DIAS (comparabilidade) ----
# ---------------------------------------------------------------------------- #

# REFERÊNCIA == 2019
base_a <- base[priv0 == 1,.(media_dia1 = mean(dia_1, na.rm = T),
                            media_dia2 = mean(dia_2, na.rm = T),
                            obs = .N),
               by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] 

base_a <- base_a %>%
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    
    
    #dia1_ 2019 REF
    v1_d1 = ifelse(ano == 2018, media_dia1, NA), 
    v2_d1 = max(v1_d1, na.rm = T),
    d.media_d1 = media_dia1 - v2_d1,
    
    v1_d2 = ifelse(ano == 2018, media_dia2, NA), #Note que inverte
    v2_d2 = max(v1_d2, na.rm = T),
    d.media_d2 = media_dia2 - v2_d2
    
    
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>% 
  select(-c(v1_d1, v2_d1, v2_d2, v1_d2))



p_list <- list()

d_list <- c("d.media_d1", "d.media_d2")

## A. TC ----

for (i in d_list){
  
  #Com Controles
  
  ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  p_list[[as.character(paste0("nc_",i,"|TC"))]] <-
    rdrobust(
      y = base_a[[i]][base_a$ano == 2019],
      x = base_a$dist_hv_border[base_a$ano == 2018],
      c = 0,
      h = bw_main_a,
      b = bw_bias_a,
      p =1,
      cluster = base_a$seg[base_a$ano == 2018],
      weights = base_a$obs[base_a$ano == 2018],
      vce = "hc0",
      covs = cbind(
        ef, 
        base_a$lat[base_a$ano == 2018], 
        base_a$lon[base_a$ano == 2018]
      )
    )
  
  
}
rm(ef, i)





dias <- data.frame(
  coef = do.call(rbind,lapply(p_list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(p_list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(p_list, FUN = function(x){x$pv[3]})),
  n = do.call(rbind, lapply(p_list, FUN = function(x){x$N_h}))
) %>% 
  mutate(
    N = n.1 + n.2
  )


dias <- dias %>% 
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "**", 
                         ifelse(pv < 0.05, "*", 
                                ifelse(pv < 0.1, "", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]"),
    #esp = 1:4,
    id = 1
  ) %>%
  select(-c(pv))





result <- data.frame(
  var = names,
  
  d1 = rep(NA, times = length(names)),
  d2 = rep(NA, times = length(names))
)


#AVG
result$d1[1] <- dias$coef[[1]]
result$d1[2] <- dias$se[[1]]
result$d1[3] <- dias$N[[1]]
result$d2[1] <- dias$coef[[2]]
result$d2[2] <- dias$se[[2]]
result$d2[3] <- dias$N[[2]]








colnames(result) <- c(" ","(1)", "(2)")


latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Dias_v1.tex")


rm(dias, p_list, result, d_list, latex_table)

# ---------------------------------------------------------------------------- #
## 7.1 Gráfico dias (19-18) ------
# ---------------------------------------------------------------------------- #


fig <- list()

temp <- base_a %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main_a ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.media_d1)#,
    #subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(ano == 2019) %>% 
  select(d.media_d1) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(ano == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(ano == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(ano == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(ano == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              #h = bw_main_a,
                            nbins = 35,
              #b = bw_bias_a,
              weights = temp$obs[temp$ano == 2018],
              #subset = temp$subset == 1,
              hide = T,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-160*10^5,160*10^5,4*10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linewidth = 1) + 
  scale_color_brewer(palette = "Set1") + 
  labs(x = "Distance to DST Border (km)",
       y = "Average ENEM \n Score (1st Day)") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  #ylim(-20,50) + 
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/RDD_Dias.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/RDD_Dias.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)





# ---------------------------------------------------------------------------- #
## 7.2 Gráfico dias (18-17) ------
# ---------------------------------------------------------------------------- #
### 7.2.1 Dados ----
# ---------------------------------------------------------------------------- #

#Agregando os dados para 2018 e 2017
base_c <- base_temp[priv0 == 1,.(media_dia1 = mean(dia_1, na.rm = T),
                                 media_dia2 = mean(dia_2, na.rm = T),
                                 obs = .N),
                    by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] %>%
  filter(as.numeric(ano) %in% c(2017,2018)) #Filtrando para anos relevantes




base_c <- base_c %>%
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    
    
    #dia1_ 2019 REF
    v1_d1 = ifelse(ano == 2017, media_dia1, NA), 
    v2_d1 = max(v1_d1, na.rm = T),
    d.media_d1 = media_dia1 - v2_d1,
    
    v1_d2 = ifelse(ano == 2017, media_dia2, NA), #Note que inverte
    v2_d2 = max(v1_d2, na.rm = T),
    d.media_d2 = media_dia2 - v2_d2,
    
    
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>% 
  select(-c(v1_d1, v2_d1, v2_d2, v1_d2))


# ---------------------------------------------------------------------------- #
### 7.2.2 Gráfico ----
# ---------------------------------------------------------------------------- #


fig <- list()

temp <- base_c %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main_a ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.media_d1)#,
    #subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(ano == 2018) %>% 
  select(d.media_d1) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(ano == 2017) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(ano == 2017) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(ano == 2017) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(ano == 2017) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              #h = bw_main_a,
                            nbins = 35,
              #b = bw_bias_a,
              weights = temp$obs[temp$ano == 2017],
              #subset = temp$subset == 1,
              hide = T,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-160*10^5,160*10^5,4*10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linewidth = 1) + 
  scale_color_brewer(palette = "Set1") + 
  labs(x = "Distance to DST Border (km)",
       y = "Average ENEM \n Score (1st Day)") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  #ylim(-30,40) + 
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/RDD_Dias_17.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/RDD_Dias_17.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)




# ---------------------------------------------------------------------------- #
# 8.1 Abstenções ----
# ---------------------------------------------------------------------------- #
gc()

base_abs <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_abs_2019.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_abs_2018.RDS"))) %>%
  setDT()



base_a <- base_abs[priv0 == 1,.(media_abs = mean(abs, na.rm = T),obs = .N),
                   by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] 


#Calculando as diferenças
base_a <- base_a %>%
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1)) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>%
  group_by(mun_prova) %>% 
  mutate(
    #ABS
    v1 = ifelse(ano == 2018, media_abs, NA),
    v2 = max(v1, na.rm = T),
    d.media_abs = media_abs - v2
  ) %>%
  ungroup() %>%
  select(-c(dup1,dup2,v1,v2))




##8.1 Reg ----


rabs_list <- list()

d_list <- c("d.media_abs")

### A. TC----

for (i in d_list){
  
  
  #Sem Controles
  rabs_list[[as.character(paste0("nc_",i,"|TC"))]] <-
    rdrobust(
      y = base_a[[i]][base_a$ano == 2019],
      x = base_a$dist_hv_border[base_a$ano == 2018],
      c = 0,
      p =1,
      h = bw_main_a,
      b = bw_bias_a,
      cluster = base_a$seg[base_a$ano == 2018],
      weights = base_a$obs[base_a$ano == 2018],
      vce = "hc0"
    )
  
  
  
  
  
  #Com Controles
  ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  rabs_list[[as.character(paste0("cc_",i,"|TC"))]] <-
    rdrobust(
      y = base_a[[i]][base_a$ano == 2019],
      x = base_a$dist_hv_border[base_a$ano == 2018],
      c = 0,
      p =1,
      h = bw_main_a,
      b = bw_bias_a,
      cluster = base_a$seg[base_a$ano == 2018],
      weights = base_a$obs[base_a$ano == 2018],
      vce = "hc0",
      covs = cbind(
        ef, 
        base_a$lat[base_a$ano == 2018], 
        base_a$lon[base_a$ano == 2018]
      )
    )
  
  
  
}


rm(ef)




##8.2 Tab ----


abs_tab <- data.frame(
  coef = do.call(rbind,lapply(rabs_list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(rabs_list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(rabs_list, FUN = function(x){x$pv[3]})),
  n = do.call(rbind, lapply(rabs_list, FUN = function(x){x$N_h}))
)


abs_tab <- abs_tab %>% 
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "**", 
                         ifelse(pv < 0.05, "*", 
                                ifelse(pv < 0.1, "", "")
                         ))),
    se =paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]"),
    #esp = 1:4,
    id = 1
  ) %>% 
  select(-c(pv))


result <- data.frame(
  var = names,
  control = rep(NA, times = length(names))
)


# BASE A

result$control[1] <- abs_tab$coef[[2]]
result$control[2] <- abs_tab$se[[2]]
result$control[3] <- abs_tab$N[[2]]





colnames(result) <- c("", "(1)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lc",
  linesep = ""
)

resultabs <- result

writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Abs_v1.tex")

rm(abs_tab, latex_table,i, result, d_list, rabs_list)

# ---------------------------------------------------------------------------- #
## 8.3 Por dia ----
# ---------------------------------------------------------------------------- #

base_abs <- base_abs %>% 
  mutate(abs_1d = ifelse(
    abs_rd == 1 &
      abs_ch == 1 &
      abs_lc == 1 ,
    1,
    0),
    
    abs_2d = ifelse(
          abs_cn == 1 &
          abs_mt == 1,
        1,
        0)
    )




base_a <- base_abs[priv0 == 1,.(media_dia1 = mean(abs_1d, na.rm = T),
                            media_dia2 = mean(abs_2d, na.rm = T),
                            media_abs = mean(abs, na.rm = T),
                            obs = .N),
               by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] 

base_a <- base_a %>%
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    
    
    #dia1_ 2019 REF
    v1_d1 = ifelse(ano == 2018, media_dia1, NA), 
    v2_d1 = max(v1_d1, na.rm = T),
    d.media_d1 = media_dia1 - v2_d1,
    
    v1_d2 = ifelse(ano == 2018, media_dia2, NA), #Note que inverte
    v2_d2 = max(v1_d2, na.rm = T),
    d.media_d2 = media_dia2 - v2_d2,
    
    #ABS
    v1 = ifelse(ano == 2018, media_abs, NA),
    v2 = max(v1, na.rm = T),
    d.media_abs = media_abs - v2
    
    
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>% 
  select(-c(v1_d1, v2_d1, v2_d2, v1_d2))



p_list <- list()

d_list <- c("d.media_abs","d.media_d1", "d.media_d2")

### A. TC ----

for (i in d_list){
  
  #Com Controles
  
  ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  p_list[[as.character(paste0("nc_",i,"|TC"))]] <-
    rdrobust(
      y = base_a[[i]][base_a$ano == 2019],
      x = base_a$dist_hv_border[base_a$ano == 2018],
      c = 0,
      p =1,
      h = bw_main_a,
      b = bw_bias_a,
      cluster = base_a$seg[base_a$ano == 2018],
      weights = base_a$obs[base_a$ano == 2018],
      vce = "hc0",
      covs = cbind(
        ef, 
        base_a$lat[base_a$ano == 2018], 
        base_a$lon[base_a$ano == 2018]
      )
    )
  
  
}
rm(ef, i)





dias <- data.frame(
  coef = do.call(rbind,lapply(p_list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(p_list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(p_list, FUN = function(x){x$pv[3]})),
  n = do.call(rbind, lapply(p_list, FUN = function(x){x$N_h}))
) %>% 
  mutate(
    N = n.1 + n.2
  )


dias <- dias %>% 
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "**", 
                         ifelse(pv < 0.05, "*", 
                                ifelse(pv < 0.1, "", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]"),
    #esp = 1:4,
    id = 1
  ) %>%
  select(-c(pv))





result <- data.frame(
  var = names,
  both = rep(NA, times = length(names)),
  d1 = rep(NA, times = length(names)),
  d2 = rep(NA, times = length(names))
)


#AVG

result$both[1] <- dias$coef[[1]]
result$both[2] <- dias$se[[1]]
result$both[3] <- dias$N[[1]]


result$d1[1] <- dias$coef[[2]]
result$d1[2] <- dias$se[[2]]
result$d1[3] <- dias$N[[2]]
result$d2[1] <- dias$coef[[3]]
result$d2[2] <- dias$se[[3]]
result$d2[3] <- dias$N[[3]]








colnames(result) <- c(" ","(1)", "(2)", "(3)")


latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Abs_Dias_v1.tex")
rm(base_abs, resultabs)
