# ---------------------------------------------------------------------------- #
# Regressions
# Main estimations and Robustness
# Last edited by: Tuffy Licciardi Issa
# Date: 09/03/2026
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
      esc_pai %in% c("D","E","F") ~ 1, #With High school
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
base_a <- base[priv0 == 1,.(media    = mean(media, na.rm = T),
                            idade    = mean(id18, na.rm = T),
                            temp_d1  = mean(temp_d1, na.rm = T),
                            renda    = mean(renda1, na.rm = T),
                            escm     = mean(escm, na.rm = T),
                            mae_trab = mean(mae_trab_man, na.rm = T),
                            fem      = mean(fem1, na.rm = T),
                            dom5     = mean(dom5, na.rm = T),
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
    
    # v1idade = ifelse(ano == 2018, idade, NA),
    # v2idade = max(v1idade, na.rm = T),
    # didade  = idade - v2idade,
    
    v1temp = ifelse(ano == 2018, temp_d1, NA),
    v2temp = max(v1temp, na.rm = T),
    dtemp  = temp_d1 - v2temp,
    
    v1rend = ifelse(ano == 2018, renda, NA),
    v2rend = max(v1rend, na.rm = T),
    drenda = renda - v2rend,
    
    v1escm = ifelse(ano == 2018, escm, NA),
    v2escm = max(v1escm, na.rm = T),
    descm  = escm - v2escm,
    
    v1maet = ifelse(ano == 2018, mae_trab, NA),
    v2maet = max(v1maet, na.rm = T),
    dmaet  = mae_trab - v2temp,
    
    v1fem  = ifelse(ano == 2018, fem, NA),
    v2fem  = max(v1fem, na.rm = T),
    dfem   = fem - v2fem,
    
    v1dom5 = ifelse(ano == 2018, dom5, NA),
    v2dom5 = max(v1dom5, na.rm = T),
    ddom5  = dom5 - v2dom5
    
    
    
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_nota, v2_nota, v1fem, v2fem, v1dom5, v2dom5,
            #v1idade, v2idade,
            v1temp, v2temp, v1rend, v2rend, v1escm, v2escm, v1maet, v2maet)) %>% 
  group_by(mun_prova) %>% 
  mutate(
    d.h13 = h13[ano == 2019] + h13[ano == 2018], #2 = Manteve, 1 mudou, 0 nunca
    d.h12 = h12[ano == 2019] + h12[ano == 2018],
    d.h11 = h11[ano == 2019] + h11[ano == 2018],
    d.h10 = h10[ano == 2019] + h10[ano == 2018]
  )

### 1.1.0 Controles ----
ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
ef <- ef %>% select(-1,-2)

list <- list()

### 1.1.1 Main ----
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
    #Controls
    base_a$dtemp[base_a$ano == 2019],
    base_a$drenda[base_a$ano == 2019],
    base_a$descm[base_a$ano == 2019],
    base_a$dmaet[base_a$ano == 2019],
    base_a$dfem[base_a$ano == 2019],
    base_a$ddom5[base_a$ano == 2019],
    #Timezones
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

#Saving Pol2 bandwidth
# ----------------------------------------- #
bw_main_p  <- list[["2019-2018|pol"]]$bws[1]
bw_bias_p  <- list[["2019-2018|pol"]]$bws[2]

# ----------------------------------------- #

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
    #Controls
    base_a$dtemp[base_a$ano == 2019],
    base_a$drenda[base_a$ano == 2019],
    base_a$descm[base_a$ano == 2019],
    base_a$dmaet[base_a$ano == 2019],
    base_a$dfem[base_a$ano == 2019],
    base_a$ddom5[base_a$ano == 2019],
    #Timezones
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
  b09 = rep(NA, times = length(names2))
  # ,
  # bw7 = rep(NA, times = length(names2)),
  # bw9 = rep(NA, times = length(names2))
)



# ---------------- #
#Building result table
# ---------------- #

# #BW Res.
# result$bw7[1] <- t10$coef[[5]] #18-17
# result$bw7[2] <- t10$se[[5]]
# result$bw7[3] <- t10$N[[5]]
# 
# result$bw9[1] <- t10$coef[[1]] #19-18
# result$bw9[2] <- t10$se[[1]]
# result$bw9[3] <- t10$N[[1]]
# 
# #BW Esc.
# result$bw7[4] <- t10$coef[[7]] #18-17
# result$bw7[5] <- t10$se[[7]]
# result$bw7[6] <- t10$N[[7]]
# 
# result$bw9[4] <- t10$coef[[3]] #19-18
# result$bw9[5] <- t10$se[[3]]
# result$bw9[6] <- t10$N[[3]]

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

colnames(result) <- c("", "(1)", "(2)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/DIFF_dists_res_esc.tex")
rm(ef, rlist, result, t10, latex_table, base_res, base_esc, names2)

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
# 2. GRAPHS / DATA EXPORT (Section 2) ----
# ---------------------------------------------------------------------------- #
# Purpose of this section:
# - Build municipality-level aggregated datasets for plotting
# - Compute first-differences (2019 - 2018) for several outcomes
# - Build sub-samples (with/without high-school, by timezone)
# - Prepare and export a .dta file for Stata/plotting
# ---------------------------------------------------------------------------- #
# Required packages:
# data.table, dplyr, haven, (fastDummies if used elsewhere)
# Put these at the top of master.R: library(data.table); library(dplyr); library(haven)
# ---------------------------------------------------------------------------- #
## 2.1 Stata Export ----
# ---------------------------------------------------------------------------- #
### 2.1.1 Create timezone indicator (time13) ----
# ---------------------------------------------------------------------------- #
# Objective: classify municipalities as UTC-3 (time13 == 1) vs UTC-4 (time13 == 0)
# Notes:
# - Classification is based on 'uf' (state) with explicit exceptions for certain AM municipalities.
# - Keep the exception list documented so the reviewer can reproduce the classification.
base <- base %>%
  group_by(mun_prova) %>%
  mutate(
    time13 = case_when(
      # States that follow UTC-3 (Brasília time)
      uf %in% c("GO", "DF", "MG", "ES", "RJ", "SP", "SC", "PR", "RS", "BA", "SE",
                "AL", "PE", "PB", "RN", "CE", "PI", "MA", "TO", "PA", "AP") ~ 1,
      
      # States that are mostly UTC-4 (with AM exceptions listed below)
      uf %in% c("MT", "MS", "RO", "RR") | (uf == "AM" &
                                             !mun_prova %in% c(
                                               1300201, 1300607, 1300706, 1301407, 1301506, 1301654, 1301803, 1301951,
                                               1302306, 1302405, 1303502, 1303908, 1304062
                                             )) ~ 0,
      
      # Otherwise: set NA (investigate if many NAs appear)
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------------------------- #
### 2.1.2 Aggregate municipality-level data (df_cmo) — all private schools ----
# ---------------------------------------------------------------------------- #
# Purpose:
# - Aggregate individual data to municipality-year level
# - Keep only 2018 and 2019
# - Keep municipalities observed in both years
# - Compute first-differences using 2018 as baseline (value_2019 - value_2018)
#
# Variables aggregated:
# - media: overall average score
# - media_dia1 / media_dia2: average score by test day
# - media_rd, media_cn, media_lc, media_ch, media_mt: subject-specific averages
# - mediabl / mediabh: average correct rates for "easy" / "hard" questions
# - obs: number of observations (municipal sample size)
df_cmo <- base %>%
  group_by(mun_prova, ano, dist_hv_border) %>% 
  summarise(
    media       = mean(media, na.rm = TRUE),
    media_dia1  = mean(dia_1, na.rm = TRUE),
    media_dia2  = mean(dia_2, na.rm = TRUE),
    media_rd    = mean(rd, na.rm = TRUE),
    media_cn    = mean(cn, na.rm = TRUE),
    media_lc    = mean(lc, na.rm = TRUE),
    media_ch    = mean(ch, na.rm = TRUE),
    media_mt    = mean(mt, na.rm = TRUE),
    mediabl     = mean(acerto_pbl, na.rm = TRUE),
    mediabh     = mean(acerto_pbh, na.rm = TRUE),
    obs         = n(),
    .groups     = "drop" #ungroup
  ) %>% 
  # Keep only 2018 and 2019
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_prova, ano) %>%
  group_by(mun_prova) %>%
  mutate(
    # Ensure municipalities are present in both years:
    dup1 = 1,
    dup2 = sum(dup1),
    
    # Use 2018 as baseline. For each outcome we compute:
    # dXXX = value_2019 - value_2018
    v1_nota = ifelse(ano == 2018, media, NA),
    v2_nota = max(v1_nota, na.rm = TRUE),
    dmedia  = media - v2_nota,
    
    v1_rd = ifelse(ano == 2018, media_rd, NA),
    v2_rd = max(v1_rd, na.rm = TRUE),
    dmedia_rd = media_rd - v2_rd,
    
    v1_cn = ifelse(ano == 2018, media_cn, NA),
    v2_cn = max(v1_cn, na.rm = TRUE),
    dmedia_cn = media_cn - v2_cn,
    
    v1_ch = ifelse(ano == 2018, media_ch, NA),
    v2_ch = max(v1_ch, na.rm = TRUE),
    dmedia_ch = media_ch - v2_ch,
    
    v1_lc = ifelse(ano == 2018, media_lc, NA),
    v2_lc = max(v1_lc, na.rm = TRUE),
    dmedia_lc = media_lc - v2_lc,
    
    v1_mt = ifelse(ano == 2018, media_mt, NA),
    v2_mt = max(v1_mt, na.rm = TRUE),
    dmedia_mt = media_mt - v2_mt,
    
    v1_d1 = ifelse(ano == 2018, media_dia1, NA),
    v2_d1 = max(v1_d1, na.rm = TRUE),
    dmedia_d1 = media_dia1 - v2_d1,
    
    v1_d2 = ifelse(ano == 2018, media_dia2, NA),
    v2_d2 = max(v1_d2, na.rm = TRUE),
    dmedia_d2 = media_dia2 - v2_d2,
    
    v1_pbl = ifelse(ano == 2018, mediabl, NA),
    v2_pbl = max(v1_pbl, na.rm = TRUE),
    d.mediabl = mediabl - v2_pbl,
    
    v1_pbh = ifelse(ano == 2018, mediabh, NA),
    v2_pbh = max(v1_pbh, na.rm = TRUE),
    d.mediabh = mediabh - v2_pbh
  ) %>%
  ungroup() %>%
  # Keep only municipalities present in both years
  filter(dup2 == 2) %>%
  group_by(mun_prova) %>%
  mutate(
    # obs_r: reference weight = number of observations in 2018
    obs_r = obs[ano == 2018]
  ) %>%
  ungroup() %>%
  # Remove temporary helper variables used exclusively to compute diffs
  select(-c(dup2, dup1, v1_nota, v2_nota,
            v1_rd, v2_rd, v1_cn, v2_cn, v1_ch, v2_ch,
            v1_lc, v2_lc, v1_mt, v2_mt,
            v1_d1, v2_d1, v2_d2, v1_d2,
            v1_pbl, v2_pbl, v1_pbh, v2_pbh))

# ---------------------------------------------------------------------------- #
### 2.1.3 Subsamples by mother's education (with/without high school) ----
# ---------------------------------------------------------------------------- #
# These subsets are used to compare municipalities where the 'mother' has HS (escm == 1)
# vs those where the mother does not (escm == 0). Procedure is identical:
# aggregate -> keep 2018 & 2019 -> keep municipalities with both years -> compute diff.
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
    v2_nota = max(v1_nota, na.rm = TRUE),
    dmedia_escm1 = media - v2_nota
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup2, dup1, v1_nota, v2_nota))

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
    v2_nota = max(v1_nota, na.rm = TRUE),
    dmedia_escm0 = media - v2_nota
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup2, dup1, v1_nota, v2_nota))

# ---------------------------------------------------------------------------- #
## 2.4 Subsamples by timezone (UTC-3 vs UTC-4)
# ---------------------------------------------------------------------------- #
# Build df_time1 (UTC-3) and df_time0 (UTC-4) — same aggregation/diff logic.
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
    v2_nota = max(v1_nota, na.rm = TRUE),
    dmedia_bra1 = media - v2_nota   # difference for UTC-3 (Brasília)
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup2, dup1, v1_nota, v2_nota))

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
    v2_nota = max(v1_nota, na.rm = TRUE),
    dmedia_bra0 = media - v2_nota   # difference for UTC-4
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup2, dup1, v1_nota, v2_nota))

# ---------------------------------------------------------------------------- #
### 2.1.5 Merge targeted subsample diffs into the main df_cmo for export ----
# ---------------------------------------------------------------------------- #
# Keep df_cmo filtered to 2019 and left-join the subsample diffs created above.
df_cmo <- df_cmo %>%
  filter(ano == 2019) %>%
  left_join(df_escm0 %>% filter(ano == 2019) %>% select(mun_prova, dmedia_escm0),
            by = "mun_prova") %>%
  left_join(df_escm1 %>% filter(ano == 2019) %>% select(mun_prova, dmedia_escm1),
            by = "mun_prova") %>%
  left_join(df_time1 %>% filter(ano == 2019) %>% select(mun_prova, dmedia_bra1),
            by = "mun_prova") %>%
  left_join(df_time0 %>% filter(ano == 2019) %>% select(mun_prova, dmedia_bra0),
            by = "mun_prova")

# Remove intermediate objects to save memory
rm(df_escm0, df_escm1, df_time0, df_time1)

# ---------------------------------------------------------------------------- #
### 2.1.6 Build alternative control base (placebo) using 2017-2018 — base_c ----
# ---------------------------------------------------------------------------- #
# Purpose: use 2017-2018 as placebo / pre-trend check.
base_c <- base %>%
  filter(priv0 == 1) %>%
  group_by(mun_prova, ano, dist_hv_border, seg, lat, lon) %>%
  summarise(media = mean(media, na.rm = TRUE),
            obs   = n(),
            .groups = "drop") %>%
  filter(as.numeric(ano) %in% c(2017, 2018)) %>%
  arrange(mun_prova, ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2017, media, NA),
    v2_nota = max(v1_nota, na.rm = TRUE),
    d.media = media - v2_nota
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup2, dup1, v1_nota, v2_nota)) %>%
  mutate(trat = ifelse(dist_hv_border > 0, 1, 0))

# Attach the 2018-2017 difference to df_cmo for plotting/placebo tables
df_cmo <- df_cmo %>%
  left_join(
    base_c %>% filter(ano == 2018) %>% select(mun_prova, d.media) %>% rename(dmedia_2018 = d.media),
    by = "mun_prova"
  )

# ---------------------------------------------------------------------------- #
### 2.1.7 Final adjustments and labeling (ready for export) ----
# ---------------------------------------------------------------------------- #
# - Rename some variables for clarity in Stata
# - Create dist_km for plotting in kilometers
# - Drop intermediate columns not needed in final .dta
df_cmo <- df_cmo %>%
  rename(dmedia_easy = d.mediabl,
         dmedia_hard = d.mediabh) %>%
  mutate(dist_km = dist_hv_border / 1000) %>%
  select(-c(dist_hv_border, obs, obs_r,
            media, media_dia1, media_dia2, media_rd, media_cn, media_lc, media_ch, media_mt,
            mediabl, mediabh, ano))

# Add variable labels (useful when opening .dta in Stata)
attr(df_cmo$dmedia, "label")       <- "Score 2019-2018"
attr(df_cmo$dmedia_rd, "label")    <- "Essay score 2019-2018"
attr(df_cmo$dmedia_cn, "label")    <- "Natural sciences 2019-2018"
attr(df_cmo$dmedia_ch, "label")    <- "Humanities 2019-2018"
attr(df_cmo$dmedia_lc, "label")    <- "Language 2019-2018"
attr(df_cmo$dmedia_mt, "label")    <- "Math 2019-2018"
attr(df_cmo$dmedia_d1, "label")    <- "Day1 2019-2018"
attr(df_cmo$dmedia_d2, "label")    <- "Day2 2019-2018"
attr(df_cmo$dmedia_easy, "label")  <- "Easy Qs 2019-2018"
attr(df_cmo$dmedia_hard, "label")  <- "Hard Qs 2019-2018"
attr(df_cmo$dmedia_escm0, "label") <- "Mother WITHOUT HS 2019-2018"
attr(df_cmo$dmedia_escm1, "label") <- "Mother WITH HS 2019-2018"
attr(df_cmo$dmedia_bra1, "label")  <- "UTC-3 (Brasilia) 2019-2018"
attr(df_cmo$dmedia_bra0, "label")  <- "UTC-4 2019-2018"
attr(df_cmo$dmedia_2018, "label")  <- "Score 2018-2017 (placebo)"
attr(df_cmo$dist_km, "label")      <- "Distance in km"

# ---------------------------------------------------------------------------- #
### 2.1.8 Export to Stata (.dta) ----
# ---------------------------------------------------------------------------- #
library(haven)

write_dta(df_cmo, "C:/Users/tuffyli/OneDrive - Insper/Dados_HV/dados_cmogram.dta")

# Clean memory
rm(df_cmo)

# ---------------------------------------------------------------------------- #
## 2.2 Within Bandwodth Graph ----
# ---------------------------------------------------------------------------- #
### A. 1918 ----
# ---------------------------------------------------------------------------- #


base_a <- base[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
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




#### 2.3.2 POL 2 ------
plist <- list()

bins <- c(25)

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
                                             p = 2,
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
  
  
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/",i,"_pol2_1918_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 9, width = 13)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/pdf/",i,"_pol2_1918_v1.pdf"),plot = fig_gg, device = "pdf",height = 9, width = 13)
  
  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips, vars_poly_left, vars_poly_right)
  
}

rm(bins, j, plist, fig_loop)
# ---------------------------------------------------------------------------- #


### C. 1817 ----



##### 2.1.3.1 opt bw -----
base_c <- base %>% 
  filter(priv0 == 1) %>%
  group_by(mun_prova,ano,dist_hv_border,seg,lat,lon) %>%
  summarise(media = mean(media, na.rm = TRUE),
            obs = n(),
            .groups = "drop") %>%
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
#### 2.3.2 POL 2 ------
plist <- list()

bins <- c(25)

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
  
  
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/",i,"_pol2_1817_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 9, width = 13)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/pdf/",i,"_pol2_1817_v1.pdf"),plot = fig_gg, device = "pdf",height = 9, width = 13)
  
  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips, vars_poly_left, vars_poly_right)
  
}

rm(bins, j, plist, fig_loop, base_c, bw_main_c, bw_bias_c)


#Final adjustment for the remaining
base <- base %>% filter(ano != 2017)

# ---------------------------------------------------------------------------- #
# 3. Matérias----
# ---------------------------------------------------------------------------- #
#Bases


# ---------------- #
# A
# ---------------- #
base_a <- base %>% 
  filter(priv0 == 1) %>%
  group_by(mun_prova,ano,dist_hv_border,seg,lat,lon) %>%
  summarise(media_rd = mean(rd, na.rm = T),
            media_cn = mean(cn, na.rm = T),
            media_lc = mean(lc, na.rm = T),
            media_ch = mean(ch, na.rm = T),
            media_mt = mean(mt, na.rm = T),
            obs = n(),
            .groups = "drop")

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

rm(result, latex_table, notas_tab, names, d_list, p_list)

# ---------------------------------------------------------------------------- #
# 4. Redação ----
# ---------------------------------------------------------------------------- #

# Agregados dos critérios
base_a <- base %>% 
  filter(priv0 == 1) %>%
  group_by(mun_prova,ano,dist_hv_border,seg,lat,lon) %>%
  summarise(media_rd1 = mean(rd1, na.rm = T),
            media_rd2 = mean(rd2, na.rm = T),
            media_rd3 = mean(rd3, na.rm = T),
            media_rd4 = mean(rd4, na.rm = T),
            media_rd5 = mean(rd5, na.rm = T),
            obs = n(),
            .groups = "drop")


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

base_a <- base %>% 
  filter(priv0 == 1) %>%
  group_by(mun_prova,ano,dist_hv_border,seg,lat,lon) %>%
  summarise(media_ac_bl_ch = mean(acerto_bl_ch, na.rm = T),
            media_ac_bh_ch = mean(acerto_bh_ch, na.rm = T),
            media_ac_bl_cn = mean(acerto_bl_cn, na.rm = T),
            media_ac_bh_cn = mean(acerto_bh_cn, na.rm = T),
            media_ac_bl_lc = mean(acerto_bl_lc, na.rm = T),
            media_ac_bh_lc = mean(acerto_bh_lc, na.rm = T),
            media_ac_bl_mt = mean(acerto_bl_mt, na.rm = T),
            media_ac_bh_mt = mean(acerto_bh_mt, na.rm = T),
            mediabl = mean(acerto_pbl, na.rm = T),
            mediabh = mean(acerto_pbh, na.rm = T),
            obs = n(),
            .groups = "drop")


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
  group_by(mun_prova, ano) %>%
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
          " ", " "
          # ,
          # "Language",
          # " ", " ",
          # "Human S.", 
          # " ", " ",
          # "Natural S.",
          # " ", " ",
          # "Math",
          # " ", " "
          )



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

# 
# #Lang
# result$ea[4] <- mat_nota$coef[[3]]
# result$ea[5] <- mat_nota$se[[3]]
# result$ea[6] <- mat_nota$N[[3]]
# 
# result$md[4] <- mat_nota$coef[[4]]
# result$md[5] <- mat_nota$se[[4]]
# result$md[6] <- mat_nota$N[[4]]
# 
# result$df[4] <- dif_tab$coef[[2]]
# result$df[5] <- dif_tab$se[[2]]
# result$df[6] <- dif_tab$N[[2]]
# 
# 
# #Cien Humanas
# result$ea[7] <- mat_nota$coef[[5]]
# result$ea[8] <- mat_nota$se[[5]]
# result$ea[9] <- mat_nota$N[[5]]
# 
# result$md[7] <- mat_nota$coef[[6]]
# result$md[8] <- mat_nota$se[[6]]
# result$md[9] <- mat_nota$N[[6]]
# 
# result$df[7] <- dif_tab$coef[[3]]
# result$df[8] <- dif_tab$se[[3]]
# result$df[9] <- dif_tab$N[[3]]
# 
# 
# 
# #Cien Nat
# result$ea[10] <- mat_nota$coef[[7]]
# result$ea[11] <- mat_nota$se[[7]]
# result$ea[12] <- mat_nota$N[[7]]
# 
# result$md[10] <- mat_nota$coef[[8]]
# result$md[11] <- mat_nota$se[[8]]
# result$md[12] <- mat_nota$N[[8]]
# 
# result$df[10] <- dif_tab$coef[[4]]
# result$df[11] <- dif_tab$se[[4]]
# result$df[12] <- dif_tab$N[[4]]
# 
# 
# 
# #MT
# result$ea[13] <- mat_nota$coef[[9]]
# result$ea[14] <- mat_nota$se[[9]]
# result$ea[15] <- mat_nota$N[[9]]
# 
# result$md[13] <- mat_nota$coef[[10]]
# result$md[14] <- mat_nota$se[[10]]
# result$md[15] <- mat_nota$N[[10]]
# 
# result$df[13] <- dif_tab$coef[[5]]
# result$df[14] <- dif_tab$se[[5]]
# result$df[15] <- dif_tab$N[[5]]


colnames(result) <- c(" ", "(1)", "(2)", "(3)")


latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccc",
  linesep = ""
)



writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Dificuldade_v1.tex")

rm(dif_list, dif_tab, mat_nota, result, rp_list, latex_table, d_list, row1)

# ---------------------------------------------------------------------------- #
## 5.3 Mother Schooling -----
# ---------------------------------------------------------------------------- #
### 5.3.1 Regression ----
# ---------------------------------------------------------------------------- #


result <- list()

for (i in c(0:1)) {
  
  temp <- base %>% 
    filter(escm == i)

  temp_ag <- temp %>% 
    filter(priv0 == 1) %>%
    group_by(mun_prova,ano,dist_hv_border,seg,lat,lon) %>%
    summarise(mediabl = mean(acerto_pbl, na.rm = T),
              mediabh = mean(acerto_pbh, na.rm = T),
              obs = n(),
              .groups = "drop")
  
  
  
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
    
    message("Finished ", v," for mother education of type = ", i)
    
  }
  rm(ef,v, temp, temp_ag, i)
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

#Without HS
final$ea[1] <- mat_nota$coef[[1]]
final$ea[2] <- mat_nota$se[[1]]
final$ea[3] <- mat_nota$N[[1]]

final$md[1] <- mat_nota$coef[[2]]
final$md[2] <- mat_nota$se[[2]]
final$md[3] <- mat_nota$N[[2]]

final$df[1] <- mat_nota$coef[[3]]
final$df[2] <- mat_nota$se[[3]]
final$df[3] <- mat_nota$N[[3]]


#With HS
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


# ---------------------------------------------------------------------------- #
# 6. DIAS (comparabilidade) ----
# ---------------------------------------------------------------------------- #

base_a <- base %>% 
  filter(priv0 == 1) %>%
  group_by(mun_prova,ano,dist_hv_border,seg,lat,lon) %>%
  summarise(media_dia1 = mean(dia_1, na.rm = T),
            media_dia2 = mean(dia_2, na.rm = T),
            obs = n(),
            .groups = "drop")


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

# ---------------------------------------------------------------------------- #
## 6.1 Regression ----
# ---------------------------------------------------------------------------- #

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
  
  message("Finished for day: ", i)
  
}
rm(ef, i)

# ---------------------------------------------------------------------------- #
## 6.2 Result Table ----
# ---------------------------------------------------------------------------- #



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


names <- c("2019 - 2018",
           " "," ")

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


rm(dias, p_list, result, d_list, latex_table, names)



# ---------------------------------------------------------------------------- #
# 7. Abstenções ----
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


# ---------------------------------------------------------------------------- #
## 7.1 Reg per day ----
# ---------------------------------------------------------------------------- #

base_abs <- base_abs %>% 
  mutate(abs_1d = ifelse(
    abs_rd == 1 & abs_ch == 1 & abs_lc == 1 , 1, 0),
    
    abs_2d = ifelse(
      abs_cn == 1 & abs_mt == 1, 1, 0)
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


# ---------------------------------------------------------------------------- #
## 7.2 Result Table ----
# ---------------------------------------------------------------------------- #


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


names <- c("2019 - 2018",
           " "," ")


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
rm(base_abs, resultabs, latex_table, result, dias, d_list, p_list, names)

# ---------------------------------------------------------------------------- #
# 8. Heterogeneity ---- 
# ---------------------------------------------------------------------------- #
## 8.1 Race ----
# ---------------------------------------------------------------------------- #
### 8.1.1 White and Yellow -----
# ---------------------------------------------------------------------------- #
base_ab <- base %>% 
  filter(
    raca %in% c("B", "E")
  ) %>% setDT()

base_ab <- base_ab[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
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
### 8.1.2 PPI ----
# ---------------------------------------------------------------------------- #
base_ppi <- base %>% 
  filter(
    raca %in% c("C", "D", "F")
  ) %>% setDT()


base_ppi <- base_ppi[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
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
### 8.1.3 Regression by race ----
# ---------------------------------------------------------------------------- #

rlist <- list()


ano_list <- c(2018)

for(ano_ref in ano_list) {
  
  ano_comp <- ano_ref + 1
  
  
  for(df in c("base_ab", "base_ppi")) {
    
    base_a <- get(df)
    
    #Com controles
    ef <- dummy_cols(base_a$seg[base_a$ano == ano_ref])
    ef <- ef %>% select(-1,-2)
    
    rlist[[as.character(paste0(df,"_",ano_comp,"-",ano_ref,"C|TC"))]] <- rdrobust(
      y = base_a$d.media[base_a$ano == ano_comp],
      x = base_a$dist_hv_border[base_a$ano == ano_ref],
      c = 0,
      p =1,
      cluster = base_a$seg[base_a$ano == ano_ref],
      weights = base_a$obs[base_a$ano == ano_ref],
      vce = "hc0",
      covs = cbind(
        ef,
        base_a$lat[base_a$ano == ano_ref],
        base_a$lon[base_a$ano == ano_ref]
      )
    )
    
    #Banda Fixa
    rlist[[as.character(paste0(df,"_",ano_comp,"-",ano_ref,"BW|TC"))]] <- rdrobust(
      y = base_a$d.media[base_a$ano == ano_comp],
      x = base_a$dist_hv_border[base_a$ano == ano_ref],
      c = 0,
      p =1,
      h = bw_main_a,
      b = bw_bias_a,
      cluster = base_a$seg[base_a$ano == ano_ref],
      weights = base_a$obs[base_a$ano == ano_ref],
      vce = "hc0",
      covs = cbind(
        ef,
        base_a$lat[base_a$ano == ano_ref],
        base_a$lon[base_a$ano == ano_ref]
      )
    )
    
    message("Finished for race group: ",df)
  }
  rm(df)
}
rm(ef, ano_ref, ano_comp)


# ---------------------------------------------------------------------------- #
###8.1.4 Result Table ----
# ---------------------------------------------------------------------------- #
t10cc <- data.frame(
  coef = do.call(rbind,lapply(rlist, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(rlist, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(rlist, FUN = function(x){x$pv[3]})),
  n = do.call(rbind, lapply(rlist, FUN = function(x){x$N_h}))
) %>% 
  mutate(
    N = n.1 + n.2
  ) %>% 
  select(-c(n.1, n.2))



t10cc <- t10cc %>% 
  mutate(
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
# %>%
#   select(-c(pv,se)) %>%
#   setDT() %>%
#   dcast(id ~ esp, value.var = c("coef"),fill = "") %>%
#   select(-id)



names2 <- c(
  "White and Yellow",
  " "," ",
  "Afro-Brazilians and Indigenous",
  " ", " ")

result <- data.frame(
  var = names2,
  cc = rep(NA, times = length(names2)),
  bw = rep(NA, times = length(names2))
)

#BA - TC
result$cc[1] <- t10cc$coef[[1]]
result$cc[2] <- t10cc$se[[1]]
result$cc[3] <- t10cc$N[[1]]
result$bw[1] <- t10cc$coef[[2]]
result$bw[2] <- t10cc$se[[2]]
result$bw[3] <- t10cc$N[[2]]

#PPI - TC
result$cc[4] <- t10cc$coef[[3]]
result$cc[5] <- t10cc$se[[3]]
result$cc[6] <- t10cc$N[[3]]
result$bw[4] <- t10cc$coef[[4]]
result$bw[5] <- t10cc$se[[4]]
result$bw[6] <- t10cc$N[[4]]



colnames(result) <- c("", "(1)", "(2)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Raca_v1.tex")

rm(base_ab, base_ppi, result, rlist, t10cc, latex_table, names2)

# ---------------------------------------------------------------------------- #
## 8.2 Sex ----
# ---------------------------------------------------------------------------- #
### 8.2.1 FEM ----
# ---------------------------------------------------------------------------- #
base_fem <- base %>% 
  filter(fem == 1) %>% 
  setDT()


base_fem <- base_fem[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
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
### 8.2.2 MASC ----
# ---------------------------------------------------------------------------- #

base_masc <- base %>% 
  filter(fem == 0) %>% setDT()

base_masc <- base_masc[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
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
### 8.2.3 Regression by sex ----
# ---------------------------------------------------------------------------- #

rlist <- list()
ano_list <- c(2018)

for(ano_ref in ano_list) {
  
  
  ano_comp <- ano_ref + 1
  for(df in c("base_fem", "base_masc")) {
    
    base_a <- get(df)
    
    
    #Com controles
    ef <- dummy_cols(base_a$seg[base_a$ano == ano_ref])
    ef <- ef %>% select(-1,-2)
    
    rlist[[as.character(paste0(df,"_",ano_comp,"-",ano_ref,"C|TC"))]] <- rdrobust(
      y = base_a$d.media[base_a$ano == ano_comp],
      x = base_a$dist_hv_border[base_a$ano == ano_ref],
      c = 0,
      p =1,
      cluster = base_a$seg[base_a$ano == ano_ref],
      weights = base_a$obs[base_a$ano == ano_ref],
      vce = "hc0",
      covs = cbind(
        ef,
        base_a$lat[base_a$ano == ano_ref],
        base_a$lon[base_a$ano == ano_ref]
      )
    )
    
    #Banda Fixa
    rlist[[as.character(paste0(df,"_",ano_comp,"-",ano_ref,"BW|TC"))]] <- rdrobust(
      y = base_a$d.media[base_a$ano == ano_comp],
      x = base_a$dist_hv_border[base_a$ano == ano_ref],
      c = 0,
      p =1,
      h = bw_main_a,
      b = bw_bias_a,
      cluster = base_a$seg[base_a$ano == ano_ref],
      weights = base_a$obs[base_a$ano == ano_ref],
      vce = "hc0",
      covs = cbind(
        ef,
        base_a$lat[base_a$ano == ano_ref],
        base_a$lon[base_a$ano == ano_ref]
      )
    )
    
    message("Finished for sex group: ",df)
    
  }
  
}
rm(ef, ano_ref, ano_comp)



# ---------------------------------------------------------------------------- #
### 8.2.4 TAB ----
# ---------------------------------------------------------------------------- #
t10cc <- data.frame(
  coef = do.call(rbind,lapply(rlist, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(rlist, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(rlist, FUN = function(x){x$pv[3]})),
  n = do.call(rbind, lapply(rlist, FUN = function(x){x$N_h}))
) %>% 
  mutate(
    N = n.1 + n.2
  ) %>% 
  select(-c(n.1, n.2))


t10cc <- t10cc %>% 
  mutate(
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



names2 <- c(
  "Female",
  " "," ",
  "Male",
  " ", " ")

result <- data.frame(
  var = names2,
  cc = rep(NA, times = length(names2)),
  bw = rep(NA, times = length(names2))
)

#FEM - TC
result$cc[1] <- t10cc$coef[[1]]
result$cc[2] <- t10cc$se[[1]]
result$cc[3] <- t10cc$N[[1]]
result$bw[1] <- t10cc$coef[[2]]
result$bw[2] <- t10cc$se[[2]]
result$bw[3] <- t10cc$N[[2]]

#MASC - TC

result$cc[4] <- t10cc$coef[[3]]
result$cc[5] <- t10cc$se[[3]]
result$cc[6] <- t10cc$N[[3]]
result$bw[4] <- t10cc$coef[[4]]
result$bw[5] <- t10cc$se[[4]]
result$bw[6] <- t10cc$N[[4]]




colnames(result) <- c("", "(1)", "(2)")



# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Sexo_v1.tex")

rm(base_masc, result, base_fem, rlist, t10cc, df, latex_table, names2)


# ---------------------------------------------------------------------------- #
## 8.3 Mae Educ ----
# ---------------------------------------------------------------------------- #
###8.3.1 High ----
# ---------------------------------------------------------------------------- #
base_high <- base %>% 
  filter(
    esc_mae %in% c("D","E","F")
  ) %>% setDT()


base_high <- base_high[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
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
### 8.3.2 Low ----
# ---------------------------------------------------------------------------- #
base_low <- base %>% 
  filter(
    esc_mae %in% c("A", "B", "C")
  ) %>% setDT()

base_low <- base_low[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
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
###8.3.3 Regression ----
rlist <- list()


ano_list <- c(2018)

for(ano_ref in ano_list) {
  
  
  ano_comp <- ano_ref + 1
  for(df in c("base_low", "base_high")) {
    
    base_a <- get(df)
    
    
    #Com controles
    ef <- dummy_cols(base_a$seg[base_a$ano == ano_ref])
    ef <- ef %>% select(-1,-2)
    
    rlist[[as.character(paste0(df,"_",ano_comp,"-",ano_ref,"C|TC"))]] <- rdrobust(
      y = base_a$d.media[base_a$ano == ano_comp],
      x = base_a$dist_hv_border[base_a$ano == ano_ref],
      c = 0,
      p =1,
      cluster = base_a$seg[base_a$ano == ano_ref],
      weights = base_a$obs[base_a$ano == ano_ref],
      vce = "hc0",
      covs = cbind(
        ef,
        base_a$lat[base_a$ano == ano_ref],
        base_a$lon[base_a$ano == ano_ref]
      )
    )
    
    
    #Banda Fixa
    rlist[[as.character(paste0(df,"_",ano_comp,"-",ano_ref,"BW|TC"))]] <- rdrobust(
      y = base_a$d.media[base_a$ano == ano_comp],
      x = base_a$dist_hv_border[base_a$ano == ano_ref],
      c = 0,
      p =1,
      h = bw_main_a,
      b = bw_bias_a,
      cluster = base_a$seg[base_a$ano == ano_ref],
      weights = base_a$obs[base_a$ano == ano_ref],
      vce = "hc0",
      covs = cbind(
        ef,
        base_a$lat[base_a$ano == ano_ref],
        base_a$lon[base_a$ano == ano_ref]
      )
    )
    
    
  }
  
}
rm(ef, ano_ref, ano_comp, ano_list)


# ---------------------------------------------------------------------------- #
###8.3.4 Result Table ----
# ---------------------------------------------------------------------------- #
t10cc <- data.frame(
  coef = do.call(rbind,lapply(rlist, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(rlist, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(rlist, FUN = function(x){x$pv[3]})),
  n = do.call(rbind, lapply(rlist, FUN = function(x){x$N_h}))
) %>% 
  mutate(
    N = n.1 + n.2
  ) %>% 
  select(-c(n.1, n.2))


t10cc <- t10cc %>% 
  mutate(
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


names2 <- c(
  "Low Education",
  " "," ",
  "High Education",
  " ", " ")

result <- data.frame(
  var = names2,
  cc = rep(NA, times = length(names2)),
  bw = rep(NA, times = length(names2))
)

#Low - TC
result$cc[1] <- t10cc$coef[[1]]
result$cc[2] <- t10cc$se[[1]]
result$cc[3] <- t10cc$N[[1]]
result$bw[1] <- t10cc$coef[[2]]
result$bw[2] <- t10cc$se[[2]]
result$bw[3] <- t10cc$N[[2]]

#High - TC
result$cc[4] <- t10cc$coef[[3]]
result$cc[5] <- t10cc$se[[3]]
result$cc[6] <- t10cc$N[[3]]
result$bw[4] <- t10cc$coef[[4]]
result$bw[5] <- t10cc$se[[4]]
result$bw[6] <- t10cc$N[[4]]



colnames(result) <- c("", "(1)", "(2)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Mae_Education_v1.tex")

rm(base_low, result, base_high, rlist, t10cc, df, latex_table, base_a, ano_list, names, names2)


# ---------------------------------------------------------------------------- #
# 9.Migration ------
# ---------------------------------------------------------------------------- #
## 9.1 Main specification regression ----
# ---------------------------------------------------------------------------- #

var_list <- c(
  "nonmig1", # MUN PROVA = RESIDENCIA = ESCOLA
  #"nonmig2", # Mun PROVA = RESIDENCIA != ESCOLA
  "nonmig3" #, # MUN PROVA != RESIDENCIA = ESCOLA
  #"nonmig4"  # Mun PROVA = ESCOLA != RESIDENCIA
)

rlist  <- list()

for (i in var_list) {
  
  cat("Rodando para:", i, "\n")
  
  base <- base %>% setDT()
  
  base_y <- base[get(i) == 1]
  
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
  
  
  
  
  
  ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
# ---------------------------------------------------------------------------- #
## 9.2 Table ----
# ---------------------------------------------------------------------------- #
  
  
  rlist[[as.character(paste0(i,"|C"))]] <- rdrobust(
    y = base_a$d.media[base_a$ano == 2019],
    x = base_a$dist_hv_border[base_a$ano == 2018],
    c = 0,
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
  
  
  rlist[[as.character(paste0(i,"|BW"))]] <- rdrobust(
    y = base_a$d.media[base_a$ano == 2019],
    x = base_a$dist_hv_border[base_a$ano == 2018],
    c = 0,
    p =1,
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
rm(ef,i, base_y)

tab <- data.frame(
  coef = do.call(rbind,lapply(rlist, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(rlist, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(rlist, FUN = function(x){x$pv[3]})),
  n.1 = do.call(rbind,lapply(rlist, FUN = function(x){x$N_h[1]})),
  n.2 = do.call(rbind,lapply(rlist, FUN = function(x){x$N_h[2]}))
)


tab <- tab %>% 
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "**", 
                         ifelse(pv < 0.05, "*", 
                                ifelse(pv < 0.1, "", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]"),
  ) %>%
  select(-c(pv))

row <- c(
  "No Migration",
  " "," ",
  # "Group 2 (School)",
  # " "," ",
  "Migration to ENEM",
  " ", " " #,
  # "Group 4 (Residency)",
  # " ", " "
)

result <- data.frame(
  var = row,
  cc = rep(NA, times = length(names))
  #,
  #bw = rep(NA, times = length(names))
)


#NonMig1


result$cc[1] <- tab$coef[[1]]
result$cc[2] <- tab$se[[1]]
result$cc[3] <- tab$N[[1]]

# result$bw[1] <- tab$coef[[2]]
# result$bw[2] <- tab$se[[2]]
# result$bw[3] <- tab$N[[2]]

#NonMig2


result$cc[4] <- tab$coef[[3]]
result$cc[5] <- tab$se[[3]]
result$cc[6] <- tab$N[[3]]

# result$bw[4] <- tab$coef[[4]]
# result$bw[5] <- tab$se[[4]]
# result$bw[6] <- tab$N[[4]]

# #NonMig3
# 
# 
# result$cc[7] <- tab$coef[[5]]
# result$cc[8] <- tab$se[[5]]
# result$cc[9] <- tab$N[[5]]
# 
# result$bw[7] <- tab$coef[[6]]
# result$bw[8] <- tab$se[[6]]
# result$bw[9] <- tab$N[[6]]
# 
# #NonMig4
# 
# 
# result$cc[10] <- tab$coef[[7]]
# result$cc[11] <- tab$se[[7]]
# result$cc[12] <- tab$N[[7]]
# 
# result$bw[10] <- tab$coef[[8]]
# result$bw[11] <- tab$se[[8]]
# result$bw[12] <- tab$N[[8]]


colnames(result) <- c("", "(1)", "(2)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/grupos_mig_v2.tex")

rm(latex_table, result, row,  tab, rlist, var_list)

# ---------------------------------------------------------------------------- #
# 10. Time Zone ----
# ---------------------------------------------------------------------------- #
## 10.1 Data -----
# ---------------------------------------------------------------------------- #
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
    
    
    new_dist = dist_hv_border/100000) %>% 
  setDT()

base_a <- base[priv0 == 1,.(media = mean(media, na.rm = T),
                            obs = .N),
               by = .(mun_prova,ano,dist_hv_border,seg,lat,lon, uf)] %>% 
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
  filter(dup2 == 2)


#Filtro de mun por fuso

# ---------------------------------------------------------------------------- #
## 10.2. Reg Loop ----
# ---------------------------------------------------------------------------- #




result_time <- list()

time_list <- c(13, 12) #11 should be aborted with missing treatmnent obs.

for (i in time_list){
  
  if (i == 13){
    b_temp <- base_a %>%
      group_by(mun_prova) %>% 
      filter(uf %in% c("GO", "DF", "MG", "ES", "RJ", "SP", "SC", "PR", "RS", "BA", "SE",
                       "AL", "PE", "PB", "RN", "CE","PI", "MA", "TO", "PA", "AP")) %>% 
      arrange(mun_prova, ano) %>% 
      mutate(
        lat = as.factor(lat),
        lon = as.factor(lon)
      )
    
  } else if(i == 12){
    b_temp <- base_a %>%
      group_by(mun_prova) %>% 
      filter(uf %in% c("MT", "MS", "RO", "RR") | uf == "AM" &
               !mun_prova %in%
               c(1300201, 1300607, 1300706, 1301407, 1301506, 1301654, 1301803, 1301951,
                 1302306, 1302405, 1303502, 1303908, 1304062)) %>%
      arrange(mun_prova, ano) %>% 
      mutate(
        lat = as.factor(lat),
        lon = as.factor(lon)
      )
  } else {
    b_temp <- base_a %>%
      group_by(mun_prova) %>% 
      filter(uf == "AC" | mun_prova %in% c(1300201, 1300607, 1300706, 1301407, 1301506,
                                           1301654, 1301803, 1301951, 1302306, 1302405,
                                           1303502, 1303908, 1304062)) %>%       
      arrange(mun_prova, ano) %>% 
      mutate(
        lat = as.factor(lat),
        lon = as.factor(lon)
      )
  }
  
  
  summary(b_temp$lat)
  summary(b_temp$lon)
  
  print(paste0("Rows:", nrow(b_temp), " ", i))
  
  
  
  
  result_time[[as.character(i)]] <- rdrobust(
    y = b_temp$d.media[b_temp$ano == 2019],
    x = b_temp$dist_hv_border[b_temp$ano == 2018],
    c = 0,
    p = 1,
    cluster = b_temp$mun_prova[b_temp$ano == 2018],
    weights = b_temp$obs[b_temp$ano == 2018],
    vce = "hc0",
    covs = cbind(
      b_temp$lat[b_temp$ano == 2018],
      b_temp$lon[b_temp$ano == 2018]
    )
  )
  
  message("Terminado para: ", i)
  
  
  rm(b_temp)
}
# ---------------------------------------------------------------------------- #
## 10.3 Result Table ----
# ---------------------------------------------------------------------------- #


result_tab <- data.frame(
  coef = do.call(rbind,lapply(result_time, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(result_time, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(result_time, FUN = function(x){x$pv[3]})),
  n = do.call(rbind, lapply(result_time, FUN = function(x){x$N_h}))
)


result_tab <- result_tab %>% 
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "**", 
                         ifelse(pv < 0.05, "*", 
                                ifelse(pv < 0.1, "", "")
                         ))),
    se =paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]"),
    esp = 1:2,
    id = 1
  ) %>% 
  select(-c(pv))
# setDT() %>%
# dcast(id ~ esp, value.var = c("coef"),fill = "") %>%
# select(-id)


names <- c("2019 - 2018",
           " ",
           " ")

result <- data.frame(
  var = names,
  fuso2 = rep(NA, times = length(names)),
  fuso1 = rep(NA, times = length(names))
)




result$fuso1[1] <- result_tab$coef[[1]]
result$fuso1[2] <- result_tab$se[[1]]
result$fuso1[3] <- result_tab$N[[1]]

result$fuso2[1] <- result_tab$coef[[2]]
result$fuso2[2] <- result_tab$se[[2]]
result$fuso2[3] <- result_tab$N[[2]]




colnames(result) <- c("", "(1)", "(2)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/fuso_resultado_v1.tex")

rm(result, result_tab, latex_table,i, result_time, names, time_list)

# ---------------------------------------------------------------------------- #
# 11. Bandwith test ----
# ---------------------------------------------------------------------------- #

base_a <- base[priv0 == 1,.(media = mean(media, na.rm = T),
                            obs = .N),
               by = .(mun_prova,ano,dist_hv_border,seg,lat,lon, uf)] %>% 
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
  filter(dup2 == 2)

# Estes valores estão no item 2.
# h <- rlist_a[[3]]$bws[1]
# b <- rlist_a[[3]]$bws[2]

h <- bw_main_a
b <- bw_bias_a


# Range de bandwidths
bws <- seq(0.3*h,1.5*h,0.1*h)

# Efeitos fixos
ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
ef <- ef %>% select(-1,-2)

rlist_bw <- list()

# Loop nos cutoffs
for (c in 1:length(bws)) {
  
  print(paste0("Rodada: ", c))
  
  rlist_bw[[c]] <- rdrobust(
    y = base_a$d.media[base_a$ano == 2019],
    x = base_a$dist_hv_border[base_a$ano == 2018],
    c = 0,
    cluster = base_a$seg[base_a$ano == 2018],
    vce = "hc0",
    weights = base_a$obs[base_a$ano == 2018],
    covs = cbind(ef,
                 base_a$lat[base_a$ano == 2018],
                 base_a$lon[base_a$ano == 2018]),
    h = bws[c],
    b = b
  )
  
} # fim do loop nos cutoffs (c)

rm(ef, bws, bws_b)


# save(rlist_bw, file = "results/v8/rdd_bw.RData")
# rm(h,b,rlist_bw)
# 
# # Lista de estimativas
# load(file = "results/v8/rdd_bw.RData")

# Lista de coeficientes
tablist_bw <- data.frame(
  coef = do.call(rbind,lapply(rlist_bw, FUN = function(x){x$coef[3]})),
  ll = do.call(rbind,lapply(rlist_bw, FUN = function(x){x$ci[3,1]})),
  ul = do.call(rbind,lapply(rlist_bw, FUN = function(x){x$ci[3,2]})),
  c = seq(0.3,1.5,0.1)
)

# Gráfico
graph <- ggplot(data = tablist_bw) +
  geom_hline(yintercept = 0, color = "red") +
  geom_line(mapping = aes(x = c, y = coef, group = 1)) +
  geom_ribbon(mapping = aes(x = c, ymin = ll, ymax = ul), alpha = 0.2) +
  theme_bw() +
  ylab("Coefficient") +
  xlab("Multiple of the Optimal Bandwidth") +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        strip.text = element_text(size = 20)) + 
  scale_x_continuous(breaks = seq(0.1,1.5,0.2), labels = as.character(seq(0.1,1.5,0.2)))

print(graph)


ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bandwith_v2.png", plot = graph,device = "png", width = 15, height = 10)
ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/bandwith_v2.pdf", plot = graph,device = "pdf", width = 8, height = 6)

rm(graph, tablist_bw, rlist_bw, h, b, c, base_a)

# ---------------------------------------------------------------------------- #
# 12. Balance Figs ----
# ---------------------------------------------------------------------------- #
## 12.1 Lists  -----
# ---------------------------------------------------------------------------- #


var_list <- c("id18", "fem", "ppi", "escp", "escm", "dom5",
              "renda1", "pibpc", "pai_trab_man", "mae_trab_man",
              "temp_d1", "temp_d2", "umid_d1", "umid_d2")

vnames <- c(
  '18 years old',
  'Female',
  'African Brazilian \n or Indigenous',
  'Father with \n highschool',
  'Mother with \n highschool',
  '5 or more people \n in household',
  'Up to 1 MW \nhousehold income',
  'GDP per capita',
  'Father in \n manual labor',
  'Mother in \n manual labor',
  "Temperature - Day 1",
  "Temperature - Day 2",
  "Humidity - Day 1",
  "Humidity - Day 2"
)

# ---------------------------------------------------------------------------- #
## 12.2 Regression  -----
# ---------------------------------------------------------------------------- #

base_inpe_t <- base %>%
  filter(abs(dist_hv_border) <= bw_main_a) %>% 
  setDT()

mun_nopair <- list()

for (i in var_list) {
  
  #Base
  temp <- base_inpe_t %>% 
    filter(priv0 == 1) %>% 
    filter(abs(dist_hv_border) < bw_main_a) %>% 
    filter(!is.na(.data[[i]]))
  
  # --- Municipal Level ---- #
  
  base_mun <- temp %>% 
    group_by(mun_prova, lat, lon, seg, dist_hv_border, ano) %>% 
    summarise(
      var_y = mean(.data[[i]], na.rm = T),
      obs = n(),
      .groups = "drop"
    ) %>% 
    #Organizing
    filter(as.numeric(ano) %in% c(2018, 2019)) %>%
    arrange(mun_prova, ano) %>%
    group_by(mun_prova) %>%
    
    #Year difference
    mutate(
      dup1 = 1,
      dup2 = sum(dup1),
      v1_var_y = ifelse(ano == 2018, var_y, NA),
      v2_var_y = max(v1_var_y, na.rm = T),
      dvar_y = var_y - v2_var_y
    ) %>%
    ungroup() %>%
    filter(dup2 == 2) %>%
    select(-c(dup2, dup1, v1_var_y, v2_var_y))
  
  #Seg dummy 
  ef <- dummy_cols(base_mun$seg[base_mun$ano == 2018]) 
  ef <- ef %>% select(-1,-2)
  
  

  #Mun results - No pairing
  mun_nopair[[as.character(paste0("dif_",i))]] <-
    rdrobust(
      y = base_mun$var_y[base_mun$ano == 2019],
      x = base_mun$dist_hv_border[base_mun$ano == 2018],
      c = 0,
      p = 1,
      h = bw_main_a,
      b = bw_bias_a,
      weights = base_mun$obs[base_mun$ano == 2018],
      cluster = base_mun$seg[base_mun$ano == 2018],
      vce = "hc0",
      covs = cbind(ef,
                   base_mun$lat[base_mun$ano == 2018],
                   base_mun$lon[base_mun$ano == 2018]  )
    )
  
  
  
  message("Finished for: ", i)
  
  rm(base_mun, ef )
  
}

# ---------------------------------------------------------------------------- #
## 12.3 Dataframe ----
# ---------------------------------------------------------------------------- #

covs <- data.frame(
  ano = rep(2017:2017, length(vnames)),
  var = vnames,
  ep18 = rep(NA, times = length(vnames))
) %>% 
  mutate(
    ano = "Both"
  )



# ----------------- #
# ---- No Pair ---- #
# ----------------- #

covs_np <- data.frame(
  ano = rep(2017:2017, length(vnames)),
  var = vnames,
  ep18 = rep(NA, times = length(vnames))
) %>% 
  mutate(
    ano = "Both"
  )

covs_np$ep18[covs_np$var == vnames[1]] <- mun_nopair[["dif_id18"]]$z[[3]] #id18
covs_np$ep18[covs_np$var == vnames[2]] <- mun_nopair[["dif_fem"]]$z[[3]] #fem
covs_np$ep18[covs_np$var == vnames[3]] <- mun_nopair[["dif_ppi"]]$z[[3]] #ppi
covs_np$ep18[covs_np$var == vnames[4]] <- mun_nopair[["dif_escp"]]$z[[3]] #escp
covs_np$ep18[covs_np$var == vnames[5]] <- mun_nopair[["dif_escm"]]$z[[3]] #escm
covs_np$ep18[covs_np$var == vnames[6]] <- mun_nopair[["dif_dom5"]]$z[[3]] #dom5
covs_np$ep18[covs_np$var == vnames[7]] <- mun_nopair[["dif_renda1"]]$z[[3]] #renda1
covs_np$ep18[covs_np$var == vnames[8]] <- mun_nopair[["dif_pibpc"]]$z[[3]] #pibpc
covs_np$ep18[covs_np$var == vnames[9]] <- mun_nopair[["dif_pai_trab_man"]]$z[[3]] #trab manual Pai
covs_np$ep18[covs_np$var == vnames[10]] <- mun_nopair[["dif_mae_trab_man"]]$z[[3]] #trab manual Mae
covs_np$ep18[covs_np$var == vnames[11]] <- mun_nopair[["dif_temp_d1"]]$z[[3]] #temp1d
covs_np$ep18[covs_np$var == vnames[12]] <- mun_nopair[["dif_temp_d2"]]$z[[3]] #temp2d
covs_np$ep18[covs_np$var == vnames[13]] <- mun_nopair[["dif_umid_d1"]]$z[[3]] #um1d
covs_np$ep18[covs_np$var == vnames[14]] <- mun_nopair[["dif_umid_d2"]]$z[[3]] #um2d


# ---------------------------------------------------------------------------- #
## 12.4 Plot  ---- 
# ---------------------------------------------------------------------------- #


# ----------------- #
# ---- No Pair ---- #
# ----------------- #

plot_covs <- ggplot(data = covs_np) +
  theme_bw() +
  labs(x = 't-statistic', y = NULL) +
  scale_x_continuous(breaks = c(-1.96, 0, +1.96),
                     labels = c(-1.96, '', 1.96),
                     limits = c(-6, 6)) +
  geom_vline(xintercept = c(-1.96, 1.96), color = 'red', linetype = 'dashed', linewidth = 1) +
  geom_point(aes(x = ep18, y = var), color = '#1A2D99', size = 2.25) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

print(plot_covs)

ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/covs_test_dif_.png"), device = "png", height = 10, width = 7)
ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/covs_test_dif_.eps"), device = "eps", height = 10, width = 7)

rm( covs_np, plot_covs, temp, mun_nopair, var_list, vnames, i, covs, base_inpe_t,
    base, base_a)



# ---------------------------------------------------------------------------- #
# 13. SAEB ----
# ---------------------------------------------------------------------------- #

saeb_base <- readRDS("Z:/Tuffy/Paper - HV/Bases/saeb_total.RDS") %>% 
  setDT()


# ---------------------------------------------------------------------------- #
##13.1 Base ----
# ---------------------------------------------------------------------------- #

mun_exp <- base_a %>%
  filter(ano == 2019) %>% 
  select(mun_prova)


saeb_base <- saeb_base %>% 
  mutate(in_base = ifelse(mun_prova %in% mun_exp$mun_prova, 1, 0))

saeb_base <- saeb_base %>% 
  filter(in_base == 1 )
rm(mun_exp)

temp <- base_a %>% 
  filter(ano == 2018) %>% 
  select(mun_prova, lat, lon, dist_hv_border, seg)


saeb_base <- saeb_base %>% 
  left_join(temp, by = c("mun_prova" = "mun_prova"))

rm(temp)

# ---------------------------------------------------------------------------- #
## 13.2 Regression (BW) ----
# ---------------------------------------------------------------------------- #

# Lista para armazenar resultados
rlist_saeb <- list()


for (i in c("5","9","3")) {
  
  temp <- saeb_base %>% 
    filter(serie == i,
           !is.na(seg)) %>% 
    group_by(mun_prova) %>% 
    mutate( 
      dup1 = 1,
      dup2 = sum(dup1),
      v1 = ifelse(ano == 2017, lp, NA),
      v2 = max(v1, na.rm = T),
      d.media_lp = lp - v2) %>%
    
    #Média Mat
    mutate(
      v1 = ifelse(ano == 2017, mt, NA),
      v2 = max(v1, na.rm = T),
      d.media_mt = mt - v2) %>%
    filter(dup2 == 2) %>% 
    ungroup() %>%
    select(-c(v1,v2, dup1, dup2))  
  
  
  
  
  
  # 
  # temp <- saeb_base %>% 
  #   mutate(subset = case_when(
  #     abs(dist_hv_border) < bw_main_a ~ 1,
  #     .default = 0
  #   )
  #   ) %>% 
  #   filter(
  #     !is.na(d.media),
  #     subset == 1
  #   )
  # 
  # Dependent variable
  yv_lp <- temp %>%
    filter(ano == 2019) %>% 
    select(d.media_lp) 
  
  yv_mt <- temp %>%
    filter(ano == 2019) %>% 
    select(d.media_mt) 
  
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
  
  #peso
  
  w_lp <- temp %>% 
    filter(ano == 2017) %>% 
    select(lp_peso)
  
  w_mt <- temp %>% 
    filter(ano == 2017) %>% 
    select(mt_peso)
  
  ef <- dummy_cols(clu$seg)
  ef <- ef %>% select(-1,-2)
  
  
  
  
  
  
  # Lista para armazenamento dos resultados
  
  # Regressão LP --------------------------------------------------------------#
  rlist_saeb[[paste0("LP|",i)]] <- rdrobust(
    y = yv_lp$d.media_lp,
    x = xv$dist_hv_border,
    c = 0,
    h = bw_main_a,
    b = bw_bias_a,
    weights = w_lp,
    cluster = clu,
    vce = "hc0",
    covs = cbind(ef,
                 latv,
                 lonv)
  )
  
  # # Calculating average values of the dependent variable, within the optimal bandwidth
  # bw <- rlist_saeb[[ind]][[1]]$bws[1, 1]
  # media_l <- base %>% 
  #   filter(dist_hv_border %between% c(-bw,0) & !is.na(lp) & !is.na(peso_aluno_lp)) %>% 
  #   summarise(media_l = weighted.mean(lp,peso_aluno_lp))
  # 
  # media_h <- base %>% 
  #   filter(dist_hv_border %between% c(0,bw) & !is.na(lp) & !is.na(peso_aluno_lp)) %>% 
  #   summarise(media_l = weighted.mean(lp,peso_aluno_lp))
  # 
  # rlist_saeb[[ind]][[1]]$media_l <- media_l
  # rlist_saeb[[ind]][[1]]$media_h <- media_h
  # 
  # rm(bw,media_l,media_h)
  
  # Regressão MT --------------------------------------------------------------#
  
  rlist_saeb[[paste0("MT|",i)]] <- rdrobust(
    y = yv_mt$d.media_mt,
    x = xv$dist_hv_border,
    c = 0,
    h = bw_main_a,
    b = bw_bias_a,
    weights = w_mt,
    cluster = clu,
    vce = "hc0",
    covs = cbind(ef,
                 latv,
                 lonv)
  )
  
  # # Calculating average values of the dependent variable, within the optimal bandwidth
  # bw <- rlist_saeb[[ind]][[2]]$bws[1, 1]
  # media_l <- base %>% 
  #   filter(dist_hv_border %between% c(-bw,0) & !is.na(mt) & !is.na(peso_aluno_mt)) %>% 
  #   summarise(media_l = weighted.mean(mt,peso_aluno_mt))
  # 
  # media_h <- base %>% 
  #   filter(dist_hv_border %between% c(0,bw) & !is.na(mt) & !is.na(peso_aluno_mt)) %>% 
  #   summarise(media_l = weighted.mean(mt,peso_aluno_mt))
  # 
  # rlist_saeb[[ind]][[2]]$media_l <- media_l
  # rlist_saeb[[ind]][[2]]$media_h <- media_h
  # 
  # rm(bw,media_l,media_h)
  # 
  # -------------------------------------------------------------------------#
  
  message("Finished for group: ", i)
  rm(ef)
  
  
}
rm(clu, latv, lonv, temp, w_lp, w_mt, xv, yv_lp, yv_mt, i)

# ---------------------------------------------------------------------------- #
## 13.3 Tabelas ----
# ---------------------------------------------------------------------------- #

tab <- data.frame(
  coef = do.call(rbind,lapply(rlist_saeb, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(rlist_saeb, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(rlist_saeb, FUN = function(x){x$pv[3]})),
  n.1 = do.call(rbind, lapply(rlist_saeb, FUN = function(x){x$N_h[1]})),
  n.2 = do.call(rbind, lapply(rlist_saeb, FUN = function(x){x$N_h[2]}))
  
)


tab <- tab %>% 
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "**", 
                         ifelse(pv < 0.05, "*", 
                                ifelse(pv < 0.1, "", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]")
  ) %>%
  select(-c(pv))

names <- c("Language",
           " "," ",
           "Math",
           " ", " ")

result <- data.frame(
  var = names,
  ef5 = rep(NA, times = length(names)),
  ef9 = rep(NA, times = length(names)),
  em3 = rep(NA, times = length(names))
)

#5EF
#Lp
result$ef5[1] <- tab$coef[[1]]
result$ef5[2] <- tab$se[[1]]
result$ef5[3] <- tab$N[[1]]
##Mt
result$ef5[4] <- tab$coef[[2]]
result$ef5[5] <- tab$se[[2]]
result$ef5[6] <- tab$N[[2]]


#9EF
#Lp
result$ef9[1] <- tab$coef[[3]]
result$ef9[2] <- tab$se[[3]]
result$ef9[3] <- tab$N[[3]]
##Mt
result$ef9[4] <- tab$coef[[4]]
result$ef9[5] <- tab$se[[4]]
result$ef9[6] <- tab$N[[4]]

#3EM
#Lp
result$em3[1] <- tab$coef[[5]]
result$em3[2] <- tab$se[[5]]
result$em3[3] <- tab$N[[5]]
##Mt
result$em3[4] <- tab$coef[[6]]
result$em3[5] <- tab$se[[6]]
result$em3[6] <- tab$N[[6]]


colnames(result) <- c("", "(1)", "(2)", "(3)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Saeb.tex")

rm(rlist_saeb, saeb_base, tab,result, names, latex_table, base, base_a )


# ---------------------------------------------------------------------------- #
# 14. Year Lvl. ----
# ---------------------------------------------------------------------------- #
## 14.1 Data ----
# ---------------------------------------------------------------------------- #
#' Vamos inciar a estimação das diferenças anuais através da abertura das bases de dados
#' e da agregação destas para o nível municipal.

# 1) Abrindo a base de dados:
base <- data.frame() #Criando a base de dados vazia que será preenchida pelos dados anuais

for(ano in 2013:2019) { #loop para abertura das bases de dados
  
  #Abrindo a base de dados de cada ano
  base_temp <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_",ano,".RDS")) %>% 
    select( #selecionando apenas as colunas principais
      mun_prova, ano, id_enem, hv, priv0, lon, lat, dist_hv_border, seg, media, idade, conclusao
    ) %>% 
    filter(
      conclusao == 2 #mantendo apenas alunos concluintes
    )
  
  
  #Agregando a base de dados para o nível municipal
  base_nota <- base_temp[priv0 == 1,.(media_nota = mean(media, na.rm = T), #nota média
                                      obs = .N), #n° de observações
                         by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] #variáveis de agregação
  
  base_nota <- base_nota %>% 
    filter(abs(dist_hv_border) <= bw_main_a) #selecionando apenas os municípios dentro da janela ótima
  
  
  summary(base$conclusao)
  
  #Salvando a base de dados agregada no nível municipal:
  saveRDS(base_nota, file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/Agregados/base_nota_ag_",ano,".RDS"))
  
  base <- rbind(base, base_temp) %>%  #unindo as bases de dados]
    setDT()
  message("Finalizado para ", ano)
  
  rm(base_temp, base_nota, ano_ref) #removendo os itensque não serão mais utilizados
}



base_a <- base[priv0 == 1,.(media_nota = mean(media, na.rm = T),
                            obs = .N),
               by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)]

# ---------------------------------------------------------------------------- #
## 14.2 Regression Yearly ----
# ---------------------------------------------------------------------------- #

#2) Com os resultados da nota em nível:
c_rlist <- list() #lista para armazenar os resultados
#t_rlist <- list()


ano_list <- c(2013:2019) #Lista dos anos que serão comparados no loop

#Loop de regressão por ano:
for(ano_ref in ano_list) {
  
  base_t <- base_a %>% 
    filter(ano == ano_ref) #Selecionando apenas os dados no ano de referência
  
  #Com controles no ano de referência
  ef <- dummy_cols(base_t$seg[base_t$ano == ano_ref]) #EF de segmento
  ef <- ef %>% select(-1,-2)
  
  c_rlist[[as.character(paste0(ano_ref,"C|TC"))]] <- rdrobust( #Resultado armazenado na lista
    y = base_t$media_nota[base_t$ano == ano_ref], #nota media no município
    x = base_t$dist_hv_border[base_t$ano == ano_ref], #RV de distância
    c = 0, #Ponto de corte
    p =1,
    h = bw_main_a, #Limitando para a banda ótima de ref.
    b = bw_bias_a, #Limitando para a banda ótima de ref.
    cluster = base_t$seg[base_t$ano == ano_ref], #cluster no segmento
    weights = base_t$obs[base_t$ano == ano_ref], #peso nas amostras
    vce = "hc0", 
    covs = cbind( #covariadas
      ef,
      base_t$lat[base_t$ano == ano_ref],
      base_t$lon[base_t$ano == ano_ref]
    )
  )
  
  
}

# ---------------------------------------------------------------------------- #
##14.3 Plot ----
# ---------------------------------------------------------------------------- #
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





p <- ggplot(t10cc, aes(x = ano, y = coef)) +
  geom_errorbar(aes(ymin = coef - se, ymax = coef + se), width = 0.2, size = 0.8, color = "black") +
  geom_point(size = 3, color = "black") +
  geom_hline(yintercept = 0, color = "#D62728", linewidth = 1) +
  labs(x = "Year", y = "Average ENEM Score Coefficient") +
  geom_vline(xintercept = 2018.5, color = "#BEBEBE", linetype = "dashed", size = 0.8) +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    legend.position = "none"
  ) +
  scale_x_continuous(breaks = t10cc$ano) 


print(p)

ggsave(plot = p, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/anos_lvl.png"), device = "png", height = 7, width = 10)
ggsave(plot = p, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/anos_lvl.eps"), device = "eps", height = 7, width = 10)



rm(base_a, base_t, c_rlist, result, rlist, t10cc, t10nc, ano, ano_list,
   latex_table, names, ef, p, base, ano_ref, bw_main_a, bw_main_p, bw_bias_a, bw_bias_p)





# ---------------------------------------------------------------------------- #
# 15. Filter + Desc ----
# ---------------------------------------------------------------------------- #



base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/base_nota_2019.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/base_nota_2018.RDS"))) %>%
  setDT()


base_trei <- base %>% filter(treineiro == 1)

gc()

rm(base)


summary(base_trei$mun_escola)

# ---------------------------------------------------------------------------- #
## 15.1 Data Mock ----
# ---------------------------------------------------------------------------- #




base_trei <- base_trei[,.(media = mean(media, na.rm = T), obs = .N),
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
## 15.2 Regs ----
# ---------------------------------------------------------------------------- #



ef2 <- dummy_cols(base_trei$seg[base_trei$ano == 2018])
ef2 <- ef2 %>% select(-1,-2)


list <- list()



# ---------------------------------------------------------------------------- #
### 15.2.1 Mock -----
# ---------------------------------------------------------------------------- #


list[[as.character(paste0(2019,"-",2018,"C|Trei"))]] <- rdrobust(
  y = base_trei$d.media[base_trei$ano == 2019],
  x = base_trei$dist_hv_border[base_trei$ano == 2018],
  c = 0,
  cluster = base_trei$seg[base_trei$ano == 2018],
  weights = base_trei$obs[base_trei$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef2,
    base_trei$lat[base_trei$ano == 2018],
    base_trei$lon[base_trei$ano == 2018]
  )
)

# ---------------------------------------------------------------------------- #
## 15.3 TC ----
# ---------------------------------------------------------------------------- #

base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2019.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2018.RDS"))) %>%
  setDT() %>% 
  filter(conclusao == 2)




#Todos
base_t <- base[,.(media = mean(media, na.rm = T), obs = .N),
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




#Todos con em Pub
base_a <- base[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
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

#Todos conc em Priv
base_p <- base[priv1 == 1,.(media = mean(media, na.rm = T), obs = .N),
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

#Todos con, pub - Fed
base_psf <- base[dep_adm %in% c(2,3),.(media = mean(media, na.rm = T), obs = .N),
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

#Todos con, pub - Fed
base_et <- base[dep_adm == 2,.(media = mean(media, na.rm = T), obs = .N),
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


#Todos con, pub - Fed
base_fed <- base[dep_adm == 1,.(media = mean(media, na.rm = T), obs = .N),
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

base_mun <- base[dep_adm == 3,.(media = mean(media, na.rm = T), obs = .N),
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
###15.3.1 Reg -----
# ---------------------------------------------------------------------------- #


ef <- dummy_cols(base_t$seg[base_t$ano == 2018])
ef <- ef %>% select(-1,-2)

# ---------------------------------------------------------------------------- #
#### 15.3.1.1 Todos Concluintes ----
# ---------------------------------------------------------------------------- #
list[[as.character(paste0(2019,"-",2018,"C|TC"))]] <- rdrobust(
  y = base_t$d.media[base_t$ano == 2019],
  x = base_t$dist_hv_border[base_t$ano == 2018],
  c = 0,
  cluster = base_t$seg[base_t$ano == 2018],
  weights = base_t$obs[base_t$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_t$lat[base_t$ano == 2018],
    base_t$lon[base_t$ano == 2018]
  )
)

# ---------------------------------------------------------------------------- #
#### 15.3.1.2  Escola P ----
# ---------------------------------------------------------------------------- #

ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
ef <- ef %>% select(-1,-2)



list[[as.character(paste0(2019,"-",2018,"C|TCPub"))]] <- rdrobust(
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
#### 15.3.1.3 Escola Priv ----
# ---------------------------------------------------------------------------- #
ef <- dummy_cols(base_p$seg[base_p$ano == 2018])
ef <- ef %>% select(-1,-2)



list[[as.character(paste0(2019,"-",2018,"C|Priv"))]] <- rdrobust(
  y = base_p$d.media[base_p$ano == 2019],
  x = base_p$dist_hv_border[base_p$ano == 2018],
  c = 0,
  cluster = base_p$seg[base_p$ano == 2018],
  weights = base_p$obs[base_p$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_p$lat[base_p$ano == 2018],
    base_p$lon[base_p$ano == 2018]
  )
)

# ---------------------------------------------------------------------------- #
#### 15.3.1.4 Esc Pub - Fed ----
# ---------------------------------------------------------------------------- #
ef <- dummy_cols(base_psf$seg[base_psf$ano == 2018])
ef <- ef %>% select(-1,-2)



list[[as.character(paste0(2019,"-",2018,"C|PubSF"))]] <- rdrobust(
  y = base_psf$d.media[base_psf$ano == 2019],
  x = base_psf$dist_hv_border[base_psf$ano == 2018],
  c = 0,
  cluster = base_psf$seg[base_psf$ano == 2018],
  weights = base_psf$obs[base_psf$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_psf$lat[base_psf$ano == 2018],
    base_psf$lon[base_psf$ano == 2018]
  )
)

# ---------------------------------------------------------------------------- #
#### 15.3.1.5 Esc Estadual ----
# ---------------------------------------------------------------------------- #

ef <- dummy_cols(base_et$seg[base_et$ano == 2018])
ef <- ef %>% select(-1,-2)



list[[as.character(paste0(2019,"-",2018,"C|PubEsd"))]] <- rdrobust(
  y = base_et$d.media[base_et$ano == 2019],
  x = base_et$dist_hv_border[base_et$ano == 2018],
  c = 0,
  cluster = base_et$seg[base_et$ano == 2018],
  weights = base_et$obs[base_et$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_et$lat[base_et$ano == 2018],
    base_et$lon[base_et$ano == 2018]
  )
)

# ---------------------------------------------------------------------------- #
#### 15.3.1.6 Esc FED ----
# ---------------------------------------------------------------------------- #
ef <- dummy_cols(base_fed$seg[base_fed$ano == 2018])
ef <- ef %>% select(-1,-2)



list[[as.character(paste0(2019,"-",2018,"C|PubFed"))]] <- rdrobust(
  y = base_fed$d.media[base_fed$ano == 2019],
  x = base_fed$dist_hv_border[base_fed$ano == 2018],
  c = 0,
  cluster = base_fed$seg[base_fed$ano == 2018],
  weights = base_fed$obs[base_fed$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_fed$lat[base_fed$ano == 2018],
    base_fed$lon[base_fed$ano == 2018]
  )
)

# ---------------------------------------------------------------------------- #
#### 15.3.1.7 Esc MUN ----
# ---------------------------------------------------------------------------- #

ef <- dummy_cols(base_mun$seg[base_mun$ano == 2018])
ef <- ef %>% select(-1,-2)



list[[as.character(paste0(2019,"-",2018,"C|PubMun"))]] <- rdrobust(
  y = base_mun$d.media[base_mun$ano == 2019],
  x = base_mun$dist_hv_border[base_mun$ano == 2018],
  c = 0,
  cluster = base_mun$seg[base_mun$ano == 2018],
  weights = base_mun$obs[base_mun$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_mun$lat[base_mun$ano == 2018],
    base_mun$lon[base_mun$ano == 2018]
  )
)

# ---------------------------------------------------------------------------- #
### 15.4 Result Table -----
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



names <- c(#"%Concluded High School", "% ","% ",
           "Mock Examinees",
           " ", " ",
           "All Administrative School Types",
           " "," ",
           "Main Result",
           " ", " ",
           "Private School",
           " "," ",
           "Municipal + State School",
           " ", " ",
           "State School",
           " "," ",
           "Federal School",
           " ", " ",
           "Municipal School",
           " ", " ")

result <- data.frame(
  var = names,
  con = rep(NA, times = length(names)))



result$con[1] <- t10$coef[[1]]
result$con[2] <- t10$se[[1]]
result$con[3] <- t10$N[[1]]

result$con[4] <- t10$coef[[2]]
result$con[5] <- t10$se[[2]]
result$con[6] <- t10$N[[2]]

result$con[7] <- t10$coef[[3]]
result$con[8] <- t10$se[[3]]
result$con[9] <- t10$N[[3]]

result$con[10] <- t10$coef[[4]]
result$con[11] <- t10$se[[4]]
result$con[12] <- t10$N[[4]]

result$con[13] <- t10$coef[[5]]
result$con[14] <- t10$se[[5]]
result$con[15] <- t10$N[[5]]

result$con[16] <- t10$coef[[6]]
result$con[17] <- t10$se[[6]]
result$con[18] <- t10$N[[6]]

result$con[19] <- t10$coef[[7]]
result$con[20] <- t10$se[[7]]
result$con[21] <- t10$N[[7]]

result$con[22] <- t10$coef[[8]]
result$con[23] <- t10$se[[8]]
result$con[24] <- t10$N[[8]]


colnames(result) <- c("", "(1)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/comp_amostras_v1.tex")

rm(list = ls())
gc()
