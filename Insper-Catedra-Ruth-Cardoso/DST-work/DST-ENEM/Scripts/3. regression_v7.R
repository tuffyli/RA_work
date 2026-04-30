# ---------------------------------------------------------------------------- #
# Regressions
# Main estimations and Robustness
# Last edited by: Tuffy Licciardi Issa
# Date: 24/04/2026
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
library(kableExtra)

# ---------------------------------------------------------------------------- #
## 1.0 Data -----
# ---------------------------------------------------------------------------- #


base <- readRDS("Z:/Tuffy/Paper - HV/Bases/base_final.RDS") 
# ---------------------------------------------------------------------------- #
## 1.1 Base agregada ----
# ---------------------------------------------------------------------------- #


# ---------- #
#Res
# ---------- #
# base_res <- base[priv0 == 1,.(media    = mean(media, na.rm = T),
#                               escm     = mean(escm, na.rm = T),
#                               fem      = mean(fem, na.rm = T),
#                               idade    = mean(id18, na.rm = T),
#                               tempd1   = mean(temp_d1, na.rm = T),
#                               umidd2   = mean(umid_d2, na.rm = T),
#                               umidd1   = mean(umid_d1, na.rm = T),
#                               h13 = first(h13),
#                               h12 = first(h12),
#                               h11 = first(h11),
#                               h10 = first(h10),
#                               obs = .N),
#                  by = .(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res)] %>% 
#   filter(as.numeric(ano) %in% c(2018,2019)) %>% 
#   ungroup() %>% 
#   arrange(mun_res,ano) %>%
#   group_by(mun_res) %>%
#   #filter(!is.na(escm)) %>% 
#   mutate(
#     dup1 = 1,
#     dup2 = sum(dup1),
#     v1_nota = ifelse(ano == 2018, media, NA),
#     v2_nota = max(v1_nota, na.rm = T),
#     d.media = media - v2_nota
#     
#   ) %>%
#   ungroup() %>% 
#   filter(dup2 == 2) %>% 
#   select(-c(dup2, dup1, v1_nota, v2_nota)) 

# 
# base_res <- base_res %>%
#   group_by(mun_res) %>% 
#   filter(is.finite(descm) & !is.na(descm)) %>% 
#   filter(n_distinct(ano) == 2) %>% 
#   ungroup()


vars_diff <- c(
  "media",
  "escm", "escp", "pessoa", "empr_dom",
  "n_ban", "n_qua", "n_car", "n_gel", "n_cel",
  "pc", "internet", "renda1", "renda110", "renda10",
  "pibpc",
  "fem", "idade", "ppi",
  "tempd1", "tempd2", "umidd2", "umidd1"
)

base_res <- base[priv0 == 1, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  h10 = first(h10),
  obs = .N
), by = .(mun_res, ano, dist_hv_res, seg_res, lat_res, lon_res)] %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_res)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_res[[v1]] <- ifelse(base_res$ano == 2018, base_res[[v]], NA_real_)
  
  base_res[[v2]] <- ave(
    base_res[[v1]], 
    base_res$mun_res, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_res[[dv]] <- base_res[[v]] - base_res[[v2]]
  
  base_res[[dv]][!is.finite(base_res[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_res), value = TRUE)
base_res <- base_res %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia)

# ---------------------------------------------------------------------------- #
### 1.1.1 Aux Base ----
# ---------------------------------------------------------------------------- #

base_aux <- base_res %>% 
  select(mun_res, ano, h13:dumidd1) %>% 
  select(-obs, -d.media)



list <- list()

efr <- dummy_cols(base_res$seg_res[base_res$ano == 2018])
efr <- efr %>% select(-1,-2)


# ---------------------------------------------------------------------------- #
### 1.1.2 Main (Fuso) ----
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2019,"-",2018,"|fuso"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2019],
  x = base_res$dist_hv_res[base_res$ano == 2018],
  c = 0,
  cluster = base_res$seg_res[base_res$ano == 2018],
  weights = base_res$obs[base_res$ano == 2018],
  vce = "hc0",
  covs = cbind(
    efr,
    base_res$lat_res[base_res$ano == 2018],
    base_res$lon_res[base_res$ano == 2018],
    base_res$h13[base_res$ano == 2019],
    base_res$h12[base_res$ano == 2019],
    base_res$h11[base_res$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
### 1.1.3 Temperature ----
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2019,"-",2018,"|dem+temp"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2019],
  x = base_res$dist_hv_res[base_res$ano == 2018],
  c = 0,
  p = 1,
  cluster = base_res$seg_res[base_res$ano == 2018],
  weights = base_res$obs[base_res$ano == 2018],
  vce = "hc0",
  covs = cbind(
    efr,
    base_res$lat_res[base_res$ano == 2018],
    base_res$lon_res[base_res$ano == 2018],
    #Dists
    base_res$dtempd1[base_res$ano == 2019], #Temperature
    base_res$dumidd1[base_res$ano == 2019], #Humidity d1
    base_res$dumidd2[base_res$ano == 2019], #Humidty d2
    base_res$dtempd2[base_res$ano == 2019], #temp2
    
    #Parents and Individuals Characteristics
    base_res$descm[base_res$ano == 2019], #mother educ
    base_res$dfem[base_res$ano == 2019], #Female
    base_res$dppi[base_res$ano == 2019], #PPI
    base_res$didade[base_res$ano == 2019], #Age
    base_res$descp[base_res$ano == 2019], #father educ
    
    #Fuso
    base_res$h13[base_res$ano == 2019],
    base_res$h12[base_res$ano == 2019],
    base_res$h11[base_res$ano == 2019]
  )
)



# ---------------------------------------------------------------------------- #
### 1.1.4 Fuso + Temp + Socio  ----
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2019,"-",2018,"|fuso+temp+house"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2019],
  x = base_res$dist_hv_res[base_res$ano == 2018],
  c = 0,
  p = 1, 
  cluster = base_res$seg_res[base_res$ano == 2018],
  weights = base_res$obs[base_res$ano == 2018],
  vce = "hc0",
  covs = cbind(
    efr,
    base_res$lat_res[base_res$ano == 2018],
    base_res$lon_res[base_res$ano == 2018],
    #Weather
    base_res$dtempd1[base_res$ano == 2019], #Temperature
    base_res$dumidd1[base_res$ano == 2019], #Humidity d1
    base_res$dumidd2[base_res$ano == 2019], #Humidty d2
    base_res$dtempd2[base_res$ano == 2019], #temp2
    
    #Parents and Individuals Characteristics
    base_res$descm[base_res$ano == 2019], #mother educ
    base_res$dfem[base_res$ano == 2019], #Female
    base_res$dppi[base_res$ano == 2019], #PPI
    base_res$didade[base_res$ano == 2019], #Age
    base_res$descp[base_res$ano == 2019], #father educ
    
    #House
    base_res$dn_ban[base_res$ano == 2019], #bathrooms
    base_res$dpessoa[base_res$ano == 2019], #people in household
    base_res$dn_qua[base_res$ano == 2019], #houses
    base_res$dn_car[base_res$ano == 2019], #cars
    base_res$dn_gel[base_res$ano == 2019], #geladeira
    base_res$dn_cel[base_res$ano == 2019], #cel
    base_res$dpc[base_res$ano == 2019],    #pc
    base_res$dinternet[base_res$ano == 2019], #internet
    base_res$dempr_dom[base_res$ano == 2019], #Housekeeping
    
    
    #Timezones
    base_res$h13[base_res$ano == 2019],
    base_res$h12[base_res$ano == 2019],
    base_res$h11[base_res$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
### 1.1.5 All ----
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2019,"-",2018,"|all"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2019],
  x = base_res$dist_hv_res[base_res$ano == 2018],
  c = 0,
  p = 1, 
  cluster = base_res$seg_res[base_res$ano == 2018],
  weights = base_res$obs[base_res$ano == 2018],
  vce = "hc0",
  covs = cbind(
    efr,
    base_res$lat_res[base_res$ano == 2018],
    base_res$lon_res[base_res$ano == 2018],
    
    #Weather
    base_res$dtempd1[base_res$ano == 2019], #Temperature
    base_res$dumidd1[base_res$ano == 2019], #Humidity d1
    base_res$dumidd2[base_res$ano == 2019], #Humidty d2
    base_res$dtempd2[base_res$ano == 2019], #temp2
    
    #House
    base_res$dn_ban[base_res$ano == 2019], #bathrooms
    base_res$dpessoa[base_res$ano == 2019], #people in household
    base_res$dn_qua[base_res$ano == 2019], #houses
    base_res$dn_car[base_res$ano == 2019], #cars
    base_res$dn_gel[base_res$ano == 2019], #geladeira
    base_res$dn_cel[base_res$ano == 2019], #cel
    base_res$dpc[base_res$ano == 2019],    #pc
    base_res$dinternet[base_res$ano == 2019], #internet
    base_res$dempr_dom[base_res$ano == 2019], #Housekeeping
    
    #remaining
    base_res$descm[base_res$ano == 2019], #mother educ
    base_res$dfem[base_res$ano == 2019], #Female
    base_res$dppi[base_res$ano == 2019], #PPI
    base_res$didade[base_res$ano == 2019], #Age
    base_res$descp[base_res$ano == 2019], #father educ
    base_res$drenda1[base_res$ano == 2019], #wage < 1MW
    base_res$drenda110[base_res$ano == 2019], #wage 1MW - 10MW
    base_res$drenda10[base_res$ano == 2019], #wage > 10MW
    base_res$dpibpc[base_res$ano == 2019], #pibpc
    
    #Fuso
    base_res$h13[base_res$ano == 2019],
    base_res$h12[base_res$ano == 2019],
    base_res$h11[base_res$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
#Extração da banda ótima
bw_main_r  <- list[["2019-2018|all"]]$bws[1]
bw_bias_r  <- list[["2019-2018|all"]]$bws[2]

#Salvando a banda principal
save(bw_main_r, bw_bias_r,
     file = "Z:/Tuffy/Paper - HV/Resultados/bandwidths_2019_2018_Res_all.RData")
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
### 1.1.6 Pol + All ----
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2019,"-",2018,"|all+p2"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2019],
  x = base_res$dist_hv_res[base_res$ano == 2018],
  c = 0,
  p = 2, 
  cluster = base_res$seg_res[base_res$ano == 2018],
  weights = base_res$obs[base_res$ano == 2018],
  vce = "hc0",
  covs = cbind(
    efr,
    base_res$lat_res[base_res$ano == 2018],
    base_res$lon_res[base_res$ano == 2018],
    
    #Weather
    base_res$dtempd1[base_res$ano == 2019], #Temperature
    base_res$dumidd1[base_res$ano == 2019], #Humidity d1
    base_res$dumidd2[base_res$ano == 2019], #Humidty d2
    base_res$dtempd2[base_res$ano == 2019], #temp2
    
    #House
    base_res$dn_ban[base_res$ano == 2019], #bathrooms
    base_res$dpessoa[base_res$ano == 2019], #people in household
    base_res$dn_qua[base_res$ano == 2019], #houses
    base_res$dn_car[base_res$ano == 2019], #cars
    base_res$dn_gel[base_res$ano == 2019], #geladeira
    base_res$dn_cel[base_res$ano == 2019], #cel
    base_res$dpc[base_res$ano == 2019],    #pc
    base_res$dinternet[base_res$ano == 2019], #internet
    base_res$dempr_dom[base_res$ano == 2019], #Housekeeping
    
    #remaining
    base_res$descm[base_res$ano == 2019], #mother educ
    base_res$dfem[base_res$ano == 2019], #Female
    base_res$dppi[base_res$ano == 2019], #PPI
    base_res$didade[base_res$ano == 2019], #Age
    base_res$descp[base_res$ano == 2019], #father educ
    base_res$drenda1[base_res$ano == 2019], #wage < 1MW
    base_res$drenda110[base_res$ano == 2019], #wage 1MW - 10MW
    base_res$drenda10[base_res$ano == 2019], #wage > 10MW
    base_res$dpibpc[base_res$ano == 2019], #pibpc
    
    #Fuso
    base_res$h13[base_res$ano == 2019],
    base_res$h12[base_res$ano == 2019],
    base_res$h11[base_res$ano == 2019]
  )
)


# ---------------------------------------------------------------------------- #
### 1.1.7 Tabela  ----
# ---------------------------------------------------------------------------- #

t10 <- data.frame(
  coef   = sapply(list, function(x) x$coef[3]),
  se     = sapply(list, function(x) x$se[3]),
  pv     = sapply(list, function(x) x$pv[3]),
  n_left = sapply(list, function(x) x$N_h[1]),
  n_rght = sapply(list, function(x) x$N_h[2]),
  bw     = sapply(list, function(x) x$bws[1, 1]),
  totr   = sapply(list, function(x) x$N[2]),
  totl   = sapply(list, function(x) x$N[1])
)
print(t10)


# ---------------------------------------------------------------------------- #
# Helpers
# ---------------------------------------------------------------------------- #

fmt_est <- function(est, pv) {
  paste0(
    formatC(est, digits = 2, format = "f"),
    ifelse(pv < 0.01, "**",
           ifelse(pv < 0.05, "*",
                  ifelse(pv < 0.10, " ", "")))
  )
}

fmt_se <- function(se) {
  paste0("(", formatC(se, digits = 2, format = "f"), ")")
}

fmt_n <- function(n) {
  formatC(n, format = "d", big.mark = ",")
}

fmt_npair <- function(nl, nr) {
  paste0( fmt_n(nl), ", ", fmt_n(nr))
}

fmt_bw <- function(bw) {
  paste0(
    formatC(abs(bw) / 1000, digits = 0, format = "f"),
    " km"
  )
}



# ---------------------------------------------------------------------------- #
# Build final AER-style table
# ---------------------------------------------------------------------------- #

result <- data.frame(
  ` ` = c(
    "2019 - 2018",
    " ",
    "N = N$_L$, N$_R$",
    "BW",
    "Main Controls",
    "Temperature",
    "Sociodemographic Characteristics",
    "All Controls"
  ),
  `(1)` = c(
    fmt_est(t10$coef[1], t10$pv[1]),
    fmt_se(t10$se[1]),
    fmt_npair(t10$n_left[1], t10$n_rght[1]),
    fmt_bw(t10$bw[1]),
    "Yes",
    "No",
    "No",
    "--"
  ),
  `(2)` = c(
    fmt_est(t10$coef[2], t10$pv[2]),
    fmt_se(t10$se[2]),
    fmt_npair(t10$n_left[2], t10$n_rght[2]),
    fmt_bw(t10$bw[2]),
    "Yes",
    "Yes",
    "No",
    "--"
    
  ),
  `(3)` = c(
    fmt_est(t10$coef[3], t10$pv[3]),
    fmt_se(t10$se[3]),
    fmt_npair(t10$n_left[3], t10$n_rght[3]),
    fmt_bw(t10$bw[3]),
    "Yes",
    "Yes",
    "Yes",
    "--"
  ),
  `(4)` = c(
    fmt_est(t10$coef[4], t10$pv[4]),
    fmt_se(t10$se[4]),
    fmt_npair(t10$n_left[4], t10$n_rght[4]),
    fmt_bw(t10$bw[4]),
    "--",
    "--",
    "--",
    "Yes"
  ),
  `(5)` = c(
    fmt_est(t10$coef[5], t10$pv[5]),
    fmt_se(t10$se[5]),
    fmt_npair(t10$n_left[5], t10$n_rght[5]),
    fmt_bw(t10$bw[5]),
    "--",
    "--",
    "--",
    "Yes"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------- #
# Latex
# ---------------------------------------------------------------------------- #

latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lccccc",
  linesep = "",
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/DIFF_Principal_TC_v5.tex")

rm(latex_table, t10, result, list)
# ---------------------------------------------------------------------------- #
## 1.2 Dists -----
# ---------------------------------------------------------------------------- #

# Municipalities with special timezone treatment in Amazonas
am_mun_special <- c(
  1300201, 1300607, 1300706, 1301407, 1301506, 1301654, 1301803, 1301951,
  1302306, 1302405, 1303502, 1303908, 1304062
)


# Storing results
list <- list()

# ---------------------------------------------------------------------------- #
### 1.2.1 2018-2017 ----
#### 1.2.1.1 Data ----
# ---------------------------------------------------------------------------- #

# ---------- #
#Res
# ---------- #


vars_diff <- c(
  "media",
  "escm", "escp", "pessoa", "empr_dom",
  "n_ban", "n_qua", "n_car", "n_gel", "n_cel",
  "pc", "internet", "renda1", "renda110", "renda10",
  "pibpc",
  "fem", "idade", "ppi",
  "tempd1", "tempd2", "umidd2", "umidd1"
)

base_a <- base[priv0 == 1, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  h10 = first(h10),
  obs = .N
), by = .(mun_prova, ano, dist_hv_border, seg, lat, lon)] %>%
  filter(as.numeric(ano) %in% c(2017, 2018)) %>%
  arrange(mun_prova, ano) %>%
  group_by(mun_prova) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_a)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_a[[v1]] <- ifelse(base_a$ano == 2017, base_a[[v]], NA_real_)
  
  base_a[[v2]] <- ave(
    base_a[[v1]], 
    base_a$mun_prova, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_a[[dv]] <- base_a[[v]] - base_a[[v2]]
  
  base_a[[dv]][!is.finite(base_a[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_a), value = TRUE)
base_a <- base_a %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia) %>% 
  mutate(
    aux_uf = mun_prova %/% 100000,
    
    # Exam start time dummies
    h13 = ifelse(
      aux_uf %in% c(
        52, 53, 31, 32, 33, 35, 42, 41, 43,
        29, 28, 27, 26, 25, 24, 23, 22, 21, 17, 15, 16
      ),
      1,0
    ),
    
    h12 = ifelse(
      aux_uf %in% c(51, 50, 11, 14) |
        (aux_uf == 13 & !mun_prova %in% am_mun_special),
      1, 0
    ),
    
    h11 = ifelse(
      
      aux_uf == 12 |
        mun_prova %in% am_mun_special,
      1, 0
    )
  ) %>% 
  select(-aux_uf)


# --------- #
# School 
# --------- #

base_esc <- base[priv0 == 1, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  h10 = first(h10),
  obs = .N
), by = .(mun_escola, ano, dist_hv_esc, seg_esc, lat_esc, lon_esc)] %>%
  filter(ano %in% c(2017, 2018)) %>%
  group_by(mun_escola, dist_hv_esc, seg_esc, lat_esc, lon_esc) %>%
  filter(n_distinct(ano) == 2, n() == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_esc)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_esc[[v1]] <- ifelse(base_esc$ano == 2017, base_esc[[v]], NA_real_)
  
  base_esc[[v2]] <- ave(
    base_esc[[v1]], 
    base_esc$mun_escola, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_esc[[dv]] <- base_esc[[v]] - base_esc[[v2]]
  
  base_esc[[dv]][!is.finite(base_esc[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_esc), value = TRUE)
base_esc <- base_esc %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia) %>% 
  mutate(
    aux_uf = mun_escola %/% 100000,
    
    # Exam start time dummies
    h13 = ifelse(
      aux_uf %in% c(
        52, 53, 31, 32, 33, 35, 42, 41, 43,
        29, 28, 27, 26, 25, 24, 23, 22, 21, 17, 15, 16
      ),
      1,0
    ),
    
    h12 = ifelse(
      aux_uf %in% c(51, 50, 11, 14) |
        (aux_uf == 13 & !mun_escola %in% am_mun_special),
      1, 0
    ),
    
    h11 = ifelse(
      
      aux_uf == 12 |
        mun_escola %in% am_mun_special,
      1, 0
    )
  ) %>% 
  select(-aux_uf)

summary(base_esc)

# ---------------------------------------------------------------------------- #
#### 1.2.2.2 Regression ----
# ---------------------------------------------------------------------------- #

# ----------- #
# --- Res ---
# ----------- #
#Controls
ef <- dummy_cols(base_a$seg[base_a$ano == 2017])
ef <- ef %>% select(-1,-2)

#without
list[[as.character(paste0(2018,"-",2017,"prova|0"))]] <- rdrobust(
  y = base_a$d.media[base_a$ano == 2018],
  x = base_a$dist_hv_border[base_a$ano == 2017],
  c = 0,
  cluster = base_a$seg[base_a$ano == 2017],
  weights = base_a$obs[base_a$ano == 2017],
  vce = "hc0",
  covs = cbind(
    ef,
    base_a$lat[base_a$ano == 2017],
    base_a$lon[base_a$ano == 2017],
    
    #Weather
    base_a$dtempd1[base_a$ano == 2018], #Temperature
    base_a$dumidd1[base_a$ano == 2018], #Humidity d1
    base_a$dumidd2[base_a$ano == 2018], #Humidty d2
    base_a$dtempd2[base_a$ano == 2018], #temp2
    
    #House
    base_a$dn_ban[base_a$ano == 2018], #bathrooms
    base_a$dpessoa[base_a$ano == 2018], #people in household
    base_a$dn_qua[base_a$ano == 2018], #houses
    base_a$dn_car[base_a$ano == 2018], #cars
    base_a$dn_gel[base_a$ano == 2018], #geladeira
    base_a$dn_cel[base_a$ano == 2018], #cel
    base_a$dpc[base_a$ano == 2018],    #pc
    base_a$dinternet[base_a$ano == 2018], #internet
    base_a$dempr_dom[base_a$ano == 2018], #Housekeeping
    
    #remaining
    base_a$descm[base_a$ano == 2018], #mother educ
    base_a$dfem[base_a$ano == 2018], #Female
    base_a$dppi[base_a$ano == 2018], #PPI
    base_a$didade[base_a$ano == 2018], #Age
    base_a$descp[base_a$ano == 2018], #father educ
    base_a$drenda1[base_a$ano == 2018], #wage < 1MW
    base_a$drenda110[base_a$ano == 2018], #wage 1MW - 10MW
    base_a$drenda10[base_a$ano == 2018], #wage > 10MW
    base_a$dpibpc[base_a$ano == 2018], #pibpc
    
    #Fuso
    base_a$h13[base_a$ano == 2018],
    base_a$h12[base_a$ano == 2018],
    base_a$h11[base_a$ano == 2018]
  )
)

# ----------- #
# --- Esc ---
# ----------- #

#Controls
ef <- dummy_cols(base_esc$seg_esc[base_esc$ano == 2017])
ef <- ef %>% select(-1,-2)



#without
list[[as.character(paste0(2018,"-",2017,"esc|0"))]] <- rdrobust(
  y = base_esc$d.media[base_esc$ano == 2018],
  x = base_esc$dist_hv_esc[base_esc$ano == 2017],
  c = 0,
  cluster = base_esc$seg_esc[base_esc$ano == 2017],
  weights = base_esc$obs[base_esc$ano == 2017],
  vce = "hc0",
  covs = cbind(
    ef,
    base_esc$lat_esc[base_esc$ano == 2017],
    base_esc$lon_esc[base_esc$ano == 2017],
    
    #Weather
    base_esc$dtempd1[base_esc$ano == 2018], #Temperature
    base_esc$dumidd1[base_esc$ano == 2018], #Humidity d1
    base_esc$dumidd2[base_esc$ano == 2018], #Humidty d2
    base_esc$dtempd2[base_esc$ano == 2018], #temp2
    
    #House
    base_esc$dn_ban[base_esc$ano == 2018], #bathrooms
    base_esc$dpessoa[base_esc$ano == 2018], #people in household
    base_esc$dn_qua[base_esc$ano == 2018], #houses
    base_esc$dn_car[base_esc$ano == 2018], #cars
    base_esc$dn_gel[base_esc$ano == 2018], #geladeira
    base_esc$dn_cel[base_esc$ano == 2018], #cel
    base_esc$dpc[base_esc$ano == 2018],    #pc
    base_esc$dinternet[base_esc$ano == 2018], #internet
    base_esc$dempr_dom[base_esc$ano == 2018], #Housekeeping
    
    #remaining
    base_esc$descm[base_esc$ano == 2018], #mother educ
    base_esc$dfem[base_esc$ano == 2018], #Female
    base_esc$dppi[base_esc$ano == 2018], #PPI
    base_esc$didade[base_esc$ano == 2018], #Age
    base_esc$descp[base_esc$ano == 2018], #father educ
    base_esc$drenda1[base_esc$ano == 2018], #wage < 1MW
    base_esc$drenda110[base_esc$ano == 2018], #wage 1MW - 10MW
    base_esc$drenda10[base_esc$ano == 2018], #wage > 10MW
    base_esc$dpibpc[base_esc$ano == 2018], #pibpc
    
    #Fuso
    base_esc$h13[base_esc$ano == 2018],
    base_esc$h12[base_esc$ano == 2018],
    base_esc$h11[base_esc$ano == 2018]
  )
)




# ---------------------------------------------------------------------------- #
### 1.2.4 Main Table Reprod. ----
# ---------------------------------------------------------------------------- #
#### 1.2.4.1 Data ----
# ---------------------------------------------------------------------------- #

# ---------- #
#Prova
# ---------- #
base_a <- base[priv0 == 1, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  h10 = first(h10),
  obs = .N
), by = .(mun_prova, ano, dist_hv_border, seg, lat, lon)] %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_prova, ano) %>%
  group_by(mun_prova) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_a)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_a[[v1]] <- ifelse(base_a$ano == 2018, base_a[[v]], NA_real_)
  
  base_a[[v2]] <- ave(
    base_a[[v1]], 
    base_a$mun_prova, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_a[[dv]] <- base_a[[v]] - base_a[[v2]]
  
  base_a[[dv]][!is.finite(base_a[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_a), value = TRUE)
base_a <- base_a %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia)

# --------- #
# School 
# --------- #



base_esc <- base[priv0 == 1, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  h10 = first(h10),
  obs = .N
), by = .(mun_escola, ano, dist_hv_esc, seg_esc, lat_esc, lon_esc)] %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_escola, ano) %>%
  group_by(mun_escola) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_esc)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_esc[[v1]] <- ifelse(base_esc$ano == 2018, base_esc[[v]], NA_real_)
  
  base_esc[[v2]] <- ave(
    base_esc[[v1]], 
    base_esc$mun_escola, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_esc[[dv]] <- base_esc[[v]] - base_esc[[v2]]
  
  base_esc[[dv]][!is.finite(base_esc[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_esc), value = TRUE)
base_esc <- base_esc %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia)

# base_esc <- base_esc %>% 
#   group_by(mun_escola) %>% 
#   filter(is.finite(descm) & !is.na(descm)) %>% 
#   filter(n_distinct(ano) == 2) %>% 
#   ungroup()


# ---------------------------------------------------------------------------- #
#### 1.2.4.2 EXAM ----
# ---------------------------------------------------------------------------- #
##### 1.2.4.2.1 Regression ----
# ---------------------------------------------------------------------------- #


ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
ef <- ef %>% select(-1,-2)



# ---------------------------------------------------------------------------- #
### All
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2019,"-",2018,"|all"))]] <- rdrobust(
  y = base_a$d.media[base_a$ano == 2019],
  x = base_a$dist_hv_border[base_a$ano == 2018],
  c = 0,
  p = 1,
  cluster = base_a$seg[base_a$ano == 2018],
  weights = base_a$obs[base_a$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_a$lat[base_a$ano == 2018],
    base_a$lon[base_a$ano == 2018],
    #Weather
    base_a$dtempd1[base_a$ano == 2019], #Temperature
    base_a$dumidd1[base_a$ano == 2019], #Humidity d1
    base_a$dumidd2[base_a$ano == 2019], #Humidty d2
    base_a$dtempd2[base_a$ano == 2019], #temp2
    
    #House
    base_a$dn_ban[base_a$ano == 2019], #bathrooms
    base_a$dpessoa[base_a$ano == 2019], #people in household
    base_a$dn_qua[base_a$ano == 2019], #houses
    base_a$dn_car[base_a$ano == 2019], #cars
    base_a$dn_gel[base_a$ano == 2019], #geladeira
    base_a$dn_cel[base_a$ano == 2019], #cel
    base_a$dpc[base_a$ano == 2019],    #pc
    base_a$dinternet[base_a$ano == 2019], #internet
    base_a$dempr_dom[base_a$ano == 2019], #Housekeeping
    
    #remaining
    base_a$descm[base_a$ano == 2019], #mother educ
    base_a$dfem[base_a$ano == 2019], #Female
    base_a$dppi[base_a$ano == 2019], #PPI
    base_a$didade[base_a$ano == 2019], #Age
    base_a$descp[base_a$ano == 2019], #father educ
    base_a$drenda1[base_a$ano == 2019], #wage < 1MW
    base_a$drenda110[base_a$ano == 2019], #wage 1MW - 10MW
    base_a$drenda10[base_a$ano == 2019], #wage > 10MW
    base_a$dpibpc[base_a$ano == 2019], #pibpc
    
    #Fuso
    base_a$h13[base_a$ano == 2019],
    base_a$h12[base_a$ano == 2019],
    base_a$h11[base_a$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
# Pol + All
# ---------------------------------------------------------------------------- #


list[[as.character(paste0(2019,"-",2018,"|all+pol"))]] <- rdrobust(
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
    #Weather
    base_a$dtempd1[base_a$ano == 2019], #Temperature
    base_a$dumidd1[base_a$ano == 2019], #Humidity d1
    base_a$dumidd2[base_a$ano == 2019], #Humidty d2
    base_a$dtempd2[base_a$ano == 2019], #temp2
    
    #House
    base_a$dn_ban[base_a$ano == 2019], #bathrooms
    base_a$dpessoa[base_a$ano == 2019], #people in household
    base_a$dn_qua[base_a$ano == 2019], #houses
    base_a$dn_car[base_a$ano == 2019], #cars
    base_a$dn_gel[base_a$ano == 2019], #geladeira
    base_a$dn_cel[base_a$ano == 2019], #cel
    base_a$dpc[base_a$ano == 2019],    #pc
    base_a$dinternet[base_a$ano == 2019], #internet
    base_a$dempr_dom[base_a$ano == 2019], #Housekeeping
    
    #remaining
    base_a$descm[base_a$ano == 2019], #mother educ
    base_a$dfem[base_a$ano == 2019], #Female
    base_a$dppi[base_a$ano == 2019], #PPI
    base_a$didade[base_a$ano == 2019], #Age
    base_a$descp[base_a$ano == 2019], #father educ
    base_a$drenda1[base_a$ano == 2019], #wage < 1MW
    base_a$drenda110[base_a$ano == 2019], #wage 1MW - 10MW
    base_a$drenda10[base_a$ano == 2019], #wage > 10MW
    base_a$dpibpc[base_a$ano == 2019], #pibpc
    
    #Fuso
    base_a$h13[base_a$ano == 2019],
    base_a$h12[base_a$ano == 2019],
    base_a$h11[base_a$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
##### 1.2.4.2.2 Table ----- 
# ---------------------------------------------------------------------------- #


t10 <- data.frame(
  coef   = sapply(list, function(x) x$coef[3]),
  se     = sapply(list, function(x) x$se[3]),
  pv     = sapply(list, function(x) x$pv[3]),
  n_left = sapply(list, function(x) x$N_h[1]),
  n_rght = sapply(list, function(x) x$N_h[2]),
  bw     = sapply(list, function(x) x$bws[1, 1]),
  totr   = sapply(list, function(x) x$N[2]),
  totl   = sapply(list, function(x) x$N[1])
)
print(t10)


# ---------------------------------------------------------------------------- #
# Build final AER-style table
# ---------------------------------------------------------------------------- #

result <- data.frame(
  ` ` = c(
    "ENEM",
    " ",
    "N = N$_L$, N$_R$",
    "BW"
  ),
  `(1)` = c( #2018 - 2017
    fmt_est(t10$coef[1], t10$pv[1]),
    fmt_se(t10$se[1]),
    fmt_npair(t10$n_left[1], t10$n_rght[1]),
    fmt_bw(t10$bw[1])
  ),
  `(2)` = c( #2019 - 2018 all
    fmt_est(t10$coef[3], t10$pv[3]),
    fmt_se(t10$se[3]),
    fmt_npair(t10$n_left[3], t10$n_rght[3]),
    fmt_bw(t10$bw[3])
  ),
  `(3)` = c( #1918 all pol2
    fmt_est(t10$coef[4], t10$pv[4]),
    fmt_se(t10$se[4]),
    fmt_npair(t10$n_left[4], t10$n_rght[4]),
    fmt_bw(t10$bw[4])
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------- #
# Latex
# ---------------------------------------------------------------------------- #

latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lccccc",
  linesep = "",
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/prova_principal.tex")
rm(ef, result, t10, latex_table)

# ---------------------------------------------------------------------------- #
#### 1.2.4.3 School ----
# ---------------------------------------------------------------------------- #
##### 1.2.4.3.1 Regression ----
# ---------------------------------------------------------------------------- #
### Controles
ef <- dummy_cols(base_esc$seg_esc[base_esc$ano == 2018])
ef <- ef %>% select(-1,-2)


# ---------------------------------------------------------------------------- #
### All
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2019,"-",2018,"esc|all"))]] <- rdrobust(
  y = base_esc$d.media[base_esc$ano == 2019],
  x = base_esc$dist_hv_esc[base_esc$ano == 2018],
  c = 0,
  p = 1,
  cluster = base_esc$seg_esc[base_esc$ano == 2018],
  weights = base_esc$obs[base_esc$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_esc$lat_esc[base_esc$ano == 2018],
    base_esc$lon_esc[base_esc$ano == 2018],
    #Weather
    base_esc$dtempd1[base_esc$ano == 2019], #Temperature
    base_esc$dumidd1[base_esc$ano == 2019], #Humidity d1
    base_esc$dumidd2[base_esc$ano == 2019], #Humidty d2
    base_esc$dtempd2[base_esc$ano == 2019], #temp2
    
    #House
    base_esc$dn_ban[base_esc$ano == 2019], #bathrooms
    base_esc$dpessoa[base_esc$ano == 2019], #people in household
    base_esc$dn_qua[base_esc$ano == 2019], #houses
    base_esc$dn_car[base_esc$ano == 2019], #cars
    base_esc$dn_gel[base_esc$ano == 2019], #geladeira
    base_esc$dn_cel[base_esc$ano == 2019], #cel
    base_esc$dpc[base_esc$ano == 2019],    #pc
    base_esc$dinternet[base_esc$ano == 2019], #internet
    base_esc$dempr_dom[base_esc$ano == 2019], #Housekeeping
    
    #remaining
    base_esc$descm[base_esc$ano == 2019], #mother educ
    base_esc$dfem[base_esc$ano == 2019], #Female
    base_esc$dppi[base_esc$ano == 2019], #PPI
    base_esc$didade[base_esc$ano == 2019], #Age
    base_esc$descp[base_esc$ano == 2019], #father educ
    base_esc$drenda1[base_esc$ano == 2019], #wage < 1MW
    base_esc$drenda110[base_esc$ano == 2019], #wage 1MW - 10MW
    base_esc$drenda10[base_esc$ano == 2019], #wage > 10MW
    base_esc$dpibpc[base_esc$ano == 2019], #pibpc
    
    #Fuso
    base_esc$h13[base_esc$ano == 2019],
    base_esc$h12[base_esc$ano == 2019],
    base_esc$h11[base_esc$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
# Pol + All
# ---------------------------------------------------------------------------- #


list[[as.character(paste0(2019,"-",2018,"esc|all+pol"))]] <- rdrobust(
  y = base_esc$d.media[base_esc$ano == 2019],
  x = base_esc$dist_hv_esc[base_esc$ano == 2018],
  c = 0,
  p = 2,
  cluster = base_esc$seg_esc[base_esc$ano == 2018],
  weights = base_esc$obs[base_esc$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_esc$lat_esc[base_esc$ano == 2018],
    base_esc$lon_esc[base_esc$ano == 2018],
    #Weather
    base_esc$dtempd1[base_esc$ano == 2019], #Temperature
    base_esc$dumidd1[base_esc$ano == 2019], #Humidity d1
    base_esc$dumidd2[base_esc$ano == 2019], #Humidty d2
    base_esc$dtempd2[base_esc$ano == 2019], #temp2
    
    #House
    base_esc$dn_ban[base_esc$ano == 2019], #bathrooms
    base_esc$dpessoa[base_esc$ano == 2019], #people in household
    base_esc$dn_qua[base_esc$ano == 2019], #houses
    base_esc$dn_car[base_esc$ano == 2019], #cars
    base_esc$dn_gel[base_esc$ano == 2019], #geladeira
    base_esc$dn_cel[base_esc$ano == 2019], #cel
    base_esc$dpc[base_esc$ano == 2019],    #pc
    base_esc$dinternet[base_esc$ano == 2019], #internet
    base_esc$dempr_dom[base_esc$ano == 2019], #Housekeeping
    
    #remaining
    base_esc$descm[base_esc$ano == 2019], #mother educ
    base_esc$dfem[base_esc$ano == 2019], #Female
    base_esc$dppi[base_esc$ano == 2019], #PPI
    base_esc$didade[base_esc$ano == 2019], #Age
    base_esc$descp[base_esc$ano == 2019], #father educ
    base_esc$drenda1[base_esc$ano == 2019], #wage < 1MW
    base_esc$drenda110[base_esc$ano == 2019], #wage 1MW - 10MW
    base_esc$drenda10[base_esc$ano == 2019], #wage > 10MW
    base_esc$dpibpc[base_esc$ano == 2019], #pibpc
    
    #Fuso
    base_esc$h13[base_esc$ano == 2019],
    base_esc$h12[base_esc$ano == 2019],
    base_esc$h11[base_esc$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
##### 1.2.4.3.2 Table ----- 
# ---------------------------------------------------------------------------- #
t10 <- data.frame(
  coef   = sapply(list, function(x) x$coef[3]),
  se     = sapply(list, function(x) x$se[3]),
  pv     = sapply(list, function(x) x$pv[3]),
  n_left = sapply(list, function(x) x$N_h[1]),
  n_rght = sapply(list, function(x) x$N_h[2]),
  bw     = sapply(list, function(x) x$bws[1, 1]),
  totr   = sapply(list, function(x) x$N[2]),
  totl   = sapply(list, function(x) x$N[1])
)
print(t10)


# ---------------------------------------------------------------------------- #
# Build final AER-style table
# ---------------------------------------------------------------------------- #

result <- data.frame(
  ` ` = c(
    "School",
    " ",
    "N = N$_L$, N$_R$",
    "BW"
  ),
  `(1)` = c( #2018 - 2017
    fmt_est(t10$coef[2], t10$pv[2]),
    fmt_se(t10$se[2]),
    fmt_npair(t10$n_left[2], t10$n_rght[2]),
    fmt_bw(t10$bw[2])
  ),
  `(2)` = c(
    fmt_est(t10$coef[5], t10$pv[5]),
    fmt_se(t10$se[5]),
    fmt_npair(t10$n_left[5], t10$n_rght[5]),
    fmt_bw(t10$bw[5])
  ),
  `(3)` = c(
    fmt_est(t10$coef[6], t10$pv[6]),
    fmt_se(t10$se[6]),
    fmt_npair(t10$n_left[6], t10$n_rght[6]),
    fmt_bw(t10$bw[6])
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------- #
# Latex
# ---------------------------------------------------------------------------- #

latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lccccc",
  linesep = "",
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/esc_principal.tex")

rm(ef, list, result, t10, latex_table)
rm(base_esc, base_a, bw_main_a, bw_bias_a)

# ---------------------------------------------------------------------------- #
## 1.3 Main 2018-2017 ----
# ---------------------------------------------------------------------------- #


vars_diff <- c(
  "media",
  "escm", "escp", "pessoa", "empr_dom",
  "n_ban", "n_qua", "n_car", "n_gel", "n_cel",
  "pc", "internet", "renda1", "renda110", "renda10",
  "pibpc",
  "fem", "idade", "ppi",
  "tempd1", "tempd2", "umidd2", "umidd1"
)

base_res <- base[priv0 == 1, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  h10 = first(h10),
  obs = .N
), by = .(mun_res, ano, dist_hv_res, seg_res, lat_res, lon_res)] %>%
  filter(as.numeric(ano) %in% c(2017, 2018)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup() 


for (v in vars_diff) {
  
  if (!v %in% names(base_res)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_res[[v1]] <- ifelse(base_res$ano == 2017, base_res[[v]], NA_real_)
  
  base_res[[v2]] <- ave(
    base_res[[v1]], 
    base_res$mun_res, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_res[[dv]] <- base_res[[v]] - base_res[[v2]]
  
  base_res[[dv]][!is.finite(base_res[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_res), value = TRUE)
base_res <- base_res %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia) %>% 
  mutate(
    aux_uf = mun_res %/% 100000,
    
    # Exam start time dummies
    h13 = ifelse(
      aux_uf %in% c(
        52, 53, 31, 32, 33, 35, 42, 41, 43,
        29, 28, 27, 26, 25, 24, 23, 22, 21, 17, 15, 16
      ),
      1,0
    ),
    
    h12 = ifelse(
      aux_uf %in% c(51, 50, 11, 14) |
        (aux_uf == 13 & !mun_res %in% am_mun_special),
      1, 0
    ),
    
    h11 = ifelse(
      
      aux_uf == 12 |
        mun_res %in% am_mun_special,
      1, 0
    )
  ) %>% 
  select(-aux_uf)



# 
# # ---------- #
# #Res
# # ---------- #
# base_res <- base[priv0 == 1,.(media    = mean(media, na.rm = T),
#                               escm     = mean(escm, na.rm = T),
#                               fem      = mean(fem, na.rm = T),
#                               idade    = mean(id18, na.rm = T),
#                               tempd1   = mean(temp_d1, na.rm = T),
#                               umidd2   = mean(umid_d2, na.rm = T),
#                               umidd1   = mean(umid_d1, na.rm = T),
#                               h13 = first(h13),
#                               h12 = first(h12),
#                               h11 = first(h11),
#                               h10 = first(h10),
#                               obs = .N),
#                  by = .(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res)] %>% 
#   filter(as.numeric(ano) %in% c(2018,2017)) %>% 
#   ungroup() %>% 
#   arrange(mun_res,ano) %>%
#   group_by(mun_res) %>%
#   #filter(!is.na(escm)) %>% 
#   mutate(
#     dup1 = 1,
#     dup2 = sum(dup1),
#     v1_nota = ifelse(ano == 2017, media, NA),
#     v2_nota = max(v1_nota, na.rm = T),
#     d.media = media - v2_nota
#     
#   ) %>%
#   ungroup() %>% 
#   filter(dup2 == 2) %>% 
#   select(-c(dup2, dup1, v1_nota, v2_nota)) 




# ---------------------------------------------------------------------------- #
### 1.4.1 Main Regression ----
# ---------------------------------------------------------------------------- #

list <- list()

efr <- dummy_cols(base_res$seg_res[base_res$ano == 2017])
efr <- efr %>% select(-1,-2)

### Main 
list[[as.character(paste0(2018,"-",2017,"|fuso"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2018],
  x = base_res$dist_hv_res[base_res$ano == 2017],
  c = 0,
  cluster = base_res$seg_res[base_res$ano == 2017],
  weights = base_res$obs[base_res$ano == 2017],
  vce = "hc0",
  covs = cbind(
    efr,
    base_res$lat_res[base_res$ano == 2017],
    base_res$lon_res[base_res$ano == 2017],
    base_res$h13[base_res$ano == 2018],
    base_res$h12[base_res$ano == 2018],
    base_res$h11[base_res$ano == 2018]
  )
)


# ---------------------------------------------------------------------------- #
### 1.4.2 Temperature ----
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2018,"-",2017,"|dem"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2018],
  x = base_res$dist_hv_res[base_res$ano == 2017],
  c = 0,
  cluster = base_res$seg_res[base_res$ano == 2017],
  weights = base_res$obs[base_res$ano == 2017],
  vce = "hc0",
  covs = cbind(
    efr,
    base_res$lat_res[base_res$ano == 2017],
    base_res$lon_res[base_res$ano == 2017],
    base_res$h13[base_res$ano == 2018],
    base_res$h12[base_res$ano == 2018],
    base_res$h11[base_res$ano == 2018],
    #Dists
    #Parents and Individuals Characteristics
    base_res$descm[base_res$ano == 2018], #mother educ
    base_res$dfem[base_res$ano == 2018], #Female
    base_res$dppi[base_res$ano == 2018], #PPI
    base_res$didade[base_res$ano == 2018], #Age
    base_res$descp[base_res$ano == 2018], #father educ
    
    base_res$dtempd1[base_res$ano == 2018], #Temperature
    base_res$dumidd1[base_res$ano == 2018], #Humidity d1
    base_res$dumidd2[base_res$ano == 2018], #Humidty d2
    base_res$dtempd2[base_res$ano == 2018] #temp2
  )
)

# ---------------------------------------------------------------------------- #
### 1.4.3 Fuso + Temp + Socio ----
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2018,"-",2017,"|fuso+temp+house"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2018],
  x = base_res$dist_hv_res[base_res$ano == 2017],
  c = 0,
  cluster = base_res$seg_res[base_res$ano == 2017],
  weights = base_res$obs[base_res$ano == 2017],
  vce = "hc0",
  covs = cbind(
    efr,
    base_res$lat_res[base_res$ano == 2017],
    base_res$lon_res[base_res$ano == 2017],
    #Weather
    base_res$dtempd1[base_res$ano == 2018], #Temperature
    base_res$dumidd1[base_res$ano == 2018], #Humidity d1
    base_res$dumidd2[base_res$ano == 2018], #Humidty d2
    base_res$dtempd2[base_res$ano == 2018], #temp2
    
    #Parents and Individuals Characteristics
    base_res$descm[base_res$ano == 2018], #mother educ
    base_res$dfem[base_res$ano == 2018], #Female
    base_res$dppi[base_res$ano == 2018], #PPI
    base_res$didade[base_res$ano == 2018], #Age
    base_res$descp[base_res$ano == 2018], #father educ
    #House
    base_res$dn_ban[base_res$ano == 2018], #bathrooms
    base_res$dpessoa[base_res$ano == 2018], #people in household
    base_res$dn_qua[base_res$ano == 2018], #houses
    base_res$dn_car[base_res$ano == 2018], #cars
    base_res$dn_gel[base_res$ano == 2018], #geladeira
    base_res$dn_cel[base_res$ano == 2018], #cel
    base_res$dpc[base_res$ano == 2018],    #pc
    base_res$dinternet[base_res$ano == 2018], #internet
    base_res$dempr_dom[base_res$ano == 2018], #Housekeeping
    
    #Timezones
    base_res$h13[base_res$ano == 2018],
    base_res$h12[base_res$ano == 2018],
    base_res$h11[base_res$ano == 2018]
  )
)

# ---------------------------------------------------------------------------- #
### 1.1.4 All ----
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2018,"-",2017,"|all"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2018],
  x = base_res$dist_hv_res[base_res$ano == 2017],
  c = 0,
  p = 1, 
  cluster = base_res$seg_res[base_res$ano == 2017],
  weights = base_res$obs[base_res$ano == 2017],
  vce = "hc0",
  covs = cbind(
    efr,
    base_res$lat_res[base_res$ano == 2017],
    base_res$lon_res[base_res$ano == 2017],
    #Weather
    base_res$dtempd1[base_res$ano == 2018], #Temperature
    base_res$dumidd1[base_res$ano == 2018], #Humidity d1
    base_res$dumidd2[base_res$ano == 2018], #Humidty d2
    base_res$dtempd2[base_res$ano == 2018], #temp2
    
    #Parents and Individuals Characteristics
    base_res$descm[base_res$ano == 2018], #mother educ
    base_res$dfem[base_res$ano == 2018], #Female
    base_res$dppi[base_res$ano == 2018], #PPI
    base_res$didade[base_res$ano == 2018], #Age
    base_res$descp[base_res$ano == 2018], #father educ
    #House
    base_res$dn_ban[base_res$ano == 2018], #bathrooms
    base_res$dpessoa[base_res$ano == 2018], #people in household
    base_res$dn_qua[base_res$ano == 2018], #houses
    base_res$dn_car[base_res$ano == 2018], #cars
    base_res$dn_gel[base_res$ano == 2018], #geladeira
    base_res$dn_cel[base_res$ano == 2018], #cel
    base_res$dpc[base_res$ano == 2018],    #pc
    base_res$dinternet[base_res$ano == 2018], #internet
    base_res$dempr_dom[base_res$ano == 2018], #Housekeeping
    
    #Timezones
    base_res$h13[base_res$ano == 2018],
    base_res$h12[base_res$ano == 2018],
    base_res$h11[base_res$ano == 2018]
  )
)

# ---------------------------------------------------------------------------- #
### 1.4.5 Pol + All ----
# ---------------------------------------------------------------------------- #

list[[as.character(paste0(2018,"-",2017,"|pol+all"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2018],
  x = base_res$dist_hv_res[base_res$ano == 2017],
  c = 0,
  p = 2,
  cluster = base_res$seg_res[base_res$ano == 2017],
  weights = base_res$obs[base_res$ano == 2017],
  vce = "hc0",
  covs = cbind(
    efr,
    base_res$lat_res[base_res$ano == 2017],
    base_res$lon_res[base_res$ano == 2017],
    
    #Weather
    base_res$dtempd1[base_res$ano == 2018], #Temperature
    base_res$dumidd1[base_res$ano == 2018], #Humidity d1
    base_res$dumidd2[base_res$ano == 2018], #Humidty d2
    base_res$dtempd2[base_res$ano == 2018], #temp2
    
    #House
    base_res$dn_ban[base_res$ano == 2018], #bathrooms
    base_res$dpessoa[base_res$ano == 2018], #people in household
    base_res$dn_qua[base_res$ano == 2018], #houses
    base_res$dn_car[base_res$ano == 2018], #cars
    base_res$dn_gel[base_res$ano == 2018], #geladeira
    base_res$dn_cel[base_res$ano == 2018], #cel
    base_res$dpc[base_res$ano == 2018],    #pc
    base_res$dinternet[base_res$ano == 2018], #internet
    base_res$dempr_dom[base_res$ano == 2018], #Housekeeping
    
    #remaining
    base_res$descm[base_res$ano == 2018], #mother educ
    base_res$dfem[base_res$ano == 2018], #Female
    base_res$dppi[base_res$ano == 2018], #PPI
    base_res$didade[base_res$ano == 2018], #Age
    base_res$descp[base_res$ano == 2018], #father educ
    base_res$drenda1[base_res$ano == 2018], #wage < 1MW
    base_res$drenda110[base_res$ano == 2018], #wage 1MW - 10MW
    base_res$drenda10[base_res$ano == 2018], #wage > 10MW
    base_res$dpibpc[base_res$ano == 2018], #pibpc
    
    #Fuso
    base_res$h13[base_res$ano == 2018],
    base_res$h12[base_res$ano == 2018],
    base_res$h11[base_res$ano == 2018]
  )
)

# ---------------------------------------------------------------------------- #
### 1.4.6 Tabela  ----
# ---------------------------------------------------------------------------- #

t10 <- data.frame(
  coef   = sapply(list, function(x) x$coef[3]),
  se     = sapply(list, function(x) x$se[3]),
  pv     = sapply(list, function(x) x$pv[3]),
  n_left = sapply(list, function(x) x$N_h[1]),
  n_rght = sapply(list, function(x) x$N_h[2]),
  bw     = sapply(list, function(x) x$bws[1, 1]),
  totr   = sapply(list, function(x) x$N[2]),
  totl   = sapply(list, function(x) x$N[1])
)
print(t10)


# ---------------------------------------------------------------------------- #
# Build final AER-style table
# ---------------------------------------------------------------------------- #

result <- data.frame(
  ` ` = c(
    "2018 - 2017",
    " ",
    "N = N$_L$, N$_R$",
    "BW",
    "Time Zones",
    "All Controls"
  ),
  `(1)` = c(
    fmt_est(t10$coef[1], t10$pv[1]),
    fmt_se(t10$se[1]),
    fmt_npair(t10$n_left[1], t10$n_rght[1]),
    fmt_bw(t10$bw[1]),
    "No",
    "No"
  ),
  `(2)` = c(
    fmt_est(t10$coef[2], t10$pv[2]),
    fmt_se(t10$se[2]),
    fmt_npair(t10$n_left[2], t10$n_rght[2]),
    fmt_bw(t10$bw[2]),
    "Yes",
    "No"
  ),
  `(3)` = c(
    fmt_est(t10$coef[3], t10$pv[3]),
    fmt_se(t10$se[3]),
    fmt_npair(t10$n_left[3], t10$n_rght[3]),
    fmt_bw(t10$bw[3]),
    "Yes",
    "Yes"
  ),
  `(4)` = c(
    fmt_est(t10$coef[4], t10$pv[4]),
    fmt_se(t10$se[4]),
    fmt_npair(t10$n_left[4], t10$n_rght[4]),
    fmt_bw(t10$bw[4]),
    "Yes",
    "No"
  ),
  `(5)` = c(
    fmt_est(t10$coef[5], t10$pv[5]),
    fmt_se(t10$se[5]),
    fmt_npair(t10$n_left[5], t10$n_rght[5]),
    fmt_bw(t10$bw[5]),
    "Yes",
    "Yes"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------- #
# Latex
# ---------------------------------------------------------------------------- #

latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lccccc",
  linesep = "",
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/DIFF_Principal_1817.tex")

rm(latex_table, t10, result, list, vars_diff, dv, v, temp_cols, v1, v2)

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
    
    aux_res = mun_res %/% 100000,
    
    time13 = case_when(
      # States that follow UTC-3 (Brasília time)
      aux_res %in% c(20:49, 17, 15, 16, 52, 53) ~ 1,
      
      # States that are mostly UTC-4 (with AM exceptions listed below)
      aux_res %in% c(51, 50, 11, 14) | aux_res == 13 &
        !mun_res %in%
        c(1300201, 1300607, 1300706, 1301407, 1301506, 1301654, 1301803, 1301951,
          1302306, 1302405, 1303502, 1303908, 1304062) ~ 0,
      
      # Otherwise: set NA (investigate if many NAs appear)
      TRUE ~ NA_real_
    )
  ) %>%
  select(-aux_res) %>% 
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
  
  group_by(mun_res, ano, dist_hv_res) %>% 
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
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
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
  group_by(mun_res) %>%
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
  group_by(mun_res, ano, dist_hv_res) %>%
  summarise(media = mean(media, na.rm = TRUE), .groups = "drop") %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
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
  group_by(mun_res, ano, dist_hv_res) %>%
  summarise(media = mean(media, na.rm = TRUE), .groups = "drop") %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
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
  group_by(mun_res, ano, dist_hv_res) %>%
  summarise(media = mean(media, na.rm = TRUE), .groups = "drop") %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
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
  group_by(mun_res, ano, dist_hv_res) %>%
  summarise(media = mean(media, na.rm = TRUE), .groups = "drop") %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
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
  left_join(df_escm0 %>% filter(ano == 2019) %>% select(mun_res, dmedia_escm0),
            by = "mun_res") %>%
  left_join(df_escm1 %>% filter(ano == 2019) %>% select(mun_res, dmedia_escm1),
            by = "mun_res") %>%
  left_join(df_time1 %>% filter(ano == 2019) %>% select(mun_res, dmedia_bra1),
            by = "mun_res") %>%
  left_join(df_time0 %>% filter(ano == 2019) %>% select(mun_res, dmedia_bra0),
            by = "mun_res")

# Remove intermediate objects to save memory
rm(df_escm0, df_escm1, df_time0, df_time1)

# ---------------------------------------------------------------------------- #
### 2.1.6 Build alternative control base (placebo) using 2017-2018 — base_c ----
# ---------------------------------------------------------------------------- #
# Purpose: use 2017-2018 as placebo / pre-trend check.
base_c <- base %>%
  filter(priv0 == 1) %>%
  group_by(mun_res, ano, dist_hv_res, seg_res, lat_res, lon_res) %>%
  summarise(media = mean(media, na.rm = TRUE),
            obs   = n(),
            .groups = "drop") %>%
  filter(as.numeric(ano) %in% c(2017, 2018)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
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
  mutate(trat = ifelse(dist_hv_res > 0, 1, 0))

# Attach the 2018-2017 difference to df_cmo for plotting/placebo tables
df_cmo <- df_cmo %>%
  left_join(
    base_c %>% filter(ano == 2018) %>% select(mun_res, d.media) %>% rename(dmedia_2018 = d.media),
    by = "mun_res"
  )

# ---------------------------------------------------------------------------- #
### 2.1.7 Final adjustments and labeling (ready for export) ----
# ---------------------------------------------------------------------------- #
# - Rename some variables for clarity in Stata
# - Create dist_km for plotting in kilometers
# - Drop intermediate columns not needed in final .dta
df_cmo <- df_cmo %>%
  group_by(mun_res) %>% 
  mutate(d.mediabl = ifelse(!is.finite(d.mediabl), NA, d.mediabl)) %>% 
  ungroup() %>% 
  rename(dmedia_easy = d.mediabl,
         dmedia_hard = d.mediabh) 

summary(df_cmo)

df_cmo <- df_cmo %>%
  mutate(dist_km = dist_hv_res / 1000,
         
         main_bandwidth = bw_main_r) %>%
  select(-c(dist_hv_res, obs, obs_r,
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

# ---------------------------------------------------------------------------- #
## 2.2 Within Bandwodth Graph ----
# ---------------------------------------------------------------------------- #
### 2.2.1 1918 ----
# ---------------------------------------------------------------------------- #

base <- base %>% 
  setDT()


vars_diff <- c(
  "media",
  "escm", "escp", "pessoa", "empr_dom",
  "n_ban", "n_qua", "n_car", "n_gel", "n_cel",
  "pc", "internet", "renda1", "renda110", "renda10",
  "pibpc",
  "fem", "idade", "ppi",
  "tempd1", "tempd2", "umidd2", "umidd1"
)

base_res <- base[priv0 == 1, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  h10 = first(h10),
  obs = .N
), by = .(mun_res, ano, dist_hv_res, seg_res, lat_res, lon_res)] %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_res)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_res[[v1]] <- ifelse(base_res$ano == 2018, base_res[[v]], NA_real_)
  
  base_res[[v2]] <- ave(
    base_res[[v1]], 
    base_res$mun_res, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_res[[dv]] <- base_res[[v]] - base_res[[v2]]
  
  base_res[[dv]][!is.finite(base_res[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_res), value = TRUE)
base_res <- base_res %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia)


# ---------------------------------------------------------------------------- #
#### 2.2.1.1 POL 2 ------
# ---------------------------------------------------------------------------- #
plist <- list()

bins <- c(25)

for(j in bins) {
  
  temp <- base_res %>%
    mutate(subset = case_when(
      abs(dist_hv_res) < bw_main_r ~ 1,
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
    select(dist_hv_res)
  
  # Clusters
  clu <- temp %>%
    filter(ano == 2018) %>%
    select(seg_res)
  
  # Latitude
  latv <- temp %>%
    filter(ano == 2018) %>%
    select(lat_res)
  
  # Longitude
  lonv <- temp %>%
    filter(ano == 2018) %>%
    select(lon_res)
  
  #time 
  time <- temp %>% 
    filter(ano == 2019) %>% 
    select(h11, h12, h13)
  
  #all
  all <- temp %>% 
    filter(ano == 2019) %>% 
    select(dtempd1, dtempd2, dumidd1, dumidd2,
           
           dn_ban, dn_qua, dn_car, dn_gel, dn_cel, dpc, dinternet,
           
           dfem, descm, dppi, didade, descp,
           
           drenda1, drenda110, drenda10, dpibpc)
  

  ef <- dummy_cols(clu$seg_res)
  ef <- ef %>% select(-1,-2)
  
  # Estimando parâmetros do gráfico
  plist[[as.character(paste0(j))]] <- rdplot(y = yv$vd,
                                             x = xv$dist_hv_res,
                                             c = 0,
                                             p = 2,
                                             #binselect = "esmv",
                                             kernel = "triangular",
                                             h = bw_main_r,
                                             weights = temp$obs[temp$ano == 2018],
                                             subset = temp$subset == 1,
                                             hide = T,
                                             masspoints= "adjust",
                                             covs = cbind(ef,latv,lonv, time, all),
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
  
  
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/controls/img/",i,"_pol2_1918_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 9, width = 13)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/controls/img/pdf/",i,"_pol2_1918_v1.pdf"),plot = fig_gg, device = "pdf",height = 9, width = 13)
  
  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips, vars_poly_left, vars_poly_right)
  
}

rm(bins, j, plist, fig_loop)
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
### 2.2.2 1817 ----
# ---------------------------------------------------------------------------- #
##### 2.2.2.1 opt bw -----
# ---------------------------------------------------------------------------- #

vars_diff <- c(
  "media",
  "escm", "escp", "pessoa", "empr_dom",
  "n_ban", "n_qua", "n_car", "n_gel", "n_cel",
  "pc", "internet", "renda1", "renda110", "renda10",
  "pibpc",
  "fem", "idade", "ppi",
  "tempd1", "tempd2", "umidd2", "umidd1"
)

base_c <- base[priv0 == 1, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  h10 = first(h10),
  obs = .N
), by = .(mun_res, ano, dist_hv_res, seg_res, lat_res, lon_res)] %>%
  filter(as.numeric(ano) %in% c(2017, 2018)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup() 


for (v in vars_diff) {
  
  if (!v %in% names(base_c)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_c[[v1]] <- ifelse(base_c$ano == 2017, base_c[[v]], NA_real_)
  
  base_c[[v2]] <- ave(
    base_c[[v1]], 
    base_c$mun_res, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_c[[dv]] <- base_c[[v]] - base_c[[v2]]
  
  base_c[[dv]][!is.finite(base_c[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_c), value = TRUE)
base_c <- base_c %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia) %>% 
  mutate(
    aux_uf = mun_res %/% 100000,
    
    # Exam start time dummies
    h13 = ifelse(
      aux_uf %in% c(
        52, 53, 31, 32, 33, 35, 42, 41, 43,
        29, 28, 27, 26, 25, 24, 23, 22, 21, 17, 15, 16
      ),
      1,0
    ),
    
    h12 = ifelse(
      aux_uf %in% c(51, 50, 11, 14) |
        (aux_uf == 13 & !mun_res %in% am_mun_special),
      1, 0
    ),
    
    h11 = ifelse(
      
      aux_uf == 12 |
        mun_res %in% am_mun_special,
      1, 0
    )
  ) %>% 
  select(-aux_uf)


ef <- dummy_cols(base_c$seg_res[base_c$ano == 2017])
ef <- ef %>% select(-1,-2)

list <- list()

list[[as.character(paste0(2018,"-",2017,"C|NF"))]] <- rdrobust(
  y = base_c$d.media[base_c$ano == 2018],
  x = base_c$dist_hv_res[base_c$ano == 2017],
  c = 0,
  cluster = base_c$seg_res[base_c$ano == 2017],
  weights = base_c$obs[base_c$ano == 2017],
  vce = "hc0",
  covs = cbind(
    ef,
    base_c$lat_res[base_c$ano == 2017],
    base_c$lon_res[base_c$ano == 2017],
    #Weather
    base_c$dtempd1[base_c$ano == 2018], #Temperature
    base_c$dumidd1[base_c$ano == 2018], #Humidity d1
    base_c$dumidd2[base_c$ano == 2018], #Humidty d2
    base_c$dtempd2[base_c$ano == 2018], #temp2
    
    #Parents and Individuals Characteristics
    base_c$descm[base_c$ano == 2018], #mother educ
    base_c$dfem[base_c$ano == 2018], #Female
    base_c$dppi[base_c$ano == 2018], #PPI
    base_c$didade[base_c$ano == 2018], #Age
    base_c$descp[base_c$ano == 2018], #father educ
    #House
    base_c$dn_ban[base_c$ano == 2018], #bathrooms
    base_c$dpessoa[base_c$ano == 2018], #people in household
    base_c$dn_qua[base_c$ano == 2018], #houses
    base_c$dn_car[base_c$ano == 2018], #cars
    base_c$dn_gel[base_c$ano == 2018], #geladeira
    base_c$dn_cel[base_c$ano == 2018], #cel
    base_c$dpc[base_c$ano == 2018],    #pc
    base_c$dinternet[base_c$ano == 2018], #internet
    base_c$dempr_dom[base_c$ano == 2018], #Housekeeping
    
    #Timezones
    base_c$h13[base_c$ano == 2018],
    base_c$h12[base_c$ano == 2018],
    base_c$h11[base_c$ano == 2018]
  )
)

# ---------------------------------------------------------------------------- #
#Extração da banda ótima
bw_main_c  <- list[["2018-2017C|NF"]]$bws[1]
bw_bias_c  <- list[["2018-2017C|NF"]]$bws[2]
# ---------------------------------------------------------------------------- #

rm(list, ef)
# ---------------------------------------------------------------------------- #
#### 2.2.2.2 POL 2 ------
# ---------------------------------------------------------------------------- #
plist <- list()

bins <- c(25)

for(j in bins) {
  
  temp <- base_c %>%
    mutate(subset = case_when(
      abs(dist_hv_res) < bw_main_r ~ 1,
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
    select(dist_hv_res)
  
  # Clusters
  clu <- temp %>%
    filter(ano == 2017) %>%
    select(seg_res)
  
  # Latitude
  latv <- temp %>%
    filter(ano == 2017) %>%
    select(lat_res)
  
  # Longitude
  lonv <- temp %>%
    filter(ano == 2017) %>%
    select(lon_res)
  
  ef <- dummy_cols(clu$seg_res)
  ef <- ef %>% select(-1,-2)
  
  #time 
  time <- temp %>% 
    filter(ano == 2018) %>% 
    select(h11, h12, h13)
  
  #all
  all <- temp %>% 
    filter(ano == 2018) %>% 
    select(dtempd1, dtempd2, dumidd1, dumidd2,
           
           dn_ban, dn_qua, dn_car, dn_gel, dn_cel, dpc, dinternet,
           
           dfem, descm, dppi, didade, descp,
           
           drenda1, drenda110, drenda10, dpibpc)
  
  
  # Estimando parâmetros do gráfico
  plist[[as.character(paste0(j))]] <- rdplot(y = yv$vd,
                                             x = xv$dist_hv_res,
                                             c = 0,
                                             p = 2,
                                             #binselect = "esmv",
                                             kernel = "triangular",
                                             h = bw_main_c,
                                             weights = temp$obs[temp$ano == 2017],
                                             subset = temp$subset == 1,
                                             hide = T,
                                             masspoints= "adjust",
                                             covs = cbind(ef,latv,lonv, time, all),
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
  
  
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/controls/img/",i,"_pol2_1817_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 9, width = 13)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/controls/img/pdf/",i,"_pol2_1817_v1.pdf"),plot = fig_gg, device = "pdf",height = 9, width = 13)
  
  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips, vars_poly_left, vars_poly_right)
  
}

rm(bins, j, plist, fig_loop, base_c, bw_main_c, bw_bias_c, yv,xv, all, time, am_mun_special,
   dv, v, v1, v2, vars_diff)

# ---------------------------------------------------------------------------- #
# 3. Matérias ----
# ---------------------------------------------------------------------------- #
#Bases


# ---------------- #
# A
# ---------------- #
base_res <- base %>% 
  filter(priv0 == 1,
         ano %in% c(2018:2019)) %>%
  group_by(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res) %>%
  summarise(media_rd = mean(rd, na.rm = T),
            media_cn = mean(cn, na.rm = T),
            media_lc = mean(lc, na.rm = T),
            media_ch = mean(ch, na.rm = T),
            media_mt = mean(mt, na.rm = T),
            obs = n(),
            .groups = "drop")

#Calculando as diferenças

#Base A
base_res <- base_res %>%
  arrange(mun_res,ano) %>%
  group_by(mun_res) %>%
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
            v1_cn, v2_cn, v1_ch,v2_ch,v1_lc,v2_lc,v1_mt,v2_mt)) %>% 
  left_join(base_aux, by = c("mun_res", "ano"))




#Calculando as diferenças




# ---------------------------------------------------------------------------- #
## 3.1. Regression ----
# ---------------------------------------------------------------------------- #

p_list <- list()

d_list <- c("d.media_rd", "d.media_lc", "d.media_ch", "d.media_cn", "d.media_mt")

for (i in d_list){
  
  #Com Controles
  
  ef <- dummy_cols(base_res$seg_res[base_res$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  p_list[[as.character(paste0("wc_",i,"|TC"))]] <-
    rdrobust(
      y = base_res[[i]][base_res$ano == 2019],
      x = base_res$dist_hv_res[base_res$ano == 2018],
      c = 0,
      p = 1,
      h = bw_main_r,
      b = bw_bias_r,
      cluster = base_res$seg_res[base_res$ano == 2018],
      weights = base_res$obs[base_res$ano == 2018],
      vce = "hc0",
      covs = cbind(
        ef, 
        base_res$lat_res[base_res$ano == 2018],
        base_res$lon_res[base_res$ano == 2018],
        
        #Weather
        base_res$dtempd1[base_res$ano == 2019], #Temperature
        base_res$dumidd1[base_res$ano == 2019], #Humidity d1
        base_res$dumidd2[base_res$ano == 2019], #Humidty d2
        base_res$dtempd2[base_res$ano == 2019], #temp2
        
        #House
        base_res$dn_ban[base_res$ano == 2019], #bathrooms
        base_res$dpessoa[base_res$ano == 2019], #people in household
        base_res$dn_qua[base_res$ano == 2019], #houses
        base_res$dn_car[base_res$ano == 2019], #cars
        base_res$dn_gel[base_res$ano == 2019], #geladeira
        base_res$dn_cel[base_res$ano == 2019], #cel
        base_res$dpc[base_res$ano == 2019],    #pc
        base_res$dinternet[base_res$ano == 2019], #internet
        base_res$dempr_dom[base_res$ano == 2019], #Housekeeping
        
        #remaining
        base_res$descm[base_res$ano == 2019], #mother educ
        base_res$dfem[base_res$ano == 2019], #Female
        base_res$dppi[base_res$ano == 2019], #PPI
        base_res$didade[base_res$ano == 2019], #Age
        base_res$descp[base_res$ano == 2019], #father educ
        base_res$drenda1[base_res$ano == 2019], #wage < 1MW
        base_res$drenda110[base_res$ano == 2019], #wage 1MW - 10MW
        base_res$drenda10[base_res$ano == 2019], #wage > 10MW
        base_res$dpibpc[base_res$ano == 2019], #pibpc
        
        #Fuso
        base_res$h13[base_res$ano == 2019],
        base_res$h12[base_res$ano == 2019],
        base_res$h11[base_res$ano == 2019]
      )
    )
  
  
}



rm(ef, i)
# ---------------------------------------------------------------------------- #
## 3.2 Table ----
# ---------------------------------------------------------------------------- #

notas_tab <- data.frame(
  coef   = sapply(p_list, function(x) x$coef[3]),
  se     = sapply(p_list, function(x) x$se[3]),
  pv     = sapply(p_list, function(x) x$pv[3]),
  n_left = sapply(p_list, function(x) x$N_h[1]),
  n_rght = sapply(p_list, function(x) x$N_h[2]),
  bw     = sapply(p_list, function(x) x$bws[1, 1]),
  totr   = sapply(p_list, function(x) x$N[2]),
  totl   = sapply(p_list, function(x) x$N[1])
)

print(notas_tab)


result <- data.frame(
  ` ` = c(
    "2019 - 2018",
    " ",
    "N = N$_L$, N$_R$",
    "BW",
    "Municipalities"
  ),
  `(1)` = c(
    fmt_est(notas_tab$coef[1], notas_tab$pv[1]),
    fmt_se(notas_tab$se[1]),
    fmt_npair(notas_tab$n_left[1], notas_tab$n_rght[1]),
    fmt_bw(notas_tab$bw[1]),
    formatC(notas_tab$totr[1] + notas_tab$totl[1], format = "d", big.mark = ",")
  ),
  `(2)` = c(
    fmt_est(notas_tab$coef[2], notas_tab$pv[2]),
    fmt_se(notas_tab$se[2]),
    fmt_npair(notas_tab$n_left[2], notas_tab$n_rght[2]),
    fmt_bw(notas_tab$bw[2]),
    formatC(notas_tab$totr[2] + notas_tab$totl[2], format = "d", big.mark = ",")
    
  ),
  `(3)` = c(
    fmt_est(notas_tab$coef[3], notas_tab$pv[3]),
    fmt_se(notas_tab$se[3]),
    fmt_npair(notas_tab$n_left[3], notas_tab$n_rght[3]),
    fmt_bw(notas_tab$bw[3]),
    formatC(notas_tab$totr[3] + notas_tab$totl[3], format = "d", big.mark = ",")
    
  ),
  `(4)` = c(
    fmt_est(notas_tab$coef[4], notas_tab$pv[4]),
    fmt_se(notas_tab$se[4]),
    fmt_npair(notas_tab$n_left[4], notas_tab$n_rght[4]),
    fmt_bw(notas_tab$bw[4]),
    formatC(notas_tab$totr[4] + notas_tab$totl[4], format = "d", big.mark = ",")
    
  ),
  `(5)` = c(
    fmt_est(notas_tab$coef[5], notas_tab$pv[5]),
    fmt_se(notas_tab$se[5]),
    fmt_npair(notas_tab$n_left[5], notas_tab$n_rght[5]),
    fmt_bw(notas_tab$bw[5]),
    formatC(notas_tab$totr[5] + notas_tab$totl[5], format = "d", big.mark = ",")
    
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)


# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lccccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/Materias_v1.tex")
#5548
rm(result, latex_table, notas_tab, d_list, p_list)

# ---------------------------------------------------------------------------- #
# 4. Redação ----
# ---------------------------------------------------------------------------- #

# Agregados dos critérios
base_res <- base %>% 
  filter(priv0 == 1,
         ano %in% c(2018:2019)) %>%
  group_by(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res) %>%
  summarise(media_rd1 = mean(rd1, na.rm = T),
            media_rd2 = mean(rd2, na.rm = T),
            media_rd3 = mean(rd3, na.rm = T),
            media_rd4 = mean(rd4, na.rm = T),
            media_rd5 = mean(rd5, na.rm = T),
            obs = n(),
            .groups = "drop")


#Calculando as diferenças
base_res <- base_res %>%
  arrange(mun_res,ano) %>%
  group_by(mun_res) %>%
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
  select(-c(dup1,dup2,v1_rd1,v2_rd1,v1_rd2,v2_rd2,v1_rd3,v2_rd3,v1_rd4,v2_rd4,v1_rd5,v2_rd5)) %>% 
  left_join(base_aux, by = c("mun_res", "ano"))

# ---------------------------------------------------------------------------- #
##4.1 Regression ----
# ---------------------------------------------------------------------------- #

p_list <- list()

d_list <- c("d.media_rd1", "d.media_rd2", "d.media_rd3", "d.media_rd4", "d.media_rd5")


for (i in d_list){
  
  #Com Controles
  
  ef <- dummy_cols(base_res$seg_res[base_res$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  p_list[[as.character(paste0("Wc_",i,"|TC"))]] <-
    rdrobust(
      y = base_res[[i]][base_res$ano == 2019],
      x = base_res$dist_hv_res[base_res$ano == 2018],
      c = 0,
      h = bw_main_r,
      b = bw_bias_r,
      cluster = base_res$seg_res[base_res$ano == 2018],
      weights = base_res$obs[base_res$ano == 2018],
      vce = "hc0",
      covs = cbind(
        ef, 
        base_res$lat_res[base_res$ano == 2018],
        base_res$lon_res[base_res$ano == 2018],
        
        #Weather
        base_res$dtempd1[base_res$ano == 2019], #Temperature
        base_res$dumidd1[base_res$ano == 2019], #Humidity d1
        base_res$dumidd2[base_res$ano == 2019], #Humidty d2
        base_res$dtempd2[base_res$ano == 2019], #temp2
        
        #House
        base_res$dn_ban[base_res$ano == 2019], #bathrooms
        base_res$dpessoa[base_res$ano == 2019], #people in household
        base_res$dn_qua[base_res$ano == 2019], #houses
        base_res$dn_car[base_res$ano == 2019], #cars
        base_res$dn_gel[base_res$ano == 2019], #geladeira
        base_res$dn_cel[base_res$ano == 2019], #cel
        base_res$dpc[base_res$ano == 2019],    #pc
        base_res$dinternet[base_res$ano == 2019], #internet
        base_res$dempr_dom[base_res$ano == 2019], #Housekeeping
        
        #remaining
        base_res$descm[base_res$ano == 2019], #mother educ
        base_res$dfem[base_res$ano == 2019], #Female
        base_res$dppi[base_res$ano == 2019], #PPI
        base_res$didade[base_res$ano == 2019], #Age
        base_res$descp[base_res$ano == 2019], #father educ
        base_res$drenda1[base_res$ano == 2019], #wage < 1MW
        base_res$drenda110[base_res$ano == 2019], #wage 1MW - 10MW
        base_res$drenda10[base_res$ano == 2019], #wage > 10MW
        base_res$dpibpc[base_res$ano == 2019], #pibpc
        
        #Fuso
        base_res$h13[base_res$ano == 2019],
        base_res$h12[base_res$ano == 2019],
        base_res$h11[base_res$ano == 2019]
      )
    )
  
  
}
rm(ef,i)


# ---------------------------------------------------------------------------- #
##4.2 Table ----
# ---------------------------------------------------------------------------- #

red_tab <- data.frame(
  coef   = sapply(p_list, function(x) x$coef[3]),
  se     = sapply(p_list, function(x) x$se[3]),
  pv     = sapply(p_list, function(x) x$pv[3]),
  n_left = sapply(p_list, function(x) x$N_h[1]),
  n_rght = sapply(p_list, function(x) x$N_h[2]),
  bw     = sapply(p_list, function(x) x$bws[1, 1]),
  totr   = sapply(p_list, function(x) x$N[2]),
  totl   = sapply(p_list, function(x) x$N[1])
)


result <- data.frame(
  ` ` = c(
    "Proficiency in formal written language",
    " ",
    "Comprehension of essay theme",
    " ",
    "Organization and structure of arguments",
    " ",
    "Use of linguistic mechanisms",
    " ", 
    "Proposal of intervention",
    " ",
    "N = N$_L$, N$_R$",
    "Bandwidth",
    "Municipalities"
  ),
  `(1)` = c(
    fmt_est(red_tab$coef[1], red_tab$pv[1]),
    fmt_se(red_tab$se[1]),
    fmt_est(red_tab$coef[2], red_tab$pv[2]),
    fmt_se(red_tab$se[2]),
    fmt_est(red_tab$coef[3], red_tab$pv[3]),
    fmt_se(red_tab$se[3]),
    fmt_est(red_tab$coef[4], red_tab$pv[4]),
    fmt_se(red_tab$se[4]),
    fmt_est(red_tab$coef[5], red_tab$pv[5]),
    fmt_se(red_tab$se[5]),
    fmt_npair(red_tab$n_left[1], red_tab$n_rght[1]),
    fmt_bw(red_tab$bw[1]),
    formatC(red_tab$totr[1] + red_tab$totl[1], format = "d", big.mark = ",")
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE)


# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lc",
  linesep = ""
)

writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/Redacao_v1.tex")
#5548

rm(p_list, red_tab, result, d_list, latex_table, row)


# ---------------------------------------------------------------------------- #
# 5. Dificuldade ----
# ---------------------------------------------------------------------------- #
gc()
# Agregados dos critérios

base_res <- base %>% 
  filter(priv0 == 1,
         ano %in% c(2018:2019)) %>%
  group_by(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res) %>%
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
base_res <- base_res %>%
  arrange(mun_res,ano) %>%
  group_by(mun_res) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1)) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  group_by(mun_res) %>%
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
            v1_pbh, v1_pbl, v2_pbl, v2_pbh)) %>% 
  left_join(base_aux, by = c("mun_res", "ano"))

# ---------------------------------------------------------------------------- #
##5.1 Regression ----
# ---------------------------------------------------------------------------- #
rp_list <- list()

d_list <- c("d.mediabl", "d.mediabh", "d.media_bl_lc", "d.media_bh_lc",
            "d.media_bl_ch", "d.media_bh_ch", "d.media_bl_cn", "d.media_bh_cn",
            "d.media_bl_mt", "d.media_bh_mt")

for (i in d_list){
  
  #Com Controles
  
  ef <- dummy_cols(base_res$seg_res[base_res$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  rp_list[[as.character(paste0("cc_",i,"|TC"))]] <-
    rdrobust(
      y = base_res[[i]][base_res$ano == 2019],
      x = base_res$dist_hv_res[base_res$ano == 2018],
      c = 0,
      h = bw_main_r,
      b = bw_bias_r,
      cluster = base_res$seg_res[base_res$ano == 2018],
      weights = base_res$obs[base_res$ano == 2018],
      vce = "hc0",
      covs = cbind(
        ef,
        base_res$lat_res[base_res$ano == 2018],
        base_res$lon_res[base_res$ano == 2018],
        
        #Weather
        base_res$dtempd1[base_res$ano == 2019], #Temperature
        base_res$dumidd1[base_res$ano == 2019], #Humidity d1
        base_res$dumidd2[base_res$ano == 2019], #Humidty d2
        base_res$dtempd2[base_res$ano == 2019], #temp2
        
        #House
        base_res$dn_ban[base_res$ano == 2019], #bathrooms
        base_res$dpessoa[base_res$ano == 2019], #people in household
        base_res$dn_qua[base_res$ano == 2019], #houses
        base_res$dn_car[base_res$ano == 2019], #cars
        base_res$dn_gel[base_res$ano == 2019], #geladeira
        base_res$dn_cel[base_res$ano == 2019], #cel
        base_res$dpc[base_res$ano == 2019],    #pc
        base_res$dinternet[base_res$ano == 2019], #internet
        base_res$dempr_dom[base_res$ano == 2019], #Housekeeping
        
        #remaining
        base_res$descm[base_res$ano == 2019], #mother educ
        base_res$dfem[base_res$ano == 2019], #Female
        base_res$dppi[base_res$ano == 2019], #PPI
        base_res$didade[base_res$ano == 2019], #Age
        base_res$descp[base_res$ano == 2019], #father educ
        base_res$drenda1[base_res$ano == 2019], #wage < 1MW
        base_res$drenda110[base_res$ano == 2019], #wage 1MW - 10MW
        base_res$drenda10[base_res$ano == 2019], #wage > 10MW
        base_res$dpibpc[base_res$ano == 2019], #pibpc
        
        #Fuso
        base_res$h13[base_res$ano == 2019],
        base_res$h12[base_res$ano == 2019],
        base_res$h11[base_res$ano == 2019]
      )
    )
  
  
}
rm(ef,i)


# ---------------------------------------------------------------------------- #
###5.1.1 Dif (Easy - Hard)----
# ---------------------------------------------------------------------------- #
base_res <- base_res %>%
  group_by(mun_res, ano) %>%
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
  
  ef <- dummy_cols(base_res$seg_res[base_res$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  dif_list[[as.character(paste0("cc_",i,"|TC"))]] <-
    rdrobust(
      y = base_res[[i]][base_res$ano == 2019],
      x = base_res$dist_hv_res[base_res$ano == 2018],
      c = 0,
      h = bw_main_r,
      b = bw_bias_r,
      cluster = base_res$seg_res[base_res$ano == 2018],
      weights = base_res$obs[base_res$ano == 2018],
      vce = "hc0",
      covs = cbind(
        ef,
        base_res$lat_res[base_res$ano == 2018],
        base_res$lon_res[base_res$ano == 2018],
        
        #Weather
        base_res$dtempd1[base_res$ano == 2019], #Temperature
        base_res$dumidd1[base_res$ano == 2019], #Humidity d1
        base_res$dumidd2[base_res$ano == 2019], #Humidty d2
        base_res$dtempd2[base_res$ano == 2019], #temp2
        
        #House
        base_res$dn_ban[base_res$ano == 2019], #bathrooms
        base_res$dpessoa[base_res$ano == 2019], #people in household
        base_res$dn_qua[base_res$ano == 2019], #houses
        base_res$dn_car[base_res$ano == 2019], #cars
        base_res$dn_gel[base_res$ano == 2019], #geladeira
        base_res$dn_cel[base_res$ano == 2019], #cel
        base_res$dpc[base_res$ano == 2019],    #pc
        base_res$dinternet[base_res$ano == 2019], #internet
        base_res$dempr_dom[base_res$ano == 2019], #Housekeeping
        
        #remaining
        base_res$descm[base_res$ano == 2019], #mother educ
        base_res$dfem[base_res$ano == 2019], #Female
        base_res$dppi[base_res$ano == 2019], #PPI
        base_res$didade[base_res$ano == 2019], #Age
        base_res$descp[base_res$ano == 2019], #father educ
        base_res$drenda1[base_res$ano == 2019], #wage < 1MW
        base_res$drenda110[base_res$ano == 2019], #wage 1MW - 10MW
        base_res$drenda10[base_res$ano == 2019], #wage > 10MW
        base_res$dpibpc[base_res$ano == 2019], #pibpc
        
        #Fuso
        base_res$h13[base_res$ano == 2019],
        base_res$h12[base_res$ano == 2019],
        base_res$h11[base_res$ano == 2019]
      )
    )
  
  
}
rm(ef,i, new_list)


# ---------------------------------------------------------------------------- #
##5.2 Table ----
# ---------------------------------------------------------------------------- #
### 5.2.1 Main ----
# ---------------------------------------------------------------------------- #

mat_nota <- data.frame(
  coef   = sapply(rp_list, function(x) x$coef[3]),
  se     = sapply(rp_list, function(x) x$se[3]),
  pv     = sapply(rp_list, function(x) x$pv[3]),
  n_left = sapply(rp_list, function(x) x$N_h[1]),
  n_rght = sapply(rp_list, function(x) x$N_h[2]),
  bw     = sapply(rp_list, function(x) x$bws[1, 1]),
  totr   = sapply(rp_list, function(x) x$N[2]),
  totl   = sapply(rp_list, function(x) x$N[1])
)


# ---------------------------------------------------------------------------- #
### 5.2.1 Dif. Table ----
# ---------------------------------------------------------------------------- #

dif_tab <- data.frame(
  coef   = sapply(dif_list, function(x) x$coef[3]),
  se     = sapply(dif_list, function(x) x$se[3]),
  pv     = sapply(dif_list, function(x) x$pv[3]),
  n_left = sapply(dif_list, function(x) x$N_h[1]),
  n_rght = sapply(dif_list, function(x) x$N_h[2]),
  bw     = sapply(dif_list, function(x) x$bws[1, 1]),
  totr   = sapply(dif_list, function(x) x$N[2]),
  totl   = sapply(dif_list, function(x) x$N[1])
)


# ---------------------------------------------------------------------------- #
### 5.2.3 Latex ----
# ---------------------------------------------------------------------------- #

result <- data.frame(
  ` ` = c(
    "2019 - 2018",
    " ",
    "N = N$_L$, N$_R$",
    "Bandwidth",
    "Municipalities"
  ),
  `(1)` = c(
    fmt_est(mat_nota$coef[1], mat_nota$pv[1]),
    fmt_se(mat_nota$se[1]),
    fmt_npair(mat_nota$n_left[1], mat_nota$n_rght[1]),
    fmt_bw(mat_nota$bw[1]),
    formatC(mat_nota$totr[1] + mat_nota$totl[1], format = "d", big.mark = ",")
  ),
  `(2)` = c(
    fmt_est(mat_nota$coef[2], mat_nota$pv[2]),
    fmt_se(mat_nota$se[2]),
    fmt_npair(mat_nota$n_left[2], mat_nota$n_rght[2]),
    fmt_bw(mat_nota$bw[2]),
    formatC(mat_nota$totr[2] + mat_nota$totl[2], format = "d", big.mark = ",")
    
  ),
  `(3)` = c( #For the dif_tab
    fmt_est(dif_tab$coef[1], dif_tab$pv[1]),
    fmt_se(dif_tab$se[1]),
    fmt_npair(dif_tab$n_left[1], dif_tab$n_rght[1]),
    fmt_bw(dif_tab$bw[1]),
    formatC(dif_tab$totr[1] + dif_tab$totl[1], format = "d", big.mark = ",")
    
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)



# ---------------------------------------------------------------------------- #
# Latex saving 
# ---------------------------------------------------------------------------- #

latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F, 
  align = "lccc",
  linesep = ""
)



writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/Dificuldade_v1.tex")
# N = Total 5540. Por mate 5548
rm(dif_list, dif_tab, mat_nota, result, rp_list, latex_table, d_list)

# ---------------------------------------------------------------------------- #
## 5.3 Mother Schooling -----
# ---------------------------------------------------------------------------- #
### 5.3.1 Regression ----
# ---------------------------------------------------------------------------- #


result <- list()

for (i in c(0:1)) {
  
  temp <- base %>% 
    filter(escm == i,
           ano %in% c(2018:2019))
  
  temp_ag <- temp %>% 
    filter(priv0 == 1) %>%
    group_by(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res) %>%
    summarise(mediabl = mean(acerto_pbl, na.rm = T),
              mediabh = mean(acerto_pbh, na.rm = T),
              obs = n(),
              .groups = "drop")
  
  
  
  #Base adjustments
  temp_ag <- temp_ag %>%
    arrange(mun_res,ano) %>%
    group_by(mun_res) %>%
    filter(!is.na(mediabh) & !is.na(mediabl)) %>% 
    mutate(
      dup1 = 1,
      dup2 = sum(dup1)) %>%
    ungroup() %>%
    filter(dup2 == 2) %>%
    group_by(mun_res) %>%
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
    group_by(mun_res, ano) %>%
    mutate(
      dif_avg = d.mediabl - d.mediabh
    ) %>% ungroup() %>% 
    left_join(base_aux, by = c("mun_res", "ano"))
  
  
  
  
  
  for(v in c("d.mediabl", "d.mediabh", "dif_avg")) {
    
    
    
    
    #Com Controles
    
    ef <- dummy_cols(temp_ag$seg_res[temp_ag$ano == 2018])
    ef <- ef %>% select(-1,-2)
    
    
    result[[as.character(paste0(i,"_",v))]] <-
      rdrobust(
        y = temp_ag[[v]][temp_ag$ano == 2019],
        x = temp_ag$dist_hv_res[temp_ag$ano == 2018],
        c = 0,
        h = bw_main_r,
        b = bw_bias_r,
        cluster = temp_ag$seg_res[temp_ag$ano == 2018],
        weights = temp_ag$obs[temp_ag$ano == 2018],
        vce = "hc0",
        covs = cbind(
          ef,
          temp_ag$lat_res[temp_ag$ano == 2018],
          temp_ag$lon_res[temp_ag$ano == 2018],
          #All
          temp_ag$dtempd1[temp_ag$ano == 2019], #Temperature
          temp_ag$descm[temp_ag$ano == 2019], #mother educ
          temp_ag$dn_ban[temp_ag$ano == 2019], #bathrooms
          temp_ag$dumidd1[temp_ag$ano == 2019], #Humidity d1
          temp_ag$dumidd2[temp_ag$ano == 2019], #Humidty d2
          temp_ag$dfem[temp_ag$ano == 2019], #Female
          temp_ag$dppi[temp_ag$ano == 2019], #PPI
          temp_ag$didade[temp_ag$ano == 2019], #Age
          temp_ag$descp[temp_ag$ano == 2019], #father educ
          
          temp_ag$dpessoa[temp_ag$ano == 2019], #people in household
          temp_ag$dn_qua[temp_ag$ano == 2019], #houses
          temp_ag$dn_car[temp_ag$ano == 2019], #cars
          temp_ag$dn_gel[temp_ag$ano == 2019], #geladeira
          temp_ag$dn_cel[temp_ag$ano == 2019], #cel
          temp_ag$dpc[temp_ag$ano == 2019],    #pc
          temp_ag$dinternet[temp_ag$ano == 2019], #internet
          temp_ag$dempr_dom[temp_ag$ano == 2019], #housekeeping
          
          temp_ag$drenda1[temp_ag$ano == 2019], #wage < 1MW
          temp_ag$drenda110[temp_ag$ano == 2019], #wage 1MW - 10MW
          temp_ag$drenda10[temp_ag$ano == 2019], #wage > 10MW
          temp_ag$dpibpc[temp_ag$ano == 2019], #pibpc
          
          temp_ag$dtempd2[temp_ag$ano == 2019], #temp2
          
          #Fuso
          temp_ag$h13[temp_ag$ano == 2019],
          temp_ag$h12[temp_ag$ano == 2019],
          temp_ag$h11[temp_ag$ano == 2019]
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
  coef   = sapply(result, function(x) x$coef[3]),
  se     = sapply(result, function(x) x$se[3]),
  pv     = sapply(result, function(x) x$pv[3]),
  n_left = sapply(result, function(x) x$N_h[1]),
  n_rght = sapply(result, function(x) x$N_h[2]),
  bw     = sapply(result, function(x) x$bws[1, 1]),
  totr   = sapply(result, function(x) x$N[2]),
  totl   = sapply(result, function(x) x$N[1])
)



result <- data.frame(
  ` ` = c(
    "Mother without High School",
    " ",
    "N = N$_L$, N$_R$",
    "BW",
    "Municipalities",
    "Mother with High School",
    " ",
    "N = N$_L$, N$_R$",
    "BW",
    "Municipalities"
  ),
  `(1)` = c(#Low
    fmt_est(mat_nota$coef[1], mat_nota$pv[1]),
    fmt_se(mat_nota$se[1]),
    fmt_npair(mat_nota$n_left[1], mat_nota$n_rght[1]),
    fmt_bw(mat_nota$bw[1]),
    formatC(mat_nota$totr[1] + mat_nota$totl[1], format = "d", big.mark = ","),
    #High
    fmt_est(mat_nota$coef[4], mat_nota$pv[4]),
    fmt_se(mat_nota$se[4]),
    fmt_npair(mat_nota$n_left[4], mat_nota$n_rght[4]),
    fmt_bw(mat_nota$bw[4]),
    formatC(mat_nota$totr[4] + mat_nota$totl[4], format = "d", big.mark = ",")
  ),
  `(2)` = c(#Low
    fmt_est(mat_nota$coef[2], mat_nota$pv[2]),
    fmt_se(mat_nota$se[2]),
    fmt_npair(mat_nota$n_left[2], mat_nota$n_rght[2]),
    fmt_bw(mat_nota$bw[2]),
    formatC(mat_nota$totr[2] + mat_nota$totl[2], format = "d", big.mark = ","),
    #High
    fmt_est(mat_nota$coef[5], mat_nota$pv[5]),
    fmt_se(mat_nota$se[5]),
    fmt_npair(mat_nota$n_left[5], mat_nota$n_rght[5]),
    fmt_bw(mat_nota$bw[5]),
    formatC(mat_nota$totr[5] + mat_nota$totl[5], format = "d", big.mark = ",")
    
  ),
  `(3)` = c( #Low
    fmt_est(mat_nota$coef[3], mat_nota$pv[3]),
    fmt_se(mat_nota$se[3]),
    fmt_npair(mat_nota$n_left[3], mat_nota$n_rght[3]),
    fmt_bw(mat_nota$bw[3]),
    formatC(mat_nota$totr[3] + mat_nota$totl[3], format = "d", big.mark = ","),
    #High
    fmt_est(mat_nota$coef[6], mat_nota$pv[6]),
    fmt_se(mat_nota$se[6]),
    fmt_npair(mat_nota$n_left[6], mat_nota$n_rght[6]),
    fmt_bw(mat_nota$bw[6]),
    formatC(mat_nota$totr[6] + mat_nota$totl[6], format = "d", big.mark = ",")
    
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)






# ---------------------------------------------------------------------------- #
### 5.3.3 Latex ----
# ---------------------------------------------------------------------------- #




latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lccccc",
  linesep = ""
)



writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/Dificuldade_mae.tex")
#0 = 5465, 1 = 5433
rm( mat_nota, result, latex_table)

# ---------------------------------------------------------------------------- #
# 6. DIAS (comparabilidade) ----
# ---------------------------------------------------------------------------- #

base_res <- base %>% 
  filter(priv0 == 1,
         ano %in% c(2018:2019)) %>%
  group_by(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res) %>%
  summarise(media_dia1 = mean(dia_1, na.rm = T),
            media_dia2 = mean(dia_2, na.rm = T),
            obs = n(),
            .groups = "drop")


base_res <- base_res %>%
  arrange(mun_res,ano) %>%
  group_by(mun_res) %>%
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
  select(-c(v1_d1, v2_d1, v2_d2, v1_d2, dup1, dup2)) %>% 
  left_join(base_aux, by = c("mun_res", "ano"))



result <- list()

d_list <- c("d.media_d1", "d.media_d2")

# ---------------------------------------------------------------------------- #
## 6.1 Regression ----
# ---------------------------------------------------------------------------- #

for (i in d_list){
  
  #Com Controles
  
  ef <- dummy_cols(base_res$seg_res[base_res$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  result[[as.character(paste0("nc_",i,"|TC"))]] <-
    rdrobust(
      y = base_res[[i]][base_res$ano == 2019],
      x = base_res$dist_hv_res[base_res$ano == 2018],
      c = 0,
      h = bw_main_r,
      b = bw_bias_r,
      p = 1,
      cluster = base_res$seg_res[base_res$ano == 2018],
      weights = base_res$obs[base_res$ano == 2018],
      vce = "hc0",
      covs = cbind(
        ef, 
        base_res$lat_res[base_res$ano == 2018],
        base_res$lon_res[base_res$ano == 2018],
        
        #Weather
        base_res$dtempd1[base_res$ano == 2019], #Temperature
        base_res$dumidd1[base_res$ano == 2019], #Humidity d1
        base_res$dumidd2[base_res$ano == 2019], #Humidty d2
        base_res$dtempd2[base_res$ano == 2019], #temp2
        
        #House
        base_res$dn_ban[base_res$ano == 2019], #bathrooms
        base_res$dpessoa[base_res$ano == 2019], #people in household
        base_res$dn_qua[base_res$ano == 2019], #houses
        base_res$dn_car[base_res$ano == 2019], #cars
        base_res$dn_gel[base_res$ano == 2019], #geladeira
        base_res$dn_cel[base_res$ano == 2019], #cel
        base_res$dpc[base_res$ano == 2019],    #pc
        base_res$dinternet[base_res$ano == 2019], #internet
        base_res$dempr_dom[base_res$ano == 2019], #Housekeeping
        
        #remaining
        base_res$descm[base_res$ano == 2019], #mother educ
        base_res$dfem[base_res$ano == 2019], #Female
        base_res$dppi[base_res$ano == 2019], #PPI
        base_res$didade[base_res$ano == 2019], #Age
        base_res$descp[base_res$ano == 2019], #father educ
        base_res$drenda1[base_res$ano == 2019], #wage < 1MW
        base_res$drenda110[base_res$ano == 2019], #wage 1MW - 10MW
        base_res$drenda10[base_res$ano == 2019], #wage > 10MW
        base_res$dpibpc[base_res$ano == 2019], #pibpc
        
        #Fuso
        base_res$h13[base_res$ano == 2019],
        base_res$h12[base_res$ano == 2019],
        base_res$h11[base_res$ano == 2019]
      )
    )
  
  message("Finished for day: ", i)
  
}
rm(ef, i)

# ---------------------------------------------------------------------------- #
## 6.2 Result Table ----
# ---------------------------------------------------------------------------- #



dias <- data.frame(
  coef   = sapply(result, function(x) x$coef[3]),
  se     = sapply(result, function(x) x$se[3]),
  pv     = sapply(result, function(x) x$pv[3]),
  n_left = sapply(result, function(x) x$N_h[1]),
  n_rght = sapply(result, function(x) x$N_h[2]),
  bw     = sapply(result, function(x) x$bws[1, 1]),
  totr   = sapply(result, function(x) x$N[2]),
  totl   = sapply(result, function(x) x$N[1])
)




result <- data.frame(
  ` ` = c(
    "2019 - 2018",
    " ",
    "N = N$_L$, N$_R$",
    "BW",
    "Municipalities"
  ),
  `(1)` = c(#Low
    fmt_est(dias$coef[1], dias$pv[1]),
    fmt_se(dias$se[1]),
    fmt_npair(dias$n_left[1], dias$n_rght[1]),
    fmt_bw(dias$bw[1]),
    formatC(dias$totr[1] + dias$totl[1], format = "d", big.mark = ",")
  ),
  `(2)` = c(#Low
    fmt_est(dias$coef[2], dias$pv[2]),
    fmt_se(dias$se[2]),
    fmt_npair(dias$n_left[2], dias$n_rght[2]),
    fmt_bw(dias$bw[2]),
    formatC(dias$totr[2] + dias$totl[2], format = "d", big.mark = ",")
    
  ),
  
  check.names = FALSE,
  stringsAsFactors = FALSE
)






# ---------------------------------------------------------------------------- #
### 6.2.1 Latex ----
# ---------------------------------------------------------------------------- #



latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lccccc",
  linesep = ""
)




writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/Dias_v1.tex")
#5548

rm(dias, p_list, result, d_list, latex_table)


# ---------------------------------------------------------------------------- #
# 7. Abstenções ----
# ---------------------------------------------------------------------------- #
gc()


am_mun_special <- c(
  1300201, 1300607, 1300706, 1301407, 1301506, 1301654, 1301803, 1301951,
  1302306, 1302405, 1303502, 1303908, 1304062
)


base_abs <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_abs_2019.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_abs_2018.RDS"))) %>%
  #Mig filter - over the DST border
  mutate(
    aux_res = mun_res %/% 100000,
    aux_pro = mun_prova %/% 100000,
    
    # Exam start time dummies
    h13 = ifelse(
      aux_res %in% c(
        52, 53, 31, 32, 33, 35, 42, 41, 43,
        29, 28, 27, 26, 25, 24, 23, 22, 21, 17, 15, 16
      ),
      1,0
    ),
    
    h12 = ifelse(
      aux_res %in% c(51, 50, 11, 14) |
        (aux_res == 13 & !mun_res %in% am_mun_special),
      1, 0
    ),
    
    h11 = ifelse(
      
      aux_res == 12 |
        mun_res %in% am_mun_special,
      1, 0
    ),
    
    new_dist = dist_hv_border / 100000,
    
    escm = case_when(
      esc_mae %in% c("D","E","F") ~ 1, #With high school
      esc_mae %in% c("A","B","C") ~ 0,
      .default = NA),
    
    escp = case_when(
      esc_pai %in% c("D","E","F") ~ 1, #With High school
      esc_pai %in% c("A","B","C") ~ 0,
      .default = NA
    ),
    
    renda1 = ifelse(renda_dom %in% c("A","B"),1,0),
    renda_1_10 = ifelse(renda_dom == "C", 1, 0),
    renda_10   = ifelse(renda_dom == "D", 1, 0),
    id18 = ifelse(idade == 18, 1, 0),
    dom5 = ifelse(pessoas_dom == "C", 1, 0),
    ppi = ifelse(raca %in% c("C","D","F"), 1, 0),
    fem = ifelse(mas == 0, 1, 0),
    
    over_hv_border = case_when(
      aux_res < 30 & aux_pro >= 30 ~ 1,
      aux_res >= 30 & aux_pro < 30 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(-aux_res, -aux_pro) %>% 
  #filter(over_hv_border == 0) %>%  #No migration over the border
  setDT()
# ---------------------------------------------------------------------------- #
## 7.0 INPE ---- 
# ---------------------------------------------------------------------------- #

inpe17 <- readRDS("Z:/Tuffy/Paper - HV/Bases/inpe/mun/inpe_mun_2017.rds")
inpe18 <- readRDS("Z:/Tuffy/Paper - HV/Bases/inpe/mun/inpe_mun_2018.rds")
inpe19 <- readRDS("Z:/Tuffy/Paper - HV/Bases/inpe/mun/inpe_mun_2019.rds")

base_abs <- base_abs %>% 
  mutate(
    temp_d1 = case_when(
      ano == 2019 ~ inpe19$temp_3[match(mun_prova, inpe19$codmun)],
      ano == 2018 ~ inpe18$temp_4[match(mun_prova, inpe18$codmun)],
      ano == 2017 ~ inpe17$temp_5[match(mun_prova, inpe17$codmun)],
      TRUE ~ NA
    ),
    
    temp_d2 = case_when(
      ano == 2019 ~ inpe19$temp_10[match(mun_prova, inpe19$codmun)],
      ano == 2018 ~ inpe18$temp_11[match(mun_prova, inpe18$codmun)],
      ano == 2017 ~ inpe17$temp_12[match(mun_prova, inpe17$codmun)],
      TRUE ~ NA
    ),
    
    umid_d1 = case_when(
      ano == 2019 ~ inpe19$umid_3[match(mun_prova, inpe19$codmun)],
      ano == 2018 ~ inpe18$umid_4[match(mun_prova, inpe18$codmun)],
      ano == 2017 ~ inpe17$umid_5[match(mun_prova, inpe17$codmun)],
      TRUE ~ NA
    ),
    
    umid_d2 = case_when(
      ano == 2019 ~ inpe19$umid_10[match(mun_prova, inpe19$codmun)],
      ano == 2018 ~ inpe18$umid_11[match(mun_prova, inpe18$codmun)],
      ano == 2017 ~ inpe17$umid_12[match(mun_prova, inpe17$codmun)],
      TRUE ~ NA
    ),
    
    #Residency
    temp_d1_res = case_when(
      ano == 2019 ~ inpe19$temp_3[match(mun_res, inpe19$codmun)],
      ano == 2018 ~ inpe18$temp_4[match(mun_res, inpe18$codmun)],
      ano == 2017 ~ inpe17$temp_5[match(mun_res, inpe17$codmun)],
      TRUE ~ NA
    ),
    
    temp_d2_res = case_when(
      ano == 2019 ~ inpe19$temp_10[match(mun_res, inpe19$codmun)],
      ano == 2018 ~ inpe18$temp_11[match(mun_res, inpe18$codmun)],
      ano == 2017 ~ inpe17$temp_12[match(mun_res, inpe17$codmun)],
      TRUE ~ NA
    ),
    
    umid_d1_res = case_when(
      ano == 2019 ~ inpe19$umid_3[match(mun_res, inpe19$codmun)],
      ano == 2018 ~ inpe18$umid_4[match(mun_res, inpe18$codmun)],
      ano == 2017 ~ inpe17$umid_5[match(mun_res, inpe17$codmun)],
      TRUE ~ NA
    ),
    
    umid_d2_res = case_when(
      ano == 2019 ~ inpe19$umid_10[match(mun_res, inpe19$codmun)],
      ano == 2018 ~ inpe18$umid_11[match(mun_res, inpe18$codmun)],
      ano == 2017 ~ inpe17$umid_12[match(mun_res, inpe17$codmun)],
      TRUE ~ NA
    )
  )

rm(inpe18, inpe19, inpe17)

# ---------------------------------------------------------------------------- #
#PIB

pib <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/pib.RDS") %>%
  mutate(codmun = as.integer(codmun)) %>%
  rename(mun_prova = codmun)

base_abs <- base_abs %>% 
  left_join(pib, by = c("mun_res" = "mun_prova", "ano"))

rm(pib)
# ---------------------------------------------------------------------------- #

vars_diff <- c(
  "abs",
  "escm", "escp", "pessoa", "empr_dom",
  "n_ban", "n_qua", "n_car", "n_gel", "n_cel",
  "pc", "internet", "renda1", "renda110", "renda10",
  "pibpc",
  "fem", "idade", "ppi",
  "tempd1", "tempd2", "umidd2", "umidd1"
)

base_ag_abs <- base_abs[priv0 == 1, .(
  abs    = mean(abs, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  obs = .N
), by = .(mun_res, ano)] %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_ag_abs)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_ag_abs[[v1]] <- ifelse(base_ag_abs$ano == 2018, base_ag_abs[[v]], NA_real_)
  
  base_ag_abs[[v2]] <- ave(
    base_ag_abs[[v1]], 
    base_ag_abs$mun_res, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_ag_abs[[dv]] <- base_ag_abs[[v]] - base_ag_abs[[v2]]
  
  base_ag_abs[[dv]][!is.finite(base_ag_abs[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_ag_abs), value = TRUE)
base_ag_abs <- base_ag_abs %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) 


rm(vars_diff, v, v1, v2)
# ---------------------------------------------------------------------------- #
## 7.1 Reg ----
# ---------------------------------------------------------------------------- #

base_a <- base_ag_abs

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
base_a <- base_a %>% 
  left_join(mun_hv %>% rename(dist_hv_res = dist_hv_border) %>% #Coordinates of Residency 
              rename(lat_res = lat, 
                     lon_res = lon,
                     seg_res = seg), 
            by = c("mun_res" = "co_municipio")) 




result <- list()

d_list <- c("dabs")


for (i in d_list){
  
  #Com Controles
  ef <- dummy_cols(base_a$seg_res[base_a$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  result[[as.character(paste0("nc_",i,"|TC"))]] <-
    rdrobust(
      y = base_a[[i]][base_a$ano == 2019],
      x = base_a$dist_hv_res[base_a$ano == 2018],
      c = 0,
      p = 1,
      h = bw_main_r,
      b = bw_bias_r,
      cluster = base_a$seg_res[base_a$ano == 2018],
      weights = base_a$obs[base_a$ano == 2018],
      vce = "hc0",
      covs = cbind(
        ef, 
        base_a$lat_res[base_a$ano == 2018],
        base_a$lon_res[base_a$ano == 2018],
        #All
        base_a$dtempd1[base_a$ano == 2019], #Temperature
        base_a$descm[base_a$ano == 2019], #mother educ
        base_a$dn_ban[base_a$ano == 2019], #bathrooms
        base_a$dumidd1[base_a$ano == 2019], #Humidity d1
        base_a$dumidd2[base_a$ano == 2019], #Humidty d2
        base_a$dfem[base_a$ano == 2019], #Female
        base_a$dppi[base_a$ano == 2019], #PPI
        base_a$didade[base_a$ano == 2019], #Age
        base_a$descp[base_a$ano == 2019], #father educ
        
        base_a$dpessoa[base_a$ano == 2019], #people in household
        base_a$dn_qua[base_a$ano == 2019], #houses
        base_a$dn_car[base_a$ano == 2019], #cars
        base_a$dn_gel[base_a$ano == 2019], #geladeira
        base_a$dn_cel[base_a$ano == 2019], #cel
        base_a$dpc[base_a$ano == 2019],    #pc
        base_a$dinternet[base_a$ano == 2019], #internet
        base_a$dempr_dom[base_a$ano == 2019], #Housekeeping
        
        base_a$drenda1[base_a$ano == 2019], #wage < 1MW
        base_a$drenda110[base_a$ano == 2019], #wage 1MW - 10MW
        base_a$drenda10[base_a$ano == 2019], #wage > 10MW
        base_a$dpibpc[base_a$ano == 2019], #pibpc
        
        base_a$dtempd2[base_a$ano == 2019], #temp2
        
        #Fuso
        base_a$h13[base_a$ano == 2019],
        base_a$h12[base_a$ano == 2019],
        base_a$h11[base_a$ano == 2019]
      )
    )
  
  
}
rm(ef, i)


# ---------------------------------------------------------------------------- #
### 7.1.1 Census vs. Inscription ----
# ---------------------------------------------------------------------------- #
#### 7.1.1.1 Data ----
# ---------------------------------------------------------------------------- #
educ <- readRDS("Z:/Tuffy/Paper - HV/Bases/census_students.RDS")




base_a <- base_a %>% 
  left_join(educ, by = c("mun_res" = "co_municipio", "ano")) %>% 
  group_by(mun_res) %>% 
  mutate(    frac2018 = obs[ano == 2018]/alunos[ano == 2018],
             frac2019 = obs[ano == 2019]/alunos[ano == 2019],
             
             dlog_frac = log(frac2019) - log(frac2018)) %>% 
  ungroup()

# ---------------------------------------------------------------------------- #
#### 7.1.1.2 Regression Diff-in-diff ----
# ---------------------------------------------------------------------------- #

d_list <- c("dlog_frac")


for (i in d_list){
  
  
  temp <- base_a %>%
    group_by(mun_res) %>% 
    filter(n_distinct(ano) == 2) %>%
    ungroup() %>% 
    filter(
      !is.na(seg_res) &
        !is.na(lat_res) &
        !is.na(lon_res) &
        !is.na(dist_hv_res) &
        !is.na(obs) &
        !is.na(alunos)
    ) %>% 
    filter(#dist_hv_border <= bw_main_r,
      ano == 2019)
  
  #Com Controles
  
  ef <- dummy_cols(temp$seg_res)
  ef <- ef %>% select(-1,-2)
  
  
  result[[as.character(paste0("nc_",i,"|TC"))]] <-
    rdrobust(
      y = temp[[i]][temp$ano == 2019],
      x = temp$dist_hv_res,
      c = 0,
      p = 1,
      h = bw_main_r,
      b = bw_bias_r,
      cluster = temp$seg_res,
      vce = "hc0",
      covs = cbind(
        ef, 
        temp$lat_res,
        temp$lon_res,
        #All
        temp$dtempd1, #Temperature
        temp$descm, #mother educ
        temp$dn_ban, #bathrooms
        temp$dumidd1, #Humidity d1
        temp$dumidd2, #Humidty d2
        temp$dfem, #Female
        temp$dppi, #PPI
        temp$didade, #Age
        temp$descp, #father educ
        
        temp$dpessoa, #people in household
        temp$dn_qua, #houses
        temp$dn_car, #cars
        temp$dn_gel, #geladeira
        temp$dn_cel, #cel
        temp$dpc,    #pc
        temp$dinternet, #internet
        temp$dempr_dom, #housekeeping
        
        temp$drenda1, #wage < 1MW
        temp$drenda110, #wage 1MW - 10MW
        temp$drenda10, #wage > 10MW
        temp$dpibpc, #pibpc
        
        temp$dtempd2, #temp2
        
        #Fuso
        temp$h13,
        temp$h12,
        temp$h11
      )
    )
  
  
}
rm(ef, i)






# ---------------------------------------------------------------------------- #
## 7.2 Result Table ----
# ---------------------------------------------------------------------------- #

fmt_est_abs <- function(est, pv) {
  paste0(
    formatC(est, digits = 4, format = "f"),
    ifelse(pv < 0.01, "**",
           ifelse(pv < 0.05, "*",
                  ifelse(pv < 0.10, " ", "")))
  )
}

fmt_se_abs <- function(se) {
  paste0("(", formatC(se, digits = 4, format = "f"), ")")
}

dias <- data.frame(
  coef   = sapply(result, function(x) x$coef[3]),
  se     = sapply(result, function(x) x$se[3]),
  pv     = sapply(result, function(x) x$pv[3]),
  n_left = sapply(result, function(x) x$N_h[1]),
  n_rght = sapply(result, function(x) x$N_h[2]),
  bw     = sapply(result, function(x) x$bws[1, 1]),
  totr   = sapply(result, function(x) x$N[2]),
  totl   = sapply(result, function(x) x$N[1])
)

dias


result <- data.frame(
  ` ` = c(
    "2019 - 2018",
    " ",
    "N = N$_L$, N$_R$",
    "BW",
    "Municipalities"
  ),
  `(1)` = c(#Test Takers
    fmt_est_abs(dias$coef[2], dias$pv[2]),
    fmt_se_abs(dias$se[2]),
    fmt_npair(dias$n_left[2], dias$n_rght[2]),
    fmt_bw(dias$bw[2]),
    formatC(dias$totr[2] + dias$totl[2], format = "d", big.mark = ",")
    
  ),
  `(2)` = c(#Abst.
    fmt_est_abs(dias$coef[1], dias$pv[1]),
    fmt_se_abs(dias$se[1]),
    fmt_npair(dias$n_left[1], dias$n_rght[1]),
    fmt_bw(dias$bw[1]),
    formatC(dias$totr[1] + dias$totl[1], format = "d", big.mark = ",")
  ),
  
  check.names = FALSE,
  stringsAsFactors = FALSE
)



# ---------------------------------------------------------------------------- #
### 7.2.1 Latex ----
# ---------------------------------------------------------------------------- #



latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lccccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/Abs_Dias_v1.tex")
#N = 5559
rm(latex_table, result, dias, d_list, p_list, names, base_abs, educ, temp,
   base_mun, base_a, base_ag_abs)


# ---------------------------------------------------------------------------- #
# 8. Heterogeneity ---- 
# ---------------------------------------------------------------------------- #
## 8.1 Race ----
# ---------------------------------------------------------------------------- #
### 8.1.1 White and Yellow -----
# ---------------------------------------------------------------------------- #
base <- base %>% ungroup()

base_ab <- base %>%  
  filter(
    raca %in% c("B", "E"),
    ano %in% c(2018:2019)
  ) %>% setDT()

base_ab <- base_ab %>% 
  filter(priv0 == 1) %>% 
  group_by(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res) %>% 
  summarise(
    media       = mean(media, na.rm = TRUE),
    obs         = n(),
    .groups     = "drop" #ungroup
  ) %>% 
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
  select(-c(dup2, dup1, v1_nota, v2_nota)) %>% 
  left_join(base_aux, by = c("mun_res", "ano"))

# ---------------------------------------------------------------------------- #
### 8.1.2 PPI ----
# ---------------------------------------------------------------------------- #
base_ppi <- base %>% 
  filter(
    raca %in% c("C", "D", "F"),
    ano %in% c(2018:2019)
  ) %>% setDT()


base_ppi <- base_ppi %>% 
  filter(priv0 == 1) %>% 
  group_by(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res) %>% 
  summarise(
    media       = mean(media, na.rm = TRUE),
    obs         = n(),
    .groups     = "drop" #ungroup
  ) %>% 
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
  select(-c(dup2, dup1, v1_nota, v2_nota)) %>% 
  left_join(base_aux, by = c("mun_res", "ano"))


# ---------------------------------------------------------------------------- #
### 8.1.3 Regression by race ----
# ---------------------------------------------------------------------------- #

rlist <- list()


ano_list <- 2018

for(ano_ref in ano_list) {
  
  ano_comp <- ano_ref + 1
  
  
  for(df in c("base_ab", "base_ppi")) {
    
    base_a <- get(df)
    
    #Com controles
    ef <- dummy_cols(base_a$seg_res[base_a$ano == ano_ref])
    ef <- ef %>% select(-1,-2)
    
    rlist[[as.character(paste0(df,"_",ano_comp,"-",ano_ref,"C|TC"))]] <- rdrobust(
      y = base_a$d.media[base_a$ano == ano_comp],
      x = base_a$dist_hv_res[base_a$ano == ano_ref],
      c = 0,
      p =1,
      cluster = base_a$seg_res[base_a$ano == ano_ref],
      weights = base_a$obs[base_a$ano == ano_ref],
      vce = "hc0",
      covs = cbind(
        ef,
        base_a$lat_res[base_a$ano == 2018],
        base_a$lon_res[base_a$ano == 2018],
        #All
        base_a$dtempd1[base_a$ano == 2019], #Temperature
        base_a$descm[base_a$ano == 2019], #mother educ
        base_a$dn_ban[base_a$ano == 2019], #bathrooms
        base_a$dumidd1[base_a$ano == 2019], #Humidity d1
        base_a$dumidd2[base_a$ano == 2019], #Humidty d2
        base_a$dfem[base_a$ano == 2019], #Female
        base_a$dppi[base_a$ano == 2019], #PPI
        base_a$didade[base_a$ano == 2019], #Age
        base_a$descp[base_a$ano == 2019], #father educ
        
        base_a$dpessoa[base_a$ano == 2019], #people in household
        base_a$dn_qua[base_a$ano == 2019], #houses
        base_a$dn_car[base_a$ano == 2019], #cars
        base_a$dn_gel[base_a$ano == 2019], #geladeira
        base_a$dn_cel[base_a$ano == 2019], #cel
        base_a$dpc[base_a$ano == 2019],    #pc
        base_a$dinternet[base_a$ano == 2019], #internet
        base_a$dempr_dom[base_a$ano == 2019], #housekeeping
        
        base_a$drenda1[base_a$ano == 2019], #wage < 1MW
        base_a$drenda110[base_a$ano == 2019], #wage 1MW - 10MW
        base_a$drenda10[base_a$ano == 2019], #wage > 10MW
        base_a$dpibpc[base_a$ano == 2019], #pibpc
        
        base_a$dtempd2[base_a$ano == 2019], #temp2
        
        #Fuso
        base_a$h13[base_a$ano == 2019],
        base_a$h12[base_a$ano == 2019],
        base_a$h11[base_a$ano == 2019]
      )
    )
    
    #Banda Fixa
    rlist[[as.character(paste0(df,"_",ano_comp,"-",ano_ref,"BW|TC"))]] <- rdrobust(
      y = base_a$d.media[base_a$ano == ano_comp],
      x = base_a$dist_hv_res[base_a$ano == ano_ref],
      c = 0,
      p = 1,
      h = bw_main_r,
      b = bw_bias_r,
      cluster = base_a$seg_res[base_a$ano == ano_ref],
      weights = base_a$obs[base_a$ano == ano_ref],
      vce = "hc0",
      covs = cbind(
        ef,
        base_a$lat_res[base_a$ano == 2018],
        base_a$lon_res[base_a$ano == 2018],
        #All
        base_a$dtempd1[base_a$ano == 2019], #Temperature
        base_a$descm[base_a$ano == 2019], #mother educ
        base_a$dn_ban[base_a$ano == 2019], #bathrooms
        base_a$dumidd1[base_a$ano == 2019], #Humidity d1
        base_a$dumidd2[base_a$ano == 2019], #Humidty d2
        base_a$dfem[base_a$ano == 2019], #Female
        base_a$dppi[base_a$ano == 2019], #PPI
        base_a$didade[base_a$ano == 2019], #Age
        base_a$descp[base_a$ano == 2019], #father educ
        
        base_a$dpessoa[base_a$ano == 2019], #people in household
        base_a$dn_qua[base_a$ano == 2019], #houses
        base_a$dn_car[base_a$ano == 2019], #cars
        base_a$dn_gel[base_a$ano == 2019], #geladeira
        base_a$dn_cel[base_a$ano == 2019], #cel
        base_a$dpc[base_a$ano == 2019],    #pc
        base_a$dinternet[base_a$ano == 2019], #internet
        base_a$dempr_dom[base_a$ano == 2019], #housekeeping
        
        base_a$drenda1[base_a$ano == 2019], #wage < 1MW
        base_a$drenda110[base_a$ano == 2019], #wage 1MW - 10MW
        base_a$drenda10[base_a$ano == 2019], #wage > 10MW
        base_a$dpibpc[base_a$ano == 2019], #pibpc
        
        base_a$dtempd2[base_a$ano == 2019], #temp2
        
        #Fuso
        base_a$h13[base_a$ano == 2019],
        base_a$h12[base_a$ano == 2019],
        base_a$h11[base_a$ano == 2019]
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
  coef   = sapply(rlist, function(x) x$coef[3]),
  se     = sapply(rlist, function(x) x$se[3]),
  pv     = sapply(rlist, function(x) x$pv[3]),
  n_left = sapply(rlist, function(x) x$N_h[1]),
  n_rght = sapply(rlist, function(x) x$N_h[2]),
  bw     = sapply(rlist, function(x) x$bws[1, 1]),
  totr   = sapply(rlist, function(x) x$N[2]),
  totl   = sapply(rlist, function(x) x$N[1])
)



result <- data.frame(
  ` ` = c(
    "White and Yellow",
    " ", " ", " ",
    "Afro-Brazilians and Indigenous",
    " ", " ", " "
  ),
  `(1)` = c(#Low
    fmt_est(t10cc$coef[2], t10cc$pv[2]),
    fmt_se(t10cc$se[2]),
    paste0("N$_L$ = ", fmt_n(t10cc$n_left[2]),", N$_R$ = ", fmt_n(t10cc$n_rght[2])),
    paste0("BW = ", fmt_bw(t10cc$bw[2])),
    #High
    fmt_est(t10cc$coef[4], t10cc$pv[4]),
    fmt_se(t10cc$se[4]),
    paste0("N$_L$ = ", fmt_n(t10cc$n_left[4]),", N$_R$ = ", fmt_n(t10cc$n_rght[4])),
    paste0("BW = ", fmt_bw(t10cc$bw[4]))
    
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------- #
# Latex 
# ---------------------------------------------------------------------------- #


# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/Raca_v1.tex")
#5417, 5336
rm(base_ab, base_ppi, result, rlist, t10cc, latex_table)

# ---------------------------------------------------------------------------- #
## 8.2 Sex ----
# ---------------------------------------------------------------------------- #
### 8.2.1 FEM ----
# ---------------------------------------------------------------------------- #
base_fem <- base %>% 
  filter(fem == 1,
         ano %in% c(2018, 2019)) %>% 
  setDT()


base_fem <- base_fem %>% 
  filter(priv0 == 1) %>% 
  group_by(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res) %>% 
  summarise(
    media       = mean(media, na.rm = TRUE),
    obs         = n(),
    .groups     = "drop" #ungroup
  ) %>% 
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
  select(-c(dup2, dup1, v1_nota, v2_nota)) %>% 
  left_join(base_aux, by = c("mun_res", "ano"))

# ---------------------------------------------------------------------------- #
### 8.2.2 MASC ----
# ---------------------------------------------------------------------------- #

base_masc <- base %>% 
  filter(fem == 0,
         ano %in% c(2018,2019)) %>%
  setDT()

base_masc <- base_masc %>% 
  filter(priv0 == 1) %>% 
  group_by(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res) %>% 
  summarise(
    media       = mean(media, na.rm = TRUE),
    obs         = n(),
    .groups     = "drop" #ungroup
  ) %>% 
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
  select(-c(dup2, dup1, v1_nota, v2_nota)) %>% 
  left_join(base_aux, by = c("mun_res", "ano"))

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
    ef <- dummy_cols(base_a$seg_res[base_a$ano == ano_ref])
    ef <- ef %>% select(-1,-2)
    
    rlist[[as.character(paste0(df,"_",ano_comp,"-",ano_ref,"C|TC"))]] <- rdrobust(
      y = base_a$d.media[base_a$ano == ano_comp],
      x = base_a$dist_hv_res[base_a$ano == ano_ref],
      c = 0,
      p = 1,
      cluster = base_a$seg_res[base_a$ano == ano_ref],
      weights = base_a$obs[base_a$ano == ano_ref],
      vce = "hc0",
      covs = cbind(
        ef,
        base_a$lat_res[base_a$ano == 2018],
        base_a$lon_res[base_a$ano == 2018],
        #All
        base_a$dtempd1[base_a$ano == 2019], #Temperature
        base_a$descm[base_a$ano == 2019], #mother educ
        base_a$dn_ban[base_a$ano == 2019], #bathrooms
        base_a$dumidd1[base_a$ano == 2019], #Humidity d1
        base_a$dumidd2[base_a$ano == 2019], #Humidty d2
        base_a$dfem[base_a$ano == 2019], #Female
        base_a$dppi[base_a$ano == 2019], #PPI
        base_a$didade[base_a$ano == 2019], #Age
        base_a$descp[base_a$ano == 2019], #father educ
        
        base_a$dpessoa[base_a$ano == 2019], #people in household
        base_a$dn_qua[base_a$ano == 2019], #houses
        base_a$dn_car[base_a$ano == 2019], #cars
        base_a$dn_gel[base_a$ano == 2019], #geladeira
        base_a$dn_cel[base_a$ano == 2019], #cel
        base_a$dpc[base_a$ano == 2019],    #pc
        base_a$dinternet[base_a$ano == 2019], #internet
        base_a$dempr_dom[base_a$ano == 2019], #housekeeper
        
        base_a$drenda1[base_a$ano == 2019], #wage < 1MW
        base_a$drenda110[base_a$ano == 2019], #wage 1MW - 10MW
        base_a$drenda10[base_a$ano == 2019], #wage > 10MW
        base_a$dpibpc[base_a$ano == 2019], #pibpc
        
        base_a$dtempd2[base_a$ano == 2019], #temp2
        
        #Fuso
        base_a$h13[base_a$ano == 2019],
        base_a$h12[base_a$ano == 2019],
        base_a$h11[base_a$ano == 2019]
      )
    )
    
    #Banda Fixa
    rlist[[as.character(paste0(df,"_",ano_comp,"-",ano_ref,"BW|TC"))]] <- rdrobust(
      y = base_a$d.media[base_a$ano == ano_comp],
      x = base_a$dist_hv_res[base_a$ano == ano_ref],
      c = 0,
      p = 1,
      h = bw_main_r,
      b = bw_bias_r,
      cluster = base_a$seg_res[base_a$ano == ano_ref],
      weights = base_a$obs[base_a$ano == ano_ref],
      vce = "hc0",
      covs = cbind(
        ef,
        base_a$lat_res[base_a$ano == 2018],
        base_a$lon_res[base_a$ano == 2018],
        #All
        base_a$dtempd1[base_a$ano == 2019], #Temperature
        base_a$descm[base_a$ano == 2019], #mother educ
        base_a$dn_ban[base_a$ano == 2019], #bathrooms
        base_a$dumidd1[base_a$ano == 2019], #Humidity d1
        base_a$dumidd2[base_a$ano == 2019], #Humidty d2
        base_a$dfem[base_a$ano == 2019], #Female
        base_a$dppi[base_a$ano == 2019], #PPI
        base_a$didade[base_a$ano == 2019], #Age
        base_a$descp[base_a$ano == 2019], #father educ
        
        base_a$dpessoa[base_a$ano == 2019], #people in household
        base_a$dn_qua[base_a$ano == 2019], #houses
        base_a$dn_car[base_a$ano == 2019], #cars
        base_a$dn_gel[base_a$ano == 2019], #geladeira
        base_a$dn_cel[base_a$ano == 2019], #cel
        base_a$dpc[base_a$ano == 2019],    #pc
        base_a$dinternet[base_a$ano == 2019], #internet
        base_a$dempr_dom[base_a$ano == 2019], #house
        
        base_a$drenda1[base_a$ano == 2019], #wage < 1MW
        base_a$drenda110[base_a$ano == 2019], #wage 1MW - 10MW
        base_a$drenda10[base_a$ano == 2019], #wage > 10MW
        base_a$dpibpc[base_a$ano == 2019], #pibpc
        
        base_a$dtempd2[base_a$ano == 2019], #temp2
        
        #Fuso
        base_a$h13[base_a$ano == 2019],
        base_a$h12[base_a$ano == 2019],
        base_a$h11[base_a$ano == 2019]
      )
    )
    
    message("Finished for sex group: ",df)
    
  }
  
}
rm(ef, ano_ref, ano_comp)



# ---------------------------------------------------------------------------- #
###8.2.4 Result Table ----
# ---------------------------------------------------------------------------- #

t10cc <- data.frame(
  coef   = sapply(rlist, function(x) x$coef[3]),
  se     = sapply(rlist, function(x) x$se[3]),
  pv     = sapply(rlist, function(x) x$pv[3]),
  n_left = sapply(rlist, function(x) x$N_h[1]),
  n_rght = sapply(rlist, function(x) x$N_h[2]),
  bw     = sapply(rlist, function(x) x$bws[1, 1]),
  totr   = sapply(rlist, function(x) x$N[2]),
  totl   = sapply(rlist, function(x) x$N[1])
)




result <- data.frame(
  ` ` = c(
    "Female",
    " ", " ", " ",
    "Male",
    " ", " ", " "
  ),
  `(1)` = c(#Low
    fmt_est(t10cc$coef[2], t10cc$pv[2]),
    fmt_se(t10cc$se[2]),
    paste0("N$_L$ = ", fmt_n(t10cc$n_left[2]),", N$_R$ = ", fmt_n(t10cc$n_rght[2])),
    paste0("BW = ", fmt_bw(t10cc$bw[2])),
    #High
    fmt_est(t10cc$coef[4], t10cc$pv[4]),
    fmt_se(t10cc$se[4]),
    paste0("N$_L$ = ", fmt_n(t10cc$n_left[4]),", N$_R$ = ", fmt_n(t10cc$n_rght[4])),
    paste0("BW = ", fmt_bw(t10cc$bw[4]))
    
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------- #
# Latex 
# ---------------------------------------------------------------------------- #


# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/Sexo_v1.tex")

rm(base_masc, result, base_fem, rlist, t10cc, df, latex_table)


# ---------------------------------------------------------------------------- #
## 8.3 Mae Educ ----
# ---------------------------------------------------------------------------- #
###8.3.1 High ----
# ---------------------------------------------------------------------------- #
base_high <- base %>% 
  filter(
    esc_mae %in% c("D","E","F"),
    ano %in% c(2018, 2019)
    
  ) %>% setDT()


base_high <- base_high %>% 
  filter(priv0 == 1) %>% 
  group_by(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res) %>% 
  summarise(
    media       = mean(media, na.rm = TRUE),
    obs         = n(),
    .groups     = "drop" #ungroup
  ) %>% 
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
  select(-c(dup2, dup1, v1_nota, v2_nota)) %>% 
  left_join(base_aux, by = c("mun_res", "ano"))

# ---------------------------------------------------------------------------- #
### 8.3.2 Low ----
# ---------------------------------------------------------------------------- #
base_low <- base %>% 
  filter(
    esc_mae %in% c("A", "B", "C"),
    ano %in% c(2018, 2019)
  ) %>% setDT()

base_low <- base_low %>% 
  filter(priv0 == 1) %>% 
  group_by(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res) %>% 
  summarise(
    media       = mean(media, na.rm = TRUE),
    obs         = n(),
    .groups     = "drop" #ungroup
  ) %>% 
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
  select(-c(dup2, dup1, v1_nota, v2_nota)) %>% 
  left_join(base_aux, by = c("mun_res", "ano"))

# ---------------------------------------------------------------------------- #
###8.3.3 Regression ----
# ---------------------------------------------------------------------------- #
rlist <- list()


ano_list <- 2018

for(ano_ref in ano_list) {
  
  
  ano_comp <- ano_ref + 1
  for(df in c("base_low", "base_high")) {
    
    base_a <- get(df)
    
    
    #Com controles
    ef <- dummy_cols(base_a$seg_res[base_a$ano == ano_ref])
    ef <- ef %>% select(-1,-2)
    
    rlist[[as.character(paste0(df,"_",ano_comp,"-",ano_ref,"C|TC"))]] <- rdrobust(
      y = base_a$d.media[base_a$ano == ano_comp],
      x = base_a$dist_hv_res[base_a$ano == ano_ref],
      c = 0,
      p = 1,
      cluster = base_a$seg_res[base_a$ano == ano_ref],
      weights = base_a$obs[base_a$ano == ano_ref],
      vce = "hc0",
      covs = cbind(
        ef,
        base_a$lat_res[base_a$ano == 2018],
        base_a$lon_res[base_a$ano == 2018],
        #All
        base_a$dtempd1[base_a$ano == 2019], #Temperature
        base_a$descm[base_a$ano == 2019], #mother educ
        base_a$dn_ban[base_a$ano == 2019], #bathrooms
        base_a$dumidd1[base_a$ano == 2019], #Humidity d1
        base_a$dumidd2[base_a$ano == 2019], #Humidty d2
        base_a$dfem[base_a$ano == 2019], #Female
        base_a$dppi[base_a$ano == 2019], #PPI
        base_a$didade[base_a$ano == 2019], #Age
        base_a$descp[base_a$ano == 2019], #father educ
        
        base_a$dpessoa[base_a$ano == 2019], #people in household
        base_a$dn_qua[base_a$ano == 2019], #houses
        base_a$dn_car[base_a$ano == 2019], #cars
        base_a$dn_gel[base_a$ano == 2019], #geladeira
        base_a$dn_cel[base_a$ano == 2019], #cel
        base_a$dpc[base_a$ano == 2019],    #pc
        base_a$dinternet[base_a$ano == 2019], #internet
        base_a$dempr_dom[base_a$ano == 2019], #house
        
        
        base_a$drenda1[base_a$ano == 2019], #wage < 1MW
        base_a$drenda110[base_a$ano == 2019], #wage 1MW - 10MW
        base_a$drenda10[base_a$ano == 2019], #wage > 10MW
        base_a$dpibpc[base_a$ano == 2019], #pibpc
        
        base_a$dtempd2[base_a$ano == 2019], #temp2
        
        #Fuso
        base_a$h13[base_a$ano == 2019],
        base_a$h12[base_a$ano == 2019],
        base_a$h11[base_a$ano == 2019]
      )
    )
    
    
    #Banda Fixa
    rlist[[as.character(paste0(df,"_",ano_comp,"-",ano_ref,"BW|TC"))]] <- rdrobust(
      y = base_a$d.media[base_a$ano == ano_comp],
      x = base_a$dist_hv_res[base_a$ano == ano_ref],
      c = 0,
      p = 1,
      h = bw_main_r,
      b = bw_bias_r,
      cluster = base_a$seg_res[base_a$ano == ano_ref],
      weights = base_a$obs[base_a$ano == ano_ref],
      vce = "hc0",
      covs = cbind(
        ef,
        base_a$lat_res[base_a$ano == 2018],
        base_a$lon_res[base_a$ano == 2018],
        #All
        base_a$dtempd1[base_a$ano == 2019], #Temperature
        base_a$descm[base_a$ano == 2019], #mother educ
        base_a$dn_ban[base_a$ano == 2019], #bathrooms
        base_a$dumidd1[base_a$ano == 2019], #Humidity d1
        base_a$dumidd2[base_a$ano == 2019], #Humidty d2
        base_a$dfem[base_a$ano == 2019], #Female
        base_a$dppi[base_a$ano == 2019], #PPI
        base_a$didade[base_a$ano == 2019], #Age
        base_a$descp[base_a$ano == 2019], #father educ
        
        base_a$dpessoa[base_a$ano == 2019], #people in household
        base_a$dn_qua[base_a$ano == 2019], #houses
        base_a$dn_car[base_a$ano == 2019], #cars
        base_a$dn_gel[base_a$ano == 2019], #geladeira
        base_a$dn_cel[base_a$ano == 2019], #cel
        base_a$dpc[base_a$ano == 2019],    #pc
        base_a$dinternet[base_a$ano == 2019], #internet
        base_a$dempr_dom[base_a$ano == 2019], #house
        
        base_a$drenda1[base_a$ano == 2019], #wage < 1MW
        base_a$drenda110[base_a$ano == 2019], #wage 1MW - 10MW
        base_a$drenda10[base_a$ano == 2019], #wage > 10MW
        base_a$dpibpc[base_a$ano == 2019], #pibpc
        
        base_a$dtempd2[base_a$ano == 2019], #temp2
        
        #Fuso
        base_a$h13[base_a$ano == 2019],
        base_a$h12[base_a$ano == 2019],
        base_a$h11[base_a$ano == 2019]
      )
    )
    
    
  }
  
}
rm(ef, ano_ref, ano_comp, ano_list)


# ---------------------------------------------------------------------------- #
###8.3.4 Result Table ----
# ---------------------------------------------------------------------------- #
t10cc <- data.frame(
  coef   = sapply(rlist, function(x) x$coef[3]),
  se     = sapply(rlist, function(x) x$se[3]),
  pv     = sapply(rlist, function(x) x$pv[3]),
  n_left = sapply(rlist, function(x) x$N_h[1]),
  n_rght = sapply(rlist, function(x) x$N_h[2]),
  bw     = sapply(rlist, function(x) x$bws[1, 1]),
  totr   = sapply(rlist, function(x) x$N[2]),
  totl   = sapply(rlist, function(x) x$N[1])
)




result <- data.frame(
  ` ` = c(
    "Low Education",
    " ", " ", " ",
    "High Education",
    " ", " ", " "
  ),
  `(1)` = c(#Low
    fmt_est(t10cc$coef[2], t10cc$pv[2]),
    fmt_se(t10cc$se[2]),
    paste0("N$_L$ = ", fmt_n(t10cc$n_left[2]),", N$_R$ = ", fmt_n(t10cc$n_rght[2])),
    paste0("BW = ", fmt_bw(t10cc$bw[2])),
    #High
    fmt_est(t10cc$coef[4], t10cc$pv[4]),
    fmt_se(t10cc$se[4]),
    paste0("N$_L$ = ", fmt_n(t10cc$n_left[4]),", N$_R$ = ", fmt_n(t10cc$n_rght[4])),
    paste0("BW = ", fmt_bw(t10cc$bw[4]))
    
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------- #
# Latex 
# ---------------------------------------------------------------------------- #


# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/Mae_Education_v1.tex")

rm(result, rlist, t10cc, df, base_high, base_low, latex_table, base_a, ano_list, names, names2)

# ---------------------------------------------------------------------------- #
## 8.4 Age ----
# ---------------------------------------------------------------------------- #
###8.4.1 Data ----
# ---------------------------------------------------------------------------- #

#Age filter dummy
base <- base %>%
  mutate(
    old = ifelse(
      idade > 18 & conclusao == 2, 1,
      ifelse( idade %in% c(17, 18), 0, NA))
  ) %>% setDT()

# ------------------------ #
### 8.4.2 Reression. ----
# ------------------------ #
rlist <- list()

for (j in c(0:1)){
  
  base_y <- base %>%
    filter( old == j) %>% 
    filter(ano %in% c(2018:2019))
  
  base_a <- base_y[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
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
    select(-c(dup2, dup1, v1_nota, v2_nota)) %>% 
    left_join(base_aux, by = c("mun_res", "ano"))
  
  rm(base_y)
  
  
  
  ef <- dummy_cols(base_a$seg_res[base_a$ano == 2018])
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
    x = base_a$dist_hv_res[base_a$ano == 2018],
    c = 0,
    cluster = base_a$seg_res[base_a$ano == 2018],
    weights = base_a$obs[base_a$ano == 2018],
    vce = "hc0",
    covs = cbind(
      ef,
      base_a$lat_res[base_a$ano == 2018],
      base_a$lon_res[base_a$ano == 2018],
      #All
      base_a$dtempd1[base_a$ano == 2019], #Temperature
      base_a$descm[base_a$ano == 2019], #mother educ
      base_a$dn_ban[base_a$ano == 2019], #bathrooms
      base_a$dumidd1[base_a$ano == 2019], #Humidity d1
      base_a$dumidd2[base_a$ano == 2019], #Humidty d2
      base_a$dfem[base_a$ano == 2019], #Female
      base_a$dppi[base_a$ano == 2019], #PPI
      base_a$didade[base_a$ano == 2019], #Age
      base_a$descp[base_a$ano == 2019], #father educ
      
      base_a$dpessoa[base_a$ano == 2019], #people in household
      base_a$dn_qua[base_a$ano == 2019], #houses
      base_a$dn_car[base_a$ano == 2019], #cars
      base_a$dn_gel[base_a$ano == 2019], #geladeira
      base_a$dn_cel[base_a$ano == 2019], #cel
      base_a$dpc[base_a$ano == 2019],    #pc
      base_a$dinternet[base_a$ano == 2019], #internet
      base_a$dempr_dom[base_a$ano == 2019], #house
      
      base_a$drenda1[base_a$ano == 2019], #wage < 1MW
      base_a$drenda110[base_a$ano == 2019], #wage 1MW - 10MW
      base_a$drenda10[base_a$ano == 2019], #wage > 10MW
      base_a$dpibpc[base_a$ano == 2019], #pibpc
      
      base_a$dtempd2[base_a$ano == 2019], #temp2
      
      #Fuso
      base_a$h13[base_a$ano == 2019],
      base_a$h12[base_a$ano == 2019],
      base_a$h11[base_a$ano == 2019]
    )
  )
  
  
  rlist[[as.character(paste0("old =",j,"|BW"))]] <- rdrobust(
    y = base_a$d.media[base_a$ano == 2019],
    x = base_a$dist_hv_res[base_a$ano == 2018],
    c = 0,
    cluster = base_a$seg_res[base_a$ano == 2018],
    weights = base_a$obs[base_a$ano == 2018],
    vce = "hc0",
    h = bw_main_r,
    b = bw_bias_r,
    covs = cbind(
      ef,
      base_a$lat_res[base_a$ano == 2018],
      base_a$lon_res[base_a$ano == 2018],
      #All
      base_a$dtempd1[base_a$ano == 2019], #Temperature
      base_a$descm[base_a$ano == 2019], #mother educ
      base_a$dn_ban[base_a$ano == 2019], #bathrooms
      base_a$dumidd1[base_a$ano == 2019], #Humidity d1
      base_a$dumidd2[base_a$ano == 2019], #Humidty d2
      base_a$dfem[base_a$ano == 2019], #Female
      base_a$dppi[base_a$ano == 2019], #PPI
      base_a$didade[base_a$ano == 2019], #Age
      base_a$descp[base_a$ano == 2019], #father educ
      
      base_a$dpessoa[base_a$ano == 2019], #people in household
      base_a$dn_qua[base_a$ano == 2019], #houses
      base_a$dn_car[base_a$ano == 2019], #cars
      base_a$dn_gel[base_a$ano == 2019], #geladeira
      base_a$dn_cel[base_a$ano == 2019], #cel
      base_a$dpc[base_a$ano == 2019],    #pc
      base_a$dinternet[base_a$ano == 2019], #internet
      base_a$dempr_dom[base_a$ano == 2019], #house
      
      base_a$drenda1[base_a$ano == 2019], #wage < 1MW
      base_a$drenda110[base_a$ano == 2019], #wage 1MW - 10MW
      base_a$drenda10[base_a$ano == 2019], #wage > 10MW
      base_a$dpibpc[base_a$ano == 2019], #pibpc
      
      base_a$dtempd2[base_a$ano == 2019], #temp2
      
      #Fuso
      base_a$h13[base_a$ano == 2019],
      base_a$h12[base_a$ano == 2019],
      base_a$h11[base_a$ano == 2019]
    )
  )
  
  
}
rm(j, ef)

# ---------------------------------------------------------------------------- #
### 8.4.3 Result Table ----
# ---------------------------------------------------------------------------- #

t10cc <- data.frame(
  coef   = sapply(rlist, function(x) x$coef[3]),
  se     = sapply(rlist, function(x) x$se[3]),
  pv     = sapply(rlist, function(x) x$pv[3]),
  n_left = sapply(rlist, function(x) x$N_h[1]),
  n_rght = sapply(rlist, function(x) x$N_h[2]),
  bw     = sapply(rlist, function(x) x$bws[1, 1]),
  totr   = sapply(rlist, function(x) x$N[2]),
  totl   = sapply(rlist, function(x) x$N[1])
)




result <- data.frame(
  ` ` = c(
    "Younger",
    " ", " ", " ",
    "Older",
    " ", " ", " "
  ),
  `(1)` = c(#Low
    fmt_est(t10cc$coef[2], t10cc$pv[2]),
    fmt_se(t10cc$se[2]),
    paste0("N$_L$ = ", fmt_n(t10cc$n_left[2]),", N$_R$ = ", fmt_n(t10cc$n_rght[2])),
    paste0("BW = ", fmt_bw(t10cc$bw[2])),
    #High
    fmt_est(t10cc$coef[4], t10cc$pv[4]),
    fmt_se(t10cc$se[4]),
    paste0("N$_L$ = ", fmt_n(t10cc$n_left[4]),", N$_R$ = ", fmt_n(t10cc$n_rght[4])),
    paste0("BW = ", fmt_bw(t10cc$bw[4]))
    
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------- #
# Latex 
# ---------------------------------------------------------------------------- #


# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lcc",
  linesep = ""
)

writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/OLDER_vs_young_v1.tex")

rm(base_a, names, result, rlist, tab, latex_table, base_mun)

# ---------------------------------------------------------------------------- #
# 9.Migration ------
# ---------------------------------------------------------------------------- #
## 9.1 Main specification regression ----
# ---------------------------------------------------------------------------- #

var_list <- c(
  "over_hv_border", #without those migrating over the border
  "nonmig1", # MUN PROVA = RESIDENCIA = ESCOLA
  #"nonmig2", # Mun PROVA = RESIDENCIA != ESCOLA
  "nonmig3" #, # MUN PROVA != RESIDENCIA = ESCOLA
  #"nonmig4"  # Mun PROVA = ESCOLA != RESIDENCIA
)

rlist  <- list()

for (i in var_list) {
  
  cat("Rodando para:", i, "\n")
  
  base <- base %>% setDT()
  
  if (i == "over_hv_border") {
    
    base_y <- base[get(i) == 0]
    
  } else {
    
    base_y <- base[get(i) == 1]
  }
  
  base_a <- base_y[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
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
    select(-c(dup2, dup1, v1_nota, v2_nota)) %>% 
    left_join(base_aux, by = c("mun_res", "ano"))
  
  
  
  
  
  ef <- dummy_cols(base_a$seg_res[base_a$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  
  rlist[[as.character(paste0(i,"|C"))]] <- rdrobust(
    y = base_a$d.media[base_a$ano == 2019],
    x = base_a$dist_hv_res[base_a$ano == 2018],
    c = 0,
    p = 1,
    cluster = base_a$seg_res[base_a$ano == 2018],
    weights = base_a$obs[base_a$ano == 2018],
    vce = "hc0",
    covs = cbind(
      ef,
      base_a$lat_res[base_a$ano == 2018],
      base_a$lon_res[base_a$ano == 2018],
      #All
      base_a$dtempd1[base_a$ano == 2019], #Temperature
      base_a$descm[base_a$ano == 2019], #mother educ
      base_a$dn_ban[base_a$ano == 2019], #bathrooms
      base_a$dumidd1[base_a$ano == 2019], #Humidity d1
      base_a$dumidd2[base_a$ano == 2019], #Humidty d2
      base_a$dfem[base_a$ano == 2019], #Female
      base_a$dppi[base_a$ano == 2019], #PPI
      base_a$didade[base_a$ano == 2019], #Age
      base_a$descp[base_a$ano == 2019], #father educ
      
      base_a$dpessoa[base_a$ano == 2019], #people in household
      base_a$dn_qua[base_a$ano == 2019], #houses
      base_a$dn_car[base_a$ano == 2019], #cars
      base_a$dn_gel[base_a$ano == 2019], #geladeira
      base_a$dn_cel[base_a$ano == 2019], #cel
      base_a$dpc[base_a$ano == 2019],    #pc
      base_a$dinternet[base_a$ano == 2019], #internet
      base_a$dempr_dom[base_a$ano == 2019], #house
      
      base_a$drenda1[base_a$ano == 2019], #wage < 1MW
      base_a$drenda110[base_a$ano == 2019], #wage 1MW - 10MW
      base_a$drenda10[base_a$ano == 2019], #wage > 10MW
      base_a$dpibpc[base_a$ano == 2019], #pibpc
      
      base_a$dtempd2[base_a$ano == 2019], #temp2
      
      #Fuso
      base_a$h13[base_a$ano == 2019],
      base_a$h12[base_a$ano == 2019],
      base_a$h11[base_a$ano == 2019]
    )
  )
  
  
  rlist[[as.character(paste0(i,"|BW"))]] <- rdrobust(
    y = base_a$d.media[base_a$ano == 2019],
    x = base_a$dist_hv_res[base_a$ano == 2018],
    c = 0,
    p =1,
    cluster = base_a$seg_res[base_a$ano == 2018],
    weights = base_a$obs[base_a$ano == 2018],
    vce = "hc0",
    h = bw_main_r,
    b = bw_bias_r,
    covs = cbind(
      ef,
      base_a$lat_res[base_a$ano == 2018],
      base_a$lon_res[base_a$ano == 2018],
      #All
      base_a$dtempd1[base_a$ano == 2019], #Temperature
      base_a$descm[base_a$ano == 2019], #mother educ
      base_a$dn_ban[base_a$ano == 2019], #bathrooms
      base_a$dumidd1[base_a$ano == 2019], #Humidity d1
      base_a$dumidd2[base_a$ano == 2019], #Humidty d2
      base_a$dfem[base_a$ano == 2019], #Female
      base_a$dppi[base_a$ano == 2019], #PPI
      base_a$didade[base_a$ano == 2019], #Age
      base_a$descp[base_a$ano == 2019], #father educ
      
      base_a$dpessoa[base_a$ano == 2019], #people in household
      base_a$dn_qua[base_a$ano == 2019], #houses
      base_a$dn_car[base_a$ano == 2019], #cars
      base_a$dn_gel[base_a$ano == 2019], #geladeira
      base_a$dn_cel[base_a$ano == 2019], #cel
      base_a$dpc[base_a$ano == 2019],    #pc
      base_a$dinternet[base_a$ano == 2019], #internet
      base_a$dempr_dom[base_a$ano == 2019], #house
      
      base_a$drenda1[base_a$ano == 2019], #wage < 1MW
      base_a$drenda110[base_a$ano == 2019], #wage 1MW - 10MW
      base_a$drenda10[base_a$ano == 2019], #wage > 10MW
      base_a$dpibpc[base_a$ano == 2019], #pibpc
      
      base_a$dtempd2[base_a$ano == 2019], #temp2
      
      #Fuso
      base_a$h13[base_a$ano == 2019],
      base_a$h12[base_a$ano == 2019],
      base_a$h11[base_a$ano == 2019]
    )
  )
  
  
  
}
rm(ef,i, base_y)

# ---------------------------------------------------------------------------- #
## 9.2 Table ----
# ---------------------------------------------------------------------------- #


t10cc <- data.frame(
  coef   = sapply(rlist, function(x) x$coef[3]),
  se     = sapply(rlist, function(x) x$se[3]),
  pv     = sapply(rlist, function(x) x$pv[3]),
  n_left = sapply(rlist, function(x) x$N_h[1]),
  n_rght = sapply(rlist, function(x) x$N_h[2]),
  bw     = sapply(rlist, function(x) x$bws[1, 1]),
  totr   = sapply(rlist, function(x) x$N[2]),
  totl   = sapply(rlist, function(x) x$N[1])
)




result <- data.frame(
  ` ` = c(
    "No Migration Over the DST Border",
    " ", " ", " ",
    "No Migration",
    " ", " ", " ",
    "Migration to ENEM",
    " ", " ", " " 
  ),
  `(1)` = c(#Over border
    fmt_est(t10cc$coef[1], t10cc$pv[1]),
    fmt_se(t10cc$se[1]),
    paste0("N$_L$ = ", fmt_n(t10cc$n_left[1]),", N$_R$ = ", fmt_n(t10cc$n_rght[1])),
    paste0("BW = ", fmt_bw(t10cc$bw[1])),
    #No mig
    fmt_est(t10cc$coef[3], t10cc$pv[3]),
    fmt_se(t10cc$se[3]),
    paste0("N$_L$ = ", fmt_n(t10cc$n_left[3]),", N$_R$ = ", fmt_n(t10cc$n_rght[3])),
    paste0("BW = ", fmt_bw(t10cc$bw[3])),
    #mig
    fmt_est(t10cc$coef[5], t10cc$pv[5]),
    fmt_se(t10cc$se[5]),
    paste0("N$_L$ = ", fmt_n(t10cc$n_left[5]),", N$_R$ = ", fmt_n(t10cc$n_rght[5])),
    paste0("BW = ", fmt_bw(t10cc$bw[5]))
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)


# ---------------------------------------------------------------------------- #
# Latex 
# ---------------------------------------------------------------------------- #


# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lcc",
  linesep = ""
)

writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/grupos_mig_v2.tex")

rm(latex_table, result, row,  tab, rlist, var_list, t10cc,j)

# ---------------------------------------------------------------------------- #
# 10. Time Zone ----
# ---------------------------------------------------------------------------- #
## 10.1 Data -----
# ---------------------------------------------------------------------------- #

base <- base %>% 
  mutate(aux_res = mun_res %/% 100000)


base_a <- base[priv0 == 1,.(media = mean(media, na.rm = T),
                            obs = .N),
               by = .(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res, aux_res)] %>% 
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
  left_join(base_aux, by = c("mun_res", "ano"))


#Filtro de mun por fuso

# ---------------------------------------------------------------------------- #
## 10.2. Reg Loop ----
# ---------------------------------------------------------------------------- #


result_time <- list()

time_list <- c(13, 12) #11 should be aborted with missing treatmnent obs.

for (i in time_list){
  
  if (i == 13){
    b_temp <- base_a %>%
      group_by(mun_res) %>% 
      filter(aux_res %in% c(52, 53, 31:35, 41:43, 29, 28,
                            27, 26, 25, 24, 23, 22, 21, 17, 15, 16)) %>% 
      arrange(mun_res, ano) %>% 
      mutate(
        lat = as.factor(lat_res),
        lon = as.factor(lon_res)
      )
    
  } else if(i == 12){
    b_temp <- base_a %>%
      group_by(mun_res) %>% 
      filter(aux_res %in% c(51, 50, 11, 14) | aux_res == 13 &
               !mun_res %in%
               c(1300201, 1300607, 1300706, 1301407, 1301506, 1301654, 1301803, 1301951,
                 1302306, 1302405, 1303502, 1303908, 1304062)) %>%
      arrange(mun_res, ano) %>% 
      mutate(
        lat = as.factor(lat_res),
        lon = as.factor(lon_res)
      )
  } else {
    b_temp <- base_a %>%
      group_by(mun_res) %>% 
      filter(aux_res == 12 | mun_res %in% c(1300201, 1300607, 1300706, 1301407, 1301506,
                                            1301654, 1301803, 1301951, 1302306, 1302405,
                                            1303502, 1303908, 1304062)) %>%       
      arrange(mun_res, ano) %>% 
      mutate(
        lat = as.factor(lat_res),
        lon = as.factor(lon_res)
      )
  }
  
  
  summary(b_temp$lat)
  summary(b_temp$lon)
  
  print(paste0("Rows:", nrow(b_temp), " ", i))
  
  
  
  
  result_time[[as.character(i)]] <- rdrobust(
    y = b_temp$d.media[b_temp$ano == 2019],
    x = b_temp$dist_hv_res[b_temp$ano == 2018],
    c = 0,
    p = 1,
    h = bw_main_r,
    b = bw_bias_r,
    cluster = b_temp$seg_res[b_temp$ano == 2018],
    weights = b_temp$obs[b_temp$ano == 2018],
    vce = "hc0",
    covs = cbind(
      b_temp$lat_res[b_temp$ano == 2018],
      b_temp$lon_res[b_temp$ano == 2018],
      #All
      b_temp$dtempd1[b_temp$ano == 2019], #Temperature
      b_temp$descm[b_temp$ano == 2019], #mother educ
      b_temp$dn_ban[b_temp$ano == 2019], #bathrooms
      b_temp$dumidd1[b_temp$ano == 2019], #Humidity d1
      b_temp$dumidd2[b_temp$ano == 2019], #Humidty d2
      b_temp$dfem[b_temp$ano == 2019], #Female
      b_temp$dppi[b_temp$ano == 2019], #PPI
      b_temp$didade[b_temp$ano == 2019], #Age
      b_temp$descp[b_temp$ano == 2019], #father educ
      
      b_temp$dpessoa[b_temp$ano == 2019], #people in household
      b_temp$dn_qua[b_temp$ano == 2019], #houses
      b_temp$dn_car[b_temp$ano == 2019], #cars
      b_temp$dn_gel[b_temp$ano == 2019], #geladeira
      b_temp$dn_cel[b_temp$ano == 2019], #cel
      b_temp$dpc[b_temp$ano == 2019],    #pc
      b_temp$dinternet[b_temp$ano == 2019], #internet
      b_temp$dempr_dom[b_temp$ano == 2019], #housekeeping
      
      b_temp$drenda1[b_temp$ano == 2019], #wage < 1MW
      b_temp$drenda110[b_temp$ano == 2019], #wage 1MW - 10MW
      b_temp$drenda10[b_temp$ano == 2019], #wage > 10MW
      b_temp$dpibpc[b_temp$ano == 2019], #pibpc
      
      b_temp$dtempd2[b_temp$ano == 2019], #temp2
      
      #Fuso
      b_temp$h13[b_temp$ano == 2019],
      b_temp$h12[b_temp$ano == 2019],
      b_temp$h11[b_temp$ano == 2019]
    )
  )
  
  message("Terminado para: ", i)
  
  
  rm(b_temp)
}
# ---------------------------------------------------------------------------- #
## 10.3 Result Table ----
# ---------------------------------------------------------------------------- #


result_tab <- data.frame(
  coef   = sapply(result_time, function(x) x$coef[3]),
  se     = sapply(result_time, function(x) x$se[3]),
  pv     = sapply(result_time, function(x) x$pv[3]),
  n_left = sapply(result_time, function(x) x$N_h[1]),
  n_rght = sapply(result_time, function(x) x$N_h[2]),
  bw     = sapply(result_time, function(x) x$bws[1, 1]),
  totr   = sapply(result_time, function(x) x$N[2]),
  totl   = sapply(result_time, function(x) x$N[1])
)



result <- data.frame(
  ` ` = c(
    "2019 - 2018",
    " ",
    "N = N$_L$, N$_R$",
    "BW"  ),
  `(1)` = c(#UTC -4
    fmt_est(result_tab$coef[2], result_tab$pv[2]),
    fmt_se(result_tab$se[2]),
    fmt_npair(result_tab$n_left[2], result_tab$n_rght[2]),
    fmt_bw(result_tab$bw[2])  ),
  `(2)` = c(#UTC -3
    fmt_est(result_tab$coef[1], result_tab$pv[1]),
    fmt_se(result_tab$se[1]),
    fmt_npair(result_tab$n_left[1], result_tab$n_rght[1]),
    fmt_bw(result_tab$bw[1])
  ),
  
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------- #
# Latex
# ---------------------------------------------------------------------------- #

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/fuso_resultado_v1.tex")

rm(result, result_tab, latex_table,i, result_time, time_list)

# ---------------------------------------------------------------------------- #
# 11. Bandwith test ----
# ---------------------------------------------------------------------------- #

base_a <- base[priv0 == 1,.(media = mean(media, na.rm = T),
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
  left_join(base_aux, by = c("mun_res", "ano"))

# Estes valores estão no item 2.
# h <- rlist_a[[3]]$bws[1]
# b <- rlist_a[[3]]$bws[2]

h <- bw_main_r
b <- bw_bias_r


# Range de bandwidths
bws <- seq(0.2*h,1.5*h,0.1*h)

# Efeitos fixos
ef <- dummy_cols(base_a$seg_res[base_a$ano == 2018])
ef <- ef %>% select(-1,-2)

rlist_bw <- list()

# Loop nos cutoffs
for (c in 1:length(bws)) {
  
  print(paste0("Rodada: ", c))
  
  rlist_bw[[c]] <- rdrobust(
    y = base_a$d.media[base_a$ano == 2019],
    x = base_a$dist_hv_res[base_a$ano == 2018],
    c = 0,
    cluster = base_a$seg_res[base_a$ano == 2018],
    vce = "hc0",
    weights = base_a$obs[base_a$ano == 2018],
    covs = cbind(ef,
                 base_a$lat_res[base_a$ano == 2018],
                 base_a$lon_res[base_a$ano == 2018],
                 #All
                 base_a$dtempd1[base_a$ano == 2019], #Temperature
                 base_a$descm[base_a$ano == 2019], #mother educ
                 base_a$dn_ban[base_a$ano == 2019], #bathrooms
                 base_a$dumidd1[base_a$ano == 2019], #Humidity d1
                 base_a$dumidd2[base_a$ano == 2019], #Humidty d2
                 base_a$dfem[base_a$ano == 2019], #Female
                 base_a$dppi[base_a$ano == 2019], #PPI
                 base_a$didade[base_a$ano == 2019], #Age
                 base_a$descp[base_a$ano == 2019], #father educ
                 
                 base_a$dpessoa[base_a$ano == 2019], #people in household
                 base_a$dn_qua[base_a$ano == 2019], #houses
                 base_a$dn_car[base_a$ano == 2019], #cars
                 base_a$dn_gel[base_a$ano == 2019], #geladeira
                 base_a$dn_cel[base_a$ano == 2019], #cel
                 base_a$dpc[base_a$ano == 2019],    #pc
                 base_a$dinternet[base_a$ano == 2019], #internet
                 base_a$dempr_dom[base_a$ano == 2019], #housekeeping
                 
                 base_a$drenda1[base_a$ano == 2019], #wage < 1MW
                 base_a$drenda110[base_a$ano == 2019], #wage 1MW - 10MW
                 base_a$drenda10[base_a$ano == 2019], #wage > 10MW
                 base_a$dpibpc[base_a$ano == 2019], #pibpc
                 
                 base_a$dtempd2[base_a$ano == 2019], #temp2
                 
                 #Fuso
                 base_a$h13[base_a$ano == 2019],
                 base_a$h12[base_a$ano == 2019],
                 base_a$h11[base_a$ano == 2019]),
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
  c = seq(0.2,1.5,0.1)
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


ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/img/bandwith_v2.png", plot = graph,device = "png", width = 15, height = 10)
ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/img/pdf/bandwith_v2.pdf", plot = graph,device = "pdf", width = 8, height = 6)

rm(graph, tablist_bw, rlist_bw, h, b, c, base_a)

# ---------------------------------------------------------------------------- #
# 12. Balance Figs ----
# ---------------------------------------------------------------------------- #
## 12.1 Lists  -----
# ---------------------------------------------------------------------------- #


var_list <- c("id18", "fem", "ppi", "escp", "escm", # "dom5",
              "pessoas_dom", "empr_dom", "n_banheiro", "n_quartos", "n_carros",
              "n_geladeira", "n_celular", "pc", "internet",
              "renda1", "pibpc",# "pai_trab_man", "mae_trab_man",
              "temp_d1", "temp_d2", "umid_d1", "umid_d2",
              "renda_1_10", "renda_10")

vnames <- c(
  "Age 18",
  "Female",
  "African Brazilian\nor Indigenous",
  "Father with\nhigh school",
  "Mother with\nhigh school",
  #"5 or more people\nin household",
  "People in household",
  "Domestic worker\npresence",
  "Number of bathrooms",
  "Number of bedrooms",
  "Number of cars",
  "Number of refrigerators",
  "Number of cell phones",
  "Computer presence",
  "Internet access",
  "Household income\nup to 1 MW",
  "GDP per capita",
  #"Father in\nmanual labor",
  #"Mother in\nmanual labor",
  "Temperature – Day 1",
  "Temperature – Day 2",
  "Humidity – Day 1",
  "Humidity – Day 2",
  "Household income\nbetween 1 and 10 MW",
  "Household income\nmore than 10 MW"
)

# ---------------------------------------------------------------------------- #
## 12.2 Data  -----
# ---------------------------------------------------------------------------- #

base_inpe_t <- base %>%
  mutate(
    renda_1_10 = ifelse(renda_dom == "C", 1 , 0),
    renda_10   = ifelse(renda_dom == "D", 1, 0)
  ) %>% 
  filter(over_hv_border == 0) %>% 
  setDT()

# ---------------------------------------------------------------------------- #
## 12.3 Yearly Lvl ----
# ---------------------------------------------------------------------------- #

ylist <- list()

for (i in var_list){
  
  for(j in c(2018:2019)) {
    
    temp <- base_inpe_t %>% 
      filter(priv0 == 1) %>% 
      filter(!is.na(.data[[i]])) %>% 
      filter(ano == j) %>% 
      mutate(var_y = .data[[i]])
    
    
    
    #Seg dummy 
    ef <- dummy_cols(temp$seg_res) 
    ef <- ef %>% select(-1,-2)
    
    ylist[[as.character(paste0(i,"|",j))]] <-
      rdrobust(
        y = temp$var_y,
        x = temp$dist_hv_res,
        c = 0,
        p = 1,
        h = bw_main_r,
        b = bw_bias_r,
        cluster = temp$seg_res,
        vce = "hc0",
        covs = cbind(ef,
                     temp$lat_res,
                     temp$lon_res)
      )
    
    message("Year ", j," -- Finished for: ", i)
  }
  rm(i, j, temp)
}


library(stringr)
library(purrr)

covs_np <- map_dfr(names(ylist), function(name) {
  
  model <- ylist[[name]]
  
  tibble(
    var = str_extract(name, "^[^|]+"),           # variable name
    year = str_extract(name, "\\d{4}$"),         # year (2017, 2018, 2019)
    tstat = model$coef[1] / model$se[1]          # t-stat (main effect)
  )
})


var_map <- setNames(vnames, var_list)

covs_np <- covs_np %>%
  arrange(var) %>% 
  mutate(var = recode(var, !!!var_map))

plot_covs <- ggplot(data = covs_np) +
  
  theme_bw() +
  
  labs(x = 't-statistic', y = NULL, color = "Year", shape = "Year") +
  
  scale_x_continuous(
    breaks = c(-1.96, 0, 1.96),
    labels = c("-1.96", "", "1.96"),
    limits = c(-7, 7)
  ) +
  
  geom_vline(
    xintercept = c(-1.96, 1.96),
    color = 'red',
    linetype = 'dashed',
    linewidth = 1
  ) +
  
  geom_point(
    aes(x = tstat, y = var, color = year, shape = year),
    size = 2.5
  ) +
  
  scale_color_manual(values = c(
    "2018" = "#D95F02",
    "2019" = "#1A2D99"
  )) +
  
  scale_shape_manual(values = c(
    "2018" = 17,   # triangle
    "2019" = 16    # circle
  )) +
  
  theme(
    panel.grid.minor = element_blank(),
    
    axis.title.x = element_text(size = 25),
    axis.text.x  = element_text(size = 20),
    axis.text.y  = element_text(size = 18),
    
    legend.position = "bottom",
    legend.title = element_text(size = 19),
    legend.text  = element_text(size = 19)
  )

plot_covs

ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/covs_test_dif_lvl.png"), device = "png", height = 13, width = 7)
ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/covs_test_dif_lvl.eps"), device = "eps", height = 13, width = 7)

rm(covs_np, ylist, plot_covs, var_map)


# ---------------------------------------------------------------------------- #
## 12.4 Difs ----
# ---------------------------------------------------------------------------- #

ylist <- list()

for (i in var_list){
  
  temp <- base_inpe_t %>%
    filter(priv0 == 1, ano %in% c(2018:2019)) %>% 
    group_by(mun_res,ano,dist_hv_res,seg_res,lat_res,lon_res) %>% 
    summarise(
      var = mean(.data[[i]], na.rm = T),
      obs = n()) %>% 
    arrange(mun_res, ano) %>% ungroup() %>% 
    group_by(mun_res) %>% 
    filter(!is.na(var)) %>% 
    mutate(
      dup   = 1,
      dup2  = sum(dup),
      
      v1    = ifelse(ano == 2018, var, NA),
      v2    = max(v1, na.rm = T),
      var_y = var - v2
    ) %>% ungroup() %>% 
    filter(dup2 == 2) %>% 
    select(-c(dup2, dup, v1, v2)) 
  
  
  #Seg dummy 
  ef <- dummy_cols(temp$seg_res) 
  ef <- ef %>% select(-1,-2)
  
  ylist[[as.character(paste0(i))]] <-
    rdrobust(
      y = temp$var_y,
      x = temp$dist_hv_res,
      c = 0,
      p = 1,
      h = bw_main_r,
      b = bw_bias_r,
      cluster = temp$seg_res,
      vce = "hc0",
      covs = cbind(ef,
                   temp$lat_res,
                   temp$lon_res)
    )
  
  message("-- Finished for: ", i)
  
  rm(i, j, temp)
}


library(stringr)
library(purrr)

covs_np <- map_dfr(names(ylist), function(name) {
  
  model <- ylist[[name]]
  
  tibble(
    var = str_extract(name, "^[^|]+"),           # variable name
    year = str_extract(name, "\\d{4}$"),         # year (2017, 2018, 2019)
    tstat = model$coef[1] / model$se[1]          # t-stat (main effect)
  )
})


var_map <- setNames(vnames, var_list)

covs_np <- covs_np %>%
  arrange(var) %>% 
  mutate(var  = recode(var, !!!var_map),
         year = as.factor(2019))

plot_covs <- ggplot(data = covs_np) +
  
  theme_bw() +
  
  labs(x = 't-statistic', y = NULL) +
  
  scale_x_continuous(
    breaks = c(-1.96, 0, 1.96),
    labels = c("-1.96", "", "1.96"),
    limits = c(-5, 5)
  ) +
  
  geom_vline(
    xintercept = c(-1.96, 1.96),
    color = 'red',
    linetype = 'dashed',
    linewidth = 1
  ) +
  
  geom_point(
    aes(x = tstat, y = var, shape = year),
    color = "blue",
    size = 2.5
  ) +
  
  theme(
    panel.grid.minor = element_blank(),
    
    axis.title.x = element_text(size = 25),
    axis.text.x  = element_text(size = 20),
    axis.text.y  = element_text(size = 18),
    
    legend.position = "none"
    
  )

plot_covs

ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/covs_test_dif.png"), device = "png", height = 13, width = 7)
ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/covs_test_dif.eps"), device = "eps", height = 13, width = 7)

rm(covs_np, ylist, plot_covs, var_map, var_list, base_inpe_t)

# ---------------------------------------------------------------------------- #
# 13. SAEB ----
# ---------------------------------------------------------------------------- #

saeb_base <- readRDS("Z:/Tuffy/Paper - HV/Bases/saeb_total.RDS") %>% 
  setDT()


# ---------------------------------------------------------------------------- #
##13.1 Base ----
# ---------------------------------------------------------------------------- #

# mun_exp <- base_a %>%
#   filter(ano == 2019) %>% 
#   select(mun_prova)


# saeb_base <- saeb_base %>% 
#   mutate(in_base = ifelse(mun_prova %in% mun_exp$mun_prova, 1, 0))
# 
# saeb_base <- saeb_base %>% 
#   filter(in_base == 1 )
# rm(mun_exp)

base_a <- base %>% 
  filter(ano == 2018) %>% 
  group_by(mun_res, lon_res, lat_res, seg_res, dist_hv_res) %>% 
  summarise(n = n(),
            .groups = "drop")


temp <- base_a %>% 
  select(mun_res, lat_res, lon_res, dist_hv_res, seg_res)


saeb_base <- saeb_base %>% 
  left_join(temp, by = c("mun_prova" = "mun_res")) 

rm(temp)
colnames(saeb_base)



# ---------------------------------------------------------------------------- #
### 13.1.1 Aux Base ----
# ---------------------------------------------------------------------------- #



vars_diff <- c(
  "media",
  "escm", "escp", "pessoa", "empr_dom",
  "n_ban", "n_qua", "n_car", "n_gel", "n_cel",
  "pc", "internet", "renda1", "renda110", "renda10",
  "pibpc",
  "fem", "idade", "ppi",
  "tempd1", "tempd2", "umidd2", "umidd1"
)

base_res <- base[priv0 == 1, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  h10 = first(h10),
  obs = .N
), by = .(mun_res, ano, dist_hv_res, seg_res, lat_res, lon_res)] %>%
  filter(as.numeric(ano) %in% c(2017, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_res)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_res[[v1]] <- ifelse(base_res$ano == 2017, base_res[[v]], NA_real_)
  
  base_res[[v2]] <- ave(
    base_res[[v1]], 
    base_res$mun_res, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_res[[dv]] <- base_res[[v]] - base_res[[v2]]
  
  base_res[[dv]][!is.finite(base_res[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_res), value = TRUE)
base_res <- base_res %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia)

base_aux2 <- base_res %>% 
  select(mun_res, ano, h13:dumidd1) %>% 
  select(-obs, -d.media)

# ---------------------------------------------------------------------------- #
## 13.2 Regression (BW) ----
# ---------------------------------------------------------------------------- #

# Lista para armazenar resultados
rlist_saeb <- list()


for (i in c("5","9","3")) {
  
  temp <- saeb_base %>% 
    filter(serie == i,
           !is.na(seg_res)) %>% 
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
    select(-c(v1,v2, dup1, dup2)) %>% 
    left_join(base_aux2, by = c("mun_prova" = "mun_res", "ano"))
  
  
  
  
  
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
    select(dist_hv_res)
  
  # Clusters
  clu <- temp %>% 
    filter(ano == 2017) %>% 
    select(seg_res)
  
  # Latitude
  latv <- temp %>%
    filter(ano == 2017) %>% 
    select(lat_res)
  
  # Longitude
  lonv <- temp %>% 
    filter(ano == 2017) %>% 
    select(lon_res)
  
  #time 
  time <- temp %>% 
    filter(ano == 2019) %>% 
    select(h11, h12, h13)
  
  #all
  all <- temp %>% 
    filter(ano == 2019) %>% 
    select(dtempd1, dtempd2, dumidd1, dumidd2,
           
           dn_ban, dn_qua, dn_car, dn_gel, dn_cel, dpc, dinternet, dempr_dom,
           
           dfem, descm, dppi, didade, descp,
           
           drenda1, drenda110, drenda10, dpibpc)
  
  
  #peso
  
  w_lp <- temp %>% 
    filter(ano == 2017) %>% 
    select(lp_peso)
  
  w_mt <- temp %>% 
    filter(ano == 2017) %>% 
    select(mt_peso)
  
  ef <- dummy_cols(clu$seg_res)
  ef <- ef %>% select(-1,-2)
  
  
  
  
  
  
  # Lista para armazenamento dos resultados
  
  # Regressão LP --------------------------------------------------------------#
  rlist_saeb[[paste0("LP|",i)]] <- rdrobust(
    y = yv_lp$d.media_lp,
    x = xv$dist_hv_res,
    c = 0,
    h = bw_main_r,
    b = bw_bias_r,
    weights = w_lp,
    cluster = clu,
    vce = "hc0",
    covs = cbind(ef,
                 latv,
                 lonv,
                 time,
                 all)
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
    x = xv$dist_hv_res,
    c = 0,
    h = bw_main_r,
    b = bw_bias_r,
    weights = w_mt,
    cluster = clu,
    vce = "hc0",
    covs = cbind(ef,
                 latv,
                 lonv,
                 time,
                 all)
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
  coef   = sapply(rlist_saeb, function(x) x$coef[3]),
  se     = sapply(rlist_saeb, function(x) x$se[3]),
  pv     = sapply(rlist_saeb, function(x) x$pv[3]),
  n_left = sapply(rlist_saeb, function(x) x$N_h[1]),
  n_rght = sapply(rlist_saeb, function(x) x$N_h[2]),
  bw     = sapply(rlist_saeb, function(x) x$bws[1, 1]),
  totr   = sapply(rlist_saeb, function(x) x$N[2]),
  totl   = sapply(rlist_saeb, function(x) x$N[1])
)




result <- data.frame(
  ` ` = c(
    "Language",
    " ",
    "N = N$_L$, N$_R$",
    "BW",
    "Municipalities",
    "Math",
    " ",
    "N = N$_L$, N$_R$",
    "BW",
    "Municipalities"
    
  ),
  `(1)` = c(#5th
    #lang
    fmt_est(tab$coef[1], tab$pv[1]),
    fmt_se(tab$se[1]),
    fmt_npair(tab$n_left[1], tab$n_rght[1]),
    fmt_bw(tab$bw[1]),
    formatC(tab$totr[1] + tab$totl[1], format = "d", big.mark = ","),
    #math
    fmt_est(tab$coef[2], tab$pv[2]),
    fmt_se(tab$se[2]),
    fmt_npair(tab$n_left[2], tab$n_rght[2]),
    fmt_bw(tab$bw[2]),
    formatC(tab$totr[2] + tab$totl[2], format = "d", big.mark = ",")
  ),
  `(2)` = c(#9th
    #lang
    fmt_est(tab$coef[3], tab$pv[3]),
    fmt_se(tab$se[3]),
    fmt_npair(tab$n_left[3], tab$n_rght[3]),
    fmt_bw(tab$bw[3]),
    formatC(tab$totr[3] + tab$totl[3], format = "d", big.mark = ","),
    #math
    fmt_est(tab$coef[4], tab$pv[4]),
    fmt_se(tab$se[4]),
    fmt_npair(tab$n_left[4], tab$n_rght[4]),
    fmt_bw(tab$bw[4]),
    formatC(tab$totr[4] + tab$totl[4], format = "d", big.mark = ",")
    
  ),
  `(3)` = c(#3th
    #lang
    fmt_est(tab$coef[5], tab$pv[5]),
    fmt_se(tab$se[5]),
    fmt_npair(tab$n_left[5], tab$n_rght[5]),
    fmt_bw(tab$bw[5]),
    formatC(tab$totr[5] + tab$totl[5], format = "d", big.mark = ","),
    #math
    fmt_est(tab$coef[6], tab$pv[6]),
    fmt_se(tab$se[6]),
    fmt_npair(tab$n_left[6], tab$n_rght[6]),
    fmt_bw(tab$bw[6]),
    formatC(tab$totr[6] + tab$totl[6], format = "d", big.mark = ",")
    
  ),
  
  check.names = FALSE,
  stringsAsFactors = FALSE
)


print(result)

# ---------------------------------------------------------------------------- #
# LATEX
# ---------------------------------------------------------------------------- #

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/Saeb.tex")

rm(rlist_saeb, saeb_base, tab,result, latex_table, base, base_a, all, base_res,
   time, v, v1, v2, vars_diff, vnames)


# ---------------------------------------------------------------------------- #
# 14. Year Lvl. ----
# ---------------------------------------------------------------------------- #
## 14.1 Data ----
# ---------------------------------------------------------------------------- #
#' Vamos inciar a estimação das diferenças anuais através da abertura das bases de dados
#' e da agregação destas para o nível municipal.

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



# 1) Abrindo a base de dados:
base <- data.frame() #Criando a base de dados vazia que será preenchida pelos dados anuais

inpe_days <- list(
  `2013` = c(d1 = 26, d2 = 27),
  `2014` = c(d1 = 8, d2 = 9),
  `2015` = c(d1 = 24, d2 = 25),
  `2016` = c(d1 = 5,  d2 = 6),
  `2017` = c(d1 = 5,  d2 = 12),
  `2018` = c(d1 = 4,  d2 = 11),
  `2019` = c(d1 = 3,  d2 = 10)
)

for(ano in 2013:2019) { #loop para abertura das bases de dados
  message("Loop start for: ", ano)
  ini <- Sys.time()
  #Abrindo a base de dados de cada ano
  base_temp <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_",ano,".RDS")) %>% 
    select( #selecionando apenas as colunas principais
      mun_res, ano, id_enem, hv, priv0, lon, lat, dist_hv_border, seg, media, idade,
      conclusao, mun_prova,
      esc_mae, esc_pai, renda_dom, ppi, pessoas_dom, empr_dom, n_banheiro, n_quartos,
      n_carros, n_geladeira, n_celular, pc, internet, renda1, fem, id18, ppi
    ) %>% 
    filter(
      conclusao == 2, #mantendo apenas alunos concluintes
      priv0 == 1
      ) %>% 
    mutate(mun_prova = as.character(mun_prova)) %>% 
    setDT()
  
  # -------------------------------------------------------------------------- #
  # -- INPE --
  # -------------------------------------------------------------------------- #
  
  message("Opened data. Now joining INPE data...")
  # read only this year's INPE
  inpe <- readRDS(paste0("Z:/Tuffy/Paper - HV/Bases/inpe/mun/inpe_mun_", ano, ".rds")) %>% 
    setDT()
  
  dd <- inpe_days[[as.character(ano)]]
  
  inpe_prova <- inpe[
    ,
    .(
      codmun,
      anoinpe = ano,
      temp_d1 = get(paste0("temp_", dd["d1"])),
      temp_d2 = get(paste0("temp_", dd["d2"])),
      umid_d1 = get(paste0("umid_", dd["d1"])),
      umid_d2 = get(paste0("umid_", dd["d2"]))
    )
  ]
  
  base_temp[
    inpe_prova,
    on = .(mun_prova = codmun, ano = anoinpe),
    `:=`(
      temp_d1 = i.temp_d1,
      temp_d2 = i.temp_d2,
      umid_d1 = i.umid_d1,
      umid_d2 = i.umid_d2
    )
  ]
  
  rm(inpe, inpe_prova, dd)

  message("Successful merge! Now creating additional variables...")
  # -------------------------------------------------------------------------- #
  # Creating the variables  --- #
  # -------------------------------------------------------------------------- #
  
  base_temp <- base_temp %>% 
    mutate(
      
      aux_res = mun_res %/% 100000,
      
      h13 = ifelse(
          aux_res %in% c(52, 53, 31, 32, 33, 35, 42, 41, 43,
                         #"GO", "DF", "MG", "ES", "RJ", "SP", "SC", "PR", "RS",
                         29, 28, 27, 26, 25, 24, 23, 22, 21, 17, 15, 16 ),
        1, 0
      ),
      
      h12 = ifelse(
        aux_res %in% c(51, 50, 11, 14) | aux_res == 13 & !mun_res %in%
          c(1300201, 1300607, 1300706, 1301407, 1301506, 1301654, 1301803, 1301951,
            1302306, 1302405, 1303502, 1303908, 1304062),
        1, 0
        ),
      
      h11 = ifelse(
          aux_res == 12 | mun_res %in% c(1300201, 1300607, 1300706, 1301407, 1301506,
                                         1301654, 1301803, 1301951, 1302306, 1302405,
                                         1303502, 1303908, 1304062),
          1, 0
          ),
      
      
      new_dist = dist_hv_border/100000
      )  %>% 
    mutate(
      
      escm = case_when(
        esc_mae %in% c("D","E","F") ~ 1, #With high school
        esc_mae %in% c("A","B","C") ~ 0,
        .default = NA),
      
      escp = case_when(
        esc_pai %in% c("D","E","F") ~ 1, #With High school
        esc_pai %in% c("A","B","C") ~ 0,
        .default = NA),
      
      renda_1_10 = ifelse(renda_dom == "C", 1 , 0),
      renda_10   = ifelse(renda_dom == "D", 1, 0)
           )
  
  # -------------------------------------------------------------------------- #
  # -- Summarise --
  # -------------------------------------------------------------------------- #
  
  message("Variables created! Now summarizing data...")
  
  
  base_nota <- base_temp[priv0 == 1, .(
    media    = mean(media, na.rm = TRUE),
    escm     = mean(escm, na.rm = TRUE),
    escp     = mean(escp, na.rm = TRUE),
    pessoa   = mean(pessoas_dom, na.rm = TRUE),
    empr_dom = mean(empr_dom, na.rm = TRUE),
    n_ban    = mean(n_banheiro, na.rm = TRUE),
    n_qua    = mean(n_quartos, na.rm = TRUE),
    n_car    = mean(n_carros, na.rm = TRUE),
    n_gel    = mean(n_geladeira, na.rm = TRUE),
    n_cel    = mean(n_celular, na.rm = TRUE),
    pc       = mean(pc, na.rm = TRUE),
    internet = mean(internet, na.rm = TRUE),
    renda1   = mean(renda1, na.rm = TRUE),
    renda110 = mean(renda_1_10, na.rm = TRUE),
    renda10  = mean(renda_10, na.rm = TRUE),
    fem      = mean(fem, na.rm = TRUE),
    idade    = mean(id18, na.rm = TRUE),
    ppi      = mean(ppi, na.rm = TRUE),
    tempd1   = mean(temp_d1, na.rm = TRUE),
    tempd2   = mean(temp_d2, na.rm = TRUE),
    umidd2   = mean(umid_d2, na.rm = TRUE),
    umidd1   = mean(umid_d1, na.rm = TRUE),
    h13 = first(h13),
    h12 = first(h12),
    h11 = first(h11),
    obs = .N
  ), by = .(mun_res,ano)] %>%
    arrange(mun_res, ano)
  
  
  message("Summarise finished. Now adding Residency distance...")
  #Res. Dist.
  base_nota <- base_nota %>% 
    left_join(mun_hv %>% rename(dist_hv_res = dist_hv_border) %>% #Coordinates of Residency 
                rename(lat_res = lat, 
                       lon_res = lon,
                       seg_res = seg), 
              by = c("mun_res" = "co_municipio"))
  
  
  #Salvando a base de dados agregada no nível municipal:
  saveRDS(base_nota, file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/Agregados/base_nota_ag_contols",ano,".RDS"))
  message("saved agreggated data. Now binding to final DF.")
  
  base <- rbind(base, base_nota) %>%  #unindo as bases de dados]
    setDT()
  message("Finished for ", ano)
  
  rm(base_temp, base_nota) #removendo os itensque não serão mais utilizados
  fim <- Sys.time()
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  hours <- round(mins /60)
  
  message("---------------------------------------------")
  message("Elapsed time: ",hours, " hours, ",mins," mins, and ", secs, " s")
  message("---------------------------------------------")
  rm(fim, delta, mins, secs, hours, ini)
}

# Adding pibpc <- 

# PIB
pib <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/pib.RDS") %>%
  mutate(codmun = as.integer(codmun)) %>%
  rename(mun_prova = codmun)

base <- base %>% 
  left_join(pib, by = c("mun_res" = "mun_prova", "ano")) 
  
rm(pib)
# ---------------------------------------------------------------------------- #
## 14.6 Year-diff----
# ---------------------------------------------------------------------------- #
###14.6.1 Regressions ----
# ---------------------------------------------------------------------------- #



ano_list <- c(2014:2019) #Lista dos anos que serão comparados no loop


n_list <- list() #No controls
c_list <- list() #with controls



vars_diff <- c(
  "media",
  "escm", "escp", "pessoa", "empr_dom",
  "n_ban", "n_qua", "n_car", "n_gel", "n_cel",
  "pc", "internet", "renda1", "renda110", "renda10",
  "pibpc",
  "fem", "idade", "ppi",
  "tempd1", "tempd2", "umidd2", "umidd1"
)

base <- base %>% 
  setDT()

#Loop de regressão por ano:
for (ano_ref in ano_list) {
  
  ano_ref2 <- ano_ref - 1
  
  base_t <- base[
    ano %in% c(ano_ref2, ano_ref)
  ][
    order(mun_res, ano)
  ][
    , if (.N == 2 && uniqueN(ano) == 2) .SD, by = mun_res
  ]
  
  # differences and lagged weights
  for (v in vars_diff) {
    if (!v %in% names(base_t)) {
      warning(paste("Variável não encontrada:", v))
      next
    }
    
    dv <- paste0("d", v)
    base_t[, (dv) := get(v) - shift(get(v)), by = mun_res]
  }
  rm(v)
  
  base_t[, obs_lag := shift(get("obs")), by = mun_res]
  
  # keep only the higher year
  base_t <- base_t[ano == ano_ref]
  
  ef <- dummy_cols(base_t$seg_res) %>% select(-1, -2)
  
  c_list[[paste0(ano_ref, "C|TC")]] <- rdrobust(
    y = base_t$dmedia,
    x = base_t$dist_hv_res,
    c = 0,
    p = 1,
    h = bw_main_r,
    b = bw_bias_r,
    cluster = base_t$seg_res,
    weights = base_t$obs_lag,
    vce = "hc0",
    covs = cbind(
      ef,
      base_t$lat_res,
      base_t$lon_res,
      base_t$dtempd1,
      base_t$descm,
      base_t$dn_ban,
      base_t$dumidd1,
      base_t$dumidd2,
      base_t$dfem,
      base_t$dppi,
      base_t$didade,
      base_t$descp,
      base_t$dpessoa,
      base_t$dn_car,
      base_t$dn_gel,
      base_t$dn_cel,
      base_t$dpc,
      base_t$dinternet,
      base_t$dempr_dom,
      base_t$drenda1,
      base_t$drenda110,
      base_t$drenda10,
      base_t$dpibpc,
      base_t$dtempd2,
      base_t$h13,
      base_t$h12,
      base_t$h11
    )
  )
  
  n_list[[paste0(ano_ref, "NC")]] <- rdrobust(
    y = base_t$dmedia,
    x = base_t$dist_hv_res,
    c = 0,
    p = 1,
    h = bw_main_r,
    b = bw_bias_r,
    cluster = base_t$seg_res,
    weights = base_t$obs_lag,
    vce = "hc0",
    covs = cbind(
      ef,
      base_t$lat_res,
      base_t$lon_res
    )
  )
}

# ---------------------------------------------------------------------------- #
### 14.6.2 Tab ----
# ---------------------------------------------------------------------------- #

#With controls
t10cc <- data.frame(
  coef = do.call(rbind,lapply(c_list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(c_list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(c_list, FUN = function(x){x$pv[3]})),
  n = do.call(rbind, lapply(c_list, FUN = function(x){x$N_h})),
  t = do.call(rbind, lapply(c_list, FUN = function(x){x$N}))
) %>% 
  mutate(
    N = n.1 + n.2,
    Tot = t.1 + t.2,
    ano = 2014:2019,
    year_diff = c(
      "2014–2013",
      "2015–2014",
      "2016–2015",
      "2017–2016",
      "2018–2017",
      "2019–2018"
    ),
    ymin = coef - 1.96 * se,
    ymax = coef + 1.96 * se
  ) %>% 
  select(-c(n.1, n.2))

t10cc_plot <- t10cc %>%
  filter(year_diff != "2017–2016") %>%
  mutate(x_pos = row_number())

p <- ggplot(t10cc_plot, aes(x = x_pos, y = coef)) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    width = 0.2,
    size = 0.8,
    color = "black"
  ) +
  geom_point(size = 3, color = "black") +
  geom_hline(yintercept = 0, color = "#D62728", linewidth = 1) +
  labs(x = "Year Difference", y = "Average ENEM Score Coefficient") +
  #geom_vline(xintercept = 4.5, color = "#BEBEBE", linetype = "dashed", size = 0.8) +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18, angle = 35, hjust = 1),
    legend.position = "none"
  ) +
  scale_x_continuous(
    breaks = t10cc_plot$x_pos,
    labels = t10cc_plot$year_diff
  )

print(p)

ggsave(plot = p, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/controls/img/anos_dif_final.png"), device = "png", height = 7, width = 10)
ggsave(plot = p, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/controls/img/pdf/anos_dif_final.eps"), device = "eps", height = 7, width = 10)


# #Without controls
# t10cc <- data.frame(
#   coef = do.call(rbind,lapply(n_list, FUN = function(x){x$coef[3]})),
#   se = do.call(rbind,lapply(n_list, FUN = function(x){x$se[3]})),
#   pv = do.call(rbind,lapply(n_list, FUN = function(x){x$pv[3]})),
#   n = do.call(rbind, lapply(n_list, FUN = function(x){x$N_h})),
#   t = do.call(rbind, lapply(n_list, FUN = function(x){x$N}))
# ) %>% 
#   mutate(
#     N = n.1 + n.2,
#     Tot = t.1 + t.2,
#     ano = 2014:2019,
#     ymin = coef - 1.96 * se,
#     ymax = coef + 1.96 * se
#   ) %>% 
#   select(-c(n.1, n.2))
# 
# 
# 
# 
# 
# p <- ggplot(t10cc, aes(x = ano, y = coef)) +
#   geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, size = 0.8, color = "black") +
#   geom_point(size = 3, color = "black") +
#   geom_hline(yintercept = 0, color = "#D62728", linewidth = 1) +
#   labs(x = "Year", y = "Average ENEM Score Coefficient") +
#   geom_vline(xintercept = 2018.5, color = "#BEBEBE", linetype = "dashed", size = 0.8) +
#   theme_minimal(base_size = 20) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(),
#     axis.text.y = element_text(size = 18),
#     axis.text.x = element_text(size = 18),
#     legend.position = "none"
#   ) +
#   scale_x_continuous(breaks = t10cc$ano) 
# 
# 
# print(p)
# 
# ggsave(plot = p, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/controls/img/anos_dif_v4.png"), device = "png", height = 7, width = 10)
# ggsave(plot = p, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/controls/img/pdf/anos_dif_v4.eps"), device = "eps", height = 7, width = 10)



# ---------------------------------------------------------------------------- #
##14.4 Diff-in-diff ----
# ---------------------------------------------------------------------------- #

base <- base %>% 
  mutate(aux_uf = mun_res %/% 100000,
         
         treat_west = ifelse(aux_uf %in% c(12,13,14,11, #AC, AM, RR, RO
                                           51, 50 #MT, MS
         ), 1, 0),
         end_dst = ifelse(ano == 2019, 1, 0)) %>% 
  arrange(mun_res, ano)


# ---------------------------------------------------------------------------- #
### 14.4.1 Regression ----
# ---------------------------------------------------------------------------- #

library(fixest)

out <- feols(
  media_nota ~ treat_west*end_dst + abs(dist_hv_res),
  data = base,
  cluster = ~ seg_res,
  weights = ~ obs)



etable(out)


out2 <- feols(
  media_nota ~ treat_west:i(ano, ref = 2018)| 
    lat_res + lon_res,
  data = base,
  cluster = ~ seg_res,
  weights = ~ obs)



etable(out2)

iplot(out2)

# ---------------------------------------------------------------------------- #
### 14.4.2 Graph ----
# ---------------------------------------------------------------------------- #

p <- ggplot(out, aes(x = ano, y = coef)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, size = 0.8, color = "black") +
  geom_point(size = 3, color = "black") +
  geom_hline(yintercept = 0, color = "#D62728", linewidth = 1) +
  labs(x = "Year", y = "ENEM Score") +
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
rm(p, gap_x, ano_ref, ano_ref2, ano_list, ano, t10cc, t10ccplot, n_list, base_t)
# ---------------------------------------------------------------------------- #
# 15. Filter + Desc ----
# ---------------------------------------------------------------------------- #
## 15.0 Func Setup----
# ---------------------------------------------------------------------------- #

# ------------------------------------------------------------------ #
# 1. Helper: INPE day mapping for 2018-2019
# ------------------------------------------------------------------ #

inpe_days_1819 <- list(
  `2018` = c(d1 = 4, d2 = 11),
  `2019` = c(d1 = 3, d2 = 10)
)

# Municipalities with special timezone treatment in Amazonas
am_mun_special <- c(
  1300201, 1300607, 1300706, 1301407, 1301506, 1301654, 1301803, 1301951,
  1302306, 1302405, 1303502, 1303908, 1304062
)

# ------------------------------------------------------------------ #
# 2. Function to create controls/derived variables for 2018-2019
#    Excludes:
#    - school-level "_esc" variables
#    - INPE residence variables "*_res"
#    - escm / escp if you do not want "_esc variables" to include these
# ------------------------------------------------------------------ #

create_controls_1819 <- function(base) {
  base %>%
    as.data.table() %>%
    .[ano %in% c(2018, 2019)] %>%
    mutate(
      aux_res = mun_res %/% 100000,
      aux_pro = mun_prova %/% 100000,
      
      # Exam start time dummies
      h13 = ifelse(
          aux_res %in% c(
            52, 53, 31, 32, 33, 35, 42, 41, 43,
            29, 28, 27, 26, 25, 24, 23, 22, 21, 17, 15, 16
          ),
          1,0
          ),
      
      h12 = ifelse(
            aux_res %in% c(51, 50, 11, 14) |
              (aux_res == 13 & !mun_res %in% am_mun_special),
            1, 0
      ),
      
      h11 = ifelse(
          
            aux_res == 12 |
              mun_res %in% am_mun_special,
            1, 0
            ),
      
      new_dist = dist_hv_border / 100000,
      
      escm = case_when(
        esc_mae %in% c("D","E","F") ~ 1, #With high school
        esc_mae %in% c("A","B","C") ~ 0,
        .default = NA),
      
      escp = case_when(
        esc_pai %in% c("D","E","F") ~ 1, #With High school
        esc_pai %in% c("A","B","C") ~ 0,
        .default = NA
      ),
      
      
      renda_1_10 = ifelse(renda_dom == "C", 1, 0),
      renda_10   = ifelse(renda_dom == "D", 1, 0),
      
      over_hv_border = case_when(
        aux_res < 30 & aux_pro >= 30 ~ 1,
        aux_res >= 30 & aux_pro < 30 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(-aux_res, -aux_pro)
}

# ------------------------------------------------------------------ #
# 3. Function to add INPE variables by municipality of test only
#    Excludes residence INPE variables
# ------------------------------------------------------------------ #

add_inpe_prova_1819 <- function(base, inpe_path = "Z:/Tuffy/Paper - HV/Bases/inpe/mun") {
  base <- as.data.table(copy(base))
  
  for (yr in sort(unique(base$ano))) {
    if (!yr %in% c(2018, 2019)) next
    
    dd <- inpe_days_1819[[as.character(yr)]]
    
    inpe <- readRDS(file.path(inpe_path, paste0("inpe_mun_", yr, ".rds"))) %>%
      as.data.table()
    
    inpe[, codmun := as.numeric(codmun)]
    
    inpe_prova <- inpe[
      ,
      .(
        codmun,
        ano = yr,
        temp_d1 = get(paste0("temp_", dd["d1"])),
        temp_d2 = get(paste0("temp_", dd["d2"])),
        umid_d1 = get(paste0("umid_", dd["d1"])),
        umid_d2 = get(paste0("umid_", dd["d2"]))
      )
    ]
    
    base[
      inpe_prova,
      on = .(mun_prova = codmun, ano),
      `:=`(
        temp_d1 = i.temp_d1,
        temp_d2 = i.temp_d2,
        umid_d1 = i.umid_d1,
        umid_d2 = i.umid_d2
      )
    ]
    
    rm(inpe, inpe_prova, dd)
    gc()
  }
  
  base
}

# ---------------------------------------------------------------------------- #
# 15.0 Data raw ----
# ---------------------------------------------------------------------------- #

base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/base_nota_2019_trei.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/base_nota_2018_trei.RDS"))) %>%
  create_controls_1819() %>% 
  setDT()


pib <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/pib.RDS") %>%
  mutate(codmun = as.integer(codmun)) %>%
  rename(mun_prova = codmun)

base <- base %>% select(-pibpc) %>% 
  left_join(pib, by = c("mun_res" = "mun_prova", "ano"))

rm(pib)
# ---------------------------------------------------------------------------- #
##15.0.1 INPE ----
# ---------------------------------------------------------------------------- #


inpe17 <- readRDS("Z:/Tuffy/Paper - HV/Bases/inpe/mun/inpe_mun_2017.rds")
inpe18 <- readRDS("Z:/Tuffy/Paper - HV/Bases/inpe/mun/inpe_mun_2018.rds")
inpe19 <- readRDS("Z:/Tuffy/Paper - HV/Bases/inpe/mun/inpe_mun_2019.rds")

base <- base %>% 
  mutate(
    temp_d1 = case_when(
      ano == 2019 ~ inpe19$temp_3[match(mun_prova, inpe19$codmun)],
      ano == 2018 ~ inpe18$temp_4[match(mun_prova, inpe18$codmun)],
      ano == 2017 ~ inpe17$temp_5[match(mun_prova, inpe17$codmun)],
      TRUE ~ NA
    ),
    
    temp_d2 = case_when(
      ano == 2019 ~ inpe19$temp_10[match(mun_prova, inpe19$codmun)],
      ano == 2018 ~ inpe18$temp_11[match(mun_prova, inpe18$codmun)],
      ano == 2017 ~ inpe17$temp_12[match(mun_prova, inpe17$codmun)],
      TRUE ~ NA
    ),
    
    umid_d1 = case_when(
      ano == 2019 ~ inpe19$umid_3[match(mun_prova, inpe19$codmun)],
      ano == 2018 ~ inpe18$umid_4[match(mun_prova, inpe18$codmun)],
      ano == 2017 ~ inpe17$umid_5[match(mun_prova, inpe17$codmun)],
      TRUE ~ NA
    ),
    
    umid_d2 = case_when(
      ano == 2019 ~ inpe19$umid_10[match(mun_prova, inpe19$codmun)],
      ano == 2018 ~ inpe18$umid_11[match(mun_prova, inpe18$codmun)],
      ano == 2017 ~ inpe17$umid_12[match(mun_prova, inpe17$codmun)],
      TRUE ~ NA
    ),
    
    #Residency
    temp_d1_res = case_when(
      ano == 2019 ~ inpe19$temp_3[match(mun_res, inpe19$codmun)],
      ano == 2018 ~ inpe18$temp_4[match(mun_res, inpe18$codmun)],
      ano == 2017 ~ inpe17$temp_5[match(mun_res, inpe17$codmun)],
      TRUE ~ NA
    ),
    
    temp_d2_res = case_when(
      ano == 2019 ~ inpe19$temp_10[match(mun_res, inpe19$codmun)],
      ano == 2018 ~ inpe18$temp_11[match(mun_res, inpe18$codmun)],
      ano == 2017 ~ inpe17$temp_12[match(mun_res, inpe17$codmun)],
      TRUE ~ NA
    ),
    
    umid_d1_res = case_when(
      ano == 2019 ~ inpe19$umid_3[match(mun_res, inpe19$codmun)],
      ano == 2018 ~ inpe18$umid_4[match(mun_res, inpe18$codmun)],
      ano == 2017 ~ inpe17$umid_5[match(mun_res, inpe17$codmun)],
      TRUE ~ NA
    ),
    
    umid_d2_res = case_when(
      ano == 2019 ~ inpe19$umid_10[match(mun_res, inpe19$codmun)],
      ano == 2018 ~ inpe18$umid_11[match(mun_res, inpe18$codmun)],
      ano == 2017 ~ inpe17$umid_12[match(mun_res, inpe17$codmun)],
      TRUE ~ NA
    )
  )

rm(inpe18, inpe19, inpe17)

#Res. Dist.
base <- base %>% 
  left_join(mun_hv %>% rename(dist_hv_res = dist_hv_border) %>% #Coordinates of Residency 
              rename(lat_res = lat, 
                     lon_res = lon,
                     seg_res = seg), 
            by = c("mun_res" = "co_municipio")) 


base_trei <- base %>% filter(treineiro == 1) %>% 
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
  mutate(renda_1_10 = ifelse(renda_dom == "C", 1 , 0),
         renda_10   = ifelse(renda_dom == "D", 1, 0)) %>% 
  setDT()

gc()

rm(base)


summary(base_trei$mun_escola)
summary(base_trei)
# ---------------------------------------------------------------------------- #
## 15.1 Data Mock ----
# ---------------------------------------------------------------------------- #


char_vars <- c(
  "pessoas_dom", "empr_dom", "n_banheiro", "n_quartos",
  "n_carros", "n_geladeira", "n_celular", "pc", "internet"
)

base_trei[, (char_vars) := lapply(.SD, function(x) {
  x <- trimws(x)
  x[x == ""] <- NA
  as.numeric(gsub(",", ".", x, fixed = TRUE))
}), .SDcols = char_vars]


vars_diff <- c(
  "media",
  "escm", "escp", "pessoa", "empr_dom",
  "n_ban", "n_qua", "n_car", "n_gel", "n_cel",
  "pc", "internet", "renda1", "renda110", "renda10",
  "pibpc",
  "fem", "idade", "ppi",
  "tempd1", "tempd2", "umidd2", "umidd1"
)

base_trei_agg <- base_trei[, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  obs = .N
), by = .(mun_res, ano, dist_hv_res, seg_res, lat_res, lon_res)] %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()



for (v in vars_diff) {
  
  if (!v %in% names(base_trei_agg)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_trei_agg[[v1]] <- ifelse(base_trei_agg$ano == 2018, base_trei_agg[[v]], NA_real_)
  
  base_trei_agg[[v2]] <- ave(
    base_trei_agg[[v1]], 
    base_trei_agg$mun_res, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_trei_agg[[dv]] <- base_trei_agg[[v]] - base_trei_agg[[v2]]
  
  base_trei_agg[[dv]][!is.finite(base_trei_agg[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_trei_agg), value = TRUE)
base_trei_agg <- base_trei_agg %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia)


# ---------------------------------------------------------------------------- #
## 15.2 Regression ----
# ---------------------------------------------------------------------------- #



ef2 <- dummy_cols(base_trei_agg$seg_res[base_trei_agg$ano == 2018])
ef2 <- ef2 %>% select(-1,-2)


list <- list()



# ---------------------------------------------------------------------------- #
### 15.2.1 Mock -----
# ---------------------------------------------------------------------------- #


list[[as.character(paste0(2019,"-",2018,"C|Trei"))]] <- rdrobust(
  y = base_trei_agg$d.media[base_trei_agg$ano == 2019],
  x = base_trei_agg$dist_hv_res[base_trei_agg$ano == 2018],
  c = 0,
  cluster = base_trei_agg$seg_res[base_trei_agg$ano == 2018],
  weights = base_trei_agg$obs[base_trei_agg$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef2,
    
    base_trei_agg$lat_res[base_trei_agg$ano == 2018],
    base_trei_agg$lon_res[base_trei_agg$ano == 2018],
    #All
    base_trei_agg$dtempd1[base_trei_agg$ano == 2019], #Temperature
    base_trei_agg$descm[base_trei_agg$ano == 2019], #mother educ
    base_trei_agg$dumidd1[base_trei_agg$ano == 2019], #Humidity d1
    base_trei_agg$dumidd2[base_trei_agg$ano == 2019], #Humidty d2
    base_trei_agg$dfem[base_trei_agg$ano == 2019], #Female
    base_trei_agg$dppi[base_trei_agg$ano == 2019], #PPI
    base_trei_agg$didade[base_trei_agg$ano == 2019], #Age
    base_trei_agg$descp[base_trei_agg$ano == 2019], #father educ
    
    #base_trei_agg$dpessoa[base_trei_agg$ano == 2019], #people in household
    #base_trei_agg$dn_qua[base_trei_agg$ano == 2019], #houses
    #base_trei_agg$dn_car[base_trei_agg$ano == 2019], #cars
    #base_trei_agg$dn_gel[base_trei_agg$ano == 2019], #geladeira
    #base_trei_agg$dn_cel[base_trei_agg$ano == 2019], #cel
    #base_trei_agg$dpc[base_trei_agg$ano == 2019],    #pc
    #base_trei_agg$dinternet[base_trei_agg$ano == 2019], #internet
    #base_trei_agg$dempr_dom
    
    base_trei_agg$drenda1[base_trei_agg$ano == 2019], #wage < 1MW
    base_trei_agg$drenda110[base_trei_agg$ano == 2019], #wage 1MW - 10MW
    base_trei_agg$drenda10[base_trei_agg$ano == 2019], #wage > 10MW
    base_trei_agg$dpibpc[base_trei_agg$ano == 2019], #pibpc
    
    base_trei_agg$dtempd2[base_trei_agg$ano == 2019], #temp2
    
    #Fuso
    base_trei_agg$h13[base_trei_agg$ano == 2019],
    base_trei_agg$h12[base_trei_agg$ano == 2019],
    base_trei_agg$h11[base_trei_agg$ano == 2019]
  )
)

rm(base_trei)
# ---------------------------------------------------------------------------- #
## 15.3 Main ----
# ---------------------------------------------------------------------------- #
### 15.3.0 Data ----
# ---------------------------------------------------------------------------- #

base <- readRDS("Z:/Tuffy/Paper - HV/Bases/base_final.RDS") %>% 
  filter(ano %in% c(2019,2018)) %>% 
  setDT()


# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#Todos
# ---------------------------------------------------------------------------- #

base_t <- base[, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  obs = .N
), by = .(mun_res, ano, dist_hv_res, seg_res, lat_res, lon_res)] %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_t)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_t[[v1]] <- ifelse(base_t$ano == 2018, base_t[[v]], NA_real_)
  
  base_t[[v2]] <- ave(
    base_t[[v1]], 
    base_t$mun_res, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_t[[dv]] <- base_t[[v]] - base_t[[v2]]
  
  base_t[[dv]][!is.finite(base_t[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_t), value = TRUE)
base_t <- base_t %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia)


# ---------------------------------------------------------------------------- #
# Main
# ---------------------------------------------------------------------------- #
base_res <- base[priv0 == 1, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  obs = .N
), by = .(mun_res, ano, dist_hv_res, seg_res, lat_res, lon_res)] %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_res)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_res[[v1]] <- ifelse(base_res$ano == 2018, base_res[[v]], NA_real_)
  
  base_res[[v2]] <- ave(
    base_res[[v1]], 
    base_res$mun_res, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_res[[dv]] <- base_res[[v]] - base_res[[v2]]
  
  base_res[[dv]][!is.finite(base_res[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_res), value = TRUE)
base_res <- base_res %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia)


# ---------------------------------------------------------------------------- #
#Private 
# ---------------------------------------------------------------------------- #
base_p <- base[priv1 == 1, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  obs = .N
), by = .(mun_res, ano, dist_hv_res, seg_res, lat_res, lon_res)] %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_p)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_p[[v1]] <- ifelse(base_p$ano == 2018, base_p[[v]], NA_real_)
  
  base_p[[v2]] <- ave(
    base_p[[v1]], 
    base_p$mun_res, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_p[[dv]] <- base_p[[v]] - base_p[[v2]]
  
  base_p[[dv]][!is.finite(base_p[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_p), value = TRUE)
base_p <- base_p %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia)


# ---------------------------------------------------------------------------- #
#State + Mun 
# ---------------------------------------------------------------------------- #

base_psf <- base[dep_adm %in% c(2,3), .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  obs = .N
), by = .(mun_res, ano, dist_hv_res, seg_res, lat_res, lon_res)] %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_psf)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_psf[[v1]] <- ifelse(base_psf$ano == 2018, base_psf[[v]], NA_real_)
  
  base_psf[[v2]] <- ave(
    base_psf[[v1]], 
    base_psf$mun_res, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_psf[[dv]] <- base_psf[[v]] - base_psf[[v2]]
  
  base_psf[[dv]][!is.finite(base_psf[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_psf), value = TRUE)
base_psf <- base_psf %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia)

# ---------------------------------------------------------------------------- #
# Federal
# ---------------------------------------------------------------------------- #
base_fed <- base[dep_adm == 1, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  obs = .N
), by = .(mun_res, ano, dist_hv_res, seg_res, lat_res, lon_res)] %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_fed)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_fed[[v1]] <- ifelse(base_fed$ano == 2018, base_fed[[v]], NA_real_)
  
  base_fed[[v2]] <- ave(
    base_fed[[v1]], 
    base_fed$mun_res, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_fed[[dv]] <- base_fed[[v]] - base_fed[[v2]]
  
  base_fed[[dv]][!is.finite(base_fed[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_fed), value = TRUE)
base_fed <- base_fed %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia)

# ---------------------------------------------------------------------------- #
# State 
# ---------------------------------------------------------------------------- #

base_et <- base[dep_adm == 2, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  obs = .N
), by = .(mun_res, ano, dist_hv_res, seg_res, lat_res, lon_res)] %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_et)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_et[[v1]] <- ifelse(base_et$ano == 2018, base_et[[v]], NA_real_)
  
  base_et[[v2]] <- ave(
    base_et[[v1]], 
    base_et$mun_res, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_et[[dv]] <- base_et[[v]] - base_et[[v2]]
  
  base_et[[dv]][!is.finite(base_et[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_et), value = TRUE)
base_et <- base_et %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia)

# ---------------------------------------------------------------------------- #
# Mun
# ---------------------------------------------------------------------------- #
base_mun <- base[dep_adm == 3, .(
  media    = mean(media, na.rm = TRUE),
  escm     = mean(escm, na.rm = TRUE),
  escp     = mean(escp, na.rm = TRUE),
  pessoa   = mean(pessoas_dom, na.rm = TRUE),
  empr_dom = mean(empr_dom, na.rm = TRUE),
  n_ban    = mean(n_banheiro, na.rm = TRUE),
  n_qua    = mean(n_quartos, na.rm = TRUE),
  n_car    = mean(n_carros, na.rm = TRUE),
  n_gel    = mean(n_geladeira, na.rm = TRUE),
  n_cel    = mean(n_celular, na.rm = TRUE),
  pc       = mean(pc, na.rm = TRUE),
  internet = mean(internet, na.rm = TRUE),
  renda1   = mean(renda1, na.rm = TRUE),
  renda110 = mean(renda_1_10, na.rm = TRUE),
  renda10  = mean(renda_10, na.rm = TRUE),
  pibpc    = mean(pibpc, na.rm = TRUE),
  fem      = mean(fem, na.rm = TRUE),
  idade    = mean(id18, na.rm = TRUE),
  ppi      = mean(ppi, na.rm = TRUE),
  tempd1   = mean(temp_d1, na.rm = TRUE),
  tempd2   = mean(temp_d2, na.rm = TRUE),
  umidd2   = mean(umid_d2, na.rm = TRUE),
  umidd1   = mean(umid_d1, na.rm = TRUE),
  h13 = first(h13),
  h12 = first(h12),
  h11 = first(h11),
  obs = .N
), by = .(mun_res, ano, dist_hv_res, seg_res, lat_res, lon_res)] %>%
  filter(as.numeric(ano) %in% c(2018, 2019)) %>%
  arrange(mun_res, ano) %>%
  group_by(mun_res) %>%
  filter(n_distinct(ano) == 2) %>%
  ungroup()


for (v in vars_diff) {
  
  if (!v %in% names(base_mun)) {
    warning(paste("Variável não encontrada:", v))
    next
  }
  
  v1 <- paste0("v1_", v)
  v2 <- paste0("v2_", v)
  dv <- paste0("d", v)
  
  base_mun[[v1]] <- ifelse(base_mun$ano == 2018, base_mun[[v]], NA_real_)
  
  base_mun[[v2]] <- ave(
    base_mun[[v1]], 
    base_mun$mun_res, 
    FUN = function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }
  )
  
  base_mun[[dv]] <- base_mun[[v]] - base_mun[[v2]]
  
  base_mun[[dv]][!is.finite(base_mun[[dv]])] <- NA
}

temp_cols <- grep("^(v1_|v2_)", names(base_mun), value = TRUE)
base_mun <- base_mun %>% select(-all_of(temp_cols)) %>% 
  mutate(across(everything(), ~ replace(.x, is.infinite(.x), NA))) %>%  #Turning INF to NA
  rename(d.media = dmedia)

# ---------------------------------------------------------------------------- #
###15.3.1 Regression -----
# ---------------------------------------------------------------------------- #


ef <- dummy_cols(base_t$seg_res[base_t$ano == 2018])
ef <- ef %>% select(-1,-2)

# ---------------------------------------------------------------------------- #
#### 15.3.1.1 Todos Concluintes ----
# ---------------------------------------------------------------------------- #
list[[as.character(paste0(2019,"-",2018,"C|TC"))]] <- rdrobust(
  y = base_t$d.media[base_t$ano == 2019],
  x = base_t$dist_hv_res[base_t$ano == 2018],
  c = 0,
  cluster = base_t$seg_res[base_t$ano == 2018],
  weights = base_t$obs[base_t$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_t$lat_res[base_t$ano == 2018],
    base_t$lon_res[base_t$ano == 2018],
    
    #Weather
    base_t$dtempd1[base_t$ano == 2019], #Temperature
    base_t$dumidd1[base_t$ano == 2019], #Humidity d1
    base_t$dumidd2[base_t$ano == 2019], #Humidty d2
    base_t$dtempd2[base_t$ano == 2019], #temp2
    
    #House
    base_t$dn_ban[base_t$ano == 2019], #bathrooms
    base_t$dpessoa[base_t$ano == 2019], #people in household
    base_t$dn_qua[base_t$ano == 2019], #houses
    base_t$dn_car[base_t$ano == 2019], #cars
    base_t$dn_gel[base_t$ano == 2019], #geladeira
    base_t$dn_cel[base_t$ano == 2019], #cel
    base_t$dpc[base_t$ano == 2019],    #pc
    base_t$dinternet[base_t$ano == 2019], #internet
    base_t$dempr_dom[base_t$ano == 2019], #Housekeeping
    
    #remaining
    base_t$descm[base_t$ano == 2019], #mother educ
    base_t$dfem[base_t$ano == 2019], #Female
    base_t$dppi[base_t$ano == 2019], #PPI
    base_t$didade[base_t$ano == 2019], #Age
    base_t$descp[base_t$ano == 2019], #father educ
    base_t$drenda1[base_t$ano == 2019], #wage < 1MW
    base_t$drenda110[base_t$ano == 2019], #wage 1MW - 10MW
    base_t$drenda10[base_t$ano == 2019], #wage > 10MW
    base_t$dpibpc[base_t$ano == 2019], #pibpc
    
    #Fuso
    base_t$h13[base_t$ano == 2019],
    base_t$h12[base_t$ano == 2019],
    base_t$h11[base_t$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
#### 15.3.1.2  Escola Pub ----
# ---------------------------------------------------------------------------- #

ef <- dummy_cols(base_res$seg_res[base_res$ano == 2018])
ef <- ef %>% select(-1,-2)



list[[as.character(paste0(2019,"-",2018,"C|TCPub"))]] <- rdrobust(
  y = base_res$d.media[base_res$ano == 2019],
  x = base_res$dist_hv_res[base_res$ano == 2018],
  c = 0,
  p = 1, 
  cluster = base_res$seg_res[base_res$ano == 2018],
  weights = base_res$obs[base_res$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_res$lat_res[base_res$ano == 2018],
    base_res$lon_res[base_res$ano == 2018],
    
    #Weather
    base_res$dtempd1[base_res$ano == 2019], #Temperature
    base_res$dumidd1[base_res$ano == 2019], #Humidity d1
    base_res$dumidd2[base_res$ano == 2019], #Humidty d2
    base_res$dtempd2[base_res$ano == 2019], #temp2
    
    #House
    base_res$dn_ban[base_res$ano == 2019], #bathrooms
    base_res$dpessoa[base_res$ano == 2019], #people in household
    base_res$dn_qua[base_res$ano == 2019], #houses
    base_res$dn_car[base_res$ano == 2019], #cars
    base_res$dn_gel[base_res$ano == 2019], #geladeira
    base_res$dn_cel[base_res$ano == 2019], #cel
    base_res$dpc[base_res$ano == 2019],    #pc
    base_res$dinternet[base_res$ano == 2019], #internet
    base_res$dempr_dom[base_res$ano == 2019], #Housekeeping
    
    #remaining
    base_res$descm[base_res$ano == 2019], #mother educ
    base_res$dfem[base_res$ano == 2019], #Female
    base_res$dppi[base_res$ano == 2019], #PPI
    base_res$didade[base_res$ano == 2019], #Age
    base_res$descp[base_res$ano == 2019], #father educ
    base_res$drenda1[base_res$ano == 2019], #wage < 1MW
    base_res$drenda110[base_res$ano == 2019], #wage 1MW - 10MW
    base_res$drenda10[base_res$ano == 2019], #wage > 10MW
    base_res$dpibpc[base_res$ano == 2019], #pibpc
    
    #Fuso
    base_res$h13[base_res$ano == 2019],
    base_res$h12[base_res$ano == 2019],
    base_res$h11[base_res$ano == 2019]
  )
  )

# ---------------------------------------------------------------------------- #
#### 15.3.1.3 Escola Priv ----
# ---------------------------------------------------------------------------- #



ef <- dummy_cols(base_p$seg_res[base_p$ano == 2018])
ef <- ef %>% select(-1,-2)



list[[as.character(paste0(2019,"-",2018,"C|Priv"))]] <- rdrobust(
  y = base_p$d.media[base_p$ano == 2019],
  x = base_p$dist_hv_res[base_p$ano == 2018],
  c = 0,
  cluster = base_p$seg_res[base_p$ano == 2018],
  weights = base_p$obs[base_p$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_p$lat_res[base_p$ano == 2018],
    base_p$lon_res[base_p$ano == 2018],
    
    #Weather
    base_p$dtempd1[base_p$ano == 2019], #Temperature
    base_p$dumidd1[base_p$ano == 2019], #Humidity d1
    base_p$dumidd2[base_p$ano == 2019], #Humidty d2
    base_p$dtempd2[base_p$ano == 2019], #temp2
    
    #House
    base_p$dn_ban[base_p$ano == 2019], #bathrooms
    base_p$dpessoa[base_p$ano == 2019], #people in household
    base_p$dn_qua[base_p$ano == 2019], #houses
    base_p$dn_car[base_p$ano == 2019], #cars
    base_p$dn_gel[base_p$ano == 2019], #geladeira
    base_p$dn_cel[base_p$ano == 2019], #cel
    base_p$dpc[base_p$ano == 2019],    #pc
    base_p$dinternet[base_p$ano == 2019], #internet
    base_p$dempr_dom[base_p$ano == 2019], #Housekeeping
    
    #remaining
    base_p$descm[base_p$ano == 2019], #mother educ
    base_p$dfem[base_p$ano == 2019], #Female
    base_p$dppi[base_p$ano == 2019], #PPI
    base_p$didade[base_p$ano == 2019], #Age
    base_p$descp[base_p$ano == 2019], #father educ
    base_p$drenda1[base_p$ano == 2019], #wage < 1MW
    base_p$drenda110[base_p$ano == 2019], #wage 1MW - 10MW
    base_p$drenda10[base_p$ano == 2019], #wage > 10MW
    base_p$dpibpc[base_p$ano == 2019], #pibpc
    
    #Fuso
    base_p$h13[base_p$ano == 2019],
    base_p$h12[base_p$ano == 2019],
    base_p$h11[base_p$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
#### 15.3.1.4 Esc Pub - Fed ----
# ---------------------------------------------------------------------------- #
ef <- dummy_cols(base_psf$seg_res[base_psf$ano == 2018])
ef <- ef %>% select(-1,-2)



list[[as.character(paste0(2019,"-",2018,"C|PubSF"))]] <- rdrobust(
  y = base_psf$d.media[base_psf$ano == 2019],
  x = base_psf$dist_hv_res[base_psf$ano == 2018],
  c = 0,
  cluster = base_psf$seg_res[base_psf$ano == 2018],
  weights = base_psf$obs[base_psf$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_psf$lat_res[base_psf$ano == 2018],
    base_psf$lon_res[base_psf$ano == 2018],
    
    #Weather
    base_psf$dtempd1[base_psf$ano == 2019], #Temperature
    base_psf$dumidd1[base_psf$ano == 2019], #Humidity d1
    base_psf$dumidd2[base_psf$ano == 2019], #Humidty d2
    base_psf$dtempd2[base_psf$ano == 2019], #temp2
    
    #House
    base_psf$dn_ban[base_psf$ano == 2019], #bathrooms
    base_psf$dpessoa[base_psf$ano == 2019], #people in household
    base_psf$dn_qua[base_psf$ano == 2019], #houses
    base_psf$dn_car[base_psf$ano == 2019], #cars
    base_psf$dn_gel[base_psf$ano == 2019], #geladeira
    base_psf$dn_cel[base_psf$ano == 2019], #cel
    base_psf$dpc[base_psf$ano == 2019],    #pc
    base_psf$dinternet[base_psf$ano == 2019], #internet
    base_psf$dempr_dom[base_psf$ano == 2019], #Housekeeping
    
    #remaining
    base_psf$descm[base_psf$ano == 2019], #mother educ
    base_psf$dfem[base_psf$ano == 2019], #Female
    base_psf$dppi[base_psf$ano == 2019], #PPI
    base_psf$didade[base_psf$ano == 2019], #Age
    base_psf$descp[base_psf$ano == 2019], #father educ
    base_psf$drenda1[base_psf$ano == 2019], #wage < 1MW
    base_psf$drenda110[base_psf$ano == 2019], #wage 1MW - 10MW
    base_psf$drenda10[base_psf$ano == 2019], #wage > 10MW
    base_psf$dpibpc[base_psf$ano == 2019], #pibpc
    
    #Fuso
    base_psf$h13[base_psf$ano == 2019],
    base_psf$h12[base_psf$ano == 2019],
    base_psf$h11[base_psf$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
#### 15.3.1.5 Esc Estadual ----
# ---------------------------------------------------------------------------- #

ef <- dummy_cols(base_et$seg_res[base_et$ano == 2018])
ef <- ef %>% select(-1,-2)



list[[as.character(paste0(2019,"-",2018,"C|PubEsd"))]] <- rdrobust(
  y = base_et$d.media[base_et$ano == 2019],
  x = base_et$dist_hv_res[base_et$ano == 2018],
  c = 0,
  cluster = base_et$seg_res[base_et$ano == 2018],
  weights = base_et$obs[base_et$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_et$lat_res[base_et$ano == 2018],
    base_et$lon_res[base_et$ano == 2018],
    
    #Weather
    base_et$dtempd1[base_et$ano == 2019], #Temperature
    base_et$dumidd1[base_et$ano == 2019], #Humidity d1
    base_et$dumidd2[base_et$ano == 2019], #Humidty d2
    base_et$dtempd2[base_et$ano == 2019], #temp2
    
    #House
    base_et$dn_ban[base_et$ano == 2019], #bathrooms
    base_et$dpessoa[base_et$ano == 2019], #people in household
    base_et$dn_qua[base_et$ano == 2019], #houses
    base_et$dn_car[base_et$ano == 2019], #cars
    base_et$dn_gel[base_et$ano == 2019], #geladeira
    base_et$dn_cel[base_et$ano == 2019], #cel
    base_et$dpc[base_et$ano == 2019],    #pc
    base_et$dinternet[base_et$ano == 2019], #internet
    base_et$dempr_dom[base_et$ano == 2019], #Housekeeping
    
    #remaining
    base_et$descm[base_et$ano == 2019], #mother educ
    base_et$dfem[base_et$ano == 2019], #Female
    base_et$dppi[base_et$ano == 2019], #PPI
    base_et$didade[base_et$ano == 2019], #Age
    base_et$descp[base_et$ano == 2019], #father educ
    base_et$drenda1[base_et$ano == 2019], #wage < 1MW
    base_et$drenda110[base_et$ano == 2019], #wage 1MW - 10MW
    base_et$drenda10[base_et$ano == 2019], #wage > 10MW
    base_et$dpibpc[base_et$ano == 2019], #pibpc
    
    #Fuso
    base_et$h13[base_et$ano == 2019],
    base_et$h12[base_et$ano == 2019],
    base_et$h11[base_et$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
#### 15.3.1.6 Esc FED ----
# ---------------------------------------------------------------------------- #
ef <- dummy_cols(base_fed$seg_res[base_fed$ano == 2018])
ef <- ef %>% select(-1,-2)



list[[as.character(paste0(2019,"-",2018,"C|PubFed"))]] <- rdrobust(
  y = base_fed$d.media[base_fed$ano == 2019],
  x = base_fed$dist_hv_res[base_fed$ano == 2018],
  c = 0,
  cluster = base_fed$seg_res[base_fed$ano == 2018],
  weights = base_fed$obs[base_fed$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_fed$lat_res[base_fed$ano == 2018],
    base_fed$lon_res[base_fed$ano == 2018],
    
    #Weather
    base_fed$dtempd1[base_fed$ano == 2019], #Temperature
    base_fed$dumidd1[base_fed$ano == 2019], #Humidity d1
    base_fed$dumidd2[base_fed$ano == 2019], #Humidty d2
    base_fed$dtempd2[base_fed$ano == 2019], #temp2
    
    #House
    base_fed$dn_ban[base_fed$ano == 2019], #bathrooms
    base_fed$dpessoa[base_fed$ano == 2019], #people in household
    base_fed$dn_qua[base_fed$ano == 2019], #houses
    base_fed$dn_car[base_fed$ano == 2019], #cars
    base_fed$dn_gel[base_fed$ano == 2019], #geladeira
    base_fed$dn_cel[base_fed$ano == 2019], #cel
    base_fed$dpc[base_fed$ano == 2019],    #pc
    base_fed$dinternet[base_fed$ano == 2019], #internet
    base_fed$dempr_dom[base_fed$ano == 2019], #Housekeeping
    
    #remaining
    base_fed$descm[base_fed$ano == 2019], #mother educ
    base_fed$dfem[base_fed$ano == 2019], #Female
    base_fed$dppi[base_fed$ano == 2019], #PPI
    base_fed$didade[base_fed$ano == 2019], #Age
    base_fed$descp[base_fed$ano == 2019], #father educ
    base_fed$drenda1[base_fed$ano == 2019], #wage < 1MW
    base_fed$drenda110[base_fed$ano == 2019], #wage 1MW - 10MW
    base_fed$drenda10[base_fed$ano == 2019], #wage > 10MW
    base_fed$dpibpc[base_fed$ano == 2019], #pibpc
    
    #Fuso
    base_fed$h13[base_fed$ano == 2019],
    base_fed$h12[base_fed$ano == 2019],
    base_fed$h11[base_fed$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
#### 15.3.1.7 Esc MUN ----
# ---------------------------------------------------------------------------- #

ef <- dummy_cols(base_mun$seg_res[base_mun$ano == 2018])
ef <- ef %>% select(-1,-2)



list[[as.character(paste0(2019,"-",2018,"C|PubMun"))]] <- rdrobust(
  y = base_mun$d.media[base_mun$ano == 2019],
  x = base_mun$dist_hv_res[base_mun$ano == 2018],
  c = 0,
  cluster = base_mun$seg_res[base_mun$ano == 2018],
  weights = base_mun$obs[base_mun$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_mun$lat_res[base_mun$ano == 2018],
    base_mun$lon_res[base_mun$ano == 2018],
    
    #Weather
    base_mun$dtempd1[base_mun$ano == 2019], #Temperature
    base_mun$dumidd1[base_mun$ano == 2019], #Humidity d1
    base_mun$dumidd2[base_mun$ano == 2019], #Humidty d2
    base_mun$dtempd2[base_mun$ano == 2019], #temp2
    
    #House
    base_mun$dn_ban[base_mun$ano == 2019], #bathrooms
    base_mun$dpessoa[base_mun$ano == 2019], #people in household
    base_mun$dn_qua[base_mun$ano == 2019], #houses
    base_mun$dn_car[base_mun$ano == 2019], #cars
    base_mun$dn_gel[base_mun$ano == 2019], #geladeira
    base_mun$dn_cel[base_mun$ano == 2019], #cel
    base_mun$dpc[base_mun$ano == 2019],    #pc
    base_mun$dinternet[base_mun$ano == 2019], #internet
    base_mun$dempr_dom[base_mun$ano == 2019], #Housekeeping
    
    #remaining
    base_mun$descm[base_mun$ano == 2019], #mother educ
    base_mun$dfem[base_mun$ano == 2019], #Female
    base_mun$dppi[base_mun$ano == 2019], #PPI
    base_mun$didade[base_mun$ano == 2019], #Age
    base_mun$descp[base_mun$ano == 2019], #father educ
    base_mun$drenda1[base_mun$ano == 2019], #wage < 1MW
    base_mun$drenda110[base_mun$ano == 2019], #wage 1MW - 10MW
    base_mun$drenda10[base_mun$ano == 2019], #wage > 10MW
    base_mun$dpibpc[base_mun$ano == 2019], #pibpc
    
    #Fuso
    base_mun$h13[base_mun$ano == 2019],
    base_mun$h12[base_mun$ano == 2019],
    base_mun$h11[base_mun$ano == 2019]
  )
)

# ---------------------------------------------------------------------------- #
### 15.4 Result Table -----
# ---------------------------------------------------------------------------- #
tab <- data.frame(
  coef   = sapply(list, function(x) x$coef[3]),
  se     = sapply(list, function(x) x$se[3]),
  pv     = sapply(list, function(x) x$pv[3]),
  n_left = sapply(list, function(x) x$N_h[1]),
  n_rght = sapply(list, function(x) x$N_h[2]),
  bw     = sapply(list, function(x) x$bws[1, 1]),
  totr   = sapply(list, function(x) x$N[2]),
  totl   = sapply(list, function(x) x$N[1])
)

print(tab)



result <- data.frame(
  ` ` = c(
    "Main Result",
    " ", " ", " ",
    "Mock Examinees",
    " ", " ", " ",
    "All Administrative School Types",
    " ", " ", " ",
    "Private School",
    " ", " ", " ",
    "Municipal + State School",
    " ", " ", " ",
    "State School",
    " ", " ", " ",
    "Federal School",
    " ", " ", " ",
    "Municipal School",
    " ", " ", " "
  ),
  `(1)` = c(
    #Main
    fmt_est(tab$coef[3], tab$pv[3]),
    fmt_se(tab$se[3]),
    paste0("N$_L$ = ", fmt_n(tab$n_left[3]),", N$_R$ = ", fmt_n(tab$n_rght[3])),
    paste0("BW = ", fmt_bw(tab$bw[3])),
    #mock
    fmt_est(tab$coef[1], tab$pv[1]),
    fmt_se(tab$se[1]),
    paste0("N$_L$ = ", fmt_n(tab$n_left[1]),", N$_R$ = ", fmt_n(tab$n_rght[1])),
    paste0("BW = ", fmt_bw(tab$bw[1])),
    #All
    fmt_est(tab$coef[2], tab$pv[2]),
    fmt_se(tab$se[2]),
    paste0("N$_L$ = ", fmt_n(tab$n_left[2]),", N$_R$ = ", fmt_n(tab$n_rght[2])),
    paste0("BW = ", fmt_bw(tab$bw[2])),
    #Priv
    fmt_est(tab$coef[4], tab$pv[4]),
    fmt_se(tab$se[4]),
    paste0("N$_L$ = ", fmt_n(tab$n_left[4]),", N$_R$ = ", fmt_n(tab$n_rght[4])),
    paste0("BW = ", fmt_bw(tab$bw[4])),
    #Mun State
    fmt_est(tab$coef[5], tab$pv[5]),
    fmt_se(tab$se[5]),
    paste0("N$_L$ = ", fmt_n(tab$n_left[5]),", N$_R$ = ", fmt_n(tab$n_rght[5])),
    paste0("BW = ", fmt_bw(tab$bw[5])),
    #State
    fmt_est(tab$coef[6], tab$pv[6]),
    fmt_se(tab$se[6]),
    paste0("N$_L$ = ", fmt_n(tab$n_left[6]),", N$_R$ = ", fmt_n(tab$n_rght[6])),
    paste0("BW = ", fmt_bw(tab$bw[6])),
    #Federal
    fmt_est(tab$coef[7], tab$pv[7]),
    fmt_se(tab$se[7]),
    paste0("N$_L$ = ", fmt_n(tab$n_left[7]),", N$_R$ = ", fmt_n(tab$n_rght[7])),
    paste0("BW = ", fmt_bw(tab$bw[7])),
    #Mun
    fmt_est(tab$coef[8], tab$pv[8]),
    fmt_se(tab$se[8]),
    paste0("N$_L$ = ", fmt_n(tab$n_left[8]),", N$_R$ = ", fmt_n(tab$n_rght[8])),
    paste0("BW = ", fmt_bw(tab$bw[8]))
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)


# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  escape = F,
  align = "lc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/controls/comp_amostras_v1.tex")

rm(list = ls())
gc()
