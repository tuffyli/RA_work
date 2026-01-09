# ---------------------------------------------------------------------------- #
# Regressions
# Main estimations and Robustness
# Last edited by: Tuffy Licciardi Issa
# Date: 09/01/2026
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Libraries -----
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Reg Para os mais velhos ----
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



base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2019.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2018.RDS"))) %>%
  setDT() %>% 
  filter(conclusao == 2)

summary(base %>% select(idade, conclusao, esc_mae))

test <- base %>% 
  filter(priv0 == 1)

length(unique(test$id_enem))
rm(test)


base <- base %>% 
  mutate(
    
    escm = case_when(
      esc_mae %in% c("D","E","F") ~ 0,
      esc_mae %in% c("A","B","C") ~ 1,
      .default = NA),
    
    mae_trab_man = case_when(
      emp_mae %in% c("A","B","C") ~ 1,
      emp_mae %in% c("D","E","F") ~ 0,
      .default = NA
    ),
    
    pai_trab_man = case_when(
      emp_pai %in% c("A","B","C") ~ 1,
      emp_pai %in% c("D","E","F") ~ 0,
      .default = NA
    )
  )


base_a <- base[priv0 == 1,.(media = mean(media, na.rm = T),
                            idade = mean(id18, na.rm = T),
                            esc_mae = mean(escm, na.rm = T),
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
    d.media = media - v2_nota,
    
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_nota, v2_nota))

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
#rm(ef,list,base_a)



hv18 <- weighted.mean(base_a$media[base_a$ano == 2018 & base_a$dist_hv_border > 0 & abs(base_a$dist_hv_border) <= bw_main_a],
                      w = base_a$obs[base_a$ano == 2018 & base_a$dist_hv_border > 0 & abs(base_a$dist_hv_border) <= bw_main_a])
hv18_out <- weighted.mean(base_a$media[base_a$ano == 2018 & base_a$dist_hv_border < 0 & abs(base_a$dist_hv_border) <= bw_main_a],
                          w = base_a$obs[base_a$ano == 2018 & base_a$dist_hv_border < 0 & abs(base_a$dist_hv_border) <= bw_main_a])


hv18 - hv18_out

9.28/(hv18-hv18_out)


### 1.1.2 Add Control ----

list[[as.character(paste0(2019,"-",2018,"S|NF"))]] <- rdrobust(
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
    base_a$esc_mae[base_a$ano == 2018],
    base_a$idade[base_a$ano == 2018]
  )
)

### 1.1.3 Pol ----

list[[as.character(paste0(2019,"-",2018,"P|NF"))]] <- rdrobust(
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


## 1.2 Tabela  ----
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
  mor = rep(NA, times = length(names)),
  po2 = rep(NA, times = length(names))
)

# #TC Segmentos
# result$seg[1] <- t10$coef[[2]]
# result$seg[2] <- t10$se[[2]]
# result$seg[3] <- t10$N[[2]]

#TC Controles
result$con[1] <- t10$coef[[1]]
result$con[2] <- t10$se[[1]]
result$con[3] <- t10$N[[1]]

#Mais Cont
result$mor[1] <- t10$coef[[2]]
result$mor[2] <- t10$se[[2]]
result$mor[3] <- t10$N[[2]]


#TC Pol 2° Grau
result$po2[1] <- t10$coef[[3]]
result$po2[2] <- t10$se[[3]]
result$po2[3] <- t10$N[[3]]





colnames(result) <- c("", "(1)", "(2)", "(3)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/DIFF_Principal_TC_v2.tex")
rm(ef, list, result, t10, latex_table)

# ---------------------------------------------------------------------------- #
# ##1.3 Comparação Young vs. Old ----
# # ---------------------------------------------------------------------------- #

base <- base %>%
  mutate(
    old = ifelse(
      idade > 18 & conclusao == 2, 1,
      ifelse( idade %in% c(17, 18), 0, NA))
  )



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


# # ---------------------------------------------------------------------------- #
#2. Gráfico ----
## 2.1 Principal ----
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


#### 2.1.1 19-18 ----
fig <- list()

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
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main_a,
              nbins = 25,
              #b = bw_bias_a,
              weights = temp$obs[temp$ano == 2018],
              subset = temp$subset == 1,
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

xtips <- seq(-4*10^5,4*10^5,10^5)

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
       y = "Overall ENEM \n average") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-20,20) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/RDD_Principal_1918_v2.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/RDD_Principal_1918_v2.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)

# #### 2.1.2 19-17 ----
# 
# 
# base_temp <- base %>%
#   select(-old) %>%
#   bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2017.RDS"))) %>%
#   setDT() %>%
#   filter(conclusao == 2)
# 
# ##### 2.1.2.1 opt bw -----
# base_b <- base_temp[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
#                     by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] %>%
#   filter(as.numeric(ano) %in% c(2017,2019)) %>%
#   arrange(mun_prova,ano) %>%
#   group_by(mun_prova) %>%
#   mutate(
#     dup1 = 1,
#     dup2 = sum(dup1),
#     v1_nota = ifelse(ano == 2017, media, NA),
#     v2_nota = max(v1_nota, na.rm = T),
#     d.media = media - v2_nota
#   ) %>%
#   ungroup() %>%
#   filter(dup2 == 2) %>%
#   select(-c(dup2, dup1, v1_nota, v2_nota))
# 
# 
# ef <- dummy_cols(base_b$seg[base_b$ano == 2017])
# ef <- ef %>% select(-1,-2)
# 
# list <- list()
# 
# list[[as.character(paste0(2019,"-",2017,"C|NF"))]] <- rdrobust(
#   y = base_b$d.media[base_b$ano == 2019],
#   x = base_b$dist_hv_border[base_b$ano == 2017],
#   c = 0,
#   cluster = base_b$seg[base_b$ano == 2017],
#   weights = base_b$obs[base_b$ano == 2017],
#   vce = "hc0",
#   covs = cbind(
#     ef,
#     base_b$lat[base_b$ano == 2017],
#     base_b$lon[base_b$ano == 2017]
#   )
# )
# 
# # ---------------------------------------------------------------------------- #
# #Extração da banda ótima
# bw_main_b  <- list[["2019-2017C|NF"]]$bws[1]
# bw_bias_b  <- list[["2019-2017C|NF"]]$bws[2]
# # ---------------------------------------------------------------------------- #
# 
# rm(list, ef)
# ##### 2.1.2.2. graph -----
# fig <- list()
# 
# temp <- base_b %>%
#   mutate(subset = case_when(
#     abs(dist_hv_border) < bw_main_b ~ 1,
#     .default = 0
#   )
#   ) %>%
#   filter(
#     !is.na(d.media),
#     subset == 1
#   )
# 
# # Dependent variable
# yv <- temp %>%
#   filter(ano == 2019) %>%
#   select(d.media) %>%
#   rename(vd = 1)
# 
# #45696
# # Running variable
# xv <- temp %>%
#   filter(ano == 2017) %>%
#   select(dist_hv_border)
# 
# # Clusters
# clu <- temp %>%
#   filter(ano == 2017) %>%
#   select(seg)
# 
# # Latitude
# latv <- temp %>%
#   filter(ano == 2017) %>%
#   select(lat)
# 
# # Longitude
# lonv <- temp %>%
#   filter(ano == 2017) %>%
#   select(lon)
# 
# ef <- dummy_cols(clu$seg)
# ef <- ef %>% select(-1,-2)
# 
# # Estimando parâmetros do gráfico
# fig <- rdplot(y = yv$vd,
#               x = xv$dist_hv_border,
#               c = 0,
#               p = 1,
#               nbins = 25,
#               #binselect = "esmv",
#               kernel = "triangular",
#               h = bw_main_b,
#               weights = temp$obs[temp$ano == 2017],
#               subset = temp$subset == 1,
#               hide = F,
#               masspoints= "adjust",
#               covs = cbind(ef,latv,lonv)
# )
# 
# rm(yv, xv, clu, latv, lonv, ef, temp)
# 
# 
# 
# # Vetores e valores auxiliares
# fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
# fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1
# 
# x_r_sta <- 0
# x_r_end <- max(fig$vars_poly$rdplot_x)
# y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2],
#                   min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
#                   max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
# y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]
# 
# x_l_sta <- min(fig$vars_poly$rdplot_x)
# x_l_end <- 0
# 
# y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
# y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2],
#                   max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
#                   min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
# 
# xtips <- seq(-4*10^5,4*10^5,10^5)
# 
# # Gráfico
# fig_gg <- ggplot() +
#   geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
#              alpha = 0.5, size = 2, show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   scale_color_brewer(palette = "Set1") +
#   geom_segment(aes(x = x_l_sta,
#                    xend = x_l_end,
#                    y = y_l_sta,
#                    yend = y_l_end),
#                size = 1.5, color = "#E41A4C") +
#   geom_segment(aes(x = x_r_sta,
#                    xend = x_r_end,
#                    y = y_r_sta,
#                    yend = y_r_end),
#                size = 1.5, color = "#377EB8") +
#   labs(x = "Distance to DST Border (km)",
#        y = "Overall ENEM \n average") +
#   theme_bw() +
#   scale_x_continuous(breaks = xtips,
#                      labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
#   ylim(-5,25) + 
#   theme(axis.title.x = element_text(size = 35),
#         axis.title.y = element_text(size = 35),
#         axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
#         axis.text.y = element_text(size = 30))
# 
# 
# fig_gg
# 
# ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/RDD_Principal_1917_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
# ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/RDD_Principal_1917_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)
# 
# rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)
# 
# 
# 
# # ---------------------------------------------------------------------------- #

#### 2.1.3 18-17 ----


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
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              nbins = 25,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main_c,
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

ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/RDD_Principal_1817_v2.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/RDD_Principal_1817_v2.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)



# ---------------------------------------------------------------------------- #
## 2.2 Bins ----
# ---------------------------------------------------------------------------- #

### A. 1918 ----
#### 2.2.1 POL 1 ------
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
      ylim(-15,15) + 
    theme(axis.title.x = element_text(size = 35),
          axis.title.y = element_text(size = 35),
          axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 30))

  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/1_bins_",i,"_pol1_1918_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/1_bins_",i,"_pol1_1918_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)

}


#### 2.2.2 POL 2 ------
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
                                             p = 4,
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
         y = "Overall ENEM \n average") +
    theme_bw() +
    scale_x_continuous(breaks = xtips,
                       labels = (xtips /1000 ) %>% formatC(digits = 0, format = "f")) +
      ylim(-15,15) + 
    theme(axis.title.x = element_text(size = 35),
          axis.title.y = element_text(size = 35),
          axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 30))


  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/1_bins_",i,"_pol4_1918_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/1_bins_",i,"_pol4_1918_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips, vars_poly_left, vars_poly_right)

}

rm(bins, j, plist, fig_loop)
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #

### B. 1917 ----
#### 2.3.1 POL 1 ------
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
    theme(axis.title.x = element_text(size = 35),
          axis.title.y = element_text(size = 35),
          axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 30))

  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/2_bins_",i,"_pol1_1917_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/2_bins_",i,"_pol1_1917_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)

}


#### 2.3.2 POL 2 ------
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
                                             p = 4,
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
    theme(axis.title.x = element_text(size = 35),
          axis.title.y = element_text(size = 35),
          axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 30))


  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/2_bins_",i,"_pol4_1917_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/2_bins_",i,"_pol4_1917_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

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
         y = "Overall ENEM \n average") +
    theme_bw() +
    scale_x_continuous(breaks = xtips,
                       labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
      ylim(-5,25) + 
    theme(axis.title.x = element_text(size = 35),
          axis.title.y = element_text(size = 35),
          axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 30))

  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/3_bins_",i,"_pol1_1817_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/3_bins_",i,"_pol1_1817_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)

}


#### 2.3.2 POL 2 ------
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
                                             p = 4,
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
         y = "Overall ENEM \n average") +
    theme_bw() +
    scale_x_continuous(breaks = xtips,
                       labels = (xtips /1000 ) %>% formatC(digits = 0, format = "f")) +
      ylim(-5,25) + 
    theme(axis.title.x = element_text(size = 35),
          axis.title.y = element_text(size = 35),
          axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 30))


  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/3_bins_",i,"_pol4_1817_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
  ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bins/3_bins_",i,"_pol4_1817_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

  rm(i,fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips, vars_poly_left, vars_poly_right)

}

rm(bins, j, plist, fig_loop, base_c, base_b, base_temp, bw_bias_b, bw_main_b,
   bw_main_c, bw_bias_c)
# ---------------------------------------------------------------------------- #

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
## 3.3 Gráfico redação ----
# ---------------------------------------------------------------------------- #

fig <- list()

temp <- base_a %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main_a ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.media_rd),
    subset == 1
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
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main_a,
              nbins = 25,
              #b = bw_bias_a,
              weights = temp$obs[temp$ano == 2018],
              subset = temp$subset == 1,
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

xtips <- seq(-4*10^5,4*10^5,10^5)

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
  ylim(15,160) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/RDD_Redacao.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/RDD_Redacao.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)




rm(notas_tab, p_list, result, d_list, latex_table)


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


# # ---------------------------------------------------------------------------- #
# # 5. Dificuldade ----
# # ---------------------------------------------------------------------------- #
# gc()
# # Agregados dos critérios
# base_a <- base[priv0 == 1,.(media_ac_bl_ch = mean(acerto_bl_ch, na.rm = T),
#                             media_ac_bh_ch = mean(acerto_bh_ch, na.rm = T),
#                             media_ac_bl_cn = mean(acerto_bl_cn, na.rm = T),
#                             media_ac_bh_cn = mean(acerto_bh_cn, na.rm = T),
#                             media_ac_bl_lc = mean(acerto_bl_lc, na.rm = T),
#                             media_ac_bh_lc = mean(acerto_bh_lc, na.rm = T),
#                             media_ac_bl_mt = mean(acerto_bl_mt, na.rm = T),
#                             media_ac_bh_mt = mean(acerto_bh_mt, na.rm = T),
#                             mediabl = mean(acerto_pbl, na.rm = T),
#                             mediabh = mean(acerto_pbh, na.rm = T),
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
#     dup2 = sum(dup1)) %>% 
#   ungroup() %>% 
#   filter(dup2 == 2) %>%
#   group_by(mun_prova) %>% 
#   mutate(
#     
#     #TOTAL
#     
#     v1_pbl = ifelse(ano == 2018, mediabl, NA),
#     v2_pbl = max(v1_pbl, na.rm = T),
#     d.mediabl = mediabl - v2_pbl,
#     
#     v1_pbh = ifelse(ano == 2018, mediabh, NA),
#     v2_pbh = max(v1_pbh, na.rm = T),
#     d.mediabh = mediabh - v2_pbh,
#     
#     # Ciências Humanas
#     ## Facil
#     v1_bl_ch = ifelse(ano == 2018, media_ac_bl_ch, NA),
#     v2_bl_ch = max(v1_bl_ch, na.rm = T),
#     d.media_bl_ch = media_ac_bl_ch - v2_bl_ch,
#     
#     ## Dificil
#     v1_bh_ch = ifelse(ano == 2018, media_ac_bh_ch, NA),
#     v2_bh_ch = max(v1_bh_ch, na.rm = T),
#     d.media_bh_ch = media_ac_bh_ch - v2_bh_ch,
#     
#     
#     # Ciências Naturais
#     ## Facil
#     v1_bl_cn = ifelse(ano == 2018, media_ac_bl_cn, NA),
#     v2_bl_cn = max(v1_bl_cn, na.rm = T),
#     d.media_bl_cn = media_ac_bl_cn - v2_bl_cn,
#     
#     ## Dificil
#     v1_bh_cn = ifelse(ano == 2018, media_ac_bh_cn, NA),
#     v2_bh_cn = max(v1_bh_cn, na.rm = T),
#     d.media_bh_cn = media_ac_bh_cn - v2_bh_cn,
#     
#     
#     # Lingua
#     ## Facil
#     v1_bl_lc = ifelse(ano == 2018, media_ac_bl_lc, NA),
#     v2_bl_lc = max(v1_bl_lc, na.rm = T),
#     d.media_bl_lc = media_ac_bl_lc - v2_bl_lc,
#     
#     ## Dificil
#     v1_bh_lc = ifelse(ano == 2018, media_ac_bh_lc, NA),
#     v2_bh_lc = max(v1_bh_lc, na.rm = T),
#     d.media_bh_lc = media_ac_bh_lc - v2_bh_lc,
#     
#     
#     # Matematica
#     ## Facil
#     v1_bl_mt = ifelse(ano == 2018, media_ac_bl_mt, NA),
#     v2_bl_mt = max(v1_bl_mt, na.rm = T),
#     d.media_bl_mt = media_ac_bl_mt - v2_bl_mt,
#     
#     ## Dificil
#     v1_bh_mt = ifelse(ano == 2018, media_ac_bh_mt, NA),
#     v2_bh_mt = max(v1_bh_mt, na.rm = T),
#     d.media_bh_mt = media_ac_bh_mt - v2_bh_mt
#     
#   ) %>%
#   ungroup() %>%
#   filter(dup2 == 2) %>%
#   select(-c(dup1,dup2,
#             v1_bl_ch,v2_bl_ch, v1_bh_ch, v2_bh_ch,
#             v1_bl_cn,v2_bl_cn, v1_bh_cn, v2_bh_cn,
#             v1_bl_lc,v2_bl_lc, v1_bh_lc, v2_bh_lc,
#             v1_bl_mt,v2_bl_mt, v1_bh_mt, v2_bh_mt,
#             v1_pbh, v1_pbl, v2_pbl, v2_pbh))
# 
# 
# ##5.1 Reg ----
# rp_list <- list()
# 
# d_list <- c("d.mediabl", "d.mediabh", "d.media_bl_lc", "d.media_bh_lc",
#             "d.media_bl_ch", "d.media_bh_ch", "d.media_bl_cn", "d.media_bh_cn",
#             "d.media_bl_mt", "d.media_bh_mt")
# 
# ### A. TC ----
# for (i in d_list){
#   
#   #Com Controles
#   
#   ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
#   ef <- ef %>% select(-1,-2)
#   
#   
#   rp_list[[as.character(paste0("CC_",i,"|TC"))]] <-
#     rdrobust(
#       y = base_a[[i]][base_a$ano == 2019],
#       x = base_a$dist_hv_border[base_a$ano == 2018],
#       c = 0,
#       h = bw_main_a,
#       b = bw_bias_a,
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
# ###5.1.1DIF----
# #### A. TC ----
# base_a <- base_a %>% 
#   group_by(mun_prova,
#            ano) %>% 
#   mutate(
#     dif_avg = d.mediabl - d.mediabh,
#     dif_lc = d.media_bl_lc - d.media_bh_lc,
#     dif_ch = d.media_bl_ch - d.media_bh_ch,
#     dif_cn = d.media_bl_cn - d.media_bh_cn,
#     dif_mt = d.media_bl_mt - d.media_bh_mt
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
#   dif_list[[as.character(paste0("cc_",i,"|TC"))]] <-
#     rdrobust(
#       y = base_a[[i]][base_a$ano == 2019],
#       x = base_a$dist_hv_border[base_a$ano == 2018],
#       c = 0,
#       h = bw_main_a,
#       b = bw_bias_a,
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
# rm(ef,i, new_list)
# 
# 
# 
# ##5.2 Tab ----
# 
# 
# mat_nota <- data.frame(
#   coef = do.call(rbind,lapply(rp_list, FUN = function(x){x$coef[3]})),
#   se = do.call(rbind,lapply(rp_list, FUN = function(x){x$se[3]})),
#   pv = do.call(rbind,lapply(rp_list, FUN = function(x){x$pv[3]})),
#   n = do.call(rbind,lapply(rp_list, FUN = function(x){x$N_h}))
# )
# 
# 
# 
# 
# 
# mat_nota <- mat_nota %>% 
#   mutate(
#     coef = paste0(formatC(x = coef, digits = 2, format = "f"),
#                   ifelse(pv < 0.01, "**", 
#                          ifelse(pv < 0.05, "*", 
#                                 ifelse(pv < 0.1, "", "")
#                          ))),
#     se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
#     pv = formatC(x = pv, digits = 3, format = "f"),
#     N = paste0("[N = ", n.1 + n.2, "]"),
#     #esp = 1:20,
#     id = 1
#   ) %>%
#   select(-c(pv))
# 
# 
# 
# 
# ## DIF TAB ----
# 
# dif_tab <- data.frame(
#   coef = do.call(rbind,lapply(dif_list, FUN = function(x){x$coef[3]})),
#   se = do.call(rbind,lapply(dif_list, FUN = function(x){x$se[3]})),
#   pv = do.call(rbind,lapply(dif_list, FUN = function(x){x$pv[3]})),
#   n = do.call(rbind,lapply(dif_list, FUN = function(x){x$N_h}))
# ) 
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
#     #esp = 1:10,
#     id = 1
#   ) %>%
#   select(-c(pv))
# 
# 
# 
# row1 <- c("Easier",
#           " "," ",
#           "More Difficult",
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
#   avg = rep(NA, times = length(row1)),
#   lc = rep(NA, times = length(row1)),
#   ch = rep(NA, times = length(row1)),
#   cn = rep(NA, times = length(row1)),
#   mt = rep(NA, times = length(row1))
# )
# 
# ####### TC ----
# #AVG
# result$avg[1] <- mat_nota$coef[[1]]
# result$avg[2] <- mat_nota$se[[1]]
# result$avg[3] <- mat_nota$N[[1]]
# result$avg[4] <- mat_nota$coef[[2]]
# result$avg[5] <- mat_nota$se[[2]]
# result$avg[6] <- mat_nota$N[[2]]
# 
# result$avg[7] <- dif_tab$coef[[1]]
# result$avg[8] <- dif_tab$se[[1]]
# result$avg[9] <- dif_tab$N[[1]]
# 
# #Lang
# result$lc[1] <- mat_nota$coef[[3]]
# result$lc[2] <- mat_nota$se[[3]]
# result$lc[3] <- mat_nota$N[[3]]
# result$lc[4] <- mat_nota$coef[[4]]
# result$lc[5] <- mat_nota$se[[4]]
# result$lc[6] <- mat_nota$N[[4]]
# 
# result$lc[7] <- dif_tab$coef[[2]]
# result$lc[9] <- dif_tab$se[[2]]
# result$lc[9] <- dif_tab$N[[2]]
# 
# 
# #Cien Humanas
# result$ch[1] <- mat_nota$coef[[5]]
# result$ch[2] <- mat_nota$se[[5]]
# result$ch[3] <- mat_nota$N[[5]]
# result$ch[4] <- mat_nota$coef[[6]]
# result$ch[5] <- mat_nota$se[[6]]
# result$ch[6] <- mat_nota$N[[6]]
# 
# result$ch[7] <- dif_tab$coef[[3]]
# result$ch[8] <- dif_tab$se[[3]]
# result$ch[9] <- dif_tab$N[[3]]
# 
# 
# #Cien Nat
# result$cn[1] <- mat_nota$coef[[7]]
# result$cn[2] <- mat_nota$se[[7]]
# result$cn[3] <- mat_nota$N[[7]]
# result$cn[4] <- mat_nota$coef[[8]]
# result$cn[5] <- mat_nota$se[[8]]
# result$cn[6] <- mat_nota$N[[8]]
# 
# result$cn[7] <- dif_tab$coef[[4]]
# result$cn[8] <- dif_tab$se[[4]]
# result$cn[9] <- dif_tab$N[[4]]
# 
# 
# #MT
# result$mt[1] <- mat_nota$coef[[9]]
# result$mt[2] <- mat_nota$se[[9]]
# result$mt[3] <- mat_nota$N[[9]]
# result$mt[4] <- mat_nota$coef[[10]]
# result$mt[5] <- mat_nota$se[[10]]
# result$mt[6] <- mat_nota$N[[10]]
# 
# result$mt[7] <- dif_tab$coef[[5]]
# result$mt[8] <- dif_tab$se[[5]]
# result$mt[9] <- dif_tab$N[[5]]
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
# 
# writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Dificuldade_v1.tex")
# 
# 
# rm(dif_list, dif_tab, mat_nota, result, rp_list, latex_table, d_list, row1)
# 
# 
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
    d.media_d2 = media_dia2 - v2_d2,
    
    
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>% 
  select(-c(v1_d1, v2_d1, v2_d2, v1_d2))



p_list <- list()

d_list <- c("d.media_d1", "d.media_d2")

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
# 8.1 Abstenções ----
# ---------------------------------------------------------------------------- #
gc()

base_abs <- base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_abs_2019.RDS")) %>%
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

# ---------------------------------------------------------------------------- #
# 9. Anexo ----
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
## 9.0 OLD ----
# ---------------------------------------------------------------------------- #

#Sem filtro de ESCOLA Pública
base_nf <- readRDS("Z:/Tuffy/Paper - HV/Bases/No filter/base_nota_2019.RDS") %>%
  bind_rows(readRDS("Z:/Tuffy/Paper - HV/Bases/No filter/base_nota_2018.RDS")) %>% 
  setDT()

summary(base_nf$conclusao)


base_nf <- base_nf %>% 
  mutate(
    conclude = ifelse(
      conclusao == 1, 1,
      ifelse( conclusao == 2, 0, NA))
  )



rlist <- list()

for (j in c(0:1)){
  
  print(j)
  base_y <- base_nf %>% 
    filter( conclude == j)
  
  base_a <- base_y[,.(media = mean(media, na.rm = T), obs = .N),
                   by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] %>% 
    filter(as.numeric(ano) %in% c(2018,2019)) %>% 
    arrange(mun_prova,ano) %>%
    group_by(mun_prova) %>%
    mutate(
      dup1 = 1,
      dup2 = sum(dup1)) %>%
    ungroup() %>% 
    filter(dup2 == 2) %>% 
    group_by(mun_prova) %>% 
    mutate(
      v1_nota = ifelse(ano == 2018, media, NA),
      v2_nota = max(v1_nota, na.rm = T),
      d.media = media - v2_nota 
    ) %>% 
    select(-c(dup2, dup1, v1_nota, v2_nota))
  
  rm(base_y)
  
  
  
  ef <- dummy_cols(base_a$seg[base_a$ano == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  rlist[[as.character(paste0("old =",j,"|NC"))]] <- rdrobust(
    y = base_a$d.media[base_a$ano == 2019],
    x = base_a$dist_hv_border[base_a$ano == 2018],
    c = 0,
    cluster = base_a$seg[base_a$ano == 2018],
    weights = base_a$obs[base_a$ano == 2018],
    vce = "hc0"
    # ,
    # covs = cbind(
    #   ef,
    #   base_a$lat[base_a$ano == 2018],
    #   base_a$lon[base_a$ano == 2018]
    # )
  )
  
  
  
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
  "Senior Year",
  " "," ",
  "Concluded",
  " "," "
)

result <- data.frame(
  var = names,
  nc = rep(NA, times = length(names)),
  cc= rep(NA, times = length(names)),
  bw = rep(NA, times = length(names))
  
)


result$nc[1] <- tab$coef[[1]]
result$nc[2] <- tab$se[[1]]
result$nc[3] <- tab$N[[1]]
result$cc[1] <- tab$coef[[2]]
result$cc[2] <- tab$se[[2]]
result$cc[3] <- tab$N[[2]]
result$bw[1] <- tab$coef[[3]]
result$bw[2] <- tab$se[[3]]
result$bw[3] <- tab$N[[3]]

result$nc[4] <- tab$coef[[4]]
result$nc[5] <- tab$se[[4]]
result$nc[6] <- tab$N[[4]]
result$cc[4] <- tab$coef[[5]]
result$cc[5] <- tab$se[[5]]
result$cc[6] <- tab$N[[5]]
result$bw[4] <- tab$coef[[6]]
result$bw[5] <- tab$se[[6]]
result$bw[6] <- tab$N[[6]]



colnames(result) <- c("", "(1)", "(2)", "(3)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/conc_vs_nonconc_v1.tex")

rm(base_a, names, result, rlist, tab, latex_table, base_nf)






# ---------------------------------------------------------------------------- #
# 10. Anos ----
# ---------------------------------------------------------------------------- #

base <- data.frame()

for(ano in 2013:2019) {
  base_temp <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_",ano,".RDS")) %>% 
    select(
      mun_prova, ano, id_enem, hv, priv0, lon, lat, dist_hv_border, seg, media, idade, conclusao
    ) %>% 
    filter(
      conclusao == 2
    )
  
  base_nota <- base_temp[priv0 == 1,.(media_nota = mean(media, na.rm = T),
                              obs = .N),
                 by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] 
  
  base_nota <- base_nota %>% 
    filter(abs(dist_hv_border) <= bw_main_a)
  
  
  summary(base$conclusao)

  saveRDS(base_nota, file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/Agregados/base_nota_ag_",ano,".RDS"))
  
  base <- rbind(base, base_temp)
  
  rm(base_temp, base_nota)
}


pad <- function(x){
  x = (x - mean(x, na.rm = T)) / 
    sd(x, na.rm = T)
  return(x)
}


base <- base %>% 
  mutate(media_p = pad(media))

base_a <- base[priv0 == 1,.(media_nota = mean(media, na.rm = T),
                            obs = .N),
               by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] 



### Reg ----
rlist <- list()
c_rlist <- list()




ano_list <- c(2013:2018)

### A. TC ----

for(ano_ref in ano_list) {
  
  
  ano_comp <- ano_ref + 1
  
  
  base_t <- base_a %>%
    filter(as.numeric(ano) %in% c(ano_ref,ano_comp)) %>% 
    arrange(mun_prova,ano) %>%
    group_by(mun_prova) %>%
    mutate(
      dup1 = 1,
      dup2 = sum(dup1)) %>% 
    ungroup() %>% 
    filter(dup2 == 2) %>% 
    group_by(mun_prova) %>%
    mutate(
      v1_nota = ifelse(ano == ano_ref, media_nota, NA),
      v2_nota = max(v1_nota, na.rm = T),
      d.media = media_nota - v2_nota 
    ) %>%
    ungroup() %>% 
    select(-c(dup2, dup1, v1_nota, v2_nota))
  
  
  
  
  
  #Com controles
  ef <- dummy_cols(base_t$seg[base_t$ano == ano_ref])
  ef <- ef %>% select(-1,-2)
  
  c_rlist[[as.character(paste0(ano_comp,"-",ano_ref,"C|TC"))]] <- rdrobust(
    y = base_t$d.media[base_t$ano == ano_comp],
    x = base_t$dist_hv_border[base_t$ano == ano_ref],
    c = 0,
    h = bw_main_a,
    b = bw_bias_a,
    cluster = base_t$seg[base_t$ano == ano_ref],
    weights = base_t$obs[base_t$ano == ano_ref],
    vce = "hc0",
    covs = cbind(
      ef,
      base_t$lat[base_t$ano == ano_ref],
      base_t$lon[base_t$ano == ano_ref]
    )
  )
  
  rm(ano_ref, ano_comp, base_t)
  
}
rm(ef)




### Resultados ----


t10cc <- data.frame(
  coef = do.call(rbind,lapply(c_rlist, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(c_rlist, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(c_rlist, FUN = function(x){x$pv[3]})),
  n = do.call(rbind, lapply(c_rlist, FUN = function(x){x$N_h}))
) %>% 
  mutate(
    N = n.1 + n.2
  ) %>% 
  select(-c(n.1, n.2))





names <- c("2014 - 2013",
           " ",
           " ",
           "2015 - 2014",
           " ",
           " ",
           "2016 - 2015",
           " ",
           " ",
           "2017 - 2016",
           " ",
           " ",
           "2018 - 2017",
           " ",
           " ",
           "2019 - 2018",
           " ",
           " ")

result <- data.frame(
  var = names,
  #tcnc = rep(NA, times = length(names)),
  tccc = rep(NA, times = length(names))#,
  #jcnc = rep(NA, times = length(names)),
  #jccc = rep(NA, times = length(names))
)



#Controles
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
    id = 1:6
  ) 
# %>%
#   select(-c(pv,se)) %>%
#   setDT() %>%
#   dcast(id ~ esp, value.var = c("coef"),fill = "") %>%
#   select(-id)

#Base A
result$tccc[1] <- t10cc$coef[[1]]
result$tccc[2] <- t10cc$se[[1]]
result$tccc[3] <- t10cc$N[[1]]
result$tccc[4] <- t10cc$coef[[2]]
result$tccc[5] <- t10cc$se[[2]]
result$tccc[6] <- t10cc$N[[2]]
result$tccc[7] <- t10cc$coef[[3]]
result$tccc[8] <- t10cc$se[[3]]
result$tccc[9] <- t10cc$N[[3]]
result$tccc[10] <- t10cc$coef[[4]]
result$tccc[11] <- t10cc$se[[4]]
result$tccc[12] <- t10cc$N[[4]]
result$tccc[13] <- t10cc$coef[[5]]
result$tccc[14] <- t10cc$se[[5]]
result$tccc[15] <- t10cc$N[[5]]
result$tccc[16] <- t10cc$coef[[6]]
result$tccc[17] <- t10cc$se[[6]]
result$tccc[18] <- t10cc$N[[6]]





# t10_final <- cbind(t10nc, t10cc)
# 

colnames(result) <- c("", "(1)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lc",
  linesep = ""
)

writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Anos_v1.tex")

# ---------------------------------------------------------------------------- #
## 10.1 Nível ----
# ---------------------------------------------------------------------------- #


c_rlist <- list()

ano_list <- c(2013:2019)

for(ano_ref in ano_list) {
  
  base_t <- base_a %>% 
    filter(ano == ano_ref)
  
  #Com controles
  ef <- dummy_cols(base_t$seg[base_t$ano == ano_ref])
  ef <- ef %>% select(-1,-2)
  
  c_rlist[[as.character(paste0(ano_ref,"C|TC"))]] <- rdrobust(
    y = base_t$media_nota[base_t$ano == ano_ref],
    x = base_t$dist_hv_border[base_t$ano == ano_ref],
    c = 0,
    h = bw_main_a,
    b = bw_bias_a,
    cluster = base_t$seg[base_t$ano == ano_ref],
    weights = base_t$obs[base_t$ano == ano_ref],
    vce = "hc0",
    covs = cbind(
      ef,
      base_t$lat[base_t$ano == ano_ref],
      base_t$lon[base_t$ano == ano_ref]
    )
  )
  
  rm(ano_ref, base_t)
  
}



t10cc <- data.frame(
  coef = do.call(rbind,lapply(c_rlist, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(c_rlist, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(c_rlist, FUN = function(x){x$pv[3]})),
  n = do.call(rbind, lapply(c_rlist, FUN = function(x){x$N_h})),
  t = do.call(rbind, lapply(c_rlist, FUN = function(x){x$N}))
) %>% 
  mutate(
    N = n.1 + n.2,
    Tot = t.1 + t.2
  ) %>% 
  select(-c(n.1, n.2))





names <- c("2013",
           " "," ",
           "2014",
           " "," ",
           "2015",
           " "," ",
           "2016",
           " "," ",
           "2017",
           " "," ",
           "2018",
           " "," ",
           "2019",
           " ", " ")

result <- data.frame(
  var = names,
  #tcnc = rep(NA, times = length(names)),
  tccc = rep(NA, times = length(names)),
  mun = rep(NA, times = length(names))
  #jccc = rep(NA, times = length(names))
)



#Controles
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
    id = 1:7
  ) 
# %>%
#   select(-c(pv,se)) %>%
#   setDT() %>%
#   dcast(id ~ esp, value.var = c("coef"),fill = "") %>%
#   select(-id)

#Base A
result$tccc[1] <- t10cc$coef[[1]]
result$tccc[2] <- t10cc$se[[1]]
result$tccc[3] <- t10cc$N[[1]]
result$tccc[4] <- t10cc$coef[[2]]
result$tccc[5] <- t10cc$se[[2]]
result$tccc[6] <- t10cc$N[[2]]
result$tccc[7] <- t10cc$coef[[3]]
result$tccc[8] <- t10cc$se[[3]]
result$tccc[9] <- t10cc$N[[3]]
result$tccc[10] <- t10cc$coef[[4]]
result$tccc[11] <- t10cc$se[[4]]
result$tccc[12] <- t10cc$N[[4]]
result$tccc[13] <- t10cc$coef[[5]]
result$tccc[14] <- t10cc$se[[5]]
result$tccc[15] <- t10cc$N[[5]]
result$tccc[16] <- t10cc$coef[[6]]
result$tccc[17] <- t10cc$se[[6]]
result$tccc[18] <- t10cc$N[[6]]
result$tccc[19] <- t10cc$coef[[7]]
result$tccc[20] <- t10cc$se[[7]]
result$tccc[21] <- t10cc$N[[7]]

result$mun[1] <- " "
result$mun[2] <- t10cc$Tot[[1]]
result$mun[3] <- " "
result$mun[4] <- " "
result$mun[5] <- t10cc$Tot[[2]]
result$mun[6] <- " "
result$mun[7] <- " "
result$mun[8] <- t10cc$Tot[[3]]
result$mun[9] <- " "
result$mun[10] <-  " "
result$mun[11] <- t10cc$Tot[[4]]
result$mun[12] <- " "
result$mun[13] <- " "
result$mun[14] <- t10cc$Tot[[5]]
result$mun[15] <- " "
result$mun[16] <- " "
result$mun[17] <- t10cc$Tot[[6]]
result$mun[18] <- " "
result$mun[19] <- " "
result$mun[20] <- t10cc$Tot[[6]]
result$mun[21] <- " "



# t10_final <- cbind(t10nc, t10cc)
# 

colnames(result) <- c("", "(1)", "(2)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Anos_nivel_v1.tex")




###10.2 Gráfico ----
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
      labs(x = "Year", y = "Average ENEM Score") +
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
 

p

ggsave(plot = p, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/anos_lvl.png"), device = "png", height = 7, width = 10)
ggsave(plot = p, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/anos_lvl.eps"), device = "eps", height = 7, width = 10)



rm(base_a, base_t, c_rlist, result, rlist, t10cc, t10nc, ano, ano_list, latex_table, names, ef)

# ---------------------------------------------------------------------------- #
## 10.3 Picchetti ----
# ---------------------------------------------------------------------------- #

base_a <- base[priv0 == 1,.(media_nota = mean(media, na.rm = T),
                            obs = .N),
               by = .(mun_prova,ano,dist_hv_border,seg,lat,lon,hv)] 

#Criando as var


base_test1 <- base_a %>% 
  #dummy_cols(select_columns = "ano", remove_first_dummy = FALSE, ignore_na = TRUE) %>% 
  group_by(mun_prova) %>% 
  mutate(
    dup = 1,
    dup2 = sum(dup)

    
    # peso_aux = ifelse(ano == 2018, obs, NA),
    # peso = max(peso_aux, na.rm = T)
  ) %>% 
  ungroup() %>% 
  filter( abs(dist_hv_border) <= bw_main_a,
          #dup2 == max(dup2),
          ano <= 2016) %>% 
  select(-c( dup, dup2)) %>% 
  mutate(
    ano = as.integer(ano),
    d_2013 = ifelse(ano == 2013, 1, 0),
    d_2014 = ifelse(ano == 2014, 1, 0),
    d_2015 = ifelse(ano == 2015, 1, 0),
    d_2016 = ifelse(ano == 2016, 1, 0)
  )


saveRDS(base_test1, file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/Agregados/base_2013_2016.RDS"))



ef <- dummy_cols(base_test1$seg[base_test1$ano == 2016])


test <- rdrobust(
  y = base_test1$media_nota[base_test1$ano == 2016],
  x = base_test1$dist_hv_border[base_test1$ano == 2016],
  c = 0,
  h = bw_main_a,
  b = bw_bias_a,
  cluster = base_test1$seg[base_test1$ano == 2016],
  weights = base_test1$obs[base_test1$ano == 2016],
  vce = "hc0",
  covs = cbind(
    ef,
    base_test1$lat[base_test1$ano == 2016],
    base_test1$lon[base_test1$ano == 2016]
  )
)
rm(test)


#base_test1 <- transform(base_test1, seg_ano = paste(mun_prova, ano))

#base_test1$segf <- as.factor(base_test1$seg)


# 
# base_test2 <- base_test1 %>% 
#   group_by( ano ) %>% 
#   summarise(
#     y_m = mean(media_nota)
#   ) 
# 
# 
# 
# est1_16 <- feols(
#   media_nota ~ 
#     
#     #dist_hv_border : ano_2013 +
#     dist_hv_border : ano_2014 +
#     dist_hv_border : ano_2015 +
#     dist_hv_border : ano_2016 +
#     
#     
#     #dist_hv_border : ano_2013 : hv +
#     dist_hv_border : ano_2014 : hv + 
#     dist_hv_border : ano_2015 : hv +
#     dist_hv_border : ano_2016 : hv |
#     lon + lat + seg + mun_prova,
#   weights = base_test1$obs,
#   vcov = "hetero",
#   
#   data = base_test1,
# )

est1_16 <- feols(
  media_nota ~  
    1 + 
    dist_hv_border : d_2013 +
    dist_hv_border : d_2014 +
    dist_hv_border : d_2015 +
    dist_hv_border : d_2016 +
    
    hv : dist_hv_border : d_2013 +
    hv : dist_hv_border : d_2014 +
    hv : dist_hv_border : d_2015 +
    hv : dist_hv_border : d_2016 +
    lon + lat |mun_prova,
  weights = base_test1$obs,
  vcov = "hetero",
  data = base_test1
)

etable(est1_16)


est1_16 <- feols(
  media_nota ~  
    1 + i(ano,dist_hv_border) + hv:i(ano, dist_hv_border)+
    lon + lat |mun_prova,
  weights = base_test1$obs,
  vcov = "hetero",
  data = base_test1
)


# 
# est2_16 <- feols(
#   media_nota ~  
#     1 + 
#     seg : d_2013 +
#     seg : d_2014 +
#     seg : d_2015 +
#     seg : d_2016 +
#     
#     hv : seg : d_2013 +
#     hv : seg : d_2014 +
#     hv : seg : d_2015 +
#     hv : seg : d_2016|
#     lon + lat  + mun_prova + dist_hv_border,
#   weights = base_test1$obs,
#   vcov = "hetero",
#   data = base_test1
#)

etable(est1_16)



# 
# 
# collinearity(est1_16, verbose = TRUE)
# coefs_with_na <- coef(est1_16, collin = TRUE)
# etable(est1_16, coef = coefs_with_na, drop = NULL)
# 
# 
# names(coef(est1_16))
# 
# 
# wald(est1_16, 
#      c("hv:ano::2014:dist_hv_border = hv:ano::2016:dist_hv_border",
#        "hv:ano::2015:dist_hv_border = hv:ano::2016:dist_hv_border"))



# 
# est2_16 <- feols(
#   media_nota ~ dist_hv_border*i(ano, ref = 2013)*hv | lon + lat + seg,
#   weights = base_test1$peso,
#   vcov = "hetero",
#   data = base_test1,
# )
# 
# 
# 
# etable(est2_16)


base_test2 <- base_a %>% 
  filter(ano >= 2017 & ano != 2019) %>% 
  dummy_cols(select_columns = "ano", remove_first_dummy = FALSE) %>% 
  group_by(mun_prova) %>% 
  mutate(
    dup = 1,
    dup2 = sum(dup),
    peso_aux = ifelse(ano == 2018, obs, NA),
    peso = max(peso_aux, na.rm = T)
  ) %>% 
  ungroup() %>% 
  filter(dup2 == max(dup2),
         abs(dist_hv_border) <= bw_main_a) %>% 
  select(-c(peso_aux, dup, dup2)) %>% 
  mutate(
    ano = as.integer(ano),
    d_2017 = ifelse(ano == 2017, 1, 0),
    d_2018 = ifelse(ano == 2018, 1, 0)
  )


saveRDS(base_test2, file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/Agregados/base_2017_2018.RDS"))
write_dta(base_test2, "Z:/Tuffy/Paper - HV/Bases/No_age_filt/Agregados/base_2017_2018.dta")



ef <- dummy_cols(base_test2$seg[base_test2$ano == 2017])

test <- rdrobust(
  y = base_test2$media_nota[base_test2$ano == 2017],
  x = base_test2$dist_hv_border[base_test2$ano == 2017],
  c = 0,
  h = bw_main_a,
  b = bw_bias_a,
  cluster = base_test2$seg[base_test2$ano == 2017],
  weights = base_test2$obs[base_test2$ano == 2017],
  vce = "hc0",
  covs = cbind(
    ef,
    base_test2$lat[base_test2$ano == 2017],
    base_test2$lon[base_test2$ano == 2017]
  )
)

rm(test)




est1_17 <- feols(
  media_nota ~  
    1 + 
    dist_hv_border : d_2017 +
    dist_hv_border : d_2018 +
    
    hv : dist_hv_border : d_2017 +
    hv : dist_hv_border : d_2018 +
    lon + lat | mun_prova,
  weights = base_test2$obs,
  vcov = "hetero",
  data = base_test2
)



# est2_17 <- feols(
#   media_nota ~ dist_hv_border*i(ano, ref = 2017)*hv | lon + lat + seg,
#   weights = base_test2$peso,
#   vcov = "hetero",
#   data = base_test2,
#   
# )

etable(est1_17, drop = NULL)


est1_17 <- feols(
  media_nota ~  
    1 + i(ano, dist_hv_border) + hv:i(ano,dist_hv_border) +
    lon + lat | mun_prova,
  weights = base_test2$obs,
  vcov = "hetero",
  data = base_test2
)


etable(est1_17)


summary(base_test2)

###WALD ----

## Tabelas -----

etable(est1_17,
       tex = TRUE,
       file = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Picchetti_2017.tex")


wt_1718 <- linearHypothesis(
  est1_17,
  "ano::2017:dist_hv_border:hv = ano::2018:dist_hv_border:hv"
)

wt_1718

chi_sq17 <- round(wt_1718$Chisq[2], 3)
df17      <- wt_1718$Df[2]
p_val17   <- round(wt_1718$`Pr(>Chisq)`[2], 3)


names <- c(
  "2013 - 2016",
  " ",
  "2017 - 2018"
)

result <- data.frame(
  var = names,
  chi = rep(NA, times = length(names)),
  df= rep(NA, times = length(names)),
  p_val = rep(NA, times = length(names))
  
)

result$chi[2] <- " "
result$df[2] <- " "
result$p_val[2] <-  " " 


result$chi[3] <- chi_sq17
result$df[3] <- df17
result$p_val[3] <- p_val17 





etable(est1_16,
       tex = TRUE,
       file = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Picchetti_2014.tex")



wt_16 <- linearHypothesis(
  est1_16,
  c("ano::2013:dist_hv_border:hv = ano::2015:dist_hv_border:hv",
  "ano::2014:dist_hv_border:hv = ano::2015:dist_hv_border:hv")
)

wt_16

chi_sq16 <- round(wt_16$Chisq[2], 3)
df16      <- wt_16$Df[2]
p_val16   <- round(wt_16$`Pr(>Chisq)`[2], 5)

result$chi[1] <- chi_sq16
result$df[1] <- df16
result$p_val[1] <- p_val16 




latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccc",
  linesep = ""
)

writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Picchetti_Wald.tex")




rm(base_test1, base_test2, est1_16, est1_17, est2_16, est2_17, lh, chi2, pval,
   df, wald_note, chi_sq16, df16, p_val16, wt_16, wt_1718, df17, p_val17, chi_sq17,
   result, latex_table)
# ---------------------------------------------------------------------------- #
# 11. Dist de idade ----
# ---------------------------------------------------------------------------- #
# 
# base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2019.RDS")) %>%
#   bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2018.RDS"))) %>%
#   setDT() %>% 
#   filter(conclusao == 2)
# 
# plot <- ggplot(base %>% filter(ano == 2019), aes(x = idade)) +
#   geom_histogram() +
#   xlim(10, 30) +
#   scale_x_continuous(breaks = c(10:30)
#                      ,format = "f") 
# 
# plot
# 
# 
# 
# library(dplyr)
# library(ggplot2)
# library(scales)
# 
# options(scipen = 999)
# 
# 
# fig_gg <- ggplot(base, aes(x = idade, fill = factor(ano))) +
#   geom_histogram(
#     position = "identity", alpha = 0.3, binwidth = 1, color = "black",
#     boundary = 10
#   )+
#   scale_fill_manual(values = c("2018" = "yellow", "2019" = "blue"), name = "Ano") +
#   scale_x_continuous(
#     breaks = 15:25,
#     limits = c(14, 25),
#     minor_breaks = NULL
#   ) +
#   scale_y_continuous(labels = label_number()) +
#   labs(x = "Age", y = "Students") +
#   theme_minimal() +
#   theme(
#     panel.grid.minor = element_blank(),
#     axis.title.x = element_text(size = 35),
#     axis.title.y = element_text(size = 35),
#     axis.text.x = element_text(size = 25, vjust = 0.5, hjust = 1.3),
#     axis.text.y = element_text(size = 25),
#     legend.title = element_text(size = 30),
#     legend.text = element_text(size = 27)
#   ) +
#   coord_cartesian(expand = FALSE) 
# 
# 
# fig_gg
# 
# 
# 
# ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/histograma_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
# ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/histograma_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)
# 
# 
# 
# fig_pv <- ggplot(base %>% filter(priv0 == 1), aes(x = idade, fill = factor(ano))) +
#   geom_histogram(
#     position = "identity", alpha = 0.3, binwidth = 1, color = "black",
#     boundary = 10
#   )+
#   scale_fill_manual(values = c("2018" = "yellow", "2019" = "blue"), name = "Ano") +
#   scale_x_continuous(
#     breaks = 15:25,
#     limits = c(14, 25),
#     minor_breaks = NULL
#   ) +
#   scale_y_continuous(labels = label_number(), limits = c(0, 530000)) +
#   labs(x = "Age", y = "Students") +
#   theme_minimal() +
#   theme(
#     panel.grid.minor = element_blank(),
#     axis.title.x = element_text(size = 35),
#     axis.title.y = element_text(size = 35),
#     axis.text.x = element_text(size = 25, vjust = 0.5, hjust = 1.3),
#     axis.text.y = element_text(size = 25),
#     legend.title = element_text(size = 30),
#     legend.text = element_text(size = 27)
#   ) +
#   coord_cartesian(expand = FALSE) 
# 
# 
# fig_pv
# 
# 
# 
# ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/histograma_priv_v1.png"),plot = fig_pv, device = "png",dpi = 300, height = 6, width = 9)
# ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/histograma_priv_v1.pdf"),plot = fig_pv, device = "pdf",height = 7, width = 10)
# 
# 
# 
# rm(fig_gg, fig_pv, df2)

# ---------------------------------------------------------------------------- #
# 12. Heterogeneity ---- 
# #Lembrando que os testes adicionais (arquivo) são testes de heterogeneidade para a amostra total
# ---------------------------------------------------------------------------- #


base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2019.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2018.RDS"))) %>%
  setDT() %>% 
  filter(conclusao == 2)


## 12.1 Raca ----
### A. TC ----


####12.1.1 Brancos e Amarelos -----
base_ab <- base %>% 
  filter(
    raca %in% c("B", "E")
  )

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


####12.1.2 PPI----

base_ppi <- base %>% 
  filter(
    raca %in% c("C", "D", "F")
  )


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


#####Reg ----

rlist <- list()


ano_list <- c(2018)

for(ano_ref in ano_list) {
  
  
  ano_comp <- ano_ref + 1
  for(df in c("base_ab", "base_ppi")) {
    
    base_a <- get(df)
    
    # #Sem controles
    # rlist[[as.character(paste0(df,"_",ano_comp,"-",ano_ref,"NC|TC"))]] <- rdrobust(
    #   y = base_a$d.media[base_a$ano == ano_comp],
    #   x = base_a$dist_hv_border[base_a$ano == ano_ref],
    #   c = 0,
    #   cluster = base_a$seg[base_a$ano == ano_ref],
    #   weights = base_a$obs[base_a$ano == ano_ref],
    #   vce = "hc0"
    #   # covs = cbind(
    #   #   ef, 
    #   #   base_a$lat[base_a$dif == i], 
    #   #   base_a$lon[base_a$dif == i]
    #   # )
    # )
    
    
    #Com controles
    ef <- dummy_cols(base_a$seg[base_a$ano == ano_ref])
    ef <- ef %>% select(-1,-2)
    
    rlist[[as.character(paste0(df,"_",ano_comp,"-",ano_ref,"C|TC"))]] <- rdrobust(
      y = base_a$d.media[base_a$ano == ano_comp],
      x = base_a$dist_hv_border[base_a$ano == ano_ref],
      c = 0,
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
rm(ef, ano_ref, ano_comp)



###12.2.1 TAB ----
# ------------------------------------ #
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

rm(base_ab, base_ppi, result, rlist, t10cc, df, latex_table, names2)


## 12.2 Sexo ----
### A. TC ----

####12.2.1 FEM ----
base_fem <- base %>% 
  filter(
    fem == 1
  )


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


#### 12.2.2 MASC ----
base_masc <- base %>% 
  filter(
    fem == 0
  )

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

##### reg ----
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
rm(ef, ano_ref, ano_comp)




###12.2.5 TAB ----
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



## 12.3 Mae Educ ----
### A. TC ----
####12.3.1 High ----
base_high <- base %>% 
  filter(
    esc_mae %in% c("D","E","F")
  )


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


#### 12.3.2 Low ----
base_low <- base %>% 
  filter(
    esc_mae %in% c("A", "B", "C")
  )

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

##### reg ----
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
rm(ef, ano_ref, ano_comp)



###12.3.5 TAB ----
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

rm(base_low, result, base_high, rlist, t10cc, df, latex_table, base_a, ano_list, names)


# ---------------------------------------------------------------------------- #
# Mig ----
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
## 7.Migração ------
# ---------------------------------------------------------------------------- #

base <- base %>% 
  mutate(
    mig_dummy = ifelse(
      !is.na(mun_prova) & !is.na(mun_res) & mun_prova != mun_res, 1,
      ifelse(!is.na(mun_prova) & !is.na(mun_res), 0 , NA)
    )
  )



### 7.1 Regressão ----
rlist <- list()

for (j in c(0:1)){
  
  base_y <- base %>% 
    filter( mig_dummy == j)
  
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
  
  
  
  
  rlist[[as.character(paste0("mig =",j,"|C"))]] <- rdrobust(
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
  
  
  rlist[[as.character(paste0("mig =",j,"|BW"))]] <- rdrobust(
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
    N = paste0("[N = ",formatC(x = N, digits = 0, format = "f"),"]")
  ) 

names <- c(
  "No Migration",
  " "," ",
  "Migration",
  " "," "
)

result <- data.frame(
  var = names,
  cc= rep(NA, times = length(names)),
  bw = rep(NA, times = length(names))
  
)


result$cc[1] <- tab$coef[[1]]
result$cc[2] <- tab$se[[1]]
result$cc[3] <- tab$N[[1]]
result$bw[1] <- tab$coef[[2]]
result$bw[2] <- tab$se[[2]]
result$bw[3] <- tab$N[[2]]


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


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/migration_v1.tex")

rm(base_a, names, result, rlist, tab, latex_table)


# ---------------------------------------------------------------------------- #
## 7.2 + var ----
# ---------------------------------------------------------------------------- #

var_list <- c(
  "nonmig1", # MUN PROVA = RESIDENCIA = ESCOLA
  "nonmig2", # Mun PROVA = RESIDENCIA != ESCOLA
  "nonmig3", # MUN PROVA != RESIDENCIA = ESCOLA
  "nonmig4"  # Mun PROVA = ESCOLA != RESIDENCIA
)

rlist  <- list()

for (i in var_list) {
  
  cat("Rodando para:", i, "\n")
  
  
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
  
  
  
  
  
  rlist[[as.character(paste0(i,"|C"))]] <- rdrobust(
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
  
  
  rlist[[as.character(paste0(i,"|BW"))]] <- rdrobust(
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
  "Group 1 (None)",
  " "," ",
  "Group 2 (School)",
  " "," ",
  "Group 3 (Exam)",
  " ", " ",
  "Group 4 (Residency)",
  " ", " "
)

result <- data.frame(
  var = row,
  cc = rep(NA, times = length(names)),
  bw = rep(NA, times = length(names))
)


#NonMig1


result$cc[1] <- tab$coef[[1]]
result$cc[2] <- tab$se[[1]]
result$cc[3] <- tab$N[[1]]

result$bw[1] <- tab$coef[[2]]
result$bw[2] <- tab$se[[2]]
result$bw[3] <- tab$N[[2]]

#NonMig2


result$cc[4] <- tab$coef[[3]]
result$cc[5] <- tab$se[[3]]
result$cc[6] <- tab$N[[3]]

result$bw[4] <- tab$coef[[4]]
result$bw[5] <- tab$se[[4]]
result$bw[6] <- tab$N[[4]]

#NonMig3


result$cc[7] <- tab$coef[[5]]
result$cc[8] <- tab$se[[5]]
result$cc[9] <- tab$N[[5]]

result$bw[7] <- tab$coef[[6]]
result$bw[8] <- tab$se[[6]]
result$bw[9] <- tab$N[[6]]

#NonMig4


result$cc[10] <- tab$coef[[7]]
result$cc[11] <- tab$se[[7]]
result$cc[12] <- tab$N[[7]]

result$bw[10] <- tab$coef[[8]]
result$bw[11] <- tab$se[[8]]
result$bw[12] <- tab$N[[8]]


colnames(result) <- c("", "(1)", "(2)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/grupos_mig_v1.tex")






# ---------------------------------------------------------------------------- #
# # 13. Falsification ----
# # ---------------------------------------------------------------------------- #
# 
# # ---------------------------------------------------------------------------- #

# ## 13.1 Complete -----
# # ---------------------------------------------------------------------------- #
# 
# 
# base_a <- base[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
#                by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] %>% 
#   filter(as.numeric(ano) %in% c(2018,2019)) %>% 
#   arrange(mun_prova,ano) %>%
#   group_by(mun_prova) %>%
#   mutate(
#     dup1 = 1,
#     dup2 = sum(dup1),
#     v1_nota = ifelse(ano == 2018, media, NA),
#     v2_nota = max(v1_nota, na.rm = T),
#     d.media = media - v2_nota 
#   ) %>%
#   ungroup() %>% 
#   filter(dup2 == 2) %>% 
#   select(-c(dup2, dup1, v1_nota, v2_nota))
# 
# 
# ###13.1.1 A TC-----
# # Listas para armazenar resultados
# rlist_cutseg <- list()
# 
# 
# 
# base_temp <- base_a %>% 
#   filter(ano == 2019) %>% 
#   setDT()
# 
# 
# 
# dp_s <- seq(-3*bw_main_a ,3*bw_main_a, bw_main_a)
# dp_s
# # 
# # dp <- base_temp[, .(dp = sd(dist_hv_border))]
# # dp <- dp[1,1] %>% as.numeric()
# # dp_s <- seq(-0.3*dp,0.3*dp,0.1*dp)
# #
# #rm(dp)
# 
# # Loop nos cutoffs
# for (c in 1:length(dp_s)) {
#   
#   # Aviso
#   print(paste0("c: ",c," ano: "))
#   
#   # Vetores da regressão
#   
#   c_val <- dp_s[c]
#   
#   if (c_val < 0) {
#     
#     c_val <- c_val + 1000
#     
#     base_t <- base_a %>%
#       filter(dist_hv_border < 0)
#     
#   } else if (c_val > 0) {
#     
#     c_val <- c_val - 1000
#     
#     base_t <- base_a %>%
#       filter(dist_hv_border > 0)
#     
#   } else if (c_val == 0) {
#     
#     base_t <- base_a
#     
#   }
#   
#   
#   
#   # Dependent variable
#   yv <- base_t %>% 
#     filter(ano == 2019
#            
#     ) %>%
#     select(d.media) %>% 
#     rename(vd = 1)
#   
#   # Running variable
#   xv <- base_t %>% 
#     filter(ano == 2018
#            
#     ) %>%
#     select(dist_hv_border)
#   
#   # Latitude
#   latv <- base_t %>% 
#     filter(ano == 2018
#            
#     ) %>%
#     select(lat)
#   
#   # Longitude
#   lonv <- base_t %>% 
#     filter(ano == 2018
#            
#     ) %>%
#     select(lon)
#   
#   # Clusters
#   clu <- base_t %>% 
#     filter(ano == 2018
#            
#     ) %>%
#     select(seg)
#   
#   peso <- base_t %>% 
#     filter(ano == 2018
#            
#     ) %>%
#     select(obs)
#   
#   # Modelo com efeitos fixos
#   ef <- dummy_cols(clu$seg)
#   ef <- ef %>% select(-1,-2)
#   
#   assign(
#     "resultados_seg",
#     rdrobust(
#       y = yv$vd,
#       x = xv$dist_hv_border,
#       c = c_val,
#       h = bw_main_a, 
#       b = bw_bias_a,
#       cluster = clu$seg,
#       weights = peso$obs,
#       vce = "hc0",
#       covs = cbind(ef,latv,lonv)
#     )
#   )
#   
#   rm(ef)
#   
#   # Armazenando resultados do ano
#   rlist_cutseg[[c]] <- resultados_seg
#   rm(resultados_seg)
#   
#   rm(c,yv, xv, latv, lonv, clu)
#   
# } # fim do loop por índice de ano (ano)
# 
# # save(rlist_cutseg, file = "results/v8/rdd_cutoff.RData")
# # 
# # 
# # # Lista de estimativas
# # load(file = "results/v8/rdd_cutoff.RData")
# 
# # Lista de coeficientes
# tablist_cutseg <- data.frame(
#   coef = do.call(rbind,lapply(rlist_cutseg, FUN = function(x){x$coef[3]})),
#   ll = do.call(rbind,lapply(rlist_cutseg, FUN = function(x){x$ci[3,1]})),
#   ul = do.call(rbind,lapply(rlist_cutseg, FUN = function(x){x$ci[3,2]})),
#   c = dp_s
# ) 
# 
# # Gráfico
# breaks_vec <- tablist_cutseg$c
# labes_2 <- c("-3SD", "-2SD", "-1SD", "0", "1SD", "2SD", "3SD")
# labels_vec <- c("-3*BW", "-2*BW", "-1*BW", "0", "1*BW", "2*BW", "3*BW")
# labels_vec2 <- c("-0.3*SD", "-0.2*SD", "-0.1*SD", "0", "0.1*SD", "0.2*SD", "0.3*SD")
# 
# 
# graph <-  ggplot(data = tablist_cutseg) +
#   geom_hline(yintercept = 0, color = "red") +
#   geom_point(mapping = aes(x = c, y = coef)) +
#   geom_line(mapping = aes(x = c, y = coef, group = 1)) +
#   geom_errorbar(mapping = aes(x = c, ymin = ll, ymax = ul), width = 30000) +
#   theme_bw() +
#   ylab("Coefficient (SD)") +
#   xlab("Distance (Bandwidth)") +
#   theme(axis.title = element_text(size = 25),
#         axis.text = element_text(size = 20),
#         strip.text = element_text(size = 20)) + 
#   scale_x_continuous(breaks = dp_s
#                      , labels = labels_vec
#   )
# 
# 
# graph 
# 
# ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/cutoff_v6.png", plot = graph,device = "png", width = 15, height = 10)
# ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/cutoff_v6.eps", plot = graph,device = "eps")
# rm(graph, tablist_cutseg, rlist_cutseg)
# 
# 
# 
# 
# ###13.1.1 A TC-----
# # Listas para armazenar resultados
# rlist_cutseg <- list()
# 
# 
# 
# base_temp <- base_a %>% 
#   filter(ano == 2019) %>% 
#   setDT()
# 
# inter <- bw_main_a/4
# 
# 
# dp_s <- seq(-3*bw_main_a ,3*bw_main_a, bw_main_a)
# dp_s
# 
# # dp <- base_temp[, .(dp = sd(dist_hv_border))]
# # dp <- dp[1,1] %>% as.numeric()
# # dp_s <- seq(-0.3*dp,0.3*dp,0.1*dp)
# # rm(dp)
# 
# # Loop nos cutoffs
# for (c in 1:length(dp_s)) {
#   
#   # Aviso
#   print(paste0("c: ",c," ano: "))
#   
#   # Vetores da regressão
#   
#   c_val <- dp_s[c]
#   
#   if (c_val < 0) {
#     
#     c_val <- c_val + 1000
#     
#     base_t <- base_a %>%
#       filter(dist_hv_border < 0)
#     
#   } else if (c_val > 0) {
#     
#     c_val <- c_val - 1000
#     
#     base_t <- base_a %>%
#       filter(dist_hv_border > 0)
#     
#   } else if (c_val == 0) {
#     
#     base_t <- base_a
#     
#   }
#   
#   
#   
#   # Dependent variable
#   yv <- base_t %>% 
#     filter(ano == 2019
#            
#     ) %>%
#     select(d.media) %>% 
#     rename(vd = 1)
#   
#   # Running variable
#   xv <- base_t %>% 
#     filter(ano == 2018
#            
#     ) %>%
#     select(dist_hv_border)
#   
#   # Latitude
#   latv <- base_t %>% 
#     filter(ano == 2018
#            
#     ) %>%
#     select(lat)
#   
#   # Longitude
#   lonv <- base_t %>% 
#     filter(ano == 2018
#            
#     ) %>%
#     select(lon)
#   
#   # Clusters
#   clu <- base_t %>% 
#     filter(ano == 2018
#            
#     ) %>%
#     select(seg)
#   
#   peso <- base_t %>% 
#     filter(ano == 2018
#            
#     ) %>%
#     select(obs)
#   
#   # Modelo com efeitos fixos
#   ef <- dummy_cols(clu$seg)
#   ef <- ef %>% select(-1,-2)
#   
#   assign(
#     "resultados_seg",
#     rdrobust(
#       y = yv$vd,
#       x = xv$dist_hv_border,
#       c = c_val,
#       h = bw_main_a, 
#       b = bw_bias_a,
#       cluster = clu$seg,
#       weights = peso$obs,
#       vce = "hc0",
#       covs = cbind(ef,latv,lonv)
#     )
#   )
#   
#   rm(ef)
#   
#   # Armazenando resultados do ano
#   rlist_cutseg[[c]] <- resultados_seg
#   rm(resultados_seg)
#   
#   rm(c,yv, xv, latv, lonv, clu)
#   
# } # fim do loop por índice de ano (ano)
# 
# # save(rlist_cutseg, file = "results/v8/rdd_cutoff.RData")
# # 
# # 
# # # Lista de estimativas
# # load(file = "results/v8/rdd_cutoff.RData")
# 
# # Lista de coeficientes
# tablist_cutseg <- data.frame(
#   coef = do.call(rbind,lapply(rlist_cutseg, FUN = function(x){x$coef[3]})),
#   ll = do.call(rbind,lapply(rlist_cutseg, FUN = function(x){x$ci[3,1]})),
#   ul = do.call(rbind,lapply(rlist_cutseg, FUN = function(x){x$ci[3,2]})),
#   c = dp_s
# ) 
# 
# # Gráfico
# breaks_vec <- tablist_cutseg$c
# labels_vec <- c("-3*BW", "-2*BW", "-1*BW", "0", "1*BW", "2*BW", "3*BW")
# 
# 
# graph <-  ggplot(data = tablist_cutseg) +
#   geom_hline(yintercept = 0, color = "red") +
#   geom_point(mapping = aes(x = c, y = coef)) +
#   geom_line(mapping = aes(x = c, y = coef, group = 1)) +
#   geom_errorbar(mapping = aes(x = c, ymin = ll, ymax = ul), width = 60000) +
#   theme_bw() +
#   ylab("Coefficient (SD)") +
#   xlab("Distance (SD)") +
#   theme(axis.title = element_text(size = 25),
#         axis.text = element_text(size = 20),
#         strip.text = element_text(size = 20)) + 
#   scale_x_continuous(breaks = dp_s
#                      , labels = labels_vec
#   )
# 
# 
# graph 
# 
# ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/cutoff/ba_cutoff_v5.png", plot = graph,device = "png", width = 15, height = 10)
# ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/cutoff/ba_cutoff_v5.eps", plot = graph,device = "eps")
# rm(graph, tablist_cutseg, rlist_cutseg)
# 
# 
# 
# 
# 
# 
# 
# 
# # ---------------------------------------------------------------------------- #
# 14. Seg ---- 
# ---------------------------------------------------------------------------- #
base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2019.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2018.RDS"))) %>%
  setDT() %>% 
  filter(conclusao == 2)

#Agregando a base

base_a <- base[priv0 == 1,.(media = mean(media, na.rm = T),
                            obs = .N),
               by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] 

base_a <- base_a %>%
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_d1 = ifelse(ano == 2018, media, NA),
    v2_d1 = max(v1_d1, na.rm = T),
    d.media = media - v2_d1,
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>% 
  select(-c(v1_d1, v2_d1))



seg_list <- c("N", "M", "S")


result_seg <- list()

###8.5.1 loop ---------

for (i in seg_list){
  
  if (i == "M"){
    base_temp <- base_a %>%
      group_by(mun_prova, seg) %>% 
      filter(seg %in% c(4,3,2)) %>% 
      arrange(mun_prova, ano) %>% 
      mutate(
        lat = as.factor(lat),
        lon = as.factor(lon)
      )
    
  } else if(i == "S"){
    base_temp <- base_a %>%
      group_by(mun_prova, seg) %>% 
      filter(seg %in% c(6,5,7)) %>% 
      arrange(mun_prova, ano) %>% 
      mutate(
        lat = as.factor(lat),
        lon = as.factor(lon)
      )
  } else {
    base_temp <- base_a %>%
      group_by(mun_prova, seg) %>% 
      filter(seg %in% c(1)) %>% 
      arrange(mun_prova, ano) %>% 
      mutate(
        lat = as.factor(lat),
        lon = as.factor(lon)
      )
  }
  
  
  summary(base_temp$lat)
  summary(base_temp$lon)
  
  print(paste0("Rows:", nrow(base_temp), " ", i))
  
  
  
  
  result_seg[[as.character(i)]] <- rdrobust(
    y = base_temp$d.media[base_temp$ano == 2019],
    x = base_temp$dist_hv_border[base_temp$ano == 2018],
    c = 0,
    cluster = base_temp$mun_prova[base_temp$ano == 2018],
    weights = base_temp$obs[base_temp$ano == 2018],
    vce = "hc0",
    covs = cbind(
      base_temp$lat[base_temp$ano == 2018],
      base_temp$lon[base_temp$ano == 2018]
    )
  )
  
  
  
  # yv <- base_temp %>% 
  #   filter(ano == 2019) %>% 
  #   select(d.media) %>% 
  #   filter(!is.na(d.media)) %>% 
  #   rename(vd = 1)
  # 
  # # Running variable
  # xv <- base_temp %>% 
  #   filter(ano == 2018) %>% 
  #   select(dist_hv_border) %>% 
  #   filter(!is.na(2018))
  # 
  # # Latitude
  # latv <- base_temp %>%
  #   filter(ano == 2018) %>% 
  #   select(lat) %>% 
  #   filter(!is.na(lat))
  # 
  # # Longitude
  # lonv <- base_temp %>% 
  #   filter(ano == 2018) %>% 
  #   select(lon) %>% 
  #   filter(!is.na(lon))
  # 
  # # Clusters
  # clu <- base_temp %>%
  #   filter(ano == 2018) %>% 
  #   select(seg) %>% 
  #   filter(!is.na(seg))
  
  # print(nrow(xv))
  # print(cor(latv$lat, lonv$lon))
  # print(var(latv$lat))
  # print(var(lonv$lon))
  
  # 
  # result_seg[[as.character(i)]] <- rdrobust (
  #   y = yv$vd,
  #   x = xv$dist_hv_border,
  #   c = 0,
  #   #cluster = clu$seg,
  #   vce = "hc0",
  #   covs = cbind(latv,lonv)
  # )
  
  
  
  # base_2018 <- base_temp %>% filter(ano == 2018) %>% arrange(mun_prova)
  # base_2019 <- base_temp %>% filter(ano == 2019) %>% arrange(mun_prova)
  # 
  # # Verifica se tem o mesmo número de linhas
  # if (nrow(base_2018) == nrow(base_2019)) {
  #   result_seg[[as.character(i)]] <- rdrobust(
  #     y = base_2019$d.media,
  #     x = base_2018$dist_hv_border,
  #     c = 0,
  #     weights = base_2018$obs,
  #     vce = "hc0"
  #   )
  # } else {
  #   warning(paste("Segmento", i, "com número de linhas diferentes entre 2018 e 2019"))
  # }
  
  
  
  rm(base_temp)
}




result_tab <- data.frame(
  coef = do.call(rbind,lapply(result_seg, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(result_seg, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(result_seg, FUN = function(x){x$pv[3]})),
  n = do.call(rbind, lapply(result_seg, FUN = function(x){x$N_h}))
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
    esp = 1:3,
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
  nor = rep(NA, times = length(names)),
  mid = rep(NA, times = length(names)),
  sul = rep(NA, times = length(names))
)



result$nor[1] <- result_tab$coef[[1]]
result$nor[2] <- result_tab$se[[1]]
result$nor[3] <- result_tab$N[[1]]

result$mid[1] <- result_tab$coef[[2]]
result$mid[2] <- result_tab$se[[2]]
result$mid[3] <- result_tab$N[[2]]

result$sul[1] <- result_tab$coef[[3]]
result$sul[2] <- result_tab$se[[3]]
result$sul[3] <- result_tab$N[[3]]



colnames(result) <- c("", "(1)", "(2)", "(3)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/SEG_Resultados_peso_v1.tex")

rm(result, result_tab, latex_table,i, result_seg, names,
   seg_list)



# ---------------------------------------------------------------------------- #
# 15. Bandwith ----
# ---------------------------------------------------------------------------- #

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

graph


ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/bandwith_v2.png", plot = graph,device = "png", width = 15, height = 10)
ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/bandwith_v2.pdf", plot = graph,device = "pdf", width = 8, height = 6)
rm(graph, tablist_bw, rlist_bw)



# ---------------------------------------------------------------------------- #
# 16. Comp 17-16 ----
# ---------------------------------------------------------------------------- #

base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2017.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2016.RDS"))) %>%
  setDT() %>% 
  filter(conclusao == 2)

summary(base$idade)

base_a <- base[priv0 == 1,.(media = mean(media, na.rm = T), obs = .N),
               by = .(mun_prova,ano,dist_hv_border,seg,lat,lon)] %>% 
  filter(as.numeric(ano) %in% c(2016,2017)) %>% 
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2016, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    d.media = media - v2_nota 
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_nota, v2_nota))

### 1.1.1 Controles ----
ef <- dummy_cols(base_a$seg[base_a$ano == 2016])
ef <- ef %>% select(-1,-2)

list <- list()
## A. Sem Filtro de Idade ----
list[[as.character(paste0(2017,"-",2016,"C|NF"))]] <- rdrobust(
  y = base_a$d.media[base_a$ano == 2017],
  x = base_a$dist_hv_border[base_a$ano == 2016],
  c = 0,
  cluster = base_a$seg[base_a$ano == 2016],
  weights = base_a$obs[base_a$ano == 2016],
  vce = "hc0",
  covs = cbind(
    ef,
    base_a$lat[base_a$ano == 2016],
    base_a$lon[base_a$ano == 2016]
  )
)

### 1.1.3 Pol ----

list[[as.character(paste0(2017,"-",2016,"P|NF"))]] <- rdrobust(
  y = base_a$d.media[base_a$ano == 2017],
  x = base_a$dist_hv_border[base_a$ano == 2016],
  c = 0,
  p = 2, 
  cluster = base_a$seg[base_a$ano == 2016],
  weights = base_a$obs[base_a$ano == 2016],
  vce = "hc0",
  covs = cbind(
    ef,
    base_a$lat[base_a$ano == 2016],
    base_a$lon[base_a$ano == 2016]
  )
)


## 1.2 Tabela  ----
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



names <- c("2017 - 2016",
           " "," ")

result <- data.frame(
  var = names,
  con = rep(NA, times = length(names)),
  po2 = rep(NA, times = length(names))
)

# #TC Segmentos
# result$seg[1] <- t10$coef[[2]]
# result$seg[2] <- t10$se[[2]]
# result$seg[3] <- t10$N[[2]]

#TC Controles
result$con[1] <- t10$coef[[1]]
result$con[2] <- t10$se[[1]]
result$con[3] <- t10$N[[1]]

#TC Pol 2° Grau
result$po2[1] <- t10$coef[[2]]
result$po2[2] <- t10$se[[2]]
result$po2[3] <- t10$N[[2]]





colnames(result) <- c("", "(1)", "(2)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcc",
  linesep = ""
)

# ---------------------------------------------------------------------------- #
#Extração da banda ótima
bw_main_76  <- list[["2017-2016C|NF"]]$bws[1]
bw_bias_76  <- list[["2017-2016C|NF"]]$bws[2]
# ---------------------------------------------------------------------------- #


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/DIFF_1716_Principal_TC_v1.tex")
rm(ef, list, result, t10, latex_table)

# ----------------------------------------------------------------------------- #
##16.2 Matérias----
# ----------------------------------------------------------------------------- #

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
    v1_rd = ifelse(ano == 2016, media_rd, NA),
    v2_rd = max(v1_rd, na.rm = T),
    d.media_rd = media_rd - v2_rd,
    
    #Ciências Naturais
    v1_cn = ifelse(ano == 2016, media_cn, NA),
    v2_cn = max(v1_cn, na.rm = T),
    d.media_cn = media_cn - v2_cn,
    
    #Ciências Humanas
    v1_ch = ifelse(ano == 2016, media_ch, NA),
    v2_ch = max(v1_ch, na.rm = T),
    d.media_ch = media_ch - v2_ch,
    
    #Lingua Portuguesa
    v1_lc = ifelse(ano == 2016, media_lc, NA),
    v2_lc = max(v1_lc, na.rm = T),
    d.media_lc = media_lc - v2_lc,
    
    #Matematica
    v1_mt = ifelse(ano == 2016, media_mt, NA),
    v2_mt = max(v1_mt, na.rm = T),
    d.media_mt = media_mt - v2_mt
    
  ) %>%
  ungroup() %>%
  filter(dup2 == 2) %>%
  select(-c(dup1,dup2,v1_rd,v2_rd,
            v1_cn, v2_cn, v1_ch,v2_ch,v1_lc,v2_lc,v1_mt,v2_mt))



p_list <- list()

d_list <- c("d.media_rd", "d.media_lc", "d.media_ch", "d.media_cn", "d.media_mt")

### A. TC ----
for (i in d_list){
  
  #Com Controles
  
  ef <- dummy_cols(base_a$seg[base_a$ano == 2016])
  ef <- ef %>% select(-1,-2)
  
  
  p_list[[as.character(paste0("wc_",i,"|TC"))]] <-
    rdrobust(
      y = base_a[[i]][base_a$ano == 2017],
      x = base_a$dist_hv_border[base_a$ano == 2016],
      c = 0,
      h = bw_main_76,
      b = bw_bias_76,
      cluster = base_a$seg[base_a$ano == 2016],
      weights = base_a$obs[base_a$ano == 2016],
      vce = "hc0",
      covs = cbind(
        ef, 
        base_a$lat[base_a$ano == 2016], 
        base_a$lon[base_a$ano == 2016]
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

names <- c("2017 - 2016",
           " "," ")

result <- data.frame(
  var = names,
  rd = rep(NA, times = length(names)),
  lc = rep(NA, times = length(names)),
  mt = rep(NA, times = length(names)),
  ch = rep(NA, times = length(names)),
  cn = rep(NA, times = length(names))
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


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Materias_16_17_v1.tex")

rm(notas_tab, p_list, result, d_list, latex_table, base, base_a, bw_bias_76,
   bw_main_76,b,h,c)



# ---------------------------------------------------------------------------- #
# 17. Balance Figs ----
# ---------------------------------------------------------------------------- #
## 17.1 (2018) ----
### 17.1.1 INPE ----
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
# Base de dados

### 17.1.2 Unidos ----
base_2018 <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2018.RDS")) %>%
  setDT() %>%
  mutate(codmun = as.character(mun_prova)) %>%
  merge(inpe, by = "codmun") %>%
  select(-codmun) %>% 
  filter(priv0 == 1)

colnames(base_2018)

base_2018 <- base_2018 %>% 
  rename(vento_d1 = vento_4,
         vento_d2 = vento_11,
         temp_d1 = temp_4,
         temp_d2 = temp_11,
         prec_d1 = prec_4,
         prec_d2 = prec_11,
         umid_d1 = umid_4,
         umid_d2 = umid_11,
         pm25_d1 = pm25_4,
         pm25_d2 = pm25_11,
         o3_d1 = o3_4,
         o3_d2 = o3_11
  )

rm(inpe)

##17.2 (2019) ----

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

### 17.2.2 Unidos ----
base_2019 <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2019.RDS")) %>%
  setDT() %>%
  mutate(codmun = as.character(mun_prova)) %>%
  merge(inpe, by = "codmun") %>%
  select(-codmun)



base_2019 <- base_2019 %>% 
  rename(vento_d1 = vento_3,
         vento_d2 = vento_10,
         temp_d1 = temp_3,
         temp_d2 = temp_10,
         prec_d1 = prec_3,
         prec_d2 = prec_10,
         umid_d1 = umid_3,
         umid_d2 = umid_10,
         pm25_d1 = pm25_3,
         pm25_d2 = pm25_10,
         o3_d1 = o3_3,
         o3_d2 = o3_10
  )


rm(inpe)



## 17.3 ALL ----
base_inpe <- bind_rows(base_2018,base_2019)

rm(base_2018,base_2019)

base_inpe <- base_inpe %>% 
  select(
    -c(o3_d1, o3_d2, pm25_d1, pm25_d2, vento_d1, vento_d2)
  )

base_inpe <- base_inpe %>% 
  mutate(
    
    escm = case_when(
      esc_mae %in% c("D","E","F") ~ 0,
      esc_mae %in% c("A","B","C") ~ 1,
      .default = NA),
    
    mae_trab_man = case_when(
      emp_mae %in% c("A","B","C") ~ 1,
      emp_mae %in% c("D","E","F") ~ 0,
      .default = NA
    ),
    
    pai_trab_man = case_when(
      emp_pai %in% c("A","B","C") ~ 1,
      emp_pai %in% c("D","E","F") ~ 0,
      .default = NA
    )
  )

##17.4 REG ----

var_list <- c("id18", "fem", "ppi", "escp", "escm", "dom5",
              "renda1", "pibpc", "pai_trab_man", "mae_trab_man",
              "temp_d1", "temp_d2", "umid_d1", "umid_d2")



rlist <- list()

for (year in c(2018:2019)){
  
  base_y <- base_inpe %>% 
    filter(ano == year)
  
  
  
  
  for (i in var_list){
    
    print(i)
    
    base_ag <- base_y %>% filter(!is.na(.data[[i]]))
    
    
    ef <- dummy_cols(base_ag$seg)
    ef <- ef %>% select(-1,-2)
    
    
    rlist[[as.character(paste0(year,"_",i))]] <-
      rdrobust(
        y = base_ag[[i]],
        x = base_ag$dist_hv_border,
        c = 0,
        h = bw_main_a,
        b = bw_bias_a,
        cluster = base_ag$seg,
        vce = "hc0",
        covs = cbind(
          ef, 
          base_ag$lat, 
          base_ag$lon
        )
      )
    
    
    
    
    
    
    
  }
  
}
rm(ef,i, base_y, base_ag)




##17.5 TABELA --------------------------------------------------------------


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


##### 2018 ----
covs <- data.frame(
  ano = rep(2017:2017, length(vnames)),
  var = vnames,
  ep = rep(NA, times = length(vnames))
) %>% 
  mutate(
    ano = "2018"
  )


covs$ep[covs$var == vnames[1]] <- rlist[[1]]$z[3] #id18
covs$ep[covs$var == vnames[2]] <- rlist[[2]]$z[3] #fem
covs$ep[covs$var == vnames[3]] <- rlist[[3]]$z[3] #ppi
covs$ep[covs$var == vnames[4]] <- rlist[[4]]$z[3] #escp
covs$ep[covs$var == vnames[5]] <- rlist[[5]]$z[3] #escm
covs$ep[covs$var == vnames[6]] <- rlist[[6]]$z[3] #dom5
covs$ep[covs$var == vnames[7]] <- rlist[[7]]$z[3] #renda1
covs$ep[covs$var == vnames[8]] <- rlist[[8]]$z[3] #pibpc
covs$ep[covs$var == vnames[9]] <- rlist[[9]]$z[3] #trab manual Pai
covs$ep[covs$var == vnames[10]] <- rlist[[10]]$z[3] #trab manual Mae
covs$ep[covs$var == vnames[11]] <- rlist[[11]]$z[3] #temp1d
covs$ep[covs$var == vnames[12]] <- rlist[[12]]$z[3] #temp2d
covs$ep[covs$var == vnames[13]] <- rlist[[13]]$z[3] #um1d
covs$ep[covs$var == vnames[14]] <- rlist[[14]]$z[3] #um2d



plot_covs <- ggplot(data = covs) +
  theme_bw() + 
  labs(x = 't-statistic', y = NULL) + 
  scale_x_continuous(breaks = c(-1.96, 0, +1.96), 
                     labels = c(-1.96, '', 1.96), 
                     limits = c(-4.999, 4.999)) + 
  geom_vline(xintercept = c(-1.96, 1.96), color = 'red', linetype = 'dashed', linewidth = 1) + 
  geom_point(aes(x = ep, y = var), color = '#1A2D99', size = 2.25) + 
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

plot_covs

ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/new_covs_test_",2018,"_.png"), device = "png", height = 10, width = 7)
ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/new_covs_test_",2018,"_.eps"), device = "eps", height = 10, width = 7)

rm( covs, plot_covs)


#### 2019 ----

covs <- data.frame(
  ano = rep(2017:2017, length(vnames)),
  var = vnames,
  ep = rep(NA, times = length(vnames))
) %>% 
  mutate(
    ano = "2019"
  )


covs$ep[covs$var == vnames[1]] <- rlist[[15]]$z[3] #id18
covs$ep[covs$var == vnames[2]] <- rlist[[16]]$z[3] #fem
covs$ep[covs$var == vnames[3]] <- rlist[[17]]$z[3] #ppi
covs$ep[covs$var == vnames[4]] <- rlist[[18]]$z[3] #escp
covs$ep[covs$var == vnames[5]] <- rlist[[19]]$z[3] #escm
covs$ep[covs$var == vnames[6]] <- rlist[[20]]$z[3] #dom5
covs$ep[covs$var == vnames[7]] <- rlist[[21]]$z[3] #renda1
covs$ep[covs$var == vnames[8]] <- rlist[[22]]$z[3] #pibpc
covs$ep[covs$var == vnames[9]] <- rlist[[23]]$z[3] #trab manual Pai
covs$ep[covs$var == vnames[10]] <- rlist[[24]]$z[3] #trab manual Mae
covs$ep[covs$var == vnames[11]] <- rlist[[25]]$z[3] #temp1d
covs$ep[covs$var == vnames[12]] <- rlist[[26]]$z[3] #temp2d
covs$ep[covs$var == vnames[13]] <- rlist[[27]]$z[3] #um1d
covs$ep[covs$var == vnames[14]] <- rlist[[28]]$z[3] #um2d



plot_covs <- ggplot(data = covs) +
  theme_bw() + 
  labs(x = 't-statistic', y = NULL) + 
  scale_x_continuous(breaks = c(-1.96, 0, +1.96), 
                     labels = c(-1.96, '', 1.96), 
                     limits = c(-4.999, 4.999)) + 
  geom_vline(xintercept = c(-1.96, 1.96), color = 'red', linetype = 'dashed', linewidth = 1) + 
  geom_point(aes(x = ep, y = var), color = '#1A2D99', size = 2.25) + 
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))


plot_covs


ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/new_covs_test_",2019,"_.png"), device = "png", height = 10, width = 7)
ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/new_covs_test_",2019,"_.eps"), device = "eps", height = 10, width = 7)

rm( covs, plot_covs)




#### Juntos -----



covs <- data.frame(
  ano = rep(2017:2017, length(vnames)),
  var = vnames,
  ep18 = rep(NA, times = length(vnames)),
  ep19 = rep(NA, times = length(vnames))
) %>% 
  mutate(
    ano = "Both"
  )

covs$ep18[covs$var == vnames[1]] <- rlist[[1]]$z[3] #id18
covs$ep18[covs$var == vnames[2]] <- rlist[[2]]$z[3] #fem
covs$ep18[covs$var == vnames[3]] <- rlist[[3]]$z[3] #ppi
covs$ep18[covs$var == vnames[4]] <- rlist[[4]]$z[3] #escp
covs$ep18[covs$var == vnames[5]] <- rlist[[5]]$z[3] #escm
covs$ep18[covs$var == vnames[6]] <- rlist[[6]]$z[3] #dom5
covs$ep18[covs$var == vnames[7]] <- rlist[[7]]$z[3] #renda1
covs$ep18[covs$var == vnames[8]] <- rlist[[8]]$z[3] #pibpc
covs$ep18[covs$var == vnames[9]] <- rlist[[9]]$z[3] #trab manual Pai
covs$ep18[covs$var == vnames[10]] <- rlist[[10]]$z[3] #trab manual Mae
covs$ep18[covs$var == vnames[11]] <- rlist[[11]]$z[3] #temp1d
covs$ep18[covs$var == vnames[12]] <- rlist[[12]]$z[3] #temp2d
covs$ep18[covs$var == vnames[13]] <- rlist[[13]]$z[3] #um1d
covs$ep18[covs$var == vnames[14]] <- rlist[[14]]$z[3] #um2d




covs$ep19[covs$var == vnames[1]] <- rlist[[15]]$z[3] #id18
covs$ep19[covs$var == vnames[2]] <- rlist[[16]]$z[3] #fem
covs$ep19[covs$var == vnames[3]] <- rlist[[17]]$z[3] #ppi
covs$ep19[covs$var == vnames[4]] <- rlist[[18]]$z[3] #escp
covs$ep19[covs$var == vnames[5]] <- rlist[[19]]$z[3] #escm
covs$ep19[covs$var == vnames[6]] <- rlist[[20]]$z[3] #dom5
covs$ep19[covs$var == vnames[7]] <- rlist[[21]]$z[3] #renda1
covs$ep19[covs$var == vnames[8]] <- rlist[[22]]$z[3] #pibpc
covs$ep19[covs$var == vnames[9]] <- rlist[[23]]$z[3] #trab manual Pai
covs$ep19[covs$var == vnames[10]] <- rlist[[24]]$z[3] #trab manual Mae
covs$ep19[covs$var == vnames[11]] <- rlist[[25]]$z[3] #temp1d
covs$ep19[covs$var == vnames[12]] <- rlist[[26]]$z[3] #temp2d
covs$ep19[covs$var == vnames[13]] <- rlist[[27]]$z[3] #um1d
covs$ep19[covs$var == vnames[14]] <- rlist[[28]]$z[3] #um2d



plot_covs <- ggplot(data = covs) +
  theme_bw() + 
  labs(x = 't-statistic', y = NULL) + 
  scale_x_continuous(breaks = c(-1.96, 0, +1.96), 
                     labels = c(-1.96, '', 1.96), 
                     limits = c(-5.1, 5.1)) + 
  geom_vline(xintercept = c(-1.96, 1.96), color = 'red', linetype = 'dashed', linewidth = 1) + 
  geom_point(aes(x = ep18, y = var), color = '#1A2D99', size = 2.25) + 
  geom_point(aes(x = ep19, y = var), color = '#fc8d62', size = 2.25) + 
  
  scale_color_manual(
    values = c("2018" = '#1A2D99', "2019" = '#fc8d62')
  ) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )


plot_covs

library(tidyverse)

covs_long <- covs %>%
  pivot_longer(cols = starts_with("ep"),
               names_to = "ep",
               values_to = "tstat")

# Step 2: Rename the ep variable for better legend labels
covs_long <- covs_long %>%
  mutate(ep = recode_factor(ep,
                            "ep18" = "2018",
                            "ep19" = "2019"))

# Step 3: Plot
plot_covs <- ggplot(data = covs_long) +
  theme_bw() + 
  labs(x = 't-statistic', y = NULL, color = "Year") + 
  scale_x_continuous(
    breaks = c(-1.96, 0, +1.96), 
    labels = c("-1.96", "", "1.96"), 
    limits = c(-5.1, 5.1)
  ) + 
  geom_vline(xintercept = c(-1.96, 1.96), 
             color = 'red', 
             linetype = 'dashed', 
             linewidth = 1) + 
  geom_point(aes(x = tstat, y = var, color = ep), size = 2.25) +
  scale_color_manual(
    values = c("2018" = "#1A2D99", "2019" = "#D55E00")
  ) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 19),
    
    legend.position = c(1.01, 0),  
    legend.justification = c(0, 0),  
    
    plot.margin = margin(c(10, 80, 10, 10), "pt")  
  )

plot_covs



ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/both_covs_test.png"), device = "png", height = 10, width = 8)
ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/both_covs_test.eps"), device = "eps", height = 10, width = 8)

rm( covs, plot_covs)

# ---------------------------------------------------------------------------- #
# 17.6 Match ----
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
## BW Map ----
# ---------------------------------------------------------------------------- #
line <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/line.RDS")
base <- base_inpe %>%
  filter(ano == 2019) %>% 
  select(mun_prova) %>%
  unique() %>%
  mutate(amostra = 1)




mun_hv <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/mun_hv.RDS") %>%
  left_join(base, by = c("co_municipio" = "mun_prova")) %>%
  mutate(
    dist_hv = ifelse(amostra == 1, dist_hv_border / 1000, NA),
    in_band = ifelse(abs(dist_hv_border) <= bw_main_a, 1, 0),
    in_enem = ifelse(co_municipio %in% mun_enem$mun_prova, 1, 0),
    
    final = case_when(
      in_band == 1 & in_enem == 1 ~ 2,
      in_band == 1 & in_enem == 0 ~ 1,
      TRUE ~ 0
    ),
    final = as.factor(final)
  ) 


mun_hv <- mun_hv %>%
  mutate(
    final = factor(final,
                   levels = c("0", "1", "2"),
                   labels = c("Out", "In BW", "In BW & ENEM")
    )
  )


# Mapa de clusters
map <- ggplot(mun_hv) +
  geom_sf(aes(fill = factor(final)) ) +
  geom_sf(data = line, color = "blue") +
  scale_fill_manual(
    name = "Groups",
    values = c(
      "Out"           = "#66c2a5",     # soft green
      "In BW"         = "#E0D268",     # muted yellow
      "In BW & ENEM"  = "#CB4C4E"      # muted red
    ),
    drop = FALSE
  ) +
  guides(fill = guide_legend(
    direction   = "vertical",
    keywidth    = unit(0.8, "cm"),
    keyheight   = unit(0.8, "cm"),
    override.aes = list(alpha = 1)
  )) +
  theme_bw() +
  theme(
    legend.position    = c(0.005, 0.005),        # top-right corner
    legend.justification = c("left", "bottom"),  # align legend box corner
    legend.background  = element_rect(fill = "white", color = "black", size = 0.2),
    legend.key         = element_rect(color = NA),
    axis.text          = element_text(size = 16),
    axis.title         = element_text(size = 18, face = "bold"),
    legend.text        = element_text(size = 16),
    legend.title       = element_text(size= 18)
  ) 





map

ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/mapas/map_band.png"),plot = map,device = "png", dpi = 300)


### Balance -----
temp <- base_inpe[priv0 == 1,.(media = mean(media, na.rm = T),
                            obs = .N),
               by = .(mun_prova,ano,dist_hv_border,seg,lat,lon, hv)] %>% 
  filter(as.numeric(ano) %in% c(2018,2019)) %>% 
  arrange(mun_prova,ano) %>%
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2018, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    d.media = media - v2_nota,
    
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_nota, v2_nota)) %>% 
  mutate( hv = ifelse(dist_hv_border < 0 , 1, 0))

summary(temp$media)

treated <- temp %>% filter(hv == 1, ano == 2018,
                           abs(dist_hv_border) <= bw_main_a )
control <- temp %>% filter(hv == 0, ano == 2018,
                           abs(dist_hv_border) <= bw_main_a )

library(geosphere)



treated_coords <- treated %>% select(lon, lat)
control_coords <- control %>% select(lon, lat)

# Distance matrix: rows = treated, cols = control
dist_matrix <- distm(treated_coords, control_coords, fun = distHaversine)

# For each treated unit, find the control unit with the smallest distance
nearest_index <- apply(dist_matrix, 1, which.min)

# Add row_treated (if not done before)
treated$row_treated <- 1:nrow(treated)

# After selecting matched control units:
paired_control <- control[nearest_index, ]

# Ensure matched pairs stay aligned
treated_matched <- treated
control_matched <- paired_control

# Treatment indicator
treated_matched$matched_treat <- 1
control_matched$matched_treat <- 0

# Pair ID
treated_matched$pair_id <- treated_matched$row_treated
control_matched$pair_id <- treated_matched$row_treated  # same pair_id

# Bind matched dataset
matched_df <- bind_rows(treated_matched, control_matched)

matched_df_id_pair <- matched_df %>% select(mun_prova, pair_id)

#
rm(treated_coords, control_coords, dist_matrix, nearest_index, treated, control,
   paired_control, treated_matched,control_matched)





### T Test ----
base_inpe_t <- base_inpe %>%
  filter(abs(dist_hv_border) <= bw_main_a) %>% 
  left_join(matched_df_id_pair %>% select(mun_prova, pair_id), by = "mun_prova")


summary(base_inpe_t %>% select(id18, hv, pair_id))

library(broom)

test_covariates <- function(data, var_list, treat_var = "hv", fe_var = "pair_id") {
  results <- lapply(var_list, function(var) {
    formula_str <- paste0(var, " ~ ", treat_var, " | ", fe_var)
    model <- feols(as.formula(formula_str), data = data)
    
    tidy_result <- broom::tidy(model)
    treat_row <- tidy_result[tidy_result$term == treat_var, ]
    
    # Compute z-stat manually
    z_stat <- treat_row$estimate / treat_row$std.error
    
    data.frame(
      variable = var,
      estimate = treat_row$estimate,
      std.error = treat_row$std.error,
      statistic = z_stat,
      p.value = treat_row$p.value
    )
  })
  
  do.call(rbind, results)
}

results_list <- list()

for (year in c(2019, 2018)) {
  
  temp <- base_inpe_t %>% 
    filter(ano == year)
  
  results_covs <- test_covariates(data = temp, var_list = var_list)
  
  results_list[[as.character(year)]] <- results_covs
  

    print(paste("Resultados para o ano", year))
  print(results_covs)
}


## Tabela ----
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


##### 2018 ----
covs <- data.frame(
  ano = rep(2017:2017, length(vnames)),
  var = vnames,
  ep = rep(NA, times = length(vnames))
) %>% 
  mutate(
    ano = "2018"
  )


covs$ep[covs$var == vnames[1]] <- results_list[["2018"]]$statistic[1] #id18
covs$ep[covs$var == vnames[2]] <- results_list[["2018"]]$statistic[2] #fem
covs$ep[covs$var == vnames[3]] <- results_list[["2018"]]$statistic[3] #ppi
covs$ep[covs$var == vnames[4]] <- results_list[["2018"]]$statistic[4] #escp
covs$ep[covs$var == vnames[5]] <- results_list[["2018"]]$statistic[5] #escm
covs$ep[covs$var == vnames[6]] <- results_list[["2018"]]$statistic[6] #dom5
covs$ep[covs$var == vnames[7]] <- results_list[["2018"]]$statistic[7] #renda1
covs$ep[covs$var == vnames[8]] <- results_list[["2018"]]$statistic[8] #pibpc
covs$ep[covs$var == vnames[9]] <- results_list[["2018"]]$statistic[9] #trab manual Pai
covs$ep[covs$var == vnames[10]] <- results_list[["2018"]]$statistic[10] #trab manual Mae
covs$ep[covs$var == vnames[11]] <- results_list[["2018"]]$statistic[11] #temp1d
covs$ep[covs$var == vnames[12]] <- results_list[["2018"]]$statistic[12] #temp2d
covs$ep[covs$var == vnames[13]] <- results_list[["2018"]]$statistic[13] #um1d
covs$ep[covs$var == vnames[14]] <- results_list[["2018"]]$statistic[14] #um2d



plot_covs <- ggplot(data = covs) +
  theme_bw() + 
  labs(x = 't-statistic', y = NULL) + 
  scale_x_continuous(breaks = c(-1.96, 0, +1.96), 
                     labels = c(-1.96, '', 1.96), 
                     limits = c(-12.43, 12.43)) + 
  geom_vline(xintercept = c(-1.96, 1.96), color = 'red', linetype = 'dashed', linewidth = 1) + 
  geom_point(aes(x = ep, y = var), color = '#1A2D99', size = 2.25) + 
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

plot_covs

ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/matched_covs_test_",2018,"_.png"), device = "png", height = 10, width = 7)
ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/matched_covs_test_",2018,"_.eps"), device = "eps", height = 10, width = 7)

rm( covs, plot_covs)

##### 2019 ----
covs <- data.frame(
  ano = rep(2017:2017, length(vnames)),
  var = vnames,
  ep = rep(NA, times = length(vnames))
) %>% 
  mutate(
    ano = "2019"
  )


covs$ep[covs$var == vnames[1]] <- results_list[["2019"]]$statistic[1] #id18
covs$ep[covs$var == vnames[2]] <- results_list[["2019"]]$statistic[2] #fem
covs$ep[covs$var == vnames[3]] <- results_list[["2019"]]$statistic[3] #ppi
covs$ep[covs$var == vnames[4]] <- results_list[["2019"]]$statistic[4] #escp
covs$ep[covs$var == vnames[5]] <- results_list[["2019"]]$statistic[5] #escm
covs$ep[covs$var == vnames[6]] <- results_list[["2019"]]$statistic[6] #dom5
covs$ep[covs$var == vnames[7]] <- results_list[["2019"]]$statistic[7] #renda1
covs$ep[covs$var == vnames[8]] <- results_list[["2019"]]$statistic[8] #pibpc
covs$ep[covs$var == vnames[9]] <- results_list[["2019"]]$statistic[9] #trab manual Pai
covs$ep[covs$var == vnames[10]] <- results_list[["2019"]]$statistic[10] #trab manual Mae
covs$ep[covs$var == vnames[11]] <- results_list[["2019"]]$statistic[11] #temp1d
covs$ep[covs$var == vnames[12]] <- results_list[["2019"]]$statistic[12] #temp2d
covs$ep[covs$var == vnames[13]] <- results_list[["2019"]]$statistic[13] #um1d
covs$ep[covs$var == vnames[14]] <- results_list[["2019"]]$statistic[14] #um2d



plot_covs <- ggplot(data = covs) +
  theme_bw() + 
  labs(x = 't-statistic', y = NULL) + 
  scale_x_continuous(breaks = c(-1.96, 0, +1.96), 
                     labels = c(-1.96, '', 1.96), 
                     limits = c(-7.2, 7.2)) + 
  geom_vline(xintercept = c(-1.96, 1.96), color = 'red', linetype = 'dashed', linewidth = 1) + 
  geom_point(aes(x = ep, y = var), color = '#1A2D99', size = 2.25) + 
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

plot_covs

ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/matched_covs_test_",2019,"_.png"), device = "png", height = 10, width = 7)
ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/matched_covs_test_",2019,"_.eps"), device = "eps", height = 10, width = 7)

rm( covs, plot_covs)



######## JUNTOS ----

covs <- data.frame(
  ano = rep(2017:2017, length(vnames)),
  var = vnames,
  ep18 = rep(NA, times = length(vnames)),
  ep19 = rep(NA, times = length(vnames))
) %>% 
  mutate(
    ano = "Both"
  )

covs$ep18[covs$var == vnames[1]] <- results_list[["2018"]]$statistic[1] #id18
covs$ep18[covs$var == vnames[2]] <- results_list[["2018"]]$statistic[2] #fem
covs$ep18[covs$var == vnames[3]] <- results_list[["2018"]]$statistic[3] #ppi
covs$ep18[covs$var == vnames[4]] <- results_list[["2018"]]$statistic[4] #escp
covs$ep18[covs$var == vnames[5]] <- results_list[["2018"]]$statistic[5] #escm
covs$ep18[covs$var == vnames[6]] <- results_list[["2018"]]$statistic[6] #dom5
covs$ep18[covs$var == vnames[7]] <- results_list[["2018"]]$statistic[7] #renda1
covs$ep18[covs$var == vnames[8]] <- results_list[["2018"]]$statistic[8] #pibpc
covs$ep18[covs$var == vnames[9]] <- results_list[["2018"]]$statistic[9] #trab manual Pai
covs$ep18[covs$var == vnames[10]] <- results_list[["2018"]]$statistic[10] #trab manual Mae
covs$ep18[covs$var == vnames[11]] <- results_list[["2018"]]$statistic[11] #temp1d
covs$ep18[covs$var == vnames[12]] <- results_list[["2018"]]$statistic[12] #temp2d
covs$ep18[covs$var == vnames[13]] <- results_list[["2018"]]$statistic[13] #um1d
covs$ep18[covs$var == vnames[14]] <- results_list[["2018"]]$statistic[14] #um2d




covs$ep19[covs$var == vnames[1]] <- results_list[["2019"]]$statistic[1] #id19
covs$ep19[covs$var == vnames[2]] <- results_list[["2019"]]$statistic[2] #fem
covs$ep19[covs$var == vnames[3]] <- results_list[["2019"]]$statistic[3] #ppi
covs$ep19[covs$var == vnames[4]] <- results_list[["2019"]]$statistic[4] #escp
covs$ep19[covs$var == vnames[5]] <- results_list[["2019"]]$statistic[5] #escm
covs$ep19[covs$var == vnames[6]] <- results_list[["2019"]]$statistic[6] #dom5
covs$ep19[covs$var == vnames[7]] <- results_list[["2019"]]$statistic[7] #renda1
covs$ep19[covs$var == vnames[8]] <- results_list[["2019"]]$statistic[8] #pibpc
covs$ep19[covs$var == vnames[9]] <- results_list[["2019"]]$statistic[9] #trab manual Pai
covs$ep19[covs$var == vnames[10]] <- results_list[["2019"]]$statistic[10] #trab manual Mae
covs$ep19[covs$var == vnames[11]] <- results_list[["2019"]]$statistic[11] #temp1d
covs$ep19[covs$var == vnames[12]] <- results_list[["2019"]]$statistic[12] #temp2d
covs$ep19[covs$var == vnames[13]] <- results_list[["2019"]]$statistic[13] #um1d
covs$ep19[covs$var == vnames[14]] <- results_list[["2019"]]$statistic[14] #um2d


plot_covs <- ggplot(data = covs) +
  theme_bw() + 
  labs(x = 't-statistic', y = NULL) + 
  scale_x_continuous(breaks = c(-1.96, 0, +1.96), 
                     labels = c(-1.96, '', 1.96), 
                     limits = c(-5.1, 5.1)) + 
  geom_vline(xintercept = c(-1.96, 1.96), color = 'red', linetype = 'dashed', linewidth = 1) + 
  geom_point(aes(x = ep18, y = var), color = '#1A2D99', size = 2.25) + 
  geom_point(aes(x = ep19, y = var), color = '#fc8d62', size = 2.25) + 
  
  scale_color_manual(
    values = c("2018" = '#1A2D99', "2019" = '#fc8d62')
  ) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 19)
  )


plot_covs

library(tidyverse)

covs_long <- covs %>%
  pivot_longer(cols = starts_with("ep"),
               names_to = "ep",
               values_to = "tstat")

# Step 2: Rename the ep variable for better legend labels
covs_long <- covs_long %>%
  mutate(ep = recode_factor(ep,
                            "ep18" = "2018",
                            "ep19" = "2019"))

# Step 3: Plot
plot_covs <- ggplot(data = covs_long) +
  theme_bw() + 
  labs(x = 't-statistic', y = NULL, color = "Year") + 
  scale_x_continuous(
    breaks = c(-1.96, 0, +1.96), 
    labels = c("-1.96", "", "1.96"), 
    limits = c(-6.35, 12.35)
  ) + 
  geom_vline(xintercept = c(-1.96, 1.96), 
             color = 'red', 
             linetype = 'dashed', 
             linewidth = 1) + 
  geom_point(aes(x = tstat, y = var, color = ep), size = 2.25) +
  scale_color_manual(
    values = c("2018" = "#1A2D99", "2019" = "#D55E00")
  ) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 19),
    
    legend.position = c(1.01, 0),  
    legend.justification = c(0, 0),  
    
    plot.margin = margin(c(10, 80, 10, 10), "pt")  
  )

plot_covs



ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/both_matched_covs_test.png"), device = "png", height = 10, width = 8)
ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/both_matched_covs_test.eps"), device = "eps", height = 10, width = 8)

rm( covs, plot_covs)


# ---------------------------------------------------------------------------- #
# 17.7 In Border ----
# ---------------------------------------------------------------------------- #

mun_border <- readRDS( file = paste0("Z:/Tuffy/Paper - HV/Bases/mun_touching.RDS"))





### Balance -----
temp <- base_inpe[priv0 == 1,.(media = mean(media, na.rm = T),
                               obs = .N),
                  by = .(mun_prova,ano,dist_hv_border,seg,lat,lon, hv)] %>% 
  filter(as.numeric(ano) %in% c(2018,2019)) %>% 
  arrange(mun_prova,ano) %>%
  filter(mun_prova %in% mun_border$co_municipio) %>% 
  group_by(mun_prova) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_nota = ifelse(ano == 2018, media, NA),
    v2_nota = max(v1_nota, na.rm = T),
    d.media = media - v2_nota,
    
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_nota, v2_nota)) %>% 
  mutate( hv = ifelse(dist_hv_border < 0 , 1, 0)) 



treated <- temp %>% filter(hv == 1, ano == 2018,
                           abs(dist_hv_border) <= bw_main_a)
control <- temp %>% filter(hv == 0, ano == 2018,
                           abs(dist_hv_border) <= bw_main_a)

library(geosphere)



treated_coords <- treated %>% select(lon, lat)
control_coords <- control %>% select(lon, lat)

# Distance matrix: rows = treated, cols = control
dist_matrix <- distm(treated_coords, control_coords, fun = distHaversine)

# For each treated unit, find the control unit with the smallest distance
nearest_index <- apply(dist_matrix, 1, which.min)

# Add row_treated (if not done before)
treated$row_treated <- 1:nrow(treated)

# After selecting matched control units:
paired_control <- control[nearest_index, ]

# Ensure matched pairs stay aligned
treated_matched <- treated
control_matched <- paired_control

# Treatment indicator
treated_matched$matched_treat <- 1
control_matched$matched_treat <- 0

# Pair ID
treated_matched$pair_id <- treated_matched$row_treated
control_matched$pair_id <- treated_matched$row_treated  # same pair_id

# Bind matched dataset
matched_df <- bind_rows(treated_matched, control_matched)

matched_df_id_pair <- matched_df %>% select(mun_prova, pair_id)

#
rm(treated_coords, control_coords, dist_matrix, nearest_index, treated, control,
   paired_control, treated_matched,control_matched)





### T Test ----
base_inpe_t <- base_inpe %>%
  filter(mun_prova %in% matched_df_id_pair$mun_prova) %>% 
  left_join(matched_df_id_pair %>% select(mun_prova, pair_id), by = "mun_prova")


summary(base_inpe_t %>% select(id18, hv, pair_id))

library(broom)

test_covariates <- function(data, var_list, treat_var = "hv", fe_var = "pair_id") {
  results <- lapply(var_list, function(var) {
    formula_str <- paste0(var, " ~ ", treat_var, " | ", fe_var)
    model <- feols(as.formula(formula_str), data = data)
    
    tidy_result <- broom::tidy(model)
    treat_row <- tidy_result[tidy_result$term == treat_var, ]
    
    # Compute z-stat manually
    z_stat <- treat_row$estimate / treat_row$std.error
    
    data.frame(
      variable = var,
      estimate = treat_row$estimate,
      std.error = treat_row$std.error,
      statistic = z_stat,
      p.value = treat_row$p.value
    )
  })
  
  do.call(rbind, results)
}

results_list <- list()

for (year in c(2019, 2018)) {
  
  temp <- base_inpe_t %>% 
    filter(ano == year)
  
  results_covs <- test_covariates(data = temp, var_list = var_list)
  
  results_list[[as.character(year)]] <- results_covs
  
  
  print(paste("Resultados para o ano", year))
  print(results_covs)
}


## Tabela ----
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


##### 2018 ----
covs <- data.frame(
  ano = rep(2017:2017, length(vnames)),
  var = vnames,
  ep = rep(NA, times = length(vnames))
) %>% 
  mutate(
    ano = "2018"
  )


covs$ep[covs$var == vnames[1]] <- results_list[["2018"]]$statistic[1] #id18
covs$ep[covs$var == vnames[2]] <- results_list[["2018"]]$statistic[2] #fem
covs$ep[covs$var == vnames[3]] <- results_list[["2018"]]$statistic[3] #ppi
covs$ep[covs$var == vnames[4]] <- results_list[["2018"]]$statistic[4] #escp
covs$ep[covs$var == vnames[5]] <- results_list[["2018"]]$statistic[5] #escm
covs$ep[covs$var == vnames[6]] <- results_list[["2018"]]$statistic[6] #dom5
covs$ep[covs$var == vnames[7]] <- results_list[["2018"]]$statistic[7] #renda1
covs$ep[covs$var == vnames[8]] <- results_list[["2018"]]$statistic[8] #pibpc
covs$ep[covs$var == vnames[9]] <- results_list[["2018"]]$statistic[9] #trab manual Pai
covs$ep[covs$var == vnames[10]] <- results_list[["2018"]]$statistic[10] #trab manual Mae
covs$ep[covs$var == vnames[11]] <- results_list[["2018"]]$statistic[11] #temp1d
covs$ep[covs$var == vnames[12]] <- results_list[["2018"]]$statistic[12] #temp2d
covs$ep[covs$var == vnames[13]] <- results_list[["2018"]]$statistic[13] #um1d
covs$ep[covs$var == vnames[14]] <- results_list[["2018"]]$statistic[14] #um2d



plot_covs <- ggplot(data = covs) +
  theme_bw() + 
  labs(x = 't-statistic', y = NULL) + 
  scale_x_continuous(breaks = c(-1.96, 0, +1.96), 
                     labels = c(-1.96, '', 1.96), 
                     limits = c(-6.5, 6.5)) + 
  geom_vline(xintercept = c(-1.96, 1.96), color = 'red', linetype = 'dashed', linewidth = 1) + 
  geom_point(aes(x = ep, y = var), color = '#1A2D99', size = 2.25) + 
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

plot_covs

ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/border_matched_covs_test_",2018,"_.png"), device = "png", height = 10, width = 7)
ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/border_matched_covs_test_",2018,"_.eps"), device = "eps", height = 10, width = 7)

rm( covs, plot_covs)

##### 2019 ----
covs <- data.frame(
  ano = rep(2017:2017, length(vnames)),
  var = vnames,
  ep = rep(NA, times = length(vnames))
) %>% 
  mutate(
    ano = "2019"
  )


covs$ep[covs$var == vnames[1]] <- results_list[["2019"]]$statistic[1] #id18
covs$ep[covs$var == vnames[2]] <- results_list[["2019"]]$statistic[2] #fem
covs$ep[covs$var == vnames[3]] <- results_list[["2019"]]$statistic[3] #ppi
covs$ep[covs$var == vnames[4]] <- results_list[["2019"]]$statistic[4] #escp
covs$ep[covs$var == vnames[5]] <- results_list[["2019"]]$statistic[5] #escm
covs$ep[covs$var == vnames[6]] <- results_list[["2019"]]$statistic[6] #dom5
covs$ep[covs$var == vnames[7]] <- results_list[["2019"]]$statistic[7] #renda1
covs$ep[covs$var == vnames[8]] <- results_list[["2019"]]$statistic[8] #pibpc
covs$ep[covs$var == vnames[9]] <- results_list[["2019"]]$statistic[9] #trab manual Pai
covs$ep[covs$var == vnames[10]] <- results_list[["2019"]]$statistic[10] #trab manual Mae
covs$ep[covs$var == vnames[11]] <- results_list[["2019"]]$statistic[11] #temp1d
covs$ep[covs$var == vnames[12]] <- results_list[["2019"]]$statistic[12] #temp2d
covs$ep[covs$var == vnames[13]] <- results_list[["2019"]]$statistic[13] #um1d
covs$ep[covs$var == vnames[14]] <- results_list[["2019"]]$statistic[14] #um2d



plot_covs <- ggplot(data = covs) +
  theme_bw() + 
  labs(x = 't-statistic', y = NULL) + 
  scale_x_continuous(breaks = c(-1.96, 0, +1.96), 
                     labels = c(-1.96, '', 1.96), 
                     limits = c(-6.5, 6.5)) + 
  geom_vline(xintercept = c(-1.96, 1.96), color = 'red', linetype = 'dashed', linewidth = 1) + 
  geom_point(aes(x = ep, y = var), color = '#1A2D99', size = 2.25) + 
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

plot_covs

ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/border_matched_covs_test_",2019,"_.png"), device = "png", height = 10, width = 7)
ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/border_matched_covs_test_",2019,"_.eps"), device = "eps", height = 10, width = 7)



######## JUNTOS ----
covs <- data.frame(
  ano = rep(2017:2017, length(vnames)),
  var = vnames,
  ep18 = rep(NA, times = length(vnames)),
  ep19 = rep(NA, times = length(vnames))
) %>% 
  mutate(
    ano = "Both"
  )

covs$ep18[covs$var == vnames[1]] <- results_list[["2018"]]$statistic[1] #id18
covs$ep18[covs$var == vnames[2]] <- results_list[["2018"]]$statistic[2] #fem
covs$ep18[covs$var == vnames[3]] <- results_list[["2018"]]$statistic[3] #ppi
covs$ep18[covs$var == vnames[4]] <- results_list[["2018"]]$statistic[4] #escp
covs$ep18[covs$var == vnames[5]] <- results_list[["2018"]]$statistic[5] #escm
covs$ep18[covs$var == vnames[6]] <- results_list[["2018"]]$statistic[6] #dom5
covs$ep18[covs$var == vnames[7]] <- results_list[["2018"]]$statistic[7] #renda1
covs$ep18[covs$var == vnames[8]] <- results_list[["2018"]]$statistic[8] #pibpc
covs$ep18[covs$var == vnames[9]] <- results_list[["2018"]]$statistic[9] #trab manual Pai
covs$ep18[covs$var == vnames[10]] <- results_list[["2018"]]$statistic[10] #trab manual Mae
covs$ep18[covs$var == vnames[11]] <- results_list[["2018"]]$statistic[11] #temp1d
covs$ep18[covs$var == vnames[12]] <- results_list[["2018"]]$statistic[12] #temp2d
covs$ep18[covs$var == vnames[13]] <- results_list[["2018"]]$statistic[13] #um1d
covs$ep18[covs$var == vnames[14]] <- results_list[["2018"]]$statistic[14] #um2d




covs$ep19[covs$var == vnames[1]] <- results_list[["2019"]]$statistic[1] #id19
covs$ep19[covs$var == vnames[2]] <- results_list[["2019"]]$statistic[2] #fem
covs$ep19[covs$var == vnames[3]] <- results_list[["2019"]]$statistic[3] #ppi
covs$ep19[covs$var == vnames[4]] <- results_list[["2019"]]$statistic[4] #escp
covs$ep19[covs$var == vnames[5]] <- results_list[["2019"]]$statistic[5] #escm
covs$ep19[covs$var == vnames[6]] <- results_list[["2019"]]$statistic[6] #dom5
covs$ep19[covs$var == vnames[7]] <- results_list[["2019"]]$statistic[7] #renda1
covs$ep19[covs$var == vnames[8]] <- results_list[["2019"]]$statistic[8] #pibpc
covs$ep19[covs$var == vnames[9]] <- results_list[["2019"]]$statistic[9] #trab manual Pai
covs$ep19[covs$var == vnames[10]] <- results_list[["2019"]]$statistic[10] #trab manual Mae
covs$ep19[covs$var == vnames[11]] <- results_list[["2019"]]$statistic[11] #temp1d
covs$ep19[covs$var == vnames[12]] <- results_list[["2019"]]$statistic[12] #temp2d
covs$ep19[covs$var == vnames[13]] <- results_list[["2019"]]$statistic[13] #um1d
covs$ep19[covs$var == vnames[14]] <- results_list[["2019"]]$statistic[14] #um2d


plot_covs <- ggplot(data = covs) +
  theme_bw() + 
  labs(x = 't-statistic', y = NULL) + 
  scale_x_continuous(breaks = c(-1.96, 0, +1.96), 
                     labels = c(-1.96, '', 1.96), 
                     limits = c(-5.1, 5.1)) + 
  geom_vline(xintercept = c(-1.96, 1.96), color = 'red', linetype = 'dashed', linewidth = 1) + 
  geom_point(aes(x = ep18, y = var), color = '#1A2D99', size = 2.25) + 
  geom_point(aes(x = ep19, y = var), color = '#fc8d62', size = 2.25) + 
  
  scale_color_manual(
    values = c("2018" = '#1A2D99', "2019" = '#fc8d62')
  ) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )


plot_covs

library(tidyverse)

covs_long <- covs %>%
  pivot_longer(cols = starts_with("ep"),
               names_to = "ep",
               values_to = "tstat")

# Step 2: Rename the ep variable for better legend labels
covs_long <- covs_long %>%
  mutate(ep = recode_factor(ep,
                            "ep18" = "2018",
                            "ep19" = "2019"))

# Step 3: Plot
plot_covs <- ggplot(data = covs_long) +
  theme_bw() + 
  labs(x = 't-statistic', y = NULL, color = "Year") + 
  scale_x_continuous(
    breaks = c(-1.96, 0, +1.96), 
    labels = c("-1.96", "", "1.96"), 
    limits = c(-5.5, 5.5)
  ) + 
  geom_vline(xintercept = c(-1.96, 1.96), 
             color = 'red', 
             linetype = 'dashed', 
             linewidth = 1) + 
  geom_point(aes(x = tstat, y = var, color = ep), size = 2.25) +
  scale_color_manual(
    values = c("2018" = "#1A2D99", "2019" = "#D55E00")
  ) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 19),
    
    legend.position = c(1.01, 0),  
    legend.justification = c(0, 0),  
    
    plot.margin = margin(c(10, 80, 10, 10), "pt")  
    
  )

plot_covs


ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/both_border_covs_test.png"), device = "png", height = 10, width = 8)
ggsave(plot = plot_covs, filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/img/pdf/both_border_covs_test.eps"), device = "eps", height = 10, width = 8)



rm( covs, plot_covs, base_inpe, base_inpe_t, line, map, matched_df,
    matched_df_id_pair, mun_border, mun_enem, mun_hv, results_covs, results_list,
    vnames, year, test_covariates, var_list)



# ---------------------------------------------------------------------------- #
# 18. Filter + Desc ----
# ---------------------------------------------------------------------------- #



base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/base_nota_2019.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/base_nota_2018.RDS"))) %>%
  setDT()



base_con <- base %>% filter(conclusao == 1)

base_trei <- base %>% filter(treineiro == 1)

gc()

rm(base)


summary(base_con$mun_escola)
summary(base_trei$mun_escola)

# ---------------------------------------------------------------------------- #
## 18.1 EST C+T ----
# ---------------------------------------------------------------------------- #

base_con <- base_con[,.(media = mean(media, na.rm = T), obs = .N),
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


### 18.1.1 Regs ----

ef <- dummy_cols(base_con$seg[base_con$ano == 2018])
ef <- ef %>% select(-1,-2)

ef2 <- dummy_cols(base_trei$seg[base_trei$ano == 2018])
ef2 <- ef2 %>% select(-1,-2)

all.equal(ef, ef2)


list <- list()

#### A. Formandos ----
list[[as.character(paste0(2019,"-",2018,"C|Conc"))]] <- rdrobust(
  y = base_con$d.media[base_con$ano == 2019],
  x = base_con$dist_hv_border[base_con$ano == 2018],
  c = 0,
  cluster = base_con$seg[base_con$ano == 2018],
  weights = base_con$obs[base_con$ano == 2018],
  vce = "hc0",
  covs = cbind(
    ef,
    base_con$lat[base_con$ano == 2018],
    base_con$lon[base_con$ano == 2018]
  )
)

#### B. Treineiros -----
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
## 18.2. TC ----
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
###18.2.1 Reg -----
# ---------------------------------------------------------------------------- #


ef <- dummy_cols(base_t$seg[base_t$ano == 2018])
ef <- ef %>% select(-1,-2)


#### C. Todos Concluintes ----
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


#### D.  Escola P ----
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

#### E. Escola Priv ----
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


#### F. Esc Pub - Fed ----
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

#### G. Esc Estadual ----
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

#### H. Esc FED ----
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

#### I. Esc MUN ----
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


###4. Tabelas -----
t10 <- data.frame(
  coef = do.call(rbind,lapply(list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(list, FUN = function(x){x$pv[3]})),
  n = do.call(rbind,lapply(list, FUN = function(x){x$N_h}))
)
print(t10)

bw_main_a  <- list[["2019-2018C|TC"]]$bws[1]
bw_bias_a  <- list[["2019-2018C|TC"]]$bws[2]

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



names <- c("Concluded High School",
           " "," ",
           "Mock Examinees",
           " ", " ",
           "Senior Year",
           " "," ",
           "Senior Year Public School",
           " ", " ",
           "Senior Year Private School",
           " "," ",
           "Senior Municipal + State School",
           " ", " ",
           "Senior State School",
           " "," ",
           "Senior Federal School",
           " ", " ",
           "Senior Municipal School",
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

result$con[25] <- t10$coef[[9]]
result$con[26] <- t10$se[[9]]
result$con[27] <- t10$N[[9]]

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
rm(ef, list, result, t10, latex_table)



# ---------------------------------------------------------------------------- #
# ***SAEB*** ----------------
# ---------------------------------------------------------------------------- #

saeb_base <- readRDS("Z:/Tuffy/Paper - HV/Bases/saeb_total.RDS")



 ##19.1 Base ----
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
    d.media = media - v2_nota,
    
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_nota, v2_nota))


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

## 19.2 Reg ----

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
  
  rm(ef)

  
}
rm(clu, latv, lonv, temp, w_lp, w_mt, xv, yv_lp, yv_mt, i)

## 19.3 Tabelas ----

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

rm(rlist_saeb, saeb_base, tab,result, names)


# ---------------------------------------------------------------------------- #
# 19.4 ALL ----
# ---------------------------------------------------------------------------- #


saeb_base <- readRDS("Z:/Tuffy/Paper - HV/Bases/saeb_total.RDS")



##19.1 Base ----
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
    d.media = media - v2_nota,
    
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_nota, v2_nota))



temp <- base_a %>% 
  filter(ano == 2018) %>% 
  select(mun_prova, lat, lon, dist_hv_border, seg)


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


saeb_base <- saeb_base %>% 
  group_by(mun_prova) %>% 
  left_join(mun_hv, by = c("mun_prova" = "co_municipio")) %>% 
  mutate(hv = ifelse(mun_prova %/% 100000 > 30, 1, 0),
         dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
  ungroup()
  

summary(saeb_base)
rm(mun_hv)

summary(saeb_base %>% select(dist_hv_border, lat, lon))

nrow(saeb_base %>% filter(serie == 3, ano == 2017))


### 19.4.1 Reg ----

# Lista para armazenar resultados
rlist_saeb <- list()


for (i in c("5","9","3")) {
  
  temp <- saeb_base %>% 
    filter(serie == i,
           !is.na(seg) &
             !is.na(dist_hv_border) &
             !is.na(lat) &
             !is.na(lon)) %>% 
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
  

  
  rm(ef)
  
  
}

rm(clu, latv, lonv, temp, w_lp, w_mt, xv, yv_lp, yv_mt, i)

### 19.4.2 Tabelas ----

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


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Saeb_all.tex")


###19.4.3 BW ----

rlist_saeb <- list()


for (i in c("5","9","3")) {
  
  temp <- saeb_base %>% 
    filter(serie == i,
           !is.na(seg) &
             !is.na(dist_hv_border) &
             !is.na(lat) &
             !is.na(lon)) %>% 
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
    # h = bw_main_a,
    # b = bw_bias_a,
    weights = w_lp,
    cluster = clu,
    vce = "hc0",
    covs = cbind(ef,
                 latv,
                 lonv)
  )
  
  
  # Regressão MT --------------------------------------------------------------#
  
  rlist_saeb[[paste0("MT|",i)]] <- rdrobust(
    y = yv_mt$d.media_mt,
    x = xv$dist_hv_border,
    c = 0,
    # h = bw_main_a,
    # b = bw_bias_a,
    weights = w_mt,
    cluster = clu,
    vce = "hc0",
    covs = cbind(ef,
                 latv,
                 lonv)
  )
  
  
  
  rm(ef)
  
  
}

rm(clu, latv, lonv, temp, w_lp, w_mt, xv, yv_lp, yv_mt, i)

### 19.4.2 Tabelas ----

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


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/Saeb_all_outbw.tex")

rm(rlist_saeb, saeb_base, tab,result, names)




# ---------------------------------------------------------------------------- #
# 14. Base Desc ----
# ---------------------------------------------------------------------------- #

## 14.1 19 ----
gc()

base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/base_nota_2019.RDS"))
summary(base %>% select(conclusao, treineiro))

in_both <- unique(base$id_enem)

both_days19 <- nrow(base %>% filter(ano == 2019))

base <- base %>% 
  select(treineiro, conclusao, mun_prova, id_enem)

temp <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/enem_abs_2019_v4.RDS"))

temp <- temp %>% 
  mutate(
    abs = ifelse(
      abs_rd == 1 &
        abs_cn == 1 &
        abs_ch == 1 &
        abs_lc == 1 &
        abs_mt == 1,
      1,
      0),
    priv = ifelse(dep_adm == 4, 1, 0),
    priv0 = ifelse(priv == 0, 1, NA),
    test = ifelse(id_enem %in% in_both, 1, 0))

total19 <- nrow(temp)

summary(base %>% select(conclusao, treineiro))




base <- base %>% 
  mutate(presente = ifelse(id_enem %in% in_both, 1, 0))

nrow(base %>% filter(presente == 1))
gc()

trei_19 <- sum(temp$treineiro == 1)/nrow(temp)
ntrei_19 <- nrow(temp %>% filter(treineiro == 1))

conc_19 <- sum(temp$conclusao == 1)/nrow(temp)
nconc_19 <- nrow(temp %>% filter(conclusao == 1))

both_days19 <- nrow(base)/nrow(temp)
n_both_days19 <- nrow(base)

in_em_19 <- sum(temp$conclusao == 2)/nrow(temp)
nin_em_19 <- nrow(temp %>% filter(conclusao == 2))

pub_em_19 <- nrow(temp %>% filter(conclusao == 2, priv0 == 1))/nrow(temp)
npub_em_19 <- nrow(temp %>% filter(conclusao == 2, priv0 == 1))

##14.2 18 ----
gc()

base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/base_nota_2018.RDS"))
summary(base %>% select(conclusao, treineiro))

in_both <- unique(base$id_enem)

both_days18 <- nrow(base %>% filter(ano == 2018))

base <- base %>% 
  select(treineiro, conclusao, mun_prova, id_enem)

temp <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/TODOS/enem_abs_2018_v4.RDS"))

temp <- temp %>% 
  mutate(
    abs = ifelse(
      abs_rd == 1 &
        abs_cn == 1 &
        abs_ch == 1 &
        abs_lc == 1 &
        abs_mt == 1,
      1,
      0),
    priv = ifelse(dep_adm == 4, 1, 0),
    priv0 = ifelse(priv == 0, 1, NA),
    test = ifelse(id_enem %in% in_both, 1, 0))

total18 <- nrow(temp)

summary(base %>% select(conclusao, treineiro))




base <- base %>% 
  mutate(presente = ifelse(id_enem %in% in_both, 1, 0))

nrow(base %>% filter(presente == 1))
gc()

trei_18 <- sum(temp$treineiro == 1)/nrow(temp)
ntrei_18 <- nrow(temp %>% filter(treineiro == 1))

both_days18 <- nrow(base)/nrow(temp)
n_both_days18 <- nrow(base)

conc_18 <- sum(temp$conclusao == 1)/nrow(temp)
nconc_18 <- nrow(temp %>% filter(conclusao == 1))

in_em_18 <- sum(temp$conclusao == 2)/nrow(temp)
nin_em_18 <- nrow(temp %>% filter(conclusao == 2))

pub_em_18 <- nrow(temp %>% filter(conclusao == 2, priv0 == 1))/nrow(temp)
npub_em_18 <- nrow(temp %>% filter(conclusao == 2, priv0 == 1))





#Amostras TC e JC
# > View(base)
# > nrow(base %>% filter(ano == 2018))
# [1] 1130696
# > nrow(base %>% filter(ano == 2019))
# [1] 953912
# > nrow(base %>% filter(ano == 2018))/5513747
# [1] 0.2050685
# > nrow(base %>% filter(ano == 2019))/5513747
# [1] 0.1730061
# > nrow(base %>% filter(ano == 2018, idade %in% c(17:18)))
# [1] 921311
# > nrow(base %>% filter(ano == 2019, idade %in% c(17:18)))
# [1] 794091
# > nrow(base %>% filter(ano == 2018, idade %in% c(17:18)))/5513747
# [1] 0.1670934
# > nrow(base %>% filter(ano == 2019, idade %in% c(17:18)))/5513747
# [1] 0.1440202


##13.3 Tabela

names <- c(
  "Total",
  "Presence in Both Days",
  "Senior Year",
  "Senior Year (Public Schools)",
  "Senior Year (PS + Both days)",
  "Senior Year (Final Filtering)",
  "Concluded High School",
  "Mock Applicant")

result <- data.frame(
  var = names,
  #y18 = rep(NA, times = length(names)),
  n18 = rep(NA, times = length(names)),
  #y19 = rep(NA, times = length(names)),
  n19 = rep(NA, times = length(names))
)

#result$y18[1] <- 1.00
#result$y19[1] <- 1.00
result$n18[1] <- total18
result$n19[1] <- total19

#result$y18[2] <- both_days18
#result$y19[2] <- both_days19
result$n18[2] <- n_both_days18
result$n19[2] <- n_both_days19

#result$y18[3] <- in_em_18
#result$y19[3] <- in_em_19
result$n18[3] <- nin_em_18
result$n19[3] <- nin_em_19

#result$y18[4] <- pub_em_18
#result$y19[4] <- pub_em_19
result$n18[4] <- npub_em_18
result$n19[4] <- npub_em_19

#result$y18[7] <- conc_18
#result$y19[7] <- conc_19
result$n18[7] <- nconc_18
result$n19[7] <- nconc_19

#result$y18[8] <- trei_18
#result$y19[8] <- trei_19
result$n18[8] <- ntrei_18
result$n19[8] <- ntrei_19




base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2019.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2018.RDS"))) %>%
  setDT() %>% 
  filter(conclusao == 2)

no_age_18 <- nrow(base %>% filter(ano == 2018, priv0 == 1))
no_age_19 <- nrow(base %>% filter(ano == 2019, priv0 == 1))


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


final_18 <- sum(base_a$obs[base_a$ano == 2018])
final_19 <- sum(base_a$obs[base_a$ano == 2019])

#result$y18[3] <- in_em_18
#result$y19[3] <- in_em_19
result$n18[5] <- no_age_18
result$n19[5] <- no_age_19

#result$y18[4] <- pub_em_18
#result$y19[4] <- pub_em_19
result$n18[6] <- final_18
result$n19[6] <- final_19

(final_18 + final_19)/ (1196984+983079)


print(result)

options(scipen = 999)  # discourage scientific notation globally
result[, -1] <- round(result[, -1], 3)  # assuming 1st column is text
print(result)



colnames(result) <- c("",  "N",  "N")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/disc_v4.tex")

rm(both_days18, both_days19, conc_18, conc_19,
   in_em_18, in_em_19, latex_table, n_both_days18, n_both_days19,
   names, nconc_18, nconc_19, nin_em_18, nin_em_19, npub_em_18, npub_em_19,
   ntrei_18, ntrei_19, pub_em_18, pub_em_19, total18, total19, trei_18, trei_19)

