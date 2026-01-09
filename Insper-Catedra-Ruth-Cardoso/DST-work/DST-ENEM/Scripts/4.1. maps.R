# ---------------------------------------------------------------------------- #
# Arquivo para Criação do Mapa
# ---------------------------------------------------------------------------- #

# Data de criação: 14/11/2023
# Criado por: Bruno Komatsu

# Última modificação: 09/01/2025
# Modificado por: Tuffy Issa

# Descrição: 
# Cria as bases de dados do ENEM

# ---------------------------------------------------------------------------- #
# Bibliotecas ----
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
# 1. Carregando SF com distâncias ----
# ---------------------------------------------------------------------------- #


line <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/line.RDS")
base <- readRDS(file = paste0("Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/base_nota_",2019,".RDS")) %>%
  select(mun_prova) %>%
  unique() %>%
  mutate(amostra = 1)

mun_hv <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/mun_hv.RDS") %>%
  left_join(base, by = c("co_municipio" = "mun_prova")) %>%
  mutate(
    dist_hv = ifelse(amostra == 1, dist_hv_border / 1000, NA)
  )
# ---------------------------------------------------------------------------- #
#2. Mapa de distâncias ---- 
# ---------------------------------------------------------------------------- #
map <- ggplot(mun_hv) +
  # 1) Fronteiras Municipais como a primeira camada
  geom_sf(
    data = mun_hv,
    fill = NA,
    color = "grey70",     
    linewidth = 0.2        
  ) +
  
  # 2) Cor de cada município
  geom_sf(
    data = mun_hv,
    aes(fill = dist_hv),
    color = NA
  ) +
  
  # 3) Escala de cores das distâncias
  scale_fill_viridis_c(
    option = "A", direction = -1,
                       na.value = "transparent",
                       name = "Distance to Daylight Saving Time Border (Km)",
                       guide = guide_colorbar(
                         direction = "horizontal",
                         title.position = "top",
                         barwidth = 25
                       )
  ) +
  geom_sf(data = line, color = "blue") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15))


map
# Salvando o mapa
ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/mapas/map_v2.png",plot = map,device = "png",dpi = 300)
#ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/mapas/map_v2.eps",plot = map,device = "eps")
#ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/mapas/map_v2.pdf",plot = map,device = "pdf")

# ---------------------------------------------------------------------------- #
#3. Mapa de clusters ---- 
# ---------------------------------------------------------------------------- #

#Capturando os valores de bordas do mapa geral
bbox_line <- st_bbox(line)

#Adicionando um limite de plottagem menor, mas dentro do intervalo estabelecido

pad_x <- (bbox_line$xmax - bbox_line$xmin) * 0.15
pad_y <- (bbox_line$ymax - bbox_line$ymin) * 0.15

map <- ggplot(mun_hv) +
  geom_sf(aes(fill = factor(seg))) +
  scale_fill_manual(
    name = "Clusters",
    values = brewer.pal(n = 7, name = "Set2"),
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      barwidth = 25
    )
  ) +
  geom_sf(data = line, color = "blue", linewidth = 1.5) +
  coord_sf(
    xlim = c(bbox_line$xmin - pad_x, bbox_line$xmax + pad_x),
    ylim = c(bbox_line$ymin - pad_y, bbox_line$ymax + pad_y)
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15))


map


# Salvando o mapa
ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/mapas/map_seg.png",plot = map,device = "png",dpi = 300)
#ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/mapas/map_seg.eps",plot = map,device = "eps")
#ggsave(filename = "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/mapas/map_seg.pdf",plot = map,device = "pdf")


# ---------------------------------------------------------------------------- #
#4. Mapa da banda ----
# ---------------------------------------------------------------------------- #

#Bandas ótimas
load("Z:/Tuffy/Paper - HV/Resultados/bandwidths_2019_2018_NF.RData")





mun_hv <- mun_hv %>%
  mutate(
    in_band = ifelse(abs(dist_hv_border) <= bw_main_a, 1, 0),
    in_enem = ifelse(!is.na(amostra), 1, 0),
    
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
                   labels = c("Out", "Bandwidth", "ENEM and Bandwidth")
    )
  )


# Mapa de clusters
map <- ggplot(mun_hv) +
  
  geom_sf(
    data = mun_hv,
    fill = NA,
    color = "grey70",     
    linewidth = 0.2        
  ) +
  
  scale_fill_manual(
    name = "Groups",
    values = c(
      "Out"           = "#E0E0E0",     
      "Bandwidth"         = "#E0D268",     
      "ENEM and Bandwidth"  = "#1B9E77"      
    ),
    drop = FALSE
  ) +
  geom_sf(aes(fill = factor(final)) ) +
  
  geom_sf(data = line, color = "blue", linewidth = 1.5) +
  coord_sf(
    xlim = c(bbox_line$xmin - pad_x, bbox_line$xmax + pad_x),
    ylim = c(bbox_line$ymin - pad_y, bbox_line$ymax + pad_y)
  ) +
  theme_bw() +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,              # force single row
      byrow = TRUE
    )
  ) +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 25, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.key.width = unit(1.4, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.spacing.x = unit(0.6, "cm"),
    legend.title = element_text(size = 20),
    legend.text  = element_text(size = 15)
  )





map

ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/mapas/map_band_v2.png"),plot = map,device = "png", dpi = 300)


# --------------------------------------------------------------------------- #
# 5. Horário de Início ----
# --------------------------------------------------------------------------- #
## 5.1 Dados ----
# --------------------------------------------------------------------------- #


mun_hv <- mun_hv %>% 
  mutate(horario_18 = case_when(
    uf %in% c("GO", "DF", "MG", "ES", "RJ", "SP", "SC", "PR", "RS") ~ "13h",
    
    uf %in% c("BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI", "MA", "TO", "MT",
              "MS", "PA", "AP") ~ "12h",
    
    uf %in% c("RO", "RR") | uf == "AM" & !co_municipio %in% 
      c(1300201, 1300607, 1300706, 1301407, 1301506, 1301654, 1301803, 1301951,
        1302306, 1302405, 1303502, 1303908, 1304062)  ~ "11h",
    
    uf == "AC" | co_municipio %in% c(1300201, 1300607, 1300706, 1301407, 1301506,
                                   1301654, 1301803, 1301951, 1302306, 1302405,
                                   1303502, 1303908, 1304062) ~ "10h",
    TRUE ~ NA),
    
    horario_19 = case_when(
      uf %in% c("GO", "DF", "MG", "ES", "RJ", "SP", "SC", "PR", "RS", "BA", "SE",
                "AL", "PE", "PB", "RN", "CE","PI", "MA", "TO", "PA", "AP") ~ "13h",
      
      uf %in% c("MT", "MS", "RO", "RR") | uf == "AM" & !co_municipio %in%
        c(1300201, 1300607, 1300706, 1301407, 1301506, 1301654, 1301803, 1301951,
          1302306, 1302405, 1303502, 1303908, 1304062) ~ "12h",
      
      uf == "AC" | co_municipio %in% c(1300201, 1300607, 1300706, 1301407, 1301506,
                                     1301654, 1301803, 1301951, 1302306, 1302405,
                                     1303502, 1303908, 1304062) ~ "11h",
      TRUE ~ NA),
    
    horario_18 = factor(horario_18, levels = c("10h", "11h", "12h", "13h")),
    horario_19 = factor(horario_19, levels = c("11h", "12h", "13h")))

#------------------------------------------------------------------------------#
## 5.2 Com HV ----
# -----------------------------------------------------------------------------#

# Mapa de clusters
map <- ggplot(mun_hv %>% arrange(horario_18)) +
  
  geom_sf(
    data = mun_hv,
    fill = NA,
    color = "transparent",     
    linewidth = 0.2        
  ) +
  
  scale_fill_manual(
    name = "Groups",
    values = c(
      "10h"  = "#6A3D9A", 
      "11h"  = "#0072B2",
      "12h"  = "#E69F00",     
      "13h"  = "#009E73"      
    ),
    drop = FALSE
  ) +
  geom_sf(aes(fill = factor(horario_18)) ) +
  
  geom_sf(data = line, color = "blue") +
  theme_bw() +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,              # force single row
      byrow = TRUE
    )
  ) +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 25, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.key.width = unit(1.4, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.spacing.x = unit(0.6, "cm"),
    legend.title = element_text(size = 20),
    legend.text  = element_text(size = 15)
  )

map

ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/mapas/map_band_h18.png"),plot = map,device = "png", dpi = 300)


#------------------------------------------------------------------------------#
## 5.3 Sem HV ----
# -----------------------------------------------------------------------------#

# Mapa de clusters
map <- ggplot(mun_hv %>% arrange(horario_19)) +
  
  geom_sf(
    data = mun_hv,
    fill = NA,
    color = "transparent",     
    linewidth = 0.2        
  ) +
  
  scale_fill_manual(
    name = "Groups",
    values = c(
      "11h"  = "#0072B2",
      "12h"  = "#E69F00",     
      "13h"  = "#009E73"      
    ),
    drop = FALSE
  ) +
  geom_sf(aes(fill = factor(horario_19)) ) +
  
  geom_sf(data = line, color = "blue") +
  theme_bw() +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,              # force single row
      byrow = TRUE
    )
  ) +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 25, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.key.width = unit(1.4, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.spacing.x = unit(0.6, "cm"),
    legend.title = element_text(size = 20),
    legend.text  = element_text(size = 15)
  )

map

ggsave(filename = paste0("Z:/Tuffy/Paper - HV/Resultados/definitive/notas/mapas/map_band_h19.png"),plot = map,device = "png", dpi = 300)


