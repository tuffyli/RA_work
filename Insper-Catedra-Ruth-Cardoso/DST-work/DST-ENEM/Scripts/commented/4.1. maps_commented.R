# ---------------------------------------------------------------------------- #
# Map Creation File
# ---------------------------------------------------------------------------- #

# Creation date: 2023-11-14
# Created by: Bruno Komatsu

# Last modification: 2025-04-10
# Modified by: Tuffy Issa

# Description: 
# Creates the ENEM datasets

# ---------------------------------------------------------------------------- #
# Libraries ----
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
library(ggpattern)

# ---------------------------------------------------------------------------- #
# Paths ----
# ---------------------------------------------------------------------------- #

# 01_master.R should define these paths before sourcing this script. The fallback
# below keeps the script reproducible when run from the `reprod/` folder.
path_from_env <- function(env, default) {
  value <- Sys.getenv(env, unset = NA_character_)
  if (!is.na(value) && nzchar(value)) {
    return(normalizePath(value, winslash = "/", mustWork = FALSE))
  }
  normalizePath(default, winslash = "/", mustWork = FALSE)
}

if (!exists("reprod_path", inherits = TRUE)) {
  reprod_path <- path_from_env("HV_REPROD_PATH", getwd())
}

if (!exists("project_path", inherits = TRUE)) {
  project_path <- path_from_env("HV_PROJECT_PATH", file.path(reprod_path, ".."))
}

if (!exists("data_path", inherits = TRUE)) {
  data_path <- path_from_env("HV_DATA_PATH", file.path(project_path, "data"))
}

if (!exists("intermediate_path", inherits = TRUE)) {
  intermediate_path <- path_from_env("HV_INTERMEDIATE_PATH",
                                     file.path(data_path, "intermediate"))
}

if (!exists("processed_path", inherits = TRUE)) {
  processed_path <- path_from_env("HV_PROCESSED_PATH", file.path(data_path, "processed"))
}

if (!exists("revision_path", inherits = TRUE)) {
  revision_path <- path_from_env("HV_REVISION_PATH",
                                 file.path(intermediate_path, "revision"))
}

if (!exists("no_age_path", inherits = TRUE)) {
  no_age_path <- path_from_env("HV_NO_AGE_PATH",
                               file.path(intermediate_path, "no_age_filter"))
}

bandwidth_path <- file.path(intermediate_path, "bandwidths")
outputs_path <- file.path(project_path, "outputs")
controls_output_path <- file.path(outputs_path, "controls")
controls_maps_path <- file.path(controls_output_path, "maps")

dir.create(bandwidth_path, recursive = TRUE, showWarnings = FALSE)
dir.create(controls_maps_path, recursive = TRUE, showWarnings = FALSE)

path_paste <- function(root, ...) {
  file.path(root, paste0(...))
}

.path_objects <- c(
  "path_from_env", "path_paste", "clear_workspace", ".path_objects",
  "reprod_path", "project_path", "data_path", "intermediate_path",
  "processed_path", "revision_path", "no_age_path", "bandwidth_path",
  "outputs_path", "controls_output_path", "controls_maps_path"
)

clear_workspace <- function(extra_keep = character(), envir = parent.frame()) {
  keep <- unique(c(.path_objects, extra_keep))
  rm(list = setdiff(ls(envir = envir), keep), envir = envir)
  invisible(gc())
}





# ---------------------------------------------------------------------------- #
# 1. Load SF with distances ----
# ---------------------------------------------------------------------------- #
# Loads the saved border geometry and municipality-distance layer, then flags the
# municipalities that appear in the ENEM sample.


line <- readRDS(file = file.path(revision_path, "line.RDS"))
base <- readRDS(file = path_paste(no_age_path, "base_nota_", 2019, ".RDS")) %>%
  select(mun_prova) %>%
  unique() %>%
  mutate(amostra = 1)

mun_hv <- readRDS(file = file.path(revision_path, "mun_hv.RDS")) %>%
  left_join(base, by = c("co_municipio" = "mun_prova")) %>%
  mutate(
    dist_hv = ifelse(amostra == 1, dist_hv_border / 1000, NA),
    res_dist_hv = dist_hv_border/1000
  )
# ---------------------------------------------------------------------------- #
#2. Distance map ---- 
# ---------------------------------------------------------------------------- #
# Draws the signed distance-to-border surface and a zoomed alternative focused on
# the DST frontier.
map <- ggplot(mun_hv) +
  # 1) Municipal borders as the first layer
  geom_sf(
    data = mun_hv,
    fill = NA,
    color = "grey70",     
    linewidth = 0.2        
  ) +
  
  # 2) Color for each municipality
  geom_sf(
    data = mun_hv,
    aes(fill = res_dist_hv),
    color = NA
  ) +
  
  # 3) Distance color scale
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


# Save the map
ggsave(filename = file.path(controls_maps_path, "map_v2.png"),plot = map,device = "png",dpi = 300, width = 5.5, height = 4.5)
#ggsave(filename = file.path(controls_maps_path, "map_v2.eps"),plot = map,device = "eps")
#ggsave(filename = file.path(controls_maps_path, "map_v2.pdf"),plot = map,device = "pdf")

map_aer <- ggplot() +
  
  #  Municipal borders (background layer)
  geom_sf(
    data = mun_hv,
    fill = NA,
    color = "grey80",      # subtle
    linewidth = 0.15
  ) +
  
  #  Heatmap on top
  geom_sf(
    data = mun_hv,
    aes(fill = res_dist_hv),
    color = NA
  ) +
  
  # DST border (main feature)
  geom_sf(
    data = line,
    color = "blue",
    linewidth = 1.2
  ) +
  
  scale_fill_viridis_c(
    option = "A",
    direction = -1,
    na.value = "transparent",
    name = NULL,
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      barwidth = 20
    )
  ) +
  
  coord_sf(expand = FALSE) +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    axis.text = element_text(size = 10, color = "grey30"),
    axis.title = element_text(size = 11),
    
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  )

map_aer

ggsave(filename = file.path(controls_maps_path, "map_v3.png"),plot = map_aer,device = "png",dpi = 300, width = 5.5, height = 4.5)


# ---------------------------------------------------------------------------- #
#3. Cluster map ---- 
# ---------------------------------------------------------------------------- #
# Builds the map of nearest border segments, including a version with UF borders
# emphasized for clearer publication use.

# Capture boundary values from the full map
bbox_line <- st_bbox(line)

# Add a smaller plotting limit within the established interval

pad_x <- (bbox_line$xmax - bbox_line$xmin) * 0.20
pad_y <- (bbox_line$ymax - bbox_line$ymin) * 0.20

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


print(map)


uf_boundaries <- mun_hv %>%
  group_by(uf) %>%
  summarise(do_union = TRUE) %>%      # dissolve just to get outer border
  st_boundary()                       # keep only the border lines

uf_layer <- mun_hv %>%
  group_by(uf) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

map <- ggplot(mun_hv) +
  
  # Base municipalities with clusters
  geom_sf(aes(fill = factor(seg)),
          color = "grey85",
          linewidth = 0.1) +
  
  # UF borders ON TOP (clean highlighting layer)
  geom_sf(data = uf_boundaries,
          color = "black",
          linewidth = 0.8) +
  
  # DST border
  geom_sf(data = line,
          color = "blue",
          linewidth = 1.5) +
  
  scale_fill_manual(
    name = "Nearest Border Segment",
    values = brewer.pal(n = 7, name = "Dark2"),
    guide = guide_legend(
      direction = "horizontal",
      title.position = "top",
      nrow = 1
    )
  ) +
  
  coord_sf(
    xlim = c(bbox_line$xmin - pad_x, bbox_line$xmax + pad_x),
    ylim = c(bbox_line$ymin - pad_y, bbox_line$ymax + pad_y),
    expand = FALSE
  ) +
  
  theme_void() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    axis.text = element_text(size = 10, color = "grey30"),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11)
  )

print(map)


# 
# library(ggpattern)
# 
# mun_hv <- mun_hv %>%
#   ungroup() %>%
#   st_as_sf()
# 
# 
# library(ggplot2)
# library(sf)
# library(ggpattern)
# library(RColorBrewer)
# 
# # Convert UF to factor (important for manual scales)
# mun_hv$uf <- factor(mun_hv$uf)
# 
# # Define angles and spacing for each UF
# uf_levels <- levels(mun_hv$uf)
# 
# angle_values <- setNames(
#   seq(0, 150, length.out = length(uf_levels)),
#   uf_levels
# )
# 
# spacing_values <- setNames(
#   seq(0.015, 0.045, length.out = length(uf_levels)),
#   uf_levels
# )
# 
# map <- ggplot(mun_hv) +
#   
#   geom_sf_pattern(
#     aes(
#       fill = factor(seg),
#       pattern_angle = uf,
#       pattern_spacing = uf
#     ),
#     pattern = "stripe",
#     pattern_density = 0.4,
#     pattern_fill = "black",
#     pattern_colour = "black",
#     pattern_size = 0.3,
#     color = "grey60"
#   ) +
#   
#   scale_pattern_angle_manual(
#     values = angle_values
#   ) +
#   
#   scale_pattern_spacing_manual(
#     values = spacing_values
#   ) +
#   
#   scale_fill_manual(
#     name = "Clusters",
#     values = brewer.pal(n = 7, name = "Set2")
#   ) +
#   
#   geom_sf(
#     data = line,
#     color = "blue",
#     linewidth = 1.5
#   ) +
#   
#   coord_sf(
#     xlim = c(bbox_line$xmin - pad_x, bbox_line$xmax + pad_x),
#     ylim = c(bbox_line$ymin - pad_y, bbox_line$ymax + pad_y)
#   ) +
#   guides(
#     pattern_angle = "none",
#     pattern_spacing = "none"
#   ) +
#   theme_bw() +
#   theme(
#     legend.position = "bottom",
#     axis.text = element_text(size = 20),
#     axis.title = element_text(size = 25, face = "bold"),
#     legend.title = element_text(size = 20),
#     legend.text = element_text(size = 15)
#   )
# 
# map
# 
# print(map)

# Save the map
ggsave(filename = file.path(controls_maps_path, "map_seg2.png"),plot = map,device = "png",dpi = 300)
#ggsave(filename = file.path(controls_maps_path, "map_seg.eps"),plot = map,device = "eps")
#ggsave(filename = file.path(controls_maps_path, "map_seg.pdf"),plot = map,device = "pdf")

rm(uf_layer, uf_boundaries)

# ---------------------------------------------------------------------------- #
#4. Bandwidth map ----
# ---------------------------------------------------------------------------- #
## 4.1 Exam Mun ----
# ---------------------------------------------------------------------------- #
# Marks municipalities that are inside the optimal RD bandwidth and also appear
# in the exam-municipality ENEM sample.

# Optimal bandwidths
load(file.path(bandwidth_path, "bandwidths_2019_2018_Res_all.RData"))





mun_hv <- mun_hv %>%
  mutate(
    in_band = ifelse(abs(dist_hv_border) <= bw_main_r, 1, 0),
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


# Cluster map
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
      "Bandwidth"         = "#BDB76B",     
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

map_aer <- ggplot(mun_hv) +
  
  geom_sf(aes(fill = final),
          color = "grey92",
          linewidth = 0.05) +
  
  geom_sf(data = line,
          color = "black",
          linewidth = 1.1) +
  
  scale_fill_manual(
    name = NULL,
    values = c(
      "Out" = "grey85",
      "Bandwidth" = "#C2B280",
      "ENEM and Bandwidth" = "#4C9F70"
    ),
    drop = FALSE
  ) +
  
  coord_sf(
    xlim = c(bbox_line$xmin - pad_x, bbox_line$xmax + pad_x),
    ylim = c(bbox_line$ymin - pad_y, bbox_line$ymax + pad_y),
    expand = FALSE
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    axis.text = element_text(size = 10, color = "grey30", margin = margin(t = 5, r = 5)),
    axis.title = element_text(size = 11),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    
    plot.margin = margin(8, 8, 8, 8)
  )

map_aer

ggsave(filename = paste0(file.path(controls_maps_path, "map_band_v2.png")),plot = map,device = "png", dpi = 300)
ggsave(filename = paste0(file.path(controls_maps_path, "map_band_v3.png")),plot = map_aer,device = "png", dpi = 300, width = 5.5, height = 4.5)

rm(map_aer)

# ---------------------------------------------------------------------------- #
## 4.2 Res Mun ----
# ---------------------------------------------------------------------------- #
# Reuses the optimal bandwidth to show the residence-municipality inclusion area.
# Optimal bandwidths



mun_hv <- mun_hv %>%
  mutate(
    in_band = ifelse(abs(dist_hv_border) <= bw_main_r, 1, 0),

    final = case_when(
      in_band == 1 ~ 1,
      TRUE ~ 0
    ),
    final = as.factor(final)
  ) 


mun_hv <- mun_hv %>%
  mutate(
    final = factor(final,
                   levels = c("0", "1"),
                   labels = c("Out", "Residency Optimal Bandwidth")
    )
  )


map_aer <- ggplot(mun_hv) +
  
  geom_sf(aes(fill = final),
          color = "grey92",
          linewidth = 0.05) +
  
  geom_sf(data = line,
          color = "black",
          linewidth = 1.1) +
  
  scale_fill_manual(
    name = NULL,
    values = c(
      "Out" = "grey85",
      "Residency Optimal Bandwidth" = "#4C9F70"
    ),
    drop = FALSE
  ) +
  
  coord_sf(
    xlim = c(bbox_line$xmin - pad_x, bbox_line$xmax + pad_x),
    ylim = c(bbox_line$ymin - pad_y, bbox_line$ymax + pad_y),
    expand = FALSE
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    axis.text = element_text(size = 10, color = "grey30", margin = margin(t = 5, r = 5)),
    axis.title = element_text(size = 11),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    
    plot.margin = margin(8, 8, 8, 8)
  )

map_aer

ggsave(filename = file.path(controls_maps_path, "map_band_res.png"), plot = map, device = "png", dpi = 300)
ggsave(filename = file.path(controls_maps_path, "map_band_res_aer.png"), plot = map_aer, device = "png", dpi = 300, width = 5.5, height = 4.5)

rm(map_aer)

# --------------------------------------------------------------------------- #
# 5. Start Time ----
# --------------------------------------------------------------------------- #
## 5.1 Data ----
# --------------------------------------------------------------------------- #
# Creates municipality-level exam start-time categories for the DST year and the
# post-DST comparison year.


mun_hv <- mun_hv %>% 
  mutate(horario_18 = case_when(
    uf %in% c("GO", "DF", "MG", "ES", "RJ", "SP", "SC", "PR", "RS") ~ "13h (DST)",
    
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
    
    horario_18 = case_when(
      horario_18 == "12h" & hv == 1 ~ "12h (DST)",
      TRUE ~ horario_18
    ),
    
    horario_18 = factor(
      horario_18,
      levels = c("10h", "11h", "12h", "12h (DST)", "13h (DST)")
    ),
    
    horario_19 = factor(horario_19, levels = c("11h", "12h", "13h")))

#------------------------------------------------------------------------------#
## 5.2 With DST ----
# -----------------------------------------------------------------------------#
# Plots the start-time groups when daylight saving time was active.

dst_contour <- mun_hv %>%
  filter(hv == 1) %>%
  st_union() %>% 
  st_sf() %>% 
  st_cast("POLYGON")


map <- ggplot(mun_hv %>% arrange(horario_18)) +
  geom_sf(fill = NA, color = "transparent") +
  geom_sf(aes(fill = horario_18), color = NA) +
  geom_sf(data = dst_contour, color = "red", fill = NA, linewidth = 2.0) +
  scale_fill_manual(
    name = "Groups",
    values = c(
      "10h" = "#6A3D9A",
      "11h" = "#0072B2",
      "12h" = "#E69F00",
      "12h (DST)" = "#F1B44C",
      "13h (DST)" = "#4DBF9F"
    )
  ) +
  coord_sf(
    xlim = c(-77, -30),
    ylim = c(-36, 7),
    expand = FALSE
  ) +
  theme_bw() +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 2,
      byrow = TRUE
    )
  ) +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.spacing.x = unit(0.25, "cm"),
    legend.title = element_text(size = 15),
    legend.text  = element_text(size = 13),
    plot.margin = margin(4, 4, 4, 4)
  )

print(map)

ggsave(filename = paste0(file.path(controls_maps_path, "map_band_h18v2.png")),plot = map,device = "png", width = 6.5, height = 5.2, units = "in", dpi = 300, bg = "white")



#------------------------------------------------------------------------------#
## 5.3 Without DST ----
# -----------------------------------------------------------------------------#
# Plots the start-time groups after daylight saving time ended.

# Cluster map
map <- ggplot(mun_hv %>% arrange(horario_19)) +
  
  geom_sf(
    fill = NA,
    color = "transparent"
  ) +
  
  geom_sf(aes(fill = factor(horario_19)),
          color = NA) +
  
  # DST boundary
  geom_sf(data = dst_contour,
          color = "red",
          fill = NA,
          linewidth = 2.0) +
  
  scale_fill_manual(
    name = "Groups",
    values = c(
      "11h" = "#0072B2",
      "12h" = "#E69F00",     
      "13h" = "#009E73"      
    ),
    drop = FALSE
  ) +
  coord_sf(
    xlim = c(-77, -30),
    ylim = c(-36, 7),
    expand = FALSE
  ) +
  theme_bw()+
  guides(
    fill = guide_legend(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,              # force single row
      byrow = TRUE
    )
  )  +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.spacing.x = unit(0.25, "cm"),
    legend.title = element_text(size = 15),
    legend.text  = element_text(size = 13),
    plot.margin = margin(4, 4, 4, 4)
  )

map

ggsave(filename = paste0(file.path(controls_maps_path, "map_band_h19v2.png")),plot = map,device = "png", width = 6.5, height = 5.2, units = "in", dpi = 300, bg = "white")


clear_workspace()
gc()
