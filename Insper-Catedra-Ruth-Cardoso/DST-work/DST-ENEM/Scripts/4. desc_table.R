# ---------------------------------------------------------------------------- #
# Descrição da base de dados + Anexo
# Library -----
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

# ---------------------------------------------------------------------------- #
# Base e Descrição ------
# ---------------------------------------------------------------------------- #
base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2019.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2018.RDS"))) %>%
  setDT() %>% 
  filter(conclusao == 2)

base_abs <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_abs_",2018,".RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_abs_",2019,".RDS"))) %>%
  select(id_enem,priv, hv, ano, abs) %>% 
  setDT()

# Lista de variáveis
vlist <- c(
  "media",
  "cn",
  "ch",
  "lc",
  "mt",
  "rd",
  "dia_1",
  "dia_2",
  "acerto",
  "abs",
  "dist_hv_pos",
  "idade",
  "fem",
  "ppi",
  "escp",
  "dom5",
  "renda1",
  "pibpc"
)

base_nota <- base %>%
  mutate(
    dist_hv_pos = abs(dist_hv_border)/1000,
    merge = 1
  ) %>% 
  merge(y = base_abs, by = c("id_enem", "ano"), all.y = T) %>%
  mutate(
    priv = ifelse(merge == 1, priv.x, NA),
    priv = ifelse(is.na(merge), priv.y, priv),
    hv = ifelse(merge == 1, hv.x, NA),
    hv = ifelse(is.na(merge), hv.y, hv)
  ) %>%
  select(-priv.x,priv.y,hv.x,hv.y) %>%
  setDT() 


mun_presente_ambos_anos <- base_nota %>%
  distinct(mun_prova, ano) %>%
  count(mun_prova) %>%
  filter(n == 2) %>%
  pull(mun_prova)


base_nota <- base_nota %>%
  filter(mun_prova %in% mun_presente_ambos_anos) %>% 
  setDT()

rm(mun_presente_ambos_anos)

summary(base_nota$abs)

# Valores -----

# base_nota <- base_nota[priv0 == 1,
#                 c(
#                   .(obs = .N),  # conta de observações
#                   lapply(.SD, mean, na.rm = TRUE)  # média das variáveis de interesse
#                 ),
#                 .SDcols = vlist,
#                 by = .(mun_prova, ano, dist_hv_border, seg, lat, lon, hv, priv)
# ]

# medias0 <- base_nota[priv == 0 & hv == 0,
#                 lapply(.SD, mean
#                        #weighted.mean, w = obs
#                        , na.rm = T),
#                 .SDcols = vlist] %>%
#   as.numeric()
# 
# 
# medias1 <- base_nota[priv == 0 & hv == 1,
#                      lapply(.SD, mean
#                             #weighted.mean,
#                             na.rm = T),
#                      .SDcols = vlist] %>%
#   as.numeric()
# 
# dps0 <- base_nota[priv == 0 & hv == 0,
#                   lapply(.SD, sd, na.rm = T),
#                   .SDcols = vlist] %>%
#   as.numeric()
# 
# dps1 <- base_nota[priv == 0 & hv == 1,
#                   lapply(.SD, sd, na.rm = T),
#                   .SDcols = vlist] %>%
#   as.numeric()
# 
# 
# obs0 <- base[priv == 0 & hv == 0,
#                  lapply(.SD, function(x) sum(!is.na(x))),
#                  .SDcols = vlist] %>% 
#   as.numeric()
# 
# obs1 <- base[priv == 0 & hv == 1,
#                  lapply(.SD, function(x) sum(!is.na(x))),
#                  .SDcols = vlist] %>% 
#   as.numeric()


medias0 <- base_nota[priv == 0 & hv == 0,
                     lapply(.SD, mean, na.rm = T),
                     .SDcols = vlist] %>%
  as.numeric()

medias1 <- base_nota[priv == 0 & hv == 1,
                     lapply(.SD, mean, na.rm = T),
                     .SDcols = vlist] %>%
  as.numeric()

dps0 <- base_nota[priv == 0 & hv == 0,
                  lapply(.SD, sd, na.rm = T),
                  .SDcols = vlist] %>%
  as.numeric()

dps1 <- base_nota[priv == 0 & hv == 1,
                  lapply(.SD, sd, na.rm = T),
                  .SDcols = vlist] %>%
  as.numeric()


obs0 <- base_nota[priv == 0 & hv == 0,
                  lapply(.SD, FUN = function(x) {sum(!is.na(x))}),
                  .SDcols = vlist] %>%
  as.numeric()

obs1 <- base_nota[priv == 0 & hv == 1,
                  lapply(.SD, FUN = function(x) {sum(!is.na(x))}),
                  .SDcols = vlist] %>%
  as.numeric()

medias <- data.frame(
  medias1 = c(
    format(x = medias1[1:8], digits = 1, scientific = F),
    format(x = medias1[9:10], digits = 2, scientific = F),
    format(x = medias1[11], digits = 1, scientific = F),
    format(x = medias1[12:17], digits = 2, scientific = F),
    format(x = medias1[18], digits = 1, scientific = F, big.mark = ",")
  ),
  dps1 = c(
    format(x = dps1[1:8], digits = 1, scientific = F),
    format(x = dps1[9:10], digits = 1, scientific = F),
    format(x = dps1[11], digits = 1, scientific = F),
    format(x = dps1[12:17], digits = 2, scientific = F),
    format(x = dps1[18], digits = 1, scientific = F, big.mark = ",")
  ),
  obs1 = format(x = obs1[1:18], digits = 1, scientific = F, big.mark = ","),
  medias0 = c(
    format(x = medias0[1:8], digits = 1, scientific = F),
    format(x = medias0[9:10], digits = 2, scientific = F),
    format(x = medias0[11], digits = 1, scientific = F),
    format(x = medias0[12:17], digits = 2, scientific = F),
    format(x = medias0[18], digits = 1, scientific = F, big.mark = ",")
  ),
  dps0 = c(
    format(x = dps0[1:8], digits = 1, scientific = F),
    format(x = dps0[9:10], digits = 1, scientific = F),
    format(x = dps0[11], digits = 1, scientific = F),
    format(x = dps0[12:17], digits = 2, scientific = F),
    format(x = dps0[18], digits = 1, scientific = F, big.mark = ",")
  ),
  obs0 = format(x = obs0[1:18], digits = 1, scientific = F, big.mark = ",")
)




medias <- data.frame(
  medias1 = c(
    format(x = medias1[1:8], digits = 1, scientific = F),
    format(x = medias1[9:10], digits = 2, scientific = F),
    format(x = medias1[11], digits = 1, scientific = F),
    format(x = medias1[12:17], digits = 2, scientific = F),
    format(x = medias1[18], digits = 1, scientific = F, big.mark = ",")
  ),
  dps1 = c(
    format(x = dps1[1:8], digits = 1, scientific = F),
    format(x = dps1[9:10], digits = 1, scientific = F),
    format(x = dps1[11], digits = 1, scientific = F),
    format(x = dps1[12:17], digits = 2, scientific = F),
    format(x = dps1[18], digits = 1, scientific = F, big.mark = ",")
  ),
  obs1 = format(x = obs1[1:18], digits = 1, scientific = F, big.mark = ","),
  medias0 = c(
    format(x = medias0[1:8], digits = 1, scientific = F),
    format(x = medias0[9:10], digits = 2, scientific = F),
    format(x = medias0[11], digits = 1, scientific = F),
    format(x = medias0[12:17], digits = 2, scientific = F),
    format(x = medias0[18], digits = 1, scientific = F, big.mark = ",")
  ),
  dps0 = c(
    format(x = dps0[1:8], digits = 1, scientific = F),
    format(x = dps0[9:10], digits = 1, scientific = F),
    format(x = dps0[11], digits = 1, scientific = F),
    format(x = dps0[12:17], digits = 2, scientific = F),
    format(x = dps0[18], digits = 1, scientific = F, big.mark = ",")
  ),
  obs0 = format(x = obs0[1:18], digits = 1, scientific = F, big.mark = ",")
)



rm(medias0,medias1,dps0,dps1,obs0,obs1,base_abs,base_ag, base)

row.names(medias) <- c(
  "ENEM Avg. Score", 
  "Natural Sciences", "Human Sciences", "Language", "Mathematics", "Composition",
  "Day 1", "Day 2", 
  "Pr. Right Answers",
  "Absence",
  "Distance to DST border (km)",
  "Age", "Female", "African Brazilian or native", "Father completed middle school",
  "Big household", "Household income up to 1 MW", "Municipal per capita GDP"
)

colnames(medias) <- c(
  "Mean",
  "Std. Dev.",
  "Obs.",
  "Mean",
  "Std. Dev.",
  "Obs."
)


print.xtable(
  x = medias,
  include.rownames = T,
  include.colnames = T,
  file = "Z:/Tuffy/Paper - HV/Resultados/desc_table_v2.tex",
  sanitize.colnames.function = function(x) {
    x
  },
  only.contents = T
)


rm(list = ls())
gc()
# ---------------------------------------------------------------------------- #
# Migração ----
# ---------------------------------------------------------------------------- #



base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/Bases/No_age_filt/base_nota_2019.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/Bases/No_age_filt/base_nota_2018.RDS"))) %>%
  setDT()


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
  ) %>%
  mutate(
    old = ifelse(
      idade > 18 & conclusao == 2, 1,
      ifelse( idade %in% c(17, 18), 0, NA))
  ) %>% 
  mutate(
    mig_dummy = ifelse(
      !is.na(mun_prova) & !is.na(mun_res) & mun_prova != mun_res, 1,
      ifelse(!is.na(mun_prova) & !is.na(mun_res), 0 , NA)
    )
  ) %>% filter(conclusao == 2)


# ---------------------------------------------------------------------------- #
## Desc ----
# ---------------------------------------------------------------------------- #

base_desc <- base %>% 
  filter(priv0 == 1) %>% 
  group_by(ano) %>% 
  rename(Year = ano) %>% 
  summarise(
    `Total Students` = n(),
    Migration = sum(mig_dummy, na.rm = T),
    `Group 1 (None)` = sum(nonmig1, na.rm = T),
    `Group 2 (School)`  = sum(nonmig2, na.rm = T),
    `Group 3 (Exam)`  = sum(nonmig3, na.rm = T),
    `Group 4 (Residency)`  = sum(nonmig4, na.rm = T),
    
    .groups = "drop"
  )


#Transposing it
df_tidy <- base_desc %>%
  pivot_longer(-Year, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = Year, values_from = value) %>% 
  column_to_rownames("variable")



# ---------------------------------------------------------------------------- #
## Saving ----
# ---------------------------------------------------------------------------- #

latex_table <- knitr::kable(
  df_tidy,
  format = "latex",
  booktabs = TRUE,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/notas/migration_desc.tex")



