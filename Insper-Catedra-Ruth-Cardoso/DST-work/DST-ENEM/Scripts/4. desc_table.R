# ---------------------------------------------------------------------------- #
# Data description
# Last edited by: Tuffy Licciardi Issa
# Date: 09/03/2026
# ---------------------------------------------------------------------------- #
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
# 1. Main Data and Description ------
# ---------------------------------------------------------------------------- #
base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2019.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2018.RDS"))) %>%
  setDT() %>% 
  filter(conclusao == 2)


#Temperature data
inpe_18 <- readRDS("Z:/Tuffy/Paper - HV/Bases/inpe/mun/inpe_mun_2018.rds") %>% 
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
  ) %>% 
  mutate(ano = 2018)

inpe_19 <- readRDS("Z:/Tuffy/Paper - HV/Bases/inpe/mun/inpe_mun_2019.rds") %>% 
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
  ) %>% 
  mutate(ano = 2019)


inpe <- inpe_18 %>% bind_rows(inpe_19)

base <- base %>% 
  left_join(inpe %>% mutate(codmun = as.integer(codmun)), by = c("mun_prova" = "codmun", "ano"))


rm(inpe_18, inpe_19, inpe)


# Creating the variables
base <- base %>% 
  mutate(
    
    escm = case_when(
      esc_mae %in% c("D","E","F") ~ 1, #With high school
      esc_mae %in% c("A","B","C") ~ 0,
      .default = NA),
    
    escp = case_when(
      esc_pai %in% c("D","E","F") ~ 1,
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
  ) %>% 
  filter(conclusao == 2)

#Absence database
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
  "escm",
  "pai_trab_man",
  "mae_trab_man",
  "dom5",
  "renda1",
  "pibpc",
  "id18",
  "temp_d1",
  "temp_d2",
  "umid_d1",
  "umid_d2"
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

# ---------------------------------------------------------------------------- #
## 1.1 Values -----
# ---------------------------------------------------------------------------- #


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
    format(x = medias1[12:20], digits = 2, scientific = F),
    format(x = medias1[21], digits = 1, scientific = F, big.mark = ","),
    format(x = medias1[22], digits = 2, scientific = F),
    format(x = medias1[23:26], digits = 1, scientific = F)
  ),
  dps1 = c(
    format(x = dps1[1:8], digits = 1, scientific = F),
    format(x = dps1[9:10], digits = 1, scientific = F),
    format(x = dps1[11], digits = 1, scientific = F),
    format(x = dps1[12:20], digits = 2, scientific = F),
    format(x = dps1[21], digits = 1, scientific = F, big.mark = ","),
    format(x = dps1[22], digits = 2, scientific = F),
    format(x = dps1[23:26], digits = 1, scientific = F)
  ),
  obs1 = format(x = obs1[1:26], digits = 1, scientific = F, big.mark = ","),
  
  medias0 = c(
    format(x = medias0[1:8], digits = 1, scientific = F),
    format(x = medias0[9:10], digits = 2, scientific = F),
    format(x = medias0[11], digits = 1, scientific = F),
    format(x = medias0[12:20], digits = 2, scientific = F),
    format(x = medias0[21], digits = 1, scientific = F, big.mark = ","),
    format(x = medias0[22], digits = 2, scientific = F),
    format(x = medias0[23:26], digits = 1, scientific = F)
  ),
  dps0 = c(
    format(x = dps0[1:8], digits = 1, scientific = F),
    format(x = dps0[9:10], digits = 1, scientific = F),
    format(x = dps0[11], digits = 1, scientific = F),
    format(x = dps0[12:20], digits = 2, scientific = F),
    format(x = dps0[21], digits = 1, scientific = F, big.mark = ","),
    format(x = dps0[22], digits = 2, scientific = F),
    format(x = dps0[23:26], digits = 1, scientific = F)
  ),
  obs0 = format(x = obs0[1:26], digits = 1, scientific = F, big.mark = ",")
)





rm(medias0,medias1,dps0,dps1,obs0,obs1,base_abs,base_ag, base)

row.names(medias) <- c(
  "ENEM Avg. Score", 
  "Natural Sciences", "Human Sciences", "Language", "Mathematics", "Composition",
  "Day 1", "Day 2", 
  "Pr. Right Answers",
  "Absence",
  "Distance to DST border (km)",
  "Age", "Female", "African Brazilian or native",
  "Father with high school", "Mother with high school",
  "Father in manual labor", "Mother in manual labor",
  "Household with 5 or more people",
  "Household income up to 1 MW", 
  "Municipal per capita GDP",
  "18 years-old",
  "Temperature - Day 1",
  "Temperature - Day 2",
  "Humidity - Day 1",
  "Humidity - Day 2"
  
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
  file = "Z:/Tuffy/Paper - HV/Resultados/definitive/desc_table_v2.tex",
  sanitize.colnames.function = function(x) {
    x
  },
  only.contents = T
)


rm(list = ls())
gc()


# ---------------------------------------------------------------------------- #
# 2. Migration ----
# ---------------------------------------------------------------------------- #



base <- readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2019.RDS")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HV/Bases/No_age_filt/base_nota_2018.RDS"))) %>%
  setDT()

#Mun. with enem
mun_list19 <- unique(base$mun_prova[base$ano == 2019])
mun_list18 <- unique(base$mun_prova[base$ano == 2018])

base <- base %>%
  mutate(
    old = ifelse(
      idade > 18 & conclusao == 2, 1,
      ifelse( idade %in% c(17, 18), 0, NA))
  ) %>% 
  mutate(
    mig_dummy = ifelse(
      !is.na(mun_prova) & !is.na(mun_res) & mun_prova != mun_res, 1,
      ifelse(!is.na(mun_prova) & !is.na(mun_res), 0 , NA)
    ),
    
    school_no_enem = case_when(
      ano == 2019 & !is.na(mun_escola) & !mun_escola %in% mun_list19 ~ 1,
      ano == 2018 & !is.na(mun_escola) & !mun_escola %in% mun_list18 ~ 1,
      TRUE ~ 0
    ),
    
    school_wt_enem = case_when(
      ano == 2019 & !is.na(mun_escola) & mun_escola %in% mun_list19 ~ 1,
      ano == 2018 & !is.na(mun_escola) & mun_escola %in% mun_list18 ~ 1,
      TRUE ~ 0
    )
  ) %>% filter(conclusao == 2)


# ---------------------------------------------------------------------------- #
##2.1 Desc Table ----
# ---------------------------------------------------------------------------- #

base_desc <- base %>% 
  filter(priv0 == 1) %>% 
  group_by(ano) %>% 
  rename(Year = ano) %>% 
  summarise(
    `Total Students` = n(),
    `School Mun. with ENEM` = sum(school_wt_enem, na.rm = T),
    `School Mun. without ENEM` = sum(school_no_enem, na.rm = T),
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
## 2.2 Saving ----
# ---------------------------------------------------------------------------- #

latex_table <- knitr::kable(
  df_tidy,
  format = "latex",
  booktabs = TRUE,
  align = "lcc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HV/Resultados/definitive/migration_desc.tex")

# ---------------------------------------------------------------------------- #
# 3. Base Desc ----
# ---------------------------------------------------------------------------- #
## 3.1 (2019) ----
# ---------------------------------------------------------------------------- #
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

# ---------------------------------------------------------------------------- #
##3.2 (2018) ----
# ---------------------------------------------------------------------------- #
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

