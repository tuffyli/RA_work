# ---------------------------------------------------------------------------- #
# Matching
# Last edited by: Tuffy Licciardi Issa
# Date: 15/01/2026
# ---------------------------------------------------------------------------- #

#'In this code I will create a [propensity score matching] between municipalities
#'with a similar school structure. I will create, firtly, a more general one, then
#'I will apply a filter to remove the oulier municipalities and do the match.


# ---------------------------------------------------------------------------- #
# Libraries -----
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(readxl)
library(writexl)
library(lmtest)
library(fixest)
library(MatchIt)
library(xtable)
library(data.table)
library(stargazer)
library(AER)
library(sf)
library(janitor)
library(geobr)
library(RColorBrewer)
library(ggnewscale)
library(cobalt)
library(did)
library(MatchIt)
library(fastDummies)
library(broom)
library(rdrobust)
library(knitr)
library(kableExtra)
library(scales)
library(httr2)
library(glue)
library(ggbreak)
library(ggtext)
library(jsonlite)
library(haven)
library(ggplot2)
library(grid)
library(patchwork)



# ---------------------------------------------------------------------------- #
# 1. Data ----
# ---------------------------------------------------------------------------- #


#Starting with the scholl data
df_school <- readRDS("Z:/Tuffy/Paper - Educ/Dados/censo_escolar_base_v2.rds") %>% 
  mutate(codmun = as.character(codmun %/% 10),
         new_psc = ifelse(pre_tot > 0, 1, 0),
         new_day = ifelse(day_tot > 0, 1, 0)
  )

#We will calculate the municipal exposure, weighted by the enrollment numbers
df_school <- df_school %>% 
  group_by(codmun, ano) %>% 
  summarise(
    total_enroll = sum(enroll, na.rm = T),
    total_presch = sum(pre_tot, na.rm = T),
    total_daycar = sum(day_tot, na.rm = T),
    
    #numeral variables
    class     = total_enroll / sum(classroom, na.rm = T),
    n_employee = sum(employee, na.rm = T),                 #New contracts
    employee  = total_enroll / n_employee,
    schools   = n(),
    
    
    #School characteristics
    exp_troom = sum(enroll*teach_room, na.rm = T)/total_enroll,
    exp_lab   = sum(enroll*lab_dummy, na.rm = T) /total_enroll,
    exp_lib   = sum(enroll*lib_dummy, na.rm = T) /total_enroll,
    exp_play  = sum(enroll*play_area, na.rm = T) /total_enroll,
    exp_lunch = sum(enroll*lunch, na.rm = T)     /total_enroll,
    
    #Courses Student Exposure
    exp_psch  = sum((pre_tot + day_tot)*kinder, na.rm = T)    /total_enroll,
    exp_elem  = sum(ef_tot*elementary, na.rm = T)/total_enroll,
    exp_high  = sum(em_tot*high, na.rm = T)      /total_enroll,
    exp_inc   = sum(esp_tot*inclusion, na.rm = T) /total_enroll,
    
    #Childhood exp
    child_psch = sum(pre_tot*preschool, na.rm = T),
    child_dayc = sum(day_tot*daycare, na.rm = T),
    
    new_child_psch = sum(pre_tot, na.rm = T),
    new_child_dayc = sum(day_tot, na.rm = T),
    
    #Numb. Schools
    n_daycare = sum(daycare, na.rm = T),
    n_preschl = sum(preschool, na.rm = T),
    
    new_n_daycare = sum(new_day, na.rm = T),
    new_n_preschl = sum(new_psc, na.rm = T),
    
    .groups = "drop"
  ) %>% 
  mutate(k = ano - 2007)


#Main regression dataframe
df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf_flags.rds") %>% 
  select(codigo_ibge, ano, uf, nome, dosage, aluno_dosage, PIBpc,
         des_edu_pc, des_fund_pc, des_med_pc, des_inf_pc) %>% 
  mutate(across(c(codigo_ibge, uf), as.character))


#Combining both databases
df_comb <- df_school %>% 
  left_join(df_reg %>%
              filter(ano %in% c(2005:2018)) %>%
              mutate(codigo_ibge = as.character(codigo_ibge)),
            by = c("codmun" = "codigo_ibge", "ano" = "ano")) %>% 
  
  #Separating for Winner and Loser
  mutate(win_status = case_when(
    dosage > 0 ~ 1, #Winner
    dosage < 0 ~ 0, #Loser
    TRUE ~ NA
  )) %>% 
  
  mutate(uf = as.numeric(codmun) %/% 10000,
         uf = as.factor(uf))

# ---------------------------------------------------------------------------- #
# 2. Matching ----
# ---------------------------------------------------------------------------- #


mout <- matchit( win_status ~ uf + schools + exp_troom + exp_lab + exp_lib + class +
                   exp_play + exp_lunch + new_child_psch + new_child_dayc + PIBpc,
                 data = df_comb %>% filter(ano == 2007 & !is.na(win_status)),
                 method = "nearest",
                 distance = "glm")

mout

summary(mout)

plot(mout, type = "density", interactive = FALSE,
     which.xs = ~schools + PIBpc + new_child_psch)

plot(summary(mout))

# 
# 
# 
# 
# library(cobalt)
# library(ggplot2)
# 
# # 1. build bal.tab (explicit)
# bt <- bal.tab(mout, un = TRUE)   # un=TRUE ensures unadjusted stats are present
# 
# # 2. inspect what variables bal.tab actually has (these are the names love.plot will use)
# vars_bt <- rownames(bt$Balance)
# head(vars_bt, 40)                # show the first 40 names so you can see the pattern
# length(vars_bt)                  # how many rows / covariates
# 
# # 3. check available diff columns (Unadjusted and Adjusted)
# colnames(bt$Balance)
# # and quick peek of diffs for first rows
# bt$Balance[1:10, c("Diff.Un", "Diff.Adj")]   # may be named slightly different depending on package version
# 
# 
# 
# 
# # robustly remove uf dummies from bal.tab rownames
# vars_keep <- vars_bt[ !grepl("uf\\|(^uf\\b)|\\buf[[:punct:]]?\\d+$", vars_bt) ]
# 
# # if you are unsure which were removed, show them:
# vars_removed <- setdiff(vars_bt, vars_keep)
# vars_removed
# 
# # optionally manually inspect vars_keep
# vars_keep
# 
# 
# 
# 
# # robustly remove uf dummies from bal.tab rownames
# vars_keep <- vars_bt[ !grepl("uf\\|(^uf\\b)|\\buf[[:punct:]]?\\d+$", vars_bt) ]
# 
# # if you are unsure which were removed, show them:
# vars_removed <- setdiff(vars_bt, vars_keep)
# vars_removed
# 
# # optionally manually inspect vars_keep
# vars_keep
# 
# 
# p <- love.plot(
#   bt,                          # use bal.tab object
#   stats = "mean.diffs",        # exact allowed string
#   variables = vars_keep,       # variables from bal.tab with uf dummies removed
#   abs = TRUE,                  # absolute standardized differences
#   sample = "both",             # plot both "All" and "Matched" points
#   var.order = "unadjusted",    # order by unadjusted diffs
#   shapes = c(1, 19),           # hollow = all, filled = matched
#   colors = c("gray60", "black")
# )
# 
# # Cosmetic tweaks (ggplot object)
# p <- p +
#   theme_minimal(base_size = 14) +
#   labs(x = "Absolute Standardized Mean Difference",
#        title = "Covariate balance: Before and After Matching") +
#   theme(legend.position = "bottom",
#         panel.grid.minor = element_blank(),
#         axis.text.y = element_text(size = 10))
# 
# print(p)
# 
# 
# 
# 
# library(cobalt)
# library(ggplot2)
# 
# # ensure bal.tab and plot exist (recreate if needed)
# bt <- bal.tab(mout, un = TRUE)
# 
# vars_bt <- rownames(bt$Balance)
# vars_keep <- vars_bt   # or filter if you want
# 
# p <- love.plot(
#   bt,
#   thresholds = 0.1,
#   stats = "mean.diffs",
#   variables = vars_keep,
#   abs = TRUE,
#   sample = "both",
#   var.order = "unadjusted",
#   shapes = c(1, 19),
#   colors = c("gray60", "black"),
#   line = TRUE
# )
# 
# # build and inspect internal data frames produced by the plot
# gb <- ggplot_build(p)
# 
# # print column names for each data frame so you can see what's inside
# cat("Layers produced by love.plot() and their column names:\n")
# lapply(seq_along(gb$data), function(i) {
#   nm <- names(gb$data[[i]])
#   cat(sprintf("Layer %d: %s\n", i, paste(nm, collapse = ", ")))
#   invisible(NULL)
# })
# 
# # find first layer that has both 'x' and 'y' columns
# layer_with_xy <- which(sapply(gb$data, function(d) all(c("x","y") %in% names(d))))
# cat("Layers that contain both 'x' and 'y':", paste(layer_with_xy, collapse = ", "), "\n")
# 
# # If none found, try to find a layer with 'x' and 'y' equivalents (like 'xmin/xmax' or 'y' & 'yend')
# if (length(layer_with_xy) == 0) {
#   # try to find numeric columns that look like coordinates
#   possible <- which(sapply(gb$data, function(d) {
#     nms <- names(d)
#     any(c("x", "xmin", "xend", "xmiddle") %in% nms) && any(c("y","ymin","yend") %in% nms)
#   }))
#   cat("Fallback candidate layers (with xmin/xmax or ymin/ymax):", paste(possible, collapse = ", "), "\n")
#   layer_with_xy <- possible
# }
# 
# # Add larger points from the first candidate layer that seems right
# if (length(layer_with_xy) >= 1) {
#   i <- layer_with_xy[1]
#   df_pts <- gb$data[[i]]
#   
#   # determine actual x and y column names to use
#   xcol <- if ("x" %in% names(df_pts)) "x" else if ("xmin" %in% names(df_pts) && "xmax" %in% names(df_pts)) {
#     # compute mid-point between xmin/xmax
#     df_pts$x <- (df_pts$xmin + df_pts$xmax)/2
#     "x"
#   } else if ("xend" %in% names(df_pts)) {
#     df_pts$x <- df_pts$xend; "x"
#   } else stop("Could not find an x coordinate column in the candidate layer.")
#   
#   ycol <- if ("y" %in% names(df_pts)) "y" else if ("ymin" %in% names(df_pts) && "ymax" %in% names(df_pts)) {
#     df_pts$y <- (df_pts$ymin + df_pts$ymax)/2
#     "y"
#   } else stop("Could not find a y coordinate column in the candidate layer.")
#   
#   # If the layer has a 'group' column, keep it; otherwise create a fallback group
#   if (!("group" %in% names(df_pts))) df_pts$group <- seq_len(nrow(df_pts))
#   
#   # Now add enlarged points using the exact column names
#   p2 <- p +
#     geom_point(
#       data = df_pts,
#       mapping = aes_string(x = xcol, y = ycol, shape = "factor(group)", color = "factor(group)"),
#       size = 3.2,
#       stroke = 0.9,
#       inherit.aes = FALSE
#     )
#   
#   # print and save
#   print(p2)
#   ggsave("balance_plot_pretty_points.png", p2, width = 10, height = 8, dpi = 300)
#   cat("Added enlarged points from layer", i, "and saved 'balance_plot_pretty_points.png'.\n")
# } else {
#   cat("No suitable layer with x/y coordinates found; returning original plot.\n")
#   print(p)
# }
# 



# ---------------------------------------------------------------------------- #
# 3. Plot ----
# ---------------------------------------------------------------------------- #

# Love plot (INLINE)
custom_labels <- c(
  distance                  = "Distance",
  uf                        = "UF",
  schools                   = "Schools",
  exp_troom                 = "Exp. Teacher Room",
  exp_lab                   = "Exp. Labs",
  exp_lib                   = "Exp. Library",
  exp_lunch                 = "Exp. Lunch",
  exp_play                  = "Exp. Play Area",
  new_child_psch            = "Children in Preschool",
  new_child_dayc            = "Children in Daycare",
  class                     = "Class",
  uf_11                     = "UF-RO",  # Rondônia
  uf_12                     = "UF-AC",  # Acre
  uf_13                     = "UF-AM",  # Amazonas
  uf_14                     = "UF-RR",  # Roraima
  uf_15                     = "UF-PA",  # Pará
  uf_16                     = "UF-AP",  # Amapá
  uf_17                     = "UF-TO",  # Tocantins
  
  uf_21                     = "UF-MA",  # Maranhão
  uf_22                     = "UF-PI",  # Piauí
  uf_23                     = "UF-CE",  # Ceará
  uf_24                     = "UF-RN",  # Rio Grande do Norte
  uf_25                     = "UF-PB",  # Paraíba
  uf_26                     = "UF-PE",  # Pernambuco
  uf_27                     = "UF-AL",  # Alagoas
  uf_28                     = "UF-SE",  # Sergipe
  uf_29                     = "UF-BA",  # Bahia
  
  uf_31                     = "UF-MG",  # Minas Gerais
  uf_32                     = "UF-ES",  # Espírito Santo
  uf_33                     = "UF-RJ",  # Rio de Janeiro
  uf_35                     = "UF-SP",  # São Paulo
  
  uf_41                     = "UF-PR",  # Paraná
  uf_42                     = "UF-SC",  # Santa Catarina
  uf_43                     = "UF-RS",  # Rio Grande do Sul
  
  uf_50                     = "UF-MS",  # Mato Grosso do Sul
  uf_51                     = "UF-MT",  # Mato Grosso
  uf_52                     = "UF-GO",  # Goiás
  uf_53                     = "UF-DF"   # Distrito Federal
  
)

love <- love.plot(
  mout,
  stats         = "mean.diffs",
  abs           = TRUE,
  threshold     = 0.10,
  colors        = c("black", "red"),
  line          = TRUE,
  var.names     = custom_labels,
  #sample        = "both",
  var.order     = "unadjusted"
  #drop.distance = TRUE,
  #var_distance  = "decreasing"
) +
  labs(
    title = paste0("Covs. Adjustment: ")
  ) +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

love

# ---------------------------------------------------------------------------- #
# 4. Regression ----
# ---------------------------------------------------------------------------- #
## 4.1 Main -----
# --------------------------------------------------------------------------- -#

#Extracting the matched municipalities
matched <- match.data(mout) %>% 
  select(codmun, distance)


#Opening the complete data
data <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf_flags.rds")



df_spend <- data %>% 
  filter(tipo == "Municipal") %>% 
  mutate(dif_rs_aluno_100 = dif_rs_aluno / 100,
         region = case_when(
           as.numeric(codigo_ibge) %/% 100000 == 1 ~ "Norte",        #North
           as.numeric(codigo_ibge) %/% 100000 == 2 ~ "Nordeste",     #Northeast
           as.numeric(codigo_ibge) %/% 100000 == 3 ~ "Sudeste",      #Southeast
           as.numeric(codigo_ibge) %/% 100000 == 4 ~ "Sul",          #South
           as.numeric(codigo_ibge) %/% 100000 == 5 ~ "Centro-Oeste", #Central-West
           TRUE ~ NA
         )
  ) %>%
  #Groups
  mutate(grupo = case_when(
    dosage > 0 ~ "Winner",   # net beneficiary
    dosage < 0 ~ "Loser",   # net contributer
    TRUE ~ NA_character_
  ),
  
  net_fundeb = (receita_real - receita_simulada)/real_des_edu[ano == 2007],
  
  var_rob2  = (receita_real - receita_simulada)/real_des_edu[ano == 2006]
  
  ) %>% 
  filter(tipo == "Municipal") %>% 
  #Manually creating dummy values
  mutate(d05 = ifelse(ano == 2005, 1, 0),
         d06 = ifelse(ano == 2006, 1, 0),
         d07 = ifelse(ano == 2007, 1, 0),
         d08 = ifelse(ano == 2008, 1, 0),
         d09 = ifelse(ano == 2009, 1, 0),
         d10 = ifelse(ano == 2010, 1, 0),
         d11 = ifelse(ano == 2011, 1, 0),
         d12 = ifelse(ano == 2012, 1, 0),
         d13 = ifelse(ano == 2013, 1, 0),
         d14 = ifelse(ano == 2014, 1, 0),
         d15 = ifelse(ano == 2015, 1, 0),
         d16 = ifelse(ano == 2016, 1, 0),
         d17 = ifelse(ano == 2017, 1, 0),
         d18 = ifelse(ano == 2018, 1, 0),
         d19 = ifelse(ano == 2019, 1, 0),
         d20 = ifelse(ano == 2020, 1, 0),
         d21 = ifelse(ano == 2021, 1, 0)) %>% 
  filter(codigo_ibge %in% matched$codmun)



# ---------------------------------------------------------------------------- #
### 4.1.1 Regression -----
# ---------------------------------------------------------------------------- #

est_10 <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_spend %>% group_by (codigo_ibge) %>% 
                  #filter(ano < 2011) %>% 
                  #filter(all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                  ungroup(),
                vcov = "hetero")

est_prof <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_spend %>% 
                    #filter(ano < 2011) %>% 
                    filter(dosage == 1 | flag_spend70 == 0 & flag_spendm20 == 0),
                  vcov = "hetero")

etable(est_10, est_prof
)


# ---------------------------------------------------------------------------- #
## 4.2 SAEB ----
# ---------------------------------------------------------------------------- #

#Saeb dataframe
df_saeb <- readRDS( "Z:/Tuffy/Paper - Educ/Dados/saeb_nvl_aluno.rds") %>%
  mutate(codmun = ifelse(ano >= 2007, as.numeric(codmun) %/% 10, codmun),
         codmun = as.character(codmun)) %>%   #Arranging for the older municipal codes
  filter(codmun < 600000)



#Main regression dataframe
df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") %>% 
  select(codigo_ibge, ano, uf, nome, dif_coef_pp, dosage, aluno_dosage, PIBpc) %>% 
  mutate(across(c(codigo_ibge, uf), as.character))

# ---------------------------------------------------------------------------- #
### 4.2.1 Regression ----
# ---------------------------------------------------------------------------- #

#Combining both dataframes
df <- df_saeb %>% 
  left_join(df_reg %>%
              mutate(codigo_ibge = as.character(codigo_ibge)),
            by = c("codmun" = "codigo_ibge", "ano" = "ano")) %>% 
  mutate(
    sexo = as.numeric(sexo),
    raca = as.numeric(raca),
    mae_educ = as.numeric(mae_educ)
  ) %>% 
  mutate(age_exp = ifelse(treat_exp > 1, 1, treat_exp), #Based on Carrillo
         
         anos_exp = ano - 2007,
         anos_exp = ifelse(grade == 9, anos_exp - 2, anos_exp), #Adjsutment for the 9th grade
         
         ano_nasc = ano - (idade), #Birth year as reference
         
         peso = as.numeric(peso), #Weights
         
         age_late = case_when(
           grade == 5 & idade > 10 ~ 1, #Is older than the ideal age
           grade == 9 & idade > 14 ~ 1,  #Is older than the ideal age
           TRUE ~ 0
         ),
         age_dist = ifelse(idade_aux == 0, 1, 0), #Age distortion
         
         post_treat = ifelse(ano > 2007, 1, 0),
         
         nasc_post  = ifelse(ano_nasc >= 2002, 1, 0),
         
         rob_winner_dummy = ifelse(dosage > 0, 1, 0),
         
         treatment_expo = case_when( #Treatment exposition
           ano_nasc <= 2000 ~ 0,
           ano_nasc == 2001 ~ 1,
           ano_nasc == 2002 ~ 2,
           ano_nasc >= 2003 ~ 3,
           TRUE ~ NA
         ),
         
         #Separating the Brazilian Regions
         region = case_when(
           as.numeric(codmun) %/% 100000 == 1 ~ "Norte",        #North
           as.numeric(codmun) %/% 100000 == 2 ~ "Nordeste",     #Northeast
           as.numeric(codmun) %/% 100000 == 3 ~ "Sudeste",      #Southeast
           as.numeric(codmun) %/% 100000 == 4 ~ "Sul",          #South
           as.numeric(codmun) %/% 100000 == 5 ~ "Centro-Oeste", #Central-West
           TRUE ~ NA
         )
  ) %>% 
  mutate(grupo = case_when(
    aluno_dosage > 0 ~ "Winner",   # net beneficiary
    aluno_dosage < 0 ~ "Loser",   # net contributer
    TRUE ~ NA_character_),
    
    grupo = factor(grupo, levels = c("Loser", "Winner")) #Beneficiary dumm
  ) %>% 
  filter(as.numeric(ano_nasc) < 2008) %>% # This remove younger than expected people in 2017  -> ideal age = 10.
  mutate(ano_nasc = as.factor(ano_nasc)) %>% 
  filter(!is.na(uf)) %>% 
  filter(codmun %in% matched$codmun)



#Creating a dataframe for each school
df_mun_school <- df %>% 
  mutate(uf = as.numeric(codmun) %/% 10000) %>% 
  #Grouping by School
  group_by(codmun, cod_escola, grupo, ano, id_uf) %>% 
  summarise(students = n(),
            students_peso = sum(peso, na.rm = T),
            students_mpes = sum(peso_mt, na.rm = T),
            students_ppes = sum(peso_lp, na.rm= T),
            .groups = "drop") %>% 
  arrange(codmun, ano, id_uf) %>% 
  mutate_all(as.numeric) %>% 
  #Grouping by Municipality
  group_by(codmun, ano, grupo, id_uf) %>% 
  summarise(total_students = sum(students),
            total_students_peso = sum(students_peso),
            total_students_mpes = sum(students_mpes), #using the math weight as reference
            total_students_ppes = sum(students_ppes),
            schools = n(),
            .groups = "drop") %>% 
  arrange(codmun, ano, id_uf) %>% 
  #Filtering municipalities that appear every year
  group_by(codmun, ano) %>% 
  mutate(aux1 = 1) %>% 
  ungroup(ano) %>% 
  mutate(aux2 = sum(aux1, na.rm = T)) %>% 
  filter(aux2 == 7) %>% #Appears in every year
  select(-c(aux1, aux2)) 



#Filters
filter_saeb <- list()
filter_saeb[["in_saeb"]] <- as.character(unique(df_mun_school$codmun)) #List of municipalities present in all SAEB



#Weights
df_w5 <- df %>% filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]) %>% select(peso, peso_mt, peso_lp)  #5th grade




main_mat_f <- feols(as.numeric(profic_mat) ~ aluno_dosage : i(ano, grupo, ref = 2007)
                    + PIBpc #Controls
                    | codmun + ano + uf^ano, #FE
                    data = df %>% filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]), #Only 5h grade
                    weights = df_w5$peso_mt,
                    vcov = "hetero")

main_pot_f <- feols(as.numeric(profic_port) ~ aluno_dosage : i(ano, grupo, ref = 2007)
                    + PIBpc #Controls
                    | codmun + ano + uf^ano, #FE
                    data = df %>% filter(grade == 5 & codmun %in% filter_saeb[["in_saeb"]]), #Only 5h grade
                    weights = df_w5$peso_lp,
                    vcov = "hetero")


etable(main_mat_f, main_pot_f)

