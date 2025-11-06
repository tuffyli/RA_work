# ---------------------------------------------------------------------------- #
# Robustness tests
# Last edited by: Tuffy Licciardi Issa
# Date: 05/11/2025
# ---------------------------------------------------------------------------- #

#' *************************************************************************** #
#' DISCLAIMER:
#' 
#' The databases I utilize in this code were all previously created in posterior
#' code scripts. 
#' 
#' Objective: In this coding session I seek to further breakdown the effects per-
#' ceived in the main regressions documents. Analysis over composition and other
#' factors were conducted aimed to uncover the pretrends results noted in the main
#' estimations.
#' *************************************************************************** #


# ---------------------------------------------------------------------------- #
# Libraries -----
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(readxl)
library(writexl)
library(lmtest)
library(fixest)
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
library(ggfixest)   # ggiplot
library(patchwork)

#Deactivating scientific notation
options(scipen = 999)

# ---------------------------------------------------------------------------- #
# 1. Enrollment ----
# ---------------------------------------------------------------------------- #

#'The first step is to breakdown the enrollment in each municipality. I believe
#'that the Bolsa Família program may be related to the noticed results in SAEB
#'for the year of 2007. Since its implementation in 2003, it is possible that the
#'net beneficiaries municipalities and net contributers may be associated with 
#'some variable related to the program exposure. It is important to highlight how 
#'it promotes the minimal presence in shcools

# ---------------- #
## 1.1 Data ----
# ---------------- #

df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") %>% 
  filter(tipo == "Municipal" &
           rede == "Pública")



summary(df_reg)

# --------------- #
## 1.2 Regression -----
# --------------- #

### 1.2.1 Dosage -----
est5d <- feols(peso_5 ~ dosage * i(k, ref = 0) +
                PIBpc |
                codigo_ibge + ano + uf^ano,
              data = df_reg,
              vcov = "hetero")


est9d <- feols(peso_9 ~ dosage * i(k, ref = 0) +
                PIBpc |
                codigo_ibge + ano + uf^ano,
              data = df_reg,
              vcov = "hetero")

etable(est5d, est9d)

#### 1.2.1.1 Graph ----

#Uniting both result tables into a single dataframe
df_es <- broom::tidy(est5d, conf.int = TRUE) %>%   #Extraction of the main variables and results
  slice((which(term == "PIBpc") + 1):n()) %>%      #Removing unwanted variables
  mutate(
    time_to_treat = str_extract(term, "(?<=k::)-?\\d+"),
    time_to_treat = as.numeric(time_to_treat),
    grupo = "5° Ano"                               #Group
    ) %>% 
  rbind(
    broom::tidy(est9d, conf.int = TRUE) %>% 
      slice((which(term == "PIBpc") + 1):n()) %>% 
      mutate(
        time_to_treat = str_extract(term, "(?<=k::)-?\\d+"),
        time_to_treat = as.numeric(time_to_treat),
        grupo = "9° Ano"
      ))

#Adding the values for the reference year for better plotting
temp_edu <- df_es %>%
  distinct(grupo) %>%             # one row per group (5° / 9°)
  mutate(
    term = "time_to_treat:0",     # or another label that fits your pattern
    estimate = 0,
    std.error = 0,
    statistic = 0,
    p.value = 1,
    conf.low = 0,
    conf.high = 0,
    time_to_treat = 0
  )

# combine with your main dataframe
df_es <- bind_rows(df_es, temp_edu) %>%
  arrange(grupo, time_to_treat) %>% 
  bind_rows(
    temp_edu %>%
      distinct(grupo) %>%
      mutate(
        time_to_treat = 0,
        estimate = 0,
        conf.low = 0,
        conf.high = 0
      )
  ) %>%
  arrange(grupo, time_to_treat)

rm(temp_edu)


#Plotting the final graph


p <- ggplot(df_es, aes(x = time_to_treat, y = estimate, group = grupo)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 0, color = "black") +
  geom_point(aes(color = grupo), shape = 15, size = 2) +
  geom_line(aes(color = grupo)) +
  #facet_wrap(~ grupo, ncol = 1) +     # <--- separate plot per group
  labs(x = "Time to Treat", y = "Matrículas",
       color = NULL, fill = NULL) +
  theme_classic() +
  theme(
    axis.line = element_line(color = "grey70"),
    panel.grid = element_blank(),
    axis.title = element_text(size = 11),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top") # anchor legend box
  ) + # anchor legend box
  scale_x_continuous(
    breaks = seq(min(df_es$time_to_treat, na.rm = TRUE),
                 max(df_es$time_to_treat, na.rm = TRUE),
                 by = 2)
  )

ggsave( #Saving image
  filename = paste0("matricula_dosage.png"),
  plot = p,
  path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
  width = 600/96, height = 420/96, dpi = 110
)


### 1.2.2 Aluno Dosage ----

est5d <- feols(peso_5 ~ aluno_dosage * i(k, ref = 0) +
                PIBpc |
                codigo_ibge + ano + uf^ano,
              data = df_reg,
              vcov = "hetero")


est9d <- feols(peso_9 ~ aluno_dosage * i(k, ref = 0) +
                PIBpc |
                codigo_ibge + ano + uf^ano,
              data = df_reg,
              vcov = "hetero")

etable(est5d, est9d)

#### 1.2.2.1  Graph ----

#Uniting both result tables into a single dataframe
df_es <- broom::tidy(est5d, conf.int = TRUE) %>%   #Extraction of the main variables and results
  slice((which(term == "PIBpc") + 1):n()) %>%      #Removing unwanted variables
  mutate(
    time_to_treat = str_extract(term, "(?<=k::)-?\\d+"),
    time_to_treat = as.numeric(time_to_treat),
    grupo = "5° Ano"                               #Group
  ) %>% 
  rbind(
    broom::tidy(est9d, conf.int = TRUE) %>% 
      slice((which(term == "PIBpc") + 1):n()) %>% 
      mutate(
        time_to_treat = str_extract(term, "(?<=k::)-?\\d+"),
        time_to_treat = as.numeric(time_to_treat),
        grupo = "9° Ano"
      ))

#Adding the values for the reference year for better plotting
temp_edu <- df_es %>%
  distinct(grupo) %>%             # one row per group (5° / 9°)
  mutate(
    term = "time_to_treat:0",     # or another label that fits your pattern
    estimate = 0,
    std.error = 0,
    statistic = 0,
    p.value = 1,
    conf.low = 0,
    conf.high = 0,
    time_to_treat = 0
  )

# combine with your main dataframe
df_es <- bind_rows(df_es, temp_edu) %>%
  arrange(grupo, time_to_treat) %>% 
  bind_rows(
    temp_edu %>%
      distinct(grupo) %>%
      mutate(
        time_to_treat = 0,
        estimate = 0,
        conf.low = 0,
        conf.high = 0
      )
  ) %>%
  arrange(grupo, time_to_treat)

rm(temp_edu)


#Plotting the final graph


p <- ggplot(df_es, aes(x = time_to_treat, y = estimate, group = grupo)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 0, color = "black") +
  geom_point(aes(color = grupo), shape = 15, size = 2) +
  geom_line(aes(color = grupo)) +
  #facet_wrap(~ grupo, ncol = 1) +     # <--- separate plot per group
  labs(x = "Time to Treat", y = "Matrículas",
       color = NULL, fill = NULL) +
  theme_classic() +
  theme(
    axis.line = element_line(color = "grey70"),
    panel.grid = element_blank(),
    axis.title = element_text(size = 11),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top") # anchor legend box
  ) + # anchor legend box
  scale_x_continuous(
    breaks = seq(min(df_es$time_to_treat, na.rm = TRUE),
                 max(df_es$time_to_treat, na.rm = TRUE),
                 by = 2)
  )

ggsave( #Saving image
  filename = paste0("matricula_aluno_dosage.png"),
  plot = p,
  path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
  width = 600/96, height = 420/96, dpi = 110
)

#Claeraing all
rm(list = ls())

# ---------------------------------------------------------------------------- #
# 2. School Characteristics ----
# ---------------------------------------------------------------------------- #

#' Similarly to the student level estimation, here I will run the regressions 
#' regarding each school's characteristics.


# ------------------ #
## 2.1 Data ------
# ------------------ #

#School data
df_school <- readRDS("Z:/Tuffy/Paper - Educ/Dados/censo_escolar_base.rds") %>% 
  mutate(codmun = as.character(codmun %/% 10),
         k = as.numeric(ano) - 2007) 


#Main regression dataframe
df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") %>% 
  select(codigo_ibge, ano, uf, nome, dif_coef_pp, dosage, aluno_dosage, PIBpc) %>% 
  mutate(across(c(codigo_ibge, uf), as.character))
 

#Combining both databases
df_comb <- df_school %>% 
  left_join(df_reg %>%
              filter(ano %in% c(2005:2017)) %>%
              mutate(codigo_ibge = as.character(codigo_ibge)),
            by = c("codmun" = "codigo_ibge", "ano" = "ano"))  



# -------------------- #
## 2.2 Structural REG ----
# -------------------- #

#' ***************
#' DISCLAIMER: Here I employed the -1 period as reference. If I want to resume
#' the usage to 0, as previously, I should change all [-1] values for [0] in the
#' codelines that follows
#' ***************

# ------------------ #
### 2.2.1 Dosage -----

#Classroom
est_class <- feols( classroom ~ dosage * i(k, ref = -1) +
                         PIBpc |
                         codmun + ano + uf.x^ano,
                       data = df_comb,
                       vcov = "hetero")
#Teacher room
est_troom <- feols( teach_room ~ dosage * i(k, ref = -1) +
                                   PIBpc |
                                   codmun + ano + uf.x^ano,
                                 data = df_comb,
                                 vcov = "hetero")
#Laboratory
est_labs <- feols(lab_dummy ~ dosage * i(k, ref = -1)
                  + PIBpc |
                    codmun + ano + uf.x^ano,
                  data = df_comb,
                  vcov = "hetero")

#Library
est_libra <- feols(lib_dummy ~ dosage * i(k, ref = -1)
                  + PIBpc |
                    codmun + ano + uf.x^ano,
                  data = df_comb,
                  vcov = "hetero")

#Play Area
est_play <- feols(play_area ~ dosage * i(k, ref = -1)
                  + PIBpc |
                    codmun + ano + uf.x^ano,
                  data = df_comb,
                  vcov = "hetero")

#Lunch
est_lunch <- feols(lunch ~ dosage * i(k, ref = -1)
                   + PIBpc |
                     codmun + ano + uf.x^ano,
                   data = df_comb,
                   vcov = "hetero")

#Employee 
est_employ <- feols(employee ~ dosage * i(k, ref = -1)
                    + PIBpc |
                      codmun + ano + uf.x^ano,
                    data = df_comb,
                    vcov = "hetero")

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ)


#### 2.2.1.1 Graph ----
event_style_plot <- function(est_obj, title = NULL, ylim = NULL) {
  # extract tidy data from fixest::etable or broom::tidy
  event_df <- broom::tidy(est_obj, conf.int = TRUE) %>% 
    mutate(
      k = str_extract(term, "(?<=k::)-?\\d+"),
      k = as.numeric(k))
  
  
  event_df <- event_df %>% 
    bind_rows(
      event_df %>%
        mutate(
          term = "k:-1",     
          estimate = 0,
          std.error = 0,
          statistic = 0,
          p.value = 1,
          conf.low = 0,
          conf.high = 0,
          k = -1
        )
    )
  
  ggplot(event_df, aes(x = k + 2007, y = estimate)) +
    # shaded standard error area
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                fill = "grey60", alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 2006, color = "black") +
    geom_point(shape = 15, size = 2, color = "black") +
    geom_line(color = "black") +
    labs(
      title = title,
      x = "Ano"
      ) +
    coord_cartesian(ylim = ylim) +
    theme_classic() +
    theme(
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11)
    ) + 
    scale_x_continuous(breaks = seq(2005, 2018, 2))

  }



# 3) Apply helper to each ggiplot object
p_class <- event_style_plot(est_class, "Salas de aula")   #Classroom
p_troom <- event_style_plot(est_troom, "Sala dos Prof.")  #Teacher's Room
p_labs  <- event_style_plot(est_labs,  "Laboratórios")    #Laboratory
p_libra <- event_style_plot(est_libra, "Biblioteca")      #Library
p_play  <- event_style_plot(est_play,  "Quadras/Parque")  #Play Area
p_lunch <- event_style_plot(est_lunch, "Merenda")         #Lunch
p_employ <- event_style_plot(est_employ,"Funcionários")   #Employee

blank <- ggplot() + theme_void()

grid_plot <- (p_class + p_troom + p_labs) /
  (p_libra + p_play + p_lunch) /
  (blank   + p_employ + blank)

 final <- grid_plot + plot_annotation(
   #title = "Event-study: infrastructure / staff outcomes",
   caption = "Estimates from feols(...) with i(k, ref = -1)"
  )

 final
 
 ggsave( #Saving image
   filename = paste0("school_structure_dosage.png"),
   plot = final,
   path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
   width = 800/96, height = 620/96, dpi = 300
 )

 rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
    final)
 
 # ----------------- #
 ### 2.2.2 Aluno Dosage ----
 #Classroom
 est_class <- feols( classroom ~ aluno_dosage * i(k, ref = -1) +
                       PIBpc |
                       codmun + ano + uf.x^ano,
                     data = df_comb,
                     vcov = "hetero")
 #Teacher room
 est_troom <- feols( teach_room ~ aluno_dosage * i(k, ref = -1) +
                       PIBpc |
                       codmun + ano + uf.x^ano,
                     data = df_comb,
                     vcov = "hetero")
 #Laboratory
 est_labs <- feols(lab_dummy ~ aluno_dosage * i(k, ref = -1)
                   + PIBpc |
                     codmun + ano + uf.x^ano,
                   data = df_comb,
                   vcov = "hetero")
 
 #Library
 est_libra <- feols(lib_dummy ~ aluno_dosage * i(k, ref = -1)
                    + PIBpc |
                      codmun + ano + uf.x^ano,
                    data = df_comb,
                    vcov = "hetero")
 
 #Play Area
 est_play <- feols(play_area ~ aluno_dosage * i(k, ref = -1)
                   + PIBpc |
                     codmun + ano + uf.x^ano,
                   data = df_comb,
                   vcov = "hetero")
 
 #Lunch
 est_lunch <- feols(lunch ~ aluno_dosage * i(k, ref = -1)
                    + PIBpc |
                      codmun + ano + uf.x^ano,
                    data = df_comb,
                    vcov = "hetero")
 
 #Employee 
 est_employ <- feols(employee ~ aluno_dosage * i(k, ref = -1)
                     + PIBpc |
                       codmun + ano + uf.x^ano,
                     data = df_comb,
                     vcov = "hetero")
 
 etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ)
 
 
 #### 2.2.2.1 Graph ----


 # 3) Apply helper to each ggiplot object
 p_class <- event_style_plot(est_class, "Salas de aula")   #Classroom
 p_troom <- event_style_plot(est_troom, "Sala dos Prof.")  #Teacher's Room
 p_labs  <- event_style_plot(est_labs,  "Laboratórios")    #Laboratory
 p_libra <- event_style_plot(est_libra, "Biblioteca")      #Library
 p_play  <- event_style_plot(est_play,  "Quadras/Parque")  #Play Area
 p_lunch <- event_style_plot(est_lunch, "Merenda")         #Lunch
 p_employ <- event_style_plot(est_employ,"Funcionários")   #Employee
 
 blank <- ggplot() + theme_void()
 
 grid_plot <- (p_class + p_troom + p_labs) /
   (p_libra + p_play + p_lunch) /
   (blank   + p_employ + blank)
 
 final <- grid_plot + plot_annotation(
   #title = "Event-study: infrastructure / staff outcomes",
   caption = "Estimates from feols(...) with i(k, ref = -1)"
 )
 
 final
 
 ggsave( #Saving image
   filename = paste0("school_structure_aluno_dosage.png"),
   plot = final,
   path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
   width = 800/96, height = 620/96, dpi = 300
 )
 
 rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
    final, est_class, est_employ, est_labs, est_libra, est_lunch, est_play,
    est_troom)


 # -------------------- #
 ## 2.3 Courses REG ----
 # -------------------- #
 
 #' ***************
 #' DISCLAIMER: Here I employed the -1 period as reference. If I want to resume
 #' the usage to 0, as previously, I should change all [-1] values for [0] in the
 #' codelines that follows
 #' ***************
 
 # ------------------ #
 ### 2.3.1 Dosage -----
 
 #Kindergarden
 est_kinder <- feols( kinder ~ dosage * i(k, ref = -1) +
                       PIBpc |
                       codmun + ano + uf.x^ano,
                     data = df_comb,
                     vcov = "hetero")
 #Elementary
 est_elem <- feols( elementary ~ dosage * i(k, ref = -1) +
                       PIBpc |
                       codmun + ano + uf.x^ano,
                     data = df_comb,
                     vcov = "hetero")
 #Highschool
 est_high <- feols(high ~ dosage * i(k, ref = -1)
                   + PIBpc |
                     codmun + ano + uf.x^ano,
                   data = df_comb,
                   vcov = "hetero")
 
 #Inclusion
 est_inclu <- feols(inclusion ~ dosage * i(k, ref = -1)
                    + PIBpc |
                      codmun + ano + uf.x^ano,
                    data = df_comb,
                    vcov = "hetero")
  
 
 
 #### 2.3.1.1 Graph ----
 p_kinder <- event_style_plot(est_kinder, "E. Infantil")  #Preschool
 p_elem <- event_style_plot(est_elem, "E. Fundamental")   #Elementary
 p_high  <- event_style_plot(est_high,  "E. Médio")       #HighSchool
 p_inclu <- event_style_plot(est_inclu, "E. Especial")    #Speical Education
 
 blank <- ggplot() + theme_void()
 
 grid_plot <- (p_kinder + p_elem) /
   (p_high + p_inclu) 
 
 final <- grid_plot + plot_annotation(
   #title = "Event-study: infrastructure / staff outcomes",
   caption = "Estimates from feols(...) with i(k, ref = -1)"
 )
 
 final
 
 ggsave( #Saving image
   filename = paste0("school_course_dosage.png"),
   plot = final,
   path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
   width = 800/96, height = 620/96, dpi = 300
 )
 
 rm(p_kinder, p_elem, p_high, p_inclu, final)
 
 # ----------------- #
 ### 2.3.2 Aluno Dosage ----
 
 #Kindergarden
 est_kinder <- feols( kinder ~ aluno_dosage * i(k, ref = -1) +
                        PIBpc |
                        codmun + ano + uf.x^ano,
                      data = df_comb,
                      vcov = "hetero")
 #Elementary
 est_elem <- feols( elementary ~ aluno_dosage * i(k, ref = -1) +
                      PIBpc |
                      codmun + ano + uf.x^ano,
                    data = df_comb,
                    vcov = "hetero")
 #Highschool
 est_high <- feols(high ~ aluno_dosage * i(k, ref = -1)
                   + PIBpc |
                     codmun + ano + uf.x^ano,
                   data = df_comb,
                   vcov = "hetero")
 
 #Inclusion
 est_inclu <- feols(inclusion ~ aluno_dosage * i(k, ref = -1)
                    + PIBpc |
                      codmun + ano + uf.x^ano,
                    data = df_comb,
                    vcov = "hetero")
 
 
 
 #### 2.3.2.1 Graph ----
 p_kinder <- event_style_plot(est_kinder, "E. Infantil")  #Preschool
 p_elem <- event_style_plot(est_elem, "E. Fundamental")   #Elementary
 p_high  <- event_style_plot(est_high,  "E. Médio")       #HighSchool
 p_inclu <- event_style_plot(est_inclu, "E. Especial")    #Speical Education
 
blank <- ggplot() + theme_void()
 
grid_plot <- (p_kinder + p_elem) /
   (p_high + p_inclu) 
 
final <- grid_plot + plot_annotation(
 #title = "Event-study: infrastructure / staff outcomes",
 caption = "Estimates from feols(...) with i(k, ref = -1)"
)
 
final
 
ggsave( #Saving image
   filename = paste0("school_course_aluno_dosage.png"),
   plot = final,
   path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
   width = 800/96, height = 620/96, dpi = 300
   )
 
rm(p_kinder, p_elem, p_high, p_inclu, final, est_elem, est_high, est_kinder,
    blank, grid_project, grid_plot)
 
 
rm(list = ls())


# ---------------------------------------------------------------------------- #
# 3. School per Municipality ---------
# ---------------------------------------------------------------------------- #

#' **********
#' Here I will aggregate the schools per municipality, and calculate the percen-
#' tage of exposure to a school facility per municipality

# ------------------- #
## 3.1 Data ----
# ------------------- #
df_school <- readRDS("Z:/Tuffy/Paper - Educ/Dados/censo_escolar_base_v2.rds") %>% 
  mutate(codmun = as.character(codmun %/% 10))

#We will calculate the municipal exposure, weighted by the enrollment numbers
df_school <- df_school %>% 
  group_by(codmun, ano) %>% 
  summarise(
    total_enroll = sum(enroll, na.rm = T),
    
    #numeral variables
    class     = total_enroll / sum(classroom, na.rm = T),
    employee  = total_enroll / sum(employee, na.rm = T),
    
    
    #School characteristics
    exp_troom = sum(enroll*teach_room, na.rm = T)/total_enroll,
    exp_lab   = sum(enroll*lab_dummy, na.rm = T) /total_enroll,
    exp_lib   = sum(enroll*lib_dummy, na.rm = T) /total_enroll,
    exp_play  = sum(enroll*play_area, na.rm = T) /total_enroll,
    exp_lunch = sum(enroll*lunch, na.rm = T)     /total_enroll,
    
    #Courses
    exp_psch  = sum(enroll*kinder, na.rm = T)    /total_enroll,
    exp_elem  = sum(enroll*elementary, na.rm = T)/total_enroll,
    exp_high  = sum(enroll*high, na.rm = T)      /total_enroll,
    exp_inc   = sum(enroll*inclusion, na.rm = T) /total_enroll,
    .groups = "drop"
  ) %>% 
  mutate(k = ano - 2007)


#Main database
df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") %>% 
  select(codigo_ibge, ano, uf, nome, dif_coef_pp, dosage, aluno_dosage, PIBpc) %>% 
  mutate(across(c(codigo_ibge, uf), as.character))


#Combining both databases
df_comb <- df_school %>% 
  left_join(df_reg %>%
              filter(ano %in% c(2005:2017)) %>%
              mutate(codigo_ibge = as.character(codigo_ibge)),
            by = c("codmun" = "codigo_ibge", "ano" = "ano"))  


rm(df_reg, df_school)
# -------------------- #
## 3.2 Structural REG ----
# -------------------- #


# ------------------ #
### 3.2.1 Dosage -----

#Classroom
est_class <- feols( class ~ dosage * i(k, ref = -1) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Teacher room
est_troom <- feols( exp_troom ~ dosage * i(k, ref = -1) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Laboratory
est_labs <- feols(exp_lab ~ dosage * i(k, ref = -1)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Library
est_libra <- feols(exp_lib ~ dosage * i(k, ref = -1)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Play Area
est_play <- feols(exp_play ~ dosage * i(k, ref = -1)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Lunch
est_lunch <- feols(exp_lunch ~ dosage * i(k, ref = -1)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Employee 
est_employ <- feols(employee ~ dosage * i(k, ref = -1)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ)


#### 3.2.1.1 Graph ----
event_style_plot <- function(est_obj, title = NULL, ylim = NULL) {
  # extract tidy data from fixest::etable or broom::tidy
  event_df <- broom::tidy(est_obj, conf.int = TRUE) %>% 
    mutate(
      k = str_extract(term, "(?<=k::)-?\\d+"),
      k = as.numeric(k))
  
  
  event_df <- event_df %>% 
    bind_rows(
      event_df %>%
        mutate(
          term = "k:-1",     
          estimate = 0,
          std.error = 0,
          statistic = 0,
          p.value = 1,
          conf.low = 0,
          conf.high = 0,
          k = -1
        )
    )
  
  # if you have event-time variable called k and confidence bounds conf.low/conf.high:
  ggplot(event_df, aes(x = k + 2007, y = estimate)) +
    # shaded standard error area
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                fill = "grey60", alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 2006, color = "black") +
    geom_point(shape = 15, size = 2, color = "black") +
    geom_line(color = "black") +
    labs(
      title = title,
      x = "Ano"
    ) +
    coord_cartesian(ylim = ylim) +
    theme_classic() +
    theme(
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11)
    ) + 
    scale_x_continuous(breaks = seq(2005, 2018, 2))
  
}



# 3) Apply helper to each ggiplot object
p_class <- event_style_plot(est_class, "Alunos/Salas de Aula")   #Classroom
p_troom <- event_style_plot(est_troom, "Sala dos Prof.")  #Teacher's Room
p_labs  <- event_style_plot(est_labs,  "Laboratórios")    #Laboratory
p_libra <- event_style_plot(est_libra, "Biblioteca")      #Library
p_play  <- event_style_plot(est_play,  "Quadras/Parque")  #Play Area
p_lunch <- event_style_plot(est_lunch, "Merenda")         #Lunch
p_employ <- event_style_plot(est_employ,"Alunos/Funcionários")   #Employee

blank <- ggplot() + theme_void()

grid_plot <- (p_troom + p_labs + p_lunch) /
  (p_libra + p_play + blank) /
  (p_class + p_employ + blank)

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("mun_school_structure_dosage.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
  width = 800/96, height = 620/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final)

# ----------------- #
### 3.2.2 Aluno Dosage ----
#Classroom
est_class <- feols( class ~ aluno_dosage * i(k, ref = -1) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Teacher room
est_troom <- feols( exp_troom ~ aluno_dosage * i(k, ref = -1) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Laboratory
est_labs <- feols(exp_lab ~ aluno_dosage * i(k, ref = -1)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Library
est_libra <- feols(exp_lib ~ aluno_dosage * i(k, ref = -1)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Play Area
est_play <- feols(exp_play ~ aluno_dosage * i(k, ref = -1)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Lunch
est_lunch <- feols(exp_lunch ~ aluno_dosage * i(k, ref = -1)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Employee 
est_employ <- feols(employee ~ aluno_dosage * i(k, ref = -1)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ)


#### 3.2.2.1 Graph ----


# 3) Apply helper to each ggiplot object
p_class <- event_style_plot(est_class, "Alunos/Salas de Aula")   #Classroom
p_troom <- event_style_plot(est_troom, "Sala dos Prof.")  #Teacher's Room
p_labs  <- event_style_plot(est_labs,  "Laboratórios")    #Laboratory
p_libra <- event_style_plot(est_libra, "Biblioteca")      #Library
p_play  <- event_style_plot(est_play,  "Quadras/Parque")  #Play Area
p_lunch <- event_style_plot(est_lunch, "Merenda")         #Lunch
p_employ <- event_style_plot(est_employ,"Alunos/Funcionários")   #Employee

blank <- ggplot() + theme_void()

grid_plot <- (p_troom + p_labs + p_lunch) /
  (p_libra + p_play + blank) /
  (p_class + p_employ + blank)


final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("mun_school_structure_aluno_dosage.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
  width = 800/96, height = 620/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final, est_class, est_employ, est_labs, est_libra, est_lunch, est_play,
   est_troom)


# ------------------- #
## 3.3 Abv vs. Blw ----
# ------------------- #

df_comb <- df_comb %>% 
  mutate(grupo = case_when(
    dosage > 0 ~ "Above",
    dosage < 0 ~ "Below",
    TRUE ~ NA
  ),
  grupo = factor(grupo))


#Library
est_libra <- feols(exp_lib ~ dosage : i(k, grupo, ref = -1)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Lunch
est_lunch <- feols(exp_lunch ~ dosage : i(k, grupo, ref = -1)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")


etable(est_libra, est_lunch)

#'As suspected the observed negative jump in lunch and library are associated with
#'the increase in the municipalities with negative dosage. Thus, it should be ta-
#'ken as reference the oposite movement.

rm(est_libra, est_lunch)

# ---------------------- #
## 3.4 Courses ----
# ---------------------- #


### 3.4.1 Dosage -----

#Kindergarden
est_kinder <- feols( exp_psch ~ dosage * i(k, ref = -1) +
                       PIBpc |
                       codmun + ano + uf^ano,
                     data = df_comb,
                     vcov = "hetero")
#Elementary
est_elem <- feols( exp_elem ~ dosage * i(k, ref = -1) +
                     PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")
#Highschool
est_high <- feols(exp_high ~ dosage * i(k, ref = -1)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

# #Inclusion
# est_inclu <- feols(inclusion ~ dosage * i(k, ref = -1)
#                    + PIBpc |
#                      codmun + ano + uf.x^ano,
#                    data = df_comb,
#                    vcov = "hetero")



#### 3.4.1.1 Graph ----
p_kinder <- event_style_plot(est_kinder, "E. Infantil")  #Preschool
p_elem <- event_style_plot(est_elem, "E. Fundamental")   #Elementary
p_high  <- event_style_plot(est_high,  "E. Médio")       #HighSchool
#p_inclu <- event_style_plot(est_inclu, "E. Especial")    #Speical Education

blank <- ggplot() + theme_void()

grid_plot <- (p_kinder + p_elem) /
  (p_high + blank) 

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("mun_school_course_dosage.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
  width = 800/96, height = 620/96, dpi = 300
)

rm(p_kinder, p_elem, p_high, final)

# ----------------- #
### 3.4.2 Aluno Dosage ----

#Kindergarden
est_kinder <- feols( exp_psch ~ aluno_dosage * i(k, ref = -1) +
                       PIBpc |
                       codmun + ano + uf^ano,
                     data = df_comb,
                     vcov = "hetero")
#Elementary
est_elem <- feols( exp_elem ~ aluno_dosage * i(k, ref = -1) +
                     PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")
#Highschool
est_high <- feols(exp_high ~ aluno_dosage * i(k, ref = -1)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

# #Inclusion
# est_inclu <- feols(inclusion ~ aluno_dosage * i(k, ref = -1)
#                    + PIBpc |
#                      codmun + ano + uf.x^ano,
#                    data = df_comb,
#                    vcov = "hetero")



#### 3.4.2.1 Graph ----
p_kinder <- event_style_plot(est_kinder, "E. Infantil")  #Preschool
p_elem <- event_style_plot(est_elem, "E. Fundamental")   #Elementary
p_high  <- event_style_plot(est_high,  "E. Médio")       #HighSchool
#p_inclu <- event_style_plot(est_inclu, "E. Especial")    #Speical Education

blank <- ggplot() + theme_void()

grid_plot <- (p_kinder + p_elem) /
  (p_high + blank) 

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("mun_school_course_aluno_dosage.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
  width = 800/96, height = 620/96, dpi = 300
)

rm(p_kinder, p_elem, p_high, p_inclu, final, est_elem, est_high, est_kinder,
   blank, grid_project, grid_plot)


rm(list = ls())





# ---------------------------------------------------------------------------- #
# 4. SAEB breakdown ----
# ---------------------------------------------------------------------------- #



# ---------------------- #
## 4.1 Data ----
# --------------------- #
 
df_saeb <- readRDS( "Z:/Tuffy/Paper - Educ/Dados/saeb_nvl_aluno.rds") %>%
   mutate(codmun = ifelse(ano >= 2007, as.numeric(codmun) %/% 10, codmun),
          codmun = as.character(codmun)) %>% #Arranging for the older municipal codes
  filter(codmun < 60000)
  
 
#Main regression dataframe
df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") %>% 
 select(codigo_ibge, ano, uf, nome, dif_coef_pp, dosage, aluno_dosage, PIBpc) %>% 
 mutate(across(c(codigo_ibge, uf), as.character))

 #Combining both dataframes
df <- df_saeb %>% 
 left_join(df_reg %>%
             filter(ano %in% c(2005:2017)) %>%
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
          
          
          peso = as.numeric(peso), #Weights
          
          age_late = case_when(
            grade == 5 & idade > 10 ~ 1, #Is older than the ideal age
            grade == 9 & idade > 14 ~ 1,  #Is older than the ideal age
            TRUE ~ 0
          ),
          age_dist = ifelse(idade_aux == 0, 1, 0), #Age distortion
          
          post_treat = ifelse(ano > 2007, 1, 0)
         ) 
  
#' I will group by municipality the number of observations in the SAEB individual
#' level data base. This will allow for comparisions between the sample through 
#' the years.
 
# ----------------- #
### 4.1.1 Group ----


df_mun <- df %>% 
  mutate(uf = as.numeric(codmun) %/% 10000) %>% 
  group_by(codmun, ano, id_uf) %>% 
  summarise(students = n(),
            .groups = "drop") %>% 
  arrange(codmun, ano, id_uf) %>% 
  mutate_all(as.numeric)


#### 4.1.1.1 Mun per year ----
#Municipalities by year
mun_tab <- df_mun %>% 
  group_by(ano) %>% 
  summarise(
    n_mun = n_distinct(codmun), #captures different municipalities
    total_students = sum(students, na.rm = T)
  ) %>% 
  arrange(ano)

table(mun_tab$ano)  

#Saving the table
latex_table <- knitr::kable(
  mun_tab,
  format = "latex",
  booktabs = TRUE,
  align = "lcccccc",
  linesep = ""
)
writeLines(latex_table, "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Desc/mun_ano.tex")


#### 4.1.1.2 UF - year -----

uf_tab <- df_mun %>% 
  group_by(id_uf) %>% 
  summarise(
    "2005" = n_distinct(codmun[ano == 2005]),
    "2007" = n_distinct(codmun[ano == 2007]),
    "2009" = n_distinct(codmun[ano == 2009]),
    "2011" = n_distinct(codmun[ano == 2011]),
    "2013" = n_distinct(codmun[ano == 2013]),
    "2015" = n_distinct(codmun[ano == 2015]),
    "2017" = n_distinct(codmun[ano == 2017]),
    .groups = "drop") %>% 
  mutate(uf = case_when( #Regions names
    id_uf == 11 ~ "RO",
    id_uf == 12 ~ "AC",
    id_uf == 13 ~ "AM",
    id_uf == 14 ~ "RR",
    id_uf == 15 ~ "PA",
    id_uf == 16 ~ "AP",
    id_uf == 17 ~ "TO",
    
    id_uf == 21 ~ "MA",
    id_uf == 22 ~ "PI",
    id_uf == 23 ~ "CE",
    id_uf == 24 ~ "RN",
    id_uf == 25 ~ "PB",
    id_uf == 26 ~ "PE",
    id_uf == 27 ~ "AL",
    id_uf == 28 ~ "SE",
    id_uf == 29 ~ "BA",
    
    id_uf == 31 ~ "MG",
    id_uf == 32 ~ "ES",
    id_uf == 33 ~ "RJ",
    id_uf == 35 ~ "SP",
    
    id_uf == 41 ~ "PR",
    id_uf == 42 ~ "SC",
    id_uf == 43 ~ "RS",
    
    id_uf == 50 ~ "MS",
    id_uf == 51 ~ "MT",
    id_uf == 52 ~ "GO",
    id_uf == 53 ~ "DF",
    T ~ NA
  )) %>%
  select(uf, everything(), -id_uf)

latex_table <- knitr::kable(
  uf_tab,
  format = "latex",
  booktabs = TRUE,
  align = "lcccccc",
  linesep = ""
)
writeLines(latex_table, "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Desc/mun_uf_ano.tex")


#df_mun %>% tab(id_uf)

#### 4.1.1.3 Students - year ----
uf_stu <- df_mun %>%
  group_by(id_uf, ano) %>%
  summarise(total_students = sum(students, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ano, values_from = total_students) %>% 
  mutate(uf = case_when( #Regions names
    id_uf == 11 ~ "RO",
    id_uf == 12 ~ "AC",
    id_uf == 13 ~ "AM",
    id_uf == 14 ~ "RR",
    id_uf == 15 ~ "PA",
    id_uf == 16 ~ "AP",
    id_uf == 17 ~ "TO",
    
    id_uf == 21 ~ "MA",
    id_uf == 22 ~ "PI",
    id_uf == 23 ~ "CE",
    id_uf == 24 ~ "RN",
    id_uf == 25 ~ "PB",
    id_uf == 26 ~ "PE",
    id_uf == 27 ~ "AL",
    id_uf == 28 ~ "SE",
    id_uf == 29 ~ "BA",
    
    id_uf == 31 ~ "MG",
    id_uf == 32 ~ "ES",
    id_uf == 33 ~ "RJ",
    id_uf == 35 ~ "SP",
    
    id_uf == 41 ~ "PR",
    id_uf == 42 ~ "SC",
    id_uf == 43 ~ "RS",
    
    id_uf == 50 ~ "MS",
    id_uf == 51 ~ "MT",
    id_uf == 52 ~ "GO",
    id_uf == 53 ~ "DF",
    T ~ NA
  )) %>%
  select(uf, everything(), -id_uf)

#Saving Table as LaTeX
latex_table <- knitr::kable(
  uf_stu,
  format = "latex",
  booktabs = TRUE,
  align = "lcccccc",
  linesep = ""
)
writeLines(latex_table, "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Desc/aluno_uf_ano.tex")
rm( latex_table)

# ----------------------- #
## 4.2 School Student ----
# ----------------------- #

#'Here I will invetigate if there was a significant difference of students within
#'a school that is present through all the observations years.


df_filter <- df_mun %>% 
  group_by(codmun) %>% 
  mutate(aux1 = 1,
         aux2 = sum(aux1, na.rm = T),
         time_to_treat = ano - 2007
         ) %>% 
  filter(aux2 == 7) %>% #Selecting only the mun present in all years
  ungroup() %>% 
  select(-aux1, -aux2)

### ------------------ #
### 4.2.1 Regression ----
### ------------------ #

est <- feols(students ~ i(time_to_treat, ref = 0) |
                               codmun,
                             data = df_filter,
                             vcov = "hetero")

etable(est)
etable(est,
       vcov = "hetero",
       headers = list(":_:" = list("N° Alunos" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Desc/num_alunos.tex", replace = TRUE)


