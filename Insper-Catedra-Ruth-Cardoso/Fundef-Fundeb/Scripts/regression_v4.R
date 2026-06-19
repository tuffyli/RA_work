# ---------------------------------------------------------------------------- #
# Regressions - Version 3
# Last edited by: Tuffy Licciardi Issa
# Date: 19/06/2025
# ---------------------------------------------------------------------------- #

#' ** ----------------------------------------------------------------------- **
#' In this version some of the graphs were discontinued from the previous. In the
#' 3.regression_v2 [both dosage variables] were present. For the following code
#' I chose to follow through utilizing only the *-ALUNO DOSAGE-* variable, since
#' it captures the effect of aditional R$ spending in one single student.
#' 
#' If needed, onde should gather the Dosage regressions from the previous document.
#' 
#' Also, I will change the specification regarding the winner vs. Loser regressions.
#' I will divide the data within each uf by its tercile of dosage variable, and then
#' compare the 1st and the 3rd terciles with the "control" being the 2nd tercile.
#' ** ----------------------------------------------------------------------- **


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
library(grid)
library(patchwork)


#Deactivating scientific notation
options(scipen = 999)

# ---------------------------------------------------------------------------- #
# Helpers ----
# ---------------------------------------------------------------------------- #

# ---- Paths ---- #

path <- "Z:/Tuffy/Paper - Educ/Resultados/v4/"
path_additional <- "Z:/Tuffy/Paper - Educ/Resultados/v4/Appendix/"
path_tables <- "Z:/Tuffy/Paper - Educ/Resultados/v4/Tables/"
path_figures <- "Z:/Tuffy/Paper - Educ/Resultados/v4/Figures/"

# ---- Functions ---- #

event_plot <- function(est_obj, treat_year = 2007,
                       ref_year = 2006,
                       title = NULL,
                       y_label = "Coefficient",
                       x_label = "Year",
                       ylim = NULL) {

  event_df <- broom::tidy(est_obj, conf.int = TRUE) %>%
    mutate(
      year = as.numeric(str_extract(term, "\\d{4}")),
      event_time = year - treat_year,
      coef_plot = estimate,
      ymin_plot = conf.low,
      ymax_plot = conf.high
    ) %>%
    filter(!is.na(year))

  # Add omitted reference year at zero (no marker, just keeps the timeline clear)
  base_row <- tibble(
    term = paste0("ref_", ref_year),
    year = ref_year,
    event_time = ref_year - treat_year,   # = -1 if treat_year = 2007
    estimate = 0,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    coef_plot = 0,
    ymin_plot = NA_real_,
    ymax_plot = NA_real_
  )

  plot_df <- bind_rows(event_df, base_row) %>%
    arrange(year)

  p <- ggplot(plot_df, aes(x = event_time, y = coef_plot)) +
    geom_errorbar(
      aes(ymin = ymin_plot, ymax = ymax_plot),
      width = 0.2,
      size = 0.8,
      color = "black",
      na.rm = TRUE
    ) +
    geom_point(size = 3, color = "black", na.rm = TRUE) +
    geom_hline(yintercept = 0, color = "#D62728", linewidth = 1) +
    geom_vline(xintercept = -1, color = "grey40", linetype = "dashed") +
    labs(x = x_label, y = y_label, title = title) +
    theme_minimal(base_size = 20) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(),
      axis.text.y = element_text(size = 18),
      axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5),
      legend.position = "none"
    ) +
    scale_x_continuous(
      breaks = sort(unique(plot_df$event_time)),
      labels = sort(unique(plot_df$event_time))
    )

  if (!is.null(ylim)) {
    p <- p + coord_cartesian(ylim = ylim)
  }

  p
}

tidy_event_model <- function(est_obj, model_name, treat_year = 2007, ref_year = 2006) {
  
  broom::tidy(est_obj, conf.int = TRUE) %>%
    mutate(
      year = as.numeric(str_extract(term, "\\d{4}")),
      coef_plot = estimate,
      ymin_plot = conf.low,
      ymax_plot = conf.high,
      model = model_name
    ) %>%
    filter(!is.na(year)) %>%
    bind_rows(
      tibble(
        term = paste0("ref_", ref_year),
        year = ref_year,
        estimate = 0,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        coef_plot = 0,
        ymin_plot = NA_real_,
        ymax_plot = NA_real_,
        model = model_name
      )
    )
}

event_plot_compare <- function(est_1, est_2,
                               name_1 = "Least dosage",
                               name_2 = "Most dosage",
                               treat_year = 2007,
                               ref_year = 2006,
                               title = NULL,
                               y_label = "Coefficient",
                               x_label = "Year",
                               ylim = NULL) {
  
  plot_df <- bind_rows(
    tidy_event_model(est_1, name_1, treat_year = treat_year, ref_year = ref_year),
    tidy_event_model(est_2, name_2, treat_year = treat_year, ref_year = ref_year)
  ) %>%
    mutate(time_event = year - treat_year) %>% 
    arrange(year, model)
  
  p <- ggplot(plot_df, aes(x = time_event, y = coef_plot, color = model, group = model)) +
    geom_errorbar(
      aes(ymin = ymin_plot, ymax = ymax_plot),
      position = position_dodge(width = 0.25),
      width = 0.15,
      size = 0.8,
      na.rm = TRUE
    ) +
    geom_point(
      position = position_dodge(width = 0.25),
      size = 3,
      na.rm = TRUE
    ) +
    geom_hline(yintercept = 0, color = "#D62728", linewidth = 1) +
    geom_vline(xintercept = -1, color = "grey40", linetype = "dashed") +
    labs(
      x = x_label,
      y = y_label,
      title = title,
      color = NULL
    ) +
    theme_minimal(base_size = 20) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(),
      axis.text.y = element_text(size = 18),
      axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5),
      legend.position = "bottom"
    ) +
    scale_x_continuous(
      breaks = sort(unique(plot_df$time_event)),
      labels = sort(unique(plot_df$time_event))
    )
  
  if (!is.null(ylim)) {
    p <- p + coord_cartesian(ylim = ylim)
  }
  
  p
}
# ---------------------------------------------------------------------------- #
# 1. Data ----
# Opening the main data base and then creating dividing the groups by the terciles.
# ---------------------------------------------------------------------------- #

df_main <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf_flags.rds")

# ---------------------------------------------------------------------------- #
## 1.1. Group Variable ----
# ---------------------------------------------------------------------------- #

df_main <- df_main %>%
  mutate(cod_uf = codigo_ibge %/% 10000) %>% 
  group_by(cod_uf) %>%
  mutate(
    dosage_tercile = ntile(dosage, 3),
    #1. Lowest,
    #2. Middle,
    #3. Highest
    
    post_treat = ifelse(ano > 2006, 1, 0)
  ) %>%
  ungroup()

df_main <- df_main %>% 
  mutate( 
    log_mat_total = log(mat_total),
    log_mat_fun   = log(mat_fun),
    log_mat_med   = log(mat_med),
    log_mat_inf   = log(mat_inf),
    log_mat_eja   = log(mat_eja),
    log_mat_esp   = log(mat_esp))

# Now lets check if the quantity are similar within each uf
check <- df_main %>% 
  group_by(cod_uf, uf) %>% 
  summarise(
    ter_1 = n_distinct(codigo_ibge[dosage_tercile == 1]),
    ter_2 = n_distinct(codigo_ibge[dosage_tercile == 2]),
    ter_3 = n_distinct(codigo_ibge[dosage_tercile == 3]),
    
    #Mean dosage
    mean_dos_1 = round(mean(dosage[dosage_tercile == 1], na.rm = T), 3),
    mean_dos_2 = round(mean(dosage[dosage_tercile == 2], na.rm = T), 3),
    mean_dos_3 = round(mean(dosage[dosage_tercile == 3], na.rm = T), 3),
    
    .groups = "drop"
  )


# Saving the table
tex_table <- check %>%
  select(-cod_uf) %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    longtable = FALSE,
    caption = "Distribution of schools across dosage terciles by UF",
    label = "tab:dosage_terciles",
    align = c("l", "r", "r", "r", "r", "r", "r")
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    font_size = 9
  )

writeLines(tex_table, paste(path_additional,"appendix_dosage_terciles.tex"))

rm(tex_table, check)

# ---------------------------------------------------------------------------- #
# 2. Education Spend per-student ----
# ---------------------------------------------------------------------------- #
## 2.1 Main ----
# ---------------------------------------------------------------------------- #
### 2.1.1 ATT ----
# ---------------------------------------------------------------------------- #


#Main
est_1 <- feols(real_des_edu_pa ~ aluno_dosage*post_treat + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_main,
               cluster = ~codigo_ibge)
#Inf
est_2 <- feols(real_des_inf_pa ~ aluno_dosage*post_treat + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_main,
               cluster = ~codigo_ibge)
#Fund
est_3 <- feols(real_des_fun_pa ~ aluno_dosage*post_treat + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_main,
               cluster = ~codigo_ibge)
#Em
est_4 <- feols(real_des_med_pa ~ aluno_dosage*post_treat + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_main,
               cluster = ~codigo_ibge)


etable(est_1, est_2, est_3, est_4)


# --- Saving ---- #
etable(
  est_1, est_2, est_3, est_4,
  tex = TRUE,
  drop = "PIBpc",
  file = paste(path_tables,"main_studosage.tex"),
  digits = 3,
  replace = T,
  title = "Effects of Fundef Student Dosage on Education Spending",
  label = "tab:main_spending"
)



# ---------------------------------------------------------------------------- #
## 2.2 Dynamic ----
#----------------------------------------------------------------------------- #

#Main
est_1 <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                               | codigo_ibge + ano + uf^ano,
                               data = df_main,
                               cluster = ~codigo_ibge)
#Inf
est_2 <- feols(real_des_inf_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_main,
               cluster = ~codigo_ibge)
#Fund
est_3 <- feols(real_des_fun_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_main,
               cluster = ~codigo_ibge)
#Em
est_4 <- feols(real_des_med_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_main,
               cluster = ~codigo_ibge)


etable(est_1, est_2, est_3, est_4)
# ---------------------------------------------------------------------------- #
### 2.2.1 Table ----
# ---------------------------------------------------------------------------- #

etable(
  est_1, est_2, est_3, est_4,
  tex = TRUE,
  file = paste(path_tables,"event_study_studosage.tex"),
  digits = 3,
  replace = T,
  title = "Effects of Fundef Student Dosage on Education Spending",
  label = "tab:stu_dosage_spending"
)

# ---------------------------------------------------------------------------- #
### 2.2.2 Figure ----
# ---------------------------------------------------------------------------- #

#Ploting for each specification
p1 <- event_plot(
  est_1,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Total Education Spending"
)

p2 <- event_plot(
  est_2,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Pre-School Spending"
)

p3 <- event_plot(
  est_3,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Middle School Spending"
)

p4 <- event_plot(
  est_4,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "High School Spending"
)

# Joining the plots into a single one
final_plot <- (p1 | p2) / (p3 | p4)

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_figures, "main_specification.pdf"),
       device = "pdf", height = 8, width = 15)

rm(p1, p2, p3, p4, final_plot,
   est_1, est_2, est_3, est_4)

# ---------------------------------------------------------------------------- #
# 3. Tercile comparision ----
# ---------------------------------------------------------------------------- #
## 3.1 Data ----
# ---------------------------------------------------------------------------- #

df_least <- df_main %>% 
  filter(dosage_tercile < 3) %>% #Least dosage group
  mutate(treat = ifelse(dosage_tercile != 2, 1, 0))
  
df_most <- df_main %>% 
  filter(dosage_tercile > 1) %>% #Most dosage group
  mutate(treat = ifelse(dosage_tercile != 2, 1, 0))


# ---------------------------------------------------------------------------- #
## 3.2 Regression ----
# ---------------------------------------------------------------------------- #

# ---- Total education ---- #
est_least1 <- feols(
  real_des_edu_pa ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_least,
  cluster = ~codigo_ibge
)

est_most1 <- feols(
  real_des_edu_pa ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_most,
  cluster = ~codigo_ibge
)

etable(est_least1, est_most1)



# ---- Pre-School ---- #
est_least2 <- feols(
  real_des_inf_pa ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_least,
  cluster = ~codigo_ibge
)

est_most2 <- feols(
  real_des_inf_pa ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_most,
  cluster = ~codigo_ibge
)

etable(est_least2, est_most2)


# ---- Middle School ---- #
est_least3 <- feols(
  real_des_fun_pa ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_least,
  cluster = ~codigo_ibge
)

est_most3 <- feols(
  real_des_fun_pa ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_most,
  cluster = ~codigo_ibge
)

etable(est_least3, est_most3)

# ---- High School ---- #
est_least4 <- feols(
  real_des_med_pa ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_least,
  cluster = ~codigo_ibge
)

est_most4 <- feols(
  real_des_med_pa ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_most,
  cluster = ~codigo_ibge
)

etable(est_least4, est_most4)

# ---------------------------------------------------------------------------- #
## 3.3 Figure ----
# ---------------------------------------------------------------------------- #

# ---- Total Spending ---- #
p1 <- event_plot_compare( est_most1, est_least1,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "Total Education Spending")

p1

# ---- Pre-School ---- #
p2 <- event_plot_compare(est_most2, est_least2,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "Pre-School Spending")

p2

# ---- Middle School ---- #
p3 <- event_plot_compare(est_most3, est_least3,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "Middle School Spending")

p3

# ---- High School ---- #
p4 <- event_plot_compare(est_most4, est_least4,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "High School Spending")

p4


# ---------------------------------------------------------------------------- #
### 3.3.1 Combined Figure ----
# ---------------------------------------------------------------------------- #

# Joining the plots into a single one
final_plot <- (p1 | p2) / (p3 | p4)  +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_figures, "least_vs_most_main_specification.pdf"),
       device = "pdf", height = 8, width = 15)

rm(p1, p2, p3, p4, final_plot,
   est_least1, est_least2, est_least3, est_least4,
   est_most1, est_most2, est_most3, est_most4)

# ---------------------------------------------------------------------------- #
# 4. Enrollment analysis ----
# ---------------------------------------------------------------------------- #
## 4.1 Complete ----
# ---------------------------------------------------------------------------- #


#Main
est_1 <- feols(log_mat_total ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_main,
               cluster = ~codigo_ibge)
#Inf
est_2 <- feols(log_mat_fun ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_main,
               cluster = ~codigo_ibge)
#Fund
est_3 <- feols(log_mat_med ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_main,
               cluster = ~codigo_ibge)
#Em
est_4 <- feols(log_mat_inf ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_main,
               cluster = ~codigo_ibge)


etable(est_1, est_2, est_3, est_4)

# ---- Saving Table ---- #

etable(
  est_1, est_2, est_3, est_4,
  tex = TRUE,
  file = paste(path_tables,"enrollment_studosage.tex"),
  digits = 3,
  replace = T,
  title = "Effects of Fundef Student Dosage on Education Spending",
  label = "tab:stu_dosage_spending"
)

# ---------------------------------------------------------------------------- #
### 4.1.1 Figure ----
# ---------------------------------------------------------------------------- #

#Ploting for each specification
p1 <- event_plot(
  est_1,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Total Education Enrollment"
)

p2 <- event_plot(
  est_2,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Pre-School Enrollment"
)

p3 <- event_plot(
  est_3,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Middle School Enrollment"
)

p4 <- event_plot(
  est_4,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "High School Enrollment"
)

# Joining the plots into a single one
final_plot <- (p1 | p2) / (p3 | p4)

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_figures, "enrollment_specification.pdf"),
       device = "pdf", height = 8, width = 15)

rm(p1, p2, p3, p4, final_plot,
   est_1, est_2, est_3, est_4)



# ---------------------------------------------------------------------------- #
## 4.2 Most vs. Least ----
# ---------------------------------------------------------------------------- #


# ---- Total education ---- #
est_least1 <- feols(
  log_mat_total ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_least,
  cluster = ~codigo_ibge
)

est_most1 <- feols(
  log_mat_total ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_most,
  cluster = ~codigo_ibge
)

etable(est_least1, est_most1)



# ---- Pre-School ---- #
est_least2 <- feols(
  log_mat_inf ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_least,
  cluster = ~codigo_ibge
)

est_most2 <- feols(
  log_mat_inf ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_most,
  cluster = ~codigo_ibge
)

etable(est_least2, est_most2)


# ---- Middle School ---- #
est_least3 <- feols(
  log_mat_fun ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_least,
  cluster = ~codigo_ibge
)

est_most3 <- feols(
  log_mat_fun ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_most,
  cluster = ~codigo_ibge
)

etable(est_least3, est_most3)

# ---- High School ---- #
est_least4 <- feols(
  log_mat_med ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_least,
  cluster = ~codigo_ibge
)

est_most4 <- feols(
  log_mat_med ~ i(ano, treat, ref = 2006) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_most,
  cluster = ~codigo_ibge
)

etable(est_least4, est_most4)

# ---------------------------------------------------------------------------- #
### 4.2.1 Figure ----
# ---------------------------------------------------------------------------- #


# ---- Total Spending ---- #
p1 <- event_plot_compare( est_most1, est_least1,
                          name_1 = "Most Dosage", name_2 = "Least Dosage",
                          ref_year = 2006, treat_year = 2007,
                          x_label = "Time to Treatment",
                          y_label = "Coefficient",
                          title = "Total Education Enrollment")

p1

# ---- Pre-School ---- #
p2 <- event_plot_compare(est_most2, est_least2,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "Pre-School Enrollment")

p2

# ---- Middle School ---- #
p3 <- event_plot_compare(est_most3, est_least3,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "Middle School Enrollment")

p3

# ---- High School ---- #
p4 <- event_plot_compare(est_most4, est_least4,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "High School Enrollment")

p4

# ---- Saving the Figure ----- #



# Joining the plots into a single one
final_plot <- (p1 | p2) / (p3 | p4)  +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_figures, "least_vs_most_enrollment_specification.pdf"),
       device = "pdf", height = 8, width = 15)

rm(p1, p2, p3, p4, final_plot,
   est_least1, est_least2, est_least3, est_least4,
   est_most1, est_most2, est_most3, est_most4)

# ---------------------------------------------------------------------------- #
# 5. School Characteristics ----
# ---------------------------------------------------------------------------- #
## 5.1 Data ----
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
  )


df_school_reg <- df_main %>% 
  select(codigo_ibge, ano, uf, nome, dosage, aluno_dosage, PIBpc,
         des_edu_pc, des_fund_pc, des_med_pc, des_inf_pc, dosage_tercile) %>% 
  mutate(across(c(codigo_ibge, uf), as.character)) %>% 
  filter(ano %in% c (2005:2018)) %>% 
  left_join(df_school, by = c("codigo_ibge" = "codmun", "ano"))

rm(df_school)

# ---------------------------------------------------------------------------- #
### 5.1.1 Most vs. Least ----
# ---------------------------------------------------------------------------- #

df_school_reg <- df_school_reg %>%
  mutate(cod_uf = as.numeric(codigo_ibge) %/% 10000) %>% 
  group_by(cod_uf) %>%
  mutate(
    dosage_tercile = ntile(dosage, 3),
    #1. Lowest,
    #2. Middle,
    #3. Highest
    
    post_treat = ifelse(ano > 2006, 1, 0)
  ) %>%
  ungroup()

# ---- Least ---- #
df_sch_least <- df_school_reg %>% 
  filter(dosage_tercile < 3) %>% #Least dosage group
  mutate(treat = ifelse(dosage_tercile != 2, 1, 0))

# ---- Most ---- #
df_sch_most <- df_school_reg %>% 
  filter(dosage_tercile > 1) %>% #Most dosage group
  mutate(treat = ifelse(dosage_tercile != 2, 1, 0))


# ---------------------------------------------------------------------------- #
## 5.2 Main Regression ----
# ---------------------------------------------------------------------------- #

# dependent variables in the order you want
y_vars <- c(
  "class",
  "exp_troom",
  "exp_lab",
  "exp_lib",
  "exp_play",
  "exp_lunch",
  "employee",
  "n_employee"
)

for (i in seq_along(y_vars)) {
  y <- y_vars[i]
  
  fml <- as.formula(
    paste0(y, " ~ i(ano, treat, ref = 2006) + PIBpc | codigo_ibge + ano + uf^ano")
  )
  
  assign(
    paste0("est_least", i),
    feols(
      fml,
      data = df_sch_least,
      cluster = ~codigo_ibge
    )
  )
  
  assign(
    paste0("est_most", i),
    feols(
      fml,
      data = df_sch_most,
      cluster = ~codigo_ibge
    )
  )
}
