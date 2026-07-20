# ---------------------------------------------------------------------------- #
# Regressions - Version 4
# Last edited by: Tuffy Licciardi Issa
# Date: 14/07/2025
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
path_descp <- "Z:/Tuffy/Paper - Educ/Resultados/v4/Descp/"
path_school <- "Z:/Tuffy/Paper - Educ/Resultados/v4/School"
path_school_fig <- "Z:/Tuffy/Paper - Educ/Resultados/v4/School/Figures/"

path_shcool2 <- "Z:/Tuffy/Paper - Educ/Resultados/v4/School/Mun_aff/"
path_school_fig2 <- "Z:/Tuffy/Paper - Educ/Resultados/v4/School/Mun_aff/Figures/"

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
                               b_size = 20,
                               t_size = 18,
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
      width = 0.38,
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
    theme_minimal(base_size = b_size) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(),
      axis.text.y = element_text(size = t_size),
      axis.text.x = element_text(size = t_size, angle = 0, hjust = 0.5),
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
# 1. Data -----
# ---------------------------------------------------------------------------- #
## 1.1 Main ----
# ---------------------------------------------------------------------------- #


df_school <- readRDS("Z:/Tuffy/Paper - Educ/Dados/final/mun_school_data.rds")


# ---------------------------------------------------------------------------- #
## 1.2 Least vs. Most ----
# ---------------------------------------------------------------------------- #

df_least <- df_school %>% 
  filter(dosage_tercile < 3) %>% #Least dosage group
  mutate(treat = ifelse(dosage_tercile != 2, 1, 0))

df_most <- df_school %>% 
  filter(dosage_tercile > 1) %>% #Most dosage group
  mutate(treat = ifelse(dosage_tercile != 2, 1, 0))

# ---------------------------------------------------------------------------- #
# 2. Enrollment analysis ----
# ---------------------------------------------------------------------------- #
## 2.1 Enrollment lvl ----
# ---------------------------------------------------------------------------- #
### 2.1.1 Table -----
# ---------------------------------------------------------------------------- #

#Main
est_1 <- feols(cre_pub_enroll ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
               | codmun + ano + uf^ano,
               data = df_school,
               cluster = ~codmun)
#Inf
est_2 <- feols( pre_pub_enroll ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)
#Fund
est_3 <- feols( inf_pub_enroll ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)
#Em
est_4 <- feols( fun_pub_enroll ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)


etable(est_1, est_2, est_3, est_4)

# ---- Saving Table ---- #

etable(
  est_1, est_2, est_3, est_4,
  tex = TRUE,
  file = file.path(path_school,"/enrollment_school_studosage.tex"),
  digits = 3,
  replace = T,
  title = "Effects of Fundef Student Dosage on Education Enrollment",
  label = "tab:stu_dosage_school_enroll"
)

# ---------------------------------------------------------------------------- #
### 2.1.2 Figure ----
# ---------------------------------------------------------------------------- #

#Ploting for each specification
p1 <- event_plot(
  est_1,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "DayCare enrollment"
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
  title = "Child Education Enrollment"
)

p4 <- event_plot(
  est_4,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Middle School Enrollment"
)

# Joining the plots into a single one
final_plot <- (p1 | p2) / (p3 | p4)

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "enrollment_specification.pdf"),
       device = "pdf", height = 8, width = 15)


rm(p1, p2, p3, p4, final_plot,
   est_1, est_2, est_3, est_4)



# ---------------------------------------------------------------------------- #
### 2.1.3 Most vs. Least ----
# ---------------------------------------------------------------------------- #


# ---- Total Child Education ---- #
est_least1 <- feols(
  inf_pub_enroll ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data =  df_least,
  cluster = ~codmun
)

est_most1 <- feols(
  inf_pub_enroll ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least1, est_most1)



# ---- Pre-School ---- #
est_least2 <- feols(
  pre_pub_enroll ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_least,
  cluster = ~codmun
)

est_most2 <- feols(
  pre_pub_enroll ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least2, est_most2)


# ---- Daycare School ---- #
est_least3 <- feols(
  cre_pub_enroll ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_least,
  cluster = ~codmun
)

est_most3 <- feols(
  cre_pub_enroll ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least3, est_most3)

# ---- Middle School ---- #
est_least4 <- feols(
  fun_pub_enroll ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_least,
  cluster = ~codmun
)

est_most4 <- feols(
  fun_pub_enroll ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least4, est_most4)

# ---------------------------------------------------------------------------- #
#### 2.1.3.1 Figure ----
# ---------------------------------------------------------------------------- #


# ---- Total Spending ---- #
p1 <- event_plot_compare( est_most1, est_least1,
                          name_1 = "Most Dosage", name_2 = "Least Dosage",
                          ref_year = 2006, treat_year = 2007,
                          x_label = "Time to Treatment",
                          y_label = "Coefficient",
                          title = "Child Education Enrollment")

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
                         title = "Daycare School Enrollment")

p3

# ---- High School ---- #
p4 <- event_plot_compare(est_most4, est_least4,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "Middle School Enrollment")

p4

# ---- Saving the Figure ----- #



# Joining the plots into a single one
final_plot <- (p2)   +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "least_vs_most_enrollment_specification_v2.pdf"),
       device = "pdf", height = 8, width = 15)


# ---------------------------------------------------------------------------- #
#### 2.1.3.2 Table -----
# ---------------------------------------------------------------------------- #

# - Pre School - #

etable(
  est_most2, est_least2, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_pre_enroll_studosage.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = c(
    "inf_pub_enroll" = "Child Education Enrollment",
    "pre_pub_enroll" = "Pre-School Enrollment"
  ),
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Enrollment",
  label = "tab:lvm_stu_dosage_preschool_enroll"
)

# - Child Education - #

etable(
  est_most1, est_least1, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_che_enroll_studosage.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = c(
    "inf_pub_enroll" = "Child Education Enrollment",
    "pre_pub_enroll" = "Pre-School Enrollment"
  ),
  title = "Least vs. Most: Effects of Fundef Student Dosage on Child Education Enrollment",
  label = "tab:lvm_stu_dosage_cheschool_enroll"
)


rm(p1, p2, p3, p4, final_plot,
   est_least1, est_least2, est_least3, est_least4,
   est_most1, est_most2, est_most3, est_most4)




# ---------------------------------------------------------------------------- #
# 3. N° of schools -----
# ---------------------------------------------------------------------------- #
## 3.1 Table -----
# ---------------------------------------------------------------------------- #

#Main
est_1 <- feols(n_pub_creche ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
               | codmun + ano + uf^ano,
               data = df_school,
               cluster = ~codmun)
#Inf
est_2 <- feols( n_pub_presco ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)
#Fund
est_3 <- feols( n_pub_fundam ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)


etable(est_1, est_2, est_3)

# ---- Saving Table ---- #

etable(
  est_1, est_2, est_3,
  tex = TRUE,
  file = file.path(path_school,"/number_school_studosage.tex"),
  digits = 3,
  replace = T,
  title = "Effects of Fundef Student Dosage on Number of Schools",
  label = "tab:stu_dosage_number_school"
)

# ---------------------------------------------------------------------------- #
## 3.2 Figure ----
# ---------------------------------------------------------------------------- #

#Ploting for each specification
p1 <- event_plot(
  est_1,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Number DayCares"
)

p2 <- event_plot(
  est_2,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Number Pre-Schools"
)


p3 <- event_plot(
  est_3,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Number Middle School"
)

# Joining the plots into a single one
final_plot <- (p1 | p2) / (p3 | plot_spacer())

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "number_schools.pdf"),
       device = "pdf", height = 8, width = 15)

rm(p1, p2, p3, p4, final_plot,
   est_1, est_2, est_3, est_4)

# ---------------------------------------------------------------------------- #
## 3.3 Most vs. Least ----
# ---------------------------------------------------------------------------- #


# ---- Total Child Education ---- #
est_least1 <- feols(
  n_pub_creche ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data =  df_least,
  cluster = ~codmun
)

est_most1 <- feols(
  n_pub_creche ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least1, est_most1)



# ---- Pre-School ---- #
est_least2 <- feols(
  n_pub_presco ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_least,
  cluster = ~codmun
)

est_most2 <- feols(
  n_pub_presco ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least2, est_most2)


# ---- Daycare School ---- #
est_least3 <- feols(
  n_pub_fundam ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_least,
  cluster = ~codmun
)

est_most3 <- feols(
  n_pub_fundam ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least3, est_most3)


# ---------------------------------------------------------------------------- #
### 3.3.1 Figure ----
# ---------------------------------------------------------------------------- #


# ---- Total Spending ---- #
p1 <- event_plot_compare( est_most1, est_least1,
                          name_1 = "Most Dosage", name_2 = "Least Dosage",
                          ref_year = 2006, treat_year = 2007,
                          x_label = "Time to Treatment",
                          y_label = "Coefficient",
                          title = "Number of DayCares")

p1

# ---- Pre-School ---- #
p2 <- event_plot_compare(est_most2, est_least2,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "Number of Pre-School")

p2

# ---- Middle School ---- #
p3 <- event_plot_compare(est_most3, est_least3,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "Number of Middle School")

p3




# Joining the plots into a single one
final_plot <- (p2)  +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "least_vs_most_number_preschools.pdf"),
       device = "pdf", height = 8, width = 15)


# ---------------------------------------------------------------------------- #
### 3.3.2 Table -----
# ---------------------------------------------------------------------------- #

# - Pre School - #

etable(
  est_most2, est_least2, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_pre_school_studosage.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = c(
    "n_pub_child" = "Child Education Schools",
    "n_pub_presco" = "Pre-Schools"
  ),
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-Schools",
  label = "tab:lvm_stu_dosage_preschool"
)


rm(p1, p2, p3, p4, final_plot,
   est_least1, est_least2, est_least3, est_least4,
   est_most1, est_most2, est_most3, est_most4)


# ---------------------------------------------------------------------------- #
# 4. N° Teachers and High education ----
# ---------------------------------------------------------------------------- #
## 4.1 Teachers -----
# ---------------------------------------------------------------------------- #
### 4.1.1 Table ----
# ---------------------------------------------------------------------------- #

#Main
est_1 <- feols(n_t_cre ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
               | codmun + ano + uf^ano,
               data = df_school,
               cluster = ~codmun)
#Inf
est_2 <- feols( n_t_pre ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)
#Fund
est_3 <- feols( n_t_fun ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)


etable(est_1, est_2, est_3)

# ---- Saving Table ---- #

etable(
  est_1, est_2, est_3,
  tex = TRUE,
  file = file.path(path_school,"/number_teachers_studosage.tex"),
  digits = 3,
  replace = T,
  title = "Effects of Fundef Student Dosage on Number of Teachers",
  label = "tab:stu_dosage_number_teachers"
)

# ---------------------------------------------------------------------------- #
### 4.1.2 Figure ----
# ---------------------------------------------------------------------------- #

#Ploting for each specification
p1 <- event_plot(
  est_1,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Teachers in DayCare"
)

p2 <- event_plot(
  est_2,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Teachers in Pre-School"
)


p3 <- event_plot(
  est_3,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Teachers in Middle School"
)

# Joining the plots into a single one
final_plot <- (p1 | p2) / (p3 | plot_spacer())

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "number_teachers.pdf"),
       device = "pdf", height = 8, width = 15)

rm(p1, p2, p3, p4, final_plot,
   est_1, est_2, est_3, est_4)

# ---------------------------------------------------------------------------- #
### 4.1.3 Most vs. Least ----
# ---------------------------------------------------------------------------- #


# ---- Total Child Education ---- #
est_least1 <- feols(
  n_t_cre ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data =  df_least,
  cluster = ~codmun
)

est_most1 <- feols(
  n_t_cre ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least1, est_most1)



# ---- Pre-School ---- #
est_least2 <- feols(
  n_t_pre ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_least,
  cluster = ~codmun
)

est_most2 <- feols(
  n_t_pre ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least2, est_most2)


# ---- Daycare School ---- #
est_least3 <- feols(
  n_t_fun ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_least,
  cluster = ~codmun
)

est_most3 <- feols(
  n_t_fun ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least3, est_most3)


# ---------------------------------------------------------------------------- #
#### 4.1.3.1 Figure ----
# ---------------------------------------------------------------------------- #


# ---- Total Spending ---- #
p1 <- event_plot_compare( est_most1, est_least1,
                          name_1 = "Most Dosage", name_2 = "Least Dosage",
                          ref_year = 2006, treat_year = 2007,
                          x_label = "Time to Treatment",
                          y_label = "Coefficient",
                          title = "Teachers in DayCares")

p1

# ---- Pre-School ---- #
p2 <- event_plot_compare(est_most2, est_least2,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "Teachers in Pre-School")

p2

# ---- Middle School ---- #
p3 <- event_plot_compare(est_most3, est_least3,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "Teachers in Middle School")

p3




# Joining the plots into a single one
final_plot <- (p2) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "least_vs_most_number_pre_teachers.pdf"),
       device = "pdf", height = 8, width = 15)


# ---------------------------------------------------------------------------- #
#### 4.1.3.2 Table ----
# ---------------------------------------------------------------------------- #

# - Pre School - #

etable(
  est_most2, est_least2, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_pre_teachers_studosage.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = c(
    "n_t_chi" = "Child Education Teachers",
    "n_t_pre" = "Pre-School Teachers"
  ),
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Teachers",
  label = "tab:lvm_stu_dosage_preschool_teachers"
)


rm(p1, p2, p3, p4, final_plot,
   est_least1, est_least2, est_least3, est_least4,
   est_most1, est_most2, est_most3, est_most4)

# ---------------------------------------------------------------------------- #
## 4.2 [LVL] High education teachers ----
# ---------------------------------------------------------------------------- #
### 4.2.1 Table ----
# ---------------------------------------------------------------------------- #

#Main
est_1 <- feols(n_t_cre_edu ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
               | codmun + ano + uf^ano,
               data = df_school,
               cluster = ~codmun)
#Inf
est_2 <- feols( n_t_pre_edu ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)
#Fund
est_3 <- feols( n_t_fun_edu ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)


etable(est_1, est_2, est_3)

# ---- Saving Table ---- #

etable(
  est_1, est_2, est_3,
  tex = TRUE,
  file = file.path(path_school,"number_teachers_edu_studosage.tex"),
  digits = 3,
  replace = T,
  title = "Effects of Fundef Student Dosage on Number of Teachers",
  label = "tab:stu_dosage_number_teachers_edu"
)

# ---------------------------------------------------------------------------- #
### 4.1.2 Figure ----
# ---------------------------------------------------------------------------- #

#Ploting for each specification
p1 <- event_plot(
  est_1,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "HE Teachers in DayCare"
)

p2 <- event_plot(
  est_2,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "HE Teachers in Pre-School"
)


p3 <- event_plot(
  est_3,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "HE Teachers in Middle School"
)

# Joining the plots into a single one
final_plot <- (p1 | p2) / (p3 | plot_spacer())

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "number_teachers_edu.pdf"),
       device = "pdf", height = 8, width = 15)

rm(p1, p2, p3, p4, final_plot,
   est_1, est_2, est_3, est_4)

# ---------------------------------------------------------------------------- #
### 4.1.3 Most vs. Least ----
# ---------------------------------------------------------------------------- #


# ---- Total Child Education ---- #
est_least1 <- feols(
  n_t_cre_edu ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data =  df_least,
  cluster = ~codmun
)

est_most1 <- feols(
  n_t_cre_edu ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least1, est_most1)



# ---- Pre-School ---- #
est_least2 <- feols(
  n_t_pre_edu ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_least,
  cluster = ~codmun
)

est_most2 <- feols(
  n_t_pre_edu ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least2, est_most2)


# ---- Daycare School ---- #
est_least3 <- feols(
  n_t_fun_edu ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_least,
  cluster = ~codmun
)

est_most3 <- feols(
  n_t_fun_edu ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least3, est_most3)


# ---------------------------------------------------------------------------- #
#### 4.1.3.1 Figure ----
# ---------------------------------------------------------------------------- #


# ---- Total Spending ---- #
p1 <- event_plot_compare( est_most1, est_least1,
                          name_1 = "Most Dosage", name_2 = "Least Dosage",
                          ref_year = 2006, treat_year = 2007,
                          x_label = "Time to Treatment",
                          y_label = "Coefficient",
                          title = "HE Teachers in DayCares")

p1

# ---- Pre-School ---- #
p2 <- event_plot_compare(est_most2, est_least2,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "HE Teachers in Pre-School")

p2

# ---- Middle School ---- #
p3 <- event_plot_compare(est_most3, est_least3,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "HE Teachers in Middle School")

p3




# Joining the plots into a single one
final_plot <- (p2)  +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "least_vs_most_number_preteachers_edu.pdf"),
       device = "pdf", height = 8, width = 15)


# ---------------------------------------------------------------------------- #
#### 4.1.3.2 Table -----
# ---------------------------------------------------------------------------- #

# - Pre School - #

etable(
  est_most2, est_least2, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_pre_teachers_edu_studosage.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = c(
    "n_t_chi_edu" = "HE Child Education Teachers",
    "n_t_pre_edu" = "HE Pre-School Teachers"
  ),
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Teachers Education",
  label = "tab:lvm_stu_dosage_preschool_teachers_edu"
)




rm(p1, p2, p3, p4, final_plot,
   est_least1, est_least2, est_least3, est_least4,
   est_most1, est_most2, est_most3, est_most4)




# ---------------------------------------------------------------------------- #
# 5. School Characteristics ----
# ---------------------------------------------------------------------------- #
## 5.1 Main Regression (L vs. M) ----
# ---------------------------------------------------------------------------- #

# dependent variables in the order you want
y_vars <- c(
  "exp_classroom",
  "exp_teachroom",
  "exp_labs",
  "exp_library",
  "exp_playarea",
  "exp_lunch",
  "exp_no_water",
  "exp_water",
  "exp_no_sewage",
  "exp_sewage",
  "exp_no_energy",
  "exp_energy",
  "exp_employee",
  "exp_t_fun",
  "exp_t_pre",
  "exp_t_cre"
)

for (i in seq_along(y_vars)) {
  y <- y_vars[i]
  
  fml <- as.formula(
    paste0(y, " ~ i(ano, treat, ref = 2006) + PIBpc | codmun + ano + uf^ano")
  )
  
  assign(
    paste0("est_least", i),
    feols(
      fml,
      data = df_least,
      cluster = ~codmun
    )
  )
  
  assign(
    paste0("est_most", i),
    feols(
      fml,
      data = df_most,
      cluster = ~codmun
    )
  )
}

# ---------------------------------------------------------------------------- #
### 5.2.1 Plot ----
# ---------------------------------------------------------------------------- #

titles <- c(
  "Exp. Classroom",
  "Exp. Teacher's Room",
  "Exp. Lab",
  "Exp. Library",
  "Exp. Play Area",
  "Exp. Lunch",
  "Exp. No Water",
  "Exp. Water",
  "Exp. No Sewage",
  "Exp. Sewage",
  "Exp. No Energy",
  "Exp. Energy",
  "Exp. Employee",
  "Exp. DC Teachers",
  "Exp. PS Teachers",
  "Exp. MS Teachers"  #16
)

plots <- vector("list", length(titles))

for(i in seq_along(titles)) {
  
  est_most  <- get(paste0("est_most", i))
  est_least <- get(paste0("est_least", i))
  
  plots[[i]] <- event_plot_compare(
    est_most,
    est_least,
    name_1 = "Most Dosage",
    name_2 = "Least Dosage",
    ref_year = 2006,
    treat_year = 2007,
    b_size = 16,
    t_size = 13,
    x_label = "Time to Treatment",
    y_label = "Coefficient",
    title = titles[i]
  )
}

names(plots) <- paste0("p", seq_along(plots))

# ---- Saving Plot ---- #

# Joining the plots into a single one
final_plot <- 
  (plots[[2]] | plots[[3]] | plots[[4]]) /
  (plots[[5]] | plots[[6]] | plots[[1]]) /
  (plots[[14]] | plots[[15]] | plots[[16]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "least_vs_most_school_characteristics.pdf"),
       device = "pdf", height = 8, width = 15)



# Joining the plots into a single one
final_plot <- 
  (plots[[7]] | plots[[8]]) /
  (plots[[9]] | plots[[10]]) /
  (plots[[11]] | plots[[12]] ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "least_vs_most_school_characteristics2.pdf"),
       device = "pdf", height = 8, width = 15)

# ---------------------------------------------------------------------------- #
## 5.2 Tables ----
# ---------------------------------------------------------------------------- #

dictio <- c(
  "exp_classroom" = "Exp. Classroom",
  "exp_teachroom" = "Exp. Teacher's Room",
  "exp_labs"      = "Exp. Lab",
  "exp_library"   = "Exp. Library",
  "exp_playarea"  = "Exp. Play Area",
  "exp_lunch"     = "Exp. Lunch",
  "exp_no_water"  = "Exp. No Water",
  "exp_water"     = "Exp. Water",
  "exp_no_sewage" = "Exp. No Sewage",
  "exp_sewage"    = "Exp. Sewage",
  "exp_no_energy" = "Exp. No Energy",
  "exp_energy"    = "Exp. Energy",
  "exp_employee"  = "Exp. Employee",
  "exp_t_fun"     = "Exp. DC Teachers",
  "exp_t_pre"     = "Exp. PS Teachers",
  "exp_t_cre"     = "Exp. MS Teachers"
)




# - All Schools Infra - #

etable(
  est_most1, est_least1, est_most2, est_least2, est_most3, est_least3, est_most4, est_least4, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_school_characteristics1.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on School Characteristics",
  label = "tab:lvm_stu_dosage_school_charac1"
)

etable(
  est_most5, est_least5, est_most6, est_least6, est_most7, est_least7, est_most8, est_least8, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_school_characteristics2.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on School Characteristics",
  label = "tab:lvm_stu_dosage_school_charac2"
)


etable(
  est_most9, est_least9, est_most10, est_least10, est_most11, est_least11, est_most12, est_least12, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_school_characteristics3.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on School Characteristics",
  label = "tab:lvm_stu_dosage_school_charac3"
)


etable(
  est_most14, est_least14, est_most15, est_least15, est_most16, est_least16, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_school_characteristics4.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on School Characteristics",
  label = "tab:lvm_stu_dosage_school_charac4"
)


rm(est_least, est_least1, est_least2, est_least3, est_least4, est_least5,
   est_least6, est_least7, est_least8, est_least9, est_least10, est_least11,
   est_least12, est_least13, est_least14, est_least15, est_least16,
   est_most, est_most1, est_most2, est_most3, est_most4, est_most5, est_most6,
   est_most7, est_most8, est_most9, est_most10, est_most11, est_most12, est_most13,
   est_most14, est_most15,
   plots, final_plot, i, fml, y, y_vars)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
# ---------------------------------------------------------------------------- #
# 6. School Type Infra ----
# ---------------------------------------------------------------------------- #
## 6.1 Data ----
# ---------------------------------------------------------------------------- #

df_type <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/mun_prop_data_school_type_dosage.rds")


# ---------------------------------------------------------------------------- #
### 6.1.2 Least vs. Most ----
# ---------------------------------------------------------------------------- #

df_least <- df_type %>% 
  filter(dosage_tercile < 3) %>% #Least dosage group
  mutate(treat = ifelse(dosage_tercile != 2, 1, 0))

df_most <- df_type %>% 
  filter(dosage_tercile > 1) %>% #Most dosage group
  mutate(treat = ifelse(dosage_tercile != 2, 1, 0))



# ---------------------------------------------------------------------------- #
## 6.2 PRE School Characteristics ----
# ---------------------------------------------------------------------------- #
### 6.2.1 Main Regression (L vs. M) ----
# ---------------------------------------------------------------------------- #

# dependent variables in the order you want
y_vars <- c(
  "pre_exp_classroom",
  "pre_exp_teachroom",
  "pre_exp_labs",
  "pre_exp_library",
  "pre_exp_playarea",
  "pre_exp_lunch",
  "pre_exp_no_water",
  "pre_exp_water_dum",
  "pre_exp_no_sewage",
  "pre_exp_sewage_dum",
  "pre_exp_no_energy",
  "pre_exp_energy_dum",
  "pre_exp_employee"
)

for (i in seq_along(y_vars)) {
  y <- y_vars[i]
  
  fml <- as.formula(
    paste0(y, " ~ i(ano, treat, ref = 2006) + PIBpc | codmun + ano + uf^ano")
  )
  
  assign(
    paste0("est_least", i),
    feols(
      fml,
      data = df_least,
      cluster = ~codmun
    )
  )
  
  assign(
    paste0("est_most", i),
    feols(
      fml,
      data = df_most,
      cluster = ~codmun
    )
  )
}

# ---------------------------------------------------------------------------- #
### 6.2.2 Plot ----
# ---------------------------------------------------------------------------- #

titles <- c(
  "Exp. Classroom",
  "Exp. Teacher's Room",
  "Exp. Lab",
  "Exp. Library",
  "Exp. Play Area",
  "Exp. Lunch",
  "Exp. No Water",
  "Exp. Water",
  "Exp. No Sewage",
  "Exp. Sewage",
  "Exp. No Energy",
  "Exp. Energy",
  "Exp. Employee"
)

plots <- vector("list", length(titles))

for(i in seq_along(titles)) {
  
  est_most  <- get(paste0("est_most", i))
  est_least <- get(paste0("est_least", i))
  
  plots[[i]] <- event_plot_compare(
    est_most,
    est_least,
    name_1 = "Most Dosage",
    name_2 = "Least Dosage",
    ref_year = 2006,
    treat_year = 2007,
    b_size = 16,
    t_size = 13,
    x_label = "Time to Treatment",
    y_label = "Coefficient",
    title = titles[i]
  )
}

names(plots) <- paste0("p", seq_along(plots))

# ---- Saving Plot ---- #

# Joining the plots into a single one
final_plot <- 
  (plots[[2]] | plots[[3]] | plots[[4]]) /
  (plots[[5]] | plots[[6]] | plots[[1]])  +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "pre_least_vs_most_school_characteristics.pdf"),
       device = "pdf", height = 8, width = 15)



# Joining the plots into a single one
final_plot <- 
  (plots[[7]] | plots[[8]]) /
  (plots[[9]] | plots[[10]]) /
  (plots[[11]] | plots[[12]] ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "pre_least_vs_most_school_characteristics2.pdf"),
       device = "pdf", height = 8, width = 15)

# ---------------------------------------------------------------------------- #
### 6.2.3 Table ----
# ---------------------------------------------------------------------------- #


dictio <- c(
  "pre_exp_classroom" = "Exp. Classroom",
  "pre_exp_teachroom" = "Exp. Teacher's Room",
  "pre_exp_labs"      = "Exp. Lab",
  "pre_exp_library"   = "Exp. Library",
  "pre_exp_playarea"  = "Exp. Play Area",
  "pre_exp_lunch"     = "Exp. Lunch",
  "pre_exp_no_water"  = "Exp. No Water",
  "pre_exp_water"     = "Exp. Water",
  "pre_exp_no_sewage" = "Exp. No Sewage",
  "pre_exp_sewage"    = "Exp. Sewage",
  "pre_exp_no_energy" = "Exp. No Energy",
  "pre_exp_energy"    = "Exp. Energy",
  "pre_exp_employee"  = "Exp. Employee",
  "pre_exp_t_fun"     = "Exp. DC Teachers",
  "pre_exp_t_pre"     = "Exp. PS Teachers",
  "pre_exp_t_cre"     = "Exp. MS Teachers"
)




# - All Schools Infra - #

etable(
  est_most1, est_least1, est_most2, est_least2, est_most3, est_least3, est_most4, est_least4, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_preschool_characteristics1.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Characteristics",
  label = "tab:lvm_stu_dosage_preschool_charac1"
)

etable(
  est_most5, est_least5, est_most6, est_least6, est_most7, est_least7, est_most8, est_least8, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_preschool_characteristics2.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Characteristics",
  label = "tab:lvm_stu_dosage_preschool_charac2"
)


etable(
  est_most9, est_least9, est_most10, est_least10, est_most11, est_least11, est_most12, est_least12, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_preschool_characteristics3.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Characteristics",
  label = "tab:lvm_stu_dosage_preschool_charac3"
)



rm(est_least, est_least1, est_least2, est_least3, est_least4, est_least5,
   est_least6, est_least7, est_least8, est_least9, est_least10, est_least11,
   est_least12, est_least13, est_least14, est_least15, est_least16,
   est_most, est_most1, est_most2, est_most3, est_most4, est_most5, est_most6,
   est_most7, est_most8, est_most9, est_most10, est_most11, est_most12, est_most13,
   est_most14, est_most15,
   plots, final_plot, i, fml, y, y_vars)

