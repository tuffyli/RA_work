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

path_school2 <- "Z:/Tuffy/Paper - Educ/Resultados/v4/School/Mun_aff/"
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


df_school <- readRDS("Z:/Tuffy/Paper - Educ/Dados/final/mun_school_data_affiliations.rds")


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
  file = file.path(path_school2,"/enrollment_school_studosage.tex"),
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
       filename = file.path(path_school_fig2, "enrollment_specification.pdf"),
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
final_plot <- (p3 | p2) / (p1 | p4)  +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig2, "least_vs_most_enrollment_specification.pdf"),
       device = "pdf", height = 8, width = 15)

rm(p1, p2, p3, p4, final_plot,
   est_least1, est_least2, est_least3, est_least4,
   est_most1, est_most2, est_most3, est_most4)

# ---------------------------------------------------------------------------- #
## 2.2 Enrollment proportion ----
# ---------------------------------------------------------------------------- #
### 2.2.1 Table -----
# ---------------------------------------------------------------------------- #

#Main
est_1 <- feols(prop_pub_inf ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
               | codmun + ano + uf^ano,
               data = df_school,
               cluster = ~codmun)
#Inf
est_2 <- feols( prop_pub_fun ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)


etable(est_1, est_2)

# ---- Saving Table ---- #

etable(
  est_1, est_2,
  tex = TRUE,
  file = file.path(path_school2,"/enrollment_studosage_school_proportion.tex"),
  digits = 3,
  replace = T,
  title = "Effects of Fundef Student Dosage on Education Enrollment",
  label = "tab:stu_dosage_school_enroll_proportion"
)

# ---------------------------------------------------------------------------- #
### 2.1.2 Figure ----
# ---------------------------------------------------------------------------- #

#Ploting for each specification

p3 <- event_plot(
  est_1,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Child Education Proportion"
)

p4 <- event_plot(
  est_2,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Middle School Proportion"
)

# Joining the plots into a single one
final_plot <- (p3 | p4)

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig2, "enrollment_specification_proportion.pdf"),
       device = "pdf", height = 8, width = 15)

rm(p1, p2, p3, p4, final_plot,
   est_1, est_2)



# ---------------------------------------------------------------------------- #
### 2.1.3 Most vs. Least ----
# ---------------------------------------------------------------------------- #


# ---- Total Child Education ---- #
est_least1 <- feols(
  prop_pub_inf ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data =  df_least,
  cluster = ~codmun
)

est_most1 <- feols(
  prop_pub_inf ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least1, est_most1)


# ---- Middle School ---- #
est_least4 <- feols(
  prop_pub_fun ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_least,
  cluster = ~codmun
)

est_most4 <- feols(
  prop_pub_fun ~ i(ano, treat, ref = 2006) + PIBpc |
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
final_plot <- (p1 | p4)  +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig2, "least_vs_most_enrollment_specification_proportion.pdf"),
       device = "pdf", height = 8, width = 15)

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
  file = file.path(path_school2,"/number_school_studosage.tex"),
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
       filename = file.path(path_school_fig2, "number_schools.pdf"),
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
final_plot <- (p1 | p2) / (p3 | plot_spacer())  +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig2, "least_vs_most_number_schools.pdf"),
       device = "pdf", height = 8, width = 15)

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
  file = file.path(path_school2,"/number_teachers_studosage.tex"),
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
       filename = file.path(path_school_fig2, "number_teachers.pdf"),
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
final_plot <- (p1 | p2) / (p3 | plot_spacer())  +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig2, "least_vs_most_number_teachers.pdf"),
       device = "pdf", height = 8, width = 15)

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
  file = file.path(path_school2,"number_teachers_edu_studosage.tex"),
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
       filename = file.path(path_school_fig2, "number_teachers_edu.pdf"),
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
final_plot <- (p1 | p2) / (p3 | plot_spacer())  +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig2, "least_vs_most_number_teachers_edu.pdf"),
       device = "pdf", height = 8, width = 15)

rm(p1, p2, p3, p4, final_plot,
   est_least1, est_least2, est_least3, est_least4,
   est_most1, est_most2, est_most3, est_most4)



# ---------------------------------------------------------------------------- #
# 5. Age exposition -----
# ---------------------------------------------------------------------------- #

#quick plot

# 2007 only
df_2007 <- df_school %>%
  filter(ano == 2007)

#---------------------------------------------------#
# Infant education
#---------------------------------------------------#

p_inf <- ggplot(df_2007,
                aes(x = prop_pub_inf,
                    y = aluno_dosage)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm",
              se = TRUE,
              color = "blue") +
  labs(
    x = "Public Infant Enrollment Rate",
    y = "Student Dosage",
    title = "2007"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(path_descp,"scatter_dosage_public_infant_2007.png"),
  plot = p_inf,
  width = 7,
  height = 5,
  dpi = 300
)

#--------------------------------------------------- #
# Fundamental education
#--------------------------------------------------- #

p_fun <- ggplot(df_2007,
                aes(x = prop_pub_fun,
                    y = aluno_dosage)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm",
              se = TRUE,
              color = "blue") +
  labs(
    x = "Public Fundamental Enrollment Rate",
    y = "Student Dosage",
    title = "2007"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(path_descp,"scatter_dosage_public_fundamental_2007.png"),
  plot = p_fun,
  width = 7,
  height = 5,
  dpi = 300
)


# ---------------------------------------------------------------------------- #
## 5.1 Table ----
# ---------------------------------------------------------------------------- #

#Main
est_1 <- feols(prop_pub_inf ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
               | codmun + ano + uf^ano,
               data = df_school,
               cluster = ~codmun)
#Inf
est_2 <- feols( prop_pub_fun ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)



etable(est_1, est_2)

# ---- Saving Table ---- #

etable(
  est_1, est_2,
  tex = TRUE,
  file = file.path(path_school2,"/age_enroll_studosage.tex"),
  digits = 3,
  replace = T,
  title = "Effects of Fundef Student Dosage on School Pariticipation",
  label = "tab:stu_dosage_age_enroll"
)

# ---------------------------------------------------------------------------- #
## 5.2 Figure ----
# ---------------------------------------------------------------------------- #

#Ploting for each specification
p1 <- event_plot(
  est_1,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Child Education Prop."
)

p2 <- event_plot(
  est_2,
  ref_year = 2006,
  x_label = "Time to Treatment",
  y_label = "Coefficient",
  title = "Middle School Prop."
)



# Joining the plots into a single one
final_plot <- (p1 | p2)

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig2, "age_prop.pdf"),
       device = "pdf", height = 8, width = 15)

rm(p1, p2, p3, p4, final_plot,
   est_1, est_2, est_3, est_4)

# ---------------------------------------------------------------------------- #
## 5.3 Most vs. Least ----
# ---------------------------------------------------------------------------- #


# ---- Total Child Education ---- #
est_least1 <- feols(
  prop_pub_inf ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data =  df_least,
  cluster = ~codmun
)

est_most1 <- feols(
  prop_pub_inf ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least1, est_most1)



# ---- Pre-School ---- #
est_least2 <- feols(
  prop_pub_fun ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_least,
  cluster = ~codmun
)

est_most2 <- feols(
  prop_pub_fun ~ i(ano, treat, ref = 2006) + PIBpc |
    codmun + ano + uf^ano,
  data = df_most,
  cluster = ~codmun
)

etable(est_least2, est_most2)



# ---------------------------------------------------------------------------- #
### 5.3.1 Figure ----
# ---------------------------------------------------------------------------- #


# ---- Total Spending ---- #
p1 <- event_plot_compare( est_most1, est_least1,
                          name_1 = "Most Dosage", name_2 = "Least Dosage",
                          ref_year = 2006, treat_year = 2007,
                          x_label = "Time to Treatment",
                          y_label = "Coefficient",
                          title = "Child Education Prop.")

p1

# ---- Pre-School ---- #
p2 <- event_plot_compare(est_most2, est_least2,
                         name_1 = "Most Dosage", name_2 = "Least Dosage",
                         ref_year = 2006, treat_year = 2007,
                         x_label = "Time to Treatment",
                         y_label = "Coefficient",
                         title = "Middle School Prop.")

p2



# Joining the plots into a single one
final_plot <- (p1 | p2)  +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig2, "least_vs_most_age_prop.pdf"),
       device = "pdf", height = 8, width = 15)

rm(p1, p2, p3, p4, final_plot,
   est_least1, est_least2, est_least3, est_least4,
   est_most1, est_most2, est_most3, est_most4)




# ---------------------------------------------------------------------------- #
# 6. School Characteristics ----
# ---------------------------------------------------------------------------- #
## 6.1 Main Regression (L vs. M) ----
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
### 6.2.1 Plot ----
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
  "Exp. MS Teachers"
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
       filename = file.path(path_school_fig2, "least_vs_most_school_characteristics.pdf"),
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
       filename = file.path(path_school_fig2, "least_vs_most_school_characteristics2.pdf"),
       device = "pdf", height = 8, width = 15)

rm(est_least, est_least1, est_least2, est_least3, est_least4, est_least5,
   est_least6, est_least7, est_least8, est_least9, est_least10, est_least11,
   est_least12, est_least13, est_least14, est_least15, est_least16,
   est_most, est_most1, est_most2, est_most3, est_most4, est_most5, est_most6,
   est_most7, est_most8, est_most9, est_most10, est_most11, est_most12, est_most13,
   est_most14, est_most15,
   plots, final_plot, i, fml, y, y_vars)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
# ---------------------------------------------------------------------------- #
# 7. School Type Infra ----
# ---------------------------------------------------------------------------- #
## 7.1 Data ----
# ---------------------------------------------------------------------------- #

df_type <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/afl_mun_prop_data_school_type_dosage.rds")


# ---------------------------------------------------------------------------- #
### 7.1.2 Least vs. Most ----
# ---------------------------------------------------------------------------- #

df_least <- df_type %>% 
  filter(dosage_tercile < 3) %>% #Least dosage group
  mutate(treat = ifelse(dosage_tercile != 2, 1, 0))

df_most <- df_type %>% 
  filter(dosage_tercile > 1) %>% #Most dosage group
  mutate(treat = ifelse(dosage_tercile != 2, 1, 0))


# ---------------------------------------------------------------------------- #
## 7.2 CRE School Characteristics ----
# ---------------------------------------------------------------------------- #
### 7.2.1 Main Regression (L vs. M) ----
# ---------------------------------------------------------------------------- #

# dependent variables in the order you want
y_vars <- c(
  "cre_exp_classroom",
  "cre_exp_teachroom",
  "cre_exp_labs",
  "cre_exp_library",
  "cre_exp_playarea",
  "cre_exp_lunch",
  "cre_exp_no_water",
  "cre_exp_water_dum",
  "cre_exp_no_sewage",
  "cre_exp_sewage_dum",
  "cre_exp_no_energy",
  "cre_exp_energy_dum",
  "cre_exp_employee"
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
### 7.2.2 Plot ----
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
       filename = file.path(path_school_fig2, "cre_least_vs_most_school_characteristics.pdf"),
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
       filename = file.path(path_school_fig2, "cre_least_vs_most_school_characteristics2.pdf"),
       device = "pdf", height = 8, width = 15)


# ---------------------------------------------------------------------------- #
## 7.3 PRE School Characteristics ----
# ---------------------------------------------------------------------------- #
### 7.3.1 Main Regression (L vs. M) ----
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
### 7.3.2 Plot ----
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
       filename = file.path(path_school_fig2, "pre_least_vs_most_school_characteristics.pdf"),
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
       filename = file.path(path_school_fig2, "pre_least_vs_most_school_characteristics2.pdf"),
       device = "pdf", height = 8, width = 15)

# ---------------------------------------------------------------------------- #
## 7.4 FUN School Characteristics ----
# ---------------------------------------------------------------------------- #
### 7.4.1 Main Regression (L vs. M) ----
# ---------------------------------------------------------------------------- #

# dependent variables in the order you want
y_vars <- c(
  "fun_exp_classroom",
  "fun_exp_teachroom",
  "fun_exp_labs",
  "fun_exp_library",
  "fun_exp_playarea",
  "fun_exp_lunch",
  "fun_exp_no_water",
  "fun_exp_water_dum",
  "fun_exp_no_sewage",
  "fun_exp_sewage_dum",
  "fun_exp_no_energy",
  "fun_exp_energy_dum",
  "fun_exp_employee"
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
### 7.4.2 Plot ----
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
       filename = file.path(path_school_fig2, "fun_least_vs_most_school_characteristics.pdf"),
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
       filename = file.path(path_school_fig2, "fun_least_vs_most_school_characteristics2.pdf"),
       device = "pdf", height = 8, width = 15)


rm(est_least, est_least1, est_least2, est_least3, est_least4, est_least5,
   est_least6, est_least7, est_least8, est_least9, est_least10, est_least11,
   est_least12, est_least13, est_least14, est_least15, est_least16,
   est_most, est_most1, est_most2, est_most3, est_most4, est_most5, est_most6,
   est_most7, est_most8, est_most9, est_most10, est_most11, est_most12, est_most13,
   est_most14, est_most15,
   plots, final_plot, i, fml, y, y_vars)

