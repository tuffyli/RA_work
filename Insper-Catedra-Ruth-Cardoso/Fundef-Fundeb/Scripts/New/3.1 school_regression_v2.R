# ---------------------------------------------------------------------------- #
# Regressions - Version 4
# Last edited by: Tuffy Licciardi Issa
# Date: 21/07/2025
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
path_school_fig_nvar <- "Z:/Tuffy/Paper - Educ/Resultados/v4/School/Figures/Newvar/"

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

event_plot_dosage <- function(est_obj,
                              ref_year = 2006,
                              treat_year = 2007,
                              title = NULL,
                              y_label = "Coefficient",
                              x_label = "Time to Treatment",
                              ylim = NULL,
                              b_size = 20,
                              t_size = 18) {
  
  # Extract only the dosage interaction terms from the regression output
  event_df <- broom::tidy(est_obj, conf.int = TRUE) %>%
    mutate(
      term = as.character(term),
      
      # Match the two dosage groups
      group = case_when(
        str_detect(term, "least_dosage") ~ "Least dosage",
        str_detect(term, "high_dosage")  ~ "High dosage",
        TRUE ~ NA_character_
      ),
      
      # Extract the year from the coefficient name
      year = as.numeric(str_extract(term, "\\d{4}")),
      event_time = year - treat_year,
      coef_plot = estimate,
      ymin_plot = conf.low,
      ymax_plot = conf.high
    ) %>%
    filter(!is.na(group), !is.na(year)) %>%
    select(group, year, event_time, coef_plot, ymin_plot, ymax_plot)
  
  # Add the omitted reference year for both groups
  base_row <- tidyr::expand_grid(
    group = c("Least dosage", "High dosage"),
    year = ref_year
  ) %>%
    mutate(
      event_time = year - treat_year,
      coef_plot = 0,
      ymin_plot = NA_real_,
      ymax_plot = NA_real_
    )
  
  plot_df <- bind_rows(event_df, base_row) %>%
    mutate(
      group = factor(group, levels = c("Least dosage", "High dosage"))
    ) %>%
    arrange(group, year)
  
  p <- ggplot(
    plot_df,
    aes(
      x = event_time,
      y = coef_plot,
      color = group,
      linetype = group,
      shape = group,
      group = group
    )
  ) +
    geom_hline(yintercept = 0, color = "#D62728", linewidth = 1) +
    geom_vline(xintercept = -1, color = "grey40", linetype = "dashed") +
    
    geom_errorbar(
      aes(ymin = ymin_plot, ymax = ymax_plot),
      position = position_dodge(width = 0.25),
      width = 0.30,
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    geom_point(
      position = position_dodge(width = 0.25),
      size = 3,
      na.rm = TRUE
    ) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.25)) +
    
    scale_color_manual(
      values = c(
        "Least dosage" = "#D55E00",  # orange
        "High dosage"  = "#009E73"   # green
      )
    ) +
    scale_linetype_manual(
      values = c(
        "Least dosage" = "solid",
        "High dosage"  = "dashed"
      )
    ) +
    scale_shape_manual(
      values = c(
        "Least dosage" = 16,
        "High dosage"  = 17
      )
    ) +
    scale_x_continuous(
      breaks = sort(unique(plot_df$event_time)),
      labels = sort(unique(plot_df$event_time))
    ) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      color = NULL,
      linetype = NULL,
      shape = NULL
    ) +
    theme_minimal(base_size = b_size) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(),
      axis.text.y = element_text(size = t_size),
      axis.text.x = element_text(size = t_size, angle = 0, hjust = 0.5),
      legend.position = "bottom"
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

df_school <- df_school  %>%  #Variables for the least vs. most dosage analysis
  mutate(
      least_dosage = as.integer(dosage_tercile == 1),
      high_dosage  = as.integer(dosage_tercile == 3),

      #Post-treatment
      post_treat = ifelse(ano >= 2007, 1, 0)
    )

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


# -------------------------------------------------------------------------- #
#### 2.1.3.3 Table Aggregated -----
# -------------------------------------------------------------------------- #

est_ag <- feols(
  pre_pub_enroll ~
    least_dosage:i(ano, ref = 2006) +
    high_dosage:i(ano, ref = 2006) +
    PIBpc |
    codmun + ano + uf^ano,
  data = df_school,
  cluster = ~codmun
)

etable(est_ag,
  tex = TRUE,
  file = file.path(path_school,"/lvm_che_enroll_studosage_join.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = c(
    "inf_pub_enroll" = "Child Education Enrollment",
    "pre_pub_enroll" = "Pre-School Enrollment"
  ),
  title = "Least vs. Most: Effects of Fundef Student Dosage on Child Education Enrollment",
  label = "tab:lvm_stu_dosage_cheschool_enroll_join"
)


# --------------------------------------------------------------------------- #
#### 2.1.3.4 Plot ------
# --------------------------------------------------------------------------- #

plot <- event_plot_dosage(
  est_ag,
  title = "Enrollment",
  x_label = "Time to Treatment",
  y_label = "Coefficient"
)

ggsave(plot = plot,
       filename = file.path(path_school_fig_nvar, "enrollment_newvar.pdf"),
       device = "pdf", height = 8, width = 15)


rm(p1, p2, p3, p4, final_plot, plot,
   est_least1, est_least2, est_least3, est_least4, est_ag,
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

# -------------------------------------------------------------------------- #
### 3.3.3 Table Aggregated -----
# -------------------------------------------------------------------------- #

est_ag <- feols(
  n_pub_presco ~
    least_dosage:i(ano, ref = 2006) +
    high_dosage:i(ano, ref = 2006) +
    PIBpc |
    codmun + ano + uf^ano,
  data = df_school,
  cluster = ~codmun
)

etable(est_ag,
  tex = TRUE,
  file = file.path(path_school,"/lvm_pre_school_studosage_join.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = c(
    "n_pub_child" = "Child Education Schools",
    "n_pub_presco" = "Pre-Schools"
  ),
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-Schools",
  label = "tab:lvm_stu_dosage_preschool_join"
)

# ---------------------------------------------------------------------------- #
### 3.3.4 Plot ----
# ---------------------------------------------------------------------------- #


plot <- event_plot_dosage(
  est_ag,
  title = "Schools",
  x_label = "Time to Treatment",
  y_label = "Coefficient"
)

ggsave(plot = plot,
       filename = file.path(path_school_fig_nvar, "preschools_newvar.pdf"),
       device = "pdf", height = 8, width = 15)


rm(p1, p2, p3, p4, final_plot, plot,
   est_least1, est_least2, est_least3, est_least4, est_ag,
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

# -------------------------------------------------------------------------- #
#### 4.1.3.2 Table Aggregated -----
# -------------------------------------------------------------------------- #

est_ag <- feols(
  n_t_pre ~
    least_dosage:i(ano, ref = 2006) +
    high_dosage:i(ano, ref = 2006) +
    PIBpc |
    codmun + ano + uf^ano,
  data = df_school,
  cluster = ~codmun
)

etable(est_ag,
  tex = TRUE,
  file = file.path(path_school,"/lvm_pre_teachers_studosage_join.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = c(
    "n_t_chi" = "Child Education Teachers",
    "n_t_pre" = "Pre-School Teachers"
  ),
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Teachers",
  label = "tab:lvm_stu_dosage_preschool_teachers_join"
)

# --------------------------------------------------------------------------- #
#### 4.1.3.3 Plot -----
# --------------------------------------------------------------------------- #

plot <- event_plot_dosage(
  est_ag,
  title = "Teachers",
  x_label = "Time to Treatment",
  y_label = "Coefficient"
)

ggsave(plot = plot,
       filename = file.path(path_school_fig_nvar, "teachers_newvar.pdf"),
       device = "pdf", height = 8, width = 15)


rm(p1, p2, p3, p4, final_plot, plot,
   est_least1, est_least2, est_least3, est_least4, est_ag,
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

# -------------------------------------------------------------------------- #
#### 4.1.3.3 Table Aggregated -----
# -------------------------------------------------------------------------- #

est_ag <- feols(
  n_t_pre_edu ~
    least_dosage:i(ano, ref = 2006) +
    high_dosage:i(ano, ref = 2006) +
    PIBpc |
    codmun + ano + uf^ano,
  data = df_school,
  cluster = ~codmun
)

etable(est_ag,
  tex = TRUE,
  file = file.path(path_school,"/lvm_pre_teachers_edu_studosage_join.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = c(
    "n_t_chi_edu" = "HE Child Education Teachers",
    "n_t_pre_edu" = "HE Pre-School Teachers"
  ),
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Teachers Education",
  label = "tab:lvm_stu_dosage_preschool_teachers_edu_join"
)

# --------------------------------------------------------------------------- #
#### 4.1.3.4 Plot -----
# --------------------------------------------------------------------------- #

plot <- event_plot_dosage(
  est_ag,
  title = "HE Teachers",
  x_label = "Time to Treatment",
  y_label = "Coefficient"
)

ggsave(plot = plot,
       filename = file.path(path_school_fig_nvar, "teachers_edu_newvar.pdf"),
       device = "pdf", height = 8, width = 15)


rm(p1, p2, p3, p4, final_plot, plot,
   est_least1, est_least2, est_least3, est_least4, est_ag,
   est_most1, est_most2, est_most3, est_most4)

# ---------------------------------------------------------------------------- #
# 5. ATT estimation ----
# ---------------------------------------------------------------------------- #
## 5.1 New var ----
# ---------------------------------------------------------------------------- #

# -- Enrollment -- #
est_enr <- feols(pre_pub_enroll ~ high_dosage:post_treat + least_dosage:post_treat + PIBpc 
                  | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)

# -- N° Schools -- #
est_nsc <- feols(n_pub_presco ~ high_dosage:post_treat + least_dosage:post_treat + PIBpc 
                  | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)

# -- N° Teachers -- #

est_nte <- feols(n_t_pre ~ high_dosage:post_treat + least_dosage:post_treat + PIBpc 
                  | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)

# -- N° Teachers Edu -- #

est_ted <- feols(n_t_pre_edu ~ high_dosage:post_treat + least_dosage:post_treat + PIBpc 
                  | codmun + ano + uf^ano,
                data = df_school,
                cluster = ~codmun)

# ---------------------------------------------------------------------------- #
### 5.1.1 Table ----
# ---------------------------------------------------------------------------- #

etable(est_enr, est_nsc, est_nte, est_ted,
  drop = "PIBpc",
  dict = c(
      "pre_pub_enroll" = "Enrollment",
      "n_pub_presco"   = "Schools",
      "n_t_pre"        = "Teachers",
      "n_t_pre_edu"    = "Highly Educated Teachers"
  ),
  file = file.path(path_school,"/join_preschool_att.tex"),
  digits = 3,
  replace = T,
  title = "Pre-School ATT")

rm(est_enr, est_nsc, est_nte, est_ted)

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
  (plots[[2]] | plots[[3]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "least_vs_most_school_characteristics1.pdf"),
       device = "pdf", height = 8, width = 15)


final_plot <- 
  (plots[[4]] | plots[[5]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "least_vs_most_school_characteristics2.pdf"),
       device = "pdf", height = 8, width = 15)


final_plot <- 
  (plots[[6]] | plots[[1]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "least_vs_most_school_characteristics3.pdf"),
       device = "pdf", height = 8, width = 15)

final_plot <- 
  (plots[[14]] | plots[[15]]) /
  (plots[[16]] | plot_spacer()) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "least_vs_most_school_characteristics4.pdf"),
       device = "pdf", height = 8, width = 15)



# Water
final_plot <- 
  (plots[[7]] | plots[[8]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "least_vs_most_school_characteristics_wat.pdf"),
       device = "pdf", height = 8, width = 15)

# Sewage
final_plot <- 
  (plots[[9]] | plots[[10]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "least_vs_most_school_characteristics_sew.pdf"),
       device = "pdf", height = 8, width = 15)

# Energy
final_plot <- 
  (plots[[11]] | plots[[12]] ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot

ggsave(plot = final_plot,
       filename = file.path(path_school_fig, "least_vs_most_school_characteristics_ene.pdf"),
       device = "pdf", height = 8, width = 15)

# ---------------------------------------------------------------------------- #
## 6.2 Tables ----
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
  est_most1, est_least1, est_most2, est_least2, 
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
  est_most3, est_least3, est_most4, est_least4, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_school_characteristics2.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on School Characteristics",
  label = "tab:lvm_stu_dosage_school_charac1"
)

etable(
  est_most5, est_least5, est_most6, est_least6, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_school_characteristics3.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on School Characteristics",
  label = "tab:lvm_stu_dosage_school_charac2"
)

etable(
  est_most7, est_least7, est_most8, est_least8, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_school_characteristics4.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on School Characteristics",
  label = "tab:lvm_stu_dosage_school_charac2"
)


etable(
  est_most9, est_least9, est_most10, est_least10,
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_school_characteristics5.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on School Characteristics",
  label = "tab:lvm_stu_dosage_school_charac3"
)

etable(
  est_most11, est_least11, est_most12, est_least12, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_school_characteristics6.tex"),
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
  file = file.path(path_school,"/lvm_least_vs_most_school_characteristics7.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on School Characteristics",
  label = "tab:lvm_stu_dosage_school_charac4"
)

# ---------------------------------------------------------------------------- #
### 6.2.1 Table Aggregated -----
# ---------------------------------------------------------------------------- #

# -- Regression -- #

for (i in seq_along(y_vars)) {
  y <- y_vars[i]
  
  fml <- as.formula(
    paste0(y, " ~ least_dosage:i(ano, ref = 2006) +
              high_dosage:i(ano, ref = 2006) + PIBpc | codmun + ano + uf^ano")
  )
  
  assign(
    paste0("est_ag", i),
    feols(
      fml,
      data = df_school,
      cluster = ~codmun
    )
  )
}

# -- Saving Table -- #

etable(
  est_ag1, est_ag2, est_ag3,
  tex = TRUE,
  file = file.path(path_school,"/join_lvm_least_vs_most_school_characteristics1.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on School Characteristics",
  label = "tab:join_lvm_stu_dosage_school_charac1"
)

etable(
  est_ag4, est_ag5, est_ag6,
  tex = TRUE,
  file = file.path(path_school,"/join_lvm_least_vs_most_school_characteristics2.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on School Characteristics",
  label = "tab:join_lvm_stu_dosage_school_charac1"
)

etable(
  est_ag7, est_ag8, est_ag9, 
  tex = TRUE,
  file = file.path(path_school,"/join_lvm_least_vs_most_school_characteristics3.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on School Characteristics",
  label = "tab:join_lvm_stu_dosage_school_charac2"
)

etable(
  est_ag10, est_ag11, est_ag12, 
  tex = TRUE,
  file = file.path(path_school,"/join_lvm_least_vs_most_school_characteristics4.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on School Characteristics",
  label = "tab:join_lvm_stu_dosage_school_charac2"
)

etable(
  est_ag14, est_ag15, est_ag16, 
  tex = TRUE,
  file = file.path(path_school,"/join_lvm_least_vs_most_school_characteristics5.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on School Characteristics",
  label = "tab:join_lvm_stu_dosage_school_charac3"
)

# ------------------------------------------------------------------------------ #
#### 6.2.1.1 Plot ----
# ------------------------------------------------------------------------------ #

# Build one plot per aggregated regression
plots_ag <- vector("list", length(titles))

for (i in seq_along(titles)) {
  est_i <- get(paste0("est_ag", i))
  
  plots_ag[[i]] <- event_plot_dosage(
    est_i,
    ref_year = 2006,
    treat_year = 2007,
    b_size = 16,
    t_size = 13,
    x_label = "Time to Treatment",
    y_label = "Coefficient",
    title = titles[i]
  )
}

names(plots_ag) <- paste0("pag", seq_along(plots_ag))

final_plot_1 <- wrap_plots(plots_ag[c(1:2)], ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot_2 <- wrap_plots(plots_ag[c(3:4)], ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot_3 <- wrap_plots(plots_ag[c(5:6)], ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot_4 <- wrap_plots(plots_ag[c(13:16)], ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#Water
final_plot_5 <- wrap_plots(plots_ag[7:8], ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#Sewage
final_plot_6 <- wrap_plots(plots_ag[9:10], ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#Energy
final_plot_7 <- wrap_plots(plots_ag[11:12], ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  plot = final_plot_1,
  filename = file.path(path_school_fig_nvar, "school_characteristics_1.pdf"),
  device = "pdf",
  height = 8,
  width = 15
)

ggsave(
  plot = final_plot_2,
  filename = file.path(path_school_fig_nvar, "school_characteristics_2.pdf"),
  device = "pdf",
  height = 8,
  width = 15
)

ggsave(
  plot = final_plot_3,
  filename = file.path(path_school_fig_nvar, "school_characteristics_3.pdf"),
  device = "pdf",
  height = 8,
  width = 15
)

ggsave(
  plot = final_plot_4,
  filename = file.path(path_school_fig_nvar, "school_characteristics_4.pdf"),
  device = "pdf",
  height = 8,
  width = 15
)

ggsave(
  plot = final_plot_5,
  filename = file.path(path_school_fig_nvar, "school_characteristics_5.pdf"),
  device = "pdf",
  height = 8,
  width = 15
)

ggsave(
  plot = final_plot_6,
  filename = file.path(path_school_fig_nvar, "school_characteristics_6.pdf"),
  device = "pdf",
  height = 8,
  width = 15
)

ggsave(
  plot = final_plot_7,
  filename = file.path(path_school_fig_nvar, "school_characteristics_7.pdf"),
  device = "pdf",
  height = 8,
  width = 15
)

rm(est_least, est_least1, est_least2, est_least3, est_least4, est_least5,
   est_least6, est_least7, est_least8, est_least9, est_least10, est_least11,
   est_least12, est_least13, est_least14, est_least15, est_least16,
   est_most, est_most1, est_most2, est_most3, est_most4, est_most5, est_most6,
   est_most7, est_most8, est_most9, est_most10, est_most11, est_most12, est_most13,
   est_most14, est_most15, est_most16,
   est_ag1, est_ag2, est_ag3, est_ag4, est_ag5, est_ag6, est_ag7, est_ag8, est_ag9,
   est_ag10, est_ag11, est_ag12, est_ag13, est_ag14, est_ag15, est_ag16,
   plots_ag, final_plot_1, final_plot_2, final_plot_3, final_plot_4, final_plot_5,
   final_plot_6, final_plot_7,
   plots, final_plot, i, fml, y, y_vars)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
# ---------------------------------------------------------------------------- #
# 7. School Type Infra ----
# ---------------------------------------------------------------------------- #
## 7.1 Data ----
# ---------------------------------------------------------------------------- #

df_type <- readRDS("Z:/Tuffy/Paper - Educ/Dados/intermediate/afl_mun_prop_data_school_type_dosage.rds")

df_type <- df_type %>% 
  mutate(
      least_dosage = as.integer(dosage_tercile == 1),
      high_dosage  = as.integer(dosage_tercile == 3)
    )

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
## 7.2 PRE School Characteristics ----
# ---------------------------------------------------------------------------- #
### 7.2.1 Main Regression (L vs. M) ----
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
  "pre_exp_energy_dum"
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
  "Exp. Energy"
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

# Pair plots two by two: (1,2), (3,4), ..., (15,16)
pair_idx <- split(seq_along(plots), ceiling(seq_along(plots) / 2))

for (i in seq_along(pair_idx)) {
  idx <- pair_idx[[i]]

  final_plot <-
    plots[[idx[1]]] | plots[[idx[2]]] +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  print(final_plot)

  ggsave(
    plot = final_plot,
    filename = file.path(
      path_school_fig,
      paste0("pre_least_vs_most_school_characteristics", i, ".pdf")
    ),
    device = "pdf",
    height = 8,
    width = 15
  )
}

# ---------------------------------------------------------------------------- #
### 7.2.3 Table ----
# ---------------------------------------------------------------------------- #


dictio <- c(
  "pre_exp_classroom"  = "Exp. Classroom",
  "pre_exp_teachroom"  = "Exp. Teacher's Room",
  "pre_exp_labs"       = "Exp. Lab",
  "pre_exp_library"    = "Exp. Library",
  "pre_exp_playarea"   = "Exp. Play Area",
  "pre_exp_lunch"      = "Exp. Lunch",
  "pre_exp_no_water"   = "Exp. No Water",
  "pre_exp_water_dum"  = "Exp. Water",
  "pre_exp_no_sewage"  = "Exp. No Sewage",
  "pre_exp_sewage_dum" = "Exp. Sewage",
  "pre_exp_no_energy"  = "Exp. No Energy",
  "pre_exp_energy_dum" = "Exp. Energy",
  "pre_exp_employee"   = "Exp. Employee",
  "pre_exp_t_fun"      = "Exp. DC Teachers",
  "pre_exp_t_pre"      = "Exp. PS Teachers",
  "pre_exp_t_cre"      = "Exp. MS Teachers"
)




# - All Schools Infra - #

etable(
  est_most1, est_least1, est_most2, est_least2,
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
  est_most3, est_least3, est_most4, est_least4, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_preschool_characteristics2.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Characteristics",
  label = "tab:lvm_stu_dosage_preschool_charac1"
)

etable(
  est_most5, est_least5, est_most6, est_least6, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_preschool_characteristics3.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Characteristics",
  label = "tab:lvm_stu_dosage_preschool_charac2"
)

etable(
  est_most7, est_least7, est_most8, est_least8, 
  tex = TRUE,
  file = file.path(path_school,"/lvm_least_vs_most_preschool_characteristics4.tex"),
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
  file = file.path(path_school,"/lvm_least_vs_most_preschool_characteristics5.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Characteristics",
  label = "tab:lvm_stu_dosage_preschool_charac3"
)

# ---------------------------------------------------------------------------- #
### 7.2.4 Table Aggregated -----
# ---------------------------------------------------------------------------- #

# -- Regression -- #

for (i in seq_along(y_vars)) {
  y <- y_vars[i]
  
  fml <- as.formula(
    paste0(y, " ~ least_dosage:i(ano, ref = 2006) +
              high_dosage:i(ano, ref = 2006) + PIBpc | codmun + ano + uf^ano")
  )
  
  assign(
    paste0("est_ag", i),
    feols(
      fml,
      data = df_type,
      cluster = ~codmun
    )
  )
}

# -- Saving Table -- #

etable(
  est_ag1, est_ag2, est_ag3,
  tex = TRUE,
  file = file.path(path_school,"/join_lvm_least_vs_most_preschool_characteristics1.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Characteristics",
  label = "tab:join_lvm_stu_dosage_school_charac1"
)

etable(
  est_ag4, est_ag5, est_ag6,
  tex = TRUE,
  file = file.path(path_school,"/join_lvm_least_vs_most_preschool_characteristics2.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Characteristics",
  label = "tab:join_lvm_stu_dosage_school_charac1"
)

etable(
  est_ag7, est_ag8, est_ag9, 
  tex = TRUE,
  file = file.path(path_school,"/join_lvm_least_vs_most_preschool_characteristics3.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Characteristics",
  label = "tab:join_lvm_stu_dosage_school_charac2"
)

etable(
  est_ag10, est_ag11, est_ag12,
  tex = TRUE,
  file = file.path(path_school,"/join_lvm_least_vs_most_preschool_characteristics4.tex"),
  digits = 3,
  replace = T,
  drop = "PIBpc",
  dict = dictio,
  title = "Least vs. Most: Effects of Fundef Student Dosage on Pre-School Characteristics",
  label = "tab:join_lvm_stu_dosage_school_charac2"
)

# ------------------------------------------------------------------------------ #
#### 7.2.4.1 Plot ----
# ------------------------------------------------------------------------------ #

# Build one plot per aggregated regression
plots_ag <- vector("list", length(titles))

for (i in seq_along(titles)) {
  est_i <- get(paste0("est_ag", i))
  
  plots_ag[[i]] <- event_plot_dosage(
    est_i,
    ref_year = 2006,
    treat_year = 2007,
    b_size = 16,
    t_size = 13,
    x_label = "Time to Treatment",
    y_label = "Coefficient",
    title = titles[i]
  )
}

names(plots_ag) <- paste0("pag", seq_along(plots_ag))

for (i in seq(1, length(plots_ag), by = 2)) {

  final_plot <- wrap_plots(
    plots_ag[i:min(i + 1, length(plots_ag))],
    ncol = 2
  ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  ggsave(
    plot = final_plot,
    filename = file.path(
      path_school_fig_nvar,
      paste0("pre_school_characteristics_", (i + 1) %/% 2, ".pdf")
    ),
    device = "pdf",
    height = 8,
    width = 15
  )
}

rm(est_least, est_least1, est_least2, est_least3, est_least4, est_least5,
   est_least6, est_least7, est_least8, est_least9, est_least10, est_least11,
   est_least12,
   est_most, est_most1, est_most2, est_most3, est_most4, est_most5, est_most6,
   est_most7, est_most8, est_most9, est_most10, est_most11, est_most12, 
   est_ag1, est_ag2, est_ag3, est_ag4, est_ag5, est_ag6, est_ag7, est_ag8, est_ag9,
   est_ag10, est_ag11, est_ag12, est_ag13,
   dictio, df_least, df_most, df_school, df_type, 
   plots_ag, est_i, idx, pair_idx,
   plots, final_plot, i, fml, y, y_vars)










