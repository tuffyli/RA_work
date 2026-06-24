# ---------------------------------------------------------------------------- #
# Data Description - Version 4
# Last edited by: Tuffy Licciardi Issa
# Date: 19/06/2025
# ---------------------------------------------------------------------------- #


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

# ------------------------------------------------------------------- #
# Helper function:
# Computes yearly means by tercile and returns a ggplot object
# ------------------------------------------------------------------- #
make_tercile_plot <- function(data, outcome_var, plot_title,
                              y_limits = NULL,
                              x_label = "Year",
                              y_label = "Mean Log Enrollment") {
  
  # Build summary data by year and dosage tercile
  df_mean_terc <- data %>%
    group_by(ano, dosage_tercile) %>%
    summarise(
      n = sum(!is.na(.data[[outcome_var]])),
      mean_val = mean(.data[[outcome_var]], na.rm = TRUE),
      sd_val   = sd(.data[[outcome_var]], na.rm = TRUE),
      se_val   = sd_val / sqrt(n),
      .groups  = "drop"
    ) %>%
    
    # Convert tercile codes into readable labels
    mutate(
      tercile = factor(
        dosage_tercile,
        levels = c(1, 2, 3),
        labels = c("Low Dosage", "Medium Dosage", "High Dosage")
      )
    )
  
  # Build the plot
  p <- ggplot(
    df_mean_terc,
    aes(
      x = ano,
      y = mean_val,
      color = tercile,
      linetype = tercile,
      shape = tercile,
      group = tercile
    )
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.8) +
    
    # Policy year
    geom_vline(
      xintercept = 2007,
      linetype = "longdash",
      color = "grey50"
    ) +
    
    # Nice color palette with good contrast
    scale_color_manual(
      values = c(
        "Low Dosage"    = "#0072B2",
        "Medium Dosage" = "#D55E00",
        "High Dosage"   = "#009E73"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "Low Dosage"    = "solid",
        "Medium Dosage" = "dashed",
        "High Dosage"   = "dotdash"
      )
    ) +
    scale_shape_manual(
      values = c(
        "Low Dosage"    = 16,
        "Medium Dosage" = 17,
        "High Dosage"   = 15
      )
    ) +
    labs(
      title = plot_title,
      x = x_label,
      y = y_label,
      color = NULL,
      linetype = NULL,
      shape = NULL
    ) +
    scale_x_continuous(
      breaks = seq(min(df_mean_terc$ano), max(df_mean_terc$ano), by = 2)
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_blank()
    )
  
  # Optional y-axis limits without dropping data
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
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
    log_mat_total = if_else(mat_total > 0, log(mat_total), NA_real_),
    log_mat_fun   = if_else(mat_fun > 0, log(mat_fun), NA_real_),
    log_mat_med   = if_else(mat_med > 0, log(mat_med), NA_real_),
    log_mat_inf   = if_else(mat_inf > 0, log(mat_inf), NA_real_)
  )

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
# 2. Enrollment data ----
# ---------------------------------------------------------------------------- #
## 2.1 Log(Enroll) ----
# ------------------------------------------------------------------- #


# Outcomes and titles
# ------------------------------------------------------------------- #
outcomes <- c(
  "log_mat_total",
  "log_mat_inf",
  "log_mat_fun",
  "log_mat_med"
)

titles <- c(
  "Total Enrollment",
  "Pre-School Enrollment",
  "Middle School Enrollment",
  "High School Enrollment"
  )

# Optional y-limits for each plot
ylims <- list(
 NULL,
  NULL,
  NULL,
  NULL
)


# ------------------------------------------------------------------- #
# Create all four plots
# ------------------------------------------------------------------- #
plots <- vector("list", length(outcomes))

for (i in seq_along(outcomes)) {
  plots[[i]] <- make_tercile_plot(
    data = df_main,
    outcome_var = outcomes[i],
    plot_title = titles[i],
    y_limits = ylims[[i]]
  )
}


# ------------------------------------------------------------------- #
# Combine plots into a final panel
# ------------------------------------------------------------------- #
plot_final <- wrap_plots(plots, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

plot_final


ggsave(plot = plot_final,
       filename = file.path(path_descp, "/Figures/enrollment_log.pdf"),
       device = "pdf", height = 8, width = 15)

# ---------------------------------------------------------------------------- #
## 2.2 Lvl Enroll ----
# ---------------------------------------------------------------------------- #
# Outcomes and titles
# ------------------------------------------------------------------- #
outcomes <- c(
  "mat_total",
  "mat_inf",
  "mat_fun",
  "mat_med"
)

titles <- c(
  "Total Enrollment",
  "Pre-School Enrollment",
  "Middle School Enrollment",
  "High School Enrollment"
)

# Optional y-limits for each plot
ylims <- list(
  NULL,
  NULL,
  NULL,
  NULL
)


# ------------------------------------------------------------------- #
# Create all four plots
# ------------------------------------------------------------------- #
plots <- vector("list", length(outcomes))

for (i in seq_along(outcomes)) {
  plots[[i]] <- make_tercile_plot(
    data = df_main,
    outcome_var = outcomes[i],
    plot_title = titles[i],
    y_limits = ylims[[i]]
  )
}


# ------------------------------------------------------------------- #
# Combine plots into a final panel
# ------------------------------------------------------------------- #
plot_final <- wrap_plots(plots, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

plot_final


ggsave(plot = plot_final,
       filename = file.path(path_descp, "/Figures/enrollment_lvl.pdf"),
       device = "pdf", height = 8, width = 15)



# ---------------------------------------------------------------------------- #
# 3. Spending ----
# ---------------------------------------------------------------------------- #

outcomes <- c(
  "real_des_edu",
  "real_des_inf",
  "real_des_fund",
  "real_des_med"
)

titles <- c(
  "Total Education Spending",
  "Pre-School Spending",
  "Middle School Spending",
  "High School Spending"
)


# Optional y-limits for each plot
ylims <- list(
  NULL,
  NULL,
  NULL,
  NULL
)


# ------------------------------------------------------------------- #
# Create all four plots
# ------------------------------------------------------------------- #
plots <- vector("list", length(outcomes))

for (i in seq_along(outcomes)) {
  plots[[i]] <- make_tercile_plot(
    data = df_main,
    outcome_var = outcomes[i],
    plot_title = titles[i],
    y_limits = ylims[[i]]
  )
}


# ------------------------------------------------------------------- #
# Combine plots into a final panel
# ------------------------------------------------------------------- #
plot_final <- wrap_plots(plots, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )

plot_final


ggsave(plot = plot_final,
       filename = file.path(path_descp, "/Figures/spending_education.pdf"),
       device = "pdf", height = 8, width = 15)


