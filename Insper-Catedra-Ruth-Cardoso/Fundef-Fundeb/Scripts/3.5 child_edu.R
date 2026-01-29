# ---------------------------------------------------------------------------- #
# Regressions - Spending + Flags For early-childhood
# Last edited by: Tuffy Licciardi Issa
# Date: 28/01/2026
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
# 1. Data ----
# ---------------------------------------------------------------------------- #


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
         d21 = ifelse(ano == 2021, 1, 0)) #%>% 
# group_by(codigo_ibge) %>% 
# mutate(  #Fixed spend
#   spend_edu6 = real_des_edu/mat_total[ano == 2006],
#   spend_inf6 = real_des_inf/mat_inf[ano == 2006]) %>% 
# ungroup()

# ---------------------------------------------------------------------------- #
## 1.1 Dictionary ----
# ---------------------------------------------------------------------------- #

my_dict <- c(
  "aluno_dosage" = "Student dosage",
  "real_des_edu_pa" = "Total",
  "real_des_inf_pa" = "Pre-primary",
  "real_des_edu"    = "Total",
  "real_des_inf"    =  "Pre-primary",
  "PIBpc"        = "GDP per capita (R$)",
  "codigo_ibge" = "municipality",
  "uf^ano"      = "state x year",
  "ano"         = "year",
  "mat_total"   = "Total",
  "mat_inf"     = "Pre-primary",
  "grupo"       = "group"
)



# ---------------------------------------------------------------------------- #
# 2. Regressions ----
# ---------------------------------------------------------------------------- #

#Education Spending

est_nf_edu <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_spend %>% group_by (codigo_ibge) %>% 
                  #filter(ano < 2011) %>% 
                  #filter(all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                  ungroup(),
                vcov = ~codigo_ibge)

est_wf_edu <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_spend %>% 
                    #filter(ano < 2011) %>% 
                    filter(dosage == 1 | flag_spend70 == 0 & flag_spendm20 == 0),
                  vcov = ~codigo_ibge)


est_nf_inf <- feols(real_des_inf_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                    | codigo_ibge + ano + uf^ano,
                    data = df_spend %>% group_by (codigo_ibge) %>% 
                      #filter(ano < 2011) %>% 
                      #filter(all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                      ungroup(),
                    vcov = ~codigo_ibge)

est_wf_inf <- feols(real_des_inf_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                    | codigo_ibge + ano + uf^ano,
                    data = df_spend %>% 
                      #filter(ano < 2011) %>% 
                      filter(dosage == 1 | flag_spend70 == 0 & flag_spendm20 == 0),
                    vcov = ~codigo_ibge)





etable(#est_nf_edu,
  est_wf_edu,# est_nf_inf,
  est_wf_inf
)


etable( est_wf_edu, est_wf_inf, #mod_fund, mod_med,
        vcov = ~codigo_ibge,
        caption = "FUNDEB Effects on Education Spending per Student, 2005-2018",
        label = "tab:spend_wf",
        # headers = list(":_:" = list("Total Educ. Spending" = 1,
        #                             #"Fundamental" = 1, "Médio" = 1,
        #                             "Pre-Primary Educ. Spending" = 1)),
        dict = my_dict,
        tex = T,
        drop = "constant",
        file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Einf/reg_extended.tex",
        replace = TRUE)

rm(est_wf_edu, est_nf_edu, est_wf_inf, est_nf_inf)


# ---------------------------------------------------------------------------- #
# 3. Enroll reg ----
# ---------------------------------------------------------------------------- #
## 3.1 Enrollment log ----
# ---------------------------------------------------------------------------- #

df_spend <- df_spend %>% 
  mutate( log_mat_total = log(mat_total),
          log_mat_fun   = log(mat_fun),
          log_mat_med   = log(mat_med),
          log_mat_inf   = log(mat_inf),
          log_mat_eja   = log(mat_eja),
          log_mat_esp   = log(mat_esp))


# ---------------------------------------------------------------------------- #
## 3.2 Regression ----
# ---------------------------------------------------------------------------- #
### 3.2.1 Cluster ----
# ---------------------------------------------------------------------------- #

est_abra_tot <- feols(mat_total ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)

est_abra_inf <- feols(mat_inf ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)


# est_abra_fun <- feols(mat_fun ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
#                       | codigo_ibge + ano + uf^ano,
#                       data = df_spend %>% group_by (codigo_ibge) %>% 
#                         filter(ano < 2011) %>% 
#                         filter(dosage == 1 |
#                                  all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
#                         ungroup(),
#                       vcov = ~codigo_ibge)
# 
# est_abra_med <- feols(mat_med ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
#                       | codigo_ibge + ano +uf^ano,
#                       data = df_spend %>% group_by (codigo_ibge) %>% 
#                         filter(ano < 2011) %>% 
#                         filter(dosage == 1 |
#                                  all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
#                         ungroup(),
#                       vcov = ~codigo_ibge)


etable(est_abra_tot, est_abra_inf#, est_abra_fun, est_abra_med
)

etable(est_abra_tot, est_abra_inf, #est_abra_fun, est_abra_med, #mod_fund, mod_med,
       vcov = ~codigo_ibge,
       dict = my_dict,
       caption = "FUNDEB Effects on Enrollments, 2005-2018",
       label = "tab:enroll",
       tex = T,
       #headers = list(":_:" = list("Total" = 1, "Infantil" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Einf/reg_mat_cluster.tex",
       replace = TRUE)


rm(est_abra_tot, est_abra_med, est_abra_fun, est_abra_inf)


# ---------------------------------------------------------------------------- #
# 4. Spendings ----
# ---------------------------------------------------------------------------- #

#Repeating th estimations from the previous section but for the different courses

## 4.1 Regression ----
# ---------------------------------------------------------------------------- #
### 4.1.1 Cluster ----
# ---------------------------------------------------------------------------- #

est_abra_tot <- feols(real_des_edu ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)

est_abra_inf <- feols(real_des_inf ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)

# 
# est_abra_fun <- feols(real_des_fun_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
#                       | codigo_ibge + ano + uf^ano,
#                       data = df_spend %>% group_by (codigo_ibge) %>% 
#                         filter(ano < 2011) %>% 
#                         filter(dosage == 1 |
#                                  all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
#                         ungroup(),
#                       vcov = ~codigo_ibge)
# 
# est_abra_med <- feols(real_des_med_pa ~ aluno_dosage : i(ano, ref = 2006) + PIBpc
#                       | codigo_ibge + ano +uf^ano,
#                       data = df_spend %>% group_by (codigo_ibge) %>% 
#                         filter(ano < 2011) %>% 
#                         filter(dosage == 1 |
#                                  all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
#                         ungroup(),
#                       vcov = ~codigo_ibge)


etable(est_abra_tot, est_abra_inf#, est_abra_fun, est_abra_med
)

etable(est_abra_tot, est_abra_inf, #est_abra_fun, est_abra_med, #mod_fund, mod_med,
       vcov = ~codigo_ibge,
       dict = my_dict,
       caption = "FUNDEB Effects on Overall Spendings, 2005-2018",
       label = "tab:overspend",
       tex = T,
       #headers = list(":_:" = list("Total" = 1, "Infantil" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Einf/spend_tot_cluster.tex",
       replace = TRUE)


rm(est_abra_tot, est_abra_med, est_abra_fun, est_abra_inf)

# ---------------------------------------------------------------------------- #
#5. ES Win vs. Lose ----
# ---------------------------------------------------------------------------- #
## 5.0 Additional Data ----
# ---------------------------------------------------------------------------- #

new_data <- readRDS("Z:/Tuffy/Paper - Educ/Dados/enroll_state_private.rds") %>%
  group_by(codmun, ano) %>% 
  mutate( #Creating the total enrollment variables
    mat_tot_priv = ef_priv + em_priv + ed_inf_priv + eja_priv + esp_priv,
    mat_tot_stat = ef_stat + em_stat + ed_inf_stat + eja_stat + esp_stat
  ) %>% 
  ungroup() %>% 
  select(-uf) %>% 
  mutate(codmun = codmun %/% 10)


df_school <- readRDS("Z:/Tuffy/Paper - Educ/Dados/censo_escolar_base_v2.rds") %>% 
  mutate(codmun = as.character(codmun %/% 10),
         new_psc = ifelse(pre_tot > 0, 1, 0),
         new_day = ifelse(day_tot > 0, 1, 0),
         
         school_inf = ifelse(ed_inf_tot > 0, 1, 0)
  ) 


df_school <- df_school %>% 
  group_by(codmun, ano) %>% 
  summarise(
    n_school = n_distinct(school),
    n_psc    = sum(new_psc, na.rm = T),
    n_day    = sum(new_day, na.rm = T),
    
    n_inf    = sum(school_inf, na.rm = T),
    .groups = "drop"
  ) %>% 
  mutate(codmun = as.double(codmun))


df_temp <- df_spend %>% select(codigo_ibge, ano, aluno_dosage, grupo, dosage,
                               growth_spend, PIBpc, uf) %>% 
  left_join(new_data, by = c("codigo_ibge" = "codmun", "ano")) %>% 
  left_join(df_school, by = c("codigo_ibge" = "codmun", "ano"))

# ---------------------------------------------------------------------------- #
## 5.1 Plot Func ----
# ---------------------------------------------------------------------------- #

win_lose_plot <- function(est_obj, title = NULL, ylim = NULL) {
  # extract tidy data from fixest::etable or broom::tidy
  event_df <- broom::tidy(est_obj, conf.int = TRUE) %>% 
    mutate(
      ano = str_extract(term, "(?<=ano::)-?\\d+"),
      ano = as.numeric(ano),
      # Extract group (text after the last colon)
      grupo = str_extract(term, "[^:]+$"),
      grupo = as.factor(grupo))
  
  
  event_df <- event_df %>% 
    bind_rows(
      event_df %>%
        distinct(grupo) %>%
        mutate(
          term = paste0(as.character(grupo),"ano:2006"),     
          estimate = 0,
          std.error = 0,
          statistic = 0,
          p.value = 1,
          conf.low = 0,
          conf.high = 0,
          ano = 2006
        )
    ) %>% 
    filter(grupo != "PIBpc") %>% 
    rename(Group = grupo)
  
  ggplot(event_df, aes(x = ano, y = estimate, group = Group)) +
    # shaded standard error area
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 2006, color = "black") +
    geom_point(aes(color = Group), shape = 15, size = 2) +
    geom_line(aes(color = Group)) +
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




# ---------------------------------------------------------------------------- #
## 5.2 Enrollment ----
# ---------------------------------------------------------------------------- #
### 5.2.2 Broad ----
# ---------------------------------------------------------------------------- #


est_abra_tot <- feols(mat_total ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)

est_abra_inf <- feols(mat_inf ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data =df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)

# 
# est_abra_fun <- feols(mat_fun ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                       | codigo_ibge + ano + uf^ano,
#                       data = df_spend %>% group_by (codigo_ibge) %>% 
#                         #filter(ano < 2011) %>% 
#                         filter(dosage == 1 |
#                                  all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
#                         ungroup(),
#                       vcov = "hetero")
# 
# est_abra_med <- feols(mat_med ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                       | codigo_ibge + ano +uf^ano,
#                       data = df_spend %>% group_by (codigo_ibge) %>% 
#                         #filter(ano < 2011) %>% 
#                         filter(dosage == 1 |
#                                  all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
#                         ungroup(),
#                       vcov = "hetero")


# ---------------------------------------------------------------------------- #
### 5.2.3 No Filter ----
# ---------------------------------------------------------------------------- #

# 
# est_tot <- feols(mat_total ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                  | codigo_ibge + ano + uf^ano,
#                  data = df_spend,
#                  vcov = "hetero")
# 
# est_inf <- feols(mat_inf ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                  | codigo_ibge + ano + uf^ano,
#                  data = df_spend,
#                  vcov = "hetero")
# 
# 
# est_fun <- feols(mat_fun ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                  | codigo_ibge + ano + uf^ano,
#                  data = df_spend,
#                  vcov = "hetero")
# 
# est_med <- feols(mat_med ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                  | codigo_ibge + ano +uf^ano,
#                  data = df_spend,
#                  vcov = "hetero")


# -------------------------------------------- #
### 5.2.4 Plot ----
# -------------------------------------------- #

#p_tot_r <- win_lose_plot(est_rest_tot, "Restrito")    
p_tot_a <- win_lose_plot(est_abra_tot, "Total")
#p_tot_f <- win_lose_plot(est_tot,      "Sem Filtro")

#p_inf_r <- win_lose_plot(est_rest_inf, "Restrito")    
p_inf_a <- win_lose_plot(est_abra_inf, "Pre-primary")
#p_inf_f <- win_lose_plot(est_inf,      "Sem Filtro")
# 
# 
# #p_fun_r <- win_lose_plot(est_rest_fun, "Restrito")  
# p_fun_a <- win_lose_plot(est_abra_fun, "Abrangente 70-20")         
# p_fun_f <- win_lose_plot(est_fun,      "Sem Filtro")
# 
# #p_med_r <- win_lose_plot(est_rest_med, "Restrito")   
# p_med_a <- win_lose_plot(est_abra_med, "Abrangente 70-20")
# p_med_f <- win_lose_plot(est_med,      "Sem Filtro")
# 
# # 

# helper that returns a tiny ggplot containing the vertical label text
label_row <- function(text, size_pt = 10) {
  ggplot() +
    theme_void() +
    annotate("text",
             x = 0.5, y = 0.5,
             label = text,
             angle = 90,                # rotate vertical
             size = size_pt,            # ggplot size (about pts)
             fontface = "bold",
             hjust = 0.5, vjust = 0.5) +
    theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1))
}


# Create the right column with vertical labels (one per row)
labels_col <- label_row(" ",     size_pt = 6) #/   # top row
  #label_row("Pre-primary", size_pt = 6) #/   # middle row
  # label_row("Fundamental", size_pt = 6) /   # middle row
  # label_row("Médio", size_pt = 6) # bottom row

# put the 12 plots in the exact left-to-right, top-to-bottom order
plots <- list(
  p_tot_a, #p_tot_f,
  p_inf_a  #, p_inf_f,
#  p_fun_a, p_fun_f,
#  p_med_a, p_med_f
)

# normalize per-plot margins so they don't force reflow
normalize_margin <- function(p) {
  p + theme(plot.margin = grid::unit(c(2,2,2,2), "pt"))
}
plots <- lapply(plots, normalize_margin)

# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 1) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Clustered (municipality) standard-errors")

print(final)


ggsave(                                                #Saving image
  filename = paste0("win_lose_enroll_filter.png"),
  plot = final,
  create.dir = T,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Einf/",
  width = 800/96, height = 350/96, dpi = 300
)

rm(p_fun_a, p_fun_r, p_inf_a, p_inf_r, p_med_a, p_med_r, p_tot_a, p_tot_r, final, grid_plot,
   p_fun_f, p_inf_f, p_med_f, p_tot_f,
   est_abra_fun, est_abra_inf, est_abra_med, est_abra_tot, est_rest_fun, est_rest_inf,
   est_rest_med, est_rest_tot, est_fun, est_inf, est_med, est_tot)

# ---------------------------------------------------------------------------- #
## 5.3 Spending ----
# ---------------------------------------------------------------------------- #
### 5.3.1 Strict ---
# ---------------------------------------------------------------------------- #
# 
# est_rest_tot <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                       | codigo_ibge + ano + uf^ano,
#                       data = df_spend %>% 
#                         #filter(ano < 2011) %>% 
#                         filter(dosage == 1 |
#                                  flag_spend70 == 0 & flag_spendm20 == 0),
#                       vcov = "hetero")
# 
# 
# est_rest_inf <- feols(real_des_inf_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                       | codigo_ibge + ano + uf^ano,
#                       data = df_spend %>% 
#                         #filter(ano < 2011) %>% 
#                         filter(dosage == 1 |
#                                  flag_spend70 == 0 & flag_spendm20 == 0),
#                       vcov = "hetero")
# 
# 
# 
# est_rest_fun <- feols(real_des_fun_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                       | codigo_ibge + ano + uf^ano,
#                       data = df_spend %>% 
#                         #filter(ano < 2011) %>% 
#                         filter(dosage == 1 |
#                                  flag_spend70 == 0 & flag_spendm20 == 0),
#                       vcov = "hetero")
# 
# 
# 
# est_rest_med <- feols(real_des_med_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                       | codigo_ibge + ano + uf^ano,
#                       data = df_spend %>% 
#                         #filter(ano < 2011) %>% 
#                         filter(dosage == 1 |
#                                  flag_spend70 == 0 & flag_spendm20 == 0),
#                       vcov = "hetero")


#
# ---------------------------------------------------------------------------- #
### 5.3.1 Broad ----
# ---------------------------------------------------------------------------- #


est_abra_tot <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)

est_abra_inf <- feols(real_des_inf_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)

# 
# est_abra_fun <- feols(real_des_fun_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                       | codigo_ibge + ano + uf^ano,
#                       data = df_spend %>% group_by (codigo_ibge) %>% 
#                         #filter(ano < 2011) %>% 
#                         filter(dosage == 1 |
#                                  all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
#                         ungroup(),
#                       vcov = "hetero")
# 
# est_abra_med <- feols(real_des_med_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                       | codigo_ibge + ano +uf^ano,
#                       data = df_spend %>% group_by (codigo_ibge) %>% 
#                         #filter(ano < 2011) %>% 
#                         filter(dosage == 1 |
#                                  all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
#                         ungroup(),
#                       vcov = "hetero")

# ---------------------------------------------------------------------------- #
### 5.3.3 No Filter ----
# ---------------------------------------------------------------------------- #
# 
# 
# est_tot <- feols(real_des_edu_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                  | codigo_ibge + ano + uf^ano,
#                  data = df_spend,
#                  vcov = "hetero")
# 
# est_inf <- feols(real_des_inf_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                  | codigo_ibge + ano + uf^ano,
#                  data = df_spend,
#                  vcov = "hetero")
# 
# 
# est_fun <- feols(real_des_fun_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                  | codigo_ibge + ano + uf^ano,
#                  data = df_spend,
#                  vcov = "hetero")
# 
# est_med <- feols(real_des_med_pa ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                  | codigo_ibge + ano +uf^ano,
#                  data = df_spend,
#                  vcov = "hetero")


# -------------------------------------------- #
### 5.3.4 Plot ----
# -------------------------------------------- #

    
p_tot_a <- win_lose_plot(est_abra_tot, "Total")
#p_tot_f <- win_lose_plot(est_tot,      "Sem Filtro")

#p_inf_r <- win_lose_plot(est_rest_inf, "Restrito")    
p_inf_a <- win_lose_plot(est_abra_inf, "Pre-primary")      
# p_inf_f <- win_lose_plot(est_inf,      "Sem Filtro")
# 
# p_fun_r <- win_lose_plot(est_rest_fun, "Restrito")  
# p_fun_a <- win_lose_plot(est_abra_fun, "Abrangente 70-20")         
# p_fun_f <- win_lose_plot(est_fun,      "Sem Filtro")
# 
# p_med_r <- win_lose_plot(est_rest_med, "Restrito")   
# p_med_a <- win_lose_plot(est_abra_med, "Abrangente 70-20")
# p_med_f <- win_lose_plot(est_med,      "Sem Filtro")



# Create the right column with vertical labels (one per row)
labels_col <- label_row(" ",     size_pt = 6)# /   # top row
  # label_row("Infantil", size_pt = 6) /   # middle row
  # label_row("Fundamental", size_pt = 6) /   # middle row
  # label_row("Médio", size_pt = 6) # bottom row

# put the 12 plots in the exact left-to-right, top-to-bottom order
plots <- list(
  p_tot_a, #p_tot_f,
  p_inf_a  #, p_inf_f,
  # p_fun_a, p_fun_f,
  # p_med_a, p_med_f
)

plots <- lapply(plots, normalize_margin)

# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 1) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Clustered (municipality) standard-errors")

print(final)


ggsave(                                                #Saving image
  filename = paste0("win_lose_spend_filter.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Einf/",
  width = 800/96, height = 350/96, dpi = 300
)

rm(p_fun_a, p_fun_r, p_inf_a, p_inf_r, p_med_a, p_med_r, p_tot_a, p_tot_r, final, grid_plot,
   p_fun_f, p_inf_f, p_med_f, p_tot_f,
   est_abra_fun, est_abra_inf, est_abra_med, est_abra_tot, est_rest_fun, est_rest_inf,
   est_rest_med, est_rest_tot, est_fun, est_inf, est_med, est_tot, plots)

# ---------------------------------------------------------------------------- #
## 5.4 Spending Lvl----
# ---------------------------------------------------------------------------- #
### 5.4.1 Filter ----
# ---------------------------------------------------------------------------- #


est_abra_tot <- feols(real_des_edu ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)

est_abra_inf <- feols(real_des_inf ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_spend %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)
# 
# 
# est_abra_fun <- feols(real_des_fund ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                       | codigo_ibge + ano + uf^ano,
#                       data = df_spend %>% group_by (codigo_ibge) %>% 
#                         #filter(ano < 2011) %>% 
#                         filter(dosage == 1 |
#                                  all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
#                         ungroup(),
#                       vcov = "hetero")
# 
# est_abra_med <- feols(real_des_med ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                       | codigo_ibge + ano +uf^ano,
#                       data = df_spend %>% group_by (codigo_ibge) %>% 
#                         #filter(ano < 2011) %>% 
#                         filter(dosage == 1 |
#                                  all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
#                         ungroup(),
#                       vcov = "hetero")

# ---------------------------------------------------------------------------- #
### 5.3.3 No Filter ----
# ---------------------------------------------------------------------------- #


# est_tot <- feols(real_des_edu ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                  | codigo_ibge + ano + uf^ano,
#                  data = df_spend,
#                  vcov = "hetero")
# 
# est_inf <- feols(real_des_inf ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                  | codigo_ibge + ano + uf^ano,
#                  data = df_spend,
#                  vcov = "hetero")
# 
# 
# est_fun <- feols(real_des_fund ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                  | codigo_ibge + ano + uf^ano,
#                  data = df_spend,
#                  vcov = "hetero")
# 
# est_med <- feols(real_des_med ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
#                  | codigo_ibge + ano +uf^ano,
#                  data = df_spend,
#                  vcov = "hetero")


# -------------------------------------------- #
### 5.3.4 Plot ----
# -------------------------------------------- #

p_tot_a <- win_lose_plot(est_abra_tot, "Total")
#p_tot_f <- win_lose_plot(est_tot,      "Sem Filtro")

p_inf_a <- win_lose_plot(est_abra_inf, "Pre-primary")      
# #p_inf_f <- win_lose_plot(est_inf,      "Sem Filtro")
# 
# p_fun_a <- win_lose_plot(est_abra_fun, "Abrangente 70-20")         
# p_fun_f <- win_lose_plot(est_fun,      "Sem Filtro")
# 
# p_med_a <- win_lose_plot(est_abra_med, "Abrangente 70-20")
# p_med_f <- win_lose_plot(est_med,      "Sem Filtro")



# Create the right column with vertical labels (one per row)
labels_col <- label_row("",     size_pt = 6) #/   # top row
  # label_row("Infantil", size_pt = 6) /   # middle row
  # label_row("Fundamental", size_pt = 6) /   # middle row
  # label_row("Médio", size_pt = 6) # bottom row

# put the 12 plots in the exact left-to-right, top-to-bottom order
plots <- list(
  p_tot_a, #p_tot_f,
  p_inf_a  #, p_inf_f,
  # p_fun_a, p_fun_f,
  # p_med_a, p_med_f
)

plots <- lapply(plots, normalize_margin)

# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 1) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Clustered (municipality) standard-errors")

print(final)


ggsave(                                                #Saving image
  filename = paste0("win_lose_spend_lvl_filter.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Einf/",
  width = 800/96, height = 350/96, dpi = 300
)

rm(p_fun_a, p_fun_r, p_inf_a, p_inf_r, p_med_a, p_med_r, p_tot_a, p_tot_r, final, grid_plot,
   p_fun_f, p_inf_f, p_med_f, p_tot_f,
   est_abra_fun, est_abra_inf, est_abra_med, est_abra_tot, est_rest_fun, est_rest_inf,
   est_rest_med, est_rest_tot, est_fun, est_inf, est_med, est_tot)

# ---------------------------------------------------------------------------- #
##5.5 State and Private Enrollment ----
# ---------------------------------------------------------------------------- #
### 5.5.1 State ----
# ---------------------------------------------------------------------------- #

est_tot_stat <- feols( mat_tot_stat ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_temp %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)

est_inf_stat <- feols(ed_inf_stat ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_temp %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)

# ---------------------------------------------------------------------------- #
### 5.5.2 Private ----
# ---------------------------------------------------------------------------- #

est_tot_priv <- feols( mat_tot_priv ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                       | codigo_ibge + ano + uf^ano,
                       data = df_temp %>% group_by (codigo_ibge) %>% 
                         #filter(ano < 2011) %>% 
                         filter(dosage == 1 |
                                  all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                         ungroup(),
                       vcov = ~codigo_ibge)

est_inf_priv <- feols(ed_inf_priv ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                     | codigo_ibge + ano + uf^ano,
                     data = df_temp %>% group_by (codigo_ibge) %>% 
                       #filter(ano < 2011) %>% 
                       filter(dosage == 1 |
                                all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                       ungroup(),
                     vcov = ~codigo_ibge)

# -------------------------------------------- #
### 5.3.4 Plot ----
# -------------------------------------------- #

p_tot_s <- win_lose_plot(est_tot_stat, "Total")
p_inf_s <- win_lose_plot(est_inf_stat, "Pre-primary")

p_inf_p <- win_lose_plot(est_inf_priv, "Pre-primary")
p_tot_p <- win_lose_plot(est_tot_priv, "Total")


# Create the right column with vertical labels (one per row)
labels_col <- label_row("State",     size_pt = 6) /   # top row
 label_row("Private", size_pt = 6) #/   # middle row
# label_row("Fundamental", size_pt = 6) /   # middle row
# label_row("Médio", size_pt = 6) # bottom row

# put the 12 plots in the exact left-to-right, top-to-bottom order
plots <- list(
  p_tot_s, p_inf_s,
  p_tot_p, p_inf_p
  # p_fun_a, p_fun_f,
  # p_med_a, p_med_f
)

plots <- lapply(plots, normalize_margin)

# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 2) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Clustered (municipality) standard-errors")

print(final)


ggsave(                                                #Saving image
  filename = paste0("win_lose_other_enroll.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Einf/",
  width = 800/96, height = 550/96, dpi = 300
)

rm( est_inf_priv, est_inf_stat, est_tot_priv, est_tot_stat, new_data,
   plots, p_inf_p, p_inf_s, p_tot_p, p_tot_s)

# ---------------------------------------------------------------------------- #
## 5.6 N° Schools ----
# ---------------------------------------------------------------------------- #
### 5.6.1 Regression -----
# ---------------------------------------------------------------------------- #

est_tot <- feols( n_school ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                       | codigo_ibge + ano + uf^ano,
                       data = df_temp %>% group_by (codigo_ibge) %>% 
                         #filter(ano < 2011) %>% 
                         filter(dosage == 1 |
                                  all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                         ungroup(),
                       vcov = ~codigo_ibge)

est_inf <- feols(n_inf ~ aluno_dosage : i(ano, grupo, ref = 2006) + PIBpc
                      | codigo_ibge + ano + uf^ano,
                      data = df_temp %>% group_by (codigo_ibge) %>% 
                        #filter(ano < 2011) %>% 
                        filter(dosage == 1 |
                                 all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
                        ungroup(),
                      vcov = ~codigo_ibge)

# ---------------------------------------------------------------------------- #
### 5.6.2 Plot -----
# ---------------------------------------------------------------------------- #


p_tot <- win_lose_plot(est_tot, "Total")
p_inf <- win_lose_plot(est_inf, "Pre-primary")


# Create the right column with vertical labels (one per row)
labels_col <- label_row(" ",     size_pt = 6) #/   # top row
  #label_row("Private", size_pt = 6) #/   # middle row
# label_row("Fundamental", size_pt = 6) /   # middle row
# label_row("Médio", size_pt = 6) # bottom row

# put the 12 plots in the exact left-to-right, top-to-bottom order
plots <- list(
  p_tot, p_inf
  # p_fun_a, p_fun_f,
  # p_med_a, p_med_f
)

plots <- lapply(plots, normalize_margin)

# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 1) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Clustered (municipality) standard-errors")

print(final)




# ---------------------------------------------------------------------------- #
### 5.6.3  Time Plot -----
# ---------------------------------------------------------------------------- #

df_temp2 <- df_temp %>%
  group_by (codigo_ibge) %>% 
  filter(dosage == 1 |
           all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(ano, grupo) %>% 
  summarise(
    preprimary = sum(n_inf, na.rm = T),
    .groups = "drop"
  )


# ---------------------------------------------------------------------------- #
# 6. Broad and Strict Muns ----
# ---------------------------------------------------------------------------- #
#' I will stablish a list collecting the unique municipalities codes present in
#' each filtering to better select them in the future.

#1) Strict dataframe
df_rest <- df_spend %>% 
  #filter(ano < 2011) %>% 
  filter(dosage == 1 |
           flag_spend70 == 0 & flag_spendm20 == 0)


#2) Broad dataframe
df_abra <- df_spend %>%
  group_by (codigo_ibge) %>% 
  #filter(ano < 2011) %>% 
  filter(dosage == 1 |
           all(growth_spend > -20 & growth_spend < 70, na.rm = TRUE)) %>%
  ungroup()


#3) Creating list
filter_list <- list()



#4) Adding the results

filter_list[["rest"]] <- unique(df_rest$codigo_ibge) #strict

filter_list[["abra"]] <- unique(df_abra$codigo_ibge) #broad


#5) Now adding the enrollment filter
## 15%
df_enro <- df_spend %>% 
  group_by(codigo_ibge) %>% 
  filter(dosage == 1 | all(abs(growth_enroll) < 15 #| growth_enroll > -10
                           , na.rm = T))

filter_list[["enro"]] <- unique(df_enro$codigo_ibge) #15

## 20%
df_enro <- df_spend %>% 
  group_by(codigo_ibge) %>% 
  filter(dosage == 1 | all(abs(growth_enroll) < 20, na.rm = T
  ))

filter_list[["enro20"]] <- unique(df_enro$codigo_ibge) #20

## 25%
df_enro <- df_spend %>% 
  group_by(codigo_ibge) %>% 
  filter(dosage == 1 | all(abs(growth_enroll) < 25, na.rm = T
  ))

filter_list[["enro25"]] <- unique(df_enro$codigo_ibge) #25

## 30%
df_enro <- df_spend %>% 
  group_by(codigo_ibge) %>% 
  filter(dosage == 1 | all(abs(growth_enroll) < 30, na.rm = T
  ))

filter_list[["enro30"]] <- unique(df_enro$codigo_ibge) #30

## 40%
df_enro <- df_spend %>% 
  group_by(codigo_ibge) %>% 
  filter(dosage == 1 | all(abs(growth_enroll) < 40, na.rm = T
  ))

filter_list[["enro40"]] <- unique(df_enro$codigo_ibge) #40


rm(df_rest, df_abra, df_enro)

#----------------------------------------------------------------------------- #
# 7. School Infra ----
# ---------------------------------------------------------------------------- #


#' In this section I will breakdown the effects for various variables associated
#' with the daycare and preschool system.
#' 
#' First I will define the graph function for the remaining estimations


# ---------------------------------------------------------------------------- #
## 7.1 Data (Mun Lvl) ----
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
  mutate(grupo = case_when(
    dosage > 0 ~ "Winner",
    dosage < 0 ~ "Loser",
    TRUE ~ NA
  ),
  grupo = factor(grupo))


# ---------------------------------------------------------------------------- #
## 7.2 W vs. L Breakdown ----
# ---------------------------------------------------------------------------- #

#'Continung with the data, I will repeat the last estimations, while applying the
#'diferenciation within [Winners] vs. [Losers] in the data.

win_lose_plot <- function(est_obj, title = NULL, ylim = NULL) {
  # extract tidy data from fixest::etable or broom::tidy
  event_df <- broom::tidy(est_obj, conf.int = TRUE) %>% 
    mutate(
      ano = str_extract(term, "(?<=ano::)-?\\d+"),
      ano = as.numeric(ano),
      # Extract group (text after the last colon)
      grupo = str_extract(term, "[^:]+$"),
      grupo = as.factor(grupo))
  
  
  event_df <- event_df %>% 
    bind_rows(
      event_df %>%
        distinct(grupo) %>%
        mutate(
          term = paste0(as.character(grupo),"ano:2006"),     
          estimate = 0,
          std.error = 0,
          statistic = 0,
          p.value = 1,
          conf.low = 0,
          conf.high = 0,
          ano = 2006
        )
    ) %>% 
    filter(grupo != "PIBpc") %>% 
    rename(Group = grupo)
  
  ggplot(event_df, aes(x = ano, y = estimate, group = Group)) +
    # shaded standard error area
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Group), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 2006, color = "black") +
    geom_point(aes(color = Group), shape = 15, size = 2) +
    geom_line(aes(color = Group)) +
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

# ---------------------------------------------------------------------------- #
### 7.2.1 Infra reg ----
# ---------------------------------------------------------------------------- #
#### 7.2.1.1 Broad ----
# ---------------------------------------------------------------------------- #

#Classroom
est_class <- feols( class ~ aluno_dosage : i(ano, grupo, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = "hetero")
#Teacher room
est_troom <- feols( exp_troom ~ aluno_dosage : i(ano, grupo, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = "hetero")
#Laboratory
est_labs <- feols(exp_lab ~ aluno_dosage : i(ano, grupo, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                  vcov = "hetero")

#Library
est_libra <- feols(exp_lib ~ aluno_dosage : i(ano, grupo, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                   vcov = "hetero")

#Play Area
est_play <- feols(exp_play ~ aluno_dosage : i(ano, grupo, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                  vcov = "hetero")

#Lunch
est_lunch <- feols(exp_lunch ~ aluno_dosage : i(ano, grupo, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                   vcov = "hetero")

#Employee 
est_employ <- feols(employee ~ aluno_dosage : i(ano, grupo, ref = 2006)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = "hetero")


est_n_employ <- feols(n_employee ~ aluno_dosage : i(ano, grupo, ref = 2006)
                      + PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                      vcov = "hetero")

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ, est_n_employ)

# --------------------------------- #
##### 7.2.1.1.1 Graph ----
# --------------------------------- #

# 3) Apply helper to each ggiplot object
p_class <- win_lose_plot(est_class, "Aluno/Salas de aula")   #Classroom
p_troom <- win_lose_plot(est_troom, "Exp. Sala dos Prof.")  #Teacher's Room
p_labs  <- win_lose_plot(est_labs,  "Exp. Laboratórios")    #Laboratory
p_libra <- win_lose_plot(est_libra, "Exp. Biblioteca")      #Library
p_play  <- win_lose_plot(est_play,  "Exp. Quadras/Parque")  #Play Area
p_lunch <- win_lose_plot(est_lunch, "Exp. Merenda")         #Lunch
p_employ <- win_lose_plot(est_employ,"Aluno/Funcionários")   #Employee
p_nemploy <- win_lose_plot(est_n_employ, "N° Funcionários")
blank <- ggplot() + theme_void()

grid_plot <- ( p_troom + p_labs + p_libra) /
  (p_play + p_lunch + blank) /
  (p_class   + p_employ + p_nemploy)

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("winlose_school_infra_abra.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Filter/", #Saving directly to the report
  width = 1300/96, height = 720/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final, est_class, est_employ, est_labs, est_libra, est_lunch, est_play,
   est_troom)


# ---------------------------------------------------------------------------- #
#### 7.2.1.2 No Filter ----
# ---------------------------------------------------------------------------- #

#Classroom
est_class <- feols( class ~ aluno_dosage : i(ano, grupo, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Teacher room
est_troom <- feols( exp_troom ~ aluno_dosage : i(ano, grupo, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Laboratory
est_labs <- feols(exp_lab ~ aluno_dosage : i(ano, grupo, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Library
est_libra <- feols(exp_lib ~ aluno_dosage : i(ano, grupo, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Play Area
est_play <- feols(exp_play ~ aluno_dosage : i(ano, grupo, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Lunch
est_lunch <- feols(exp_lunch ~ aluno_dosage : i(ano, grupo, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Employee 
est_employ <- feols(employee ~ aluno_dosage : i(ano, grupo, ref = 2006)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")


est_n_employ <- feols(n_employee ~ aluno_dosage : i(ano, grupo, ref = 2006)
                      + PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb,
                      vcov = "hetero")

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ, est_n_employ)

# --------------------------------- #
##### 7.2.1.2.1 Graph ----
# --------------------------------- #

# 3) Apply helper to each ggiplot object
p_class <- win_lose_plot(est_class, "Aluno/Salas de aula")   #Classroom
p_troom <- win_lose_plot(est_troom, "Exp. Sala dos Prof.")  #Teacher's Room
p_labs  <- win_lose_plot(est_labs,  "Exp. Laboratórios")    #Laboratory
p_libra <- win_lose_plot(est_libra, "Exp. Biblioteca")      #Library
p_play  <- win_lose_plot(est_play,  "Exp. Quadras/Parque")  #Play Area
p_lunch <- win_lose_plot(est_lunch, "Exp. Merenda")         #Lunch
p_employ <- win_lose_plot(est_employ,"Aluno/Funcionários")   #Employee
p_nemploy <- win_lose_plot(est_n_employ, "N° Funcionários")
blank <- ggplot() + theme_void()

grid_plot <- ( p_troom + p_labs + p_libra) /
  (p_play + p_lunch + blank) /
  (p_class   + p_employ + p_nemploy)

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("winlose_school_infra_nofilter.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Filter/", #Saving directly to the report
  width = 1300/96, height = 720/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final, est_class, est_employ, est_labs, est_libra, est_lunch, est_play,
   est_troom)


# ---------------------------------------------------------------------------- #
## 7.3 Regression ----
# ---------------------------------------------------------------------------- #

event_plot <- function(est_obj, title = NULL, ylim = NULL) {
  # extract tidy data from fixest::etable or broom::tidy
  event_df <- broom::tidy(est_obj, conf.int = TRUE) %>% 
    mutate(
      ano = str_extract(term, "(?<=ano::)-?\\d+"),
      ano = as.numeric(ano)
      # Extract group (text after the last colon)
    )
  
  
  
  event_df <- event_df %>% 
    bind_rows(
      event_df %>%
        mutate(
          term = "ano:2006",     
          estimate = 0,
          std.error = 0,
          statistic = 0,
          p.value = 1,
          conf.low = 0,
          conf.high = 0,
          ano = 2006
        ))
  
  
  ggplot(event_df, aes(x = ano, y = estimate)) +
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

# ---------------------------------------------------------------------------- #
### 7.3.1 Infra reg ----
# ---------------------------------------------------------------------------- #
#### 7.3.1.1 Broad ----
# ---------------------------------------------------------------------------- #

#Classroom
est_class <- feols( class ~ aluno_dosage : i(ano, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = "hetero")
#Teacher room
est_troom <- feols( exp_troom ~ aluno_dosage : i(ano, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = "hetero")
#Laboratory
est_labs <- feols(exp_lab ~ aluno_dosage : i(ano, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                  vcov = "hetero")

#Library
est_libra <- feols(exp_lib ~ aluno_dosage : i(ano, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                   vcov = "hetero")

#Play Area
est_play <- feols(exp_play ~ aluno_dosage : i(ano, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                  vcov = "hetero")

#Lunch
est_lunch <- feols(exp_lunch ~ aluno_dosage : i(ano, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                   vcov = "hetero")

#Employee 
est_employ <- feols(employee ~ aluno_dosage : i(ano, ref = 2006)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = "hetero")


est_n_employ <- feols(n_employee ~ aluno_dosage : i(ano, ref = 2006)
                      + PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                      vcov = "hetero")

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ, est_n_employ)

# --------------------------------- #
##### 7.2.1.1.1 Graph ----
# --------------------------------- #

# 3) Apply helper to each ggiplot object
p_class <- event_plot(est_class, "Aluno/Salas de aula")   #Classroom
p_troom <- event_plot(est_troom, "Exp. Sala dos Prof.")  #Teacher's Room
p_labs  <- event_plot(est_labs,  "Exp. Laboratórios")    #Laboratory
p_libra <- event_plot(est_libra, "Exp. Biblioteca")      #Library
p_play  <- event_plot(est_play,  "Exp. Quadras/Parque")  #Play Area
p_lunch <- event_plot(est_lunch, "Exp. Merenda")         #Lunch
p_employ <- event_plot(est_employ,"Aluno/Funcionários")   #Employee
p_nemploy <- event_plot(est_n_employ, "N° Funcionários")
blank <- ggplot() + theme_void()

grid_plot <- ( p_troom + p_labs + p_libra) /
  (p_play + p_lunch + blank) /
  (p_class   + p_employ + p_nemploy)

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("school_infra_abra.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Filter/", #Saving directly to the report
  width = 1300/96, height = 720/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final, est_class, est_employ, est_labs, est_libra, est_lunch, est_play,
   est_troom)


# ---------------------------------------------------------------------------- #
#### 7.2.1.2 No Filter ----
# ---------------------------------------------------------------------------- #

#Classroom
est_class <- feols( class ~ aluno_dosage : i(ano, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Teacher room
est_troom <- feols( exp_troom ~ aluno_dosage : i(ano, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Laboratory
est_labs <- feols(exp_lab ~ aluno_dosage : i(ano, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Library
est_libra <- feols(exp_lib ~ aluno_dosage : i(ano, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Play Area
est_play <- feols(exp_play ~ aluno_dosage : i(ano, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Lunch
est_lunch <- feols(exp_lunch ~ aluno_dosage : i(ano, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Employee 
est_employ <- feols(employee ~ aluno_dosage : i(ano, ref = 2006)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")


est_n_employ <- feols(n_employee ~ aluno_dosage : i(ano, ref = 2006)
                      + PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb,
                      vcov = "hetero")

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ, est_n_employ)

# --------------------------------- #
##### 7.2.1.2.1 Graph ----
# --------------------------------- #

# 3) Apply helper to each ggiplot object
p_class <- event_plot(est_class, "Aluno/Salas de aula")   #Classroom
p_troom <- event_plot(est_troom, "Exp. Sala dos Prof.")  #Teacher's Room
p_labs  <- event_plot(est_labs,  "Exp. Laboratórios")    #Laboratory
p_libra <- event_plot(est_libra, "Exp. Biblioteca")      #Library
p_play  <- event_plot(est_play,  "Exp. Quadras/Parque")  #Play Area
p_lunch <- event_plot(est_lunch, "Exp. Merenda")         #Lunch
p_employ <- event_plot(est_employ,"Aluno/Funcionários")   #Employee
p_nemploy <- event_plot(est_n_employ, "N° Funcionários")
blank <- ggplot() + theme_void()

grid_plot <- ( p_troom + p_labs + p_libra) /
  (p_play + p_lunch + blank) /
  (p_class   + p_employ + p_nemploy)

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("school_infra_nofilter.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Filter/", #Saving directly to the report
  width = 1300/96, height = 720/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final, est_class, est_employ, est_labs, est_libra, est_lunch, est_play,
   est_troom)


# ---------------------------------------------------------------------------- #
## 7.4 Child Infra ----
# ---------------------------------------------------------------------------- #

#' Here I will analyze the effects on the infraestruture of schools that have 
#' students enrolled in the kinder garden.


#Starting with the scholl data
df_school <- readRDS("Z:/Tuffy/Paper - Educ/Dados/censo_escolar_base_v2.rds") %>% 
  mutate(codmun = as.character(codmun %/% 10),
         new_psc = ifelse(pre_tot > 0, 1, 0),
         new_day = ifelse(day_tot > 0, 1, 0)
  )

#We will calculate the municipal exposure, weighted by the enrollment numbers
df_school <- df_school %>% 
  filter(new_psc == 1 | new_day == 1) %>%  #Filter for schools with preschool
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
  mutate(grupo = case_when(
    dosage > 0 ~ "Winner",
    dosage < 0 ~ "Loser",
    TRUE ~ NA
  ),
  grupo = factor(grupo))


# ---------------------------------------------------------------------------- #
### 7.4.1 Infra reg ----
# ---------------------------------------------------------------------------- #
#### 7.4.1.1 Winlose ----
# ---------------------------------------------------------------------------- #

#Classroom
est_class <- feols( class ~ aluno_dosage : i(ano, grupo, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = ~codmun)
#Teacher room
est_troom <- feols( exp_troom ~ aluno_dosage : i(ano, grupo, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = ~codmun)
#Laboratory
est_labs <- feols(exp_lab ~ aluno_dosage : i(ano, grupo, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                  vcov = ~codmun)

#Library
est_libra <- feols(exp_lib ~ aluno_dosage : i(ano, grupo, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                   vcov = ~codmun)

#Play Area
est_play <- feols(exp_play ~ aluno_dosage : i(ano, grupo, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                  vcov = ~codmun)

#Lunch
est_lunch <- feols(exp_lunch ~ aluno_dosage : i(ano, grupo, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                   vcov = ~codmun)

#Employee 
est_employ <- feols(employee ~ aluno_dosage : i(ano, grupo, ref = 2006)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = ~codmun)


est_n_employ <- feols(n_employee ~ aluno_dosage : i(ano, grupo, ref = 2006)
                      + PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                      vcov = ~codmun)

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ, est_n_employ)

# --------------------------------- #
##### 7.4.1.1.1 Graph ----
# --------------------------------- #

# 3) Apply helper to each ggiplot object
p_class <- win_lose_plot(est_class, "Students per Classroom")   #Classroom
p_troom <- win_lose_plot(est_troom, "Exp. Teacher's Room")  #Teacher's Room
p_labs  <- win_lose_plot(est_labs,  "Exp. Lab")    #Laboratory
p_libra <- win_lose_plot(est_libra, "Exp. Library")      #Library
p_play  <- win_lose_plot(est_play,  "Exp. Play Areas")  #Play Area
p_lunch <- win_lose_plot(est_lunch, "Exp. Lunch")         #Lunch
p_employ <- win_lose_plot(est_employ,"Students per Employee")   #Employee
p_nemploy <- win_lose_plot(est_n_employ, "Total Employees")
blank <- ggplot() + theme_void()

grid_plot <- ( p_troom + p_labs + p_libra) /
  (p_play + p_lunch + blank) /
  (p_class   + p_employ + p_nemploy)

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Clustered (municipality) standard-errors"
)

final

ggsave( #Saving image
  filename = paste0("winlose_school_infra_abra_preschool.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Einf/", #Saving directly to the report
  width = 1300/96, height = 720/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final, est_class, est_employ, est_labs, est_libra, est_lunch, est_play, win_lose_plot,
   est_troom)

# ---------------------------------------------------------------------------- #
#### 7.4.1.1 Comb ----
# ---------------------------------------------------------------------------- #

#Classroom
est_class <- feols( class ~ aluno_dosage : i(ano, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = ~codmun)
#Teacher room
est_troom <- feols( exp_troom ~ aluno_dosage : i(ano, ref = 2006) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = ~codmun)
#Laboratory
est_labs <- feols(exp_lab ~ aluno_dosage : i(ano, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                  vcov = ~codmun)

#Library
est_libra <- feols(exp_lib ~ aluno_dosage : i(ano, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                   vcov = ~codmun)

#Play Area
est_play <- feols(exp_play ~ aluno_dosage : i(ano, ref = 2006)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                  vcov = ~codmun)

#Lunch
est_lunch <- feols(exp_lunch ~ aluno_dosage : i(ano, ref = 2006)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                   vcov = ~codmun)

#Employee 
est_employ <- feols(employee ~ aluno_dosage : i(ano, ref = 2006)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                    vcov = ~codmun)


est_n_employ <- feols(n_employee ~ aluno_dosage : i(ano, ref = 2006)
                      + PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb %>% filter(codmun %in% filter_list[["abra"]]),
                      vcov = ~codmun)

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ, est_n_employ)


# --------------------------------- #
##### 7.4.1.1.2 Comb Graph ----
# --------------------------------- #

# 3) Apply helper to each ggiplot object
p_class <- event_plot(est_class, "Students per Classroom")   #Classroom
p_troom <- event_plot(est_troom, "Exp. Teacher's Room")  #Teacher's Room
p_labs  <- event_plot(est_labs,  "Exp. Lab")    #Laboratory
p_libra <- event_plot(est_libra, "Exp. Library")      #Library
p_play  <- event_plot(est_play,  "Exp. Play Area")  #Play Area
p_lunch <- event_plot(est_lunch, "Exp. Lunch")         #Lunch
p_employ <- event_plot(est_employ,"Students per Employee")   #Employee
p_nemploy <- event_plot(est_n_employ, "Total Employees")
blank <- ggplot() + theme_void()

grid_plot <- ( p_troom + p_labs + p_libra) /
  (p_play + p_lunch + blank) /
  (p_class   + p_employ + p_nemploy)

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Clustered (municipality) standard-errors"
)

final

ggsave( #Saving image
  filename = paste0("school_infra_abra_preschool.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Einf/", #Saving directly to the report
  width = 1300/96, height = 720/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final, est_class, est_employ, est_labs, est_libra, est_lunch, est_play, win_lose_plot,
   est_troom)



rm(df_comb, df_reg, df_school, df_spend, est_n_employ, p_nemploy, data)




