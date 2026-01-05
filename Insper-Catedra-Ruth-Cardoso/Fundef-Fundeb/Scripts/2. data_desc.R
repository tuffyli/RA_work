# ---------------------------------------------------------------------------- #
# Data Description
# DataBase adjustment
# Last edited by: Tuffy Licciardi Issa
# Date: 05/01/2026
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

#Desativando a notação científica
options(scipen = 999)

# ---------------------------------------------------------------------------- #
# 1. Bases de dados ----
# ---------------------------------------------------------------------------- #




simulacao <- readRDS("Z:/Tuffy/Paper - Educ/Dados/simulacao_const.rds")


# 91 municípios que não ganhavam nada de FUNDEF passaram a receber verbas;
# 919 municípios tiveram um aumento do coeficiente de distribuição da política;
# O aumento mediano foi de 
print(median((simulacao %>% filter(dif_per_coef >0))$dif_per_coef))
# [1] 4.790533
# 4668 tiveram uma diminuição do coeficiente de distribuição;
# A queda mediana foi de 
print(median((simulacao %>% filter(dif_per_coef < 0))$dif_per_coef))
# [1] -6.716077


##2.1 Scatters e Quart ----

#'A primeira etapa é referente à extração dos dados municipais pelo Censo Escolar
#'de 2006, para que seja possível construir o scatter-plot com o grau de presença
#'de alunos no Ensino Público vs o privado de acordo com o salto do ganho em pp.
#'da mudança do Fundef para o Fundef.


censo <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/censo_2006_filtrado.csv")




#Selecionando as variáveis de interesse
censo <- censo %>% 
  select(X, MASCARA, ANO, CODMUNIC, UF, SIGLA, MUNIC, DEP, LOC, DPE119,
         DPE11D, NPE119, NPE11D, reg_in, reg_fin, esp_total_final, em_tot,
         ed_inf_tot, eja_tot) %>% 
  arrange(CODMUNIC, ANO)



#Mutando as variáveis:
censo <- censo %>% 
  mutate(
    
    #Ensino Fund.
    # ##DIU
    # DEF11C = replace_na(DEF11C, 0),
    # DEF11D = replace_na(DEF11D, 0),
    # DEF11E = replace_na(DEF11E, 0),
    # DEF11F = replace_na(DEF11F, 0),
    # DEF11G = replace_na(DEF11G, 0),
    # DEF11H = replace_na(DEF11H, 0),
    # DEF11I = replace_na(DEF11I, 0),
    # DEF11J = replace_na(DEF11J, 0),
    # ##NOT
    # NEF11C = replace_na(NEF11C, 0),
    # NEF11D = replace_na(NEF11D, 0),
    # NEF11E = replace_na(NEF11E, 0),
    # NEF11F = replace_na(NEF11F, 0),
    # NEF11G = replace_na(NEF11G, 0),
    # NEF11H = replace_na(NEF11H, 0),
    # NEF11I = replace_na(NEF11I, 0),
    # NEF11J = replace_na(NEF11J, 0),
    # 
    # fund_aux = DEF11C + DEF11D + DEF11E + DEF11F + DEF11G + DEF11H + DEF11I + 
    #   DEF11J + NEF11C + NEF11D + NEF11E + NEF11F + NEF11G + NEF11H + NEF11I + NEF11J,
    rural_dum = ifelse(LOC == "Rural", 1, 0),
    total_loc = case_when(
      LOC == "Rural" ~ 1,
      LOC == "Urbana" ~ 1,
      TRUE ~ NA_real_
    ), 
    
    NPE119 = replace_na(NPE119, 0),
    NPE11D = replace_na(NPE11D, 0),
    DPE119 = replace_na(DPE119, 0),
    DPE11D = replace_na(DPE11D, 0),
    creche_aux = DPE119 + DPE11D,
    preesc_aux = NPE119 + NPE11D
  )




censo_df <- censo %>% 
  group_by(CODMUNIC, DEP) %>%
  summarise(
    
    nome = first(MUNIC),
    no_uf = first(UF),
    uf = first(SIGLA),
    mat_reg_in = sum(reg_in, na.rm = TRUE),
    mat_reg_fin   = sum(reg_fin, na.rm = TRUE),
    mat_tot_esp = sum(esp_total_final, na.rm = TRUE),
    #matri_fun = sum(fund_aux, na.rm = TRUE),
    
    mat_tot_em = sum(em_tot, na.rm = TRUE),
    
    matri_creche = sum(creche_aux, na.rm = TRUE),
    matri_preesc = sum(preesc_aux, na.rm = TRUE),
    matri_EI = matri_creche + matri_preesc,
    
    mat_tot_inf = sum(ed_inf_tot, na.rm = TRUE),
    mat_tot_eja = sum(eja_tot, na.rm = TRUE),
    
    rural_tot = sum(rural_dum),
    loc_tot = sum(total_loc),
    
    
    alunos_rur = sum(
      reg_in[rural_dum == 1] + reg_fin[rural_dum == 1] + esp_total_final[rural_dum == 1] +
        em_tot[rural_dum == 1] + matri_EI[rural_dum == 1] + eja_tot[rural_dum == 1],
      na.rm = T
    ),
    
    
    .groups = "drop"
  )


test <- simulacao %>% 
  mutate(
    uf_code = as.numeric(codigo_ibge) %/% 100000
  ) %>%
  group_by(uf) %>% 
  summarise(
    check_dif = sum(dif_coef_pp),
    check_coef_fb = sum(coef_est_fnde),
    check_coef_ff = sum(coef_simulado)
    
    
  ) %>% 
  ungroup()
print(test)
rm(test)

simulacao <- left_join(simulacao,
                       censo_df %>% filter(DEP == "Municipal"), by = c("codigo_ibge" = "CODMUNIC"))





#Teste
test <- simulacao %>% 
  group_by(uf.x) %>% 
  summarise(
    check = sum(coef_simulado, na.rm = T),
    check2 = sum(coef_est_fnde, na.rm = T)
  )

print(test)
rm(test)

censo_df <- simulacao

censo_df <- censo_df %>%
  filter(codigo_ibge > 60) %>% 
  # left_join(
  #   simulacao %>% select(codigo_ibge, coef_est_fnde, coef_simulado, dif_per_coef,
  #                        d_vaa, dif_coef_pp, receita_est_fnde, receita_simulada,
  #                        receita_real, shr_inf_em),
  #   by = c("CODMUNIC" = "codigo_ibge")
  #) %>% 
  group_by(codigo_ibge) %>%
  select(-mat_tot_esp.y) %>% 
  rename(mat_tot_esp = mat_tot_esp.x) %>% 
  mutate(
    
    #Criando as proporções
    shr_einf = matri_EI/(mat_tot_esp + mat_tot_em + matri_EI + mat_tot_eja + mat_reg_in + mat_reg_fin),
    shr_em = mat_tot_em/(mat_tot_esp + mat_tot_em + matri_EI + mat_tot_eja + mat_reg_in + mat_reg_fin),
    shr_eja = mat_tot_eja/(mat_tot_esp + mat_tot_em + matri_EI + mat_tot_eja + mat_reg_in + mat_reg_fin),
    shr_new = (matri_EI + mat_tot_em + mat_tot_eja)/
      (mat_tot_esp + mat_tot_em + matri_EI + mat_tot_eja + mat_reg_in + mat_reg_fin),
    
    #Pop. Rural
    shr_rur = rural_tot/ loc_tot,
    shr_alun_rur = alunos_rur/(mat_tot_esp + mat_tot_em + matri_EI + mat_tot_eja + mat_reg_in + mat_reg_fin),
  ) 



#Construindo os Quartis
pos_breaks <- quantile(censo_df$shr_alun_rur[censo_df$shr_alun_rur > 0],
                       probs = seq(0, 1, 0.25), na.rm = TRUE)

test_breal <- quantile(censo_df$shr_alun_rur,
                       probs = seq(0, 1, 0.25), na.rm = T)

censo_df <- censo_df %>%
  mutate(
    #Alunos Rurais
    quartile_rur = case_when(
      shr_alun_rur == 0 ~ "0",
      TRUE ~ cut(shr_alun_rur,
                 breaks = pos_breaks,
                 include.lowest = TRUE,
                 labels = c("Q1", "Q2", "Q3", "Q4"))
    ),
    
    #Ensino Infantil
    quartile_efn = cut(
      shr_einf,
      breaks = quantile(censo_df$shr_einf, probs = seq(0, 1, 0.25), na.rm = T),
      include.lowest = T,
      labels = c("Q1", "Q2", "Q3", "Q4")
    ),
    
    #Novas adições
    quartile_new = cut(
      shr_new,
      breaks = quantile(censo_df$shr_new, probs = seq(0, 1, 0.25), na.rm = T),
      include.lowest = T,
      labels = c("Q1", "Q2", "Q3", "Q4")
    ),
    
    #Ensino Médio
    quartile_em = case_when(
      shr_em == 0 ~ "0",
      TRUE ~ cut(
        shr_em, 
        breaks = quantile(censo_df$shr_em[censo_df$shr_em > 0],
                          probs = seq(0, 1, 0.25), na.rm = T),
        include.lowest = T,
        labels = c("Q1", "Q2","Q3", "Q4")
      )
    ),
    
    #EJA
    quartile_eja = case_when(
      shr_eja == 0 ~ "0",
      TRUE ~ cut(
        shr_eja, 
        breaks = quantile(censo_df$shr_eja[censo_df$shr_eja > 0],
                          probs = seq(0, 1, 0.25), na.rm = T),
        include.lowest = T,
        labels = c("Q1", "Q2","Q3", "Q4")
      ))
  ) %>% 
  ungroup()



## 2.2 Scatter-Plot ----
library(ggplot2)

### 2.2.1 Ensino Básico ----
#Plot Sem Corte 
plot <- ggplot(censo_df, aes(x = shr_einf, y = dif_coef_pp)) +
  geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
  geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
  labs(
    title = "Ensino Infantil vs. Diferença no ganho",
    x = "Prop. Ensino Infantil (2006)",
    y = "Dif. Coef p.p."
  ) +
  theme_minimal(base_size = 14) +                      # clean theme
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()                 # cleaner grid
  )
plot

ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter/scatter_full.png", plot, width = 6, height = 4, dpi = 150)



#Plot Reduzido
plot <- ggplot(censo_df, aes(x = shr_einf, y = dif_coef_pp)) +
  geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
  geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
  labs(
    title = "Ensino Infantil vs. Diferença no ganho",
    x = "Prop. Ensino Infantil (2006)",
    y = "Dif. Coef p.p."
  ) +
  theme_minimal(base_size = 14) +                      # clean theme
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()                 # cleaner grid
  ) +
  coord_cartesian(ylim = c(-0.2, 0.2))   # zooms in but keeps outliers plotted


plot
ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter/scatter_zoom.png", plot, width = 6, height = 4, dpi = 150)

### 2.2.2 Ensino Médio ----

plot <- ggplot(censo_df, aes(x = shr_em, y = dif_coef_pp)) +
  geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
  geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
  labs(
    title = "Ensino Médio vs. Diferença no ganho",
    x = "Prop. Ensino Médio (2006)",
    y = "Dif. Coef p.p."
  ) +
  theme_minimal(base_size = 14) +                      # clean theme
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()                 # cleaner grid
  )
plot

ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter/scatter_em_full.png", plot, width = 6, height = 4, dpi = 150)


#Plot Reduzido
plot <- ggplot(censo_df, aes(x = shr_em, y = dif_coef_pp)) +
  geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
  geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
  labs(
    title = "Ensino Médio vs. Diferença no ganho",
    x = "Prop. Ensino Médio (2006)",
    y = "Dif. Coef p.p."
  ) +
  theme_minimal(base_size = 14) +                      # clean theme
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()                 # cleaner grid
  ) +
  coord_cartesian(ylim = c(-0.2, 0.2))   # zooms in but keeps outliers plotted


plot
ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter/scatter_em_zoom.png", plot, width = 6, height = 4, dpi = 150)


### 2.2.3 EJA ----

plot <- ggplot(censo_df, aes(x = shr_eja, y = dif_coef_pp)) +
  geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
  geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
  labs(
    title = "EJA vs. Diferença no ganho",
    x = "Prop. EJA (2006)",
    y = "Dif. Coef p.p."
  ) +
  theme_minimal(base_size = 14) +                      # clean theme
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()                 # cleaner grid
  )
plot

ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter/scatter_eja_full.png", plot, width = 6, height = 4, dpi = 150)


#Plot Reduzido
plot <- ggplot(censo_df, aes(x = shr_eja, y = dif_coef_pp)) +
  geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
  geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
  labs(
    title = "EJA vs. Diferença no ganho",
    x = "Prop. EJA (2006)",
    y = "Dif. Coef p.p."
  ) +
  theme_minimal(base_size = 14) +                      # clean theme
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()                 # cleaner grid
  ) +
  coord_cartesian(ylim = c(-0.2, 0.2))   # zooms in but keeps outliers plotted


plot
ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter/scatter_eja_zoom.png",plot, width = 6, height = 4, dpi = 150)

# ### 2.2.4 Especial---- #
# 
# plot <- ggplot(censo_df, aes(x = shr_esp, y = dif_coef_pp)) +
#   geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
#   geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
#   labs(
#     title = "Ensino Especial vs. Diferença no ganho",
#     x = "Prop. Ensino Especial (2006)",
#     y = "Dif. Coef p.p."
#   ) +
#   theme_minimal(base_size = 14) +                      # clean theme
#   theme(
#     plot.title = element_text(face = "bold", size = 16),
#     plot.subtitle = element_text(size = 12, color = "gray40"),
#     legend.position = "right",
#     panel.grid.minor = element_blank()                 # cleaner grid
#   )
# plot
# 
# ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter/scatter_esp_full.png", plot, device = cairo_pdf)
# 
# 
# #Plot Reduzido
# plot <- ggplot(censo_df, aes(x = shr_esp, y = dif_coef_pp)) +
#   geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
#   geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
#   labs(
#     title = "Ensino Especial vs. Diferença no ganho",
#     x = "Prop. Ensino Especial (2006)",
#     y = "Dif. Coef p.p."
#   ) +
#   theme_minimal(base_size = 14) +                      # clean theme
#   theme(
#     plot.title = element_text(face = "bold", size = 16),
#     plot.subtitle = element_text(size = 12, color = "gray40"),
#     legend.position = "right",
#     panel.grid.minor = element_blank()                 # cleaner grid
#   ) +
#   coord_cartesian(ylim = c(-0.2, 0.2))   # zooms in but keeps outliers plotted
# 
# 
# plot
# ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter/scatter_esp_zoom.png", plot, device = cairo_pdf)

### 2.2.4 Novos Fatores ----


plot <- ggplot(censo_df, aes(x = shr_new, y = dif_coef_pp)) +
  geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
  geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
  labs(
    title = "Adicionados vs. Diferença no ganho",
    x = "Prop. Adicionados (2006)",
    y = "Dif. Coef p.p."
  ) +
  theme_minimal(base_size = 14) +                      # clean theme
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()                 # cleaner grid
  )
plot

ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter/scatter_add_full.png", plot, width = 6, height = 4, dpi = 150)


#Plot Reduzido
plot <- ggplot(censo_df, aes(x = shr_new, y = dif_coef_pp)) +
  geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
  geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
  labs(
    title = "Adicionados vs. Diferença no ganho",
    x = "Prop. Adicionados (2006)",
    y = "Dif. Coef p.p."
  ) +
  theme_minimal(base_size = 14) +                      # clean theme
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()                 
  ) +
  coord_cartesian(ylim = c(-0.2, 0.2))   


plot
ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter/scatter_add_zoom.png", plot, width = 6, height = 4, dpi = 150)


### 2.2.5 RURAL (ESC) ----


plot <- ggplot(censo_df, aes(x = shr_rur, y = dif_coef_pp)) +
  geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
  geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
  labs(
    title = "Esc. Rural vs. Diferença no ganho",
    x = "Prop. Escolas Rurais (2006)",
    y = "Dif. Coef p.p."
  ) +
  theme_minimal(base_size = 14) +                    
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()              
  )
plot

ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter/scatter_rur_full.png", plot, width = 6, height = 4, dpi = 150)


#Plot Reduzido
plot <- ggplot(censo_df, aes(x = shr_rur, y = dif_coef_pp)) +
  geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
  geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
  labs(
    title = "Esc. Rural vs. Diferença no ganho",
    x = "Prop. Escolas Rurais (2006)",
    y = "Dif. Coef p.p."
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()                 
  ) +
  coord_cartesian(ylim = c(-0.2, 0.2))   

plot
ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter/scatter_rur_zoom.png", plot, width = 6, height = 4, dpi = 150)





### 2.2.5 RURAL (ALUNOS) ----


plot <- ggplot(censo_df, aes(x = shr_alun_rur, y = dif_coef_pp)) +
  geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
  geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
  labs(
    title = "Matriculas Rurais vs. Diferença no ganho",
    x = "Prop. Alunos Rurais (2006)",
    y = "Dif. Coef p.p."
  ) +
  theme_minimal(base_size = 14) +                     
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()               
  )
plot

ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter/scatter_rur_alu_full.png", plot, width = 6, height = 4, dpi = 150)


#Plot Reduzido
plot <- ggplot(censo_df, aes(x = shr_alun_rur, y = dif_coef_pp)) +
  geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
  geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
  labs(
    title = "Matriculas Rurais vs. Diferença no ganho",
    x = "Prop. Alunos Rurais (2006)",
    y = "Dif. Coef p.p."
  ) +
  theme_minimal(base_size = 14) +                      
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +                 
  coord_cartesian(ylim = c(-0.2, 0.2))   


plot
ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter/scatter_rur_alu_zoom.png", plot, width = 6, height = 4, dpi = 150)






## 2.3 Quartil ----
### 2.3.1 Rural ----
temp <- censo_df %>% 
  group_by(quartile_rur) %>% 
  summarise(
    mean_pp = mean(dif_coef_pp, na.rm = T),
    n = n()
  )


plot <- ggplot(temp, aes(x = quartile_rur, y = mean_pp, fill = quartile_rur)) +
  geom_col(aes(width = (n / sum(n)) * 2.5)) +  #Deixar a grossura da barra proporcional ao n° de obs
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5) +
  labs(
    title = "Média da Dif. do Coef.",
    x = "Quartil (Prop. Alunos Rurais)",
    y = "Dif. Coef (p.p.)"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  )

plot
ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Barra/alunos_rurais.jpeg", plot, width = 6, height = 4, dpi = 150)
rm(temp)

###2.3.2 Infantil -----
temp <- censo_df %>% 
  group_by(quartile_efn) %>% 
  summarise(
    mean_pp = mean(dif_coef_pp, na.rm = T)
  )


plot <- ggplot(temp, aes(x = quartile_efn, y = mean_pp, fill = quartile_efn)) +
  geom_col() +
  labs(
    title = "Média da Dif. do Coef.",
    x = "Quartil (Prop. Alunos EI)",
    y = "Dif. Coef (p.p.)"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  )

plot
ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Barra/alunos_infantil.jpeg", plot, width = 6, height = 4, dpi = 150)
rm(temp)

###2.3.3 Adições -----
temp <- censo_df %>% 
  group_by(quartile_new) %>% 
  summarise(
    mean_pp = mean(dif_coef_pp, na.rm = T)
  )


plot <- ggplot(temp, aes(x = quartile_new, y = mean_pp, fill = quartile_new)) +
  geom_col() +
  labs(
    title = "Média da Dif. do Coef.",
    x = "Quartil (Prop. Novas Categorias)",
    y = "Dif. Coef (p.p.)"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  )

plot
ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Barra/alunos_adicionados.jpeg", plot, width = 6, height = 4, dpi = 150)
rm(temp)

### 2.3.4 EM ----
temp <- censo_df %>% 
  group_by(quartile_em) %>% 
  summarise(
    mean_pp = mean(dif_coef_pp, na.rm = T),
    n = n()
  )


plot <- ggplot(temp, aes(x = quartile_em, y = mean_pp, fill = quartile_em)) +
  geom_col(aes(width = (n / sum(n)) * 5.5)) +  #Deixar a grossura da barra proporcional ao n° de obs
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5) +
  labs(
    title = "Média da Dif. do Coef.",
    x = "Quartil (Prop. Alunos EM)",
    y = "Dif. Coef (p.p.)"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  )

plot


### 2.3.5 EJA ----
temp <- censo_df %>% 
  group_by(quartile_eja) %>% 
  summarise(
    mean_pp = mean(dif_coef_pp, na.rm = T),
    n = n()
  )


plot <- ggplot(temp, aes(x = quartile_eja, y = mean_pp, fill = quartile_eja)) +
  geom_col(aes(width = (n / sum(n)) * 3.5)) +  #Deixar a grossura da barra proporcional ao n° de obs
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5) +
  labs(
    title = "Média da Dif. do Coef.",
    x = "Quartil (Prop. Alunos EJA)",
    y = "Dif. Coef (p.p.)"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  )

plot
ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Barra/alunos_eja.jpeg", plot, width = 6, height = 4, dpi = 150)

## 2.4 Testes ----

test <- censo_df %>% 
  mutate(
    uf_code = as.numeric(codigo_ibge) %/% 100000
  ) %>%
  group_by(uf.x) %>% 
  summarise(
    check_dif = sum(dif_coef_pp),
    check_coef_fb = sum(coef_est_fnde),
    check_coef_ff = sum(coef_simulado)
    
    
  ) %>% 
  ungroup()
print(test)
rm(temp, test)

##2.5 Por UF ----

temp <- censo_df %>% 
  group_by(uf.x) %>% 
  summarise(
    mean_pp = mean(dif_coef_pp, na.rm = T),
    n = n()
  )


plot <- ggplot(temp, aes(x = uf.x, y = mean_pp)) +
  geom_col(fill = "#2C3E50") +
  
  labs(
    title = "Média da Dif. do Coef.",
    x = "UF",
    y = "Dif. Coef (p.p.)"
  ) +
  #scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) 

plot

ggsave("Z:/Tuffy/Paper - Educ/Resultados/Figuras/Barra/UF_coef.jpeg", plot, width = 6, height = 4, dpi = 150)

rm(temp)

# ---------------------------------------------------------------------------- #
# 3. Gio ----
# ---------------------------------------------------------------------------- #


# Filtrar para remover linhas com valores inválidos em `dif_per_coef` ou `shr_inf_em`
simulacao_filtrada <- simulacao %>%
  filter(is.finite(dif_per_coef) & is.finite(shr_inf_em))

# Na realidade, municípios com dif_per_coef == Inf não ganhavam nada com o FUNDEF e passaram a ganhar com o FUNDEB

modelo <- lm(dif_per_coef ~ shr_inf_em, data = simulacao_filtrada)
inclinação <- coef(modelo)[2]  # Extrai a inclinação da reta

ggplot(data = simulacao_filtrada, 
       aes(x = shr_inf_em, y = dif_per_coef)) +
  geom_point(color = "#66c2a5", alpha = 0.7, size = 2) +
  stat_smooth(method = "lm", se = FALSE, color = "#1b7837", linetype = "solid") +
  labs(
    x = "Share de Matrículas em Educação Infantil e EM (%)",
    y = "Diferença percentual do CD efetivo do FUNDEB (2007)\n em relação ao CD simulado (regras do FUNDEF)",
    # title = "Relação entre Share de Matrículas e Diferença na Distribuição do FUNDEB",
    caption = paste0("Nota:<br>• Total de observações: ", nrow(simulacao),
                     "<br>• Municípios que não recebiam e passaram a receber, portanto fora do gráfico (y = Inf): ", 
                     sum(!is.finite(simulacao$dif_per_coef) | !is.finite(simulacao$shr_inf_em)),
                     "<br>• No gráfico: ", nrow(simulacao_filtrada %>% filter(dif_per_coef < 330)),
                     "<br><b>O gráfico mostra o a diferença percentual do Coeficiente de Distribuição (que é um percentual) com a mudança <br>
                     da política em relação a um CD simulado, com as regras antigas do FUNDEF. Ou seja, mostra a diferença (%) do share <br>
                     que cada rede captura do Fundo estadual quando se comparam as políticas.</b>"
    )
  ) +
  ylim(-30, 330) +
  # annotate(
  # "text", x = 70, y = 100,
  # label = paste0("Inclinação: ", round(inclinação, 2)),
  # color = "#1b7837", size = 4, hjust = 0
  # ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 11),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_markdown(hjust = 0, face = "italic")
  )

#Quem são esses municípios?
nomes <- simulacao_filtrada %>% 
  filter(shr_inf_em > 50 &
           dif_per_coef > 100)


# ---------------------------------------------------------------------------- #
# 4. Tercil  ----
# ---------------------------------------------------------------------------- #

#' Aqui analisaremos o tercil mais beneficiado quanto à mudança no coeficiente de
#' redistribuição do Fundeb

cutoff <- quantile(
  (simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp,
  0.7
)
# 
# top_tercil <- simulacao %>%
#   filter(!is.na(dif_per_coef)) %>%
#   filter(dif_coef_pp > cutoff)                

top_tercil <- simulacao %>% 
  group_by(uf) %>% 
  mutate(
    dum_ter = ifelse( !is.na(dif_per_coef) & dif_coef_pp > cutoff, 1, 0)
  ) %>% 
  summarise(
    sum_ter = sum(dum_ter, na.rm = T),
    count = n(),
    .groups = "drop"
  ) %>% 
  arrange(desc(sum_ter)) %>% 
  mutate(
    shr_top = sum_ter/count
  )

library(knitr)

latex_table <- kable(
  top_tercil,
  format = "latex",
  booktabs = TRUE,
  caption = "Número de municípios por UF no tercil superior"
)

out_path <- "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/top_tercil_table.tex"

writeLines(latex_table, out_path)

message("Saved LaTeX table to: ", out_path)




# ---------------------------------------------------------------------------- #
#5. Scatter Dosage ----
# ---------------------------------------------------------------------------- #


##5.1 Bases ----

### 5.1.1 Reg ----
df_trn <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/painel_notas_transferencias_2000_2024.csv")

df_pesosaeb <- readRDS("Z:/Tuffy/Paper - Educ/Dados/pesos_saeb3.rds")

#df_gio <- readRDS("Z:/Tuffy/Paper - Educ/Dados/Gio_df.rds")

df_sim <- readRDS("Z:/Tuffy/Paper - Educ/Dados/simulacao_const.rds") #%>% 

#sim_gio <- readRDS(("Z:/Tuffy/Paper - Educ/Dados/Gio_sim.rds"))

df_fib <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/Gastos municipais/FINBRA/Despesas/FINBRA_EDU_05_21.csv")


df_pesosaeb <- df_pesosaeb %>% 
  rename(
    peso_5 = ano_5,
    peso_9 = ano_9
  )

df_trn <- df_trn %>% 
  left_join(df_pesosaeb,
            by = c("codigo_ibge" = "CO_MUNICIPIO", "ano" = "NU_ANO_CENSO"))


# null <- anti_join(sim_gio, df_sim)
# null <- anti_join(df_gio, df_trn)
# 
# all.equal(df_gio, df_trn)
# 
# summary(df_trn)
# summary(df_sim)
# summary(df_fib)
# 

pib <- read_excel("Z:/Tuffy/Paper - Educ/Dados/PIB dos Municípios - base de dados 2002-2009.xls") %>% 
  filter(Ano >= 2005) %>% 
  select(1, 7, 8, 40)


pib2 <- read_excel("Z:/Tuffy/Paper - Educ/Dados/PIB dos Municípios - base de dados 2010-2021.xlsx") %>% 
  select(1, 7, 8, 40)



df_regt <- df_trn %>% 
  left_join(df_sim %>% select(c(codigo_ibge, coef_est_fnde:receita_real, dif_rs_aluno, rs_por_aluno_fundeb, rs_por_aluno_sim)),
            by = "codigo_ibge") %>% 
  filter(!is.na(coef_est_fnde)) %>%
  mutate(k = ano - 2007) %>%    # 2007 é o ano base
  # filter(ano %% 2 != 0) %>% 
  mutate(uf = as.factor(uf))

# Escolha dos parâmetros:
rede_reg <- "Pública"
# rede_reg <- "Estadual"
# rede_reg <- "Municipal"
# rede_reg <- "Federal"


df_regt <- df_regt %>% 
  filter(
    case_when(
      ano >= 2005 & ano %% 2 != 0 ~ rede == rede_reg, # case_when: condição ~ valor se verdadeiro
      TRUE ~ TRUE                                      # TRUE ~TRUE é basicamente um else ~ valor padrão
    )
  ) %>%
  mutate(codigo_ibge = as.numeric(str_sub(as.character(codigo_ibge), 1, -2))) 



df_regt <- df_regt %>% 
  left_join((df_fib %>% select(-c(uf))), by = c("codigo_ibge", "ano")) %>% 
  mutate(des_edu_pc = educacao/populacao,
         des_fund_pc = ensino_fundamental/populacao,
         des_med_pc = ensino_medio/populacao,
         des_inf_pc = educacao_infantil/populacao) %>% 
  filter(ano < 2013 | (ano >= 2013 & coluna == "Despesas Empenhadas")) %>% 
  relocate(despesas_totais, .after= "nome") %>%
  relocate(educacao, .after = "despesas_totais") %>% 
  relocate(populacao, .after = "educacao") %>% 
  relocate(des_fund_pc, .after = "populacao") %>% 
  relocate(des_med_pc, .after = "populacao") %>% 
  relocate(des_inf_pc, .after = "populacao") %>% 
  
  
  group_by(codigo_ibge) %>%
  mutate(ed_spending_2006 = if_else(ano == 2006, educacao, NA_real_)) %>%
  fill(ed_spending_2006, .direction = "downup") %>% # Propaga o valor para todas as linhas do grupo
  ungroup() 




colnames(pib) <- c("ano", "codigo_ibge", "nom", "PIBpc")
colnames(pib2) <- c("ano", "codigo_ibge", "nom", "PIBpc")


pib <- bind_rows(
  pib,
  pib2) %>% 
  mutate(codigo_ibge = as.numeric(str_sub(as.character(codigo_ibge), 1, -2)))

df_regt <- left_join(
  df_regt,
  pib,
  by = c("codigo_ibge" , "ano")
) %>% 
  relocate(PIBpc, .after = "nome")

rm(pib2)

df_regt <- df_regt %>%
  filter (codigo_ibge > 10) %>% 
  mutate(dif_rs_aluno_100 = dif_rs_aluno / 100) %>%  # R$ PER STUDENT DOSAGE, em centenas
  
  # Criação da SPENDING DOSAGE:
  mutate(spending_dosage_gio = dif_rs_aluno/ed_spending_2006,
         spending_dos = receita_real/ed_spending_2006,
         del_spending_dos = (receita_real - receita_simulada)/ed_spending_2006)



### 5.1.2 Censo ---------
censo <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/censo_2006_filtrado.csv")




#Selecionando as variáveis de interesse
censo <- censo %>% 
  select(X, MASCARA, ANO, CODMUNIC, UF, SIGLA, MUNIC, DEP, LOC, DPE119,
         DPE11D, NPE119, NPE11D, reg_in, reg_fin, esp_total_final, em_tot,
         ed_inf_tot, eja_tot) %>% 
  arrange(CODMUNIC, ANO)



#Mutando as variáveis:
censo <- censo %>% 
  mutate(
    
    #Ensino Fund.
    # ##DIU
    # DEF11C = replace_na(DEF11C, 0),
    # DEF11D = replace_na(DEF11D, 0),
    # DEF11E = replace_na(DEF11E, 0),
    # DEF11F = replace_na(DEF11F, 0),
    # DEF11G = replace_na(DEF11G, 0),
    # DEF11H = replace_na(DEF11H, 0),
    # DEF11I = replace_na(DEF11I, 0),
    # DEF11J = replace_na(DEF11J, 0),
    # ##NOT
    # NEF11C = replace_na(NEF11C, 0),
    # NEF11D = replace_na(NEF11D, 0),
    # NEF11E = replace_na(NEF11E, 0),
    # NEF11F = replace_na(NEF11F, 0),
    # NEF11G = replace_na(NEF11G, 0),
    # NEF11H = replace_na(NEF11H, 0),
    # NEF11I = replace_na(NEF11I, 0),
    # NEF11J = replace_na(NEF11J, 0),
    # 
    # fund_aux = DEF11C + DEF11D + DEF11E + DEF11F + DEF11G + DEF11H + DEF11I + 
    #   DEF11J + NEF11C + NEF11D + NEF11E + NEF11F + NEF11G + NEF11H + NEF11I + NEF11J,
    rural_dum = ifelse(LOC == "Rural", 1, 0),
    total_loc = case_when(
      LOC == "Rural" ~ 1,
      LOC == "Urbana" ~ 1,
      TRUE ~ NA_real_
    ), 
    
    NPE119 = replace_na(NPE119, 0),
    NPE11D = replace_na(NPE11D, 0),
    DPE119 = replace_na(DPE119, 0),
    DPE11D = replace_na(DPE11D, 0),
    creche_aux = DPE119 + DPE11D,
    preesc_aux = NPE119 + NPE11D
  )




censo_df <- censo %>% 
  group_by(CODMUNIC, DEP) %>%
  summarise(
    
    nome = first(MUNIC),
    no_uf = first(UF),
    uf = first(SIGLA),
    mat_reg_in = sum(reg_in, na.rm = TRUE),
    mat_reg_fin   = sum(reg_fin, na.rm = TRUE),
    mat_tot_esp = sum(esp_total_final, na.rm = TRUE),
    #matri_fun = sum(fund_aux, na.rm = TRUE),
    
    mat_tot_em = sum(em_tot, na.rm = TRUE),
    
    matri_creche = sum(creche_aux, na.rm = TRUE),
    matri_preesc = sum(preesc_aux, na.rm = TRUE),
    matri_EI = matri_creche + matri_preesc,
    
    mat_tot_inf = sum(ed_inf_tot, na.rm = TRUE),
    mat_tot_eja = sum(eja_tot, na.rm = TRUE),
    
    rural_tot = sum(rural_dum),
    loc_tot = sum(total_loc),
    
    
    alunos_rur = sum(
      reg_in[rural_dum == 1] + reg_fin[rural_dum == 1] + esp_total_final[rural_dum == 1] +
        em_tot[rural_dum == 1] + matri_EI[rural_dum == 1] + eja_tot[rural_dum == 1],
      na.rm = T
    ),
    
    
    .groups = "drop"
  )

### 5.1.3 Unidos ----

df_regt <- df_regt %>% 
  left_join(censo_df %>% mutate(codigo_ibge = as.numeric(str_sub(as.character(CODMUNIC), 1, -2))),
                     by = c("codigo_ibge" = "codigo_ibge"))


test <- df_regt %>%
  filter(codigo_ibge > 60) %>% 
  # left_join(
  #   simulacao %>% select(codigo_ibge, coef_est_fnde, coef_simulado, dif_per_coef,
  #                        d_vaa, dif_coef_pp, receita_est_fnde, receita_simulada,
  #                        receita_real, shr_inf_em),
  #   by = c("CODMUNIC" = "codigo_ibge")
  #) %>% 
  group_by(codigo_ibge) %>%
  # select(-mat_tot_esp.y) %>% 
  # rename(mat_tot_esp = mat_tot_esp.x) %>% 
  mutate(
    
    #Criando as proporções
    shr_einf = matri_EI/(mat_tot_esp + mat_tot_em + matri_EI + mat_tot_eja + mat_reg_in + mat_reg_fin),
    shr_em = mat_tot_em/(mat_tot_esp + mat_tot_em + matri_EI + mat_tot_eja + mat_reg_in + mat_reg_fin),
    shr_eja = mat_tot_eja/(mat_tot_esp + mat_tot_em + matri_EI + mat_tot_eja + mat_reg_in + mat_reg_fin),
    shr_new = (matri_EI + mat_tot_em + mat_tot_eja)/
      (mat_tot_esp + mat_tot_em + matri_EI + mat_tot_eja + mat_reg_in + mat_reg_fin),
    
    #Pop. Rural
    shr_rur = rural_tot/ loc_tot,
    shr_alun_rur = alunos_rur/(mat_tot_esp + mat_tot_em + matri_EI + mat_tot_eja + mat_reg_in + mat_reg_fin),
  ) %>% 
  filter(ano == 2007,
         DEP == "Municipal")

##5.2 Gráficos ----

plot <- ggplot(test, aes(x = shr_einf, y = del_spending_dos)) +
  geom_point(size = 3, alpha = 0.6, color = "#2C3E50") +
  geom_smooth(method = "lm", se = F, linetype = 2, color = "red") +
  labs(
    title = "Adicionados vs. Diferença no ganho",
    x = "Prop. Adicionados (2006)",
    y = "Dif. Coef p.p."
  ) +
  theme_minimal(base_size = 14) +                      # clean theme
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right",
    panel.grid.minor = element_blank()                 # cleaner grid
  )
plot


# ---------------------------------------------------------------------------- #
# 6. Dosage  ----
# ---------------------------------------------------------------------------- #
rm(list = ls())

#' Here I will investigate the municipalities with highests values of the two main
#' dosage variables

# --------------------------------------- #
## 6.1 Data ----
# --------------------------------------- #


df_trn <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/painel_notas_transferencias_2000_2024.csv")

df_pesosaeb <- readRDS("Z:/Tuffy/Paper - Educ/Dados/pesos_saeb3.rds")

#df_gio <- readRDS("Z:/Tuffy/Paper - Educ/Dados/Gio_df.rds")

df_sim <- readRDS("Z:/Tuffy/Paper - Educ/Dados/simulacao_const.rds") #%>% 

#sim_gio <- readRDS(("Z:/Tuffy/Paper - Educ/Dados/Gio_sim.rds"))

df_fib <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/Gastos municipais/FINBRA/Despesas/FINBRA_EDU_05_21.csv")


### 6.1.1 Weights n PIB ----
df_pesosaeb <- df_pesosaeb %>% 
  rename(
    peso_5 = ano_5,
    peso_9 = ano_9
  )

df_trn <- df_trn %>% 
  left_join(df_pesosaeb,
            by = c("codigo_ibge" = "CO_MUNICIPIO", "ano" = "NU_ANO_CENSO"))


# null <- anti_join(sim_gio, df_sim)
# null <- anti_join(df_gio, df_trn)
# 
# all.equal(df_gio, df_trn)
# 
# summary(df_trn)
# summary(df_sim)
# summary(df_fib)
# 

pib <- read_excel("Z:/Tuffy/Paper - Educ/Dados/PIB dos Municípios - base de dados 2002-2009.xls") %>% 
  filter(Ano >= 2005) %>% 
  select(1, 7, 8, 40)


pib2 <- read_excel("Z:/Tuffy/Paper - Educ/Dados/PIB dos Municípios - base de dados 2010-2021.xlsx") %>% 
  select(1, 7, 8, 40)



### 6.1.2 Main Variables ----

#### 6.1.1 Grades and coef diff ----
temp <- df_trn %>% 
  left_join(df_sim %>% select(c(codigo_ibge, coef_est_fnde:receita_real, dif_rs_aluno,
                                rs_por_aluno_fundeb, rs_por_aluno_sim, shr_inf,
                                tot_matri, total_alunos_2006)),
            by = "codigo_ibge") %>% 
  filter(!is.na(coef_est_fnde)) %>%
  mutate(k = ano - 2007) %>%    # 2007 é o ano base
  # filter(ano %% 2 != 0) %>% 
  mutate(uf = as.factor(uf))

# Escolha dos parâmetros:
rede_reg <- "Pública"
# rede_reg <- "Estadual"
# rede_reg <- "Municipal"
# rede_reg <- "Federal"


temp <- temp %>% 
  filter(
    case_when(
      ano >= 2005 & ano %% 2 != 0 ~ rede == rede_reg, # case_when: condição ~ valor se verdadeiro
      TRUE ~ TRUE                                      # TRUE ~TRUE é basicamente um else ~ valor padrão
    )
  ) %>%
  mutate(codigo_ibge = as.numeric(str_sub(as.character(codigo_ibge), 1, -2))) 



temp <- temp %>% 
  left_join((df_fib %>% select(-c(uf))), by = c("codigo_ibge", "ano")) %>% 
  mutate(des_edu_pc = educacao/populacao,
         des_fund_pc = ensino_fundamental/populacao,
         des_med_pc = ensino_medio/populacao,
         des_inf_pc = educacao_infantil/populacao) %>% 
  filter(ano < 2013 | (ano >= 2013 & coluna == "Despesas Empenhadas")) %>% 
  relocate(despesas_totais, .after= "nome") %>%
  relocate(educacao, .after = "despesas_totais") %>% 
  relocate(populacao, .after = "educacao") %>% 
  relocate(des_fund_pc, .after = "populacao") %>% 
  relocate(des_med_pc, .after = "populacao") %>% 
  relocate(des_inf_pc, .after = "populacao") %>% 
  
  
  group_by(codigo_ibge) %>%
  mutate(ed_spending_2006 = if_else(ano == 2006, educacao, NA_real_)) %>%
  fill(ed_spending_2006, .direction = "downup") %>% # Propaga o valor para todas as linhas do grupo
  ungroup() 




colnames(pib) <- c("ano", "codigo_ibge", "nom", "PIBpc")
colnames(pib2) <- c("ano", "codigo_ibge", "nom", "PIBpc")


pib <- bind_rows(
  pib,
  pib2) %>% 
  mutate(codigo_ibge = as.numeric(str_sub(as.character(codigo_ibge), 1, -2)))

temp <- left_join(
  temp,
  pib,
  by = c("codigo_ibge" , "ano")
) %>% 
  relocate(PIBpc, .after = "nome")

rm(pib2)

df_reg <- temp %>%
  filter (codigo_ibge > 10) %>% 
  mutate(dif_rs_aluno_100 = dif_rs_aluno / 100) %>%  # R$ PER STUDENT DOSAGE, em centenas
  
  ##### 6.1.1.1. SPENDING DOSAGE: ----
mutate(spending_dosage_gio = dif_rs_aluno/ed_spending_2006,
       spending_dos = receita_real/ed_spending_2006,
       del_spending_dos = (receita_real - receita_simulada)/ed_spending_2006,
       
       dosage = (receita_real - receita_simulada)/receita_real, # Preferida
       
       dosage_perc = del_spending_dos *100,
       
       aluno_dosage = (receita_real - receita_simulada)/total_alunos_2006)


colnames(df_reg)

#Label
attr(df_reg$dosage, "label") <- "Parcela da diferença de receita pelo FUNDEB (2007)"
attr(df_reg$aluno_dosage, "label") <- "Diferença de receita (2007) por aluno (2006)"


teste <- df_reg %>% 
  select(2:5, 52, 53, 56, 58, 59, 61, 62, 65, 60, 77, 78, 81:89) %>% 
  select(-ano) %>% 
  distinct() %>% 
  select(1:3, dif_coef_pp, dif_rs_aluno, dif_rs_aluno_100, del_spending_dos,
         dosage, dosage_perc, aluno_dosage, shr_inf, everything())


#cleaning unecessary datasets
rm(df_fib, df_pesosaeb, df_sim, df_trn, pib, teste, rede_reg)


## 6.2  Dosage Graph ----

p <- ggplot(data = df_reg %>% 
              filter(tipo == "Municipal" & rede == "Pública"), 
            aes(x = shr_inf, y = (dosage)*100)) +
  geom_point(color = "#66c2a5", alpha = 0.7, size = 2) +
  labs(
    x = "Share de estudantes Infantil (2006) (%)",
    y = "Diferença de receita como proporção FUNDEB (2007) (%)"
  ) +
  scale_y_continuous(
    breaks = seq(-20, 100, by = 20),
    limits = c(-30, 100)
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    limits = c(0, 100)
  ) +
  # xlim(-5, 5)+
  #ylim(-30,100)+
  stat_smooth(method = "lm", se = FALSE, color = "#1b7837", linetype = "solid") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.6) +
  theme(
    text = element_text(family = "sans", color = "black", size = 11),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_markdown(hjust = 0, face = "italic")
  )

p

ggsave(
  filename = ("scatter_shr_dos.png"), # Nome baseado no modelo
  plot = p,
  path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter",
  width = 600/96, height = 420/96, dpi = 110)

rm(p, temp)


# --------------------------- #
## 6.3 Most gain ----
# -------------------------- #

#' Here I will investigate the municipalities that presented the most positive 
#' variation within the stablished dosage variable. In addition to naming the top
#' municipalities, I will coun the amount of positive variation between each UF.
#' 
#' However it will be necessary to remove the observations for the muncipalties
#' who exhibt the dosage variable equal to 100, since this result is tipical for
#' municipalities that did not receive any money through Fundef.


# ----------------------------- #
### 6.3.1 Top Municipalities ----
# ----------------------------- #

top_mun <- df_reg %>% 
  filter(dosage != 1,
         ano == 2007) %>% 
  arrange(desc(dosage)) %>% 
  slice_head(n = 15) %>% 
  select(uf,nome, dosage)



latex_table <- kable(
  top_mun,
  format = "latex",
  booktabs = TRUE,
  caption = "Dosage - Municípios mais beneficiados"
)

out_path <- "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/dosage_top_table.tex"

writeLines(latex_table, out_path)

message("Saved LaTeX table to: ", out_path)

rm(top_mun, latex_table)

# ------------------------------------------ #
### 6.3.2 Positive Gain ----
# ------------------------------------------ #


positive <- df_reg %>%
  filter(ano == 2007) %>% 
  mutate(
    positive_aux = ifelse(!is.na(dosage) & dosage > 0, 1, 0) #Dummy for positive values
  ) %>% 
  group_by(uf) %>% 
  summarise(
    sum_uf = sum(positive_aux, na.rm = T),
    count = n(),
    .groups = "drop"
  ) %>% 
  arrange(desc(sum_uf)) %>% 
  mutate(
    shr_top = paste0(round((sum_uf/count) * 100, digits = 0),"%") #Total share of municipalities with gain
  ) %>% 
  select(-count)

colnames(positive) <- c("UF", "Número", "Percentual")



latex_table <- kable(
  positive,
  format = "latex",
  booktabs = TRUE,
  caption = "Dosage - UFs mais beneficiadas"
)

out_path <- "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Desc/dosage_uf.tex"

writeLines(latex_table, out_path)

message("Saved LaTeX table to: ", out_path)


rm(latex_table, positive)



# ----------------------------------------------------------------- #
# 7. Student Dosage ----
# ----------------------------------------------------------------- #

p <- ggplot(data = df_reg %>% 
              filter(tipo == "Municipal" & rede == "Pública"), 
            aes(x = shr_inf, y = (aluno_dosage))) +
  geom_point(color = "#66c2a5", alpha = 0.7, size = 2) +
  labs(
    x = "Share de estudantes Infantil (2006) (%)",
    y = "Aluno Dosage; Diferença de receita por matrícula (R$)"
  ) +
  scale_y_continuous(
    breaks = seq(-400, 2000, by = 200)#,
    #limits = c(-30, 100)
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 20)#,
    #limits = c(0, 100)
  ) +
  # xlim(-5, 5)+
  #ylim(-30,100)+
  stat_smooth(method = "lm", se = FALSE, color = "#1b7837", linetype = "solid") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.6) +
  theme(
    text = element_text(family = "sans", color = "black", size = 11),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_markdown(hjust = 0, face = "italic")
  )

p

ggsave(
  filename = ("scatter_shr_alu_dos.png"), # Nome baseado no modelo
  plot = p,
  path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter",
  width = 600/96, height = 420/96, dpi = 110)

rm(p, temp)





# --------------------------- #
## 7.1 Most gain ----
# -------------------------- #

#' Here I will investigate the municipalities that presented the most positive 
#' variation within the student dosage variable. In addition to naming the top
#' municipalities, I will coun the amount of positive variation between each UF.
#' 
#' Likewise, it is necessary to remove the observations for the muncipalties
#' who exhibt the dosage variable equal to 100, since this result is tipical for
#' municipalities that did not receive any money through Fundef.


# ----------------------------- #
### 7.1.1 Top Municipalities ----
# ----------------------------- #

top_mun <- df_reg %>% 
  filter(dosage != 1, # We still remove the municipalities who did not participate in FUNDEF
         ano == 2007) %>% 
  arrange(desc(aluno_dosage)) %>% 
  slice_head(n = 15) %>% 
  select(uf,nome, aluno_dosage)



latex_table <- kable(
  top_mun,
  format = "latex",
  booktabs = TRUE,
  caption = "Aluno Dosage - Municípios mais beneficiados"
)

out_path <- "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/aluno_dosage_top_table.tex"

writeLines(latex_table, out_path)

message("Saved LaTeX table to: ", out_path)

rm(top_mun, latex_table)

# ------------------------------------------ #
### 7.1.2 Positive Gain ----
# ------------------------------------------ #

#' This part I kept for framework reasons. The result should not vary from the one
#' found in #6.3.2, since the difference from financial gain will be preserve the
#' sign noted in that section. The only difference for this dosage variable is linked
#' to the disperstion of the value through studentes numbers. The direction is still
#' the same.

positive <- df_reg %>%
  filter(ano == 2007) %>% 
  mutate(
    positive_aux = ifelse(!is.na(aluno_dosage) & aluno_dosage > 0, 1, 0) #Dummy for positive values
  ) %>% 
  group_by(uf) %>% 
  summarise(
    sum_uf = sum(positive_aux, na.rm = T),
    count = n(),
    .groups = "drop"
  ) %>% 
  arrange(desc(sum_uf)) %>% 
  mutate(
    shr_top = paste0(round((sum_uf/count) * 100, digits = 0),"%") #Total share of municipalities with gain
  ) %>% 
  select(-count)

colnames(positive) <- c("UF", "Número", "Percentual")



latex_table <- kable(
  top_tercil,
  format = "latex",
  booktabs = TRUE,
  caption = "Dosage - UFs mais beneficiadas"
)

out_path <- "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/aluno_dosage_uf.tex"

writeLines(latex_table, out_path)

message("Saved LaTeX table to: ", out_path)


rm(latex_table, positive)

# ---------------------------------------------------------------------------- #
# 8. Maps ----
# ---------------------------------------------------------------------------- #

library(sf)
library(geobr)

#' I will create two maps regarding Brazil's municipality exposure to the FUNDEB
#' using [color-coding] for most affected municipalities in Brazil.

df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") %>% # FROM 3.regressions_v2 file
  select(codigo_ibge, ano, dosage, aluno_dosage) %>% 
  filter(ano == 2007)

mun <- read_municipality(code_muni = "all", year = 2007) %>% #Downloading shapefile
  mutate(code_muni = code_muni %/% 10)


#Combining dataframes
df_mun <- df_reg %>% 
  left_join(mun, by = c("codigo_ibge" = "code_muni"))


rm(df_reg, mun)


# 3. Create the two maps
## ---- 8.1 Map 1: dosage ----

minv <- min(df_mun$dosage, na.rm = TRUE)   # e.g. -0.27
maxv <- max(df_mun$dosage, na.rm = TRUE)   # e.g.  1
p <- function(x) (x - minv) / (maxv - minv) # helper to convert values to 0..1

# Make a color ramp that changes very quickly between 0 and minv
cols <- c("#a50026", "#fbb4b9", "white", "#41ab5d", "#006d2c", "blue")
vals <- p(c(minv, minv * 0.08, 0, maxv * 0.03, maxv * 0.10, maxv * 0.9, maxv))  # small gap near zero on negative side

map_dosage <- ggplot(df_mun) +
  geom_sf(aes(fill = dosage, geometry = geom), color = NA) +
  scale_fill_gradientn(
    colours = cols,
    values = vals,
    limits = c(minv, maxv),
    oob = scales::squish,
    name = "Dosage",
    guide = guide_colorbar(barwidth = unit(6, "cm"), barheight = unit(0.5, "cm"))
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  #labs(title = "Dosage per Municipality") +
  theme(legend.position = "bottom")

map_dosage

ggsave(filename = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/Mapa/map_dosage.png"),plot = map_dosage ,
       device = "png", dpi = 300)




## 8.2 Map 2: aluno_dosage ----

minv <- min(df_mun$aluno_dosage, na.rm = TRUE)   # e.g. -0.27
maxv <- max(df_mun$aluno_dosage, na.rm = TRUE)   # e.g.  1
p <- function(x) (x - minv) / (maxv - minv) # helper to convert values to 0..1

# Make a color ramp that changes very quickly between 0 and minv
cols <- c("#a50026", "#fbb4b9", "white", "#41ab5d", "#006d2c", "blue")
vals <- p(c(minv, minv * 0.08, 0, maxv * 0.01, maxv * 0.10, maxv * 0.9, maxv))  # small gap near zero on negative side

map_aluno <- ggplot(df_mun) +
  geom_sf(aes(fill = aluno_dosage, geometry = geom), color = NA) +
  scale_fill_gradientn(
    colours = cols,
    values = vals,
    limits = c(minv, maxv),
    oob = scales::squish,
    name = "Aluno",
    guide = guide_colorbar(barwidth = unit(6, "cm"), barheight = unit(0.5, "cm"))
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  #labs(title = "Dosage per Municipality") +
  theme(legend.position = "bottom")

map_aluno

ggsave(filename = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/Mapa/map_aluno_dosage.png"), plot = map_aluno,
       device = "png", dpi = 300)


rm(list = ls())
gc()

# ---------------------------------------------------------------------------- #
# 9. Spending ----
# ---------------------------------------------------------------------------- #


#Dataframe
df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds")

df_spend <- df_reg %>% 
  filter(tipo == "Municipal") %>% 
  select(
    -c(vl_nota_5_matematica, vl_nota_5_portugues, vl_nota_5_media, vl_nota_9_matematica,
       vl_nota_9_media, vl_nota_9_portugues, vl_nota_em_matematica, vl_nota_em_portugues,
       vl_nota_em_media, tx_aprovacao_iniciais, tx_aprovacao_1, tx_aprovacao_2,
       tx_aprovacao_3, tx_aprovacao_4, tx_aprovacao_5, tx_aprovacao_finais, tx_aprovacao_6,
       tx_aprovacao_7, tx_aprovacao_8, tx_aprovacao_9, tx_aprovacao_em, tx_aprovacao_1em,
       tx_aprovacao_2em, tx_aprovacao_3em, tx_aprovacao_4em, rede, X.x, X.y, peso_5, peso_9)
  ) %>%
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
  )
  ) %>% 
  filter(tipo == "Municipal")

rm(df_reg)

#' Created the Dataframe I will repeat the graphs estimations utilizing the 
#' *spending levels* and present the results.


# ---------------------------------------------------------------------------- #
## 9.2 Winners spending (Lvl) ----
# ---------------------------------------------------------------------------- #

#' The objective of this part is to understantd how the FUNDEB policy affected
#' the municipality spending.

# ------------------ #
### 9.2.1 Time Spending ----
# ------------------ #

test1 <- feols(real_des_edu ~ i(k, ref = -1),
               data = df_spend,
               vcov = "hetero")

test2 <- feols(real_des_inf ~  i(k, ref = -1),
               data = df_spend,
               vcov = "hetero")


test3 <- feols(des_edu_pc ~ i(k, ref = -1),
               data = df_spend,
               vcov = "hetero")

test4 <- feols(des_inf_pc ~ i(k, ref = -1),
               data = df_spend,
               vcov = "hetero")



etable(test1, test2, test3, test4,
       vcov = "hetero",
       headers = list(":_:" = list("Total" = 1, "Infantil" = 1,
                                   #"Fundamental" = 1, "Médio" = 1,
                                   "Total" = 1, "Infantil" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v3/Tabelas/Dosage_aluno/time_real_spending.tex",
       replace = TRUE)



# ----------------------------------- #
## 9.3 Top winners ----
# ----------------------------------- #
### 9.3.1  Dataframes ----

#Top municipalities
df_topw <- df_spend %>% 
  filter(dosage == 1)


#medium municipalities
cutoff <- quantile(df_spend$dosage, 0.5, na.rm = TRUE)
dp     <- sd(df_spend$dosage, na.rm = TRUE)

df_midw <- df_spend %>%
  filter(dosage >= cutoff - dp*0.01,
         dosage <= cutoff + dp*0.01)


#High loss
cutoff <- quantile(df_spend$dosage, 0.1, na.rm = TRUE)

df_loww <- df_spend %>%
  filter(dosage >= cutoff - dp*0.01,
         dosage <= cutoff + dp*0.01)


# ------------------------------- #
#### 9.3.1.1 Radom selection ----
# ------------------------------- #

#Selection of 5 municipalities from each group

set.seed(123)  # for reproducibility


df_topw_sample <- df_topw %>%
  distinct(codigo_ibge, .keep_all = TRUE) %>%
  slice_sample(n = 5) 

df_midw_sample <- df_midw %>%
  distinct(codigo_ibge, .keep_all = TRUE) %>%
  slice_sample(n = 5) %>% 
  mutate( grupo = "Medium")

df_loww_sample <- df_loww %>%
  distinct(codigo_ibge, .keep_all = TRUE) %>%
  slice_sample(n = 5) %>% 
  mutate( grupo = "Low")


#Filtering the dataframes
df_topw <- df_topw %>% 
  filter(codigo_ibge %in% df_topw_sample$codigo_ibge) %>% 
  mutate( grupo = "High" ) #Group Indicator for segmentation


df_midw <- df_midw %>% 
  filter(codigo_ibge %in% df_midw_sample$codigo_ibge) %>% 
  mutate( grupo = "Medium" ) #Group Indicator for segmentation


df_loww <- df_loww %>% 
  filter(codigo_ibge %in% df_loww_sample$codigo_ibge) %>% 
  mutate( grupo = "Low" ) #Group Indicator for segmentation


# ------------------------------- #
#### 9.3.1.2 Final Data ----
# ------------------------------- #

df_sample <- rbind(df_topw,
                   df_midw,
                   df_loww
) %>% 
  mutate( grupo = as.factor(grupo))


rm(df_loww, df_midw, df_topw, cutoff, dp)


# ----------------------------------- #
### 9.3.2 Level ----
# ----------------------------------- #

# ------------------------------- #
#### 9.3.2.1 Regression OLS ----
# ------------------------------- #


est_edu <- feols( real_des_edu ~ i(k, grupo, ref = 0),
                  data = df_sample,
                  vcov = "hetero"
)

est_inf <- feols( real_des_inf ~ i(k, grupo, ref = 0),
                  data = df_sample,
                  vcov = "hetero")


etable(est_edu, est_inf)


# ------------------------------- #
#### 9.3.2.3 Graph (NO OLS) ----
# ------------------------------- #


summary_df <- df_sample %>%
  group_by(grupo, ano) %>%
  summarise(
    mean_des = mean(real_des_edu, na.rm = TRUE),
    sd_des   = sd(real_des_edu, na.rm = TRUE),
    n        = sum(!is.na(real_des_edu)),
    se_des   = ifelse(n>1, sd_des / sqrt(n), NA_real_),
    .groups = "drop"
  )


p <- ggplot() +
  # individual municipality trends (faint)
  geom_line(data = df_sample,
            aes(x = ano, y = real_des_edu, group = codigo_ibge, color = grupo),
            alpha = 0.35, size = 0.4, show.legend = FALSE) +
  # mean line per group
  geom_ribbon(data = summary_df,
              aes(x = ano, ymin = mean_des - se_des, ymax = mean_des + se_des, fill = grupo),
              alpha = 0.25, inherit.aes = FALSE, show.legend = FALSE) +
  geom_line(data = summary_df,
            aes(x = ano, y = mean_des, color = grupo),
            size = 1.1) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black") +
  facet_wrap(~ grupo, scales = "free_y") +               # or scales="fixed" if you want same y axis
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(x = "Ano", y = "real_des_edu", title = "Despesas Reais Educação",
       subtitle = "Thin lines = municipalities; bold = group mean ± SE; vertical = 2007") +
  theme_minimal() +
  theme(legend.position = "bottom")


ggsave(
  filename = paste0("grafico_despesas_reais.png"),
  plot = p,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/ES/Dosage_aluno",
  width = 1200/96, height = 420/96, dpi = 110
)


# ggplot(df_sample, aes(x = ano, y = real_des_edu, group = codigo_ibge, color = grupo)) +
#   geom_line(size = 0.7) +
#   geom_point(size = 0.6) +                     # optional points on years
#   geom_vline(xintercept = 2007, linetype = "dashed", color = "black") +
#   facet_wrap(~ codigo_ibge, ncol = 5, scales = "free_y") +  # 3 rows x 5 cols = 15 plots
#   scale_x_continuous(breaks = pretty_breaks(n = 6)) +
#   scale_color_brewer(palette = "Dark2") +     # nice palette; replace if you prefer
#   labs(x = "Ano", y = "des_edu", color = "Grupo",
#        title = "des_edu por município (cada painel = 1 município)",
#        subtitle = "Vertical dashed line = 2007") +
#   theme_minimal(base_size = 11) +
#   theme(
#     legend.position = "bottom",
#     strip.text = element_text(size = 9),
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )

# ------------------------------- #
#### 9.3.2.4 Winner Loser ----
# ------------------------------- #


mod_edu_ab <- feols(
  real_des_edu ~ aluno_dosage * i(k, grupo, ref = -1) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend,
  vcov = "hetero"
)


mod_inf_ab <- feols(
  real_des_inf ~ aluno_dosage * i(k, grupo, ref = -1) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend,
  vcov = "hetero"
)


etable(mod_edu_ab, mod_inf_ab,
       vcov = "hetero",
       headers = list(":_:" = list("Total"=1, "Infantil"=1)))



##### 9.3.2.4.1 Graph ----

models_list <- list(
  edu  = mod_edu_ab,
  inf  = mod_inf_ab
)



for (model_name in names(models_list)) {
  
  
  current_model <- models_list[[model_name]]
  temp <- broom::tidy(current_model, conf.int = TRUE) %>% 
    slice((which(term == "PIBpc") + 1):n()) %>%
    mutate(
      time_to_treat = str_extract(term, "(?<=k::)-?\\d+"),
      time_to_treat = as.numeric(time_to_treat),
      grupo = str_extract(term, "(?<=grupo::)\\w+"),
      grupo = as.factor(grupo))
  
  
  
  temp_edu <- temp %>%
    distinct(grupo) %>%             # one row per group (Above / Below)
    mutate(
      term = "time_to_treat:-1",     # or another label that fits your pattern
      estimate = 0,
      std.error = 0,
      statistic = 0,
      p.value = 1,
      conf.low = 0,
      conf.high = 0,
      time_to_treat = -1
    )
  
  # combine with your main dataframe
  temp <- bind_rows(temp, temp_edu) %>%
    arrange(grupo, time_to_treat) %>% 
    bind_rows(
      temp_edu %>%
        distinct(grupo) %>%
        mutate(
          time_to_treat = -1,
          estimate = 0,
          conf.low = 0,
          conf.high = 0
        )
    ) %>%
    arrange(grupo, time_to_treat)
  
  
  p <- ggplot(temp, aes(x = time_to_treat, y = estimate, group = grupo)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = -1, color = "black") +
    geom_point(aes(color = grupo), shape = 15, size = 2) +
    geom_line(aes(color = grupo)) +
    #facet_wrap(~ grupo, ncol = 1) +     # <--- separate plot per group
    labs(x = "Time to Treat", y = "R$ per capita",
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
      breaks = seq(min(temp$time_to_treat, na.rm = TRUE),
                   max(temp$time_to_treat, na.rm = TRUE),
                   by = 2)
    )
  
  
  if (model_name == "inf"){
    p <- p +
      theme(
        axis.line = element_line(color = "grey70"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 11),
        legend.position = c(0.95, 0.17),
        legend.justification = c("right", "top") # anchor legend box
      ) + # anchor legend box
      scale_x_continuous(
        breaks = seq(min(temp$time_to_treat, na.rm = TRUE),
                     max(temp$time_to_treat, na.rm = TRUE),
                     by = 2))
  }
  
  print(p)
  
  ggsave(
    filename = paste0("lvl_grafico_", model_name, "_Dos_aluno_AB.png"),
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/ES/Dosage_aluno",
    width = 600/96, height = 420/96, dpi = 110
  )
  rm(temp_edu)
}

# ----------------------------------- #
### 9.3.3 Per-capita ----
# ----------------------------------- #

# ------------------------------- #
#### 9.3.2.1 Regression OLS ----
# ------------------------------- #


est_edu <- feols( des_edu_pc ~ i(k, grupo, ref = 0),
                  data = df_sample,
                  vcov = "hetero"
)

est_inf <- feols( des_inf_pc ~ i(k, grupo, ref = 0),
                  data = df_sample,
                  vcov = "hetero")


etable(est_edu, est_inf)


# ------------------------------- #
#### 9.3.2.3 Graph (NO OLS) ----
# ------------------------------- #


summary_df <- df_sample %>%
  group_by(grupo, ano) %>%
  summarise(
    mean_des = mean(des_edu_pc, na.rm = TRUE),
    sd_des   = sd(des_edu_pc, na.rm = TRUE),
    n        = sum(!is.na(des_edu_pc)),
    se_des   = ifelse(n>1, sd_des / sqrt(n), NA_real_),
    .groups = "drop"
  )


p <- ggplot() +
  # individual municipality trends (faint)
  geom_line(data = df_sample,
            aes(x = ano, y = des_edu_pc, group = codigo_ibge, color = grupo),
            alpha = 0.35, size = 0.4, show.legend = FALSE) +
  # mean line per group
  geom_ribbon(data = summary_df,
              aes(x = ano, ymin = mean_des - se_des, ymax = mean_des + se_des, fill = grupo),
              alpha = 0.25, inherit.aes = FALSE, show.legend = FALSE) +
  geom_line(data = summary_df,
            aes(x = ano, y = mean_des, color = grupo),
            size = 1.1) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black") +
  facet_wrap(~ grupo, scales = "fixed") +               # or scales="fixed" if you want same y axis
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(x = "Ano", y = "des_edu_pc", title = "Despesas Reais Educação per-capita",
       subtitle = "Thin lines = municipalities; bold = group mean ± SE; vertical = 2007") +
  theme_minimal() +
  theme(legend.position = "bottom")


ggsave(
  filename = paste0("grafico_despesas_reais_pc.png"),
  plot = p,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/ES/Dosage_aluno",
  width = 1200/96, height = 420/96, dpi = 110
)


# ---------------------------------------------------------------------------- #
# 10. Old Dosage ----
# ---------------------------------------------------------------------------- #


# -------------------------------- #
## 10.1 Main ----
# -------------------------------- #
test1 <- feols(real_des_edu ~ old_dosage : i(k, ref = -1),
               data = df_spend,
               vcov = "hetero")

test2 <- feols(real_des_inf ~ old_dosage : i(k, ref = -1),
               data = df_spend,
               vcov = "hetero")


test3 <- feols(des_edu_pc ~ old_dosage : i(k, ref = -1),
               data = df_spend,
               vcov = "hetero")

test4 <- feols(des_inf_pc ~ old_dosage : i(k, ref = -1),
               data = df_spend,
               vcov = "hetero")


etable(test1, test2, test3, test4
       )


rm(test1, test2, test3, test4)

# --------------------------------- #
## 10.2 Groups -----
# --------------------------------- #
### 10.2.1 Regression ----

est1 <- feols(des_edu_pc ~ abs(old_dosage) : i(k, grupo, ref = -1),
               data = df_spend,
               vcov = "hetero")

est2 <- feols(des_inf_pc ~ abs(old_dosage) : i(k, grupo, ref = -1),
               data = df_spend,
               vcov = "hetero")

est3 <- feols(des_med_pc ~ abs(old_dosage) : i(k, grupo, ref = -1),
              data = df_spend,
              vcov = "hetero")

est4 <- feols(des_fund_pc ~ abs(old_dosage) : i(k, grupo, ref = -1),
              data = df_spend,
              vcov = "hetero")


etable(est1, est2)


# --------------------------------- #
### 10.2.2 Graph ----
# --------------------------------- #

win_lose_plot <- function(est_obj, title = NULL, ylim = NULL) {
  # extract tidy data from fixest::etable or broom::tidy
  event_df <- broom::tidy(est_obj, conf.int = TRUE) %>% 
    mutate(
      k = str_extract(term, "(?<=k::)-?\\d+"),
      k = as.numeric(k),
      # Extract group (text after the last colon)
      grupo = str_extract(term, "[^:]+$"),
      grupo = as.factor(grupo))
  
  
  event_df <- event_df %>% 
    bind_rows(
      event_df %>%
        distinct(grupo) %>%
        mutate(
          term = paste0(as.character(grupo),"k:-1"),     
          estimate = 0,
          std.error = 0,
          statistic = 0,
          p.value = 1,
          conf.low = 0,
          conf.high = 0,
          k = -1
        )
    ) %>% 
    filter(grupo != "PIBpc",
           grupo != "(Intercept)")
  
  ggplot(event_df, aes(x = k + 2007, y = estimate, group = grupo)) +
    # shaded standard error area
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 2006, color = "black") +
    geom_point(aes(color = grupo), shape = 15, size = 2) +
    geom_line(aes(color = grupo)) +
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

#### 10.2.2.1 Plots ----

p_edu_pc <- win_lose_plot(est1, "Gastos Educacionais Per-capita")   #Total Education spending
p_inf_pc <- win_lose_plot(est2, "Gastos E. Infantil Per-capita")  #Preschool spending
p_med_pc <- win_lose_plot(est3, "Ensino Médio")
p_fun_pc <- win_lose_plot(est4, "Fundamental")

grid_plot <- ( p_edu_pc + p_inf_pc ) 

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("old_dosage_spending.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/ES/Robust/", #Saving directly to the report
  width = 1300/96, height = 620/96, dpi = 300
)



rm(list = ls())
gc()

# ---------------------------------------------------------------------------- #
# 11. Students numbers ----
# ---------------------------------------------------------------------------- #
## 11.1 Data ----
# ---------------------------------------------------------------------------- #

#' Opening the dataset with the enrollments for each year and each education level.

df_enroll <- readRDS("Z:/Tuffy/Paper - Educ/Dados/censo_escolar_base_v2.rds") %>% 
  group_by(codmun, ano) %>% 
  summarise(
    mat_fun = sum(ef_tot, na.rm = T),
    mat_med = sum(em_tot, na.rm = T),
    mat_inf = sum(day_tot + pre_tot, na.rm = T),
    mat_esp = sum(esp_tot, na.rm = T),
    mat_eja = sum(eja_tot, na.rm = T),
    mat_total = mat_fun + mat_med + mat_inf + mat_eja + mat_esp,
    .groups = "drop") %>% 
  mutate(codmun = codmun %/% 10) %>% 
  rename(codigo_ibge = codmun)

# Main data
df_main <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") %>% 
  filter(ano == 2007) %>% 
  #Groups
  mutate(grupo = case_when(
    dosage > 0 ~ "Winner",   # net beneficiary
    dosage < 0 ~ "Loser",   # net contributer
    TRUE ~ NA_character_
    )) %>%  
  select(codigo_ibge, grupo)

#creatng the filter variable list

data <- df_enroll %>% 
  left_join(df_main, by = c("codigo_ibge"))


rm(df_main, df_enroll)


# ---------------------------------------------------------------------------- #
## 11.2 N students ----
# ---------------------------------------------------------------------------- #
#' For the number of students per education level I will sum the total number of
#' them, combining per municipality

df_new <- data %>% 
  group_by(ano, grupo) %>% 
  summarise(mat_fun = sum(mat_fun, na.rm = T),
            mat_med = sum(mat_med, na.rm = T),
            mat_inf = sum(mat_inf, na.rm = T),
            mat_esp = sum(mat_esp, na.rm = T),
            mat_eja = sum(mat_eja, na.rm = T),
            mat_total = sum(mat_total, na.rm = T),
            .groups = "drop") %>% 
  filter(!is.na(grupo))

# ---------------------------------------------------------------------------- #
### 11.2.1 Saving ----
# ---------------------------------------------------------------------------- #


edu <- c("mat_fun", "mat_med", "mat_inf", "mat_esp", "mat_eja", "mat_total")

for(i in edu) {
  
  message("Starting... ",i,":")
  #Graph creation 
  plot <- ggplot(df_win %>% filter(!is.na(grupo)),
                 aes(x = ano, y = .data[[i]], color = grupo, group = grupo)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    theme_classic() +
    labs(
      x = "Ano",
      y = "Matrículas Totais",
      color = "Grupo"
    )
  
  print(plot)
  #saving plot
  ggsave(
    filename = paste0("num_",i,".png"),
    plot = plot,
    path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/Mat/",
    width = 1200/96, height = 420/96, dpi = 110
  )
  
  
  message("saving ",i,":")
  rm(i, plot)
  
}








