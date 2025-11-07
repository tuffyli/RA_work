# ---------------------------------------------------------------------------- #
# Regressions
# Last edited by: Tuffy Licciardi Issa
# Date: 07/11/2025
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


#Deactivating scientific notation
options(scipen = 999)

# --------------------------------------------------------------------------- #
# 1. Regression ----
# --------------------------------------------------------------------------- #

df_trn <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/painel_notas_transferencias_2000_2024.csv")

df_pesosaeb <- readRDS("Z:/Tuffy/Paper - Educ/Dados/pesos_saeb3.rds")

#df_gio <- readRDS("Z:/Tuffy/Paper - Educ/Dados/Gio_df.rds")

df_sim <- readRDS("Z:/Tuffy/Paper - Educ/Dados/simulacao_const.rds") #%>% 

#sim_gio <- readRDS(("Z:/Tuffy/Paper - Educ/Dados/Gio_sim.rds"))

df_fib <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/Gastos municipais/FINBRA/Despesas/FINBRA_EDU_05_21.csv")


#1.0 Pesos e PIB ----
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



## 1.1 Incluindo as variáveis principais ----
#[English: Including Major Variables]

### 1.1.1 Incluindo dif_per_coef nas notas: ----
#[English: Including dif_per_coef in exam scores]
temp <- df_trn %>% 
  left_join(df_sim %>% select(c(codigo_ibge, coef_est_fnde:receita_real, dif_rs_aluno,
                                rs_por_aluno_fundeb, rs_por_aluno_sim, shr_inf,
                                tot_matri, total_alunos_2006)),
            by = "codigo_ibge") %>% 
  filter(!is.na(coef_est_fnde)) %>%
  mutate(k = ano - 2007) %>%    # 2007 é o ano base [English: 2007 is the base year]
  # filter(ano %% 2 != 0) %>% 
  mutate(uf = as.factor(uf))

# Escolha dos parâmetros:
# [English: Choosing parameters]
rede_reg <- "Pública"           #Public
# rede_reg <- "Estadual"        #State
# rede_reg <- "Municipal"       #Municipal
# rede_reg <- "Federal"         #Federal


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
  ungroup()                                         # [English: reproducing the values through groups]




colnames(pib) <- c("ano", "codigo_ibge", "nom", "PIBpc")
colnames(pib2) <- c("ano", "codigo_ibge", "nom", "PIBpc")


pib <- bind_rows( # [English: Combining the PIB per-capita from different years]
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
  
  ##### SPENDING DOSAGE: ----
mutate(#spending_dosage_gio = dif_rs_aluno/ed_spending_2006,
       #spending_dos = receita_real/ed_spending_2006,
       #del_spending_dos = (receita_real - receita_simulada)/ed_spending_2006,
       
       dosage = (receita_real - receita_simulada)/receita_real,            #Prefered
       
       dosage_perc = del_spending_dos *100,
       
       aluno_dosage = (receita_real - receita_simulada)/total_alunos_2006) #Prefered


colnames(df_reg)

#Label
attr(df_reg$dosage, "label") <- "Parcela da diferença de receita pelo FUNDEB (2007)"
attr(df_reg$aluno_dosage, "label") <- "Diferença de receita (2007) por aluno (2006)"



#saving database with dosage
saveRDS(df_reg, "Z:/Tuffy/Paper - Educ/Dados/regdf.rds")

teste <- df_reg %>% 
  select(2:5, 52, 53, 56, 58, 59, 61, 62, 65, 60, 77, 78, 81:89) %>% 
  select(-ano) %>% 
  distinct() %>% 
  select(1:3, dif_coef_pp, dif_rs_aluno, dif_rs_aluno_100, del_spending_dos,
         dosage, dosage_perc, aluno_dosage, shr_inf, everything())


### 1.1.2 Graph ----
# [English: Graph]

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
    #limits = c(-30, 100)
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 20),
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
  filename = ("scatter_shr_dos.png"), # Nome baseado no modelo
  plot = p,                           # [English: Name based model]
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/Scatter",
  width = 600/96, height = 420/96, dpi = 110)

rm(p, temp)

#### Dosage de alunos
# [English: Student dosage]
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
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/Scatter",
  width = 600/96, height = 420/96, dpi = 110)

rm(p, temp)


# ---------------------------------------------------------------------------- #
# 2. Education Spending ----
# ---------------------------------------------------------------------------- #
## 2.1 Nova Base ----

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
  filter(tipo == "Municipal")


rm(df_trn, df_sim, df_pesosaeb, df_fib)


## 2.2 Dosage ----
# Spending Dosage New
mod_edu <- feols(des_edu_pc ~ dosage * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 vcov = "hetero")


mod_fund <- feols(des_fund_pc ~ dosage * i(k, ref = 0)
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_spend,
                  vcov = "hetero")

mod_med <- feols(des_med_pc ~ dosage * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 vcov = "hetero")

mod_inf <- feols(des_inf_pc ~ dosage * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 vcov = "hetero")



etable(mod_edu, mod_fund, mod_med, mod_inf, 
       vcov = "hetero", 
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)))



# Exportar:

etable(mod_edu, mod_fund, mod_med, mod_inf,
       vcov = "hetero",
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/reg_dif_dosage.tex", replace = TRUE)



### 2.2.1 ES ----


models_list <- list(
  edu  = mod_edu,
  fund = mod_fund,
  med  = mod_med,
  inf  = mod_inf
)

for (model_name in names(models_list)){
  
  current_model <- models_list[[model_name]]
  event_df <- as.data.frame(current_model$coeftable)
  print(event_df)
  
  # Criar coluna com os termos (anos relativos k)
  event_df$term <- rownames(event_df)
  event_df <- event_df %>%
    mutate(
      k = as.numeric(gsub(".*k::(-?\\d+)$", "\\1", term)), # Extrair o valor de k
      conf.low = Estimate - 1.96 * `Std. Error`,           # Limite inferior do IC
      conf.high = Estimate + 1.96 * `Std. Error`           # Limite superior do IC
    ) %>%
    add_row(
      Estimate = 0,
      `Std. Error` = 0,
      `t value` = 0,
      `Pr(>|t|)` = 0,
      term = "k::0", # Adicionar termo com referência ao modelo
      k = 0,
      conf.low = 0,
      conf.high = 0
    )
  
  # Criar o gráfico de estilo event-study
  p <- ggplot(event_df, aes(x = k + 2007, y = Estimate)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey60", alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 2007, color = "black") +
    geom_point(shape = 15, size = 2, color = "black") +
    geom_line(color = "black") +
    labs(
      # title = paste("Event-Study: ", model_name),
      x = "Ano", #(relativo a 2007)",
      y = "R$ per capita"
    ) +
    # ylim(-0.4, 0.4) +
    theme_classic() +
    theme(
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11)
    )
  
  # Exibir o gráfico no console
  print(p)
  
  # Salvar o gráfico como arquivo PNG
  ggsave(
    filename = paste0("grafico_", model_name, "dosage_des_edu.png"), # Nome baseado no modelo
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Dosage",
    width = 600/96, height = 420/96, dpi = 110
  )
  
  rm(model_name, p)
}

rm(mod_edu, mod_fund, mod_inf, mod_med, event_df, current_model, models_list)

# ------ #
### 2.2.2 Region ----

region_list <- unique(df_spend$region)


for (area in region_list) {
  
  ini <- Sys.time()
  
  message("Starting for region: ", area)
  
  
  temp <- df_spend %>% 
    filter(region == area)
  
  mod_edu <- feols(des_edu_pc ~ dosage * i(k, ref = 0)
                   + PIBpc
                   | codigo_ibge + ano + uf^ano,
                   data = df_spend,
                   vcov = "hetero")
  
  
  mod_fund <- feols(des_fund_pc ~ dosage * i(k, ref = 0)
                    + PIBpc
                    | codigo_ibge + ano + uf^ano,
                    data = df_spend,
                    vcov = "hetero")
  
  mod_med <- feols(des_med_pc ~ dosage * i(k, ref = 0)
                   + PIBpc
                   | codigo_ibge + ano + uf^ano,
                   data = df_spend,
                   vcov = "hetero")
  
  mod_inf <- feols(des_inf_pc ~ dosage * i(k, ref = 0)
                   + PIBpc
                   | codigo_ibge + ano + uf^ano,
                   data = df_spend,
                   vcov = "hetero")
  
  
  
  message("Finished for: ", area)
  
  
  
  
  etable(mod_edu, mod_fund, mod_med, mod_inf,
         vcov = "hetero",
         headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)),
         file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/region/region_",
                       tolower(area),"_gastos_educ.tex"), replace = TRUE)
  
  
  message("Saved final table (", area,")")
  fim <- Sys.time()
  
  
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  
  message("---------------------------------------------")
  message("Total time elapsed: ",mins," mins e ", secs, " s")
  message("---------------------------------------------")
  
  rm(temp, delta, ini, fim, mins, secs, mod_inf, mod_med, mod_fund, mod_edu)
  
}


## 2.3 Dosage Aluno ----

# Dosage Student
mod_edu <- feols(des_edu_pc ~ aluno_dosage * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 vcov = "hetero")


mod_fund <- feols(des_fund_pc ~ aluno_dosage * i(k, ref = 0)
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_spend,
                  vcov = "hetero")

mod_med <- feols(des_med_pc ~ aluno_dosage * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 vcov = "hetero")

mod_inf <- feols(des_inf_pc ~ aluno_dosage * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 vcov = "hetero")



etable(mod_edu, mod_fund, mod_med, mod_inf, 
       vcov = "hetero", 
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)))



# Exportar:

etable(mod_edu, mod_fund, mod_med, mod_inf,
       vcov = "hetero",
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage_aluno/reg_dif_aluno_dosage.tex", replace = TRUE)

### 2.3.1 ES ----

models_list <- list(
  edu  = mod_edu,
  fund = mod_fund,
  med  = mod_med,
  inf  = mod_inf
)

for (model_name in names(models_list)){
  
  current_model <- models_list[[model_name]]
  event_df <- as.data.frame(current_model$coeftable)
  print(event_df)
  
  # Criar coluna com os termos (anos relativos k)
  event_df$term <- rownames(event_df)
  event_df <- event_df %>%
    mutate(
      k = as.numeric(gsub(".*k::(-?\\d+)$", "\\1", term)), # Extrair o valor de k
      conf.low = Estimate - 1.96 * `Std. Error`,           # Limite inferior do IC
      conf.high = Estimate + 1.96 * `Std. Error`           # Limite superior do IC
    ) %>%
    add_row(
      Estimate = 0,
      `Std. Error` = 0,
      `t value` = 0,
      `Pr(>|t|)` = 0,
      term = "k::0", # Adicionar termo com referência ao modelo
      k = 0,
      conf.low = 0,
      conf.high = 0
    ) %>% 
    filter(!is.na(k))
  
  # Criar o gráfico de estilo event-study
  p <- ggplot(event_df, aes(x = k + 2007, y = Estimate)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey60", alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 2007, color = "black") +
    geom_point(shape = 15, size = 2, color = "black") +
    geom_line(color = "black") +
    labs(
      # title = paste("Event-Study: ", model_name),
      x = "Ano", #(relativo a 2007)",
      y = "R$ per capita"
    ) +
    # ylim(-0.4, 0.4) +
    theme_classic() +
    theme(
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11)
    )
  
  # Exibir o gráfico no console
  print(p)
  
  # Salvar o gráfico como arquivo PNG
  ggsave(
    filename = paste0("grafico_", model_name, "dosage_alun_des_edu.png"), # Nome baseado no modelo
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Dosage_aluno",
    width = 600/96, height = 420/96, dpi = 110
  )
  
  rm(model_name, p)
}

rm(mod_edu, mod_fund, mod_inf, mod_med, event_df, current_model, models_list)


### 2.3.2 Region ----

region_list <- unique(df_spend$region)


for (area in region_list) {
  
  ini <- Sys.time()
  
  message("Starting for region: ", area)
  
  
  temp <- df_spend %>% 
    filter(region == area)
  
  mod_edu <- feols(des_edu_pc ~ aluno_dosage * i(k, ref = 0)
                   + PIBpc
                   | codigo_ibge + ano + uf^ano,
                   data = df_spend,
                   vcov = "hetero")
  
  
  mod_fund <- feols(des_fund_pc ~ aluno_dosage * i(k, ref = 0)
                    + PIBpc
                    | codigo_ibge + ano + uf^ano,
                    data = df_spend,
                    vcov = "hetero")
  
  mod_med <- feols(des_med_pc ~ aluno_dosage * i(k, ref = 0)
                   + PIBpc
                   | codigo_ibge + ano + uf^ano,
                   data = df_spend,
                   vcov = "hetero")
  
  mod_inf <- feols(des_inf_pc ~ aluno_dosage * i(k, ref = 0)
                   + PIBpc
                   | codigo_ibge + ano + uf^ano,
                   data = df_spend,
                   vcov = "hetero")
  
  
  
  message("Finished for: ", area)
  
  
  
  
  etable(mod_edu, mod_fund, mod_med, mod_inf,
         vcov = "hetero",
         headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)),
         file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage_aluno/region/region_",
                       tolower(area),"_gastos_educ.tex"), replace = TRUE)
  
  
  message("Saved final table (", area,")")
  fim <- Sys.time()
  
  
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  
  message("---------------------------------------------")
  message("Total time elapsed: ",mins," mins e ", secs, " s")
  message("---------------------------------------------")
  
  rm(temp, delta, ini, fim, mins, secs, mod_inf, mod_med, mod_fund, mod_edu)
  
}

rm(list = ls())
# ---------------------------------------------------------------------------- #
#3. Daycare & Preschool ----
# ---------------------------------------------------------------------------- #

#' In this section I will breakdown the effects for various variables associated
#' with the daycare and preschool system.

# ------------------------- #
## 3.0 Data ----
# ------------------------- #


df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") 
  





# ------------------------- #
## 3.1 Enrollment ----




# ------------------------ #
## 3.2 Schools ----





# ---------------------- #
## 3.3 Spending ----
