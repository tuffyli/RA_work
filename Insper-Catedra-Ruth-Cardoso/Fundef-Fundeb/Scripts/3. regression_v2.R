# ---------------------------------------------------------------------------- #
# Regressions
# Last edited by: Tuffy Licciardi Issa
# Date: 26/11/2025
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

# --------------------------------------------------------------------------- #
# 1. Regression ----
# --------------------------------------------------------------------------- #

df_trn <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/painel_notas_transferencias_2000_2024.csv")

df_pesosaeb <- readRDS("Z:/Tuffy/Paper - Educ/Dados/pesos_saeb3.rds")

#df_gio <- readRDS("Z:/Tuffy/Paper - Educ/Dados/Gio_df.rds")

df_sim <- readRDS("Z:/Tuffy/Paper - Educ/Dados/simulacao_const.rds") #%>% 

#sim_gio <- readRDS(("Z:/Tuffy/Paper - Educ/Dados/Gio_sim.rds"))

df_fib <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/Gastos municipais/FINBRA/Despesas/FINBRA_EDU_05_21.csv")


#1.0 Weights & GDP ----
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
       
       #dosage_perc = del_spending_dos *100,
       
       aluno_dosage = (receita_real - receita_simulada)/total_alunos_2006) #Prefered


colnames(df_reg)

#Label
attr(df_reg$dosage, "label") <- "Parcela da diferença de receita pelo FUNDEB (2007)"
attr(df_reg$aluno_dosage, "label") <- "Diferença de receita (2007) por aluno (2006)"



#saving database with dosage
saveRDS(df_reg, "Z:/Tuffy/Paper - Educ/Dados/regdf.rds")

teste <- df_reg %>% 
  select(2:5, 52, 53, 56, 58, 59, 61, 62, 65, 60, 77, 78, 81:85) %>% 
  select(-ano) %>% 
  distinct() %>% 
  select(1:3, dif_coef_pp, dif_rs_aluno, dif_rs_aluno_100,
         dosage, aluno_dosage, shr_inf, everything())


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

rm(p)

#### 1.2.2.1 PIBpc ----


df07 <- df_reg %>%
  filter(tipo == "Municipal", rede == "Pública", as.numeric(ano) == 2007)

p <- ggplot(df07, aes(x = PIBpc, y = dosage * 100)) +
  geom_point(alpha = 0.7, size = 2, color = "#66c2a5") +
  geom_smooth(method = "lm", se = FALSE, color = "#1b7837") +
  labs(
    x = "PIB per-capita (2007) R$",
    y = "Diferença de receita como proporção FUNDEB (2007) (%)"
  ) +
  # scale_y_continuous(
  #   breaks = seq(-20, 100, by = 20),
  #   #limits = c(-30, 100)
  # ) +
  # scale_x_continuous(
  #   breaks = seq(0, 100, by = 20),
  #   #limits = c(0, 100)
  # ) +
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
  filename = ("scatter_pibpc_dos.png"), # Nome baseado no modelo
  plot = p,                           # [English: Name based model]
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/Scatter",
  width = 600/96, height = 420/96, dpi = 110)

rm(p, temp)

#### Dosage de alunos
# [English: Student dosage]

p <- ggplot(df07, aes(x = PIBpc, y = aluno_dosage)) +
  geom_point(alpha = 0.7, size = 2, color = "#66c2a5") +
  geom_smooth(method = "lm", se = FALSE, color = "#1b7837") +
  labs(
    x = "PIB per-capita (2007) R$",
    y = "Aluno Dosage; Diferença de receita por matrícula (R$)"
  ) +
  # scale_y_continuous(
  #   breaks = seq(-400, 2000, by = 200)#,
  #   #limits = c(-30, 100)
  # ) +
  # scale_x_continuous(
  #   breaks = seq(0, 100, by = 20)#,
  #   #limits = c(0, 100)
  # ) +
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
  filename = ("scatter_pibpc_ados.png"), # Nome baseado no modelo
  plot = p,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/Scatter",
  width = 600/96, height = 420/96, dpi = 110)

rm(p)




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



# Export:
etable(mod_edu, mod_fund, mod_med, mod_inf,
       vcov = "hetero",
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/reg_dif_dosage.tex", replace = TRUE)



### 2.2.1 ES ----


models_list <- list(
  edu  = mod_edu,
  #fund = mod_fund,
  #med  = mod_med,
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
  #fund = mod_fund,
  #med  = mod_med,
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
gc()




# ---------------------------------------------------------------------------- #
#3. School Data ----
# ---------------------------------------------------------------------------- #

#' In this section I will breakdown the effects for various variables associated
#' with the daycare and preschool system.
#' 
#' First I will define the graph function for the remaining estimations
#' 


# ------------------------- #
## 3.1 Data (Mun Lvl) ----
# ------------------------- #

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
df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") %>% 
  select(codigo_ibge, ano, uf, nome, dif_coef_pp, dosage, aluno_dosage, PIBpc,
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
## 3.2 School Characteristics ----
# ---------------------------------------------------------------------------- #
# -------------------- #
### 3.2.1 Structural REG ----
# -------------------- #

#' ***************
#' DISCLAIMER: Here I employed the -1 period as reference. If I want to resume
#' the usage to 0, as previously, I should change all [-1] values for [0] in the
#' codelines that follows
#' ***************

# ------------------ #
#### 3.2.1.1 Dosage -----

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

#Student/Employee 
est_employ <- feols(employee ~ dosage * i(k, ref = -1)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")

#Total employees
est_n_employ <- feols(n_employee ~ dosage * i(k, ref = -1)
                      + PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb,
                      vcov = "hetero")

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ, est_n_employ)


##### 3.2.1.1.1 Graph ----
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
p_nemploy <- event_style_plot(est_employ,"N° Funcionários") #N Employee
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
  filename = paste0("school_structure_dosage.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/",
  width = 800/96, height = 620/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final)

# ----------------- #
#### 3.2.1.2 Aluno Dosage ----
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

#Total employees
est_n_employ <- feols(n_employee ~ aluno_dosage * i(k, ref = -1)
                      + PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb,
                      vcov = "hetero")


etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ, est_n_employ)


##### 3.2.1.2.1 Graph ----


# 3) Apply helper to each ggiplot object
p_class <- event_style_plot(est_class, "Salas de aula")   #Classroom
p_troom <- event_style_plot(est_troom, "Sala dos Prof.")  #Teacher's Room
p_labs  <- event_style_plot(est_labs,  "Laboratórios")    #Laboratory
p_libra <- event_style_plot(est_libra, "Biblioteca")      #Library
p_play  <- event_style_plot(est_play,  "Quadras/Parque")  #Play Area
p_lunch <- event_style_plot(est_lunch, "Merenda")         #Lunch
p_employ <- event_style_plot(est_employ,"Funcionários")   #Employee
p_nemploy <- event_style_plot(est_n_employ, "N° Funcionários")


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
  filename = paste0("school_structure_aluno_dosage.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/",
  width = 800/96, height = 620/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra, p_nemploy,
   final, est_class, est_employ, est_labs, est_libra, est_lunch, est_play, est_n_employ,
   est_troom)


# -------------------- #
### 3.2.2 Courses REG ----
# -------------------- #

#' ***************
#' DISCLAIMER: Here I employed the -1 period as reference. If I want to resume
#' the usage to 0, as previously, I should change all [-1] values for [0] in the
#' codelines that follows
#' ***************

# ------------------ #
#### 3.2.2.1 Dosage -----

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
#                      codmun + ano + uf^ano,
#                    data = df_comb,
#                    vcov = "hetero")



##### 3.2.2.1.1 Graph ----
p_kinder <- event_style_plot(est_kinder, "Fração Alunos E. Infantil")  #Preschool
p_elem <- event_style_plot(est_elem, "Fração Alunos E. Fundamental")   #Elementary
p_high  <- event_style_plot(est_high,  "Fração Alunos E. Médio")       #HighSchool
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
  filename = paste0("school_course_dosage.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/",
  width = 800/96, height = 620/96, dpi = 300
)

rm(p_kinder, p_elem, p_high, final)

# ----------------- #
#### 3.2.2.2 Aluno Dosage ----

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

#Inclusion
# est_inclu <- feols(inclusion ~ aluno_dosage * i(k, ref = -1)
#                    + PIBpc |
#                      codmun + ano + uf^ano,
#                    data = df_comb,
#                    vcov = "hetero")



##### 3.2.2.2.1 Graph ----
p_kinder <- event_style_plot(est_kinder, "Fração Alunos E. Infantil")  #Preschool
p_elem <- event_style_plot(est_elem, "Fração Alunos E. Fundamental")   #Elementary
p_high  <- event_style_plot(est_high,  "Fração Alunos E. Médio")       #HighSchool
# p_inclu <- event_style_plot(est_inclu, "E. Especial")    #Speical Education

blank <- ggplot() + theme_void()

grid_plot <- (p_kinder + p_elem) /
  (p_high + blank) 

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("school_course_aluno_dosage.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/",
  width = 800/96, height = 620/96, dpi = 300
)

rm(p_kinder, p_elem, p_high, p_inclu, final, est_elem, est_high, est_kinder,
   blank, grid_project, grid_plot)




# ---------------------------------------------------------------------------- #
## 3.3 W vs. L Breakdown ----
# ---------------------------------------------------------------------------- #

#'Continung with the data, I will repeat the last estimations, while applying the
#'diferenciation within [Winners] vs. [Losers] in the data.

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
    filter(grupo != "PIBpc")
  
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

# ------------------ #
### 3.3.1 Dosage -----

#Classroom
est_class <- feols( class ~ abs(dosage) : i(k, grupo, ref = -1) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Teacher room
est_troom <- feols( exp_troom ~ abs(dosage) : i(k, grupo, ref = -1) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Laboratory
est_labs <- feols(exp_lab ~ abs(dosage) : i(k, grupo, ref = -1)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Library
est_libra <- feols(exp_lib ~ abs(dosage) : i(k, grupo, ref = -1)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Play Area
est_play <- feols(exp_play ~ abs(dosage) : i(k, grupo, ref = -1)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Lunch
est_lunch <- feols(exp_lunch ~ abs(dosage) : i(k, grupo, ref = -1)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Employee 
est_employ <- feols(employee ~ abs(dosage) : i(k, grupo, ref = -1)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")


est_n_employ <- feols(n_employee ~ abs(dosage) : i(k, grupo, ref = -1)
                      + PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb,
                      vcov = "hetero")

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ, est_n_employ)


#### 3.3.1.1 Graph ----

# 3) Apply helper to each ggiplot object
p_class <- win_lose_plot(est_class, "Salas de aula")   #Classroom
p_troom <- win_lose_plot(est_troom, "Sala dos Prof.")  #Teacher's Room
p_labs  <- win_lose_plot(est_labs,  "Laboratórios")    #Laboratory
p_libra <- win_lose_plot(est_libra, "Biblioteca")      #Library
p_play  <- win_lose_plot(est_play,  "Quadras/Parque")  #Play Area
p_lunch <- win_lose_plot(est_lunch, "Merenda")         #Lunch
p_employ <- win_lose_plot(est_employ,"Funcionários")   #Employee
p_nemploy <- win_lose_plot(est_n_employ, "N° Funcionários") #N° Employee

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
  filename = paste0("winlose_school_structure_dosage.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/", #Saving directly to the report
  width = 1500/96, height = 620/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final)

# ----------------- #
### 3.3.2 Aluno Dosage ----
#Classroom
est_class <- feols( class ~ abs(aluno_dosage) : i(k, grupo, ref = -1) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Teacher room
est_troom <- feols( exp_troom ~ abs(aluno_dosage) : i(k, grupo, ref = -1) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")
#Laboratory
est_labs <- feols(exp_lab ~ abs(aluno_dosage) : i(k, grupo, ref = -1)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Library
est_libra <- feols(exp_lib ~ abs(aluno_dosage) : i(k, grupo, ref = -1)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Play Area
est_play <- feols(exp_play ~ abs(aluno_dosage) : i(k, grupo, ref = -1)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Lunch
est_lunch <- feols(exp_lunch ~ abs(aluno_dosage) : i(k, grupo, ref = -1)
                   + PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")

#Employee 
est_employ <- feols(employee ~ abs(aluno_dosage) : i(k, grupo, ref = -1)
                    + PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")

etable(est_class, est_troom, est_labs, est_libra, est_play, est_lunch, est_employ)


##### 3.3.2.1 Graph ----


# 3) Apply helper to each ggiplot object
p_class <- win_lose_plot(est_class, "Salas de aula")   #Classroom
p_troom <- win_lose_plot(est_troom, "Sala dos Prof.")  #Teacher's Room
p_labs  <- win_lose_plot(est_labs,  "Laboratórios")    #Laboratory
p_libra <- win_lose_plot(est_libra, "Biblioteca")      #Library
p_play  <- win_lose_plot(est_play,  "Quadras/Parque")  #Play Area
p_lunch <- win_lose_plot(est_lunch, "Merenda")         #Lunch
p_employ <- win_lose_plot(est_employ,"Funcionários")   #Employee

blank <- ggplot() + theme_void()

grid_plot <- ( p_troom + p_labs + p_libra) /
  (p_play + p_lunch + blank) /
  (p_class   + p_employ + blank)

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("winlose_school_structure_aluno_dosage.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/", #Saving directly to the report
  width = 1500/96, height = 620/96, dpi = 300
)

rm(p_class, p_troom, p_labs, p_play, p_lunch, p_employ, blank, grid_plot, p_libra,
   final, est_class, est_employ, est_labs, est_libra, est_lunch, est_play,
   est_troom)


# -------------------- #
### 3.3.3 Courses REG ----
# -------------------- #

#' ***************
#' DISCLAIMER: Here I employed the -1 period as reference. If I want to resume
#' the usage to 0, as previously, I should change all [-1] values for [0] in the
#' codelines that follows
#' ***************

# ------------------ #
#### 3.3.3.1 Dosage -----

#Kindergarden
est_kinder <- feols( exp_psch ~ abs(dosage) : i(k, grupo, ref = -1) +
                       PIBpc |
                       codmun + ano + uf^ano,
                     data = df_comb,
                     vcov = "hetero")
#Elementary
est_elem <- feols( exp_elem ~ abs(dosage) : i(k, grupo, ref = -1) +
                     PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")
#Highschool
est_high <- feols(exp_high ~ abs(dosage) : i(k, grupo, ref = -1)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

# #Inclusion
# est_inclu <- feols(inclusion ~ dosage * i(k, ref = -1)
#                    + PIBpc |
#                      codmun + ano + uf^ano,
#                    data = df_comb,
#                    vcov = "hetero")



##### 3.3.3.1.1 Graph ----
p_kinder <- win_lose_plot(est_kinder, "Fração Alunos E. Infantil")  #Preschool
p_elem <- win_lose_plot(est_elem, "Fração Alunos E. Fundamental")   #Elementary
p_high  <- win_lose_plot(est_high,  "Fração Alunos E. Médio")       #HighSchool
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
  filename = paste0("winlose_school_course_dosage.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/", #Saving directly to the report
  width = 800/96, height = 620/96, dpi = 300
)

rm(p_kinder, p_elem, p_high, final)

# ----------------- #
#### 3.3.3.2 Aluno Dosage ----

#Kindergarden
est_kinder <- feols( exp_psch ~ abs(aluno_dosage) : i(k, grupo, ref = -1) +
                       PIBpc |
                       codmun + ano + uf^ano,
                     data = df_comb,
                     vcov = "hetero")
#Elementary
est_elem <- feols( exp_elem ~ abs(aluno_dosage) : i(k, grupo, ref = -1) +
                     PIBpc |
                     codmun + ano + uf^ano,
                   data = df_comb,
                   vcov = "hetero")
#Highschool
est_high <- feols(exp_high ~ abs(aluno_dosage) : i(k, grupo, ref = -1)
                  + PIBpc |
                    codmun + ano + uf^ano,
                  data = df_comb,
                  vcov = "hetero")

#Inclusion
# est_inclu <- feols(inclusion ~ aluno_dosage * i(k, ref = -1)
#                    + PIBpc |
#                      codmun + ano + uf^ano,
#                    data = df_comb,
#                    vcov = "hetero")



##### 3.3.3.2.1 Graph ----
p_kinder <- win_lose_plot(est_kinder, "Fração Alunos E. Infantil")  #Preschool
p_elem <- win_lose_plot(est_elem, "Fração Alunos E. Fundamental")   #Elementary
p_high  <- win_lose_plot(est_high,  "Fração Alunos E. Médio")       #HighSchool
# p_inclu <- event_style_plot(est_inclu, "E. Especial")    #Speical Education

blank <- ggplot() + theme_void()

grid_plot <- (p_kinder + p_elem) /
  (p_high + blank) 

final <- grid_plot + plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("winlose_school_course_aluno_dosage.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/", #Saving directly to the report
  width = 800/96, height = 620/96, dpi = 300
)

rm(p_kinder, p_elem, p_high, p_inclu, final, est_elem, est_high, est_kinder,
   blank, grid_project, grid_plot)
rm(win_lose_plot)


# ---------------------------------------------------------------------------- #
# 4. N° Preschool and Daycare ----
# ---------------------------------------------------------------------------- #
#' Here I will estimate the impact of the municipal dosage exposure over the number
#' of schools who offer early-childhood education, such as daycare and/or preschool

# ------------------- #
## 4.1 Daycare ----
# ------------------- #

#' For the Number of Daycares per municipality, I will run three main regressions,
#' where I will vary the weight being applied in the regression.

### 4.1.1 Dosage -----

#Number of daycare facilities per municipality
est_day_nw <- feols(new_n_daycare ~ dosage * i(k, ref = -1) +
                   PIBpc |
                   codmun + ano + uf^ano,
                 data = df_comb,
                 vcov = "hetero")

# 
# est_day_wa <- feols(new_n_daycare ~ dosage * i(k, ref = -1) +
#                               PIBpc |
#                               codmun + ano + uf^ano,
#                             weights = df_comb$total_enroll,  #Total municipalility enrollment
#                             data = df_comb,
#                             vcov = "hetero")
# 
# 
# est_day_ws <- feols(new_n_daycare ~ dosage * i(k, ref = -1) +
#                       PIBpc |
#                       codmun + ano + uf^ano,
#                     weights = df_comb$schools,          #Total number of schools
#                     data = df_comb,
#                     vcov = "hetero")



etable(est_day_nw)

etable(est_day_nw, 
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/daycare_numbers_d.tex"), replace = TRUE)

# ------------------- #
### 4.1.2 Aluno Dosage ----

#Number of daycare facilities per municipality
est_day_nw_ad <- feols(new_n_daycare ~ aluno_dosage * i(k, ref = -1) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")




etable(est_day_nw_ad)

etable(est_day_nw_ad,
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage_aluno/daycare_numbers_ad.tex"), replace = TRUE)






# ----------------------------------------- #
## 4.2 Preschool ----
# ----------------------------------------- #

### 4.2.1 Dosage -----

#Number of daycare facilities per municipality
est_pre_nw <- feols(new_n_preschl ~ dosage * i(k, ref = -1) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")




etable(est_pre_nw )

etable(est_pre_nw, 
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/preschool_numbers_d.tex"), replace = TRUE)

# ------------------- #
### 4.1.2 Aluno Dosage ----

#Number of daycare facilities per municipality
est_pre_nw_ad <- feols(new_n_preschl ~ aluno_dosage * i(k, ref = -1) +
                         PIBpc |
                         codmun + ano + uf^ano,
                       data = df_comb,
                       vcov = "hetero")


etable(est_pre_nw_ad )

etable(est_pre_nw_ad,
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage_aluno/preschool_numbers_ad.tex"), replace = TRUE)


# ----------------------------------------- #
## 4.3 Enrollment ----
# ----------------------------------------- #
#' Now repeating for the enrollment levels within each class category.

# -------------------- #
### 4.3.1 Daycare -----
#### 4.3.1.1 Dosage -----

#Number of daycare facilities per municipality
est_dayen_nw <- feols(new_child_dayc ~ dosage * i(k, ref = -1) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")





etable(est_dayen_nw)

etable(est_dayen_nw, 
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/daycare_enroll_d.tex"), replace = TRUE)

# ------------------- #
#### 4.3.1.2 Aluno Dosage ----

#Number of daycare facilities per municipality
est_dayen_nw_ad <- feols(new_child_dayc ~ aluno_dosage * i(k, ref = -1) +
                         PIBpc |
                         codmun + ano + uf^ano,
                       data = df_comb,
                       vcov = "hetero")



etable(est_dayen_nw_ad)

etable(est_dayen_nw_ad,
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage_aluno/daycare_enroll_ad.tex"), replace = TRUE)






# ----------------------------------------- #
### 4.3.2 Preschool ----
# ----------------------------------------- #

#### 4.3.2.1 Dosage -----

#Number of daycare facilities per municipality
est_preen_nw <- feols(new_child_psch ~ dosage * i(k, ref = -1) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")



etable(est_preen_nw)

etable(est_preen_nw,
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/preschool_enroll_d.tex"), replace = TRUE)

# ------------------- #
#### 4.3.2.2 Aluno Dosage ----

#Number of daycare facilities per municipality
est_preen_nw_ad <- feols(new_child_psch ~ aluno_dosage * i(k, ref = -1) +
                         PIBpc |
                         codmun + ano + uf^ano,
                       data = df_comb,
                       vcov = "hetero")




etable(est_preen_nw_ad)

etable(est_preen_nw_ad,
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage_aluno/preschool_enroll_ad.tex"), replace = TRUE)

# ------------- #
###4.3.3 Graph ----
# ------------- #
#### 4.3.3.1 Dosage ----

#The first Step is to label all the graphs:
p_day_nw <- event_style_plot(est_day_nw, "Creche")  #Daycare
p_pre_nw <- event_style_plot(est_pre_nw, "Pré-Escola")     #Preschool

#Students
p_dayen_nw <- event_style_plot(est_dayen_nw, "Creche")  #Daycare
p_preen_nw <- event_style_plot(est_preen_nw, "Pré-Escola")     #Preschool




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
labels_col <- label_row("N° Escolas",     size_pt = 6) /   # top row
  #label_row("Peso Alunos", size_pt = 6) /   # middle row
  label_row("N° Matrículas",size_pt = 6) # bottom row

# put the 12 plots in the exact left-to-right, top-to-bottom order
plots <- list(
  p_day_nw,  p_pre_nw,  p_dayen_nw,  p_preen_nw
  )

# normalize per-plot margins so they don't force reflow
normalize_margin <- function(p) {
  p + theme(plot.margin = grid::unit(c(2,2,2,2), "pt"))
}
plots <- lapply(plots, normalize_margin)

# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 2) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Estimates from feols(...) with i(k, ref = -1)")

print(final)


ggsave(                                                #Saving image
  filename = paste0("dosage_pesos.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/",
  width = 1300/96, height = 620/96, dpi = 300
)


#Clearing values
rm( plots, grid_plot, final,
   est_day_nw, est_day_wa, est_day_ws, est_pre_nw, est_pre_wa, est_pre_ws,
   est_dayen_nw, est_dayen_wa, est_dayen_ws, est_preen_nw, est_preen_wa, est_preen_ws,
   p_day_nw, p_day_wa, p_day_ws, p_pre_nw, p_pre_wa, p_pre_ws,
   p_dayen_nw, p_dayen_wa, p_dayen_ws, p_preen_nw, p_preen_wa, p_preen_ws)


#### 4.3.3.2 Aluno Dosage ----

#Schools
p_day_nw <- event_style_plot(est_day_nw_ad, "Creche")  #Daycare
p_pre_nw <- event_style_plot(est_pre_nw_ad, "Pré-Escola")  #Preschool

#Students
p_dayen_nw <- event_style_plot(est_dayen_nw_ad, "Creche")  #Daycare
p_preen_nw <- event_style_plot(est_preen_nw_ad, "Pré-Escola")  #Preschool




plots <- list(
  p_day_nw,  p_pre_nw,  p_dayen_nw,  p_preen_nw
)



# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 2) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Estimates from feols(...) with i(k, ref = -1)")

print(final)


ggsave(                                                #Saving image
  filename = paste0("aluno_dosage_pesos.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/",
  width = 1300/96, height = 620/96, dpi = 300
)



rm(plots, grid_plot, final, event_style_plot,
   est_day_nw_ad, est_day_wa_ad, est_day_ws_ad, est_pre_nw_ad, est_pre_wa_ad, est_pre_ws_ad,
   est_dayen_nw_ad, est_dayen_wa_ad, est_dayen_ws_ad, est_preen_nw_ad, est_preen_wa_ad, est_preen_ws_ad,
   p_day_nw, p_day_wa, p_day_ws, p_pre_nw, p_pre_wa, p_pre_ws,
   p_dayen_nw, p_dayen_wa, p_dayen_ws, p_preen_nw, p_preen_wa, p_preen_ws)


# ---------------------------------------------------------------------------- #
# 5. Winner and Loser ----
# ---------------------------------------------------------------------------- #

#' *****
#' Continuing the regressions, I will repeate the last observed ones, but dividing
#' by municipalities groups of Winner or Loser.

##5.1 Daycare ----
### 5.1.1 Dosage -----

#Number of daycare facilities per municipality
est_day_nw <- feols(new_n_daycare ~ abs(dosage) : i(k, grupo, ref = -1) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")


etable(est_day_nw)

etable(est_day_nw, 
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/daycare_numbers_abv.tex"), replace = TRUE)

# ------------------- #
### 5.1.2 Aluno Dosage ----

#Number of daycare facilities per municipality
est_day_nw_ad <- feols(new_n_daycare ~ abs(aluno_dosage) : i(k, grupo, ref = -1) +
                         PIBpc |
                         codmun + ano + uf^ano,
                       data = df_comb,
                       vcov = "hetero")




etable(est_day_nw_ad)

etable(est_day_nw_ad, 
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage_aluno/daycare_numbers_abv.tex"), replace = TRUE)






# ----------------------------------------- #
## 5.2 Preschool ----
# ----------------------------------------- #

### 5.2.1 Dosage -----

#Number of daycare facilities per municipality
est_pre_nw <- feols(new_n_preschl ~ abs(dosage) : i(k, grupo, ref = -1) +
                      PIBpc |
                      codmun + ano + uf^ano,
                    data = df_comb,
                    vcov = "hetero")

etable(est_pre_nw)

etable(est_pre_nw,
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/preschool_numbers_abv.tex"), replace = TRUE)

# ------------------- #
### 5.2.2 Aluno Dosage ----

#Number of daycare facilities per municipality
est_pre_nw_ad <- feols(new_n_preschl ~ abs(aluno_dosage) : i(k, grupo, ref = -1) +
                         PIBpc |
                         codmun + ano + uf^ano,
                       data = df_comb,
                       vcov = "hetero")



etable(est_pre_nw_ad)

etable(est_pre_nw_ad,
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage_aluno/preschool_numbers_abv.tex"), replace = TRUE)


# ----------------------------------------- #
## 5.3 Enrollment ----
# ----------------------------------------- #
#' Now repeating for the enrollment levels within each class category.

# -------------------- #
### 5.3.1 Daycare -----
#### 5.3.1.1 Dosage -----

#Number of daycare facilities per municipality
est_dayen_nw <- feols(new_child_dayc ~ abs(dosage) : i(k, grupo, ref = -1) +
                        PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb,
                      vcov = "hetero")




etable(est_dayen_nw)

etable(est_dayen_nw, 
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/daycare_enroll_abv.tex"), replace = TRUE)

# ------------------- #
#### 5.3.1.2 Aluno Dosage ----

#Number of daycare facilities per municipality
est_dayen_nw_ad <- feols(new_child_dayc ~ abs(aluno_dosage) : i(k, grupo, ref = -1) +
                           PIBpc |
                           codmun + ano + uf^ano,
                         data = df_comb,
                         vcov = "hetero")



etable(est_dayen_nw_ad)

etable(est_dayen_nw_ad,
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage_aluno/daycare_enroll_abv.tex"), replace = TRUE)






# ----------------------------------------- #
### 5.3.2 Preschool ----
# ----------------------------------------- #

#### 5.3.2.1 Dosage -----

#Number of daycare facilities per municipality
est_preen_nw <- feols(new_child_psch ~ abs(dosage) : i(k, grupo, ref = -1) +
                        PIBpc |
                        codmun + ano + uf^ano,
                      data = df_comb,
                      vcov = "hetero")


etable(est_preen_nw)

etable(est_preen_nw,
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/preschool_enroll_abv.tex"), replace = TRUE)

# ------------------- #
#### 5.3.2.2 Aluno Dosage ----

#Number of daycare facilities per municipality
est_preen_nw_ad <- feols(new_child_psch ~ abs(aluno_dosage) : i(k, grupo, ref = -1) +
                           PIBpc |
                           codmun + ano + uf^ano,
                         data = df_comb,
                         vcov = "hetero")





etable(est_preen_nw_ad)

etable(est_preen_nw_ad, 
       vcov = "hetero",
       headers = list(":_:" = list("(1)" = 1)),
       file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage_aluno/preschool_enroll_abv.tex"), replace = TRUE)

# ------------- #
###5.3.3 Graph ----
# ------------- #

event_style_abv <- function(est_obj, title = NULL, ylim = NULL) {
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
    filter(grupo != "PIBpc")
  
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


#### 5.3.3.1 Dosage ----

#The first Step is to label all the graphs:
#Schools
p_day_nw <- event_style_abv(est_day_nw, "Creche")      #Daycare
p_pre_nw <- event_style_abv(est_pre_nw, "Pré-Escola")  #Preschool

#Students
p_dayen_nw <- event_style_abv(est_dayen_nw, "Creche")      #Daycare
p_preen_nw <- event_style_abv(est_preen_nw, "Pré-Escola")  #Preschool





# put the 12 plots in the exact left-to-right, top-to-bottom order
plots <- list(
  p_day_nw,  p_pre_nw,  p_dayen_nw,  p_preen_nw
)


plots <- lapply(plots, normalize_margin)

# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 2) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Estimates from feols(...) with i(k, ref = -1)")

print(final)


ggsave(                                                #Saving image
  filename = paste0("dosage_pesos_abv.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/",
  width = 1300/96, height = 620/96, dpi = 300
)


#Clearing values
rm( plots, grid_plot, final,
    est_day_nw, est_day_wa, est_day_ws, est_pre_nw, est_pre_wa, est_pre_ws,
    est_dayen_nw, est_dayen_wa, est_dayen_ws, est_preen_nw, est_preen_wa, est_preen_ws,
    p_day_nw, p_day_wa, p_day_ws, p_pre_nw, p_pre_wa, p_pre_ws,
    p_dayen_nw, p_dayen_wa, p_dayen_ws, p_preen_nw, p_preen_wa, p_preen_ws)


#### 5.3.3.2 Aluno Dosage ----

#Schools
p_day_nw <- event_style_abv(est_day_nw_ad, "Creche")      #Daycare
p_pre_nw <- event_style_abv(est_pre_nw_ad, "Pré-Escola")  #Preschool


#Students
p_dayen_nw <- event_style_abv(est_dayen_nw_ad, "Creche")      #Daycare
p_preen_nw <- event_style_abv(est_preen_nw_ad, "Pré-Escola")  #Preschool




plots <- list(
  p_day_nw,  p_pre_nw,  p_dayen_nw,  p_preen_nw
)



# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 2) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Estimates from feols(...) with i(k, ref = -1)")

print(final)


ggsave(                                                #Saving image
  filename = paste0("aluno_dosage_pesos_abv.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/",
  width = 1300/96, height = 620/96, dpi = 300
)


rm(list = ls())
gc()
# ---------------------------------------------------------------------------- #
# <<< -------------------- >>> ----
# SAEB -----
# ---------------------------------------------------------------------------- #

#'********************************************************************************
#' [CONTINUATION]
#' The remaining of the code will be based upon the data observed for the SAEB, 
#' recreating the regressions from other works, while observing in the individual
#' level the effects of the policy change.
#'********************************************************************************


# ---------------------------------------------------------------------------- #
# 6. Data ----
# ---------------------------------------------------------------------------- #

# ------------------- #
## 6.1 Extraction ----
#Saeb dataframe
df_saeb <- readRDS( "Z:/Tuffy/Paper - Educ/Dados/saeb_nvl_aluno.rds") %>%
  mutate(codmun = ifelse(ano >= 2007, as.numeric(codmun) %/% 10, codmun),
         codmun = as.character(codmun)) %>%   #Arranging for the older municipal codes
  filter(codmun < 600000)



#Main regression dataframe
df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") %>% 
  select(codigo_ibge, ano, uf, nome, dif_coef_pp, dosage, aluno_dosage, PIBpc) %>% 
  mutate(across(c(codigo_ibge, uf), as.character))

# ------------------ #
## 6.2 Merge ----

#Combining both dataframes
df <- df_saeb %>% 
  left_join(df_reg %>%
              filter(ano %in% c(2007:2017)) %>% #Excluding 2005
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
    dosage > 0 ~ "Winner",   # net beneficiary
    dosage < 0 ~ "Loser",   # net contributer
    TRUE ~ NA_character_),
    
    grupo = factor(grupo, levels = c("Loser", "Winner")) #Beneficiary dumm
    ) %>% 
  #filter(as.numeric(ano_nasc) < 2008) %>% # This remove younger than expected people in 2017  -> ideal age = 10.
  mutate(ano_nasc = as.factor(ano_nasc))

#Weights dataframes
df_w5 <- df %>% filter(grade == 5) %>% select(peso, peso_mt, peso_lp)  #5th grade
df_w9 <- df %>% filter(grade == 9) %>% select(peso, peso_mt, peso_lp)  #9th grade


# ------------------------- #
#' Additional tweaks for better reproduction of Roberto's regression

#1. Extracting the municipalities in 2007
df_uf <- df_reg %>% 
  filter(ano == 2007)

#2. Cutoff variable in the top tercile
#' In the paper the authors specify the top tercile for each state.

df_uf <- df_uf %>%
  group_by(uf) %>%
  mutate(
    cutoff_dos = quantile(dosage, probs = 0.7, na.rm = T),
    cutoff_ald = quantile(aluno_dosage, probs = 0.7, na.rm = T),
    top_tercile_dos = ifelse(!is.na(dosage) & dosage >= cutoff_dos, 1, 0),
    top_tercile_ald = ifelse(!is.na(aluno_dosage) & aluno_dosage >= cutoff_ald, 1, 0)
  ) %>%
  ungroup() %>% 
  select(codigo_ibge, top_tercile_dos, top_tercile_ald)

#3. combining the last variables
df <- df %>% 
  left_join(df_uf, by = c("codmun" = "codigo_ibge"))


rm(df_uf)

# ---------------------------------------------------------------------------- #
# 7. Main Regression ----
# ---------------------------------------------------------------------------- #
## 7.1 Dosage ----
# ------------------ #




### 7.1.1 Years exp ----

main_mat <- feols(as.numeric(profic_mat) ~ abs(dosage) : i(ano_nasc, grupo, ref = 2002)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5) , #Only 5h grade
                  weights = df_w5$peso_mt,
                  vcov = "hetero")

main_pot <- feols(as.numeric(profic_port) ~ abs(dosage) : i(ano_nasc, grupo, ref = 2002)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  weights = df_w5$peso_lp,
                  vcov = "hetero")


# sec_mat <- feols(as.numeric(profic_mat) ~ dosage : i(ano_nasc, ref = 0)
#                  + sexo + raca + mae_educ + idade + PIBpc 
#                  | codmun + ano + uf^ano,
#                  data = df %>% filter(grade == 9),
#                  weights = df_w9$peso,
#                  vcov = "hetero")
# 
# 
# 
# sec_pot <- feols(as.numeric(profic_port) ~ dosage : i(ano_nasc, ref = 0)
#                  + sexo + raca + mae_educ + idade + PIBpc 
#                  | codmun + ano + uf^ano,
#                  data = df %>% filter(grade == 9),
#                  weights = df_w9$peso,
#                 vcov = "hetero")

etable(main_mat, main_pot
       #,sec_mat, sec_pot
       )



etable(main_mat, main_pot,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/main_nvl_individuo_idade_exp.tex", replace = TRUE)



# -------------- #
#### 7.1.1.1 Graph ----
# -------------- #

new_plot_func <- function(est_obj, title = NULL, ylim = NULL) {
  
  # extract tidy data from fixest::etable or broom::tidy
  event_df <- broom::tidy(est_obj, conf.int = TRUE) %>%
    mutate(
      k = str_extract(term, "(?<=ano_nasc::)-?\\d+"),
      k = as.numeric(k),
      # Extract group (text after the last colon)
      grupo = str_extract(term, "[^:]+$"),
      grupo = as.factor(grupo))


event_df <- event_df %>% 
  bind_rows(
    event_df %>%
      distinct(grupo) %>%
      mutate(
        term = paste0(as.character(grupo),"ano_nasc:2002"),     
        estimate = 0,
        std.error = 0,
        statistic = 0,
        p.value = 1,
        conf.low = 0,
        conf.high = 0,
        k = 2002
      )
  ) %>% 
  filter(!grupo %in% c("PIBpc", "mae_educ", "raca", "sexo", "idade"))

ggplot(event_df, aes(x = k, y = estimate, group = grupo)) +
  # shaded standard error area
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 2002, color = "black") +
  geom_point(aes(color = grupo), shape = 15, size = 2) +
  geom_line(aes(color = grupo)) +
  labs(
    title = title,
    x = "Ano de nascimento",
    y = "Nota Saeb"
  ) +
  coord_cartesian(ylim = ylim) +
  theme_classic() +
  theme(
    axis.line = element_line(color = "grey70"),
    panel.grid = element_blank(),
    axis.title = element_text(size = 11)
  ) + 
  scale_x_continuous(breaks = seq(1992, 2007, 2))

}


#Applying the new function to plot...
p_mat <- new_plot_func(main_mat, "Matemática")      #Daycare
p_pot <- new_plot_func(main_pot, "Português")



rm(main_mat, main_pot, sec_mat, sec_pot)


# ------------------------------ #
### 7.1.2 Roberto Spec -----
# ------------------------------ #





main_mat <- feols(as.numeric(profic_mat) ~ rob_winner_dummy : i(ano_nasc, ref = 2002)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5) , #Only 5h grade
                  weights = df_w5$peso_mt,
                  vcov = "hetero")

main_pot <- feols(as.numeric(profic_port) ~ rob_winner_dummy : i(ano_nasc, ref = 2002)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  weights = df_w5$peso_lp,
                  vcov = "hetero")


etable(main_mat, main_pot
       #,sec_mat, sec_pot
)



etable(main_mat, main_pot,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/roberto_nvl_individuo_idade_exp.tex", replace = TRUE)


rm(main_mat, main_pot, sec_mat, sec_pot)


# ------------------------------- #
#### 7.1.2.1 Roberto Simples ----
# ------------------------------- #

####### Data ------------------------ #
#Combining both dataframes
df_simples <- df_saeb %>% 
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
    dosage > 0 ~ "Winner",   # net beneficiary
    dosage < 0 ~ "Loser",   # net contributer
    TRUE ~ NA_character_),
    
    grupo = factor(grupo, levels = c("Loser", "Winner")) #Beneficiary dumm
  ) %>% 
  #filter(as.numeric(ano_nasc) < 2008) %>% # This remove younger than expected people in 2017  -> ideal age = 10.
  mutate(ano_nasc = as.factor(ano_nasc))

#Weights dataframes
df2_w5 <- df_simples %>% filter(grade == 5) %>% select(peso, peso_mt, peso_lp)  #5th grade
df2_w9 <- df_simples %>% filter(grade == 9) %>% select(peso, peso_mt, peso_lp)  #9th grade




# Regressions --
main_mat <- feols(as.numeric(profic_mat) ~ rob_winner_dummy * nasc_post
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df_simples %>% filter(grade == 5) , #Only 5h grade
                  weights = df2_w5$peso_mt,
                  vcov = "hetero")

main_pot <- feols(as.numeric(profic_port) ~ rob_winner_dummy * nasc_post
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df_simples %>% filter(grade == 5), #Only 5h grade
                  weights = df2_w5$peso_lp,
                  vcov = "hetero")


etable(main_mat, main_pot
       #,sec_mat, sec_pot
)



etable(main_mat, main_pot,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/roberto_v2_nvl_ind_nasc.tex", replace = TRUE)


rm(main_mat, main_pot, sec_mat, sec_pot, df_simples, df2_w5, df2_w9)



# -------------------------------- #
## 7.2 Aluno Dosage ----
# -------------------------------- #

# ----------------- #
### 7.2.1 *Years exp ----

main_mat <- feols(as.numeric(profic_mat) ~ abs(aluno_dosage) : i(ano_nasc, grupo, ref = 2002)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5) , #Only 5h grade
                  weights = df_w5$peso_mt,
                  vcov = "hetero")

main_pot <- feols(as.numeric(profic_port) ~ abs(aluno_dosage) : i(ano_nasc, grupo, ref = 2002)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  weights = df_w5$peso_lp,
                  vcov = "hetero")

# 
# sec_mat <- feols(as.numeric(profic_mat) ~ aluno_dosage : i(anos_exp, ref = 0)
#                  + sexo + raca + mae_educ + idade + PIBpc 
#                  | codmun + ano + uf^ano,
#                  data = df %>% filter(grade == 9),
#                  #weights = df$peso,
#                  vcov = "hetero")
# 
# 
# 
# sec_pot <- feols(as.numeric(profic_port) ~ aluno_dosage : i(anos_exp, ref = 0)
#                  + sexo + raca + mae_educ + idade + PIBpc 
#                  | codmun + ano + uf^ano,
#                  data = df %>% filter(grade == 9),
#                  #weights = df$peso,
#                  vcov = "hetero")

etable(main_mat, main_pot
       #,sec_mat, sec_pot
       )



etable(main_mat, main_pot,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/main_al_nvl_individuo_idade_exp.tex", replace = TRUE)



#### 7.2.1.1 Graph ----

#Applying the new function to plot...
p_mat2 <- new_plot_func(main_mat, "Matemática")      #Daycare
p_pot2 <- new_plot_func(main_pot, "Português")


rm(main_mat, main_pot, sec_mat, sec_pot)

# -------------------- #
#### 7.2.1.2 Final graphs -----
# -------------------- #


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



plots <- list(
  p_mat,  p_pot,  p_mat2,  p_pot2
)


# Create the right column with vertical labels (one per row)
labels_col <- label_row("Dosage", size_pt = 6) /   # top row
  label_row("Aluno Dosage", size_pt = 6) # bottom row

# Force a 4 columns x 3 rows layout
grid_plot <- patchwork::wrap_plots(plots, ncol = 2, nrow = 2) +
  plot_layout(guides = "collect", widths = rep(1, 2), heights = rep(1,2))

# Now combine with the label column (already defined earlier as labels_col)
final <- (labels_col | grid_plot ) +
  plot_layout(widths = c(0.6, 10), heights = c(1,1,1)) +
  plot_annotation(caption = "Estimates from feols(...) with i(k, ref = -1)")

print(final)


ggsave(                                                #Saving image
  filename = paste0("ano_nascimento_alunos.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/",
  width = 1300/96, height = 620/96, dpi = 300
)



rm(final, p_mat, p_mat2, p_pot, p_pot2, labels_cols, grid_plot)
# ---------------------------------------------------------------------------- #
## 7.3.2 Region ----

region_list <- unique(df$region)


for (area in region_list) {
  
  ini <- Sys.time()
  
  message("Starting for region: ", area)
  
  
  temp <- df %>% 
    filter(region == area,
           grade == 5)
  
  temp_w <- temp %>% # Weights dataframe
    select(peso, peso_mt, peso_lp)
  
  #5h grade
  mat_5 <- feols(as.numeric(profic_mat) ~ aluno_dosage : i(idade_exp, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc #Controls
                 | codmun + ano + uf^ano, #FE
                 data = temp %>% filter(grade == 5), #Only 5h grade
                 weights = temp_w$peso_mt,
                 vcov = "hetero")
  
  por_5 <- feols(as.numeric(profic_port) ~ aluno_dosage : i(idade_exp, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc #Controls
                 | codmun + ano + uf^ano, #FE
                 data = temp %>% filter(grade == 5), #Only 5h grade
                 weights = temp_w$peso_lp,
                 vcov = "hetero")
  
  
  message("Finished for: ", area)
  
  etable(mat_5, por_5)
  
  
  
  etable(mat_5, por_5,
         vcov = "hetero",
         headers = list(":_:" = list("Matemática" = 1,"Português" = 1)),
         file = paste0("Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage_aluno/region/region_",
                       tolower(area),"_individuo_anos_exp.tex"), replace = TRUE)
  
  
  message("Saved final table (", area,")")
  fim <- Sys.time()
  
  
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  
  message("---------------------------------------------")
  message("Total time elapsed: ",mins," mins e ", secs, " s")
  message("---------------------------------------------")
  
  rm(temp, delta, ini, fim, mins, secs, mat_5, por_5)
  
}



# ---------------------------------------------------------------------------- #
# 8. Age vs. Grade Distortion ----
# ---------------------------------------------------------------------------- #

#' Here I will investigate the effect of the FUNDEB policy in the grade-age dis-
#' tortion dummy variable. Where the ideal age-grade equals 0, while the distor-
#' tion is equal 1

# ---------------- #
## 8.1 Dosage ----

# main_dist <- feols(as.numeric(age_late) ~ dosage : i(anos_exp, ref = 0)
#                    + sexo + raca + mae_educ + idade + PIBpc #Controls
#                    | codmun + ano + uf^ano, #FE
#                    data = df %>% filter(grade == 5), #Only 5h grade
#                    #weights = df %>% filter(grade == 5) %>% select(peso),
#                    vcov = "hetero")
# 
# 
# sec_dist <- feols(as.numeric(age_late) ~ dosage : i(anos_exp, ref = 0)
#                   + sexo + raca + mae_educ + idade + PIBpc 
#                   | codmun + ano + uf^ano,
#                   data = df %>% filter(grade == 9),
#                   #weights = df$peso,
#                   vcov = "hetero")
# 
# etable(main_dist, sec_dist,
#        vcov = "hetero",
#        headers = list(":_:" = list("5°Ano" = 1,"9°Ano" = 1)),
#        file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/dist_individuo_anos_exp.tex", replace = TRUE)
# 

# ---------------- #
## 8.2 Aluno Dosage ----

main_dist <- feols(as.numeric(age_late) ~ aluno_dosage : i(anos_exp, ref = 0)
                   + sexo + raca + mae_educ + idade + PIBpc #Controls
                   | codmun + ano + uf^ano, #FE
                   data = df %>% filter(grade == 5), #Only 5h grade
                   #weights = df %>% filter(grade == 5) %>% select(peso),
                   vcov = "hetero")


sec_dist <- feols(as.numeric(age_late) ~ aluno_dosage : i(anos_exp, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc 
                  | codmun + ano + uf^ano,
                  data = df %>% filter(grade == 9),
                  #weights = df$peso,
                  vcov = "hetero")


etable(main_dist, sec_dist)
etable(main_dist, sec_dist,
       vcov = "hetero",
       headers = list(":_:" = list("5°Ano" = 1,"9°Ano" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage_aluno/dist_individuo_anos_exp.tex", replace = TRUE)



rm(main_dist, sec_dist)



# ---------------------------------------------------------------------------- #
# 9. Winner vs. Loser ----
# ---------------------------------------------------------------------------- #
#' Here I will create an auxiliar dummy indicating if the municipality is placed
#' into the group of beneficiary or contributer


## 9.1 Ref. Specification ----
#' This specification is more similar to the Rudi Rocha reference paper, regar-
#' ding the health spending policy change in Brazil. In a similar way from the
#' authors, I will divide the group into Winner and Loser, meaning the ones that
#' received more from the policy change and the ones that presented losses.

# ------------------------------------ #
## 9.2 Age Distort. ----
# ------------------------------------ #
### 9.2.1 Dosage ----
#### 9.2.1.1 Regression ----

main_dist <- feols(as.numeric(age_late) ~ abs(dosage) : i(anos_exp, grupo, ref = 0)
                   + sexo + raca + mae_educ + idade + PIBpc #Controls
                   | codmun + ano + uf^ano, #FE
                   data = df %>% filter(grade == 5), #Only 5h grade
                   #weights = df %>% filter(grade == 5) %>% select(peso),
                   vcov = "hetero")


sec_dist <- feols(as.numeric(age_late) ~ abs(dosage) : i(anos_exp, grupo, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc 
                  | codmun + ano + uf^ano,
                  data = df %>% filter(grade == 9),
                  #weights = df$peso,
                  vcov = "hetero")

etable(main_dist, sec_dist)

etable(main_dist, sec_dist,
       vcov = "hetero",
       headers = list(":_:" = list("5° Ano" = 1,"9° Ano" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/dist_age_abbe_dosage.tex", replace = TRUE)



#### 9.2.1.2 Graph ----


model_list <- list(
  ano5  = main_dist,
  ano9 = sec_dist
)

for (model_name in names(model_list)) {
  
  
  current_model <- model_list[[model_name]]
  
  temp <- broom::tidy(current_model, conf.int = TRUE) %>%
    filter(str_detect(term, "anos_exp")) %>%
    mutate(
      # Extract time (number right after "anos_exp:")
      time_exposure = str_extract(term, "(?<=anos_exp:)-?\\d+"),
      time_exposure = as.numeric(time_exposure),
      
      # Extract group (text after the last colon)
      grupo = str_extract(term, "[^:]+$"),
      grupo = as.factor(grupo)
    ) %>%
    mutate(
      time_exposure = sapply(
        str_extract_all(term, "-?\\d*\\.?\\d+"),
        function(x) if (length(x) > 0) as.numeric(tail(x, 1)) else NA_real_
      )
    ) %>%
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high, grupo, time_exposure)
  
  # If extraction gave NAs for an entire grupo, create an internal sequence (fallback).
  # This produces a centered seq: e.g. for 5 rows -> -2, -1, 0, 1, 2
  temp <- temp %>%
    group_by(grupo) %>%
    mutate(
      # if all NA in this group, make a centered sequence; else keep extracted values
      time_exposure = if (all(is.na(time_exposure))) {
        n <- n()
        seq(-floor((n-1)/2), ceiling((n-1)/2), length.out = n) %>% as.integer()
      } else {
        time_exposure
      }
    ) %>%
    ungroup() %>%
    arrange(grupo, time_exposure)
  
  # if you want an explicit 0 baseline row per grupo (if not already present),
  # create a baseline row and bind it in. This ensures there is always a point at 0.
  baseline_rows <- temp %>%
    distinct(grupo) %>%
    mutate(
      term = paste0(as.character(grupo), ":anos_exp:0"),
      estimate = 0,
      std.error = 0,
      statistic = NA_real_,
      p.value = 1,
      conf.low = 0,
      conf.high = 0,
      time_exposure = 0
    )
  
  # combine and order
  temp <- bind_rows(temp, baseline_rows) %>%
    distinct(term, grupo, time_exposure, .keep_all = TRUE) %>%  # avoid duplicate identical rows
    arrange(grupo, time_exposure)
  
  
  p <- ggplot(temp, aes(x = time_exposure, y = estimate, group = grupo)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = grupo), shape = 15, size = 2) +
    geom_line(aes(color = grupo)) +
    #facet_wrap(~ grupo, ncol = 1) +     # <--- separate plot per group
    labs(x = "Years of Exposure", y = "Nota",
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
      breaks = seq(min(temp$time_exposure, na.rm = TRUE),
                   max(temp$time_exposure, na.rm = TRUE),
                   by = 2)
    )
  
  
  p <- p +
    theme(
      legend.background = element_blank(),
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11),
      legend.position = c(0.14, 0.17),
      legend.justification = c("right", "top") # anchor legend box
    ) + # anchor legend box
    scale_x_continuous(
      breaks = seq(min(temp$time_exposure, na.rm = TRUE),
                   max(temp$time_exposure, na.rm = TRUE),
                   by = 2))
  
  
  p
  
  
  ggsave(
    filename = paste0("grafico_", model_name, "_atraso_abbe.png"),
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/",
    width = 600/96, height = 420/96, dpi = 110
  )
  rm(baseline_rows, temp, model_name, current_model, p)
}





# ------------------------- #
### 9.2.2 Aluno Dosage ----
# ------------------------- #


#### 9.2.2.1 Regression ----

main_dist <- feols(as.numeric(age_late) ~ dosage : i(anos_exp, grupo, ref = 0)
                   + sexo + raca + mae_educ + idade + PIBpc #Controls
                   | codmun + ano + uf^ano, #FE
                   data = df %>% filter(grade == 5), #Only 5h grade
                   #weights = df %>% filter(grade == 5) %>% select(peso),
                   vcov = "hetero")


sec_dist <- feols(as.numeric(age_late) ~ dosage : i(anos_exp, grupo, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc 
                  | codmun + ano + uf^ano,
                  data = df %>% filter(grade == 9),
                  #weights = df$peso,
                  vcov = "hetero")

etable(main_dist, sec_dist)

etable(main_dist, sec_dist,
       vcov = "hetero",
       headers = list(":_:" = list("5° Ano" = 1,"9° Ano" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage_aluno/dist_age_abbe_aluno_dosage.tex", replace = TRUE)



#### 9.2.2.2 Graph ----


model_list <- list(
  ano5  = main_dist,
  ano9 = sec_dist
)

for (model_name in names(model_list)) {
  
  
  current_model <- model_list[[model_name]]
  
  temp <- broom::tidy(current_model, conf.int = TRUE) %>%
    filter(str_detect(term, "anos_exp")) %>%
    mutate(
      # Extract time (number right after "anos_exp:")
      time_exposure = str_extract(term, "(?<=anos_exp:)-?\\d+"),
      time_exposure = as.numeric(time_exposure),
      
      # Extract group (text after the last colon)
      grupo = str_extract(term, "[^:]+$"),
      grupo = as.factor(grupo)
    ) %>%
    mutate(
      time_exposure = sapply(
        str_extract_all(term, "-?\\d*\\.?\\d+"),
        function(x) if (length(x) > 0) as.numeric(tail(x, 1)) else NA_real_
      )
    ) %>%
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high, grupo, time_exposure)
  
  # If extraction gave NAs for an entire grupo, create an internal sequence (fallback).
  # This produces a centered seq: e.g. for 5 rows -> -2, -1, 0, 1, 2
  temp <- temp %>%
    group_by(grupo) %>%
    mutate(
      # if all NA in this group, make a centered sequence; else keep extracted values
      time_exposure = if (all(is.na(time_exposure))) {
        n <- n()
        seq(-floor((n-1)/2), ceiling((n-1)/2), length.out = n) %>% as.integer()
      } else {
        time_exposure
      }
    ) %>%
    ungroup() %>%
    arrange(grupo, time_exposure)
  
  # if you want an explicit 0 baseline row per grupo (if not already present),
  # create a baseline row and bind it in. This ensures there is always a point at 0.
  baseline_rows <- temp %>%
    distinct(grupo) %>%
    mutate(
      term = paste0(as.character(grupo), ":anos_exp:0"),
      estimate = 0,
      std.error = 0,
      statistic = NA_real_,
      p.value = 1,
      conf.low = 0,
      conf.high = 0,
      time_exposure = 0
    )
  
  # combine and order
  temp <- bind_rows(temp, baseline_rows) %>%
    distinct(term, grupo, time_exposure, .keep_all = TRUE) %>%  # avoid duplicate identical rows
    arrange(grupo, time_exposure)
  
  
  p <- ggplot(temp, aes(x = time_exposure, y = estimate, group = grupo)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = grupo), shape = 15, size = 2) +
    geom_line(aes(color = grupo)) +
    #facet_wrap(~ grupo, ncol = 1) +     # <--- separate plot per group
    labs(x = "Years of Exposure", y = "Nota",
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
      breaks = seq(min(temp$time_exposure, na.rm = TRUE),
                   max(temp$time_exposure, na.rm = TRUE),
                   by = 2)
    )
  
  
  p <- p +
    theme(
      legend.background = element_blank(),
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11),
      legend.position = c(0.14, 0.17),
      legend.justification = c("right", "top") # anchor legend box
    ) + # anchor legend box
    scale_x_continuous(
      breaks = seq(min(temp$time_exposure, na.rm = TRUE),
                   max(temp$time_exposure, na.rm = TRUE),
                   by = 2))
  
  
  p
  
  
  ggsave(
    filename = paste0("grafico_", model_name, "_atraso_abbe_aluno_dosage.png"),
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/v2/Figuras/ES/Robust/",
    width = 600/96, height = 420/96, dpi = 110
  )
  rm(baseline_rows, temp, model_name, current_model, p)
}





# ------------------------------------ #
## 9.3 Grade -----
# ------------------------------------ #


### 9.3.1 Dosage ----

main_mat <- feols(as.numeric(profic_mat) ~ abs(dosage) : i(ano_nasc, grupo, ref = -1)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5) , #Only 5h grade
                  weights = df_w5$peso_mt,
                  vcov = "hetero")

main_pot <- feols(as.numeric(profic_port) ~ abs(dosage) : i(ano_nasc, grupo, ref = -1)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  weights = df_w5$peso_lp,
                  vcov = "hetero")


etable(main_mat, main_pot
       #,sec_mat, sec_pot
)



etable(main_mat, main_pot,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Dosage/wl_nvl_individuo_idade_exp.tex", replace = TRUE)


rm(main_mat, main_pot, sec_mat, sec_pot)





# -------------------------------- #
## 9.2 Aluno Dosage ----
# -------------------------------- #

# ----------------- #
### 9.2.1 *Years exp ----

main_mat <- feols(as.numeric(profic_mat) ~ abs(aluno_dosage) : i(ano_nasc, ref = -1)
                  + sexo + raca + mae_educ + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  weights = df_w5$peso_mt,
                  vcov = "hetero")

main_pot <- feols(as.numeric(profic_port) ~ abs(aluno_dosage) : i(ano_nasc, ref = -1)
                  + sexo + raca + mae_educ + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  weights = df_w5$peso_lp,
                  vcov = "hetero")

# 
# sec_mat <- feols(as.numeric(profic_mat) ~ aluno_dosage : i(anos_exp, ref = 0)
#                  + sexo + raca + mae_educ + idade + PIBpc 
#                  | codmun + ano + uf^ano,
#                  data = df %>% filter(grade == 9),
#                  #weights = df$peso,
#                  vcov = "hetero")
# 
# 
# 
# sec_pot <- feols(as.numeric(profic_port) ~ aluno_dosage : i(anos_exp, ref = 0)
#                  + sexo + raca + mae_educ + idade + PIBpc 
#                  | codmun + ano + uf^ano,
#                  data = df %>% filter(grade == 9),
#                  #weights = df$peso,
#                  vcov = "hetero")

etable(main_mat, main_pot
       #,sec_mat, sec_pot
)



etable(main_mat, main_pot,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/wl_nvl_individuo_idade_exp.tex", replace = TRUE)


rm(main_mat, main_pot, sec_mat, sec_pot)








# ---------------------------------------------------------------------------- #
# 10. SAEB breakdown ----
# ---------------------------------------------------------------------------- #


# ---------------------- #
## 10.1 Data ----
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
### 10.1.1 Groups Data ----


df_mun <- df %>% 
  mutate(uf = as.numeric(codmun) %/% 10000) %>% 
  group_by(codmun, ano, id_uf) %>% 
  summarise(students = n(),
            students_peso = sum(peso, na.rm = T),
            students_mpes = sum(peso_mt, na.rm = T),
            students_ppes = sum(peso_lp, na.rm = T),
            .groups = "drop") %>% 
  arrange(codmun, ano, id_uf) %>% 
  mutate_all(as.numeric)

#Creating a dataframe for each school
df_mun_school <- df %>% 
  mutate(uf = as.numeric(codmun) %/% 10000) %>% 
  #Grouping by School
  group_by(codmun, cod_escola, ano,  id_uf) %>% 
  summarise(students = n(),
            students_peso = sum(peso, na.rm = T),
            students_mpes = sum(peso_mt, na.rm = T),
            students_ppes = sum(peso_lp, na.rm= T),
            .groups = "drop") %>% 
  arrange(codmun, ano, id_uf) %>% 
  mutate_all(as.numeric) %>% 
  #Grouping by Municipality
  group_by(codmun, ano, id_uf) %>% 
  summarise(total_students = sum(students),
            total_students_peso = sum(students_peso),
            total_students_mpes = sum(students_mpes), #using the math weight as reference
            total_students_ppes = sum(students_ppes),
            schools = n(),
            .groups = "drop") %>% 
  arrange(codmun, ano, id_uf)
  

#all.equal(df_mun_school$total_students_mpes, df_mun_school$total_students_ppes)
# #[1] "Mean relative difference: 0.001738527"


#### 10.1.1.1 Mun per year ----
#Municipalities by year
mun_tab <- df_mun %>% 
  group_by(ano) %>% 
  summarise(
    n_mun = n_distinct(codmun), #captures different municipalities
    total_students = sum(students_mpes, na.rm = T)
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
writeLines(latex_table, "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Desc/mun_ano.tex")


#### 10.1.1.2 UF - year -----

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
writeLines(latex_table, "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Desc/mun_uf_ano.tex")


#df_mun %>% tab(id_uf)

#### 10.1.1.3 Students - year ----
uf_stu <- df_mun %>%
  group_by(id_uf, ano) %>%
  summarise(total_students = sum(students_mpes, na.rm = TRUE),
            .groups = "drop") %>%
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
  select(uf, everything(), -id_uf) %>% 
  mutate(across(where(is.numeric), ~ round(., 0)))

#Saving Table as LaTeX
latex_table <- knitr::kable(
  uf_stu,
  format = "latex",
  booktabs = TRUE,
  align = "lcccccc",
  linesep = ""
)
writeLines(latex_table, "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Desc/aluno_uf_ano.tex")
rm( latex_table, uf_stu, uf_tab)

# ----------------------- #
## 10.2 School Student ----
# ----------------------- #

#'Here I will invetigate if there was a significant difference of students within
#'a school that is present through all the observations years.



df_filter <- df_mun_school %>% 
  group_by(codmun) %>% 
  mutate(aux1 = 1,
         aux2 = sum(aux1, na.rm = T),
         time_to_treat = ano - 2007
  ) %>% 
  filter(aux2 == 7) %>% #Selecting only the mun present in all years
  ungroup() %>% 
  select(-aux1, -aux2) 



df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") %>% 
  select(codigo_ibge, ano, uf, nome, dif_coef_pp, dosage, aluno_dosage, PIBpc,
         des_edu_pc, des_fund_pc, des_med_pc, des_inf_pc) %>% 
  mutate(across(c(codigo_ibge, uf), as.numeric))


df_filter <- df_filter %>% 
  left_join(df_reg %>% select(codigo_ibge, ano, dosage, aluno_dosage), 
            by = c("codmun" = "codigo_ibge", "ano")) %>% 
  mutate(grupo = case_when(
    dosage > 0 ~ "Winner",   # net beneficiary
    dosage < 0 ~ "Loser",   # net contributer
    TRUE ~ NA_character_
  ),
  grupo = factor(grupo, levels = c("Loser", "Winner")) #Beneficiary dummy
  )


rm(df_reg)
### ------------------ #
### 10.2.1 Regression ----
### ------------------ #
#### 10.2.1.1 Time to Treat ----
#### ------------------ #

#Total number of students
est <- feols(total_students_mpes ~ i(time_to_treat, ref = 0) |
               codmun,
             data = df_filter,
             vcov = "hetero")


est2 <- feols(schools ~ i(time_to_treat, ref = 0) |
                codmun,
              data = df_filter,
              vcov = "hetero")

etable(est, est2)
etable(est, est2,
       vcov = "hetero",
       headers = list(":_:" = list("N° Alunos" = 1, "N° Escolas" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Desc/num_time_to_treat.tex", replace = TRUE)


### ------------------ #
#### 10.2.1.2 Winner vs. Loser ----
#### ------------------ #


est <- feols(total_students_mpes ~ i(time_to_treat, grupo, ref = 0) |
                codmun,
              data = df_filter,
              vcov = "hetero")

est2 <- feols(schools ~ i(time_to_treat, grupo, ref = 0) |
                codmun,
              data = df_filter,
              vcov = "hetero")

etable(est, est2)


etable(est, est2,
       vcov = "hetero",
       headers = list(":_:" = list("N° Alunos" = 1, "N° Escolas" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Desc/num_win_lose.tex", replace = TRUE)


### ------------------ #
#### 10.2.1.3 Winner vs. Loser DOSAGE ----
#### ------------------ #

est <- feols(total_students_mpes ~ abs(dosage):i(time_to_treat, grupo, ref = 0) |
                codmun,
              data = df_filter,
              #weights = df_filter$total_students_mpes,
              vcov = "hetero")

est2 <- feols(schools ~ abs(dosage):i(time_to_treat, grupo, ref = 0) |
                codmun,
              data = df_filter,
              #weights = df_filter$total_students_mpes,
              vcov = "hetero")

etable(est, 
       est2)

etable(est, est2,
       vcov = "hetero",
       headers = list(":_:" = list("N° Alunos" = 1, "N° Escolas" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Desc/num_win_lose_dosage.tex", replace = TRUE)




### ------------------ #
#### 10.2.1.4 Winner vs. Loser ALUNO DOSAGE ----
#### ------------------ #

est <- feols(total_students_mpes ~ abs(aluno_dosage):i(time_to_treat, grupo, ref = 0) |
               codmun,
             data = df_filter,
             #weights = df_filter$total_students_mpes,
             vcov = "hetero")

est2 <- feols(schools ~ abs(aluno_dosage):i(time_to_treat, grupo, ref = 0) |
                codmun,
              data = df_filter,
              #weights = df_filter$total_students_mpes,
              vcov = "hetero")

etable(est, 
       est2)

etable(est, est2,
       vcov = "hetero",
       headers = list(":_:" = list("N° Alunos" = 1, "N° Escolas" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/v2/Tabelas/Desc/num_win_lose_aluno_dosage.tex", replace = TRUE)





rm(list = ls())

