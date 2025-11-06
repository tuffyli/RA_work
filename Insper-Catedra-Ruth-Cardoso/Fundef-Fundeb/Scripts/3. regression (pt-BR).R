# ---------------------------------------------------------------------------- #
# Regressions
# Last edited by: Tuffy Licciardi Issa
# Date: 05/11/2025
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
# 5. Regression ----
# --------------------------------------------------------------------------- #

df_trn <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/painel_notas_transferencias_2000_2024.csv")

df_pesosaeb <- readRDS("Z:/Tuffy/Paper - Educ/Dados/pesos_saeb3.rds")

#df_gio <- readRDS("Z:/Tuffy/Paper - Educ/Dados/Gio_df.rds")

df_sim <- readRDS("Z:/Tuffy/Paper - Educ/Dados/simulacao_const.rds") #%>% 

#sim_gio <- readRDS(("Z:/Tuffy/Paper - Educ/Dados/Gio_sim.rds"))

df_fib <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/Gastos municipais/FINBRA/Despesas/FINBRA_EDU_05_21.csv")


#5.0 Pesos e PIB ----
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



## 5.1 Incluindo as variáveis principais ----
#[English: Including Major Variables]

### 5.1.1 Incluindo dif_per_coef nas notas: ----
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



#saving database with dosage
saveRDS(df_reg, "Z:/Tuffy/Paper - Educ/Dados/regdf.rds")

teste <- df_reg %>% 
  select(2:5, 52, 53, 56, 58, 59, 61, 62, 65, 60, 77, 78, 81:89) %>% 
  select(-ano) %>% 
  distinct() %>% 
  select(1:3, dif_coef_pp, dif_rs_aluno, dif_rs_aluno_100, del_spending_dos,
         dosage, dosage_perc, aluno_dosage, shr_inf, everything())


### 5.1.2 Gráfico ----
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
  path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter",
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
  path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/Scatter",
  width = 600/96, height = 420/96, dpi = 110)

rm(p, temp)

# 6. SAEB----
## 6.1 Diff Coef ----
### 6.1.1 Nível ----

df_saeb <- df_reg %>% 
  filter(tipo == "Municipal" &
           rede == "Pública")

mat_5 <- feols(vl_nota_5_matematica ~ dif_coef_pp * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               #weights = df_saeb$peso_5,
               vcov = "hetero")

port_5 <- feols(vl_nota_5_portugues ~ dif_coef_pp * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                #weights = df_saeb$peso_5,
                vcov = "hetero")

mat_9 <- feols(vl_nota_9_matematica ~ dif_coef_pp * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               #weights = df_saeb$peso_9,
               vcov = "hetero")
port_9 <- feols(vl_nota_9_portugues ~ dif_coef_pp * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                #weights = df_saeb$peso_9,
                vcov = "hetero")



etable(mat_5, port_5, mat_9, port_9, 
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))



etable(mat_5, port_5, mat_9, port_9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Diff_coef/reg_v1.tex", replace = TRUE)



### 6.1.2 Nível (Pesos)----


df_saeb <- df_reg %>% 
  filter(tipo == "Municipal" &
           rede == "Pública")


mat_5 <- feols(vl_nota_5_matematica ~ dif_coef_pp * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               weights = df_saeb$peso_5,
               vcov = "hetero")

port_5 <- feols(vl_nota_5_portugues ~ dif_coef_pp * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                weights = df_saeb$peso_5,
                vcov = "hetero")

mat_9 <- feols(vl_nota_9_matematica ~ dif_coef_pp * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               weights = df_saeb$peso_9,
               vcov = "hetero")
port_9 <- feols(vl_nota_9_portugues ~ dif_coef_pp * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                weights = df_saeb$peso_9,
                vcov = "hetero")



etable(mat_5, port_5, mat_9, port_9, 
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))



etable(mat_5, port_5, mat_9, port_9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Diff_coef/reg_pesos.tex", replace = TRUE)





## 6.2 Dif por Aluno----
### 6.2.1 Nível ----
mat_5 <- feols(vl_nota_5_matematica ~ dif_rs_aluno_100 * i(k, ref = 0) + PIBpc 
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               #weights = df_saeb$peso_5,
               vcov = "hetero")

port_5 <- feols(vl_nota_5_portugues ~ dif_rs_aluno_100 * i(k, ref = 0) + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                #weights = df_saeb$peso_5,
                vcov = "hetero")


mat_9 <- feols(vl_nota_9_matematica ~ dif_rs_aluno_100 * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               #weights = df_saeb$peso_9,
               vcov = "hetero")
port_9 <- feols(vl_nota_9_portugues ~ dif_rs_aluno_100 * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                #weights = df_saeb$peso_9,
                vcov = "hetero")



etable(mat_5, port_5, mat_9, port_9, 
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))


etable(mat_5, port_5, mat_9, port_9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Diff_RS/reg_rs_aluno_v1.tex", replace = TRUE)


#### 6.2.1.1 Event Study: ----

# Define uma lista com os objetos dos modelos
models_list <- list(
  mat_5 = mat_5,
  port_5 = port_5,
  mat_9 = mat_9,
  port_9 = port_9
)

# Loop para gerar gráficos para cada modelo
for (model_name in names(models_list)) {
  # Extrair o modelo atual
  current_model <- models_list[[model_name]]
  
  # Extrair a tabela de coeficientes do modelo atual
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
      term = paste0("k::0:", model_name), # Adicionar termo com referência ao modelo
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
      y = "Efeito estimado (pontos no SAEB)"
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
    filename = paste0("grafico_", model_name, "RS_aluno_v1.png"), # Nome baseado no modelo
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Diff_RS",
    width = 600/96, height = 420/96, dpi = 110
  )
}

rm(mat_5, mat_9, port_5, port_9, teste, event_df)


### 6.2.2 Nível (PESO) ----
mat_5 <- feols(vl_nota_5_matematica ~ dif_rs_aluno_100 * i(k, ref = 0) + PIBpc 
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               weights = df_saeb$peso_5,
               vcov = "hetero")

port_5 <- feols(vl_nota_5_portugues ~ dif_rs_aluno_100 * i(k, ref = 0) + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                weights = df_saeb$peso_5,
                vcov = "hetero")


mat_9 <- feols(vl_nota_9_matematica ~ dif_rs_aluno_100 * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               weights = df_saeb$peso_9,
               vcov = "hetero")
port_9 <- feols(vl_nota_9_portugues ~ dif_rs_aluno_100 * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                weights = df_saeb$peso_9,
                vcov = "hetero")



etable(mat_5, port_5, mat_9, port_9, 
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))


etable(mat_5, port_5, mat_9, port_9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Diff_RS/reg_rs_aluno_pesos.tex", replace = TRUE)



#### 6.2.2.1 Event Study: ----

# Define uma lista com os objetos dos modelos
models_list <- list(
  mat_5 = mat_5,
  port_5 = port_5,
  mat_9 = mat_9,
  port_9 = port_9
)

# Loop para gerar gráficos para cada modelo
for (model_name in names(models_list)) {
  # Extrair o modelo atual
  current_model <- models_list[[model_name]]
  
  # Extrair a tabela de coeficientes do modelo atual
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
      term = paste0("k::0:", model_name), # Adicionar termo com referência ao modelo
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
      y = "Efeito estimado (pontos no SAEB)"
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
    filename = paste0("grafico_", model_name, "RS_aluno_pesos.png"), # Nome baseado no modelo
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Diff_RS",
    width = 600/96, height = 420/96, dpi = 110
  )
}

rm(mat_5, mat_9, port_5, port_9, event_df)


## 6.3 Dosage ----
### 6.3.1 Nível----


mat_5 <- feols(vl_nota_5_matematica ~ dosage * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               #weights = df_saeb$peso_5,
               vcov = "hetero")

port_5 <- feols(vl_nota_5_portugues ~ dosage * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                #weights = df_saeb$peso_5,
                vcov = "hetero")

mat_9 <- feols(vl_nota_9_matematica ~ dosage * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               #weights = df_saeb$peso_9,
               vcov = "hetero")
port_9 <- feols(vl_nota_9_portugues ~ dosage * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                #weights = df_saeb$peso_9,
                vcov = "hetero")



etable(mat_5, port_5, mat_9, port_9, 
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))



etable(mat_5, port_5, mat_9, port_9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage/dosage_reg.tex", replace = TRUE)



## 6.4 Dosage Aluno----
### 6.4.1 Nível----


mat_5 <- feols(vl_nota_5_matematica ~ aluno_dosage * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               #weights = df_saeb$peso_5,
               vcov = "hetero")

port_5 <- feols(vl_nota_5_portugues ~ aluno_dosage * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                #weights = df_saeb$peso_5,
                vcov = "hetero")

mat_9 <- feols(vl_nota_9_matematica ~ aluno_dosage * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               #weights = df_saeb$peso_9,
               vcov = "hetero")
port_9 <- feols(vl_nota_9_portugues ~ aluno_dosage * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                #weights = df_saeb$peso_9,
                vcov = "hetero")



etable(mat_5, port_5, mat_9, port_9, 
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))



etable(mat_5, port_5, mat_9, port_9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/dosage_aluno_reg.tex", replace = TRUE)



rm(mat_5, port_5, mat_9, port_9)
# ------------------------------- #
## 6.5 Student Exp ----
# ------------------------------- #


#' Here I will run the regressions utilizing the student exposition to treatment
#' variable.


### 6.5.0 Data ----

df_long_wide <- df_saeb %>%
  pivot_longer(
    cols = starts_with("vl_nota_"),            # select all score columns
    names_to = c("grade", "subject"),
    names_pattern = "vl_nota_([0-9]+)_(.*)",   # capture grade number and subject
    values_to = "score"
  ) %>%
  # optional: clean subject names
  mutate(
    grade = as.integer(grade)                  # make grade numeric 5 or 9
  ) %>%
  # spread subject into two columns: mathematics | portuguese
  pivot_wider(
    names_from  = subject,
    values_from = score,
    names_glue  = "{subject}"                 # will create 'mathematics' and 'portuguese'
  ) %>%
  # reorder columns if you want: year, grade, mathematics, portuguese, ...
  select(codigo_ibge,uf,nome, ano, matematica, portugues, grade, everything() ) %>% 
  filter(!is.na(grade)) %>% #Removing the vestigial vl_media NULL values
  mutate(
    treat_exp = case_when(
      #For 5th grade in 1st row vs. 9th grade second row
      ano == 2005 & grade == 9 ~ -4/10,
      
      ano == 2005 & grade == 5 |
        ano == 2007 & grade == 9 ~ -2/10,
      
      ano == 2007 & grade == 5 |
        ano == 2009 & grade == 9 ~ 0,
      
      ano == 2009 & grade == 5 |
        ano == 2011 & grade == 9 ~ 2/10,
      
      ano == 2011 & grade == 5 |
        ano == 2013 & grade == 9 ~ 4/10,
      
      ano == 2013 & grade == 5 |
        ano == 2015 & grade == 9 ~ 6/10,
      
      ano == 2015 & grade == 5 |
        ano == 2017 & grade == 9 ~ 8/10,
      
      ano == 2017 & grade == 5 |
        ano == 2019 & grade == 9 ~ 1,
      
      ano == 2019 & grade == 5 ~ 12/10,
      
      TRUE ~ NA
    )
  ) %>% 
  select(codigo_ibge,uf,nome, ano, matematica, portugues, grade, treat_exp, everything()) %>% 
  mutate(
    group = ifelse(grade == 5, "5th", "9th"),
    group = factor(group, levels = c("5th", "9th"))
  )
  
  
# ------------------------- #
### 6.5.1 Dosage ----
# ------------------------- #

#The main regression follows. We interact the treatment exposure to the municipality
# exposure to financial gain through the policy.

#### 6.5.1.1 Main ----
mat_tot5 <- feols(as.numeric(matematica) ~ treat_exp : dosage
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_long_wide %>% filter(grade == 5) ,
               #weights = df_saeb$peso_5,
               vcov = "hetero")

port_tot5 <- feols(as.numeric(portugues) ~ treat_exp : dosage 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_long_wide %>% filter(grade == 5) ,
                #weights = df_saeb$peso_5,
                vcov = "hetero")


mat_tot9 <- feols(as.numeric(matematica) ~ treat_exp : dosage
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_long_wide %>% filter(grade == 9) ,
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

port_tot9 <- feols(as.numeric(portugues) ~ treat_exp : dosage 
                   + PIBpc
                   | codigo_ibge + ano + uf^ano,
                   data = df_long_wide %>% filter(grade == 9) ,
                   #weights = df_saeb$peso_5,
                   vcov = "hetero")



etable(mat_tot5, port_tot5, mat_tot9, port_tot9,
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1)))



etable(mat_tot5, port_tot5, mat_tot9, port_tot9, 
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1,"Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage/treat_exp_dosage_reg.tex", replace = TRUE)

#### 6.5.1.2 Log ----

mat_tot5 <- feols(log(as.numeric(matematica)) ~ treat_exp : dosage
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_long_wide %>% filter(grade == 5) ,
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

port_tot5 <- feols(log(as.numeric(portugues)) ~ treat_exp : dosage 
                   + PIBpc
                   | codigo_ibge + ano + uf^ano,
                   data = df_long_wide %>% filter(grade == 5) ,
                   #weights = df_saeb$peso_5,
                   vcov = "hetero")


mat_tot9 <- feols(log(as.numeric(matematica)) ~ treat_exp : dosage
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_long_wide %>% filter(grade == 9) ,
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

port_tot9 <- feols(log(as.numeric(portugues)) ~ treat_exp : dosage 
                   + PIBpc
                   | codigo_ibge + ano + uf^ano,
                   data = df_long_wide %>% filter(grade == 9) ,
                   #weights = df_saeb$peso_5,
                   vcov = "hetero")


etable(mat_tot5, port_tot5, mat_tot9, port_tot9, 
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage/log_treat_exp_dosage_reg.tex", replace = TRUE)

# -------------------------- #
#### 6.5.1.3 Event Study ----
# -------------------------- #




mat_tot5 <- feols(as.numeric(matematica) ~ dosage : i(treat_exp, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_long_wide %>% filter(grade == 5) ,
                 #weights = df_saeb$peso_5,
                 vcov = "hetero")

port_tot5 <- feols(as.numeric(portugues) ~ dosage : i(treat_exp, ref = 0)
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_long_wide %>% filter(grade == 5) ,
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

mat_tot9 <- feols(as.numeric(matematica) ~ dosage : i(treat_exp, ref = 0)
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_long_wide %>% filter(grade == 9) ,
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

port_tot9 <- feols(as.numeric(portugues) ~ dosage : i(treat_exp, ref = 0)
                   + PIBpc
                   | codigo_ibge + ano + uf^ano,
                   data = df_long_wide %>% filter(grade == 9) ,
                   #weights = df_saeb$peso_5,
                   vcov = "hetero")


etable(mat_tot5, port_tot5, mat_tot9, port_tot9,
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))



etable(mat_tot5, port_tot5, mat_tot9, port_tot9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage/es_treat_exp_dosage_reg.tex", replace = TRUE)


##### 6.5.1.3.1 Graph ----

# ---------------------- #
###### 5th grade  ----
  
  temp <- bind_rows(
    broom::tidy(mat_tot5, conf.int = TRUE) %>% mutate(grupo = "Matematica"),
    broom::tidy(port_tot5, conf.int = TRUE) %>% mutate(grupo = "Portugues")
    ) %>% 
    filter(str_detect(term, "treat_exp")) %>%
    mutate(
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
 
   
  temp_edu <- temp %>%
    distinct(grupo) %>%
    mutate(
      term = "time_to_treat:0",
      estimate = 0,
      std.error = 0,
      statistic = 0,
      p.value = 1,
      conf.low = 0,
      conf.high = 0,
      time_exposure = 0
    )
  
  temp <- bind_rows(temp, temp_edu) %>%
    arrange(grupo, time_exposure)
  
  p5 <- ggplot(temp, aes(x = time_exposure, y = estimate, group = grupo)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = grupo), shape = 15, size = 2) +
    geom_line(aes(color = grupo)) +
    labs(
      title = "5° Ano",
      x = "Years of Exposure",
      y = "Nota",
      color = NULL, fill = NULL) +
    theme_classic() +
    theme(
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11),
      legend.position = c(0.94, 0.17),
      legend.justification = c("right", "top"),
      legend.background = element_blank()
    ) +
    scale_x_continuous(
      breaks = seq(min(temp$time_exposure, na.rm = TRUE),
                   max(temp$time_exposure, na.rm = TRUE),
                   by = 0.2)
    )
  
  p5

  ggsave( #Saving image
    filename = paste0("carrillo_exp_5.png"),
    plot = p5,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
    width = 800/96, height = 620/96, dpi = 300
  )

  rm( mat_tot5, port_tot5, temp, temp_edu)

  # ---------------------- #
  ###### 9th grade  ----
  
  temp <- bind_rows(
    broom::tidy(mat_tot9, conf.int = TRUE) %>% mutate(grupo = "Matematica"),
    broom::tidy(port_tot9, conf.int = TRUE) %>% mutate(grupo = "Portugues")
  ) %>% 
    filter(str_detect(term, "treat_exp")) %>%
    mutate(
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
  
  
  temp_edu <- temp %>%
    distinct(grupo) %>%
    mutate(
      term = "time_to_treat:0",
      estimate = 0,
      std.error = 0,
      statistic = 0,
      p.value = 1,
      conf.low = 0,
      conf.high = 0,
      time_exposure = 0
    )
  
  temp <- bind_rows(temp, temp_edu) %>%
    arrange(grupo, time_exposure)
  
  p9 <- ggplot(temp, aes(x = time_exposure, y = estimate, group = grupo)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = grupo), shape = 15, size = 2) +
    geom_line(aes(color = grupo)) +
    labs(title = "9° Ano", x = "Years of Exposure", y = "Nota", color = NULL, fill = NULL) +
    theme_classic() +
    theme(
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11),
      legend.position = c(0.94, 0.17),
      legend.justification = c("right", "top"),
      legend.background = element_blank()
    ) +
    scale_x_continuous(
      breaks = seq(min(temp$time_exposure, na.rm = TRUE),
                   max(temp$time_exposure, na.rm = TRUE),
                   by = 0.2)
    )
  
  p9
  
  ggsave( #Saving image
    filename = paste0("carrillo_exp_9.png"),
    plot = p9,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
    width = 800/96, height = 620/96, dpi = 300
  )
  
  rm( mat_tot9, port_tot9, temp, temp_edu)
  
  
  ###### Combined -----
  
  grid_plot <- (p5 + p9)
  
  final <- grid_plot + patchwork::plot_annotation(
    #title = "Event-study: infrastructure / staff outcomes",
    caption = "Estimates from feols(...) with i(k, ref = -1)"
  )
  
  final
  
  ggsave( #Saving image
    filename = paste0("carrillo_exp_dosage.png"),
    plot = final,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
    width = 880/96, height = 520/96, dpi = 300
  )
  
  rm(p5, p9, final, grid_plot)
# --------------------------------------------------- #
### 6.5.2 Student Dosage ----
# --------------------------------------------------- #
#Repating the previous step to account fot the student dosage variable.

#### 6.5.2.1 Main ----
mat_tot5 <- feols(as.numeric(matematica) ~ treat_exp : aluno_dosage
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_long_wide %>% filter(grade == 5),
                 #weights = df_saeb$peso_5,
                 vcov = "hetero")

port_tot5 <- feols(as.numeric(portugues) ~ treat_exp : aluno_dosage 
                  + PIBpc
                  | codigo_ibge + ano + uf^ano ,
                  data = df_long_wide %>% filter(grade == 5),
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

mat_tot9 <- feols(as.numeric(matematica) ~ treat_exp : aluno_dosage
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_long_wide %>% filter(grade == 9),
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

port_tot9 <- feols(as.numeric(portugues) ~ treat_exp : aluno_dosage 
                   + PIBpc
                   | codigo_ibge + ano + uf^ano ,
                   data = df_long_wide %>% filter(grade == 9),
                   #weights = df_saeb$peso_5,
                   vcov = "hetero")



etable(mat_tot5, port_tot5, mat_tot9, port_tot9,
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))



etable(mat_tot5, port_tot5, mat_tot9, port_tot9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/treat_exp_aluno_dosage_aluno_reg.tex", replace = TRUE)



#### 6.5.2.2 Log ----

#Reapeating, but for extraction of the elasticity

mat_tot5 <- feols(log(as.numeric(matematica)) ~ treat_exp : aluno_dosage
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_long_wide %>% filter(grade == 5),
                 #weights = df_saeb$peso_5,
                 vcov = "hetero")

port_tot5 <- feols(log(as.numeric(portugues)) ~ treat_exp : aluno_dosage 
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_long_wide %>% filter(grade == 5) ,
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")



mat_tot9 <- feols(log(as.numeric(matematica)) ~ treat_exp : aluno_dosage
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_long_wide %>% filter(grade == 9),
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

port_tot9 <- feols(log(as.numeric(portugues)) ~ treat_exp : aluno_dosage 
                   + PIBpc
                   | codigo_ibge + ano + uf^ano,
                   data = df_long_wide %>% filter(grade == 9) ,
                   #weights = df_saeb$peso_5,
                   vcov = "hetero")


etable(mat_tot5, port_tot5, mat_tot9, port_tot9,
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))



etable(mat_tot5, port_tot5, mat_tot9, port_tot9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/log_treat_exp_aluno_dosage_aluno_reg.tex", replace = TRUE)




#### 6.5.2.3 Event Study ----

mat_tot5 <- feols(as.numeric(matematica) ~ aluno_dosage : i(treat_exp, ref = 0)
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_long_wide %>% filter(grade == 5) ,
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

port_tot5 <- feols(as.numeric(portugues) ~ aluno_dosage : i(treat_exp, ref = 0)
                   + PIBpc
                   | codigo_ibge + ano + uf^ano,
                   data = df_long_wide %>% filter(grade == 5) ,
                   #weights = df_saeb$peso_5,
                   vcov = "hetero")

mat_tot9 <- feols(as.numeric(matematica) ~ aluno_dosage : i(treat_exp, ref = 0)
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_long_wide %>% filter(grade == 9) ,
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

port_tot9 <- feols(as.numeric(portugues) ~ aluno_dosage : i(treat_exp, ref = 0)
                   + PIBpc
                   | codigo_ibge + ano + uf^ano,
                   data = df_long_wide %>% filter(grade == 9) ,
                   #weights = df_saeb$peso_5,
                   vcov = "hetero")


etable(mat_tot5, port_tot5, mat_tot9, port_tot9,
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))



etable(mat_tot5, port_tot5, mat_tot9, port_tot9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/es_treat_exp_aluno_dosage_reg.tex", replace = TRUE)



##### 6.5.2.3.1 Graph ----

# ---------------------- #
###### 5th grade  ----

temp <- bind_rows(
  broom::tidy(mat_tot5, conf.int = TRUE) %>% mutate(grupo = "Matematica"),
  broom::tidy(port_tot5, conf.int = TRUE) %>% mutate(grupo = "Portugues")
) %>% 
  filter(str_detect(term, "treat_exp")) %>%
  mutate(
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


temp_edu <- temp %>%
  distinct(grupo) %>%
  mutate(
    term = "time_to_treat:0",
    estimate = 0,
    std.error = 0,
    statistic = 0,
    p.value = 1,
    conf.low = 0,
    conf.high = 0,
    time_exposure = 0
  )

temp <- bind_rows(temp, temp_edu) %>%
  arrange(grupo, time_exposure)

p5 <- ggplot(temp, aes(x = time_exposure, y = estimate, group = grupo)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 0, color = "black") +
  geom_point(aes(color = grupo), shape = 15, size = 2) +
  geom_line(aes(color = grupo)) +
  labs(title = "5° Ano", x = "Years of Exposure", y = "Nota", color = NULL, fill = NULL) +
  theme_classic() +
  theme(
    axis.line = element_line(color = "grey70"),
    panel.grid = element_blank(),
    axis.title = element_text(size = 11),
    legend.position = c(0.94, 0.17),
    legend.justification = c("right", "top"),
    legend.background = element_blank()
  ) +
  scale_x_continuous(
    breaks = seq(min(temp$time_exposure, na.rm = TRUE),
                 max(temp$time_exposure, na.rm = TRUE),
                 by = 0.2)
  )

p5

ggsave( #Saving image
  filename = paste0("carrillo_exp_5.png"),
  plot = p5,
  path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
  width = 800/96, height = 620/96, dpi = 300
)

rm( mat_tot5, port_tot5, temp, temp_edu)

# ---------------------- #
###### 9th grade  ----

temp <- bind_rows(
  broom::tidy(mat_tot9, conf.int = TRUE) %>% mutate(grupo = "Matematica"),
  broom::tidy(port_tot9, conf.int = TRUE) %>% mutate(grupo = "Portugues")
) %>% 
  filter(str_detect(term, "treat_exp")) %>%
  mutate(
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


temp_edu <- temp %>%
  distinct(grupo) %>%
  mutate(
    term = "time_to_treat:0",
    estimate = 0,
    std.error = 0,
    statistic = 0,
    p.value = 1,
    conf.low = 0,
    conf.high = 0,
    time_exposure = 0
  )

temp <- bind_rows(temp, temp_edu) %>%
  arrange(grupo, time_exposure)

p9 <- ggplot(temp, aes(x = time_exposure, y = estimate, group = grupo)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 0, color = "black") +
  geom_point(aes(color = grupo), shape = 15, size = 2) +
  geom_line(aes(color = grupo)) +
  labs(title = "9° Ano", x = "Years of Exposure", y = "Nota", color = NULL, fill = NULL) +
  theme_classic() +
  theme(
    axis.line = element_line(color = "grey70"),
    panel.grid = element_blank(),
    axis.title = element_text(size = 11),
    legend.position = c(0.94, 0.17),
    legend.justification = c("right", "top"),
    legend.background = element_blank()
  ) +
  scale_x_continuous(
    breaks = seq(min(temp$time_exposure, na.rm = TRUE),
                 max(temp$time_exposure, na.rm = TRUE),
                 by = 0.2)
  )

p9

ggsave( #Saving image
  filename = paste0("carrillo_exp_9.png"),
  plot = p9,
  path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
  width = 800/96, height = 620/96, dpi = 300
)

rm( mat_tot9, port_tot9, temp, temp_edu)


###### Combined -----


grid_plot <- (p5 + p9)


final <- grid_plot + patchwork::plot_annotation(
  #title = "Event-study: infrastructure / staff outcomes",
  caption = "Estimates from feols(...) with i(k, ref = -1)"
)

final

ggsave( #Saving image
  filename = paste0("carrillo_exp_aluno_dosage.png"),
  plot = final,
  path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Robust/",
  width = 880/96, height = 520/96, dpi = 300
)


# ---------------------------------------------------------------------------- #
# 7. Gasto Educacional Per-Capita----
# ---------------------------------------------------------------------------- #
## 7.1 Nova Base ----

df_spend <- df_reg %>% 
  filter(tipo == "Municipal") %>% 
  select(
    -c(vl_nota_5_matematica, vl_nota_5_portugues, vl_nota_5_media, vl_nota_9_matematica,
       vl_nota_9_media, vl_nota_9_portugues, vl_nota_em_matematica, vl_nota_em_portugues,
       vl_nota_em_media, tx_aprovacao_iniciais, tx_aprovacao_1, tx_aprovacao_2,
       tx_aprovacao_3, tx_aprovacao_4, tx_aprovacao_5, tx_aprovacao_finais, tx_aprovacao_6,
       tx_aprovacao_7, tx_aprovacao_8, tx_aprovacao_9, tx_aprovacao_em, tx_aprovacao_1em,
       tx_aprovacao_2em, tx_aprovacao_3em, tx_aprovacao_4em, rede, X.x, X.y, peso_5, peso_9)
  )


# df_spend <- df_trn %>% 
#   left_join(df_sim %>% select(c(codigo_ibge, coef_est_fnde:receita_real, dif_rs_aluno, total_alunos_2006)),
#             by = "codigo_ibge") %>% 
#   filter(!is.na(coef_est_fnde)) %>%
#   mutate(k = ano - 2007) %>%    
#   # filter(ano %% 2 != 0) %>% # COLOCA TODOS OS ANOS, E NÃO SÓ OS QUE TEM SAEB
#   mutate(uf = as.factor(uf)) %>% 
#   select(-c(12:38), -X) %>% 
#   distinct()
# 
# 
# df_spend <- df_spend %>% 
#   mutate(codigo_ibge = as.numeric(str_sub(as.character(codigo_ibge), 1, -2))) %>% 
#   left_join((df_fib %>% select(-c(uf))), by = c("codigo_ibge", "ano")) %>% 
#   mutate(des_edu_pc = educacao/populacao,
#          des_fund_pc = ensino_fundamental/populacao,
#          des_med_pc = ensino_medio/populacao,
#          des_inf_pc = educacao_infantil/populacao) %>% 
#   filter(ano < 2013 | (ano >= 2013 & coluna == "Despesas Empenhadas")) %>% 
#   relocate(despesas_totais, .after= "nome") %>%
#   relocate(educacao, .after = "despesas_totais") %>% 
#   relocate(populacao, .after = "educacao") %>% 
#   relocate(des_fund_pc, .after = "populacao") %>% 
#   relocate(des_med_pc, .after = "populacao") %>% 
#   relocate(des_inf_pc, .after = "populacao")
# 
# 
# df_spend <- left_join(
#   df_spend,
#   pib,
#   by = c("codigo_ibge" , "ano")
# ) %>% 
#   relocate(PIBpc, .after = "nome")

df_spend <- df_spend %>%
  mutate(dif_rs_aluno_100 = dif_rs_aluno / 100) %>% 
  filter(tipo == "Municipal")


rm(df_trn, df_sim, df_pesosaeb, df_fib)

##7.2 Diff por Aluno----

mod_edu <- feols(des_edu_pc ~ dif_rs_aluno_100 * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 vcov = "hetero")


mod_fund <- feols(des_fund_pc ~ dif_rs_aluno_100 * i(k, ref = 0)
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_spend,
                  vcov = "hetero")

mod_med <- feols(des_med_pc ~ dif_rs_aluno_100 * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 vcov = "hetero")

mod_inf <- feols(des_inf_pc ~ dif_rs_aluno_100 * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 vcov = "hetero")



etable(mod_edu, mod_fund, mod_med, mod_inf, 
       vcov = "hetero", 
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)))

#Exportar

etable(mod_edu, mod_fund, mod_med, mod_inf,
       vcov = "hetero",
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Diff_RS/reg_dif_RS_aluno.tex", replace = TRUE)


### 7.2.1 ES ----



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
    filename = paste0("grafico_", model_name, "_dif_rs.png"), # Nome baseado no modelo
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Diff_RS",
    width = 600/96, height = 420/96, dpi = 110
  )
  rm(model_name, p) 
  
}

rm(mod_edu, mod_fund, mod_inf, mod_med, models_list, p, rede_reg, current_model, event_df) 


## 7.3 Coef Diff ----


mod_edu <- feols(des_edu_pc ~ dif_coef_pp * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 vcov = "hetero")


mod_fund <- feols(des_fund_pc ~ dif_coef_pp * i(k, ref = 0)
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_spend,
                  vcov = "hetero")

mod_med <- feols(des_med_pc ~ dif_coef_pp * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 vcov = "hetero")

mod_inf <- feols(des_inf_pc ~ dif_coef_pp * i(k, ref = 0)
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
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Diff_coef/reg_des_edu_CD.tex", replace = TRUE)


### 7.3.1 ES ----


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
    filename = paste0("grafico_", model_name, "_des_edu.png"), # Nome baseado no modelo
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Diff_coef",
    width = 600/96, height = 420/96, dpi = 110
  )
 
  rm(model_name, p) 
}

rm(mod_edu, mod_fund, mod_inf, mod_med, models_list, p, current_model, event_df) 


## 7.4 Dosage ----
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
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage/reg_dif_dosage.tex", replace = TRUE)



### 7.4.1 ES ----


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
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Dosage",
    width = 600/96, height = 420/96, dpi = 110
  )
  
  rm(model_name, p)
}

rm(mod_edu, mod_fund, mod_inf, mod_med, event_df, current_model, models_list)



## 7.5 Dosage Aluno ----

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
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/reg_dif_aluno_dosage.tex", replace = TRUE)

### 7.5.1 ES ----

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
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Dosage_aluno",
    width = 600/96, height = 420/96, dpi = 110
  )
  
  rm(model_name, p)
}

rm(mod_edu, mod_fund, mod_inf, mod_med, event_df, current_model, models_list)


# #### WIP ---
# 
# 
# models_list <- list(
#   edu  = mod_edu,
#   fund = mod_fund,
#   med  = mod_med,
#   inf  = mod_inf
# )
# 
# for (model_name in names(models_list)){
#   
#   current_model <- models_list[[model_name]]
#   event_df <- as.data.frame(current_model$coeftable)
#   print(event_df)
#   
#   # Criar coluna com os termos (anos relativos k)
#   event_df$term <- rownames(event_df)
#   event_df <- event_df %>%
#     mutate(
#       k = as.numeric(gsub(".*k::(-?\\d+)$", "\\1", term)), # Extrair o valor de k
#       conf.low = Estimate - 1.96 * `Std. Error`,           # Limite inferior do IC
#       conf.high = Estimate + 1.96 * `Std. Error`           # Limite superior do IC
#     ) %>%
#     add_row(
#       Estimate = 0,
#       `Std. Error` = 0,
#       `t value` = 0,
#       `Pr(>|t|)` = 0,
#       term = "k::0", # Adicionar termo com referência ao modelo
#       k = 0,
#       conf.low = 0,
#       conf.high = 0
#     )
#   
#   # Criar o gráfico de estilo event-study
#   p <- ggplot(event_df, aes(x = k + 2007, y = Estimate)) +
#     geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey60", alpha = 0.3) +
#     geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
#     geom_vline(xintercept = 2007, color = "black") +
#     geom_point(shape = 15, size = 2, color = "black") +
#     geom_line(color = "black") +
#     labs(
#       # title = paste("Event-Study: ", model_name),
#       x = "Ano", #(relativo a 2007)",
#       y = "R$ per capita"
#     ) +
#     # ylim(-0.4, 0.4) +
#     theme_classic() +
#     theme(
#       axis.line = element_line(color = "grey70"),
#       panel.grid = element_blank(),
#       axis.title = element_text(size = 11)
#     )
#   
#   # Exibir o gráfico no console
#   print(p)
#   
#   # Salvar o gráfico como arquivo PNG
#   ggsave(
#     filename = paste0("grafico_", model_name, "_dif_spdo_rs.png"), # Nome baseado no modelo
#     plot = p,
#     path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES",
#     width = 600/96, height = 420/96, dpi = 110
#   )
#   
# }
# 
# rm(mod_edu, mod_fund, mod_inf, mod_med, models_list, p, rede_reg, current_model, event_df) 
# 
# ### 5.5.4 Above n Below---
# 
# 
# df_saeb <- df_saeb %>%
#   mutate(
#     grupo = case_when(
#       dif_rs_aluno > 0 ~ "Above",   # ganho líquido
#       dif_rs_aluno < 0 ~ "Below",   # perda líquida
#       TRUE ~ NA_character_
#     ),
#     grupo = factor(grupo, levels = c("Below", "Above"))
#   )
# 
# 
# df_saeb <- df_saeb %>%
#   mutate(
#     q_dosagem = cut(
#       dif_rs_aluno_100,
#       breaks = quantile(as.numeric(dif_rs_aluno_100),
#                         probs = c(0, .25, .50, .75, 1), na.rm = TRUE),
#       include.lowest = TRUE,
#       labels = c("Q1", "Q2", "Q3", "Q4")
#     )
#   )
# 
# 
# ####5.5.4.1 Educ ---
# 
# mod_edu_ab <- feols(
#   des_edu_pc ~ dif_rs_aluno_100 * i(k, grupo, ref = 0) + PIBpc |
#     codigo_ibge + ano + uf^ano,
#   data = df_saeb, vcov = "hetero"
# )
# 
# 
# mod_fund_ab <- feols(
#   des_fund_pc ~ dif_rs_aluno_100 * i(k, grupo, ref = 0) + PIBpc |
#     codigo_ibge + ano + uf^ano,
#   data = df_saeb, vcov = "hetero"
# )
# 
# mod_med_ab <- feols(
#   des_med_pc ~ dif_rs_aluno_100 * i(k, grupo, ref = 0) + PIBpc |
#     codigo_ibge + ano + uf^ano,
#   data = df_saeb, vcov = "hetero"
# )
# 
# mod_inf_ab <- feols(
#   des_inf_pc ~ dif_rs_aluno_100 * i(k, grupo, ref = 0) + PIBpc |
#     codigo_ibge + ano + uf^ano,
#   data = df_saeb, vcov = "hetero"
# )
# 
# 
# etable(mod_edu_ab, mod_fund_ab, mod_med_ab, mod_inf_ab,
#        vcov = "hetero",
#        headers = list(":_:" = list("Total"=1, "Fundamental"=1, "Médio"=1, "Infantil"=1)))
# 
# 
# 
# 
# models_list <- list(
#   edu  = mod_edu_ab,
#   fund = mod_fund_ab,
#   med  = mod_med_ab,
#   inf  = mod_inf_ab
# )
# 
# 
# # 
# #  current_model <- models_list[["edu"]]
# #  td <- broom::tidy(current_model, conf.int = TRUE) %>%
# #    filter(str_starts(term, "abs")) %>%
# #    mutate(grupo = as.factor(case_when(
# #      str_detect(term, "Above") ~ "Above",
# #      str_detect(term, "Below") ~ "Below",
# #    )),
# #    k = as.numeric(str_sub(term, 26, -14))
# #    )
# # 
# #  print(td)
# # 
# # 
# #  # combina base + d * slope (IC aprox. ignorando covariância entre termos)
# #  event_df <- base_df %>%
# #    left_join(slope_df, by = c("k","grupo")) %>%
# #    mutate(
# #      estimate_s = coalesce(estimate_s, 0),
# #      se_s       = coalesce(se_s, 0),
# #      Estimate   = estimate_b + d_val * estimate_s,
# #      `Std. Error` = sqrt(se_b^2 + (d_val * se_s)^2),
# #      conf.low   = Estimate - 1.96 * `Std. Error`,
# #      conf.high  = Estimate + 1.96 * `Std. Error`,
# #      ano_evento = 2007 + k,
# #      grupo      = factor(grupo, levels = c("Above","Below"))
# #    ) %>%
# #    arrange(grupo, k)
# # 
# #  # adiciona baseline k=0 = 0 para ambos os grupos
# #  event_df <- bind_rows(
# #    event_df,
# #    tibble(
# #      Estimate = 0, `Std. Error` = 0, conf.low = 0, conf.high = 0,
# #      k = 0, ano_evento = 2007, grupo = factor(c("Above","Below"), levels = c("Above","Below"))
# #    )
# #  ) %>% arrange(grupo, k)
# # 
# #  # gráfico no mesmo estilo do seu loop
# #  p_facet <- ggplot(temp_edu, aes(x = time_to_treat, y = estimate, group = grupo)) +
# #    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
# #    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
# #    geom_vline(xintercept = 0, color = "black") +
# #    geom_point(aes(color = grupo), shape = 15, size = 2) +
# #    geom_line(aes(color = grupo)) +
# #    #facet_wrap(~ grupo, ncol = 1) +     # <--- separate plot per group
# #    labs(x = "Time to Treat", y = "R$ per capita",
# #        color = NULL, fill = NULL) +
# #    theme_classic() +
# #    theme(
# #      axis.line = element_line(color = "grey70"),
# #      panel.grid = element_blank(),
# #      axis.title = element_text(size = 11),
# #      legend.position = c(0.95, 0.95),
# #      legend.justification = c("right", "top") # anchor legend box
# #    ) + # anchor legend box
# #    scale_x_continuous(
# #      breaks = seq(min(temp_edu$time_to_treat, na.rm = TRUE),
# #                   max(temp_edu$time_to_treat, na.rm = TRUE),
# #                   by = 2)
# #    )
# # 
# #  p_facet
# # 
# #  print(p)
# # 
# #  
# 
# ####5.5.4.1 Gráfico ---
# 
# 
# 
# 
# for (model_name in names(models_list)) {
#   
#   current_model <- models_list[[model_name]]
#   temp <- broom::tidy(current_model, conf.int = TRUE) %>% 
#     slice((which(term == "PIBpc") + 1):n()) %>%
#     mutate(
#       time_to_treat = str_extract(term, "(?<=k::)-?\\d+"),
#       time_to_treat = as.numeric(time_to_treat),
#       grupo = str_extract(term, "(?<=grupo::)\\w+"),
#       grupo = as.factor(grupo))
#   
#   temp_edu <- temp %>%
#     distinct(grupo) %>%             # one row per group (Above / Below)
#     mutate(
#       term = "k:0",     # or another label that fits your pattern
#       estimate = 0,
#       std.error = 0,
#       statistic = 0,
#       p.value = 1,
#       conf.low = 0,
#       conf.high = 0,
#       time_to_treat = 0
#     )
#   
#   temp <- temp %>% 
#     bind_rows(
#       temp_edu %>%
#         distinct(grupo) %>%
#         mutate(
#           time_to_treat = 0,
#           estimate = 0,
#           conf.low = 0,
#           conf.high = 0
#         )
#     ) %>%
#     arrange(grupo, time_to_treat)
#   
#   # # termos de inclinação (dose): abs(dif_rs_aluno_100):i(k, grupo)
#   # slope_df <- td %>%
#   #   dplyr::filter(str_detect(term, "^abs\\(dif_rs_aluno_100\\):k::-?\\d+:grupo::(Above|Below)$")) %>%
#   #   transmute(
#   #     k = as.numeric(str_extract(term, "-?\\d+(?=:grupo::)")),
#   #     grupo = str_match(term, "grupo::(Above|Below)$")[,2],
#   #     estimate_s = estimate,
#   #     se_s = std.error
#   #   )
#   # 
#   # combina base + d * slope (IC aprox. ignorando covariância entre termos)
#   # event_df <- base_df %>%
#   #   left_join(slope_df, by = c("k","grupo")) %>%
#   #   mutate(
#   #     estimate_s = coalesce(estimate_s, 0),
#   #     se_s       = coalesce(se_s, 0),
#   #     Estimate   = estimate_b + d_val * estimate_s,
#   #     `Std. Error` = sqrt(se_b^2 + (d_val * se_s)^2),
#   #     conf.low   = Estimate - 1.96 * `Std. Error`,
#   #     conf.high  = Estimate + 1.96 * `Std. Error`,
#   #     ano_evento = 2007 + k,
#   #     grupo      = factor(grupo, levels = c("Above","Below"))
#   #   ) %>%
#   #   arrange(grupo, k)
#   # 
#   # # adiciona baseline k=0 = 0 para ambos os grupos
#   # event_df <- bind_rows(
#   #   event_df,
#   #   tibble(
#   #     Estimate = 0, `Std. Error` = 0, conf.low = 0, conf.high = 0,
#   #     k = 0, ano_evento = 2007, grupo = factor(c("Above","Below"), levels = c("Above","Below"))
#   #   )
#   # ) %>% arrange(grupo, k)
#   # 
#   # gráfico no mesmo estilo do seu loop
#   p <- ggplot(temp, aes(x = time_to_treat, y = estimate, group = grupo)) +
#     geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
#     geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
#     geom_vline(xintercept = 0, color = "black") +
#     geom_point(aes(color = grupo), shape = 15, size = 2) +
#     geom_line(aes(color = grupo)) +
#     #facet_wrap(~ grupo, ncol = 1) +     # <--- separate plot per group
#     labs(x = "Time to Treat", y = "R$ per capita",
#          color = NULL, fill = NULL) +
#     theme_classic() +
#     theme(
#       axis.line = element_line(color = "grey70"),
#       panel.grid = element_blank(),
#       axis.title = element_text(size = 11),
#       legend.position = c(0.95, 0.95),
#       legend.justification = c("right", "top") # anchor legend box
#     ) + # anchor legend box
#     scale_x_continuous(
#       breaks = seq(min(temp_edu$time_to_treat, na.rm = TRUE),
#                    max(temp_edu$time_to_treat, na.rm = TRUE),
#                    by = 2)
#     )
#   
#   
#   if (model_name == "inf"){
#     p <- p +
#       theme(
#         axis.line = element_line(color = "grey70"),
#         panel.grid = element_blank(),
#         axis.title = element_text(size = 11),
#         legend.position = c(0.95, 0.12),
#         legend.justification = c("right", "top") # anchor legend box
#       ) + # anchor legend box
#       scale_x_continuous(
#         breaks = seq(min(temp_edu$time_to_treat, na.rm = TRUE),
#                      max(temp_edu$time_to_treat, na.rm = TRUE),
#                      by = 2))
#   }
#   
#   print(p)
#   
#   ggsave(
#     filename = paste0("grafico_", model_name, "_AB.png"),
#     plot = p,
#     path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES",
#     width = 600/96, height = 420/96, dpi = 110
#   )
# }
# 
# 
# # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# 8. Depend. in log ----
# ---------------------------------------------------------------------------- #




## 8.1 Log SAEB ----
#Without weights
### 8.1.1 Dosage ----
df_saeb <- df_saeb %>% 
  filter(tipo == "Municipal" &
           rede == "Pública")


mat_5 <- feols(log(vl_nota_5_matematica) ~ dosage * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               #weights = df_saeb$peso_5,
               vcov = "hetero")

port_5 <- feols(log(vl_nota_5_portugues) ~ dosage * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                #weights = df_saeb$peso_5,
                vcov = "hetero")

mat_9 <- feols(log(vl_nota_9_matematica) ~ dosage * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               #weights = df_saeb$peso_9,
               vcov = "hetero")
port_9 <- feols(log(vl_nota_9_portugues) ~ dosage * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                #weights = df_saeb$peso_9,
                vcov = "hetero")



etable(mat_5, port_5, mat_9, port_9, 
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))



etable(mat_5, port_5, mat_9, port_9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage/log_dosage_reg.tex", replace = TRUE)


#### 8.1.1.1 ES ----

# Define uma lista com os objetos dos modelos
models_list <- list(
  mat_5 = mat_5,
  port_5 = port_5,
  mat_9 = mat_9,
  port_9 = port_9
)

# Loop para gerar gráficos para cada modelo
for (model_name in names(models_list)) {
  # Extrair o modelo atual
  current_model <- models_list[[model_name]]
  
  # Extrair a tabela de coeficientes do modelo atual
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
      term = paste0("k::0:", model_name), # Adicionar termo com referência ao modelo
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
      y = "Log Efeito estimado (pontos no SAEB)"
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
    filename = paste0("grafico_", model_name, "log_dosage_saeb.png"), # Nome baseado no modelo
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Dosage",
    width = 600/96, height = 420/96, dpi = 110
  )
  rm(p, model_name)
}

rm(mat_5, mat_9, port_5, port_9)


# ---------------------------------------------------------------------------- #


### 8.1.2 Dosage Aluno----
df_saeb <- df_saeb %>% 
  filter(tipo == "Municipal" &
           rede == "Pública")


mat_5 <- feols(log(vl_nota_5_matematica) ~ aluno_dosage * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               #weights = df_saeb$peso_5,
               vcov = "hetero")

port_5 <- feols(log(vl_nota_5_portugues) ~ aluno_dosage * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                #weights = df_saeb$peso_5,
                vcov = "hetero")

mat_9 <- feols(log(vl_nota_9_matematica) ~ aluno_dosage * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_saeb,
               #weights = df_saeb$peso_9,
               vcov = "hetero")
port_9 <- feols(log(vl_nota_9_portugues) ~ aluno_dosage * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_saeb,
                #weights = df_saeb$peso_9,
                vcov = "hetero")



etable(mat_5, port_5, mat_9, port_9, 
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))



etable(mat_5, port_5, mat_9, port_9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/log_aluno_dosage_reg.tex", replace = TRUE)


#### 8.1.2.1 ES ----

# Define uma lista com os objetos dos modelos
models_list <- list(
  mat_5 = mat_5,
  port_5 = port_5,
  mat_9 = mat_9,
  port_9 = port_9
)

# Loop para gerar gráficos para cada modelo
for (model_name in names(models_list)) {
  # Extrair o modelo atual
  current_model <- models_list[[model_name]]
  
  # Extrair a tabela de coeficientes do modelo atual
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
      term = paste0("k::0:", model_name), # Adicionar termo com referência ao modelo
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
      y = "Log Efeito estimado (pontos no SAEB)"
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
    filename = paste0("grafico_", model_name, "log_dosage_aluno_saeb.png"), # Nome baseado no modelo
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Dosage_aluno",
    width = 600/96, height = 420/96, dpi = 110
  )
  rm(p, model_name)
}

rm(mat_5, mat_9, port_5, port_9, models_list, event_df, current_model)


# ---------------------------------------------------------------------------- #

## 8.2 Log Desp. ----
###8.2.1 Dosage ----

mod_edu <- feols(log( 1 + des_edu_pc) ~ dosage * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_spend,
               #weights = df_saeb$peso_5,
               vcov = "hetero")

mod_fund <- feols(log( 1 + des_fund_pc) ~ dosage * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_spend,
                #weights = df_saeb$peso_5,
                vcov = "hetero")

mod_med <- feols(log( 1 + des_med_pc) ~ dosage * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_spend,
               #weights = df_saeb$peso_9,
               vcov = "hetero")
mod_inf <- feols(log( 1 + des_inf_pc) ~ dosage * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_spend,
                #weights = df_saeb$peso_9,
                vcov = "hetero")



etable(mod_edu, mod_fund, mod_med, mod_inf, 
       vcov = "hetero", 
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)))


etable(mod_edu, mod_fund, mod_med, mod_inf,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage/log_desp_dosage.tex", replace = TRUE)

#### 8.2.1.1 Event Study ----


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
      y = "Log R$ per capita"
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
    filename = paste0("grafico_", model_name, "_log_des_dosage_edu.png"), # Nome baseado no modelo
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Dosage",
    width = 600/96, height = 420/96, dpi = 110
  )
  
}



### 8.2.2 Dosage Aluno ----


mod_edu <- feols(log( 1 + des_edu_pc) ~ aluno_dosage * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 #weights = df_saeb$peso_5,
                 vcov = "hetero")

mod_fund <- feols(log( 1 + des_fund_pc) ~ aluno_dosage * i(k, ref = 0) 
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_spend,
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

mod_med <- feols(log( 1 + des_med_pc) ~ aluno_dosage * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 #weights = df_saeb$peso_9,
                 vcov = "hetero")
mod_inf <- feols(log( 1 + des_inf_pc) ~ aluno_dosage * i(k, ref = 0) 
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_spend,
                 #weights = df_saeb$peso_9,
                 vcov = "hetero")



etable(mod_edu, mod_fund, mod_med, mod_inf, 
       vcov = "hetero", 
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)))


etable(mod_edu, mod_fund, mod_med, mod_inf,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/log_desp_aluno_dosage.tex", replace = TRUE)

#### 8.2.1.1 Event Study ----


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
      y = "Log R$ per capita"
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
    filename = paste0("grafico_", model_name, "_log_des_dosage_aluno_edu.png"), # Nome baseado no modelo
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Dosage_aluno",
    width = 600/96, height = 420/96, dpi = 110
  )
  rm(model_name, p, event_df)
}

rm(mod_edu, mod_fund, mod_inf, mod_med, models_list)

# ---------------------------------------------------------------------------- #
# 9. Above vs. Below ----
# ---------------------------------------------------------------------------- #

df_spend <- df_spend %>%
  mutate(
    grupo = case_when(
      dosage > 0 ~ "Above",   # ganho líquido
      dosage < 0 ~ "Below",   # perda líquida
      TRUE ~ NA_character_
    ),
    grupo = factor(grupo, levels = c("Below", "Above"))
  )


df_spend <- df_spend %>%
  mutate(
    q_dosagem = cut(
      dif_rs_aluno_100,
      breaks = quantile(as.numeric(dif_rs_aluno_100),
                        probs = c(0, .25, .50, .75, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Q1", "Q2", "Q3", "Q4")
    )
  )

## 9.1 Dosage ----
###9.1.1 Nivel ----

mod_edu_ab <- feols(
  des_edu_pc ~ dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)


mod_fund_ab <- feols(
  des_fund_pc ~ dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)

mod_med_ab <- feols(
  des_med_pc ~ dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)

mod_inf_ab <- feols(
  des_inf_pc ~ dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)


etable(mod_edu_ab, mod_fund_ab, mod_med_ab, mod_inf_ab,
       vcov = "hetero",
       headers = list(":_:" = list("Total"=1, "Fundamental"=1, "Médio"=1, "Infantil"=1)))




models_list <- list(
  edu  = mod_edu_ab,
  fund = mod_fund_ab,
  med  = mod_med_ab,
  inf  = mod_inf_ab
)


# 
#  current_model <- models_list[["edu"]]
#  td <- broom::tidy(current_model, conf.int = TRUE) %>%
#    filter(str_starts(term, "abs")) %>%
#    mutate(grupo = as.factor(case_when(
#      str_detect(term, "Above") ~ "Above",
#      str_detect(term, "Below") ~ "Below",
#    )),
#    k = as.numeric(str_sub(term, 26, -14))
#    )
# 
#  print(td)
# 
# 
#  # combina base + d * slope (IC aprox. ignorando covariância entre termos)
#  event_df <- base_df %>%
#    left_join(slope_df, by = c("k","grupo")) %>%
#    mutate(
#      estimate_s = coalesce(estimate_s, 0),
#      se_s       = coalesce(se_s, 0),
#      Estimate   = estimate_b + d_val * estimate_s,
#      `Std. Error` = sqrt(se_b^2 + (d_val * se_s)^2),
#      conf.low   = Estimate - 1.96 * `Std. Error`,
#      conf.high  = Estimate + 1.96 * `Std. Error`,
#      ano_evento = 2007 + k,
#      grupo      = factor(grupo, levels = c("Above","Below"))
#    ) %>%
#    arrange(grupo, k)
# 
#  # adiciona baseline k=0 = 0 para ambos os grupos
#  event_df <- bind_rows(
#    event_df,
#    tibble(
#      Estimate = 0, `Std. Error` = 0, conf.low = 0, conf.high = 0,
#      k = 0, ano_evento = 2007, grupo = factor(c("Above","Below"), levels = c("Above","Below"))
#    )
#  ) %>% arrange(grupo, k)
# 
#  # gráfico no mesmo estilo do seu loop
#  p_facet <- ggplot(temp_edu, aes(x = time_to_treat, y = estimate, group = grupo)) +
#    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
#    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
#    geom_vline(xintercept = 0, color = "black") +
#    geom_point(aes(color = grupo), shape = 15, size = 2) +
#    geom_line(aes(color = grupo)) +
#    #facet_wrap(~ grupo, ncol = 1) +     # <--- separate plot per group
#    labs(x = "Time to Treat", y = "R$ per capita",
#        color = NULL, fill = NULL) +
#    theme_classic() +
#    theme(
#      axis.line = element_line(color = "grey70"),
#      panel.grid = element_blank(),
#      axis.title = element_text(size = 11),
#      legend.position = c(0.95, 0.95),
#      legend.justification = c("right", "top") # anchor legend box
#    ) + # anchor legend box
#    scale_x_continuous(
#      breaks = seq(min(temp_edu$time_to_treat, na.rm = TRUE),
#                   max(temp_edu$time_to_treat, na.rm = TRUE),
#                   by = 2)
#    )
# 
#  p_facet
# 
#  print(p)
# 
#  

#### 9.1.1.1 ES ----



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
  temp <- bind_rows(temp, temp_edu) %>%
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
  
  
  # # termos de inclinação (dose): abs(dif_rs_aluno_100):i(k, grupo)
  # slope_df <- td %>%
  #   dplyr::filter(str_detect(term, "^abs\\(dif_rs_aluno_100\\):k::-?\\d+:grupo::(Above|Below)$")) %>%
  #   transmute(
  #     k = as.numeric(str_extract(term, "-?\\d+(?=:grupo::)")),
  #     grupo = str_match(term, "grupo::(Above|Below)$")[,2],
  #     estimate_s = estimate,
  #     se_s = std.error
  #   )
  # 
  # combina base + d * slope (IC aprox. ignorando covariância entre termos)
  # event_df <- base_df %>%
  #   left_join(slope_df, by = c("k","grupo")) %>%
  #   mutate(
  #     estimate_s = coalesce(estimate_s, 0),
  #     se_s       = coalesce(se_s, 0),
  #     Estimate   = estimate_b + d_val * estimate_s,
  #     `Std. Error` = sqrt(se_b^2 + (d_val * se_s)^2),
  #     conf.low   = Estimate - 1.96 * `Std. Error`,
  #     conf.high  = Estimate + 1.96 * `Std. Error`,
  #     ano_evento = 2007 + k,
  #     grupo      = factor(grupo, levels = c("Above","Below"))
  #   ) %>%
  #   arrange(grupo, k)
  # 
  # # adiciona baseline k=0 = 0 para ambos os grupos
  # event_df <- bind_rows(
  #   event_df,
  #   tibble(
  #     Estimate = 0, `Std. Error` = 0, conf.low = 0, conf.high = 0,
  #     k = 0, ano_evento = 2007, grupo = factor(c("Above","Below"), levels = c("Above","Below"))
  #   )
  # ) %>% arrange(grupo, k)
  # 
  # gráfico no mesmo estilo do seu loop
  p <- ggplot(temp, aes(x = time_to_treat, y = estimate, group = grupo)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 0, color = "black") +
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
    filename = paste0("grafico_", model_name, "_Dos_AB.png"),
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Dosage",
    width = 600/96, height = 420/96, dpi = 110
  )
  rm(temp_edu)
}


### 9.1.2 Log -------

mod_edu_ab <- feols(
  log(1 + des_edu_pc) ~ dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)


mod_fund_ab <- feols(
  log(1 + des_fund_pc) ~ dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)

mod_med_ab <- feols(
  log(1 + des_med_pc) ~ dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)

mod_inf_ab <- feols(
  log(1 + des_inf_pc) ~ dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)


etable(mod_edu_ab, mod_fund_ab, mod_med_ab, mod_inf_ab,
       vcov = "hetero",
       headers = list(":_:" = list("Total"=1, "Fundamental"=1, "Médio"=1, "Infantil"=1)))




models_list <- list(
  edu  = mod_edu_ab,
  fund = mod_fund_ab,
  med  = mod_med_ab,
  inf  = mod_inf_ab
)


# 
#  current_model <- models_list[["edu"]]
#  td <- broom::tidy(current_model, conf.int = TRUE) %>%
#    filter(str_starts(term, "abs")) %>%
#    mutate(grupo = as.factor(case_when(
#      str_detect(term, "Above") ~ "Above",
#      str_detect(term, "Below") ~ "Below",
#    )),
#    k = as.numeric(str_sub(term, 26, -14))
#    )
# 
#  print(td)
# 
# 
#  # combina base + d * slope (IC aprox. ignorando covariância entre termos)
#  event_df <- base_df %>%
#    left_join(slope_df, by = c("k","grupo")) %>%
#    mutate(
#      estimate_s = coalesce(estimate_s, 0),
#      se_s       = coalesce(se_s, 0),
#      Estimate   = estimate_b + d_val * estimate_s,
#      `Std. Error` = sqrt(se_b^2 + (d_val * se_s)^2),
#      conf.low   = Estimate - 1.96 * `Std. Error`,
#      conf.high  = Estimate + 1.96 * `Std. Error`,
#      ano_evento = 2007 + k,
#      grupo      = factor(grupo, levels = c("Above","Below"))
#    ) %>%
#    arrange(grupo, k)
# 
#  # adiciona baseline k=0 = 0 para ambos os grupos
#  event_df <- bind_rows(
#    event_df,
#    tibble(
#      Estimate = 0, `Std. Error` = 0, conf.low = 0, conf.high = 0,
#      k = 0, ano_evento = 2007, grupo = factor(c("Above","Below"), levels = c("Above","Below"))
#    )
#  ) %>% arrange(grupo, k)
# 
#  # gráfico no mesmo estilo do seu loop
#  p_facet <- ggplot(temp_edu, aes(x = time_to_treat, y = estimate, group = grupo)) +
#    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
#    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
#    geom_vline(xintercept = 0, color = "black") +
#    geom_point(aes(color = grupo), shape = 15, size = 2) +
#    geom_line(aes(color = grupo)) +
#    #facet_wrap(~ grupo, ncol = 1) +     # <--- separate plot per group
#    labs(x = "Time to Treat", y = "R$ per capita",
#        color = NULL, fill = NULL) +
#    theme_classic() +
#    theme(
#      axis.line = element_line(color = "grey70"),
#      panel.grid = element_blank(),
#      axis.title = element_text(size = 11),
#      legend.position = c(0.95, 0.95),
#      legend.justification = c("right", "top") # anchor legend box
#    ) + # anchor legend box
#    scale_x_continuous(
#      breaks = seq(min(temp_edu$time_to_treat, na.rm = TRUE),
#                   max(temp_edu$time_to_treat, na.rm = TRUE),
#                   by = 2)
#    )
# 
#  p_facet
# 
#  print(p)
# 
#  

#### 9.1.2.1 ES ----



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
  temp <- bind_rows(temp, temp_edu) %>%
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
  

  p <- ggplot(temp, aes(x = time_to_treat, y = estimate, group = grupo)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = grupo), shape = 15, size = 2) +
    geom_line(aes(color = grupo)) +
    #facet_wrap(~ grupo, ncol = 1) +     # <--- separate plot per group
    labs(x = "Time to Treat", y = "Log R$ per capita",
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
        breaks = seq(min(temp$time_to_treat, na.rm = TRUE),
                     max(temp$time_to_treat, na.rm = TRUE),
                     by = 2))
 
  
  print(p)
  
  ggsave(
    filename = paste0("grafico_", model_name, "_log_Dos_AB.png"),
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Dosage",
    width = 600/96, height = 420/96, dpi = 110
  )
  rm(temp_edu)
}



rm(list = ls());gc()
# ---------------------------------------------------------------------------- #



## 9.2 Dosage Aluno ----
###9.2.1 Nivel ----

mod_edu_ab <- feols(
  des_edu_pc ~ aluno_dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)


mod_fund_ab <- feols(
  des_fund_pc ~ aluno_dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)

mod_med_ab <- feols(
  des_med_pc ~ aluno_dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)

mod_inf_ab <- feols(
  des_inf_pc ~ aluno_dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)


etable(mod_edu_ab, mod_fund_ab, mod_med_ab, mod_inf_ab,
       vcov = "hetero",
       headers = list(":_:" = list("Total"=1, "Fundamental"=1, "Médio"=1, "Infantil"=1)))





#### 9.2.1.1 ES ----

models_list <- list(
  edu  = mod_edu_ab,
  fund = mod_fund_ab,
  med  = mod_med_ab,
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
  temp <- bind_rows(temp, temp_edu) %>%
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
  
  
  p <- ggplot(temp, aes(x = time_to_treat, y = estimate, group = grupo)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 0, color = "black") +
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
    filename = paste0("grafico_", model_name, "_Dos_aluno_AB.png"),
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Dosage_aluno",
    width = 600/96, height = 420/96, dpi = 110
  )
  rm(temp_edu)
}


### 9.2.2 Log -------

mod_edu_ab <- feols(
  log(1 + des_edu_pc) ~ aluno_dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)


mod_fund_ab <- feols(
  log(1 + des_fund_pc) ~ aluno_dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)

mod_med_ab <- feols(
  log(1 + des_med_pc) ~ aluno_dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)

mod_inf_ab <- feols(
  log(1 + des_inf_pc) ~ aluno_dosage * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_spend, vcov = "hetero"
)


etable(mod_edu_ab, mod_fund_ab, mod_med_ab, mod_inf_ab,
       vcov = "hetero",
       headers = list(":_:" = list("Total"=1, "Fundamental"=1, "Médio"=1, "Infantil"=1)))






#### 9.2.2.1 ES ----

models_list <- list(
  edu  = mod_edu_ab,
  fund = mod_fund_ab,
  med  = mod_med_ab,
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
  temp <- bind_rows(temp, temp_edu) %>%
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
  

  p <- ggplot(temp, aes(x = time_to_treat, y = estimate, group = grupo)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = grupo), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = grupo), shape = 15, size = 2) +
    geom_line(aes(color = grupo)) +
    #facet_wrap(~ grupo, ncol = 1) +     # <--- separate plot per group
    labs(x = "Time to Treat", y = "Log R$ per capita",
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
    ) +
    theme(
      legend.background = element_blank(),
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11),
      legend.position = c(0.14, 0.17),
      legend.justification = c("right", "top") # anchor legend box
    ) + # anchor legend box
    scale_x_continuous(
      breaks = seq(min(temp$time_to_treat, na.rm = TRUE),
                   max(temp$time_to_treat, na.rm = TRUE),
                   by = 2))
  
  
  print(p)
  
  ggsave(
    filename = paste0("grafico_", model_name, "_log_Dos_aluno_AB.png"),
    plot = p,
    path = "Z:/Tuffy/Paper - Educ/Resultados/Figuras/ES/Dosage_aluno",
    width = 600/96, height = 420/96, dpi = 110
  )
  
  rm(temp_edu, model_name, p, temp)

  }



rm(list = ls());gc()


# ---------------------------------------------------------------------------- #
# 10. UF ----
# ---------------------------------------------------------------------------- #
## 10.1 Bases ----
df_sim <- readRDS("Z:/Tuffy/Paper - Educ/Dados/simulacao_const.rds") #%>% 

df_uf_sim <- df_sim %>% 
  filter(nome == "GOVERNO ESTADUAL")

df_trn <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/painel_notas_transferencias_2000_2024.csv")

df_uf_trn <- df_trn %>% 
  filter(codigo_ibge %in% c(1:100))



#Vamos agregar as notas 
df_uf_saeb <- df_trn %>% 
  filter(!codigo_ibge %in% c(1:100),
         rede == "Estadual") %>% 
  mutate(codigo_ibge = codigo_ibge %/% 100000) %>% 
  select(-X) %>% 
  group_by(codigo_ibge,uf, ano) %>% 
  summarise(vl_nota_5_mat = mean(vl_nota_5_matematica, na.rm = T),
            vl_nota_5_pot = mean(vl_nota_5_portugues, na.rm = T),
            vl_nota_9_mat = mean(vl_nota_9_matematica, na.rm = T),
            vl_nota_9_pot = mean(vl_nota_9_portugues, na.rm = T),
            
            .groups = "drop") 

df_uf_saeb <- na.omit(df_uf_saeb)

#Calculando a dosage e unindo as bases

df_uf <- df_uf_sim %>% 
  select(-ano) %>% 
  mutate(dosage = (receita_real - receita_simulada)/receita_real)

df_uf_saeb <- left_join(df_uf_saeb, df_uf,
                        by = c("codigo_ibge" = "codigo_ibge", "uf" = "uf"))





pib <- read_excel("Z:/Tuffy/Paper - Educ/Dados/PIB dos Municípios - base de dados 2002-2009.xls") %>% 
  filter(Ano >= 2005) %>% 
  select(1, 7, 8, 40)


pib2 <- read_excel("Z:/Tuffy/Paper - Educ/Dados/PIB dos Municípios - base de dados 2010-2021.xlsx") %>% 
  select(1, 7, 8, 40)

colnames(pib) <- c("ano", "codigo_ibge", "nom", "PIBpc")
colnames(pib2) <- c("ano", "codigo_ibge", "nom", "PIBpc")


pib <- bind_rows(
  pib,
  pib2) %>% 
  mutate(codigo_ibge = as.numeric(str_sub(as.character(codigo_ibge), 1, -2)))

rm(pib2)

df_fib <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/Gastos municipais/FINBRA/Despesas/FINBRA_EDU_05_21.csv")

df_fib <- left_join(df_fib, pib,
                    by = c("codigo_ibge", "ano"))



df_fib <- df_fib %>%
  group_by(codigo_ibge, ano) %>% 
  mutate(PIBmun = PIBpc * populacao) %>% 
  ungroup() %>% 
  mutate(codigo_ibge = codigo_ibge %/% 10000) %>% 
  group_by(ano, codigo_ibge) %>% 
  summarise(PIBuf = sum(PIBmun, na.rm = T),
            popuf = sum(populacao, na.rm = T),
            .groups = "drop") %>% 
  mutate(Pibpc = PIBuf/popuf)

# Unindo tudo

df_uf_saeb <- left_join(df_uf_saeb, df_fib,
                        by = c("ano", "codigo_ibge"))

df_uf_saeb$k <- df_uf_saeb$ano - 2007
df_uf_saeb$dif_rs_aluno_100 <- df_uf_saeb$dif_rs_aluno / 100


rm(df_fib, df_sim, df_trn, pib, df_uf_sim, df_uf_trn, df_uf)

## 10.2 Regression ----
### 10.2.1 Diff coef ----

mat_5 <- feols(vl_nota_5_mat ~ dif_coef_pp * i(k, ref = 0)
               + Pibpc
               |  ano ,
               data = df_uf_saeb,
               #weights = df_saeb$peso_5,
               vcov = "hetero")

port_5 <- feols(vl_nota_5_pot ~ dif_coef_pp * i(k, ref = 0) 
                + Pibpc
                |  ano ,
                data = df_uf_saeb,
                #weights = df_saeb$peso_5,
                vcov = "hetero")

mat_9 <- feols(vl_nota_9_mat ~ dif_coef_pp * i(k, ref = 0)
               + Pibpc
               |  ano ,
               data = df_uf_saeb,
               #weights = df_saeb$peso_9,
               vcov = "hetero")
port_9 <- feols(vl_nota_9_pot ~ dif_coef_pp * i(k, ref = 0) 
                + Pibpc
                |  ano ,
                data = df_uf_saeb,
                #weights = df_saeb$peso_9,
                vcov = "hetero")



etable(mat_5, port_5, mat_9, port_9, 
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))



etable(mat_5, port_5, mat_9, port_9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Regressões/reg_uf.tex", replace = TRUE)


#anul <- anti_join(df_reg, df_saeb)
#all.equal(df_reg, df_saeb)
### 10.2.2 Diff Aluno ----

mat_5 <- feols(vl_nota_5_mat ~ dif_rs_aluno_100 * i(k, ref = 0)                
               + Pibpc
               |  ano ,
               data = df_uf_saeb,
               #weights = df_saeb$peso_5,
               vcov = "hetero")

port_5 <- feols(vl_nota_5_pot ~ dif_rs_aluno_100 * i(k, ref = 0)
                + Pibpc
                |  ano ,
                data = df_uf_saeb,
                #weights = df_saeb$peso_5,
                vcov = "hetero")


mat_9 <- feols(vl_nota_9_mat ~ dif_rs_aluno_100 * i(k, ref = 0)
               + Pibpc
               |  ano ,
               data = df_uf_saeb,
               #weights = df_saeb$peso_9,
               vcov = "hetero")
port_9 <- feols(vl_nota_9_pot ~ dif_rs_aluno_100 * i(k, ref = 0) 
                + Pibpc
                |  ano ,
                data = df_uf_saeb,
                #weights = df_saeb$peso_9,
                vcov = "hetero")



etable(mat_5, port_5, mat_9, port_9, 
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))


etable(mat_5, port_5, mat_9, port_9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Regressões/reg_rs_aluno_uf.tex", replace = TRUE)

###10.2.3 Dosage ----
mat_5 <- feols(vl_nota_5_mat ~ dosage * i(k, ref = 0)
              + Pibpc
              |  ano ,
              data = df_uf_saeb,
              #weights = df_saeb$peso_5,
              vcov = "hetero")

port_5 <- feols(vl_nota_5_pot ~ dosage * i(k, ref = 0) 
                + Pibpc
                |  ano ,
                data = df_uf_saeb,
                #weights = df_saeb$peso_5,
                vcov = "hetero")

mat_9 <- feols(vl_nota_9_mat ~ dosage * i(k, ref = 0)
               + Pibpc
               |  ano ,
               data = df_uf_saeb,
               #weights = df_saeb$peso_9,
               vcov = "hetero")
port_9 <- feols(vl_nota_9_pot ~ dosage * i(k, ref = 0) 
                + Pibpc
                |  ano ,
                data = df_uf_saeb,
                #weights = df_saeb$peso_9,
                vcov = "hetero")



etable(mat_5, port_5, mat_9, port_9, 
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))



etable(mat_5, port_5, mat_9, port_9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Regressões/dosage_reg_uf.tex", replace = TRUE)

