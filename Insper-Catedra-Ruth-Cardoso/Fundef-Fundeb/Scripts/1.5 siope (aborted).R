# ---------------------------------------------------------------------------- #
# SIOPE API
# DataBase adjustment
# Last edited by: Tuffy Licciardi Issa
# Date: 11/11/2025
# ---------------------------------------------------------------------------- #

#'********************************************************************************
#' [DISCLAIMER]
#' The following code was aborted due to lack of data. Howver, the extraction code
#' is provided for future needed data extractions. Thus, steps taken in this docu-
#' ment is not necessary for the main analysis.
#' ********************************************************************************



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
library(readr)

#Desativando a notação científica
options(scipen = 999)



# ---------------------------------------------------------------------------- #
# 1. API SIOPE ----
# ---------------------------------------------------------------------------- #

#' I will download the SIOPE spendure files to further analyse the municipal spending
#' in daycare and preschool.


## 1.1 Download ----

# Folder to save files
pasta_destino <- "Z:/Tuffy/Paper - Educ/Dados/SIOPE"
if (!dir.exists(pasta_destino)) dir.create(pasta_destino, recursive = TRUE)

# Vector of UFs
ufs <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA",
         "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN",
         "RO","RR","RS","SC","SE","SP","TO")

# Function for period
f_periodo <- function(ano) ifelse(ano <= 2016, 1, 6)

# Error log
falhas <- list()


# Loop through years and states
for (ano in 2000:2024) {
  for (uf in ufs) {
    
    periodo <- f_periodo(ano)
    
    url <- glue(
      "https://www.fnde.gov.br/olinda-ide/servico/DADOS_ABERTOS_SIOPE/versao/v1/odata/",
      "Despesas_Funcao_Educacao_Siope(Ano_Consulta=@Ano_Consulta,Num_Peri=@Num_Peri,Sig_UF=@Sig_UF)",
      "?@Ano_Consulta={ano}&@Num_Peri={periodo}&@Sig_UF='{uf}'",
      "&$format=text/csv"
    )
    
    caminho_arquivo <- file.path(pasta_destino, glue("siope_despesas_{uf}_{ano}_P{periodo}.csv"))
    
    resultado <- try({
      resp <- request(url) |> req_perform()
      writeBin(resp$body, caminho_arquivo)
      message(glue("✔️ {uf}-{ano} baixado com sucesso."))
      TRUE
    }, silent = TRUE)
    
    if (inherits(resultado, "try-error") || is.logical(resultado) && !resultado) {
      falhas <- append(falhas, list(tibble(uf = uf, ano = ano, periodo = periodo)))
      message(glue("⚠️ Falha em {uf}-{ano}."))
    }
  }
  rm(resp, uf, url, ano, periodo, resultado)
}

# Combine failure log
log_erros <- bind_rows(falhas)

rm(falhas, log_erros, caminho_arquivo, pasta_destino)
# ------------------ #
## 1.2 Transformation ----
# ------------------ #

arquivos <- list.files(path = pasta_destino, pattern = "\\.csv$", full.names = TRUE)
length(arquivos)  # shows how many files you have


for(i in c(1:length(arquivos))){
  
  
  if(i == 1) {
    ini1 <- Sys.time()
  }
  
  ini <- Sys.time()
  
  message("Now oppening ", i,"/",length(arquivos))
  
  temp <- read_csv(arquivos[i]) %>% 
    rename_all(tolower) %>% 
    filter(tipo == "Municipal",
           des_subf %in% c("365 - Educação Infantil (Creche)",
                           "365 - Educação Infantil (Pré-Escola)")) %>% 
    select(-c(num_peri, num_orde))
  
  
  if(i == 1) {
    
    df_final <- temp
  } else {
    
    df_final <- rbind(df_final, temp)
    
  }
  
  
  fim <- Sys.time()
  
  
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  
  message("Time elapsed for ", i,": ",mins," mins e ", secs, " s")
  message("---------------------------------------------")
  
  rm(ini, fim, delta, mins, secs, temp)
  
  
  
  
  #Total time runnig
  if(i == length(arquivos)) {
    fim1 <- Sys.time()
    
    delta <- difftime(fim1, ini1, units = "secs")
    mins <- floor(as.numeric(delta) / 60)
    secs <- round(as.numeric(delta) %% 60)
    
    message("---------------------------------------------")
    message("Total time elapsed: ",mins," mins e ", secs, " s")
    message("---------------------------------------------")
    
    rm(fim1, ini1, delta, mins, secs)
  }
  
  rm(i)
  
}

#' ********************************************************************************
#'  The problem faced beyond this point is that the separation of spending within
#'daycare and preschool only begins in 2010, which undermines the desired analysis
#'over the impacts of the FUNDEB (2008) on such variables. Thus this the next
#'steps of this code were aborted. The next steps would be deflate the observed
#'spending value and save as a supporting dataframe.
