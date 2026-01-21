# ---------------------------------------------------------------------------- #
# Data Description
# DataBase adjustment
# Last edited by: Tuffy Licciardi Issa
# Date: 21/01/2025
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


#Desativando a notação científica
options(scipen = 999)


#Func de label


add_label <- function(data, variable, label) {
  
  if(!variable %in% names(data)) {
    stop("Erro! Não há a variável especificada.")
  }
  
  #Label
  attr(data[[variable]], "label") <- label
  return(data)
}


# ---------------------------------------------------------------------------- #
# Dados ----
# ---------------------------------------------------------------------------- #
#1. FINBRA ----
## 1.1. 2005-2012 ----
amcs <- read_dta("Z:/Tuffy/Paper - Brasil/amcs.dta")

for (year in 2005:2012) {
  
  if(year == 2005) {
    finbra_2005_2012 <- data.frame()
  }
  
  filename <- paste0("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/Gastos municipais/FINBRA/Despesas/finbra_", year, ".xlsx")
  
  finbra_ano <- read_excel(filename) %>%
    mutate(ano = year) %>% 
    select(ano, everything())
  
  if (is.null(finbra_2005_2012) == TRUE) {
    finbra_2005_2012 <- finbra_ano
  } 
  else {
    finbra_2005_2012 <- bind_rows(finbra_2005_2012, finbra_ano)
  }
  
  message(paste("Ano", year, "- importação concluída."))
  print(colnames(finbra_ano))
  rm(finbra_ano, filename, year)
}


finbra_2005_2012 <- finbra_2005_2012 %>% 
  mutate(
    codmun = as.numeric(CdUF)*10000 + as.numeric(CdMun)
  ) %>% 
  relocate(MUNICIPIO, .after = "Nome Caixa") %>% 
  mutate(NomMun = toupper(coalesce(`Nome Caixa`, MUNICIPIO))) %>% 
  select(-`Nome Caixa`, -MUNICIPIO) %>% 
  relocate(NomMun, .after = UF) %>% 
  relocate(codmun, .after = CdMun)


#Filtrando as colunas
finbra_2005_2012 <- finbra_2005_2012 %>% 
  select(-c(`Legislativa`:`Demais Subfunções 11`,`Cultura`:`Demais Subfunções 28`,ncol(finbra_2005_2012),ncol(finbra_2005_2012)-1)) %>% 
  arrange(ano, codmun)


saveRDS(finbra_2005_2012, "Z:/Tuffy/Paper - Educ/Dados/finbra_2005_2012.rds")


## 1.2 2013-2021 ----

for (year in 2013:2021) {
  
  if (year == 2013) { finbra_novo <- data.frame() }
  
  
  filename <- paste0("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/Gastos municipais/FINBRA/Despesas/finbra_",year, ".csv")
  
  # Lê o arquivo CSV com a codificação apropriada
  finbra_ano <- read.csv2(filename,
                          skip = 3,
                          fileEncoding = "latin1") %>% 
    mutate(ano = year,
           NomMun = Instituição %>% 
             str_remove("Prefeitura Municipal de ") %>%
             str_sub(end = -6)
    ) %>% 
    relocate(NomMun, .after = "Instituição") %>% 
    select(-c(Instituição, Identificador.da.Conta)) %>% 
    filter(str_starts(Conta, "12") | str_starts(Conta, "FU12") |
             Conta %in% c(
               "Despesas (Exceto Intra-Orçamentárias)",
               "Despesas (Exceto Intraorçamentárias)",
               "Despesas Exceto Intraorçamentárias")
    ) %>% 
    pivot_wider(
      id_cols = c(NomMun:Coluna, ano),
      names_from = Conta,
      values_from = Valor)
  
  if (is.null(finbra_novo)) {
    finbra_novo <- finbra_ano
  } 
  else {
    finbra_novo <- bind_rows(finbra_novo, finbra_ano)
  }
  
  message(paste("Ano", year, "- importação concluída."))
  rm(finbra_ano, filename, year)
}


colnames(finbra_novo) <- c("nom_mun", "codigo_ibge", "uf", "populacao", "Coluna", "ano", "Despesas (Exceto Intra-Orçamentárias)", "Educação", "Ensino Fundamental",
                           "Educação Infantil", "Ensino Médio", "Ensino Superior", "Demais Subfunções 12", "Educação de Jovens e Adultos", 
                           "Educação Especial", "Educação Básica", "Ensino Profissional", "Despesas (Exceto Intraorçamentárias)", "Administração Geral",
                           "Despesas Exceto Intraorçamentárias", "FU12")


finbra_novo <- finbra_novo %>% 
  mutate(
    despesas_totais = coalesce(`Despesas (Exceto Intra-Orçamentárias)`, `Despesas (Exceto Intraorçamentárias)`, `Despesas Exceto Intraorçamentárias`),
    `Demais Subfunções 12` = coalesce(`Demais Subfunções 12`, `FU12`)
  ) %>% 
  clean_names() %>% 
  select(-c(administracao_geral, educacao_basica, starts_with("despesas_exceto"), fu12)) %>% 
  mutate(codigo_ibge = as.numeric(str_sub(as.character(codigo_ibge), 1, -2)))



saveRDS(finbra_novo, "Z:/Tuffy/Paper - Educ/Dados/finbra_2007_2021.rds")




#2. Transf. Fundef-Fundeb ----
## 2.1 Rede Mun ----

caminho1 <- "Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/Transferências Fundeb mun e ufs/transferências_para_municípios_2000_2006.csv"
caminho2 <- "Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/Transferências Fundeb mun e ufs/transferências_para_municípios_2007_2021.csv"

df_anual <- read_csv2(caminho1, locale = locale(encoding = "latin1")) %>%
  clean_names() %>% 
  bind_rows(read_csv2(caminho2, locale = locale(encoding = "latin1")) %>%
              clean_names()) %>% 
  arrange(codigo_ibge, ano) %>% 
  select(-codigo_siafi)

colnames(df_anual)

df_anual <- df_anual %>% 
  group_by(codigo_ibge, ano) %>%
  summarise(
    uf = first(uf),
    nome = first(municipio),
    total_politica = sum(`valor_consolidado`, na.rm = TRUE),
    total_coun = sum(`valor_consolidado`[transferencia %in% c("FUNDEF - COUN", "FUNDEB - COUN", "FUNDEB - COUN VAAT",
                                                              "FUNDEB - COUN VAAF", "AJUSTE FUNDEB - AJUSTE FUNDEB VAAF",
                                                              "AJUSTE FUNDEB - AJUSTE FUNDEB VAAT")], na.rm = TRUE),
    .groups = "drop")


summary(df_anual)

###1.3.2. Rede Estadual

df_ufs <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/Transferências Fundeb mun e ufs/transferências_para_estados_2007_2021.csv",
                    fileEncoding = "latin1") %>% 
  clean_names() %>% 
  left_join(read_excel("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/Códigos_UF.xlsx"), by = "uf") %>% 
  mutate(codigo_ibge = as.numeric(codigo_ibge)) %>% 
  group_by(codigo_ibge, ano) %>%
  summarise(
    uf = first(uf),
    nome = first(nm_uf),
    total_politica = sum(`valor_consolidado`, na.rm = TRUE),
    total_coun = sum(`valor_consolidado`[transferencia %in% c("FUNDEF - COUN", "FUNDEB - COUN", "FUNDEB - COUN VAAT",
                                                              "FUNDEB - COUN VAAF", "AJUSTE FUNDEB - AJUSTE FUNDEB VAAF",
                                                              "AJUSTE FUNDEB - AJUSTE FUNDEB VAAT")], na.rm = TRUE),
    .groups = "drop")





#Juntando as duas bases de dados
df <- bind_rows(
  df_anual,
  df_ufs)

#Salvando
saveRDS(df, "Z:/Tuffy/Paper - Educ/Dados/fundef_fundeb_2000_2001.rds")

#Para deflacionar os valores:
df_ipca <- read_excel("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/IPCA_acumulado_ano.xlsx", skip = 3)
colnames(df_ipca) = c("ano", "ipca")

df_ipca <- df_ipca %>% 
  mutate(ano = as.numeric(ano)) %>% 
  filter(ano %in% 2000:2021) %>%
  arrange(ano)


df_ipca <- df_ipca %>% 
  mutate(
    ipca = as.numeric(gsub(",",".",ipca)),
    indice = cumprod(1 + as.numeric(ipca)/100)) %>% 
  mutate(indice = indice / indice[ano == 2021])



df <- df %>% 
  left_join(df_ipca, by = "ano") %>% 
  mutate(
    total_politica_d = total_politica / indice,
    total_coun_d = total_coun / indice
  )

saveRDS(df, "Z:/Tuffy/Paper - Educ/Dados/defla_fundef_fundeb_2000_2001.rds")

rm(df_ipca, df_anual, df_ufs, caminho1, caminho2)

# ---------------------------------------------------------------------------- #
# 3. SAEB ----
# ---------------------------------------------------------------------------- #

ciclos <- c("anos_iniciais", 
            "anos_finais",
            "ensino_medio")


for (ciclo in ciclos){
  
  if (ciclo == "anos_iniciais") {
    df_apr = NULL
    df_saeb = NULL
  }
  
  
  ideb <- read_excel(paste0("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/IDEB/divulgacao_", ciclo, "_municipios_2023/divulgacao_", ciclo, "_municipios_2023/divulgacao_", ciclo, "_municipios_2023.xlsx"),
                     skip = 9) %>% 
    clean_names() %>%
    #select(-c(starts_with("vl_indicador_"), starts_with("vl_observado_"), starts_with("vl_projecao"))) %>% 
    rename(codigo_ibge = "co_municipio",
           nome = "no_municipio",
           uf = "sg_uf")
  
  # Aprovação:
  apr <- ideb %>%
    select(c(1:4, starts_with("vl_aprovacao_")))
  
  #print(colnames(apr))
  
  apr <- apr %>%
    pivot_longer(
      cols = matches("^vl_aprovacao_\\d{4}_(si_4|si|[1-4])$"),
      names_to = c("ano", "serie"),
      names_pattern = "vl_aprovacao_(\\d{4})_(si_4|si|[1-4])",
      values_to = "tx_aprov"
    ) %>%
    mutate(
      ano = as.integer(ano),
      serie_label = case_match(
        serie,
        "si_4" ~ ifelse(ciclo == "anos_iniciais", "iniciais",
                        ifelse(ciclo == "anos_finais", "finais", "em")),
        "si"   ~ "1",     # Só ocorre em anos_iniciais
        "1"    ~ ifelse(ciclo == "anos_iniciais", "2",
                        ifelse(ciclo == "anos_finais", "6", "1em")),
        "2"    ~ ifelse(ciclo == "anos_iniciais", "3",
                        ifelse(ciclo == "anos_finais", "7", "2em")),
        "3"    ~ ifelse(ciclo == "anos_iniciais", "4",
                        ifelse(ciclo == "anos_finais", "8", "3em")),
        "4"    ~ ifelse(ciclo == "anos_iniciais", "5",
                        ifelse(ciclo == "anos_finais", "9", "4em"))
      )
    ) %>% 
    select(uf, codigo_ibge, nome, rede, ano, serie_label, tx_aprov) %>% 
    pivot_wider(
      names_from = serie_label,
      names_prefix = "tx_aprovacao_",
      values_from = tx_aprov
    ) %>%
    mutate(across(
      starts_with("tx_aprovacao_"),
      ~round(as.numeric(.), 1)
    ))
  
  if (is.null(df_apr)) {
    df_apr <- apr
  } else {
    df_apr <- full_join(df_apr, apr, by = c("codigo_ibge", "uf", "nome", "rede", "ano"))
  }
  
  
  saeb <- ideb %>%
    select(c(1:4, starts_with("vl_nota_")))
  
  saeb <- saeb %>%
    mutate(across(starts_with("vl_nota_"), ~ round(as.numeric(.), 2))) %>%
    pivot_longer(
      cols = matches("^vl_nota_(media|matematica|portugues)_\\d{4}$"),
      names_to = c("indicador", "ano"),
      names_pattern = "vl_nota_(media|matematica|portugues)_(\\d{4})",
      values_to = "valor"
    ) %>%
    mutate(
      ano = as.integer(ano),
      valor = as.numeric(valor)
    ) %>%
    pivot_wider(
      id_cols = c(uf, codigo_ibge, nome, rede, ano),
      names_from = indicador,
      names_prefix = paste0("vl_nota_", ifelse(ciclo == "anos_iniciais", "5", ifelse(ciclo == "anos_finais", "9", "em")), "_"),
      values_from = valor
    )
  
  
  if (is.null(df_saeb)) {
    df_saeb <- saeb
  } else {
    df_saeb <- full_join(df_saeb, saeb, by = c("uf", "codigo_ibge", "nome", "rede", "ano"))
  }
  
  
  message("Finalizado para ", ciclo)
  rm(saeb, ideb, apr)
}  


df_ideb <- full_join(df_saeb, df_apr,
                     by = c("codigo_ibge", "rede", "uf", "ano", "nome"))

df_nota <- left_join(df, df_ideb,
                     by = c("ano", "codigo_ibge")) %>% 
  rename(uf = "uf.x",
         nome = "nome.x") %>% 
  select(-c(uf.y, nome.y))

rm(df_apr, df_saeb, df_ideb, ciclo, ciclos)


# 4. Transferencias ----

## 4.1 Notas do SAEB + Taxa de Aprovação (faltantes):----

# Como o IDEB foi criado em 2007, as notas do Saeb e as Taxas de Aprovação só estão disponíveis dessa forma para 2005 em diante.
# Então, temos que colocar a Tx. de Aprovação manualmente de 2000 a 2004 para o EF, e de 2000 a 2016 para o EM.
# Para as notas do Saeb, precisamos dos dados de 2001 e 2003 para tendências paralelas;
# Por enquanto, deixarem sem essas informações.




## 4.2 Transferências do Fundeb: ----

# rm(list =ls())
# siope <- read.csv2("C:/Users/giovannioz/Downloads/finbraRREO_MUNEST_Previsaoatualizadaexercicio/finbraRREO.csv", fileEncoding = "latin1", skip = 5) %>% 
#   filter(Cod.IBGE == 1200203) %>% 
#          # Conta %in% c("Dedução de Receita para Formação do FUNDEB", "Transferências do FUNDEB")) %>% 
#   mutate(Valor = format(round(Valor, 2), big.mark = ".", decimal.mark = ","),)
# 
# unique(siope$Coluna)
# 
# write_xlsx(siope, "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/Transferências Fundeb mun e ufs/teste_siconfi.xlsx")
# 
# 
# 
# 
# 
# siope_pda <- read.csv("C:/Users/giovannioz/Downloads/Receita_Siope (2).csv",
#                   fileEncoding = "UTF-8")
# 
# 
# unique(teste$NOM_ITEM)
# 
# teste <- siope_pda %>% 
#   filter(
#     NOM_MUNI == "Cruzeiro do Sul",
#     NOM_COLU == "Deduções FUNDEB" |
#       str_starts(NOM_ITEM, "Transferências de Recursos do Fundo de Manutenção e Desenvolvimento da Educação Básica")
#   ) %>% 
#   mutate(
#     VAL_DECL = format(round(as.numeric(VAL_DECL), 2), big.mark = ".", decimal.mark = ",")
#   )
# 
# 
# unique(siope_pda$NOM_COLU)



### 4.2.1 Baixando as planilhas pelo Portal de Dados Abertos do FNDE (RODAR UMA VEZ SÓ): ----


# # Pasta onde salvar os arquivos
# pasta_destino <- "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/SIOPE"
# if (!dir.exists(pasta_destino)) dir.create(pasta_destino, recursive = TRUE)
# 
# # Vetor de UFs
# ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
#          "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN",
#          "RO", "RR", "RS", "SC", "SE", "SP", "TO")
# 
# # Função para obter o período correto
# f_periodo <- function(ano) ifelse(ano <= 2016, 1, 6)
# 
# # Lista para registrar falhas
# falhas <- list()
# 
# # Loop
# for (ano in 2000:2024) {
#   for (uf in ufs) {
#     
#     periodo <- f_periodo(ano)
#     
#     url <- glue(
#       "https://www.fnde.gov.br/olinda-ide/servico/DADOS_ABERTOS_SIOPE/versao/v1/odata/",
#       "Receita_Siope(Ano_Consulta=@Ano_Consulta,Num_Peri=@Num_Peri,Sig_UF=@Sig_UF)",
#       "?@Ano_Consulta={ano}&@Num_Peri={periodo}&@Sig_UF='{uf}'",
#       "&$format=text/csv",
#       "&$select=TIPO,NUM_ANO,NUM_PERI,COD_UF,SIG_UF,COD_MUNI,NOM_MUNI,",
#       "COD_EXIB_FORMATADO,NOM_ITEM,IDN_CLAS,NOM_COLU,NUM_NIVE,NUM_ORDE,VAL_DECL"
#     )
#     
#     caminho_arquivo <- file.path(pasta_destino, glue("siope_{uf}_{ano}_P{periodo}.csv"))
#     
#     # Tenta baixar e salvar
#     resultado <- try({
#       resp <- request(url) |> req_perform()
#       writeBin(resp$body, caminho_arquivo)
#       message(glue("✔️ {uf}-{ano} baixado com sucesso."))
#       TRUE
#     }, silent = TRUE)
#     
#     # Se falhou, salva no log de erros
#     if (inherits(resultado, "try-error") || is.logical(resultado) && !resultado) {
#       falhas <- append(falhas, list(tibble(uf = uf, ano = ano, periodo = periodo)))
#       message(glue("⚠️ Falha em {uf}-{ano}."))
#     }
#   }
#   rm(resp, uf, url, ano, periodo, resultado)
# }
# 
# 
# # Juntar falhas em uma tabela
# log_erros <- bind_rows(falhas)



### 4.2.2 Junta os dataframes de UF por ano e criar arquivos ano a ano (RODAR UMA VEZ SÓ): ----

# for (ano in 2000:2024) {
#   
#   lista_ufs <- list()
#   
#   for (uf in ufs) {
#     
#     periodo <- f_periodo(ano)
#     
#     nome_arquivo <- file.path(pasta_destino, glue("siope_{uf}_{ano}_P{periodo}.csv"))
#     
#     if (!file.exists(nome_arquivo)) {
#       message(glue("❌ Arquivo não encontrado: {nome_arquivo}"))
#       next
#     }
#     
#     df_uf_ano <- read.csv(nome_arquivo, fileEncoding = "UTF-8") %>%
#       # filter(
#         # NOM_COLU == "Deduções FUNDEB" |
#           # str_starts(NOM_ITEM, "Transferências de Recursos do Fundo de Manutenção e Desenvolvimento da Educação Básica")
#       # ) %>%
#       mutate(ANO = ano, UF = uf) %>% 
#       mutate(COD_EXIB_FORMATADO = as.numeric(COD_EXIB_FORMATADO),
#              COD_MUNI = as.numeric(COD_MUNI)) %>% 
#       mutate(across(where(is.character), ~ .x %>%
#                       str_squish() %>%                
#                       str_remove("\\.+$") %>%       
#                       str_remove("\\s+$")))
#     
#     lista_ufs[[uf]] <- df_uf_ano
#   }
#   
#   dados_ano <- bind_rows(lista_ufs)
#   
#   assign(glue("dados_{ano}"), dados_ano)
#   
#   pasta_anos <- "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/SIOPE/Anos"
#   if (!dir.exists(pasta_anos)) dir.create(pasta_anos, recursive = TRUE)
#   
#   write.csv(dados_ano, file = file.path(pasta_anos, paste0(ano, ".csv")), row.names = FALSE)
#   
#   message(glue("✅ Dados do ano {ano} prontos."))
#   rm(lista_ufs, df_uf_ano, dados_ano)
#   rm(list = glue::glue("dados_{ano}"))
# 
#   }
# 
# rm(list = ls(pattern = "^dados_2"))


### 4.2.3 Importar todos os anos para uma lista: ----

# pasta_anos <- "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/SIOPE/Anos"
# 
# # Inicializa a lista
# lista_anos <- list()
# 
# # Loop pelos anos
# for (ano in 2005:2024) {
#   caminho_arquivo <- file.path(pasta_anos, paste0(ano, ".csv"))
#   
#   if (!file.exists(caminho_arquivo)) {
#     message(glue::glue("❌ Arquivo não encontrado para {ano}"))
#     next
#   }
#   
#   df2 <- read.csv(caminho_arquivo, fileEncoding = "UTF-8") %>% 
#     # select(-c(NUM_NIVE, NUM_ORDE, NUM_PERI, COD_EXIB_FORMATADO, IDN_CLASS, SIG_UF, NUM_ANO)) %>% 
#     select(c(ANO, TIPO, UF, COD_UF, COD_MUNI, NOM_MUNI, NOM_ITEM, NOM_COLU, VAL_DECL)) %>% 
#     filter(NOM_COLU %in% c("Receitas Realizadas", "Deduções FUNDEB")) %>% 
#            # COD_MUNI != "null") %>% 
#     mutate(TIPO = as.character(TIPO),
#            COD_UF = as.numeric(COD_UF),
#            COD_MUNI = if_else(TIPO == "Estadual", COD_UF, COD_MUNI))
#  
#   if (ano == 2005){
#     df_filtrado <- df2 %>%
#       mutate(
#         NOM_ITEM = stringr::str_replace_all(NOM_ITEM, regex("FUNDEB", ignore_case = TRUE), "FUNDEF")
#       ) %>% 
#       filter(
#           NOM_ITEM == "Transferências de Recursos do FUNDEF" |
#           str_starts(NOM_ITEM, "Transferências da Complementação") |
#           NOM_ITEM == "DEDUÇÕES DA RECEITA CORRENTE"
#       ) 
#   }
# 
#   if (ano == 2006){
#   df_filtrado <- df2 %>%
#     filter(
#            NOM_ITEM == "Transferências Multigovernamentais" |
#            NOM_ITEM == "Transferências de Recursos do FUNDEF" |
#            str_starts(NOM_ITEM, "Transferências da Complementação") |
#            NOM_ITEM == "DEDUÇÕES DA RECEITA CORRENTE"
#            )
#   }
#   
#   if(ano >= 2007 & ano <= 2020){
#     df_filtrado <- df2 %>% 
#       filter(NOM_ITEM == "DEDUÇÕES DA RECEITA CORRENTE" |
#                NOM_ITEM == "Transferências de Recursos do FUNDEB" |
#                NOM_ITEM == "Transferências de Recursos da Complementação da União ao FUNDEB" |
#                # NOM_ITEM == "Receita da Remuneração de Depósitos Bancários de Recursos Vinculados - FUNDEB" |
#                NOM_ITEM == "Transferências Multigovernamentais")
#   }
#   
#   if(ano > 2020){
#     df_filtrado <- df2 %>% 
#       filter(str_starts(NOM_ITEM, "Transferências de Recursos do Fundo de Manutenção") & str_ends(NOM_ITEM, regex("Fundeb$", ignore_case = TRUE)) |
#                str_starts(NOM_ITEM, "Transferências de Recursos de Complementação da União") & str_ends(NOM_ITEM, regex("Fundeb$", ignore_case = TRUE)) |
#                NOM_ITEM == "Receitas Correntes" & NOM_COLU == "Deduções FUNDEB" |
#                # str_starts(NOM_ITEM, "Remuneração de Depósitos Bancários") |
#               NOM_ITEM == "Transferências Multigovernamentais"
#              ) %>% 
#       mutate(
#         NOM_ITEM = case_when(
#           str_detect(NOM_ITEM, regex("Transferências de Recursos do Fundo de Manutenção.*Fundeb$", ignore_case = TRUE)) ~ "Transferências de Recursos do Fundeb",
#           str_detect(NOM_ITEM, regex("Transferências de Recursos de Complementação da União.*Fundeb$", ignore_case = TRUE)) ~ "Transferências de Recursos de Complementação da União ao Fundeb",
#           TRUE ~ NOM_ITEM
#           )
#         ) %>% 
#       filter(NOM_COLU != "Deduções FUNDEB" | NOM_ITEM == "Receitas Correntes")
#   }
# 
#   lista_anos[[as.character(ano)]] <- df_filtrado
#   message(glue("✅{ano} importado e filtrado com sucesso."))
#   rm(df2, df_filtrado)
# }



### 4.2.4 Manipulação - Criar "Transferências Líquidas"----
# Aqui, estamos olhando apenas as receitas realizadas.

# As transferências líquidas do FUNDEF/FUNDEB podem ser calculadas como:

#                            Transferência Principal - Deduções


# lista_liq <- list()
# 
# for (ano in 2005:2024) {
#   df_ano <- lista_anos[[as.character(ano)]] %>%
#     distinct(.keep_all = TRUE) %>% 
#     select(-NOM_COLU) %>% 
#     pivot_wider(names_from = NOM_ITEM, values_from = VAL_DECL, values_fill = 0) %>%
#     group_by(
#       COD_MUNI,
#       NOM_MUNI,
#       UF,
#       ANO
#     )
#   
#   # if(ano == 2005){
#   #   df_ano <- df_ano %>%
#   #     mutate(
#   #       # `Receita total do FUNDEF/FUNDEB` = `Transferências Multigovernamentais`,
#   #       `Principal do FUNDEF/FUNDEB` = `Transferências de Recursos do FUNDEB`,
#   #       `Complementação da União` = `Transferências da Complementação da União ao FUNDEB`,
#   #       `Deduções` = `DEDUÇÕES DA RECEITA CORRENTE`,
#   #       # `Transferência Líquida 1` = `Receita total do FUNDEF/FUNDEB` - `Deduções` - `Complementação da União`,
#   #       `Transferência Líquida` = `Principal do FUNDEF/FUNDEB` - `Deduções`
#   #       # Dif = `Transferência Líquida 1` - `Transferência Líquida 2`,
#   #     ) %>%
#   #     ungroup()
#   # 
#   # }
#   
#   if (ano <= 2006) {
#     df_ano <- df_ano %>%
#       mutate(
#         # `Receita total do FUNDEF/FUNDEB` = `Transferências Multigovernamentais`,
#         `Principal do FUNDEF/FUNDEB` = `Transferências de Recursos do FUNDEF`,
#         `Complementação da União` = `Transferências da Complementação da União ao FUNDEF`,
#         `Deduções` = `DEDUÇÕES DA RECEITA CORRENTE`,
#         # `Transferência Líquida 1` = `Receita total do FUNDEF/FUNDEB` - `Deduções` - `Complementação da União`,
#         `Transferência Líquida` = `Principal do FUNDEF/FUNDEB` - `Deduções`
#         # Dif = `Transferência Líquida 1` - `Transferência Líquida 2`,
#       ) %>% 
#       ungroup()
#     
#   } 
#   
#   else if (ano >= 2007 & ano <= 2020) {
#     df_ano <- df_ano %>%
#       mutate(
#         # `Receita total do FUNDEF/FUNDEB` = `Transferências Multigovernamentais`,
#         `Principal do FUNDEF/FUNDEB` = `Transferências de Recursos do FUNDEB`,
#         `Complementação da União` = `Transferências de Recursos da Complementação da União ao FUNDEB`,
#         # `Remuneração Financeira` = `Receita da Remuneração de Depósitos Bancários de Recursos Vinculados - FUNDEB`,
#         `Deduções` = `DEDUÇÕES DA RECEITA CORRENTE`,
#         # `Transferência Líquida 1` = `Receita total do FUNDEF/FUNDEB` + `Remuneração Financeira` - `Deduções` - `Complementação da União`,
#         `Transferência Líquida` = `Principal do FUNDEF/FUNDEB` - `Deduções`
#         # Dif = `Transferência Líquida 1` - `Transferência Líquida 2`,
#       ) %>% 
#       ungroup()
#   
#   } else {
#     df_ano <- df_ano %>%
#       mutate(
#         # `Receita total do FUNDEF/FUNDEB` = `Transferências de Recursos do Fundeb` +
#         #   `Transferências de Recursos de Complementação da União ao Fundeb`,
#         `Principal do FUNDEF/FUNDEB` = `Transferências de Recursos do Fundeb`,
#         `Complementação da União` = `Transferências de Recursos de Complementação da União ao Fundeb`,
#         # `Remuneração Financeira` = `Remuneração de Depósitos Bancários de Recursos Vinculados - Fundeb`,
#         `Deduções` = `Receitas Correntes`,
#         # `Transferência Líquida 1` = `Receita total do FUNDEF/FUNDEB`  - `Deduções` - `Complementação da União`,
#         `Transferência Líquida` = `Principal do FUNDEF/FUNDEB`  - `Deduções`
#         # Dif = `Transferência Líquida 1` - `Transferência Líquida 2`,
#       ) %>% 
#       ungroup()
#   }
#   
#   df_ano <- df_ano %>% 
#     select(c(ANO, TIPO, UF, COD_UF, NOM_MUNI, COD_MUNI, `Principal do FUNDEF/FUNDEB`,
#              `Complementação da União`,
#              `Deduções`,
#              `Transferência Líquida`))
#   lista_liq[[as.character(ano)]] <- df_ano
#   rm(df_ano)
#   message(glue("✅ {ano} manipulado com sucesso"))
# }
# 
# transf <- bind_rows(lista_liq) %>% 
#   clean_names()



### 4.2.5 Deflacionar:----

# ipca <- read_excel("C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/IPCA_acumulado_ano.xlsx", skip = 1)
# colnames(ipca) = c("ano", "ipca")
# 
# ipca <- ipca %>% 
#   mutate(ano = as.numeric(ano),
#          ipca = as.numeric(str_replace(ipca, ",", "."))) %>% 
#   filter(ano %in% 2000:2024) %>% 
#   mutate(indice = as.numeric(ifelse(ano == 2021, 1, 0))) %>% 
#   arrange(ano) %>% 
#   mutate(indice = cumprod(1 + ipca/100)) %>% 
#   mutate(indice = indice / indice[ano == 2021])
# 
# transf <- transf %>% 
#   left_join(ipca,
#             by = "ano") %>% 
#   mutate(principal_do_fundef_fundeb = principal_do_fundef_fundeb / indice,
#          complementacao_da_uniao = complementacao_da_uniao / indice,
#          deducoes = deducoes / indice,
#          transferencia_liquida = transferencia_liquida / indice) %>% 
#   select(-c(indice, ipca))
# 


# rm(ipca)



### 4.2.6 Juntar tudo e exportar:----


# colnames(transf)
# colnames(transf) <- c("ano", "tipo", "uf", "cod_uf", "nome", "codigo_ibge_n", "principal", "complementacao", "deducoes", 'transf_liquida')
# 
# df1 <- df1 %>% 
#   mutate(codigo_ibge_n = as.numeric(str_sub(as.character(codigo_ibge), start = 1, end = 6)))
# 
# 
# df <- left_join(df1,
#                 transf,
#                 by = c("codigo_ibge_n", "ano", "uf")) %>% 
#   select(-nome.y) %>% 
#   rename(nome = "nome.x")
# 
# # df_t <- df %>% 
#   # select(1:5, 9, 37:43)
# 
# 
# rm(df1, lista_liq, lista_anos, transf)
# 
# write.csv2(df, file = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/painel_notas_transferencias_2000_2024.csv")





# 5. Simulação ----
## 5.1. Agreg Mun ----

mat_2006 <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/censo_2006_filtrado.csv")

mat_2006_munic <- mat_2006 %>%
  group_by(CODMUNIC, DEP, ind_quil) %>%
  summarise(
    nome = first(MUNIC),
    no_uf = first(UF),
    uf = first(SIGLA),
    mat_reg_in = sum(reg_in, na.rm = TRUE),
    mat_reg_fin   = sum(reg_fin, na.rm = TRUE),
    mat_tot_esp = sum(esp_total_final, na.rm = TRUE),
    mat_tot_em = sum(em_tot, na.rm = TRUE),
    mat_tot_inf = sum(ed_inf_tot, na.rm = TRUE),
    mat_tot_eja = sum(eja_tot, na.rm = TRUE),
    .groups = "drop"
  )

colnames(mat_2006_munic)
glimpse(mat_2006_munic %>% select(CODMUNIC, mat_reg_in, mat_reg_fin, mat_tot_esp,
                                  mat_tot_em, mat_tot_inf, mat_tot_eja))


saveRDS(mat_2006_munic, file = "Z:/Tuffy/Paper - Educ/Dados/censo_2006_filtrado_mun.rds")


## 5.2 Sim. Dados Censo (antigas) ----
### 5.2.1 Coef de distribuição ----

#' Fatores de ponderação do FUNDEF podem ser decompostos em:
#' FD1 <- 1.00 para 1 a 4 série regular
#' FD2 <- 1.05 da 5 para 8 série regula + educação especial


mat_2006 <- readRDS("Z:/Tuffy/Paper - Educ/Dados/censo_2006_filtrado_mun.rds") %>% 
  add_label("mat_reg_in", "Mat. Fund. Iniciais regular (CENSO)") %>%
  add_label("mat_reg_fin", "Mat. Fund. Finais regular (CENSO)") %>% 
  add_label("mat_tot_em", "Mat. EM (CENSO)") %>% 
  add_label("mat_tot_inf", "Mat. Infantil (CENSO)") %>% 
  add_label("mat_tot_eja", "Mat. EJA (CENSO)") %>% 
  add_label("mat_tot_esp", "Mat. Especiais (CENSO)")


# Calcula totais por estado (TA_*)
estaduais <- mat_2006 %>%
  group_by(uf) %>%
  summarise(
    TA_esp = sum(mat_tot_esp, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  add_label("TA_esp", "Total de Mat. Especiais na UF")

mat_cd <- mat_2006 %>%
  left_join(estaduais, by = "uf")



# Agrega as matrículas da rede estadual:
mat_cd <- mat_cd %>%
  group_by(DEP, uf, nome = ifelse(DEP == "Estadual", "GOVERNO ESTADUAL", nome)) %>%
  summarise(
    codigo_ibge = first(CODMUNIC),
    mat_reg_iniciais = sum(mat_reg_in, na.rm = TRUE),
    mat_reg_finais = sum(mat_reg_fin, na.rm = TRUE),
    mat_tot_esp = sum(mat_tot_esp, na.rm = TRUE),
    TA_esp = first(TA_esp),  # Mesma lógica para TA_esp
    .groups = "drop"
  ) %>% 
  mutate(codigo_ibge = ifelse(nome != "GOVERNO ESTADUAL",
                              codigo_ibge,
                              case_when(uf == "AC" ~ 12,
                                        uf == "AL" ~ 27,
                                        uf == "AP" ~ 16,
                                        uf == "AM" ~ 13,
                                        uf == "BA" ~ 29,
                                        uf == "CE" ~ 23,
                                        uf == "DF" ~ 53,
                                        uf == "ES" ~ 32,
                                        uf == "GO" ~ 52,
                                        uf == "MA" ~ 21,
                                        uf == "MT" ~ 51,
                                        uf == "MS" ~ 50,
                                        uf == "MG" ~ 31,
                                        uf == "PA" ~ 15,
                                        uf == "PB" ~ 25,
                                        uf == "PR" ~ 41,
                                        uf == "PE" ~ 26,
                                        uf == "PI" ~ 22,
                                        uf == "RJ" ~ 33,
                                        uf == "RN" ~ 24,
                                        uf == "RS" ~ 43,
                                        uf == "RO" ~ 11,
                                        uf == "RR" ~ 14,
                                        uf == "SC" ~ 42,
                                        uf == "SP" ~ 35,
                                        uf == "SE" ~ 28,
                                        uf == "TO" ~ 17,
                                        TRUE ~ NA_real_
                              ))) %>% 
  add_label("mat_reg_iniciais", "Mat. Fund. Iniciais regular (CENSO)") %>%
  add_label("mat_reg_finais", "Mat. Fund. Finais regular (CENSO)") %>% 
  add_label("mat_tot_esp", "Mat. Especiais total (CENSO)") %>% 
  add_label("TA_esp", "Total Mat. Especiais do UF (CENSO)")

## 5.3 Matric. Oficiais do FUNDEF ----

lista_ufs <- list()

pasta <- "Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/ponderacao_matriculas_2007/excel/"

arquivos <- list.files(path = pasta, pattern = "\\.xlsx$", full.names = TRUE)

for (file in arquivos) {
  
  # Leitura sem cabeçalho
  df_uf <- read_excel(file, col_names = FALSE)
  
  # Renomeia todas as colunas
  colnames(df_uf) <- c("nome", "mat_creche", "mat_pre", "mat_fund_in_urb", "mat_fund_in_rur", 
                       "mat_fund_fin_urb", "mat_fund_fin_rur", "mat_fund_integ", "mat_em_urb",
                       "mat_em_rur", "mat_em_prof", "mat_esp_frac", "mat_eja", "mat_ind",
                       "coef_fundeb", "est_receitas_fundeb")
  
  # Corrige nome e converte colunas para número
  df_uf <- df_uf %>%
    filter(!is.na(mat_creche)) %>%
    distinct() %>% 
    mutate(nome = str_replace_all(nome, "[\r\n]", " ")) %>%
    mutate(nome = str_squish(nome)) %>%
    mutate(across(
      .cols = 2:ncol(.),
      .fns = ~ as.numeric(gsub(",", ".", gsub("\\.", "", as.character(.))))
    )) %>%
    filter(complete.cases(.))
  
  lista_ufs[[str_sub(basename(file), 1, 2)]] <- df_uf
  rm(df_uf)
}



al <- lista_ufs[["al"]] %>%
  mutate(nome = str_replace(nome, "TOLTAL BRASIL", "TOTAL BRASIL"))
lista_ufs[["al"]] <- al
rm(al)

dis_fed <- lista_ufs[["df"]] %>%
  mutate(nome = str_replace(nome, "TOTAL -Distrito Federal", "DISTRITO FEDERAL"))
lista_ufs[["df"]] <- dis_fed
rm(dis_fed)

## 5.4 Unico DF----

df_fnde <- data.frame()

for (uf in names(lista_ufs)) {
  
  uf_df <- lista_ufs[[uf]] %>%
    filter(!grepl("total", nome, ignore.case = TRUE)) %>% 
    mutate(uf = as.character(toupper(uf)))
  
  df_fnde <- bind_rows(df_fnde, uf_df)
  rm(uf_df, uf)
}

df_fnde <- df_fnde %>% 
  select(c(nome, uf, coef_fundeb, est_receitas_fundeb, everything())) %>% 
  rename(coef_est_fnde = "coef_fundeb",
         receita_est_fnde = "est_receitas_fundeb") %>% 
  add_label("coef_est_fnde", "Coef. de Dist. p/ o FUNDEB (2007)") %>% 
  add_label("receita_est_fnde", "Receita estimada FUNDEB (2007)") %>% 
  add_label("mat_creche", "Mat. Creche (FNDE)") %>% 
  add_label("mat_pre", "Mat. Pré-Escola (FNDE)") %>% 
  add_label("mat_fund_in_urb", "Mat. Iniciais Urbana (FNDE)") %>% 
  add_label("mat_fund_in_rur", "Mat. Iniciais Rural (FNDE)") %>% 
  add_label("mat_fund_fin_urb", "Mat. Finais Urbana (FNDE)") %>% 
  add_label("mat_fund_fin_rur", "Mat. Finais Rural (FNDE)") %>% 
  add_label("mat_fund_integ", "Mat. Fund. Integral Total (FNDE)") %>% 
  add_label("mat_em_urb", "Mat. EM Urbana (FNDE)") %>% 
  add_label("mat_em_rur", "Mat. EM Rural (FNDE)") %>% 
  add_label("mat_em_prof", "Mat. EM Profissionalizante (FNDE)") %>% 
  add_label("mat_esp_frac", "Mat. Especiais FUNDEB (FNDE)") %>% 
  add_label("mat_eja", "Mat. EJA (FNDE)") %>% 
  add_label("mat_ind", "Mat. Indígena FUNDEB (FNDE)") 

colnames(df_fnde)

test <- df_fnde %>% 
  group_by(uf) %>% 
  summarise(
    check_coef_fb = sum(coef_est_fnde)
  ) %>% 
  ungroup()

rm(test)

### 5.4.1 Calculando a proporção de alunos em cada ciclo pelos dados do Censo: ----

# Mais para frente, precisaremos dividir as matrículas do Ensino Integral entre os Anos Iniciais e Anos Finais.
# Como alguns municípios só tem ensino integral, é impossível calcular a proporção de alunos em cada ciclo pelas
# ponderações do FUNDEB do FNDE. Assim, calculemos a proporção de alunos em cada ciclo pelos dados do Censo.

# EX: TRAVESSEIRO - RS:
# 126 matrículas integrais, 0 em iniciais, 0 em finais (FNDE)
# 0 integrais (não existe), 109 em iniciais, 17 em finais (CENSO)


mat_cd <- mat_cd %>% 
  mutate(prop_1_4_mun = ifelse(is.finite(mat_reg_iniciais / (mat_reg_iniciais + mat_reg_finais)), 
                               mat_reg_iniciais / (mat_reg_iniciais + mat_reg_finais),
                               0),
         prop_5_8_mun = ifelse(is.finite(mat_reg_finais / (mat_reg_iniciais + mat_reg_finais)),
                               mat_reg_finais / (mat_reg_iniciais + mat_reg_finais),
                               0)) %>% 
  add_label("prop_1_4_mun", "Prop. de Mat. Iniciais (CENSO)") %>% 
  add_label("prop_5_8_mun", "Prop. de Mat. Finais (CENSO)")


### 5.4.2 Matríc. indígenas e especiais pelo Censo: ----

#### 5.4.2.1 Especiais: ----

df_fnde <- df_fnde %>%
  mutate(chave = paste(nome, uf, sep = "_"))

mat_cd <- mat_cd %>%
  mutate(chave = paste(nome, uf, sep = "_"))

# Verifica se todos os municípios de `df_fnde` estão em `mat_2006`
faltando_em_mat_cd <- setdiff(df_fnde$chave, mat_cd$chave)
print(faltando_em_mat_cd)

# Verifica se todos os municípios de `mat_2006` estão em `df_fnde`
faltando_em_df_fnde <- setdiff(mat_cd$chave, df_fnde$chave)
print(faltando_em_df_fnde)

rm(faltando_em_df_fnde, faltando_em_mat_cd)


# Juntando as matrículas de Ed. Especial:
df_fnde <- df_fnde %>%
  left_join(mat_cd %>%
              select(
                chave, codigo_ibge, mat_tot_esp, TA_esp, prop_1_4_mun,
                prop_5_8_mun), by = "chave"
  )

#### 5.4.2.2 Ed. Indígena e Quilombola: ----

# Filtrando as matrículas de Ed. Quilombola do dataframe inicial:

mat_ind <- mat_2006 %>% 
  filter(ind_quil == 1) %>% 
  select(c(CODMUNIC:ncol(.))) %>% 
  
  group_by(DEP, uf, nome = ifelse(DEP == "Estadual", "GOVERNO ESTADUAL", nome)) %>% 
  summarise(
    codigo_ibge = first(CODMUNIC),
    mat_reg_in_ind = sum(mat_reg_in, na.rm = TRUE),
    mat_reg_fin_ind = sum(mat_reg_fin, na.rm = TRUE),
    mat_esp_ind = sum(mat_tot_esp, na.rm = TRUE),
    mat_em_ind = sum(mat_tot_em, na.rm = TRUE),
    mat_inf_ind = sum(mat_tot_inf, na.rm = TRUE),
    mat_eja_ind = sum(mat_tot_eja, na.rm = TRUE),
    .groups = "drop") %>% 
  mutate(chave = paste(nome, uf, sep = "_")) %>% 
  mutate(codigo_ibge = ifelse(nome != "GOVERNO ESTADUAL",
                              codigo_ibge,
                              case_when(uf == "AC" ~ 12,
                                        uf == "AL" ~ 27,
                                        uf == "AP" ~ 16,
                                        uf == "AM" ~ 13,
                                        uf == "BA" ~ 29,
                                        uf == "CE" ~ 23,
                                        uf == "DF" ~ 53,
                                        uf == "ES" ~ 32,
                                        uf == "GO" ~ 52,
                                        uf == "MA" ~ 21,
                                        uf == "MT" ~ 51,
                                        uf == "MS" ~ 50,
                                        uf == "MG" ~ 31,
                                        uf == "PA" ~ 15,
                                        uf == "PB" ~ 25,
                                        uf == "PR" ~ 41,
                                        uf == "PE" ~ 26,
                                        uf == "PI" ~ 22,
                                        uf == "RJ" ~ 33,
                                        uf == "RN" ~ 24,
                                        uf == "RS" ~ 43,
                                        uf == "RO" ~ 11,
                                        uf == "RR" ~ 14,
                                        uf == "SC" ~ 42,
                                        uf == "SP" ~ 35,
                                        uf == "SE" ~ 28,
                                        uf == "TO" ~ 17,
                                        TRUE ~ NA_real_
                              ))) 


# Estas são as matrículas quilombolas e indígenas que não aparecem como matrículas de EF nos
# arquivos do FNDE para as ponderações do FUNDEB. No nosso caso, devemos computá-las como
# matrículas regulares do EF para fazer a simulação caso o FUNDEF tivesse se mantido. 



# Juntando no DF principal:

df_fnde <- left_join(
  df_fnde,
  mat_ind %>% select(c(chave, codigo_ibge, mat_reg_in_ind:mat_eja_ind)),
  by = c("codigo_ibge", "chave")
) %>% 
  relocate(mat_reg_in_ind, .after = mat_fund_in_rur) %>% 
  relocate(mat_reg_fin_ind, .after = mat_fund_fin_rur) %>%
  relocate(mat_em_ind, .after = mat_em_rur) %>%
  relocate(mat_inf_ind, .after = mat_pre) %>%
  relocate(mat_eja_ind, .after = mat_eja) %>%
  # relocate(mat_esp_ind, .after = mat_fund_fin_rur) %>%
  mutate(mat_reg_in_ind = coalesce(mat_reg_in_ind, 0),     # Garante que NA seja zero.
         mat_reg_fin_ind = coalesce(mat_reg_fin_ind, 0),
         mat_em_ind = coalesce(mat_em_ind, 0),
         mat_inf_ind = coalesce(mat_inf_ind, 0),
         mat_eja_ind = coalesce(mat_eja_ind, 0)) %>% 
  add_label("mat_reg_in_ind", "Mat. Indígena Iniciais (CENSO)") %>% 
  add_label("mat_reg_fin_ind", "Mat. Indígena Finais (CENSO)") %>% 
  add_label("mat_esp_ind", "Mat. Indígena Especiais Total (CENSO)") %>% 
  add_label("mat_em_ind", "Mat. Indígena EM (CENSO)") %>% 
  add_label("mat_inf_ind", "Mat. Indígena Infantil (CENSO)") %>% 
  add_label("mat_eja_ind", "Mat. Indígena EJA (CENSO)") 

#Test
test <- df_fnde %>% 
  group_by(uf) %>% 
  summarise(
    check_coef_fb = sum(coef_est_fnde)
  ) %>% 
  ungroup()

rm(test)

### AGORA TEMOS TODOS OS DADOS QUE PRECISAMOS PARA SIMULAR O COEFICIENTE DE DISTRIBUIÇÃO PELAS REGRAS DO FUNDEF:


## 5.5 De facto Simulação: ----

# 1. Total por estado (TA = total alunos por estado por faixa)
totais_estado2 <- df_fnde %>%
  group_by(uf) %>%
  summarise(
    # Calcular a proporção de matrículas em cada ciclo, para alocar as matrículas integrais:
    mat_1_4 = sum(mat_fund_in_urb, na.rm = TRUE) + sum(mat_fund_in_rur, na.rm = TRUE)
    + sum(mat_reg_in_ind, na.rm = TRUE),
    
    mat_5_8 = sum(mat_fund_fin_urb, na.rm = TRUE) + sum(mat_fund_fin_rur, na.rm = TRUE)
    + sum(mat_reg_fin_ind, na.rm = TRUE),
    
    prop_1_4 = mat_1_4/(mat_1_4 + mat_5_8),
    prop_5_8 = mat_5_8/(mat_1_4 + mat_5_8),
    
    # Somando o total de alunos por Estado por ciclo, 
    # Usando a proporção de alunos nos Iniciais e Finais no estado para distribuir as matrículas em tempo integral:
    
    TA_1_4      = sum(mat_fund_in_urb, na.rm = TRUE) + sum(mat_fund_in_rur, na.rm = TRUE) + 
      (sum(mat_fund_integ, na.rm = TRUE) * prop_1_4) + sum(mat_reg_in_ind, na.rm = TRUE),
    
    TA_5_8      = sum(mat_fund_fin_urb, na.rm = TRUE) + sum(mat_fund_fin_rur, na.rm = TRUE) +
      (sum(mat_fund_integ, na.rm = TRUE) * prop_5_8) + sum(mat_reg_fin_ind, na.rm = TRUE),
    
    .groups = "drop")


# Fatores
FD1 <- 1.00  # 1ª a 4ª série
FD2 <- 1.05  # 5ª a 8ª e especial



# 3. Simulação por município
simulacao <- df_fnde %>%
  left_join(totais_estado2, by = "uf") %>%
  mutate(
    mat_1_4_aux = (mat_fund_in_urb + mat_fund_in_rur + mat_reg_in_ind), # matrículas iniciais do EF regular contando as indígenas (NA1/4) - integral
    mat_5_8_aux = (mat_fund_fin_urb + mat_fund_fin_rur + mat_reg_fin_ind), # matrículas finais do EF regular contando as indígenas (NA5/8) - integral
    
    numerador = FD1 * (mat_1_4_aux + mat_fund_integ * (prop_1_4)) + # FD1 * (NA1/4 + integral)
      # O último termo é a qtd. de matrículas integral no município vezes a proporção de alunos nos anos iniciais do EF nesse município.
      FD2 * (mat_5_8_aux + mat_fund_integ * (prop_5_8) + mat_tot_esp), # FD2 * (NA5/8 + integral + NAe)
    
    denominador = FD1 * TA_1_4 + FD2 * (TA_5_8 + TA_esp),
    
    coef_simulado = numerador / denominador # Porcentagem (em decimal) do Fundo Estadual que a rede de ensino pega.
    
  )

#Teste 
test <- simulacao %>% 
  group_by(uf) %>% 
  summarise(
    check = sum(coef_simulado, na.rm = T)
  )

rm(test)
#Tudo certo

# Colocando os valores totais dos fundos estaduais:

df <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/painel_notas_transferencias_2000_2024.csv")

df_fundo <- df %>% 
  select(codigo_ibge, ano, uf, nome, total_politica, total_politica_d) %>%
  distinct() %>% 
  group_by(ano, uf) %>% 
  summarise(total_fundo_real = sum(total_politica, na.rm = TRUE),
            total_fundo_d_real = sum(total_politica_d, na.rm = TRUE),
            .groups = "drop") %>% 
  filter(ano == 2007) %>% 
  add_label("total_fundo_real", "Valor efetivo do Fundo da UF (2007), considerando compl.")


simulacao <- simulacao %>%
  left_join(df_fundo, by = "uf") %>%
  relocate(chave, .after = "uf") %>%
  mutate(receita_simulada = coef_simulado * total_fundo_real,
         
         dif_coef_pp = 100 * (coef_est_fnde - coef_simulado), # já que o coef_simulado é a porcentagem (em decimal) do fundo estadual que a rede pega.
         
         dif_per_coef = 100 * ((coef_est_fnde - coef_simulado)/ coef_simulado),
         
         tot_matri = (mat_creche + mat_pre + mat_inf_ind + mat_fund_in_urb +
                        mat_fund_in_rur + mat_reg_in_ind + mat_fund_fin_urb +
                        mat_fund_fin_rur + mat_reg_fin_ind + mat_fund_integ + # todas as matrículas integrais do EF
                        mat_em_urb + mat_em_rur + mat_em_ind + mat_em_prof + 
                        mat_eja + mat_eja_ind + mat_tot_esp),
         
         shr_inf_em = 100 * ((mat_creche + mat_pre + mat_inf_ind +  mat_em_urb + mat_em_rur + mat_em_ind + mat_em_prof)/
                               tot_matri),
         shr_inf = 100 * ((mat_creche + mat_pre + mat_inf_ind)/ tot_matri)
                            # (mat_creche + mat_pre + mat_inf_ind + mat_fund_in_urb +
                            #    mat_fund_in_rur + mat_reg_in_ind + mat_fund_fin_urb +
                            #    mat_fund_fin_rur + mat_reg_fin_ind + mat_fund_integ + # todas as matrículas integrais do EF
                            #    mat_em_urb + mat_em_rur + mat_em_ind + mat_em_prof + 
                            #    mat_eja + mat_eja_ind + mat_tot_esp)),
         

         ) %>% 
  
  select(c(nome, uf, codigo_ibge, chave, total_fundo_real, coef_est_fnde, coef_simulado, receita_est_fnde, receita_simulada, everything())) %>% 
  add_label("dif_coef_pp", "Dif. (p.p.) do Coef. com o FUNDEB vs FUNDEF (2007)") %>% 
  add_label("dif_per_coef", "Dif. (%) do Coef. com o FUNDEB vs FUNDEF (2007)") %>% 
  add_label("shr_inf_em", "Share de estudantes Infantil + EM (2006)") %>% 
  add_label("receita_simulada", "Receita simulada caso a regra do FUNDEF se mantivesse (considerando compl.)") %>% 
  add_label("coef_simulado", "Coef. de Distrib. caso a regra do FUNDEF se mantivesse (considerando compl.)") %>% 
  add_label("shr_inf", "Share de estudantes Ensino Infantil (2006)") %>% 
  add_label("tot_matri", "Total de matriculados (2006)")



#Teste
# test <- simulacao %>% 
#   group_by(uf.x) %>% 
#   summarise(
#     check = sum(coef_simulado, na.rm = T),
#     check2 = sum(coef_est_fnde, na.rm = T)
#   )
# 
# rm(test)




# Padronizar as chaves em df_fundeb_munic
df_fundeb_munic <- df %>%
  filter(ano == 2007) %>% 
  select(total_politica,
         # chave,
         codigo_ibge) %>% 
  distinct()


simulacao <- simulacao %>% 
  # left_join(df_fundeb_munic, by = "chave") %>% 
  left_join(df_fundeb_munic, by = "codigo_ibge") %>% 
  rename(receita_real = "total_politica") %>%
  select(c(codigo_ibge,
           # chave,
           nome, uf, coef_est_fnde, coef_simulado, dif_per_coef, dif_coef_pp, receita_est_fnde, receita_simulada, receita_real, shr_inf_em, shr_inf, everything())) %>% 
  add_label("receita_real", "Receita recebida pelo FUNDEB (2007)")



## 5.6 Simulação pelo VAA: ----

# Usando as ponderações do FUNDEB 2007 (art. 36 da LEI Nº 11.494, DE 20 DE JUNHO DE 2007),
# qual a mudança no Valor por Aluno/Ano (VAA)?

FDC = 0.8
FDI = 0.9
FD1_u = 1
FD1_r = 1.05
FD2_u = 1.1
FD2_r = 1.15
FD1_int = 1.25
FD3_u = 1.2
FD3_r = 1.25
FD3_p = 1.3
FD_esp = 1.2
FD_ind = 1.2
FD_eja = 0.7

# Só não considerei o EM integral pois não temos esta informação.


simulacao <- simulacao %>% 
  # matrículas ponderadas (FUNDEB 2007), já separando as matrículas indígenas:
  mutate(mp_creche = FDC * mat_creche,
         mp_pre = FDI * mat_pre,
         mp_in_u = FD1_u * mat_fund_in_urb,
         mp_in_r = FD1_r * mat_fund_in_rur,
         mp_fin_u = FD2_u * mat_fund_fin_urb,
         mp_fin_r = FD2_r * mat_fund_fin_rur,
         mp_fund_int = FD1_int * mat_fund_integ,
         mp_em_u = FD3_u * mat_em_urb,
         mp_em_r = FD3_r * (mat_em_rur - mat_em_ind),
         # mp_em_int = FD3_int * mat_em,
         mp_em_prof = FD3_p * mat_em_prof,
         mp_esp = FD_esp * (mat_tot_esp - mat_esp_ind),
         mp_ind = FD_ind * (mat_inf_ind + mat_reg_in_ind + mat_reg_fin_ind + mat_em_ind + mat_esp_ind + mat_eja_ind),
         mp_eja = FD_eja * (mat_eja - mat_eja_ind)) %>% 
  
  mutate(mp = rowSums(across(c(mp_creche:mp_eja)), na.rm = TRUE)) %>% 
  
  mutate(VAA_1_4_simulado = receita_simulada / mp,
         VAA_1_4_real = receita_real / mp) %>% 
  
  relocate(VAA_1_4_simulado, .after = receita_real) %>% 
  relocate(VAA_1_4_real, .after = VAA_1_4_simulado) %>% 
  add_label("VAA_1_4_simulado", "Valor Aluno/Ano caso mantido o FUNDEF (pond. do FUNDEB 2007)") %>% 
  add_label("VAA_1_4_real", "Valor Aluno/Ano real (pond. do FUNDEB 2007)") %>% 
  
  mutate(
    
    #DIF entre o valor por aluno real vs o simulado
    d_vaa = 100*((VAA_1_4_real - VAA_1_4_simulado) / VAA_1_4_simulado)
    
  ) %>% 
  relocate(d_vaa, .after = dif_per_coef) %>% 
  add_label("d_vaa", "Dif. (%) do VAA simulado pro real")



### 5.6.1 Adicionando número de alunos:----

simulacao <- simulacao %>% 
  mutate(
    total_alunos_2006 = mat_creche + mat_pre + mat_inf_ind + 
      mat_fund_in_urb + mat_fund_in_rur + mat_reg_in_ind + 
      mat_fund_fin_urb + mat_fund_fin_rur + mat_reg_fin_ind + 
      mat_fund_integ + mat_em_urb + mat_em_rur + mat_em_ind + mat_em_prof + 
      mat_eja + mat_eja_ind +
      mat_tot_esp) %>% 
  mutate(rs_por_aluno_fundeb = receita_real/total_alunos_2006,
         rs_por_aluno_sim = receita_simulada/total_alunos_2006) %>% 
  mutate(dif_rs_aluno = rs_por_aluno_fundeb - rs_por_aluno_sim) 



saveRDS(simulacao, "Z:/Tuffy/Paper - Educ/Dados/simulacao_const.rds")

rm(df_fundo, estaduais, lista_ufs, mat_2006_munic, mat_2006, mat_cd, mat_ind,
   totais_estado2, arquivos, FD_eja, FD_esp, FD_ind, FD1, FD1_int, FD1_r,
   FD1_u, FD2, FD2_r, FD2_u, FD3_p, FD3_r, FD3_u, FDC, FDI, file, pasta, df, df_fnde,
   df_fundeb_munic)


rm(list = ls())
gc()
# ---------------------------------------------------------------------------- #
# 6. Regression data ----
# ---------------------------------------------------------------------------- #

#Openeing all databases to join them

df_trn <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/painel_notas_transferencias_2000_2024.csv")

df_pesosaeb <- readRDS("Z:/Tuffy/Paper - Educ/Dados/pesos_saeb3.rds")

#df_gio <- readRDS("Z:/Tuffy/Paper - Educ/Dados/Gio_df.rds")

df_sim <- readRDS("Z:/Tuffy/Paper - Educ/Dados/simulacao_const.rds") #%>% 

#sim_gio <- readRDS(("Z:/Tuffy/Paper - Educ/Dados/Gio_sim.rds"))

df_fib <- read.csv2("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/Gastos municipais/FINBRA/Despesas/FINBRA_EDU_05_21.csv")


#Deflating:
df_ipca <- read_excel("Z:/Giovanni Zanetti/Av. Novo Fundeb/Dados/IPCA_acumulado_ano.xlsx", skip = 3)
colnames(df_ipca) = c("ano", "ipca")

df_ipca <- df_ipca %>% 
  mutate(ano = as.numeric(ano)) %>% 
  filter(ano %in% 2000:2021) %>%
  arrange(ano)


df_ipca <- df_ipca %>% 
  mutate(
    ipca = as.numeric(gsub(",",".",ipca)),
    indice = cumprod(1 + as.numeric(ipca)/100)) %>% 
  mutate(indice = indice / indice[ano == 2007]) #Reference year 2007

##6.1 Weights & GDP ----
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



## 6.2 Incluindo as variáveis principais ----
#[English: Including Major Variables]

### 6.2.1 Incluindo dif_per_coef nas notas: ----
#[English: Including dif_per_coef in exam scores]
temp <- df_trn %>% 
  left_join(df_sim %>% select(c(codigo_ibge, coef_est_fnde:receita_real, dif_rs_aluno,
                                rs_por_aluno_fundeb, rs_por_aluno_sim, shr_inf,
                                tot_matri, total_alunos_2006, total_fundo_d_real)),
            by = "codigo_ibge") %>% 
  filter(!is.na(coef_est_fnde)) %>%
  mutate(k = ano - 2007) %>%    # 2007 é o ano base [English: 2007 is the base year]
  # filter(ano %% 2 != 0) %>% 
  mutate(uf = as.factor(uf)) %>% 
  #Joining the deflation index
  left_join(df_ipca %>% rename(indice_ipca_07 = indice) %>% select(ano, indice_ipca_07),
            by = "ano")

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
  #Correcting Missing values
  mutate(educacao           = ifelse(educacao == 0,           NA, educacao),
         ensino_fundamental = ifelse(ensino_fundamental == 0, NA, ensino_fundamental),
         ensino_medio       = ifelse(ensino_medio == 0,       NA, ensino_medio),
         educacao_infantil  = ifelse(educacao_infantil == 0,  NA, educacao_infantil)
         ) %>% 
  mutate(#des_edu = educacao,             
         #des_fund = ensino_fundamental, #Spendings
         #des_med = ensino_medio,
         #des_inf = educacao_infantil,
         
         real_des_edu =  educacao / indice_ipca_07,            
         real_des_fund = ensino_fundamental / indice_ipca_07, #Spendings
         real_des_med = ensino_medio / indice_ipca_07,
         real_des_inf = educacao_infantil / indice_ipca_07,
         
         #Spending per-capita
         des_edu_pc = real_des_edu/populacao,
         des_fund_pc = real_des_fund/populacao,
         des_med_pc = real_des_med/populacao,
         des_inf_pc = real_des_inf/populacao) %>% 
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


#Total students
mat_mun_2006 <- readRDS("Z:/Tuffy/Paper - Educ/Dados/censo_2006_filtrado_mun.rds") %>% 
  filter(DEP == "Municipal") %>% 
  mutate(mat_reg_fund = mat_reg_in + mat_reg_fin,
         codigo_ibge = as.numeric(CODMUNIC) %/% 10) %>% #Transforming to the old code
  group_by(codigo_ibge) %>% 
  summarise(
    mat_fun = sum(mat_reg_fund, na.rm = T),
    mat_inf = sum(mat_tot_inf, na.rm = T),
    mat_med = sum(mat_tot_em, na.rm = T),
    mat_esp = sum(mat_tot_esp, na.rm = T),
    mat_eja = sum(mat_tot_eja, na.rm = T)
  ) %>% 
  mutate( total_alunos_2006 = mat_fun + mat_inf + mat_med + mat_esp + mat_eja) %>% 
  ungroup()




df_reg <- temp %>%
  filter (codigo_ibge > 10) %>% 
  select(-total_alunos_2006) %>% 
  left_join(mat_mun_2006, by = c("codigo_ibge")) %>% 
  mutate(dif_rs_aluno_100 = dif_rs_aluno / 100) %>%  # R$ PER STUDENT DOSAGE, em centenas

  # SPENDING DOSAGE:  ------------------------------------- #
  
  mutate(#spending_dosage_gio = dif_rs_aluno/ed_spending_2006,
  #spending_dos = receita_real/ed_spending_2006,
  #del_spending_dos = (receita_real - receita_simulada)/ed_spending_2006,
  
  dosage = (receita_real - receita_simulada)/receita_real,            #Prefered
  
  old_dosage = (receita_real - receita_simulada)/real_des_edu[ano == 2006] )
  
  #dosage_perc = del_spending_dos *100,
  
  #aluno_dosage = (receita_real - receita_simulada)/total_alunos_2006) #Prefered



colnames(df_reg)

#Label
attr(df_reg$dosage, "label") <- "Parcela da diferença de receita pelo FUNDEB (2007)"
attr(df_reg$aluno_dosage, "label") <- "Diferença de receita (2007) por aluno (2006)"



#saving database with dosage
saveRDS(df_reg, "Z:/Tuffy/Paper - Educ/Dados/regdf.rds")


gc()
rm(list = ls())

# ---------------------------------------------------------------------------- #
# 7. Data Cleaning and observation ----
# ---------------------------------------------------------------------------- #

data <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") %>% 
  select(-c(X.x, total_politica_d, total_coun_d, tx_aprovacao_iniciais,
            tx_aprovacao_1, tx_aprovacao_2, tx_aprovacao_3, tx_aprovacao_4,
            tx_aprovacao_5, tx_aprovacao_6, tx_aprovacao_7, tx_aprovacao_9,
            tx_aprovacao_9, tx_aprovacao_em, tx_aprovacao_1em, tx_aprovacao_2em,
            tx_aprovacao_3em, tx_aprovacao_4em, codigo_ibge_n, coef_simulado, dif_per_coef,
            dif_coef_pp, ipca, X.y, coluna)) %>% 
  rename(mat_fun_aux = mat_fun,
         mat_med_aux = mat_med,
         mat_inf_aux = mat_inf,
         mat_esp_aux = mat_esp,
         mat_eja_aux = mat_eja)
  


#Total enrollments
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
  rename(codigo_ibge = codmun) #%>% 
  #filter(ano != 2006)

#combining the data
data <- data %>% 
  left_join(df_enroll,
            by = c("ano", "codigo_ibge")) %>% 
  group_by(codigo_ibge) %>% 
  # mutate(mat_fun   = ifelse(ano == 2006, mat_fun_aux, mat_fun),
  #        mat_med   = ifelse(ano == 2006, mat_med_aux, mat_med),
  #        mat_inf   = ifelse(ano == 2006, mat_inf_aux, mat_inf),
  #        mat_esp   = ifelse(ano == 2006, mat_esp_aux, mat_esp),
  #        mat_eja   = ifelse(ano == 2006, mat_eja_aux, mat_eja),
  #        mat_total = ifelse(ano == 2006, mat_total_aux, mat_total)) %>% 
  filter(ano > 2004 & ano < 2019) %>%  # Removing NA values
  mutate(growth_enroll = ((mat_total - lag(mat_total))/lag(mat_total))*100,
         growth_spend  = ((real_des_edu - lag(real_des_edu))/lag(real_des_edu))*100) %>% 
  ungroup() %>% group_by(codigo_ibge, ano) %>% 
  mutate(
    real_des_edu_pa  = ifelse(mat_total != 0, real_des_edu/mat_total, NA),
    real_des_inf_pa  = ifelse(mat_inf != 0, real_des_inf/mat_inf, NA),
    real_des_fun_pa  = ifelse(mat_fun != 0, real_des_fund/mat_fun, NA),
    real_des_med_pa  = ifelse(mat_med != 0, real_des_med/mat_med, NA)
    ) %>% 
  ungroup() %>% 
  select(-c(mat_fun_aux, mat_med_aux, mat_inf_aux, mat_esp_aux, mat_eja_aux)) %>% 
  mutate(
    aluno_dosage = (receita_real - receita_simulada)/mat_total[ano == 2006] #Prefered final spec
  )



# ggplot(data %>% filter(is.finite(growth_spend)), aes(x = growth_spend)) +
#   geom_density(fill = "steelblue", alpha = 0.5) +
#   labs(
#     title = "Density of Growth in Spending",
#     x = "Growth (%)",
#     y = "Density"
#   ) +
#   theme_minimal()
# 
# ggplot(data %>% filter(is.finite(growth_enroll)), aes(x = growth_spend)) +
#   geom_density(fill = "steelblue", alpha = 0.5) +
#   labs(
#     title = "Density of Growth in Enrollment",
#     x = "Growth (%)",
#     y = "Density"
#   ) +
#   theme_minimal()

# ---------------------------------------------------------------------------- #
## 7.1 Flags ----
# ---------------------------------------------------------------------------- #

data <- data %>% 
  group_by(codigo_ibge) %>% 
  mutate(
    #Enrollment
    #Increas
    flag_enroll15 = ifelse(any(growth_enroll > 15, na.rm = T), 1, 0),
    flag_enroll20 = ifelse(any(growth_enroll > 20, na.rm = T), 1, 0),
    flag_enroll25 = ifelse(any(growth_enroll > 25, na.rm = T), 1, 0),
    flag_enroll30 = ifelse(any(growth_enroll > 30, na.rm = T), 1, 0),
    flag_enroll40 = ifelse(any(growth_enroll > 40, na.rm = T), 1, 0),
    #Decrease
    flag_enrollm15 = ifelse(any(growth_enroll < -15, na.rm = T), 1, 0),
    flag_enrollm20 = ifelse(any(growth_enroll < -20, na.rm = T), 1, 0),
    flag_enrollm25 = ifelse(any(growth_enroll < -25, na.rm = T), 1, 0),
    flag_enrollm30 = ifelse(any(growth_enroll < -30, na.rm = T), 1, 0),
    flag_enrollm40 = ifelse(any(growth_enroll < -40, na.rm = T), 1, 0),
    
    
    #Spend
    #Increase
    flag_spend15 = ifelse(any(growth_spend > 15, na.rm = T), 1, 0),
    flag_spend20 = ifelse(any(growth_spend > 20, na.rm = T), 1, 0),
    flag_spend25 = ifelse(any(growth_spend > 25, na.rm = T), 1, 0),
    flag_spend30 = ifelse(any(growth_spend > 30, na.rm = T), 1, 0),
    flag_spend40 = ifelse(any(growth_spend > 40, na.rm = T), 1, 0),
    flag_spend50 = ifelse(any(growth_spend > 50, na.rm = T), 1, 0),
    flag_spend60 = ifelse(any(growth_spend > 60, na.rm = T), 1, 0),
    flag_spend70 = ifelse(any(growth_spend > 70, na.rm = T), 1, 0),
    flag_spend80 = ifelse(any(growth_spend > 80, na.rm = T), 1, 0),
    #Decrease
    flag_spendm15 = ifelse(any(growth_spend < -15, na.rm = T), 1, 0),
    flag_spendm20 = ifelse(any(growth_spend < -20, na.rm = T), 1, 0),
    flag_spendm25 = ifelse(any(growth_spend < -25, na.rm = T), 1, 0),
    flag_spendm30 = ifelse(any(growth_spend < -30, na.rm = T), 1, 0),
    flag_spendm40 = ifelse(any(growth_spend < -40, na.rm = T), 1, 0),
    flag_spendm50 = ifelse(any(growth_spend < -50, na.rm = T), 1, 0),
    flag_spendm60 = ifelse(any(growth_spend < -60, na.rm = T), 1, 0),
    flag_spendm70 = ifelse(any(growth_spend < -70, na.rm = T), 1, 0),
    flag_spendm80 = ifelse(any(growth_spend < -80, na.rm = T), 1, 0),
    
  ) %>% 
  ungroup()

summary(data %>% select(flag_enroll15, flag_enroll20, flag_enroll25, flag_enroll30,
                        flag_spend15, flag_spend20, flag_spend25, flag_spend30, 
                        flag_spend40, flag_spend50, flag_spend60, flag_spend70,
                        flag_spend80, old_dosage))

test2 <- data %>% filter(flag_enroll15 == 0 & flag_enrollm15 == 0 & ano == 2007)

# ------------------------ #
### 7.1.1 Graph ----
# ------------------------ #


mean_long <- data %>%
  summarise(across(c(starts_with("flag_")),
                   ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "mean_value"
  ) %>% 
  mutate(
    color = ifelse(row_number() <= 10, "red", "darkblue"),
    growth = case_when(
      color == "red" & row_number() == 1 ~ 15,
      color == "red" & row_number() == 2 ~ 20,
      color == "red" & row_number() == 3 ~ 25,
      color == "red" & row_number() == 4 ~ 30,
      color == "red" & row_number() == 5 ~ 40,
      color == "red" & row_number() == 6 ~ -15,
      color == "red" & row_number() == 7 ~ -20,
      color == "red" & row_number() == 8 ~ -25,
      color == "red" & row_number() == 9 ~ -30,
      color == "red" & row_number() == 10 ~ -40,
      
      color == "darkblue" & row_number() == 11 ~ 15,
      color == "darkblue" & row_number() == 12 ~ 20,
      color == "darkblue" & row_number() == 13 ~ 25,
      color == "darkblue" & row_number() == 14 ~ 30,
      color == "darkblue" & row_number() == 15 ~ 40,
      color == "darkblue" & row_number() == 16 ~ 50,
      color == "darkblue" & row_number() == 17 ~ 60,
      color == "darkblue" & row_number() == 18 ~ 70,
      color == "darkblue" & row_number() == 19 ~ 80,
      color == "darkblue" & row_number() == 20 ~ -15,
      color == "darkblue" & row_number() == 21 ~ -20,
      color == "darkblue" & row_number() == 22 ~ -25,
      color == "darkblue" & row_number() == 23 ~ -30,
      color == "darkblue" & row_number() == 24 ~ -40,
      color == "darkblue" & row_number() == 25 ~ -50,
      color == "darkblue" & row_number() == 26 ~ -60,
      color == "darkblue" & row_number() == 27 ~ -70,
      color == "darkblue" & row_number() == 28 ~ -80,
      TRUE ~ NA
      
    ),
    
    mean_value = round(100 * mean_value, digits = 0)
  )

mean_long



# Make a clean group column (more semantic than color names)
plot_df <- mean_long %>%
  mutate(group = case_when(
    color == "red"      ~ "Matriculas",
    color == "darkblue" ~ "Despesas Educacionais",
    TRUE ~ "Other"
  ),
  # ensure growth categories are ordered numerically on x-axis
  growth_f = factor(growth, levels = sort(unique(growth)))
  )

# Choose colors you like
my_cols <- c("Matriculas" = "lightgreen",    # red
             "Despesas Educacionais"  = "steelblue1")    # dark blue

# dodge width controls horizontal separation; width controls bar width
pd <- position_dodge(width = 0.6)

p <- ggplot(plot_df, aes(x = growth_f, y = mean_value, fill = group)) +
          # 1) draw the Spend bars first (back)
          geom_col(data = filter(plot_df, group == "Despesas Educacionais"),
                   width = 0.55, position = pd, alpha = 0.8, color = "black") +
          # 2) draw the Enroll bars second (front)
          geom_col(data = filter(plot_df, group == "Matriculas"),
                   width = 0.55, position = pd, alpha = 0.95, color = "black") +
          # 3) add labels on top of bars (use same dodge so labels align)
          geom_text(aes(label = mean_value),
                    position = position_dodge(width = 0.6),
                    vjust = -0.5, size = 3.6, color = "black") +
          scale_fill_manual(values = my_cols, name = "Group") +
          labs(
            title = "Crescimento anual Municipal",
            x = "Threshold de crescimento (%)",
            y = "Total da amostra (%)"
          ) +
          theme_minimal(base_size = 13) +
          theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.position = "bottom"
          ) +
          # keep x ticks neat — growth are numeric categories so use them directly
          scale_x_discrete(labels = levels(plot_df$growth_f))


unique_growth <- sort(unique(plot_df$growth))     # numeric: c(-80, -70, ..., 80)


pos_le_zero <- sum(unique_growth <= 0)          

x_vline_at <- pos_le_zero + 0.5                  # e.g. 4.5

p <- p + geom_vline(xintercept = x_vline_at, color = "black", linewidth = 0.8)

p

ggsave(
  filename = paste0("dist_crescimento_2018.png"), # Nome baseado no modelo
  plot = p,
  path = "Z:/Tuffy/Paper - Educ/Resultados/v3/Figuras/ES/Robust",
  width = 600/96, height = 420/96, dpi = 110
)


rm(plot_df, mean_long, mean_df, od, p)

# --------------------------------- #
## 7.2 Saving DF flags ----
# --------------------------------- #

saveRDS(data, "Z:/Tuffy/Paper - Educ/Dados/regdf_flags.rds") #saved until 2018

