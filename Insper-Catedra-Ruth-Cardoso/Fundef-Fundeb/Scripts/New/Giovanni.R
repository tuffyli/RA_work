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

options(scipen = 999)


rm(list = ls())

# Função para adicionar label às variáveis:

add_label <- function(data, variable, label) {
  # Verifica se a variável existe no data.frame
  if (!variable %in% names(data)) {
    stop("A variável especificada não existe no data.frame.")
  }
  # Adiciona o atributo 'label' à coluna especificada
  attr(data[[variable]], "label") <- label
  return(data)
}

# Exemplo de uso:
# lb <- add_label(lb, "a", "Descrição da variável A")


#-----------------------------------------#
#--- BASE DE DADOS ANUAL POR MUNICÍPIO ---#
#-----------------------------------------#

# 1) Criação da base: transferências do Fundef e Fundeb (2007:2021)----

## 1.1) Rede Municipal:----

caminho1 <- "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/Transferências Fundeb mun e ufs/transferências_para_municípios_2000_2006.csv"
caminho2 <- "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/Transferências Fundeb mun e ufs/transferências_para_municípios_2007_2021.csv"

df_anual <- read_csv2(caminho1, locale = locale(encoding = "latin1")) %>%
  clean_names() %>% 
  bind_rows(read_csv2(caminho2, locale = locale(encoding = "latin1")) %>% clean_names()) %>% 
  arrange(codigo_ibge, ano) %>% 
  select(-codigo_siafi)

colnames(df_anual)

# Soma todos os valores; colunas de valor total da política (FUNDEB ou FUNDEF) e contribuição.
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



## 1.2) Rede Estadual:----

df_ufs <- read.csv2("C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/Transferências Fundeb mun e ufs/transferências_para_estados_2007_2021.csv",
                    fileEncoding = "latin1") %>% 
  clean_names() %>% 
  left_join(read_excel("C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/Códigos_UF.xlsx"), by = "uf") %>% 
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


df <- bind_rows(
  df_anual,
  df_ufs)


# Exportar a base:
write.csv2(df_anual, file = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/fundef_fundeb_2000_2021.csv")


# Deflacionando os valores:

ipca <- read_excel("C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/IPCA_acumulado_ano.xlsx", skip = 1)

ipca
colnames(ipca) = c("ano", "ipca")

ipca <- ipca %>% 
  mutate(ano = as.numeric(ano),
         ipca = as.numeric(str_replace(ipca, ",", "."))) %>% 
  filter(ano %in% 2000:2021) %>% 
  mutate(indice = as.numeric(ifelse(ano == 2021, 1, 0))) %>% 
  arrange(ano) %>% 
  mutate(indice = cumprod(1 + ipca/100)) %>% 
  mutate(indice = indice / indice[ano == 2021])

df <- df %>% 
  left_join(ipca,
            by = "ano") %>% 
  mutate(total_politica_d = total_politica / indice,
         total_coun_d = total_coun / indice)


rm(ipca, df_anual, df_ufs, caminho1, caminho2)

write.csv2(df, file = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/fundef_fundeb_2000_2021.csv")


# 2) Adicionando covadiadas de interesse:----
## 2.1) Notas do Saeb (outcome principal) 2005-2021 ----
### 2.1.1) Jeito manual: ----

# ideb_ef_af <- read_excel("C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/IDEB/divulgacao_anos_iniciais_municipios_2023/divulgacao_anos_iniciais_municipios_2023/divulgacao_anos_iniciais_municipios_2023.xlsx",
#                          skip = 9) %>% 
#   clean_names() %>%
#   select(-c(starts_with("vl_indicador_"), starts_with("vl_observado_"), starts_with("vl_projecao"))) %>% 
#   rename(codigo_ibge = "co_municipio",
#          nome = "no_municipio",
#          uf = "sg_uf")
# 
# colnames(ideb_ef_af)
# 
# # Aprovação:
# df_apr <- ideb_ef_af %>%
#   select(c(1:64))
# 
# df_apr <- df_apr %>%
#   pivot_longer(
#     cols = matches("^vl_aprovacao_\\d{4}_(si_4|si|[1-4])$"),
#     names_to = c("ano", "serie"),
#     names_pattern = "vl_aprovacao_(\\d{4})_(si_4|si|[1-4])",
#     values_to = "tx_aprov"
#   ) %>%
#   mutate(
#     ano = as.integer(ano),
#     serie_label = case_when(
#       serie == "si_4" ~ "iniciais",  # média dos anos iniciais
#       serie == "si"   ~ "1",         # 1º ano
#       serie == "1"    ~ "2",
#       serie == "2"    ~ "3",
#       serie == "3"    ~ "4",
#       serie == "4"    ~ "5",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   select(uf, codigo_ibge, nome, rede, ano, serie_label, tx_aprov) %>% 
#   pivot_wider(
#     names_from = serie_label,
#     names_prefix = "tx_aprovacao_",
#     values_from = tx_aprov
#   ) %>%
#   mutate(across(
#     starts_with("tx_aprovacao_"),
#     ~round(as.numeric(.), 1)
#   ))
# 
# colnames(ideb_ef_af)
# 
# df_saeb <- ideb_ef_af %>%
#   select(c(1:4, 65:94))
# 
# df_saeb <- df_saeb %>%
#   mutate(across(starts_with("vl_nota"), ~ round(as.numeric(.), 2))) %>%
#   pivot_longer(
#     cols = matches("^vl_nota_(media|matematica|portugues)_\\d{4}$"),
#     names_to = c("indicador", "ano"),
#     names_pattern = "vl_nota_(media|matematica|portugues)_(\\d{4})",
#     values_to = "valor"
#   ) %>%
#   mutate(
#     ano = as.integer(ano),
#     valor = as.numeric(valor)
#   ) %>%
#   pivot_wider(
#     id_cols = c(uf, codigo_ibge, nome, rede, ano),
#     names_from = indicador,
#     names_prefix = "vl_nota_",
#     values_from = valor
#   )
# 
# 
# # Juntando os dados em formato painel:
# 
# colnames(df_apr)
# colnames(df_saeb)
# 
# df_ideb <- left_join(
#   df_apr, df_saeb,
#   by = c("uf", "codigo_ibge", "nome", "rede", "ano")
# )
# 
# df1 <- left_join(
#   df,
#   df_ideb,
#   by = c("ano", "codigo_ibge")) %>% 
#   rename(uf = "uf.x",
#          nome = "nome.x",
#          vl_nota_matematica_5 = "vl_nota_matematica",
#          vl_nota_portugues_5 = "vl_nota_portugues",
#          vl_nota_media_5 = "vl_nota_media") %>% 
#   select(-c(uf.y, nome.y))
# 
# rm(df_apr, df_saeb, ideb_ef_af, df_ideb)

### 2.1.2) Jeito "automático" (pelo IDEB):----


ciclos <- c("anos_iniciais", 
            "anos_finais",
            "ensino_medio")

df_apr = NULL
df_saeb = NULL
for (ciclo in ciclos){
  
  ideb <- read_excel(paste0("C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/IDEB/divulgacao_", ciclo, "_municipios_2023/divulgacao_", ciclo, "_municipios_2023/divulgacao_", ciclo, "_municipios_2023.xlsx"),
                     skip = 9) %>% 
    clean_names() %>%
    select(-c(starts_with("vl_indicador_"), starts_with("vl_observado_"), starts_with("vl_projecao"))) %>% 
    rename(codigo_ibge = "co_municipio",
           nome = "no_municipio",
           uf = "sg_uf")
  
  # Aprovação:
  apr <- ideb %>%
    select(c(1:4, starts_with("vl_aprovacao_")))
  
  print(colnames(apr))
  
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
  
  rm(saeb, ideb, apr)
}  

df_ideb <- full_join(df_saeb, df_apr,
                     by = c("codigo_ibge", "rede", "uf", "ano", "nome"))

df1 <- left_join(
  df,
  df_ideb,
  by = c("ano", "codigo_ibge")) %>% 
  rename(uf = "uf.x",
         nome = "nome.x") %>% 
  select(-c(uf.y, nome.y))

rm(df_apr, df_saeb, df_ideb, ciclos, ciclo)





## 2.2) Notas do SAEB + Taxa de Aprovação (faltantes):----

# Como o IDEB foi criado em 2007, as notas do Saeb e as Taxas de Aprovação só estão disponíveis dessa forma para 2005 em diante.
# Então, temos que colocar a Tx. de Aprovação manualmente de 2000 a 2004 para o EF, e de 2000 a 2016 para o EM.
# Para as notas do Saeb, precisamos dos dados de 2001 e 2003 para tendências paralelas;
# Por enquanto, deixarem sem essas informações.




## 2.3) Transferências do Fundeb: ----

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



### 2.3.1) Baixando as planilhas pelo Portal de Dados Abertos do FNDE (RODAR UMA VEZ SÓ): ----


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



### 2.3.2) Junta os dataframes de UF por ano e criar arquivos ano a ano (RODAR UMA VEZ SÓ): ----

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


### 2.3.3) Importar todos os anos para uma lista: ----

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



### 2.3.4) Manipulação - Criar "Transferências Líquidas"----
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



### 2.3.5) Deflacionar:----

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



### 2.3.6) Juntar tudo e exportar:----


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

# 3) Simulação: quanto os municípios ganhariam se os pesos do FUNDEF tivessem se mantido?----

# Precisamos:
# Pesos do FUNDEF
# Dados do Censo Escolar de 2006
# Matrículas em cada rede de ensino por município
# Valor total do FUNDEB do Estado; soma das deduções + complementações


### 3.1) Censo Escolar de 2006 (deixar comentado):----

# censo <- read_delim("Z:/Arquivos IFB/Censo Escolar/Bases Agregadas/2006/microdados_educação_básica_2006/DADOS/CENSOESC_2006.CSV", delim = "|",
#                              col_types = cols(.default = col_double(),
#                                               CODFUNC = col_character(),
#                                               MASCARA = col_character(),
#                                               DEP     = col_character(),
#                                               LOC = col_character(),
#                                               UF = col_character(),
#                                               SIGLA = col_character(),
#                                               MUNIC = col_character(),
#                                               ED_INDIG = col_character(),
#                                               MAT_QUIL = col_character(),
#                                               MAT_ETNI = col_character(),
#                                               NIVELCRE = col_character(),
#                                               NIVELPRE = col_character(),
#                                               NIV_F1A4_8 = col_character(),
#                                               NIV_F5A8_8 = col_character(),
#                                               NIV_F9INI = col_character(),
#                                               NIV_F9FIM = col_character(),
#                                               NIVELMED = col_character(),
#                                               NIVM_INT = col_character(),
#                                               SUPL_AVA = col_character(),
#                                               SUPL_SAVA = col_character(),
#                                               EDPROFIS = col_character(),
#                                               ESP_EXCL = col_character(),
#                                               ESP_T_ES = col_character(),
#                                               ENS_INCL = col_character(),
#                                               ESC_ASSE = col_character(),
#                                               AREA_QUIL = col_character(),
#                                               ESP_S_RE = col_character(),
#                                               ESP_A_IN = col_character(),
#                                               ED_INDIG = col_character(),
#                                               ED_IN_LM = col_character(),
#                                               COD_ID_IND = col_character(),
#                                               ED_IN_LP = col_character(),
#                                               MAT_ETNI = col_character(),
#                                               MAT_QUIL = col_character(),
#                                               ESC_T_IN = col_character(),
#                                               PRED_ESC = col_character(),
#                                               TEMPLO = col_character(),
#                                               DEF11C = col_double(),  
#                                               DEF11D = col_double(),  
#                                               DEF11E = col_double(),  
#                                               DEF11F = col_double(),  
#                                               NEF11C = col_double(),  
#                                               NEF11D = col_double(),  
#                                               NEF11E = col_double(),  
#                                               NEF11F = col_double(),  
#                                               DEF11G = col_double(),  
#                                               DEF11H = col_double(),  
#                                               DEF11I = col_double(),  
#                                               DEF11J = col_double(),  
#                                               NEF11G = col_double(),  
#                                               NEF11H = col_double(),  
#                                               NEF11I = col_double(),  
#                                               NEF11J = col_double()))
#                                               
# ### 3.1.1) Agregação das matrículas de cada ciclo por ESCOLA: ----
# mat_2006 <- censo %>%
#   select(c(1:9,
# 
#            CODFUNC,
#            ED_INDIG,
#            MAT_QUIL,
#            AREA_QUIL,
#            ESC_ASSE,
#            ED_IN_LM,
#            ED_IN_LP,
#            MAT_ETNI,
#            ESC_T_IN,
# 
#            DEF11C:DEF11F, NEF11C:NEF11F, # EF iniciais (8 anos)
#            DE9F11C:DE9F11G, NE9F11C:NE9F11G, # EF iniciais (9 anos)
#            DEF11G:DEF11J, NEF11G:NEF11J, # EF finais (8 anos)
#            DE9F11H:DE9F11N, NE9F11H:NE9F11N, # EF finais (9 anos)
#            VEE1431:VEE1437, # Alunos de educação especial do EF por ano de nascimento
# 
#            #ED especial por série:
#            VEE1619:VEE1691, VEE1719:VEE1791, VEE1819:VEE1891, VEE1919:VEE1991, # 1ºEF
#            
#            VEE1612:VEE1692, VEE1712:VEE1792, VEE1812:VEE1892, VEE1912:VEE1992, # 2ºEF,
#            VEE1613:VEE1693, VEE1713:VEE1793, VEE1813:VEE1893, VEE1913:VEE1993, # 3ºEF
#            VEE1614:VEE1694, VEE1714:VEE1794, VEE1814:VEE1894, VEE1914:VEE1994, # 4ºEF
#            VEE1615:VEE1695, VEE1715:VEE1795, VEE1815:VEE1895, VEE1915:VEE1995, # 5ºEF
#            VEE1616:VEE1696, VEE1716:VEE1796, VEE1816:VEE1896, VEE1916:VEE1996, # 6ºEF
#            VEE1617:VEE1697, VEE1717:VEE1797, VEE1817:VEE1897, VEE1917:VEE1997, # 7ºEF
#            VEE1618:VEE1698, VEE1718:VEE1798, VEE1818:VEE1898, VEE1918:VEE1998, # 8ºEF
# 
#            DEM118, DEM119, DEM11A, DEM11B, DEM11C, # Alunos do EM
#            NEM118, NEM119, NEM11A, NEM11B, NEM11C,
# 
#            DPE119, NPE119, # Alunos de Creche
#            DPE11D, NPE11D, # Alunos de Pré-Escola
# 
#            DES101F:DES101A, NES101F:NES101A # Alunos do EJA
#   )) %>%
# 
#   mutate(COD_UF = as.numeric(str_sub(as.character(CODMUNIC, 1, 2)))) %>%
#   mutate(reg_in = rowSums(across(c(DEF11C:NE9F11G)), na.rm = TRUE), # Contando EF de 8 e 9 anos!
#          reg_fin = rowSums(across(c(DEF11G:NE9F11N)), na.rm = TRUE),
#          esp_iniciais = rowSums(across(c(VEE1619:VEE1994)), na.rm = TRUE),
#          esp_finais = rowSums(across(c(VEE1615:VEE1998)), na.rm = TRUE),
#          esp_soma = esp_iniciais + esp_finais,
#          esp_tot = rowSums(across(c(VEE1431:VEE1437)), na.rm = TRUE),
#          em_tot = rowSums(across(c(DEM118:NEM11C)), na.rm = TRUE),
#          creche = rowSums(across(c(DPE119, NPE119)), na.rm = TRUE),
#          pre_escola = rowSums(across(c(DPE11D, NPE11D)), na.rm = TRUE),
#          ed_inf_tot = rowSums(across(c(DPE119:NPE11D)), na.rm = TRUE),
#          eja_tot = rowSums(across(c(DES101F:NES101A)), na.rm = TRUE),
#          dif = esp_iniciais + esp_finais - esp_tot,
#          esp_total_final = pmax(esp_soma, esp_tot)) %>%  # pega o maior valor entre a soma das deficiencias por série,
#   # e da desagregação pelo ano de nascimento
#   filter(!(DEP %in% c("Particular", "Federal")), # tira escolas federais e particulares
#          CODFUNC == "Ativo") %>%  # tira escolas que não estão ativas
#   mutate(CODMUNIC = as.numeric(str_c( #concatena
#     str_sub(as.character(CODMUNIC), 1, 2), #pega os dois primeiros
#     str_sub(as.character(CODMUNIC), -5) #pega os últimos 5
#   ))) %>%
#   mutate(ind_quil = ifelse(
#     (coalesce(ED_INDIG, "n") == "s" |
#        coalesce(MAT_QUIL, "n") == "s" |
#        coalesce(AREA_QUIL, "n") == "s" |
#        coalesce(ED_IN_LM, "n") == "s" |
#        coalesce(ED_IN_LP, "n") == "s"),
#     1, 0
#   ))
# 
# # DF do censo já agregada por escola (é muito pesada para rodar toda vez):
# write.csv2(mat_2006, file = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/censo_2006_filtrado.csv")


### 3.1.2) Agregação por MUNICÍPIO: ----

mat_2006 <- read.csv2("C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/censo_2006_filtrado.csv")

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
    # mat_creche = sum(creche, na.rm = TRUE),
    # mat_pre = sum(pre_escola, na.rm = TRUE),
    mat_tot_inf = sum(ed_inf_tot, na.rm = TRUE),
    mat_tot_eja = sum(eja_tot, na.rm = TRUE),
    .groups = "drop"
  )

colnames(mat_2006_munic)

# teste <- mat_2006 %>%
#   filter(MUNIC == "ALTA FLORESTA D'OESTE") %>%
#   relocate(ind_quil, .after = "MUNIC")



# A partir destes dados, extrairemos as matrículas do EF Iniciais e EF Finais que estão
# na educação indígena, que são computadas de forma diferente no arquivo das ponderações 
# de 2007. Devemos pegar estas matrículas indígenas e quilombolas e considerá-las como
# matrículas regulares do EF. 

write.csv2(mat_2006_munic, file = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/censo_2006_filtrado_mun.csv")


## 3.2) Simulação: dados do Censo (simulação antiga): ----

### 3.2.1) Coeficiente de Distribuição simulado:----


# Fatores de ponderação do FUNDEF
# FD1 <- 1.00  # 1ª a 4ª série regular
# FD2 <- 1.05  # 5ª a 8ª série regular + educação especial

mat_2006 <- read.csv2("C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/censo_2006_filtrado_mun.csv") %>% 
  add_label("mat_reg_in", "Mat. Fund. Iniciais regular (CENSO)") %>%
  add_label("mat_reg_fin", "Mat. Fund. Finais regular (CENSO)") %>% 
  add_label("mat_tot_em", "Mat. EM (CENSO)") %>% 
  add_label("mat_tot_inf", "Mat. Infantil (CENSO)") %>% 
  # add_label("mat_creche", "Mat. Creche (CENSO)") %>% 
  # add_label("mat_pre", "Mat. Pré-Escola (CENSO)") %>% 
  add_label("mat_tot_eja", "Mat. EJA (CENSO)") %>% 
  add_label("mat_tot_esp", "Mat. Especiais (CENSO)")

# Calcula totais por estado (TA_*)
estaduais <- mat_2006 %>%
  group_by(uf) %>%
  summarise(
    # TA_1_4 = sum(mat_reg_in, na.rm = TRUE),
    # TA_5_8 = sum(mat_reg_fin, na.rm = TRUE),
    TA_esp = sum(mat_tot_esp, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  add_label("TA_esp", "Total de Mat. Especiais na UF")

# Junta totais estaduais ao dataset municipal

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
    # TA_1_4 = first(TA_1_4),  # Garante que o total por UF seja replicado corretamente
    # TA_5_8 = first(TA_5_8),  # Mesma lógica para TA_5_8
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
# add_label("mat_tot_em", "Mat. EM (CENSO)") %>% 
# add_label("mat_tot_inf", "Mat. Infantil (CENSO)") %>% 
# add_label("mat_tot_eja", "Mat. EJA (CENSO)") %>% 


# mat_cd <- mat_cd %>%
#   mutate(
#      num_cd = FD1 * mat_reg_iniciais + FD2 * (mat_reg_finais + mat_tot_esp),
# 
#    # DENOMINADOR DO CD MUNICIPAL:
#    den_cd = FD1 * TA_1_4 + FD2 * (TA_5_8 + TA_esp),
# 
#    # Coeficiente de Distribuição
#    coef_simulado1 = num_cd / den_cd
# )

# rm(mat_2006)


#----------------------#
#----------------------#

# teste <- mat_cd %>% slice(0)
# 
# teste <- teste %>% 
#   add_row(
#     CODMUNIC = 9999999,
#     DEP = "Municipal",
#     nome = "Município Teste",
#     uf = "XX",
#     mat_reg_iniciais = 3808,
#     mat_reg_finais   = 2100,
#     mat_tot_esp      = 208,
#     TA_1_4 = 120000,
#     TA_5_8 = 110000,
#     TA_esp = 5605,
#     num_cd = NA,         # será calculado
#     den_cd = NA,         # será calculado
#     coef_fundef = NA     # será calculado
#   )
# 
# teste <- teste %>% 
#   mutate(
#     num_cd = FD1 * mat_reg_iniciais + FD2 * (mat_reg_finais + mat_tot_esp),
#     
#     # DENOMINADOR DO CD MUNICIPAL:
#     den_cd = FD1 * TA_1_4 + FD2 * (TA_5_8 + TA_esp),
#     
#     # Coeficiente de Distribuição
#     coef_fundef = num_cd / den_cd
#   )
# 
# rm(teste)
#----------------------#
#----------------------#


### 3.2.2) Cálculo do valor recebido simulado: ----

# Cálculo do valor do FUNDEB em 2007:

# df_fundo <- df %>% 
#   select(codigo_ibge, ano, uf, nome, total_politica, total_politica_d) %>%
#   distinct() %>% 
#   group_by(ano, uf) %>% 
#   summarise(total_fundo = sum(total_politica, na.rm = TRUE),
#             total_fundo_d = sum(total_politica_d, na.rm = TRUE),
#             .groups = "drop") %>% 
#   filter(ano == 2007)
# 
# 
# mat_cd <- mat_cd %>%
#   left_join(df_fundo, by = "uf") %>% 
#   mutate(valor_simulado1 = coef_simulado1 * total_fundo)
# 
# rm(df_fundo, estaduais)





## 3.3) Usando as matrículas oficiais do FNDE ----

### 3.3.1) Importar todas as UFs para uma lista:----

lista_ufs <- list()

pasta <- "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/ponderacao_matriculas_2007/excel/"

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



#### 3.3.1.1) Conferindo se está correto:----

# APÓS CONFERIR, DEIXE COMENTADO.

# for (uf in names(lista_ufs)) {
#   df <- lista_ufs[[uf]]
# 
#   df_sem_total <- df %>% filter(!grepl("total", nome, ignore.case = TRUE))
# 
#   total_row <- df_sem_total %>%
#     select(-nome) %>%
#     summarise(across(everything(), sum, na.rm = TRUE)) %>%
#     mutate(nome = "Soma") %>%
#     relocate(nome)
# 
#   total_line <- df %>%
#     filter(grepl("total", nome, ignore.case = TRUE) & ((!grepl("brasil", nome, ignore.case = TRUE)) |
#                                                          !grepl("br", nome, ignore.case = TRUE)))  %>%
#     select(-nome) %>%
#     summarise(across(everything(), sum, na.rm = TRUE))
# 
#   diff_row <- total_line - total_row %>% select(-nome)
#   diff_row <- diff_row %>%
#     mutate(nome = "Diferença (Total - Soma)") %>%
#     relocate(nome)
# 
#   lista_ufs[[uf]] <- bind_rows(df, total_row, diff_row)
# }
# 
# 
# diferencas <- list()
# 
# for (uf in names(lista_ufs)) {
# 
#   df <- lista_ufs[[uf]]
# 
# 
#   linha_diferenca <- df %>% filter(nome == "Diferença (Total - Soma)")
# 
# 
#   diferencas[[uf]] <- linha_diferenca
# }
# 
# diferencas


### 3.3.2) Juntar tudo num Dataframe só:----

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

# rm(lista_ufs)

### 3.3.3) Calculando a proporção de alunos em cada ciclo pelos dados do Censo:

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


### 3.3.4) Adicionando matrículas indígenas e especiais pelo Censo: ----

#### 3.3.4.1) Especiais: ----

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
  left_join(mat_cd %>% select(chave, codigo_ibge, mat_tot_esp, TA_esp, prop_1_4_mun, prop_5_8_mun), by = "chave")

#### 3.3.4.2) Ed. Indígena e Quilombola: ----

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




### AGORA TEMOS TODOS OS DADOS QUE PRECISAMOS PARA SIMULAR O COEFICIENTE DE DISTRIBUIÇÃO PELAS REGRAS DO FUNDEF:

## 3.4) Simulação: ----

# 1. Total por estado (TA = total alunos por estado por faixa)
totais_estado2 <- df_fnde %>%
  group_by(uf) %>%
  summarise(
    # Calcular a proporção de matrículas em cada ciclo, para alocar as matrículas integrais:
    mat_1_4 = sum(mat_fund_in_urb, na.rm = TRUE) + sum(mat_fund_in_rur, na.rm = TRUE) + sum(mat_reg_in_ind, na.rm = TRUE),
    mat_5_8 = sum(mat_fund_fin_urb, na.rm = TRUE) + sum(mat_fund_fin_rur, na.rm = TRUE) + sum(mat_reg_fin_ind, na.rm = TRUE),
    
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
    # mat_fund_aux = mat_1_4_aux + mat_5_8_aux, # matrículas totais do EF regular
    
    # prop_1_4_aux = ifelse(mat_fund_aux > 0, mat_1_4_aux / mat_fund_aux, 0), # proporção do município de alunos nos Iniciais
    # prop_5_8_aux = ifelse(mat_fund_aux > 0, mat_5_8_aux / mat_fund_aux, 0), # proporção do município de alunos nos Finais
    
    numerador = FD1 * (mat_1_4_aux + mat_fund_integ * (prop_1_4)) + # FD1 * (NA1/4 + integral)
      # O último termo é a qtd. de matrículas integral no município vezes a proporção de alunos nos anos iniciais do EF nesse município.
      FD2 * (mat_5_8_aux + mat_fund_integ * (prop_5_8) + mat_tot_esp), # FD2 * (NA5/8 + integral + NAe)
    
    denominador = FD1 * TA_1_4 + FD2 * (TA_5_8 + TA_esp),
    
    coef_simulado = numerador / denominador # Porcentagem (em decimal) do Fundo Estadual que a rede de ensino pega.
    
  ) 



# Colocando os valores totais dos fundos estaduais:

df <- read.csv2("C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/painel_notas_transferencias_2000_2024.csv")

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
         
         shr_inf_em = 100 * ((mat_creche + mat_pre + mat_inf_ind +  mat_em_urb + mat_em_rur + mat_em_ind + mat_em_prof)/
                               (mat_creche + mat_pre + mat_inf_ind + 
                                  mat_fund_in_urb + mat_fund_in_rur + mat_reg_in_ind + 
                                  mat_fund_fin_urb + mat_fund_fin_rur + mat_reg_fin_ind + 
                                  mat_fund_integ + # todas as matrículas integrais do EF
                                  mat_em_urb + mat_em_rur + mat_em_ind + mat_em_prof + 
                                  mat_eja + mat_eja_ind +
                                  mat_tot_esp))) %>% 
  
  select(c(nome, uf, codigo_ibge, chave, total_fundo_real, coef_est_fnde, coef_simulado, receita_est_fnde, receita_simulada, everything())) %>% 
  add_label("dif_coef_pp", "Dif. (p.p.) do Coef. com o FUNDEB vs FUNDEF (2007)") %>% 
  add_label("dif_per_coef", "Dif. (%) do Coef. com o FUNDEB vs FUNDEF (2007)") %>% 
  add_label("shr_inf_em", "Share de estudantes Infantil + EM (2006)") %>% 
  add_label("receita_simulada", "Receita simulada caso a regra do FUNDEF se mantivesse (considerando compl.)") %>% 
  add_label("coef_simulado", "Coef. de Distrib. caso a regra do FUNDEF se mantivesse (considerando compl.)") 





# Valores distribuídos a cada município:

# Padronizar as chaves em df_fundeb_munic
df_fundeb_munic <- df %>%
  # mutate(chave = ifelse(codigo_ibge < 100, paste0("GOVERNO ESTADUAL_", uf), paste0(nome, "_", uf))) %>% 
  # mutate(
  #   chave = str_to_upper(str_replace_all(
  #     stringi::stri_trans_general(chave, "Latin-ASCII"), "['`]", ""
  #   )),
  #   chave = str_replace_all(chave, "\\s+", " ")  # Substitui múltiplos espaços por um único espaço
  # ) %>% 
  filter(ano == 2007) %>% 
  select(total_politica,
         # chave,
         codigo_ibge) %>% 
  distinct()

# Padronizar as chaves em simulacao
# simulacao <- simulacao %>%
#   mutate(
#     chave = str_to_upper(str_replace_all(
#       stringi::stri_trans_general(chave, "Latin-ASCII"), "['`]", ""
#     )),
#     chave = str_replace_all(chave, "\\s+", " ")  # Substitui múltiplos espaços por um único espaço
#   )


# Função para padronizar as chaves
# padronizar_chaves <- function(chave) {
#   chave %>%
#     str_to_upper() %>%                          # Converter para maiúsculas
#     str_replace_all("[\\s-]+", " ") %>%         # Substituir espaços e hifens por um único espaço
#     str_replace_all("['`]", "") %>%             # Remover apóstrofes e crases
#     str_trim()                                  # Remover espaços extras
# }
# 
# # Função para corrigir chaves específicas manualmente
# ajustar_chave <- function(chave) {
#   case_when(
#     # Ajustes para Alagoas (AL)
#     chave == "MAJOR IZIDORO_AL" ~ "MAJOR ISIDORO_AL",
#     chave == "OLHO D AGUA DAS FLORES_AL" ~ "OLHO DAGUA DAS FLORES_AL",
#     chave == "OLHO D AGUA DO CASADO_AL" ~ "OLHO DAGUA DO CASADO_AL",
#     chave == "OLHO D AGUA GRANDE_AL" ~ "OLHO DAGUA GRANDE_AL",
#     chave == "TANQUE D ARCA_AL" ~ "TANQUE DARCA_AL",
#     
#     # Ajustes para Bahia (BA)
#     chave == "DIAS D AVILA_BA" ~ "DIAS DAVILA_BA",
#     chave == "GOVERNADOR LOMANTO JUNIOR_BA" ~ "BARRO PRETO_BA",
#     
#     # Ajustes para Distrito Federal (DF)
#     chave == "DISTRITO FEDERAL_DF" ~ "BRASILIA_DF",
#     
#     # Ajustes para Minas Gerais (MG)
#     chave == "DONA EUZEBIA_MG" ~ "DONA EUSEBIA_MG",
#     chave == "OLHOS DAGUA_MG" ~ "OLHOS D AGUA_MG",
#     chave == "PINGO DAGUA_MG" ~ "PINGO D AGUA_MG",
#     chave == "TOCOS DO MOJI_MG" ~ "TOCOS DO MOGI_MG",
#     chave == "PONTE CHIQUE_MG" ~ "PONTO CHIQUE_MG",
#     
#     # Ajustes para Paraíba (PB)
#     chave == "CAMPO GRANDE_RN" ~ "AUGUSTO SEVERO_RN",
#     chave == "JOCA CLAUDINO_PB" ~ "SANTAREM_PB",
#     chave == "SAO DOMINGOS_PB" ~ "SAO DOMINGOS DE POMBAL_PB",
#     chave == "ITAMARACA_PE" ~ "ILHA DE ITAMARACA_PE",
#     chave == "TACIMA_PB" ~ "CAMPO DE SANTANA PB",
#     
#     # Ajustes para Piauí (PI)
#     chave == "AROEIRAS DO ITAIM_PI" ~ "AROEIRA DO ITAIM_PI",
#     chave == "BARRA DALCANTARA_PI" ~ "BARRA D ALCANTARA_PI",
#     chave == "CAPITAO GERVASIO DE OLIVEIRA_PI" ~ "CAPITAO GERVASIO OLIVEIRA_PI",
#     chave == "OLHO DAGUA DO PIAUI_PI" ~ "OLHO D AGUA DO PIAUI_PI",
#     chave == "PAU DARCO_PI" ~ "PAU DARCO DO PIAUI_PI",
#     
#     # Ajustes para Paraná (PR)
#     chave == "BELA VISTA DA CAROBA_PR" ~ "BELA VISTA DO CAROBA_PR",
#     chave == "GOIOERE_PR" ~ "GOIO ERE_PR",
#     
#     # Ajustes para Santa Catarina (SC)
#     chave == "SAO MIGUEL DOESTE_SC" ~ "SAO MIGUEL DO OESTE_SC",
#     
#     # Ajustes para Rondônia (RO)
#     chave == "SAO FELIPE DOESTE_RO" ~ "SAO FELIPE D OESTE_RO",
#     chave == "BATAIPORA_MS" ~ "BATAYPORA_MS",
#     chave == "EMBU_SP" ~ "EMBU DAS ARTES_SP",
#     chave == "CHIAPETA_RS" ~ "CHIAPETTA_RS",
#     chave == "FORTALIZA DO TABOCAO_TO" ~"TABOCAO_TO",
#     chave == "CAMPO DE SANTANA PB" ~ "CAMPO DE SANTANA_PB",
#     chave == "CUVERLANDIA_MT" ~ "CURVELANDIA_MT",
#     chave == "PRESIDENTE CASTELLO BRANCO_SC" ~ "PRESIDENTE CASTELO BRANCO_SC",
#     chave == "PARATY_RJ" ~ "PARATI_RJ",
#     chave == "SAO VALERIO DA NATIVIDADE_TO" ~ "SAO VALERIO_TO",
#     chave == "FORTALEZA DO TABOCAO_TO" ~ "TABOCAO_TO",
#     # Ajustes para outros casos
#     TRUE ~ chave  # Mantém a chave original se não houver correspondência
#   )
# }
# 
# # Padronizar e ajustar chaves em `df_fundeb_munic`
# df_fundeb_munic <- df_fundeb_munic %>%
#   mutate(
#     chave = padronizar_chaves(chave),        # Padroniza as chaves
#     chave = ajustar_chave(chave)            # Aplica ajustes manuais
#   )
# 
# # Padronizar e ajustar chaves em `simulacao`
# simulacao <- simulacao %>%
#   mutate(
#     chave = padronizar_chaves(chave),        # Padroniza as chaves
#     chave = ajustar_chave(chave)            # Aplica ajustes manuais
#   )

# # Recalcular as diferenças entre as chaves
# chaves_fundeb <- df_fundeb_munic %>% pull(chave) %>% unique()
# chaves_simulacao <- simulacao %>% pull(chave) %>% unique()
# 
# chaves_unicas_fundeb <- setdiff(chaves_fundeb, chaves_simulacao)  # Presentes apenas no df_fundeb_munic
# chaves_unicas_simulacao <- setdiff(chaves_simulacao, chaves_fundeb)  # Presentes apenas no simulacao
# 
# # Exibir resultados
# print("Chaves presentes apenas em df_fundeb_munic:")
# print(chaves_unicas_fundeb)
# 
# print("Chaves presentes apenas em simulacao:")
# print(chaves_unicas_simulacao)


simulacao <- simulacao %>% 
  # left_join(df_fundeb_munic, by = "chave") %>% 
  left_join(df_fundeb_munic, by = "codigo_ibge") %>% 
  rename(receita_real = "total_politica") %>%
  select(c(codigo_ibge,
           # chave,
           nome, uf, coef_est_fnde, coef_simulado, dif_per_coef, dif_coef_pp, receita_est_fnde, receita_simulada, receita_real, shr_inf_em, everything())) %>% 
  add_label("receita_real", "Receita recebida pelo FUNDEB (2007)")


# teste <- simulacao %>% 
#   filter(!is.finite(dif_per_coef))
# 
# teste_inf <- df %>% 
#   select(c(2:6)) %>% 
#   distinct() %>% 
#   filter(codigo_ibge %in% teste$codigo_ibge,
#          ano %in% c(2006)) 




# No fim, utilizamos o Censo Escolar de 2006 para:
# Computar as matrículas da Educação Especial da forma como era feita no FUNDEF;
# Computar as matrículas indígenas como matrículas do EF.

## 3.5) Simulação pelo VAA: ----

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
# FD3_int = 1.3
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
  
  mutate(d_vaa = 100*((VAA_1_4_real - VAA_1_4_simulado) / VAA_1_4_simulado)) %>% 
  relocate(d_vaa, .after = dif_per_coef) %>% 
  add_label("d_vaa", "Dif. (%) do VAA simulado pro real")



### 3.5.1) Adicionando número de alunos:----

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





# 4) Adicionando mais variáveis de controle: ----
## 4.1) Despesas com educação:----

# # Carregar o JSON
# json_data <- read_json("C:/Users/giovannioz/Downloads/Despesas_Siope.json", simplifyVector = TRUE) %>%
#   as_tibble()
# 
# # Transformar todas as colunas em character
# json_data$value <- json_data$value %>%
#   mutate(across(everything(), as.character))
# 
# # Converter para tibble
# json_data <- as_tibble(json_data$value)
# 
# colnames(json_data)
# 
# # Transformar todas as colunas, exceto NOM_MUNI, SIG_UF e TIPO, para numeric
# json_data <- json_data %>%
#   mutate(across(-c(NOM_MUNI, SIG_UF, TIPO, NOM_PAST, IDN_EXIB_CODI, TIP_PASTA, COD_FONTE:NOM_COLU), as.numeric))
# 
# # cat(json_data$VL_DESP_LIQU_EDU[1])
# 
# print(unique(json_data$NOM_PAST))



# # Pasta destino para salvar os arquivos JSON
# pasta_destino <- "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/SIOPE_JSON"
# if (!dir.exists(pasta_destino)) dir.create(pasta_destino, recursive = TRUE)
# # 
# # Vetor de UFs
# ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "ES", "GO", "MA",
#          "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN",
#          "RO", "RR", "RS", "SC", "SE", "SP", "TO") #sem DF
# # 
# # Função para obter o período correto
# f_periodo <- function(ano) ifelse(ano <= 2016, 1, 6)
# # 
# # Lista para registrar falhas
# falhas <- list()
# # 
# # Loop
# for (ano in 2005:2024) {
#   for (uf in ufs) {
# 
#     periodo <- f_periodo(ano)
# 
#     
#     #     url <- glue(
#     #       "https://www.fnde.gov.br/olinda-ide/servico/DADOS_ABERTOS_SIOPE/versao/v1/odata/",
#     #       "Receita_Siope(Ano_Consulta=@Ano_Consulta,Num_Peri=@Num_Peri,Sig_UF=@Sig_UF)",
#     #       "?@Ano_Consulta={ano}&@Num_Peri={periodo}&@Sig_UF='{uf}'",
#     #       "&$format=text/csv",
#     #       "&$select=TIPO,NUM_ANO,NUM_PERI,COD_UF,SIG_UF,COD_MUNI,NOM_MUNI,",
#     #       "COD_EXIB_FORMATADO,NOM_ITEM,IDN_CLAS,NOM_COLU,NUM_NIVE,NUM_ORDE,VAL_DECL"
#     #     )
#     #     
#     #     caminho_arquivo <- file.path(pasta_destino, glue("siope_{uf}_{ano}_P{periodo}.csv"))
#     
#     
#     url <- glue(
#       "https://www.fnde.gov.br/olinda-ide/servico/DADOS_ABERTOS_SIOPE/versao/v1/odata/",
#       "Dados_Gerais_Siope(Ano_Consulta=@Ano_Consulta,Num_Peri=@Num_Peri,Sig_UF=@Sig_UF)",
#       "?@Ano_Consulta={ano}&@Num_Peri={periodo}&@Sig_UF='{uf}'",
#       "&$format=json",
#       "&$select=TIPO,NUM_ANO,NUM_PERI,COD_UF,SIG_UF,COD_MUNI,NOM_MUNI,",
#       "NUM_POPU,VAL_PIB,VAL_PIB_PERCAPTO,VAL_RECE_REAL,VAL_RECE_ORCA,",
#       "VAL_DESP_EMPE,VAL_DESP_LIQU,VAL_DESP_PAGA,VAL_DESP_ORCA,VL_DESP_EMPE_EDU,VL_DESP_LIQU_EDU,VL_DESP_PAGA_EDU,VL_DESP_ORCA_EDU"
#     )
# 
#     caminho_arquivo <- file.path(pasta_destino, glue("siope_{uf}_{ano}.json"))
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
#     
#     json_data <- read_json(caminho_arquivo, simplifyVector = TRUE) %>%
#       as_tibble()
#     
#     # Transformar todas as colunas em character
#     json_data$value <- json_data$value %>%
#       mutate(across(everything(), as.character))
#     
#     # Converter para tibble
#     json_data <- as_tibble(json_data$value)
#     
#     colnames(json_data)
#     
#     # Transformar todas as colunas, exceto NOM_MUNI, SIG_UF e TIPO, para numeric
#     json_data <- json_data %>%
#       mutate(across(-c(NOM_MUNI, SIG_UF, TIPO), as.numeric))
#     
#     # Exportando:
#     caminho_saida <- file.path(pasta_destino, glue("siope_{uf}_{ano}.xlsx"))
#     write_xlsx(json_data, path = caminho_saida)
#     message(glue("📁 Arquivo XLSX criado para {uf}-{ano}"))
#   }
#   rm(resp, uf, url, ano, periodo, resultado)
# }


# # 4.2) Juntar todos os anos em um df só:
# # Definir a pasta onde estão os arquivos XLSX
# pasta_destino <- "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/SIOPE_JSON"
# 
# # Listar todos os arquivos XLSX da pasta
# arquivos_xlsx <- list.files(pasta_destino, pattern = "\\.xlsx$", full.names = TRUE)
# 
# # Ler e combinar os arquivos em um único dataframe
# dados_combinados <- arquivos_xlsx %>%
#   lapply(read_excel) %>%
#   bind_rows() %>% 
#   filter(TIPO == "Municipal",
#          !is.na(VL_DESP_EMPE_EDU)) 
# 
# 
# # Combinar os arquivos JSON em um único dataframe
# lista_json <- list()
# 
# for (ano in anos) {
#   caminho_arquivo <- file.path(pasta_destino, paste0("dados_siope_", ano, ".json"))
#   
#   if (!file.exists(caminho_arquivo)) {
#     message(glue::glue("❌ Arquivo não encontrado para o ano {ano}"))
#     next
#   }
#   
#   # Ler e processar o JSON
#   json_data <- fromJSON(caminho_arquivo, flatten = TRUE)
#   
#   # Adicionar ao data frame
#   df <- as_tibble(json_data$value) %>%
#     select(all_of(campos)) %>%
#     mutate(across(-c(TIPO, SIG_UF, NOM_MUNI), as.numeric))
#   
#   lista_json[[as.character(ano)]] <- df
#   message(glue::glue("✅ Ano {ano} processado com sucesso."))
# }
# 
# # Combinar os anos em um único dataframe
# dados_gerais <- bind_rows(lista_json)
# 
# # Salvar o resultado final
# caminho_saida <- "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/dados_siope_geral_2005_2024.csv"
# write_csv(dados_gerais, caminho_saida)
# message(glue::glue("✅ Todos os dados foram processados e salvos em ", caminho_saida))
# 
# # Visualização final
# glimpse(dados_gerais)
# 














# 4) Análise descritiva: ----


### 4.1) Resultado: ----

# 91 municípios que não ganhavam nada de FUNDEF passaram a receber verbas;
# 919 municípios tiveram um aumento do coeficiente de distribuição da política;
# O aumento mediano foi de 
print(median((simulacao %>% filter(dif_per_coef >0))$dif_per_coef))
# [1] 4.790533
# 4668 tiveram uma diminuição do coeficiente de distribuição;
# A queda mediana foi de 
print(median((simulacao %>% filter(dif_per_coef < 0))$dif_per_coef))
# [1] -6.716077



## 4.2) Gráficos e Mapas ----
#### 4.2.1) Share de matrículas infantis e de EM?----

# Quais regiões? 

simulacao <- simulacao %>% 
  mutate(regiao = case_when(
    simulacao$uf %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    simulacao$uf %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    simulacao$uf %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    simulacao$uf %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
    simulacao$uf %in% c("PR", "SC", "RS") ~ "Sul",
    TRUE ~ NA_character_ # Caso a UF não corresponda a nenhuma região
  ))

# Criar colunas de dummies para cada região
simulacao <- simulacao %>%
  mutate(
    dummy_norte = ifelse(regiao == "Norte", 1, 0),
    dummy_nordeste = ifelse(regiao == "Nordeste", 1, 0),
    dummy_centro_oeste = ifelse(regiao == "Centro-Oeste", 1, 0),
    dummy_sudeste = ifelse(regiao == "Sudeste", 1, 0),
    dummy_sul = ifelse(regiao == "Sul", 1, 0),
    benef = ifelse(dif_per_coef > 0, 1, 0)) %>% 
  relocate(regiao, .after = uf) %>% 
  filter(!is.na(dif_per_coef))

regioes <- simulacao %>% 
  group_by(benef) %>% 
  summarise(
    Norte = mean(dummy_norte),
    Nordeste = mean(dummy_nordeste),
    `Centro-Oeste` = mean(dummy_centro_oeste),
    Sudeste = mean(dummy_sudeste),
    Sul = mean(dummy_sul),
    .groups = "drop"
  ) %>% 
  filter(is.finite(benef)) %>% 
  rename(`Δ CD` = benef) %>% 
  mutate(`Δ CD` = case_when(
    `Δ CD` == 1 ~ "Positivo",
    `Δ CD` == 0 ~ "Negativo"
  ))

print(xtable(regioes), type = "latex", file = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Resultados/Fundeb x Fundef/Tabelas/regioes_sim.tex")


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


#### Variação do VAA recebido pela política comparado ao cenário em que o FUNDEF permaneceu:

# Filtrar para remover linhas com valores inválidos em `dif_per_coef` ou `shr_inf_em`
simulacao_filtrada <- simulacao %>%
  filter(is.finite(d_vaa) & is.finite(shr_inf_em))

modelo <- lm(d_vaa ~ shr_inf_em, data = simulacao_filtrada)
inclinação <- coef(modelo)[2]  # Extrai a inclinação da reta


ggplot(data = simulacao_filtrada, 
       aes(x = shr_inf_em, y = d_vaa)) +
  geom_point(color = "#66c2a5", alpha = 0.7, size = 2) +
  stat_smooth(method = "lm", se = FALSE, color = "#1b7837", linetype = "solid") +
  labs(
    x = "Share de Matrículas em Educação Infantil e EM (%)",
    y = "Diferença (%) do Valor por Aluno/Ano (VAA)\nrecebido pela política",
    # title = "Relação entre Share de Matrículas e Diferença na Distribuição do FUNDEB",
    caption = paste0("Nota:<br>• Total de observações: ", nrow(simulacao),
                     "<br>• Municípios que não recebiam e passaram a receber, portanto fora do gráfico (y = Inf): ", 
                     sum(!is.finite(simulacao$d_vaa) | !is.finite(simulacao$shr_inf_em)),
                     "<br>• No gráfico: ", nrow(simulacao_filtrada %>% filter(d_vaa < 330))
    )
  ) +
  ylim(-30, 330) +
  annotate(
    "text", x = 70, y = 100,
    label = paste0("Inclinação: ", round(inclinação, 2)),
    color = "#1b7837", size = 4, hjust = 0
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 11),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_markdown(hjust = 0, face = "italic")
  )





#### 4.1.2) Distribuição da variação do CD:

ggplot(data = simulacao_filtrada, aes(x = dif_per_coef)) +
  geom_histogram(fill = "#66c2a5", color = "#1b7837", bins = 70) +
  xlim(min(simulacao_filtrada$dif_per_coef), 101) +
  labs(
    x = "Diferença do Coeficiente de Distribuição \nSimulado para o Real (%)",
    y = "Frequência",
    title = "Distribuição da Variação (%) do Coeficiente de Distribuição",
    caption = "Nota:\n13 municípios com valores acima de 100%.\n(102.2; 110.0; 111.6; 117.7; 140.4; 169.1; 207.1; 225.3; 288.8; 318.1; 1267.2; 1484.1; 8707.1."
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 11),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0, face = "italic")
  )





#### Distribuição da variação do CD em pontos percentuais:

ggplot(data = simulacao_filtrada, aes(x = dif_coef_pp)) +
  geom_histogram(fill = "#66c2a5", color = "#1b7837", bins = 100) +
  xlim(-0.2, 0.15) +
  labs(
    x = "Diferença do Coeficiente de Distribuição \nSimulado para o Real (p.p.)",
    y = "Frequência",
    title = "Distribuição da Variação (p.p.) do Coeficiente de Distribuição",
    caption = "Nota:\n 83 redes fora da visualização.\n25 redes com aumentos maiores que 1 p.p.\nRedes estaduais representam os maiores aumentos."
  ) +
  
  # Linha no tercil mais alto:
  geom_vline(xintercept = quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.7), colour = "red4") + 
  # + geom_vline(aes(xintercept=1), colour="#BB0000", linetype="dashed")
  annotate("text", x = quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.7) + 0.01, y = 1500,
           label = paste0(round(quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.7), 3), ";\ntercil mais\nbeneficiado"),
           color = "red4", size = 4, hjust = 0) +
  
  # Linha no tercil mais baixo:
  geom_vline(xintercept = quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.3), colour = "red4") + 
  # + geom_vline(aes(xintercept=1), colour="#BB0000", linetype="dashed")
  annotate("text", x = quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.3) - 0.04, y = 1500,
           label =paste0(round(quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.3), 3), ";\ntercil menos\nbeneficiado"),
           color = "red4", size = 4, hjust = 0) +
  
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 11),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0, face = "italic")
  )


#### Histograma:

ggplot(data = simulacao_filtrada, aes(x = dif_coef_pp)) +
  geom_histogram(fill = "#66c2a5", color = "#1b7837", bins = 100) +
  xlim(-0.2, 0.15) +
  labs(
    x = "Diferença do Coeficiente de Distribuição \nSimulado para o Real (p.p.)",
    y = "Frequência",
    title = "Distribuição da Variação (p.p.) do Coeficiente de Distribuição",
    caption = "Nota:\n 83 redes fora da visualização.\n25 redes com aumentos maiores que 1 p.p.\nRedes estaduais representam os maiores aumentos."
  ) +
  
  # Linha no tercil mais alto:
  geom_vline(xintercept = quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.7), colour = "red4") + 
  # + geom_vline(aes(xintercept=1), colour="#BB0000", linetype="dashed")
  annotate("text", x = quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.7) + 0.01, y = 1500,
           label = paste0(round(quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.7), 3), ";\ntercil mais\nbeneficiado"),
           color = "red4", size = 4, hjust = 0) +
  
  # Linha no tercil mais baixo:
  geom_vline(xintercept = quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.3), colour = "red4") + 
  # + geom_vline(aes(xintercept=1), colour="#BB0000", linetype="dashed")
  annotate("text", x = quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.3) - 0.04, y = 1500,
           label =paste0(round(quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.3), 3), ";\ntercil menos\nbeneficiado"),
           color = "red4", size = 4, hjust = 0) +
  
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 11),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0, face = "italic")
  )


#### 4.1.3) Mapa dos municípios beneficiados e não beneficiados ----

# Carregar o shapefile dos municípios
muni <- read_sf("C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/Shapefiles/BR_Municipios_2023/BR_Municipios_2023.shp") %>%
  mutate(CD_MUN = as.numeric(CD_MUN)) # Garantir que CD_MUN seja numérico

# Fazer o merge entre o shapefile e o dataframe simulacao
df_mapa_mun <- left_join(muni, simulacao, by = c("CD_MUN" = "codigo_ibge"))

# Identificar municípios que não estão no dataframe simulacao (mismatch)
df_mapa_mun$benef <- ifelse(is.na(df_mapa_mun$benef), "Not_in_simulacao", as.character(df_mapa_mun$benef))

# Criar o mapa
p <- ggplot(df_mapa_mun) +
  geom_sf(
    aes(fill = factor(benef)), # Uso dinâmico de "benef"
    color = "grey90",
    size = 0.01
  ) +
  scale_fill_manual(
    values = c("1" = "#66c2a5", "0" = "grey98", "Not_in_simulacao" = "grey40"),
    name = "Status",
    labels = c("CD diminuiu", "CD aumentou", "Não está na simulação"),
    na.value = "grey40"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  ) +
  labs(
    title = "Mapa de Municípios do Brasil - FUNDEF vs FUNDEB",
    subtitle = "Distribuição de acordo com o resultado da simulação"
  );ggsave(
    "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Resultados/Fundeb x Fundef/Gráficos/Mapa_Simulacao.png",
    plot = p,
    width = 150,
    height = 160,
    units = "mm",
    dpi = 400
  )



#### Evolução das notas dos municípios que não recebiam nada antes da política:

graf <- df %>% 
  filter(codigo_ibge %in% (simulacao %>% filter(is.infinite(dif_per_coef)))$codigo_ibge) %>% 
  relocate(rede, .after = nome) %>% 
  filter(rede == "Pública") %>% 
  group_by(ano) %>% 
  summarise(
    media_5_mat = mean(vl_nota_5_matematica, na.rm = TRUE),
    media_5_lp = mean(vl_nota_5_portugues, na.rm = TRUE),
    media_9_mat = mean(vl_nota_9_matematica, na.rm = TRUE),
    media_9_lp = mean(vl_nota_9_portugues, na.rm = TRUE),
    media_em_mat = mean(vl_nota_em_matematica, na.rm = TRUE),
    media_em_lp = mean(vl_nota_em_portugues, na.rm = TRUE),
    .groups = "drop"
  )

graf_long <- graf %>%
  pivot_longer(
    cols = -ano,
    names_to = "disciplina",
    values_to = "nota"
  )

ggplot(graf_long, aes(x = ano, y = nota, color = disciplina)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Notas médias por disciplina e ano",
    x = "Ano",
    y = "Nota",
    color = "Disciplina"
  ) +
  theme_minimal()

colnames(simulacao)

#### 4.1.4) Resultado líquido em reais: histograma:

ggplot(data = simulacao, aes(x = dif_rs_aluno)) +
  geom_histogram(fill = "#66c2a5", color = "#1b7837", bins = 100) +
  # xlim(-0.2, 0.15) +
  labs(
    x = "Resultado líquido decorrente da mudança da política (R$)",
    y = "Frequência",
    # title = "Distribuição da Variação (p.p.) do Coeficiente de Distribuição",
    # caption = "Nota:\n 83 redes fora da visualização.\n25 redes com aumentos maiores que 1 p.p.\nRedes estaduais representam os maiores aumentos."
  ) +
  
  # Linha no tercil mais alto:
  # geom_vline(xintercept = quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.7), colour = "red4") + 
  # # + geom_vline(aes(xintercept=1), colour="#BB0000", linetype="dashed")
  # annotate("text", x = quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.7) + 0.01, y = 1500,
  #          label = paste0(round(quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.7), 3), ";\ntercil mais\nbeneficiado"),
  #          color = "red4", size = 4, hjust = 0) +
  # 
  # # Linha no tercil mais baixo:
  # geom_vline(xintercept = quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.3), colour = "red4") + 
  # # + geom_vline(aes(xintercept=1), colour="#BB0000", linetype="dashed")
  # annotate("text", x = quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.3) - 0.04, y = 1500,
  #          label =paste0(round(quantile((simulacao %>% filter(!is.na(dif_per_coef)))$dif_coef_pp, 0.3), 3), ";\ntercil menos\nbeneficiado"),
  #          color = "red4", size = 4, hjust = 0) +
  
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 11),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0, face = "italic")
  )

ggsave(
  filename = paste0("histograma_netdif_reais.png"), 
  plot = last_plot(),
  path = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Resultados/Fundeb x Fundef/Gráficos",
  width = 600/96, height = 420/96, dpi = 110,
)

# 5) Análise Econométrica: ----

## 5.1) Incluindo as variáveis principais ----

### 5.1.1) Incluindo dif_per_coef nas notas: ----

df_reg <- df %>% 
  left_join(simulacao %>% select(c(codigo_ibge, coef_est_fnde:receita_real, dif_rs_aluno, rs_por_aluno_fundeb, rs_por_aluno_sim)),
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


df_reg <- df_reg %>% 
  filter(
    case_when(
      ano >= 2005 & ano %% 2 != 0 ~ rede == rede_reg, # case_when: condição ~ valor se verdadeiro
      TRUE ~ TRUE                                      # TRUE ~TRUE é basicamente um else ~ valor padrão
    )
  ) %>%
  mutate(codigo_ibge = as.numeric(str_sub(as.character(codigo_ibge), 1, -2)))


finbra <- read.csv2("C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/Gastos municipais/FINBRA/Despesas/FINBRA_EDU_05_21.csv") 

df_reg <- df_reg %>% 
  left_join((finbra %>% select(-c(uf))), by = c("codigo_ibge", "ano")) %>% 
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



pib <- read_excel("C:/Users/giovannioz/Downloads/base_de_dados_2002_2009_xls/PIB dos Municípios - base de dados 2002-2009.xls") %>% 
  filter(Ano >= 2005) %>% 
  select(1, 7, 8, 40)


pib2 <- read_excel("C:/Users/giovannioz/Downloads/base_de_dados_2010_2021_xlsx/PIB dos Municípios - base de dados 2010-2021.xlsx") %>% 
  select(1, 7, 8, 40)

colnames(pib) <- c("ano", "codigo_ibge", "nom", "PIBpc")
colnames(pib2) <- c("ano", "codigo_ibge", "nom", "PIBpc")

pib <- bind_rows(
  pib,
  pib2) %>% 
  mutate(codigo_ibge = as.numeric(str_sub(as.character(codigo_ibge), 1, -2)))

df_reg <- left_join(
  df_reg,
  pib,
  by = c("codigo_ibge" , "ano")
) %>% 
  relocate(PIBpc, .after = "nome")

df_reg <- df_reg %>%
  mutate(dif_rs_aluno_100 = dif_rs_aluno / 100) %>%  # R$ PER STUDENT DOSAGE, em centenas
  
  # Criação da SPENDING DOSAGE:
  mutate(spending_dosage = dif_rs_aluno/ed_spending_2006)


colnames(df_reg)

teste <- df_reg %>% 
  select(2:5, 52, 53, 56, 58, 59, 61, 62, 60, 77) %>% 
  select(-ano) %>% 
  distinct() %>% 
  select(1:3, dif_coef_pp, dif_rs_aluno, dif_rs_aluno_100, everything())




ggplot(data = teste, 
       aes(x = dif_rs_aluno_100, y = dif_coef_pp)) +
  geom_point(color = "#66c2a5", alpha = 0.7, size = 2) +
  xlim(-5, 5)+
  ylim(-1,1)+
  stat_smooth(method = "lm", se = FALSE, color = "#1b7837", linetype = "solid") +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 11),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_markdown(hjust = 0, face = "italic")
  )
















### 5.1.2) Taxa de Distorção Idade-Série ----

# tdi <- 



### 5.1.3) Cobertura da educação ----

### 5.1.4) Taxa de analfabetismo ----

### 5.1.5) Número de matrículas ----





colnames(alunos)

# pivot_wider(
#   names_from = nu_ano_censo, 
#   values_from = starts_with("total_alunos"),
#   names_glue = "{.value}_{substr(nu_ano_censo, 3, 4)}"
# )

# pivot_wider(
#   id_cols = c(co_entidade, no_regiao, co_uf, no_uf),
#   names_from = nu_ano_censo,
#   names_glue = "{.value}_{str_sub(nu_ano_censo, -2)}",
#   values_from = c(total_alunos_1:total_alunos_3em)
#   

## 5.2) Regressões:----
### 5.2.1) Na dif do coeficiente de distribuição:----

mat_5 <- feols(vl_nota_5_matematica ~ dif_coef_pp * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_reg,
               vcov = "hetero")

port_5 <- feols(vl_nota_5_portugues ~ dif_coef_pp * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_reg,
                vcov = "hetero")

mat_9 <- feols(vl_nota_9_matematica ~ dif_coef_pp * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_reg,
               vcov = "hetero")
port_9 <- feols(vl_nota_5_portugues ~ dif_coef_pp * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_reg,
                vcov = "hetero")



etable(mat_5, port_5, mat_9, port_9, 
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))




# Exportar:

etable(mat_5, port_5, mat_9, port_9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Resultados/Fundeb x Fundef/Tabelas/reg_v1.tex", replace = TRUE)


### 5.2.2) Na dif em reais:----


mat_5 <- feols(vl_nota_5_matematica ~ dif_rs_aluno_100 * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_reg,
               vcov = "hetero")

port_5 <- feols(vl_nota_5_portugues ~ dif_rs_aluno_100 * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_reg,
                vcov = "hetero")

mat_9 <- feols(vl_nota_9_matematica ~ dif_rs_aluno_100 * i(k, ref = 0)
               + PIBpc
               | codigo_ibge + ano + uf^ano,
               data = df_reg,
               vcov = "hetero")
port_9 <- feols(vl_nota_5_portugues ~ dif_rs_aluno_100 * i(k, ref = 0) 
                + PIBpc
                | codigo_ibge + ano + uf^ano,
                data = df_reg,
                vcov = "hetero")



etable(mat_5, port_5, mat_9, port_9, 
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))




# Exportar:

etable(mat_5, port_5, mat_9, port_9,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)),
       file = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Resultados/Fundeb x Fundef/Tabelas/reg_rs_aluno.tex", replace = TRUE)




## 5.2) Representação via gráfico de event-study: ----

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
    filename = paste0("grafico_", model_name, "RS_aluno.png"), # Nome baseado no modelo
    plot = p,
    path = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Resultados/Fundeb x Fundef/Gráficos/event_study",
    width = 600/96, height = 420/96, dpi = 110
  )
}



#### 5.2.2.1) Nova especificação: "above/below": ----

df_reg <- df_reg %>%
  mutate(
    perda = ifelse(dif_rs_aluno < 0, 1, 0),
    ganho = ifelse(dif_rs_aluno > 0, 1, 0)
  )



mat_5 <- feols(
  vl_nota_5_matematica ~ i(k, ref = 0) * abs(dif_rs_aluno_100) * perda +
    i(k, ref = 0) * abs(dif_rs_aluno_100) * ganho +
    PIBpc
  | codigo_ibge + ano + uf^ano,
  data = df_reg, vcov = "hetero"
)

port_5 <- feols(
  vl_nota_5_portugues ~ i(k, ref = 0) * abs(dif_rs_aluno_100) * perda +
    i(k, ref = 0) * abs(dif_rs_aluno_100) * ganho +
    PIBpc
  | codigo_ibge + ano + uf^ano,
  data = df_reg, vcov = "hetero"
)

mat_9 <- feols(
  vl_nota_9_matematica ~ i(k, ref = 0) * abs(dif_rs_aluno_100) * perda +
    i(k, ref = 0) * abs(dif_rs_aluno_100) * ganho +
    PIBpc
  | codigo_ibge + ano + uf^ano,
  data = df_reg, vcov = "hetero"
)

port_9 <- feols(
  vl_nota_9_portugues ~ i(k, ref = 0) * abs(dif_rs_aluno_100) * perda +
    i(k, ref = 0) * abs(dif_rs_aluno_100) * ganho +
    PIBpc
  | codigo_ibge + ano + uf^ano,
  data = df_reg, vcov = "hetero"
)


etable(mat_5, port_5, mat_9, port_9, 
       vcov = "hetero", 
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1, "Matemática" = 1,"Português" = 1)))




models_list <- list(
  mat_5 = mat_5,
  port_5 = port_5,
  mat_9 = mat_9,
  port_9 = port_9
)

event_df <- broom::tidy(mat_5, conf.int = TRUE)
print(event_df, n = 30)






## 5.3) Regressões no gasto em educação per capita: ----

df_reg2 <- df %>% 
  left_join(simulacao %>% select(c(codigo_ibge, coef_est_fnde:receita_real, dif_rs_aluno, total_alunos_2006)),
            by = "codigo_ibge") %>% 
  filter(!is.na(coef_est_fnde)) %>%
  mutate(k = ano - 2007) %>%    
  # filter(ano %% 2 != 0) %>% # COLOCA TODOS OS ANOS, E NÃO SÓ OS QUE TEM SAEB
  mutate(uf = as.factor(uf)) %>% 
  select(-c(12:38), -X) %>% 
  distinct()


df_reg2 <- df_reg2 %>% 
  mutate(codigo_ibge = as.numeric(str_sub(as.character(codigo_ibge), 1, -2)))

finbra <- read.csv2("C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Dados/Gastos municipais/FINBRA/Despesas/FINBRA_EDU_05_21.csv") 

df_reg2 <- df_reg2 %>% 
  left_join((finbra %>% select(-c(uf))), by = c("codigo_ibge", "ano")) %>% 
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
  relocate(des_inf_pc, .after = "populacao")


pib <- read_excel("C:/Users/giovannioz/Downloads/base_de_dados_2002_2009_xls/PIB dos Municípios - base de dados 2002-2009.xls") %>% 
  filter(Ano >= 2005) %>% 
  select(1, 7, 8, 40)

pib2 <- read_excel("C:/Users/giovannioz/Downloads/base_de_dados_2010_2021_xlsx/PIB dos Municípios - base de dados 2010-2021.xlsx") %>% 
  select(1, 7, 8, 40)

colnames(pib) <- c("ano", "codigo_ibge", "nom", "PIBpc")
colnames(pib2) <- c("ano", "codigo_ibge", "nom", "PIBpc")

pib <- bind_rows(
  pib,
  pib2) %>% 
  mutate(codigo_ibge = as.numeric(str_sub(as.character(codigo_ibge), 1, -2)))

df_reg2 <- left_join(
  df_reg2,
  pib,
  by = c("codigo_ibge" , "ano")
) %>% 
  relocate(PIBpc, .after = "nome")

df_reg2 <- df_reg2 %>%
  mutate(dif_rs_aluno_100 = dif_rs_aluno / 100)

### 5.3.1) Na diferença de valor por aluno ----

mod_edu <- feols(des_edu_pc ~ dif_rs_aluno_100 * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_reg2,
                 vcov = "hetero")


mod_fund <- feols(des_fund_pc ~ dif_rs_aluno_100 * i(k, ref = 0)
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_reg2,
                  vcov = "hetero")

mod_med <- feols(des_med_pc ~ dif_rs_aluno_100 * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_reg2,
                 vcov = "hetero")

mod_inf <- feols(des_inf_pc ~ dif_rs_aluno_100 * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_reg2,
                 vcov = "hetero")



etable(mod_edu, mod_fund, mod_med, mod_inf, 
       vcov = "hetero", 
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)))



# Exportar:

etable(mod_edu, mod_fund, mod_med, mod_inf,
       vcov = "hetero",
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)),
       file = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Resultados/Fundeb x Fundef/Tabelas/reg_dif_RS_aluno.tex", replace = TRUE)


#### 5.3.1.1) Representação via gráfico de event-study: ----


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
    path = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Resultados/Fundeb x Fundef/Gráficos/event_study",
    width = 600/96, height = 420/96, dpi = 110
  )
  
}


### 5.3.2) Na diferença do coeficiente de distribuição: ----


mod_edu <- feols(des_edu_pc ~ dif_coef_pp * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_reg2,
                 vcov = "hetero")


mod_fund <- feols(des_fund_pc ~ dif_coef_pp * i(k, ref = 0)
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_reg2,
                  vcov = "hetero")

mod_med <- feols(des_med_pc ~ dif_coef_pp * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_reg2,
                 vcov = "hetero")

mod_inf <- feols(des_inf_pc ~ dif_coef_pp * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_reg2,
                 vcov = "hetero")



etable(mod_edu, mod_fund, mod_med, mod_inf, 
       vcov = "hetero", 
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)))



# Exportar:

etable(mod_edu, mod_fund, mod_med, mod_inf,
       vcov = "hetero",
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)),
       file = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Resultados/Fundeb x Fundef/Tabelas/reg_des_edu_CD.tex", replace = TRUE)


#### 5.3.1.1) Representação via gráfico de event-study: ----


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
    path = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Resultados/Fundeb x Fundef/Gráficos/event_study",
    width = 600/96, height = 420/96, dpi = 110
  )
  
}



### 5.3.3) Utilizando spending dosage ----

mod_edu <- feols(des_edu_pc ~ spending_dosage * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_reg,
                 vcov = "hetero")


mod_fund <- feols(des_fund_pc ~ spending_dosage * i(k, ref = 0)
                  + PIBpc
                  | codigo_ibge + ano + uf^ano,
                  data = df_reg,
                  vcov = "hetero")

mod_med <- feols(des_med_pc ~ spending_dosage * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_reg,
                 vcov = "hetero")

mod_inf <- feols(des_inf_pc ~ spending_dosage * i(k, ref = 0)
                 + PIBpc
                 | codigo_ibge + ano + uf^ano,
                 data = df_reg,
                 vcov = "hetero")



etable(mod_edu, mod_fund, mod_med, mod_inf, 
       vcov = "hetero", 
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)))



# Exportar:

etable(mod_edu, mod_fund, mod_med, mod_inf,
       vcov = "hetero",
       headers = list(":_:" = list("Total" = 1,"Fundamental" = 1, "Médio" = 1,"Infantil" = 1)),
       file = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Resultados/Fundeb x Fundef/Tabelas/reg_dif_RS_aluno.tex", replace = TRUE)


#### 5.3.1.1) Representação via gráfico de event-study: ----


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
    path = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Resultados/Fundeb x Fundef/Gráficos/event_study",
    width = 600/96, height = 420/96, dpi = 110
  )
  
}


### 5.4) Above and Below ----

df_reg <- df_reg %>%
  mutate(
    grupo = case_when(
      dif_rs_aluno > 0 ~ 1,      # ganho líquido
      dif_rs_aluno < 0 ~ 0,      # perda líquida
      TRUE ~ NA_character_
    ),
    grupo = factor(grupo, levels = c("Below","Above"))
  )

quebras <- quantile(df_reg$dif_rs_aluno_100, probs = c(0, .25, .50, .75, 1), na.rm = TRUE)
df_reg <- df_reg %>%
  mutate(
    q_dosagem = cut(dif_rs_aluno_100,
                    breaks = quebras,
                    include.lowest = TRUE,
                    labels = c("Q1","Q2","Q3","Q4"))
  )



mod_edu_ab <- feols(
  des_edu_pc ~ abs(dif_rs_aluno_100) * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_reg, vcov = "hetero"
)

mod_fund_ab <- feols(
  des_fund_pc ~ abs(dif_rs_aluno_100) * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_reg, vcov = "hetero"
)

mod_med_ab <- feols(
  des_med_pc ~ abs(dif_rs_aluno_100) * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_reg, vcov = "hetero"
)

mod_inf_ab <- feols(
  des_inf_pc ~ abs(dif_rs_aluno_100) * i(k, grupo, ref = 0) + PIBpc |
    codigo_ibge + ano + uf^ano,
  data = df_reg, vcov = "hetero"
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



current_model <- models_list[["edu"]]
td <- broom::tidy(current_model, conf.int = TRUE) %>% 
  filter(str_starts(term, "abs")) %>% 
  mutate(grupo = as.factor(case_when(
    str_detect(term, "Above") ~ "Above",
    str_detect(term, "Below") ~ "Below",
  )),
  k = as.numeric(str_sub(term, 26, -14))
  )

print(td)


# combina base + d * slope (IC aprox. ignorando covariância entre termos)
event_df <- base_df %>%
  left_join(slope_df, by = c("k","grupo")) %>%
  mutate(
    estimate_s = coalesce(estimate_s, 0),
    se_s       = coalesce(se_s, 0),
    Estimate   = estimate_b + d_val * estimate_s,
    `Std. Error` = sqrt(se_b^2 + (d_val * se_s)^2),
    conf.low   = Estimate - 1.96 * `Std. Error`,
    conf.high  = Estimate + 1.96 * `Std. Error`,
    ano_evento = 2007 + k,
    grupo      = factor(grupo, levels = c("Above","Below"))
  ) %>%
  arrange(grupo, k)

# adiciona baseline k=0 = 0 para ambos os grupos
event_df <- bind_rows(
  event_df,
  tibble(
    Estimate = 0, `Std. Error` = 0, conf.low = 0, conf.high = 0,
    k = 0, ano_evento = 2007, grupo = factor(c("Above","Below"), levels = c("Above","Below"))
  )
) %>% arrange(grupo, k)

# gráfico no mesmo estilo do seu loop
p <- ggplot(event_df, aes(x = ano_evento, y = Estimate, color = grupo, fill = grupo, group = grupo)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25, color = NA) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 2007, color = "black") +
  geom_point(shape = 15, size = 2) +
  geom_line() +
  labs(x = "Ano", y = "R$ per capita") +
  theme_classic() +
  theme(
    axis.line = element_line(color = "grey70"),
    panel.grid = element_blank(),
    axis.title = element_text(size = 11),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

print(p)























for (model_name in names(models_list)) {
  
  current_model <- models_list[[model_name]]
  td <- broom::tidy(current_model, conf.int = TRUE)
  
  # termos-base: i(k, grupo)
  base_df <- td %>%
    dplyr::filter(str_detect(term, "^k::-?\\d+:grupo::(Above|Below)$")) %>%
    transmute(
      k = as.numeric(str_extract(term, "-?\\d+$")),
      grupo = str_match(term, "grupo::(Above|Below)$")[,2],
      estimate_b = estimate,
      se_b = std.error
    )
  
  # termos de inclinação (dose): abs(dif_rs_aluno_100):i(k, grupo)
  slope_df <- td %>%
    dplyr::filter(str_detect(term, "^abs\\(dif_rs_aluno_100\\):k::-?\\d+:grupo::(Above|Below)$")) %>%
    transmute(
      k = as.numeric(str_extract(term, "-?\\d+(?=:grupo::)")),
      grupo = str_match(term, "grupo::(Above|Below)$")[,2],
      estimate_s = estimate,
      se_s = std.error
    )
  
  # combina base + d * slope (IC aprox. ignorando covariância entre termos)
  event_df <- base_df %>%
    left_join(slope_df, by = c("k","grupo")) %>%
    mutate(
      estimate_s = coalesce(estimate_s, 0),
      se_s       = coalesce(se_s, 0),
      Estimate   = estimate_b + d_val * estimate_s,
      `Std. Error` = sqrt(se_b^2 + (d_val * se_s)^2),
      conf.low   = Estimate - 1.96 * `Std. Error`,
      conf.high  = Estimate + 1.96 * `Std. Error`,
      ano_evento = 2007 + k,
      grupo      = factor(grupo, levels = c("Above","Below"))
    ) %>%
    arrange(grupo, k)
  
  # adiciona baseline k=0 = 0 para ambos os grupos
  event_df <- bind_rows(
    event_df,
    tibble(
      Estimate = 0, `Std. Error` = 0, conf.low = 0, conf.high = 0,
      k = 0, ano_evento = 2007, grupo = factor(c("Above","Below"), levels = c("Above","Below"))
    )
  ) %>% arrange(grupo, k)
  
  # gráfico no mesmo estilo do seu loop
  p <- ggplot(event_df, aes(x = ano_evento, y = Estimate, color = grupo, fill = grupo, group = grupo)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25, color = NA) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 2007, color = "black") +
    geom_point(shape = 15, size = 2) +
    geom_line() +
    labs(x = "Ano", y = "R$ per capita") +
    theme_classic() +
    theme(
      axis.line = element_line(color = "grey70"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 11),
      legend.title = element_blank(),
      legend.position = "bottom"
    )
  
  print(p)
  
  ggsave(
    filename = paste0("grafico_", model_name, "_AB.png"),
    plot = p,
    path = "C:/Users/giovannioz/OneDrive - Insper/Av. Novo Fundeb/Resultados/Fundeb x Fundef/Gráficos/event_study",
    width = 600/96, height = 420/96, dpi = 110
  )
}


# 6) Perguntas do professor ----
## 6.1) Quanto o Fundo representa do total de gastos em educação? ----

df_reg <- df_reg %>% 
  mutate(shr_fundo_edu = total_politica/educacao,
         shr_EF = )


graf_t <- notas %>% 
  filter(rede == "Pública",
         codigo_ibge>100) %>% 
  select(c(1:6, 13:21, 43:49)) %>% 
  filter(is.infinite(dif_per_coef) | dif_per_coef <0) %>%
  mutate(t = ifelse(is.infinite(dif_per_coef), 1, 0)) %>% 
  group_by(t, ano) %>% 
  summarise(
    med_mat_5 = mean(vl_nota_5_matematica, na.rm = TRUE),
    med_lp_5 = mean(vl_nota_5_portugues, na.rm = TRUE),
    med_mat_9 = mean(vl_nota_9_matematica, na.rm = TRUE),
    med_lp_9 = mean(vl_nota_9_portugues, na.rm = TRUE),
    med_mat_em = mean(vl_nota_em_matematica, na.rm = TRUE),
    med_lp_em = mean(vl_nota_em_portugues, na.rm = TRUE),
    .groups = "drop") %>% 
  select(1:3) 

