# ============================================================================ #
# SCRIPT : 00_siope_extract.R
# PURPOSE: Build the shared SIOPE cache used by both FUNDEF/FUNDEB scripts.
#          1) Download raw UF x year CSVs from FNDE Open Data (optional)
#          2) Stack raw UF files into annual CSVs
#
# OUTPUT FOLDERS (shared):
#   Dados/SIOPE/raw   -> raw UF x year CSVs: siope_{UF}_{ANO}_P{PERIODO}.csv
#   Dados/SIOPE/Anos  -> annual stacked CSVs: {ANO}.csv
#
# Notes:
#   - Set RUN_ONCE_SIOPE_DOWNLOAD <- TRUE only when you need to download raw
#     files for the first time (or to refresh missing files).
#   - Set RUN_ONCE_SIOPE_ANNUAL <- TRUE only when you need to rebuild the annual
#     stacked files from the raw cache.
# ============================================================================ #

library(tidyverse)
library(readxl)
library(httr2)
library(glue)
library(janitor)

options(scipen = 999)

# ---------------------------------------------------------------------------- #
# 0. Paths ----
# ---------------------------------------------------------------------------- #

PATH_TUFFY    <- "Z:/Tuffy/Paper - Educ"
PATH_SIOPE    <- file.path(PATH_TUFFY, "Dados/SIOPE")
PATH_SIOPE_RAW <- file.path(PATH_SIOPE, "raw")
PATH_SIOPE_AN  <- file.path(PATH_SIOPE, "Anos")

dir.create(PATH_SIOPE_RAW, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_SIOPE_AN,  recursive = TRUE, showWarnings = FALSE)

ufs <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA",
         "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN",
         "RO","RR","RS","SC","SE","SP","TO")

f_periodo <- function(ano) ifelse(ano <= 2016, 1, 6)

# ---------------------------------------------------------------------------- #
# 1. Download raw SIOPE files (one-time cache build) ----
# ---------------------------------------------------------------------------- #

RUN_ONCE_SIOPE_DOWNLOAD <- FALSE

if (RUN_ONCE_SIOPE_DOWNLOAD) {
  falhas <- list()

  for (ano in 2000:2024) {
    for (uf in ufs) {
      periodo <- f_periodo(ano)

      url <- glue(
        "https://www.fnde.gov.br/olinda-ide/servico/DADOS_ABERTOS_SIOPE/versao/v1/odata/",
        "Receita_Siope(Ano_Consulta=@Ano_Consulta,Num_Peri=@Num_Peri,Sig_UF=@Sig_UF)",
        "?@Ano_Consulta={ano}&@Num_Peri={periodo}&@Sig_UF='{uf}'",
        "&$format=text/csv",
        "&$select=TIPO,NUM_ANO,NUM_PERI,COD_UF,SIG_UF,COD_MUNI,NOM_MUNI,",
        "COD_EXIB_FORMATADO,NOM_ITEM,IDN_CLAS,NOM_COLU,NUM_NIVE,NUM_ORDE,VAL_DECL"
      )

      caminho_arquivo <- file.path(PATH_SIOPE_RAW, glue("siope_{uf}_{ano}_P{periodo}.csv"))

      if (file.exists(caminho_arquivo)) {
        message(glue("↩ Skipping existing file: {basename(caminho_arquivo)}"))
        next
      }

      resultado <- try({
        resp <- request(url) |> req_perform()
        writeBin(resp$body, caminho_arquivo)
        message(glue("✔ {uf}-{ano} downloaded."))
        TRUE
      }, silent = TRUE)

      if (inherits(resultado, "try-error") || isFALSE(resultado)) {
        falhas <- append(falhas, list(tibble(uf = uf, ano = ano, periodo = periodo)))
        message(glue("⚠ Failed: {uf}-{ano}."))
      }
    }
  }

  log_erros <- bind_rows(falhas)
  write.csv2(log_erros, file.path(PATH_SIOPE, "log_erros_download.csv"), row.names = FALSE)
}

# ---------------------------------------------------------------------------- #
# 2. Stack raw UF files into annual CSVs ----
# ---------------------------------------------------------------------------- #

RUN_ONCE_SIOPE_ANNUAL <- FALSE

if (RUN_ONCE_SIOPE_ANNUAL) {
  for (ano in 2000:2024) {
    lista_ufs <- list()

    for (uf in ufs) {
      periodo <- f_periodo(ano)
      nome_arquivo <- file.path(PATH_SIOPE_RAW, glue("siope_{uf}_{ano}_P{periodo}.csv"))

      if (!file.exists(nome_arquivo)) {
        message(glue("❌ File not found: {nome_arquivo}"))
        next
      }

      df_uf_ano <- read.csv(nome_arquivo, fileEncoding = "UTF-8") %>%
        mutate(
          ANO = ano,
          UF = uf,
          COD_EXIB_FORMATADO = as.numeric(COD_EXIB_FORMATADO),
          COD_MUNI = as.numeric(COD_MUNI)
        ) %>%
        mutate(across(where(is.character), ~ .x %>%
                        str_squish() %>%
                        str_remove("\.+$") %>%
                        str_remove("\s+$")))

      lista_ufs[[uf]] <- df_uf_ano
    }

    dados_ano <- bind_rows(lista_ufs)
    write.csv(dados_ano, file.path(PATH_SIOPE_AN, paste0(ano, ".csv")), row.names = FALSE)
    message(glue("✅ Year {ano} stacked and saved."))
  }
}

# ---------------------------------------------------------------------------- #
# 3. Optional quick validation ----
# ---------------------------------------------------------------------------- #

# Example: verify that a few expected files exist.
# print(file.exists(file.path(PATH_SIOPE_RAW, "siope_AC_2006_P1.csv")))
# print(file.exists(file.path(PATH_SIOPE_AN,  "2006.csv")))
