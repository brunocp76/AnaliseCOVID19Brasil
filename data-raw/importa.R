# Bibliotecas - Tentar manter isso o minimo possivel... -------------------
library(tidyr)
library(dplyr)

# Importacao dos Dados da Base do Curso R ---------------------------------

covid_R4DS2 <- readRDS("data-raw/covid.rds") %>%
   filter(
      regiao != "Brasil",
      !is.na(municipio),
      !is.na(codmun)
   ) %>%
   select(data, regiao, estado, municipio, coduf, codmun, cod)
   arrange(data, estado, municipio)


# Importacao de Dados do pacote datacovidbr -------------------------------

covid_brasilio <- datacovidbr::brasilio() %>%
   filter(
      place_type == "city",
      !is.na(city),
      !is.na(city_ibge_code)
   ) %>%
   select(
      data = date,
      estado = state,
      municipio = city,
      cod_ibge = city_ibge_code,
      # tipo_local = place_type,
      pop_est_2019 = estimated_population_2019,
      confirmados = confirmed,
      obitos = deaths,
      ultimo_dado = is_last,
      taxa_obitos = death_rate,
      conf_por_100k_hab = confirmed_per_100k_inhabitants,
      ordem_para_lugar = order_for_place
   ) %>%
   arrange(data, cod_ibge, estado, cidade) %>%
   glimpse()

covid_ministerio <- datacovidbr::brMinisterioSaude() %>%
   filter(
      regiao != "Brasil",
      !is.na(municipio),
      !is.na(codmun)
   ) %>%
   glimpse()


## code to prepare `importa` dataset goes here

usethis::use_data(
   importa,
   internal = FALSE,
   overwrite = TRUE,
   compress = "xz",
   version = 3
)
