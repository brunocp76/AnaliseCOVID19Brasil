# Bibliotecas - Tentar manter isso o mínimo possível... -------------------
library(tidyr)
library(dplyr)


# Dados do Curso R --------------------------------------------------------
covid_R4DS2 <- readRDS("data-raw/covid.rds") %>%
   # Filtrando só as linhas necessárias...
   filter(
      regiao != "Brasil",
      !is.na(municipio),
      !is.na(codmun)
   ) %>%
   # Organizando a bagunça dos nomes e reposicionando as colunas...
   select(
      date = data,
      semana_epidem = semanaEpi,
      regiao,
      estado,
      municipio,
      lat,
      lon,
      cod_uf = coduf,
      cod_mun = codmun,
      cod_regiao_saude = codRegiaoSaude,
      nome_regiao_saude = nomeRegiaoSaude,
      pop_tcu_2019 = populacaoTCU2019,
      eh_capital,
      em_acompanh_novos = emAcompanhamentoNovos,
      casos_novos = casosNovos,
      obitos_novos = obitosNovos,
      recuperados_novos = Recuperadosnovos,
      casos_acumulados = casosAcumulado,
      obitos_acumulados = obitosAcumulado,
      log2_obitos_novos = obitosNovos_log2,
      log2_obitos_acumulados = obitosAcumulado_log2
   ) %>%
   # Deixando a base (bem) mais leve...
   mutate(
      across(
         .cols = c(
            semana_epidem,
            cod_uf,
            cod_mun,
            cod_regiao_saude,
            pop_tcu_2019,
            em_acompanh_novos,
            casos_novos,
            obitos_novos,
            recuperados_novos,
            casos_acumulados,
            obitos_acumulados
         ),
         .fns = ~ as.integer(.x)
      )
   ) %>%
   # Deixando a base (um pouco) mais leve...
   mutate(
      across(
         .cols = c(
            regiao,
            estado,
            municipio,
            nome_regiao_saude
         ),
         .fns = ~ stringi::stri_trans_general(
            str = stringr::str_trim(.x),
            id = "Latin-ASCII"
         )
      )
   ) %>%
   # Ordenando para auxiliar no cruzamento dos dados...
   arrange(date, cod_mun, estado, municipio)

covid_R4DS2 %>%
   glimpse()

covid_R4DS2 %>%
   object.size()


# Dados do Brasil.io ------------------------------------------------------
covid_brasilio <- datacovidbr::brasilio() %>%
   # Filtrando só as linhas necessárias...
   filter(
      place_type == "city",
      !is.na(city),
      !is.na(city_ibge_code)
   ) %>%
   # Organizando a bagunça dos nomes e reposicionando as colunas...
   select(
      date,
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
   # Deixando a base (bem) mais leve...
   mutate(
      across(
         .cols = c(
            cod_ibge,
            pop_est_2019,
            confirmados,
            obitos,
            ordem_para_lugar
         ),
         .fns = ~ as.integer(.x)
      )
   ) %>%
   # Deixando a base (um pouco) mais leve...
   mutate(
      across(
         .cols = c(
            estado,
            municipio
         ),
         .fns = ~ stringi::stri_trans_general(
            str = stringr::str_trim(.x),
            id = "Latin-ASCII"
         )
      )
   ) %>%
   # Ordenando para auxiliar no cruzamento dos dados...
   arrange(date, cod_ibge, estado, municipio)


covid_brasilio %>%
   glimpse()

covid_brasilio %>%
   object.size()


# Dados do Ministério da Saúde --------------------------------------------
covid_ministerio <- datacovidbr::brMinisterioSaude() %>%
   # Filtrando só as linhas necessárias...
   filter(
      regiao != "Brasil",
      !is.na(municipio),
      !is.na(codmun)
   ) %>%
   # Organizando a bagunça dos nomes e reposicionando as colunas...
   select(
      date,
      semana_epidem = semanaEpi,
      regiao,
      estado,
      municipio,
      cod_uf = coduf,
      cod_mun = codmun,
      cod_regiao_saude = codRegiaoSaude,
      nome_regiao_saude = nomeRegiaoSaude,
      pop_tcu_2019 = populacaoTCU2019,
      em_acompanh_novos = emAcompanhamentoNovos,
      casos_novos = casosNovos,
      obitos_novos = obitosNovos,
      recuperados_novos = Recuperadosnovos,
      casos_acumulados = casosAcumulado,
      obitos_acumulados = obitosAcumulado,
      interior_metropol = `interior/metropolitana`
   ) %>%
   # Deixando a base (bem) mais leve...
   mutate(
      across(
         .cols = c(
            semana_epidem,
            cod_uf,
            cod_mun,
            cod_regiao_saude,
            pop_tcu_2019,
            em_acompanh_novos,
            casos_novos,
            obitos_novos,
            recuperados_novos,
            casos_acumulados,
            obitos_acumulados,
            interior_metropol
         ),
         .fns = ~ as.integer(.x)
      )
   ) %>%
   # Deixando a base (um pouco) mais leve...
   mutate(
      across(
         .cols = c(
            regiao,
            estado,
            municipio,
            nome_regiao_saude
         ),
         .fns = ~ stringi::stri_trans_general(
            str = stringr::str_trim(.x),
            id = "Latin-ASCII"
         )
      )
   ) %>%
   # Ordenando para auxiliar no cruzamento dos dados...
   arrange(date, cod_mun, estado, municipio)

covid_ministerio %>%
   glimpse()

covid_ministerio %>%
   object.size()


# Salvando arquivos temporários... ----------------------------------------

system.time(
   saveRDS(
      object = covid_R4DS2,
      file = "data-raw/covid_R4DS2.rds",
      ascii = FALSE,
      version = 3,
      compress = "xz"
   )
)

system.time(
   saveRDS(
      object = covid_brasilio,
      file = "data-raw/covid_brasilio.rds",
      ascii = FALSE,
      version = 3,
      compress = "xz"
   )
)

system.time(
   saveRDS(
      object = covid_ministerio,
      file = "data-raw/covid_ministerio.rds",
      ascii = FALSE,
      version = 3,
      compress = "xz"
   )
)

## code to prepare `importa` dataset goes here

usethis::use_data(
   importa,
   internal = FALSE,
   overwrite = TRUE,
   compress = "xz",
   version = 3
)
