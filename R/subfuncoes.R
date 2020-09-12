# Bibliotecas - Tentar manter isso o mínimo possível... -------------------
# library(tidyr)
# library(dplyr)


#' Limpa a tela do Console do RStudio
#'
#' @export
cls <- function() cat("\f")


#' Carrega dados auxiliares.
#'
#' Carrega e processa dados auxiliares para uso na montagem da base posteriormente.
#'
#' @param arquivo Base de Dados a ser carregada.
#'
#' @return Base auxiliar processada.
#'
#' @export
carrega_auxiliares <- function(arquivo) {
   arquivo <- load(
      file = paste0("data/", arquivo, ".rda", sep = ""),
      verbose = FALSE
   )

   return(arquivo)
}


#' Importa dados da COVID do Portal Brasil.io
#'
#' Realiza a importacao e o tratamento dos dados da COVID-19 do Portal Brasil.io.
#'
#' @return Base importada e tratada da COVID do Portal Brasil.io.
#'
#' @export
le_brasil_io <- function() {
   datacovidbr::brasilio() %>%
      dplyr::filter(
         place_type == "city",
         !is.na(city),
         !is.na(city_ibge_code)
      ) %>%
      # Calculando os dados faltantes...
      dplyr::arrange(city_ibge_code, date) %>%
      dplyr::mutate(
         contagios_novos = ifelse(
            test = city_ibge_code == lag(city_ibge_code),
            yes = confirmed - lag(confirmed),
            no = ifelse(
               test = is.na(confirmed),
               yes = 0,
               no = confirmed
            )
         ),
         obitos_novos = ifelse(
            test = city_ibge_code == lag(city_ibge_code),
            yes = deaths - lag(deaths),
            no = ifelse(
               test = is.na(deaths),
               yes = 0,
               no = deaths
            )
         )
      ) %>%
      # Organizando a bagunça dos nomes e reposicionando as colunas...
      dplyr::select(
         date,
         estado = state,
         municipio = city,
         cod_ibge = city_ibge_code,
         pop_est_2019 = estimated_population_2019,
         contagios_novos,
         obitos_novos,
         contagios_acumulados = confirmed,
         obitos_acumulados = deaths
      ) %>%
      # Deixando a base (bem) mais leve...
      dplyr::mutate(
         dplyr::across(
            .cols = c(
               cod_ibge,
               pop_est_2019,
               contagios_novos,
               obitos_novos,
               contagios_acumulados,
               obitos_acumulados
            ),
            .fns = ~ as.integer(.x)
         )
      ) %>%
      # Deixando a base (um pouco) mais leve...
      dplyr::mutate(
         dplyr::across(
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
      dplyr::arrange(
         cod_ibge,
         date,
         estado,
         municipio
      ) -> covid_brasilio

   return(covid_brasilio)
}


#' Importa dados da COVID do Ministerio da Saude
#'
#' Realiza a importacao e o tratamento dos dados da COVID-19 do Ministerio da Saude.
#'
#' ATENCAO: Esta sub-funcao consome muita memoria e leva algum tempo. Por favor aguarde...
#'
#' @return Base importada e tratada da COVID do Ministerio da Saude.
#'
#' @export
le_ministerio <- function() {
   datacovidbr::brMinisterioSaude() %>%
      # Filtrando só as linhas necessárias...
      dplyr::filter(
         regiao != "Brasil",
         !is.na(municipio),
         !is.na(codmun)
      ) %>%
      # Organizando a bagunça dos nomes e reposicionando as colunas...
      dplyr::select(
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
         contagios_novos = casosNovos,
         obitos_novos = obitosNovos,
         contagios_acumulados = casosAcumulado,
         obitos_acumulados = obitosAcumulado,
         interior_metropol = `interior/metropolitana`
      ) %>%
      # Deixando a base (bem) mais leve...
      dplyr::mutate(
         dplyr::across(
            .cols = c(
               semana_epidem,
               cod_uf,
               cod_mun,
               cod_regiao_saude,
               pop_tcu_2019,
               contagios_novos,
               obitos_novos,
               contagios_acumulados,
               obitos_acumulados,
               interior_metropol
            ),
            .fns = ~ as.integer(.x)
         )
      ) %>%
      # Deixando a base (um pouco) mais leve...
      dplyr::mutate(
         dplyr::across(
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
      # Deixando a base (um pouco) mais ajeitada...
      dplyr::mutate(
         dplyr::across(
            .cols = c(
               nome_regiao_saude
            ),
            .fns = ~ stringr::str_replace_all(
               string =
                  stringr::str_replace_all(
                     string =
                        stringr::str_replace_all(
                           string =
                              stringr::str_to_title(
                                 string = .x,
                                 locale = "pt_BR"
                              ),
                           pattern = " Da ",
                           replacement = " da "
                        ),
                     pattern = " Do ",
                     replacement = " do "
                  )
               ,
               pattern = " De ",
               replacement = " de "
            )
         )
      ) %>%
      # Ordenando para auxiliar no cruzamento dos dados...
      dplyr::arrange(
         cod_mun,
         date,
         estado,
         municipio
      ) -> covid_ministerio

   return(covid_ministerio)
}


#' Deriva dados auxiliares relacionados ao Codigio de Municipio nao-oficial.
#'
#' @return Dados auxiliares relacionados ao Codigio de Municipio.
#'
#' @export
deriva_codigo_municipio <- function() {
   covid_brasilio %>%
      dplyr::mutate(cod_mun = as.integer(trunc(cod_ibge / 10))) %>%
      dplyr::arrange(
         cod_mun,
         date
      ) %>%
      # Juntando as 2 bases grandes...
      dplyr::full_join(
         y = covid_ministerio,
         by = c("cod_mun", "date"),
         suffix = c("_brasilio", "_ministerio")
      ) %>%
      dplyr::select(
         cod_ibge,
         cod_mun,
         cod_regiao_saude,
         nome_regiao_saude,
         interior_metropol
      ) %>%
      dplyr::filter(
         !is.na(cod_ibge),
         !is.na(cod_mun)
      ) %>%
      dplyr::arrange(
         cod_mun,
         cod_ibge,
         cod_regiao_saude,
         nome_regiao_saude,
         interior_metropol
      ) %>%
      dplyr::group_by(
         cod_mun,
         cod_ibge
      ) %>%
      dplyr::summarise(
         cod_regiao_saude_budega = min(cod_regiao_saude, na.rm = TRUE),
         nome_regiao_saude_budega = min(nome_regiao_saude, na.rm = TRUE),
         interior_metropol_budega = min(interior_metropol, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
         cod_mun,
         cod_ibge,
         cod_regiao_saude = cod_regiao_saude_budega,
         nome_regiao_saude = nome_regiao_saude_budega,
         interior_metropol = interior_metropol_budega
      ) %>%
      dplyr::distinct(
         cod_mun,
         cod_ibge,
         cod_regiao_saude,
         nome_regiao_saude,
         interior_metropol
      ) %>%
      dplyr::arrange(
         cod_mun
         , cod_ibge
         , cod_regiao_saude
         , nome_regiao_saude
         , interior_metropol
      ) -> infos_chaves

   return(infos_chaves)
}

#' Processa a Base Final atualizada e enriquecida.
#'
#' @return Base Final atualizada e enriquecida.
#'
#' @export
processa_final <- function() {
   covid_brasilio %>%
      dplyr::mutate(cod_mun = as.integer(trunc(cod_ibge / 10))) %>%
      dplyr::arrange(
         cod_mun,
         date
      ) %>%
      # Juntando as 2 bases grandes...
      dplyr::full_join(
         y = covid_ministerio,
         by = c("cod_mun", "date"),
         suffix = c("_brasilio", "_ministerio")
      ) %>%
      dplyr::select(
         cod_ibge,
         cod_mun,
         date,
         dplyr::everything(),
         -semana_epidem,
         -interior_metropol
      ) %>%
      dplyr::mutate(
         cont_nov_brasilio_ministerio = coalesce(
            contagios_novos_brasilio,
            contagios_novos_ministerio
         ),
         cont_nov_ministerio_brasilio = coalesce(
            contagios_novos_ministerio,
            contagios_novos_brasilio
         ),
         obit_nov_brasilio_ministerio = coalesce(
            obitos_novos_brasilio,
            obitos_novos_ministerio
         ),
         obit_nov_ministerio_brasilio = coalesce(
            obitos_novos_ministerio,
            obitos_novos_brasilio
         )
      ) %>%
      # Agregando a chave do Código de Município do IBGE...
      dplyr::arrange(
         cod_mun
      ) %>%
      dplyr::left_join(
         y = infos_chaves,
         by = "cod_mun",
         suffix = (c("_big", "_key"))
      ) %>%
      dplyr::mutate(
         cod_ibge = coalesce(cod_ibge_key, cod_ibge_big),
         cod_regiao_saude = coalesce(cod_regiao_saude_key, cod_regiao_saude_big),
         nome_regiao_saude = coalesce(nome_regiao_saude_key, nome_regiao_saude_big)
      ) %>%
      dplyr::select(
         date,
         cod_ibge,
         cod_mun,
         dplyr::everything(),
         -ends_with("_big"),
         -ends_with("_key")
      ) %>%
      # Agregando as Informações Geográficas do Município...
      dplyr::arrange(
         cod_ibge,
      ) %>%
      dplyr::left_join(
         y = infos_geograficas,
         by = "cod_ibge"
      ) %>%
      # Agregando a Semana Epidemiológica...
      dplyr::arrange(
         date
      ) %>%
      dplyr::left_join(
         y = semana_epid,
         by = "date"
      ) %>%
      # Enfim, finalizando a base...
      dplyr::select(
         date,
         semana_epidem,
         cod_ibge,
         cod_mun,
         lat,
         lon,
         area_mun_km2,
         capital,
         interior_metropol,
         pop_est_2019,
         pop_tcu_2019,
         municipio_brasilio,
         municipio_ministerio,
         cod_regiao_saude,
         nome_regiao_saude,
         estado_brasilio,
         estado_ministerio,
         regiao,
         contagios_novos_brasilio,
         contagios_novos_ministerio,
         obitos_novos_brasilio,
         obitos_novos_ministerio,
         contagios_acumulados_brasilio,
         contagios_acumulados_ministerio,
         obitos_acumulados_brasilio,
         obitos_acumulados_ministerio,
         dplyr::everything(),
         -cod_uf
      ) %>%
      dplyr::arrange(
         cod_ibge,
         date
      )  %>%
      dplyr::mutate(
         uf = coalesce(estado_brasilio, estado_ministerio),
         municipio = coalesce(municipio_brasilio, municipio_ministerio),
         contagios_novos = coalesce(contagios_novos_ministerio, contagios_novos_brasilio),
         obitos_novos = coalesce(obitos_novos_ministerio, obitos_novos_brasilio),
         contagios_acumulados = coalesce(contagios_acumulados_ministerio, contagios_acumulados_brasilio),
         obitos_acumulados = coalesce(obitos_acumulados_ministerio, obitos_acumulados_brasilio),
         pop_2019 = coalesce(pop_est_2019, pop_tcu_2019)
      ) %>%
      dplyr::select(
         date,
         semana_epidem,
         cod_ibge,
         lat,
         lon,
         area_mun_km2,
         capital,
         interior_metropol,
         pop_2019,
         municipio,
         cod_regiao_saude,
         nome_regiao_saude,
         uf,
         regiao,
         contagios_novos,
         obitos_novos,
         contagios_acumulados,
         obitos_acumulados
      ) %>%
      dplyr::arrange(cod_ibge, date) -> covid

   return(covid)
}
