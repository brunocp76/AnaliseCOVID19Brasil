#' Limpa a tela do Console do RStudio
#'
#' @export
cls <- function() cat("\f")

#' Importa dados da COVID do Portal Brasil.io
#'
#' Realiza a importacao e o tratamento dos dados da COVID-19 do Portal Brasil.io.
#'
#' @return Base importada e tratada da COVID do Portal Brasil.io.
#'
le_brasil_io <- function() {
   covid_brasilio <<- datacovidbr::brasilio(silent = TRUE) %>%
      dplyr::filter(
         place_type == "city",
         !is.na(city),
         !is.na(city_ibge_code)
      ) %>%
      # Calculando os dados faltantes...
      dplyr::arrange(city_ibge_code, date) %>%
      dplyr::mutate(
         contagios_novos = ifelse(
            test = city_ibge_code == dplyr::lag(city_ibge_code),
            yes = confirmed - dplyr::lag(confirmed),
            no = ifelse(
               test = is.na(confirmed),
               yes = 0,
               no = confirmed
            )
         ),
         obitos_novos = ifelse(
            test = city_ibge_code == dplyr::lag(city_ibge_code),
            yes = deaths - dplyr::lag(deaths),
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
      )

   return(covid_brasilio)
}


#' Importa dados da COVID do Ministerio da Saude
#'
#' Realiza a importacao e o tratamento dos dados da COVID-19 do Ministerio da Saude (ou de seu backup).
#'
#' ATENCAO: Esta sub-funcao consome muita memoria e leva algum tempo. Por favor aguarde...
#'
#' @return Base importada e tratada da COVID do Ministerio da Saude.
#'
le_ministerio <- function() {
   covid_ministerio <<-
      tryCatch({
         datacovidbr::brMinisterioSaude(silent = TRUE) %>%
            # Filtrando so as linhas necessárias...
            dplyr::filter(
               regiao != "Brasil",
               !is.na(municipio),
               !is.na(codmun)
            ) %>%
            # Organizando a bagunca dos nomes e reposicionando as colunas...
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
               # em_acompanh_novos = emAcompanhamentoNovos,
               contagios_novos = casosNovos,
               obitos_novos = obitosNovos,
               # recuperados_novos = Recuperadosnovos,
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
            ) %>%
            # Garantindo que todos os nomes estejam arrumados...
            janitor::clean_names()
      },
      # warning = function(w) {
      #    warning-handler-code
      # },
      error = function(e) {
         cls()
         cat("Encontrei um erro na funcao brMinisterioSaude, entao lerei o arquivo de backup...\n\n")
         readRDS("data-raw/backup_covid_ministerio.rds")
      },
      finally = {
         cat("Pronto!\n\n")
      })


      # datacovidbr::brMinisterioSaude(silent = TRUE) %>%
      # # Filtrando só as linhas necessárias...
      # dplyr::filter(
      #    regiao != "Brasil",
      #    !is.na(municipio),
      #    !is.na(codmun)
      # ) %>%
      # # Organizando a bagunça dos nomes e reposicionando as colunas...
      # dplyr::select(
      #    date,
      #    semana_epidem = semanaEpi,
      #    regiao,
      #    estado,
      #    municipio,
      #    cod_uf = coduf,
      #    cod_mun = codmun,
      #    cod_regiao_saude = codRegiaoSaude,
      #    nome_regiao_saude = nomeRegiaoSaude,
      #    pop_tcu_2019 = populacaoTCU2019,
      #    contagios_novos = casosNovos,
      #    obitos_novos = obitosNovos,
      #    contagios_acumulados = casosAcumulado,
      #    obitos_acumulados = obitosAcumulado,
      #    interior_metropol = `interior/metropolitana`
      # ) %>%
      # # Deixando a base (bem) mais leve...
      # dplyr::mutate(
      #    dplyr::across(
      #       .cols = c(
      #          semana_epidem,
      #          cod_uf,
      #          cod_mun,
      #          cod_regiao_saude,
      #          pop_tcu_2019,
      #          contagios_novos,
      #          obitos_novos,
      #          contagios_acumulados,
      #          obitos_acumulados,
      #          interior_metropol
      #       ),
      #       .fns = ~ as.integer(.x)
      #    )
      # ) %>%
      # # Deixando a base (um pouco) mais leve...
      # dplyr::mutate(
      #    dplyr::across(
      #       .cols = c(
      #          regiao,
      #          estado,
      #          municipio,
      #          nome_regiao_saude
      #       ),
      #       .fns = ~ stringi::stri_trans_general(
      #          str = stringr::str_trim(.x),
      #          id = "Latin-ASCII"
      #       )
      #    )
      # ) %>%
      # # Deixando a base (um pouco) mais ajeitada...
      # dplyr::mutate(
      #    dplyr::across(
      #       .cols = c(
      #          nome_regiao_saude
      #       ),
      #       .fns = ~ stringr::str_replace_all(
      #          string =
      #             stringr::str_replace_all(
      #                string =
      #                   stringr::str_replace_all(
      #                      string =
      #                         stringr::str_to_title(
      #                            string = .x,
      #                            locale = "pt_BR"
      #                         ),
      #                      pattern = " Da ",
      #                      replacement = " da "
      #                   ),
      #                pattern = " Do ",
      #                replacement = " do "
      #             )
      #          ,
      #          pattern = " De ",
      #          replacement = " de "
      #       )
      #    )
      # ) %>%
      # # Ordenando para auxiliar no cruzamento dos dados...
      # dplyr::arrange(
      #    cod_mun,
      #    date,
      #    estado,
      #    municipio
      # )

   return(covid_ministerio)
}


#' Deriva dados auxiliares relacionados ao Codigio de Municipio nao-oficial
#'
#' @return Dados auxiliares relacionados ao Codigio de Municipio.
#'
deriva_codigo_municipio <- function() {
   infos_chaves <<- covid_brasilio %>%
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
      )

   return(infos_chaves)
}

#' Processa a Base Final atualizada e enriquecida.
#'
#' @return Base Final atualizada e enriquecida.
#'
processa_final <- function() {
   covid <<- covid_brasilio %>%
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
         cont_nov_brasilio_ministerio = dplyr::coalesce(
            contagios_novos_brasilio,
            contagios_novos_ministerio
         ),
         cont_nov_ministerio_brasilio = dplyr::coalesce(
            contagios_novos_ministerio,
            contagios_novos_brasilio
         ),
         obit_nov_brasilio_ministerio = dplyr::coalesce(
            obitos_novos_brasilio,
            obitos_novos_ministerio
         ),
         obit_nov_ministerio_brasilio = dplyr::coalesce(
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
         cod_ibge = dplyr::coalesce(cod_ibge_key, cod_ibge_big),
         cod_regiao_saude = dplyr::coalesce(cod_regiao_saude_key, cod_regiao_saude_big),
         nome_regiao_saude = dplyr::coalesce(nome_regiao_saude_key, nome_regiao_saude_big)
      ) %>%
      dplyr::select(
         date,
         cod_ibge,
         cod_mun,
         dplyr::everything(),
         -dplyr::ends_with("_big"),
         -dplyr::ends_with("_key")
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
         area_km2 = area_mun_km2,
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
         pop_2019 = dplyr::coalesce(pop_est_2019, pop_tcu_2019),
         municipio = dplyr::coalesce(municipio_brasilio, municipio_ministerio),
         uf = dplyr::coalesce(estado_brasilio, estado_ministerio),
         regiao = dplyr::case_when(
            uf %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
            uf %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
            uf %in% c("PR", "RS", "SC") ~ "Sul",
            uf %in% c("DF", "GO", "MS", "MT") ~ "Centro-Oeste",
            uf %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
            TRUE ~ NA_character_
         ),
         contagios_novos = dplyr::coalesce(contagios_novos_ministerio, contagios_novos_brasilio),
         obitos_novos = dplyr::coalesce(obitos_novos_ministerio, obitos_novos_brasilio),
         contagios_acumulados = dplyr::coalesce(contagios_acumulados_ministerio, contagios_acumulados_brasilio),
         obitos_acumulados = dplyr::coalesce(obitos_acumulados_ministerio, obitos_acumulados_brasilio)
      ) %>%
      dplyr::select(
         date,
         semana_epidem,
         cod_ibge,
         lat,
         lon,
         area_km2,
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
      dplyr::arrange(cod_ibge, date)

   return(covid)
}

#' Faz um backup da ultima atualizacao disponivel
#'
#' Quando invocado, faz uma copia do arquivo final covid para a pasta de dados brutos. Para uso interno no pacote.
#'
#' @return Backup da base mais atualizada disponivel.
#'
backup_base <- function() {
   cat("\n", "Fazendo copia de seguranca da base mais atualizadas disponivel",
       "\n\n", "Por favor aguarde...")

   saveRDS(
      object = covid,
      file = "data-raw/covid.rds",
      ascii = FALSE,
      version = 3,
      compress = "xz"
   )
}

#' Processas as Sumarizacoes de Area e Populacao
#'
#' Processa externamente as sumarizacoes de Area (em Km2) e de Populacao (conforme apurado pelo TCU no ano de 2019)
#'
#' @return Arquivos com as sumarizacoes de Area e Populacao 2019.
#'
sumarios_derivacoes <- function() {
   sumario_regioes_saude <<- covid %>%
      dplyr::group_by(cod_ibge) %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::ungroup() %>%
      dplyr::select(
         area_temp_km2 = area_km2,
         pop_temp_2019 = pop_2019,
         uf_temp = uf,
         regiao_temp = regiao,
         dplyr::everything()
      ) %>%
      dplyr::group_by(cod_regiao_saude) %>%
      dplyr::summarise(
         area_km2 = sum(area_temp_km2, na.rm = TRUE),
         pop_2019 = sum(pop_temp_2019, na.rm = TRUE),
         uf = min(uf_temp, na.rm = TRUE),
         regiao = min(regiao_temp, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(cod_regiao_saude)

   sumario_estados <<- covid %>%
      dplyr::group_by(cod_ibge) %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::ungroup() %>%
      dplyr::select(
         area_temp_km2 = area_km2,
         pop_temp_2019 = pop_2019,
         regiao_temp = regiao,
         dplyr::everything()
      ) %>%
      dplyr::group_by(uf) %>%
      dplyr::summarise(
         area_km2 = sum(area_temp_km2, na.rm = TRUE),
         pop_2019 = sum(pop_temp_2019, na.rm = TRUE),
         regiao = min(regiao_temp, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(uf)

   sumario_regioes_brasil <<- covid %>%
      dplyr::group_by(cod_ibge) %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::ungroup() %>%
      dplyr::select(
         area_temp_km2 = area_km2,
         pop_temp_2019 = pop_2019,
         dplyr::everything()
      ) %>%
      dplyr::group_by(regiao) %>%
      dplyr::summarise(
         area_km2 = sum(area_temp_km2, na.rm = TRUE),
         pop_2019 = sum(pop_temp_2019, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(regiao)

   sumario_brasil <<- covid %>%
      dplyr::group_by(cod_ibge) %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::ungroup() %>%
      dplyr::select(
         area_temp_km2 = area_km2,
         pop_temp_2019 = pop_2019,
         dplyr::everything()
      ) %>%
      dplyr::summarise(
         area_km2 = sum(area_temp_km2, na.rm = TRUE),
         pop_2019 = sum(pop_temp_2019, na.rm = TRUE)
      )
}

#' Gera a Base Derivada ao nivel de Cidades
#'
#' Gera a Base Derivada ao nivel de Cidades com diversos indicadores normalizados para as analises graficas (medias moveis de 7 dias, taxa de mortalidade de casos detectados, taxas por 100 mil habitantes, indicadores normalizados por populacao, indicadores normalizados por area, indicadores normalizados por densidade populacional e indicadores normalizados com logaritmos).
#'
#' @return Base Derivada ao nivel de Cidades.
#'
base_cidades <- function() {
   covid_cidades <<- covid %>%
      dplyr::arrange(cod_ibge, date) %>%
      ## Média Móvel (7 dias) - Cidades
      dplyr::group_by(cod_ibge) %>%
      dplyr::mutate(
         contagios_novos_mm7 = as.double(
            dplyr::coalesce(
               (
                  dplyr::lag(x = contagios_novos, n = 6L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 5L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 4L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 3L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 2L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 1L, na.rm = TRUE) +
                     contagios_novos
               ) / 7,
               0
            )
         ),
         obitos_novos_mm7 = as.double(
            dplyr::coalesce(
               (
                  dplyr::lag(x = obitos_novos, n = 6L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 5L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 4L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 3L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 2L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 1L, na.rm = TRUE) +
                     obitos_novos
               ) / 7,
               0
            )
         ),
         evolucao_contagios_mm7 = as.integer(
            dplyr::case_when(
               dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
                  contagios_novos_mm7 > 1.15 ~ 1L,
               dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
                  contagios_novos_mm7 < 0.85 ~ -1L,
               TRUE ~ 0L
            )
         ),
         evolucao_obitos_mm7 = as.integer(
            dplyr::case_when(
               dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
                  obitos_novos_mm7 > 1.15 ~ 1L,
               dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
                  obitos_novos_mm7 < 0.85 ~ -1L,
               TRUE ~ 0L
            )
         )
      ) %>%
      dplyr::ungroup() %>%
      ## Taxa de Mortalidade dos Casos Detectados - Cidades
      dplyr::mutate(
         taxa_mortalidade = as.double(ifelse(test = contagios_acumulados > 0,
                                             yes = obitos_acumulados / contagios_acumulados,
                                             no = 0))
      ) %>%
      ## Normalização a cada 100.000 habitantes - Cidades
      dplyr::mutate(
         contagios_novos_100k = as.double(contagios_novos / (pop_2019 / 100000)),
         obitos_novos_100k = as.double(obitos_novos / (pop_2019 / 100000)),
         contagios_acumulados_100k = as.double(contagios_acumulados / (pop_2019 / 100000)),
         obitos_acumulados_100k = as.double(obitos_acumulados / (pop_2019 / 100000))
      ) %>%
      ## Normalização de População (2019) - Cidades
      dplyr::mutate(
         contagios_novos_pop = as.double(contagios_novos / pop_2019),
         obitos_novos_pop = as.double(obitos_novos / pop_2019),
         contagios_acumulados_pop = as.double(contagios_acumulados / pop_2019),
         obitos_acumulados_pop = as.double(obitos_acumulados / pop_2019)
      ) %>%
      ## Normalização de Área (Km2) - Cidades
      dplyr::mutate(
         contagios_novos_area = as.double(contagios_novos / area_km2),
         obitos_novos_area = as.double(obitos_novos / area_km2),
         contagios_acumulados_area = as.double(contagios_acumulados / area_km2),
         obitos_acumulados_area = as.double(obitos_acumulados / area_km2)
      ) %>%
      ## Normalização de Densidade Populacional [População (2019) / Área (Km2)] - Cidades
      dplyr::mutate(
         contagios_novos_area = as.double(contagios_novos / (pop_2019 / area_km2)),
         obitos_novos_area = as.double(obitos_novos / (pop_2019 / area_km2)),
         contagios_acumulados_area = as.double(contagios_acumulados / (pop_2019 / area_km2)),
         obitos_acumulados_area = as.double(obitos_acumulados / (pop_2019 / area_km2))
      ) %>%
      ## Normalização Logarítmica (Base e) - Cidades
      dplyr::mutate(
         contagios_novos_ln = as.double(ifelse(test = contagios_novos > 0,
                                               yes = log(x = contagios_novos),
                                               no = 0)),
         obitos_novos_ln = as.double(ifelse(test = obitos_novos > 0,
                                            yes = log(x = obitos_novos),
                                            no = 0)),
         contagios_acumulados_ln = as.double(ifelse(test = contagios_acumulados > 0,
                                                    yes = log(x = contagios_acumulados),
                                                    no = 0)),
         obitos_acumulados_ln = as.double(ifelse(test = obitos_acumulados > 0,
                                                 yes = log(x = obitos_acumulados),
                                                 no = 0))
      ) %>%
      dplyr::arrange(cod_ibge, date)
}

#' Gera a Base Derivada ao nivel de Regioes de Saude
#'
#' Gera a Base Derivada ao nivel de Regioes de Saude com diversos indicadores normalizados para as analises graficas (medias moveis de 7 dias, taxa de mortalidade de casos detectados, taxas por 100 mil habitantes, indicadores normalizados por populacao, indicadores normalizados por area, indicadores normalizados por densidade populacional e indicadores normalizados com logaritmos).
#'
#' @return Base Derivada ao nivel de Regioes de Saude.
#'
base_regioes_saude <- function() {
   covid_regioes_saude <<- covid %>%
      dplyr::select(
         -c(cod_ibge:municipio),
         contagios_novos_regiao_saude = contagios_novos,
         obitos_novos_regiao_saude = obitos_novos,
         contagios_acumulados_regiao_saude = contagios_acumulados,
         obitos_acumulados_regiao_saude = obitos_acumulados
      ) %>%
      dplyr::arrange(
         cod_regiao_saude,
         nome_regiao_saude,
         date,
         semana_epidem
      ) %>%
      dplyr::group_by(
         cod_regiao_saude,
         nome_regiao_saude,
         date,
         semana_epidem
      ) %>%
      dplyr::summarise(
         contagios_novos = sum(contagios_novos_regiao_saude, na.rm = TRUE),
         obitos_novos = sum(obitos_novos_regiao_saude, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(
         cod_regiao_saude,
         date
      ) %>%
      dplyr::group_by(cod_regiao_saude) %>%
      dplyr::mutate(
         contagios_acumulados = cumsum(contagios_novos),
         obitos_acumulados = cumsum(obitos_novos)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(
         cod_regiao_saude,
         nome_regiao_saude,
         date,
         semana_epidem
      ) %>%
      dplyr::left_join(
         y = sumario_regioes_saude,
         by = "cod_regiao_saude"
      ) %>%
      dplyr::arrange(
         cod_regiao_saude,
         nome_regiao_saude,
         date,
         semana_epidem
      ) %>%
      ## Média Móvel (7 dias) - Regiões de Saúde
      dplyr::group_by(cod_regiao_saude) %>%
      dplyr::mutate(
         contagios_novos_mm7 = as.double(
            dplyr::coalesce(
               (
                  dplyr::lag(x = contagios_novos, n = 6L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 5L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 4L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 3L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 2L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 1L, na.rm = TRUE) +
                     contagios_novos
               ) / 7,
               0
            )
         ),
         obitos_novos_mm7 = as.double(
            dplyr::coalesce(
               (
                  dplyr::lag(x = obitos_novos, n = 6L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 5L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 4L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 3L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 2L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 1L, na.rm = TRUE) +
                     obitos_novos
               ) / 7,
               0
            )
         ),
         evolucao_contagios_mm7 = as.integer(
            dplyr::case_when(
               dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
                  contagios_novos_mm7 > 1.15 ~ 1L,
               dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
                  contagios_novos_mm7 < 0.85 ~ -1L,
               TRUE ~ 0L
            )
         ),
         evolucao_obitos_mm7 = as.integer(
            dplyr::case_when(
               dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
                  obitos_novos_mm7 > 1.15 ~ 1L,
               dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
                  obitos_novos_mm7 < 0.85 ~ -1L,
               TRUE ~ 0L
            )
         )
      ) %>%
      dplyr::ungroup() %>%
      ## Taxa de Mortalidade dos Casos Detectados - Regiões de Saúde
      dplyr::mutate(
         taxa_mortalidade = as.double(ifelse(test = contagios_acumulados > 0,
                                             yes = obitos_acumulados / contagios_acumulados,
                                             no = 0))
      ) %>%
      ## Normalização a cada 100.000 habitantes - Regiões de Saúde
      dplyr::mutate(
         contagios_novos_100k = as.double(contagios_novos / (pop_2019 / 100000)),
         obitos_novos_100k = as.double(obitos_novos / (pop_2019 / 100000)),
         contagios_acumulados_100k = as.double(contagios_acumulados / (pop_2019 / 100000)),
         obitos_acumulados_100k = as.double(obitos_acumulados / (pop_2019 / 100000))
      ) %>%
      ## Normalização de População (2019) - Regiões de Saúde
      dplyr::mutate(
         contagios_novos_pop = as.double(contagios_novos / pop_2019),
         obitos_novos_pop = as.double(obitos_novos / pop_2019),
         contagios_acumulados_pop = as.double(contagios_acumulados / pop_2019),
         obitos_acumulados_pop = as.double(obitos_acumulados / pop_2019)
      ) %>%
      ## Normalização de Área (Km2) - Regiões de Saúde
      dplyr::mutate(
         contagios_novos_area = as.double(contagios_novos / area_km2),
         obitos_novos_area = as.double(obitos_novos / area_km2),
         contagios_acumulados_area = as.double(contagios_acumulados / area_km2),
         obitos_acumulados_area = as.double(obitos_acumulados / area_km2)
      ) %>%
      ## Normalização de Densidade Populacional [População (2019) / Área (Km2)] - Regiões de Saúde
      dplyr::mutate(
         contagios_novos_area = as.double(contagios_novos / (pop_2019 / area_km2)),
         obitos_novos_area = as.double(obitos_novos / (pop_2019 / area_km2)),
         contagios_acumulados_area = as.double(contagios_acumulados / (pop_2019 / area_km2)),
         obitos_acumulados_area = as.double(obitos_acumulados / (pop_2019 / area_km2))
      ) %>%
      ## Normalização Logarítmica (Base e) - Regiões de Saúde
      dplyr::mutate(
         contagios_novos_ln = as.double(ifelse(test = contagios_novos > 0,
                                               yes = log(x = contagios_novos),
                                               no = 0)),
         obitos_novos_ln = as.double(ifelse(test = obitos_novos > 0,
                                            yes = log(x = obitos_novos),
                                            no = 0)),
         contagios_acumulados_ln = as.double(ifelse(test = contagios_acumulados > 0,
                                                    yes = log(x = contagios_acumulados),
                                                    no = 0)),
         obitos_acumulados_ln = as.double(ifelse(test = obitos_acumulados > 0,
                                                 yes = log(x = obitos_acumulados),
                                                 no = 0))
      ) %>%
      dplyr::select(
         date,
         semana_epidem,
         cod_regiao_saude,
         nome_regiao_saude,
         area_km2,
         pop_2019,
         uf,
         regiao,
         dplyr::everything()
      ) %>%
      dplyr::arrange(
         cod_regiao_saude,
         date
      )
}

#' Gera a Base Derivada ao nivel de Estados
#'
#' Gera a Base Derivada ao nivel de Estados com diversos indicadores normalizados para as analises graficas (medias moveis de 7 dias, taxa de mortalidade de casos detectados, taxas por 100 mil habitantes, indicadores normalizados por populacao, indicadores normalizados por area, indicadores normalizados por densidade populacional e indicadores normalizados com logaritmos).
#'
#' @return Base Derivada ao nivel de Estados.
#'
base_estados <- function() {
   covid_estados <<- covid %>%
      dplyr::select(
         -c(cod_ibge:nome_regiao_saude),
         contagios_novos_estado = contagios_novos,
         obitos_novos_estado = obitos_novos,
         contagios_acumulados_estado = contagios_acumulados,
         obitos_acumulados_estado = obitos_acumulados
      ) %>%
      dplyr::arrange(
         uf,
         date,
         semana_epidem
      ) %>%
      dplyr::group_by(
         uf,
         date,
         semana_epidem
      ) %>%
      dplyr::summarise(
         contagios_novos = sum(contagios_novos_estado, na.rm = TRUE),
         obitos_novos = sum(obitos_novos_estado, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(
         uf,
         date
      ) %>%
      dplyr::group_by(uf) %>%
      dplyr::mutate(
         contagios_acumulados = cumsum(contagios_novos),
         obitos_acumulados = cumsum(obitos_novos)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(
         uf,
         date,
         semana_epidem
      ) %>%
      dplyr::left_join(
         y = sumario_estados,
         by = "uf"
      ) %>%
      dplyr::arrange(
         uf,
         date,
         semana_epidem
      ) %>%
      ## Média Móvel (7 dias) - Estados
      dplyr::group_by(uf) %>%
      dplyr::mutate(
         contagios_novos_mm7 = as.double(
            dplyr::coalesce(
               (
                  dplyr::lag(x = contagios_novos, n = 6L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 5L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 4L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 3L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 2L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 1L, na.rm = TRUE) +
                     contagios_novos
               ) / 7,
               0
            )
         ),
         obitos_novos_mm7 = as.double(
            dplyr::coalesce(
               (
                  dplyr::lag(x = obitos_novos, n = 6L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 5L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 4L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 3L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 2L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 1L, na.rm = TRUE) +
                     obitos_novos
               ) / 7,
               0
            )
         ),
         evolucao_contagios_mm7 = as.integer(
            dplyr::case_when(
               dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
                  contagios_novos_mm7 > 1.15 ~ 1L,
               dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
                  contagios_novos_mm7 < 0.85 ~ -1L,
               TRUE ~ 0L
            )
         ),
         evolucao_obitos_mm7 = as.integer(
            dplyr::case_when(
               dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
                  obitos_novos_mm7 > 1.15 ~ 1L,
               dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
                  obitos_novos_mm7 < 0.85 ~ -1L,
               TRUE ~ 0L
            )
         )
      ) %>%
      dplyr::ungroup() %>%
      ## Taxa de Mortalidade dos Casos Detectados - Estados
      dplyr::mutate(
         taxa_mortalidade = as.double(ifelse(test = contagios_acumulados > 0,
                                             yes = obitos_acumulados / contagios_acumulados,
                                             no = 0))
      ) %>%
      ## Normalização a cada 100.000 habitantes - Estados
      dplyr::mutate(
         contagios_novos_100k = as.double(contagios_novos / (pop_2019 / 100000)),
         obitos_novos_100k = as.double(obitos_novos / (pop_2019 / 100000)),
         contagios_acumulados_100k = as.double(contagios_acumulados / (pop_2019 / 100000)),
         obitos_acumulados_100k = as.double(obitos_acumulados / (pop_2019 / 100000))
      ) %>%
      ## Normalização de População (2019) - Estados
      dplyr::mutate(
         contagios_novos_pop = as.double(contagios_novos / pop_2019),
         obitos_novos_pop = as.double(obitos_novos / pop_2019),
         contagios_acumulados_pop = as.double(contagios_acumulados / pop_2019),
         obitos_acumulados_pop = as.double(obitos_acumulados / pop_2019)
      ) %>%
      ## Normalização de Área (Km2) - Estados
      dplyr::mutate(
         contagios_novos_area = as.double(contagios_novos / area_km2),
         obitos_novos_area = as.double(obitos_novos / area_km2),
         contagios_acumulados_area = as.double(contagios_acumulados / area_km2),
         obitos_acumulados_area = as.double(obitos_acumulados / area_km2)
      ) %>%
      ## Normalização de Densidade Populacional [População (2019) / Área (Km2)] - Estados
      dplyr::mutate(
         contagios_novos_area = as.double(contagios_novos / (pop_2019 / area_km2)),
         obitos_novos_area = as.double(obitos_novos / (pop_2019 / area_km2)),
         contagios_acumulados_area = as.double(contagios_acumulados / (pop_2019 / area_km2)),
         obitos_acumulados_area = as.double(obitos_acumulados / (pop_2019 / area_km2))
      ) %>%
      ## Normalização Logarítmica (Base e) - Estados
      dplyr::mutate(
         contagios_novos_ln = as.double(ifelse(test = contagios_novos > 0,
                                               yes = log(x = contagios_novos),
                                               no = 0)),
         obitos_novos_ln = as.double(ifelse(test = obitos_novos > 0,
                                            yes = log(x = obitos_novos),
                                            no = 0)),
         contagios_acumulados_ln = as.double(ifelse(test = contagios_acumulados > 0,
                                                    yes = log(x = contagios_acumulados),
                                                    no = 0)),
         obitos_acumulados_ln = as.double(ifelse(test = obitos_acumulados > 0,
                                                 yes = log(x = obitos_acumulados),
                                                 no = 0))
      ) %>%
      dplyr::select(
         date,
         semana_epidem,
         uf,
         area_km2,
         pop_2019,
         regiao,
         dplyr::everything()
      ) %>%
      dplyr::arrange(
         uf,
         date
      )
}

#' Gera a Base Derivada ao nivel de Regioes do Brasil
#'
#' Gera a Base Derivada ao nivel de Regioes do Brasil com diversos indicadores normalizados para as analises graficas (medias moveis de 7 dias, taxa de mortalidade de casos detectados, taxas por 100 mil habitantes, indicadores normalizados por populacao, indicadores normalizados por area, indicadores normalizados por densidade populacional e indicadores normalizados com logaritmos).
#'
#' @return Base Derivada ao nivel de Regioes do Brasil.
#'
base_regioes_brasil <- function() {
   covid_regioes_brasil <<- covid %>%
      dplyr::select(
         -c(cod_ibge:uf),
         contagios_novos_regiao = contagios_novos,
         obitos_novos_regiao = obitos_novos,
         contagios_acumulados_regiao = contagios_acumulados,
         obitos_acumulados_regiao = obitos_acumulados
      ) %>%
      dplyr::arrange(
         regiao,
         date,
         semana_epidem
      ) %>%
      dplyr::group_by(
         regiao,
         date,
         semana_epidem
      ) %>%
      dplyr::summarise(
         contagios_novos = sum(contagios_novos_regiao, na.rm = TRUE),
         obitos_novos = sum(obitos_novos_regiao, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(
         regiao,
         date
      ) %>%
      dplyr::group_by(regiao) %>%
      dplyr::mutate(
         contagios_acumulados = cumsum(contagios_novos),
         obitos_acumulados = cumsum(obitos_novos)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(
         regiao,
         date,
         semana_epidem
      ) %>%
      dplyr::left_join(
         y = sumario_regioes_brasil,
         by = "regiao"
      ) %>%
      dplyr::arrange(
         regiao,
         date,
         semana_epidem
      ) %>%
      ## Média Móvel (7 dias) - Regiões do Brasil
      dplyr::group_by(regiao) %>%
      dplyr::mutate(
         contagios_novos_mm7 = as.double(
            dplyr::coalesce(
               (
                  dplyr::lag(x = contagios_novos, n = 6L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 5L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 4L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 3L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 2L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 1L, na.rm = TRUE) +
                     contagios_novos
               ) / 7,
               0
            )
         ),
         obitos_novos_mm7 = as.double(
            dplyr::coalesce(
               (
                  dplyr::lag(x = obitos_novos, n = 6L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 5L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 4L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 3L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 2L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 1L, na.rm = TRUE) +
                     obitos_novos
               ) / 7,
               0
            )
         ),
         evolucao_contagios_mm7 = as.integer(
            dplyr::case_when(
               dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
                  contagios_novos_mm7 > 1.15 ~ 1L,
               dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
                  contagios_novos_mm7 < 0.85 ~ -1L,
               TRUE ~ 0L
            )
         ),
         evolucao_obitos_mm7 = as.integer(
            dplyr::case_when(
               dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
                  obitos_novos_mm7 > 1.15 ~ 1L,
               dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
                  obitos_novos_mm7 < 0.85 ~ -1L,
               TRUE ~ 0L
            )
         )
      ) %>%
      dplyr::ungroup() %>%
      ## Taxa de Mortalidade dos Casos Detectados - Regiões do Brasil
      dplyr::mutate(
         taxa_mortalidade = as.double(ifelse(test = contagios_acumulados > 0,
                                             yes = obitos_acumulados / contagios_acumulados,
                                             no = 0))
      ) %>%
      ## Normalização a cada 100.000 habitantes - Regiões do Brasil
      dplyr::mutate(
         contagios_novos_100k = as.double(contagios_novos / (pop_2019 / 100000)),
         obitos_novos_100k = as.double(obitos_novos / (pop_2019 / 100000)),
         contagios_acumulados_100k = as.double(contagios_acumulados / (pop_2019 / 100000)),
         obitos_acumulados_100k = as.double(obitos_acumulados / (pop_2019 / 100000))
      ) %>%
      ## Normalização de População (2019) - Regiões do Brasil
      dplyr::mutate(
         contagios_novos_pop = as.double(contagios_novos / pop_2019),
         obitos_novos_pop = as.double(obitos_novos / pop_2019),
         contagios_acumulados_pop = as.double(contagios_acumulados / pop_2019),
         obitos_acumulados_pop = as.double(obitos_acumulados / pop_2019)
      ) %>%
      ## Normalização de Área (Km2) - Regiões do Brasil
      dplyr::mutate(
         contagios_novos_area = as.double(contagios_novos / area_km2),
         obitos_novos_area = as.double(obitos_novos / area_km2),
         contagios_acumulados_area = as.double(contagios_acumulados / area_km2),
         obitos_acumulados_area = as.double(obitos_acumulados / area_km2)
      ) %>%
      ## Normalização de Densidade Populacional [População (2019) / Área (Km2)] - Regiões do Brasil
      dplyr::mutate(
         contagios_novos_area = as.double(contagios_novos / (pop_2019 / area_km2)),
         obitos_novos_area = as.double(obitos_novos / (pop_2019 / area_km2)),
         contagios_acumulados_area = as.double(contagios_acumulados / (pop_2019 / area_km2)),
         obitos_acumulados_area = as.double(obitos_acumulados / (pop_2019 / area_km2))
      ) %>%
      ## Normalização Logarítmica (Base e) - Regiões do Brasil
      dplyr::mutate(
         contagios_novos_ln = as.double(ifelse(test = contagios_novos > 0,
                                               yes = log(x = contagios_novos),
                                               no = 0)),
         obitos_novos_ln = as.double(ifelse(test = obitos_novos > 0,
                                            yes = log(x = obitos_novos),
                                            no = 0)),
         contagios_acumulados_ln = as.double(ifelse(test = contagios_acumulados > 0,
                                                    yes = log(x = contagios_acumulados),
                                                    no = 0)),
         obitos_acumulados_ln = as.double(ifelse(test = obitos_acumulados > 0,
                                                 yes = log(x = obitos_acumulados),
                                                 no = 0))
      ) %>%
      dplyr::select(
         date,
         semana_epidem,
         regiao,
         area_km2,
         pop_2019,
         dplyr::everything()
      ) %>%
      dplyr::arrange(
         regiao,
         date
      )
}

#' Gera a Base Derivada ao nivel de Brasil
#'
#' Gera a Base Derivada ao nivel de Brasil com diversos indicadores normalizados para as analises graficas (medias moveis de 7 dias, taxa de mortalidade de casos detectados, taxas por 100 mil habitantes, indicadores normalizados por populacao, indicadores normalizados por area, indicadores normalizados por densidade populacional e indicadores normalizados com logaritmos).
#'
#' @return Base Derivada ao nivel de Brasil.
#'
base_brasil <- function() {
   covid_brasil <<- covid %>%
      dplyr::select(
         -c(cod_ibge:regiao),
         contagios_novos_brasil = contagios_novos,
         obitos_novos_brasil = obitos_novos,
         contagios_acumulados_brasil = contagios_acumulados,
         obitos_acumulados_brasil = obitos_acumulados
      ) %>%
      dplyr::arrange(
         date,
         semana_epidem
      ) %>%
      dplyr::group_by(
         date,
         semana_epidem
      ) %>%
      dplyr::summarise(
         contagios_novos = sum(contagios_novos_brasil, na.rm = TRUE),
         obitos_novos = sum(obitos_novos_brasil, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(
         date
      ) %>%
      dplyr::mutate(
         contagios_acumulados = cumsum(contagios_novos),
         obitos_acumulados = cumsum(obitos_novos)
      ) %>%
      dplyr::arrange(
         date,
         semana_epidem
      ) %>%
      dplyr::left_join(
         y = sumario_brasil,
         by = character()
      ) %>%
      dplyr::arrange(
         date,
         semana_epidem
      ) %>%
      ## Média Móvel (7 dias) - Brasil
      dplyr::mutate(
         contagios_novos_mm7 = as.double(
            dplyr::coalesce(
               (
                  dplyr::lag(x = contagios_novos, n = 6L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 5L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 4L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 3L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 2L, na.rm = TRUE) +
                     dplyr::lag(x = contagios_novos, n = 1L, na.rm = TRUE) +
                     contagios_novos
               ) / 7,
               0
            )
         ),
         obitos_novos_mm7 = as.double(
            dplyr::coalesce(
               (
                  dplyr::lag(x = obitos_novos, n = 6L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 5L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 4L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 3L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 2L, na.rm = TRUE) +
                     dplyr::lag(x = obitos_novos, n = 1L, na.rm = TRUE) +
                     obitos_novos
               ) / 7,
               0
            )
         ),
         evolucao_contagios_mm7 = as.integer(
            dplyr::case_when(
               dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
                  contagios_novos_mm7 > 1.15 ~ 1L,
               dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
                  contagios_novos_mm7 < 0.85 ~ -1L,
               TRUE ~ 0L
            )
         ),
         evolucao_obitos_mm7 = as.integer(
            dplyr::case_when(
               dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
                  obitos_novos_mm7 > 1.15 ~ 1L,
               dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
                  obitos_novos_mm7 < 0.85 ~ -1L,
               TRUE ~ 0L
            )
         )
      ) %>%
      ## Taxa de Mortalidade dos Casos Detectados - Brasil
      dplyr::mutate(
         taxa_mortalidade = as.double(ifelse(test = contagios_acumulados > 0,
                                             yes = obitos_acumulados / contagios_acumulados,
                                             no = 0))
      ) %>%
      ## Normalização a cada 100.000 habitantes - Brasil
      dplyr::mutate(
         contagios_novos_100k = as.double(contagios_novos / (pop_2019 / 100000)),
         obitos_novos_100k = as.double(obitos_novos / (pop_2019 / 100000)),
         contagios_acumulados_100k = as.double(contagios_acumulados / (pop_2019 / 100000)),
         obitos_acumulados_100k = as.double(obitos_acumulados / (pop_2019 / 100000))
      ) %>%
      ## Normalização de População (2019) - Brasil
      dplyr::mutate(
         contagios_novos_pop = as.double(contagios_novos / pop_2019),
         obitos_novos_pop = as.double(obitos_novos / pop_2019),
         contagios_acumulados_pop = as.double(contagios_acumulados / pop_2019),
         obitos_acumulados_pop = as.double(obitos_acumulados / pop_2019)
      ) %>%
      ## Normalização de Área (Km2) - Brasil
      dplyr::mutate(
         contagios_novos_area = as.double(contagios_novos / area_km2),
         obitos_novos_area = as.double(obitos_novos / area_km2),
         contagios_acumulados_area = as.double(contagios_acumulados / area_km2),
         obitos_acumulados_area = as.double(obitos_acumulados / area_km2)
      ) %>%
      ## Normalização de Densidade Populacional [População (2019) / Área (Km2)] - Brasil
      dplyr::mutate(
         contagios_novos_area = as.double(contagios_novos / (pop_2019 / area_km2)),
         obitos_novos_area = as.double(obitos_novos / (pop_2019 / area_km2)),
         contagios_acumulados_area = as.double(contagios_acumulados / (pop_2019 / area_km2)),
         obitos_acumulados_area = as.double(obitos_acumulados / (pop_2019 / area_km2))
      ) %>%
      ## Normalização Logarítmica (Base e) - Brasil
      dplyr::mutate(
         contagios_novos_ln = as.double(ifelse(test = contagios_novos > 0,
                                               yes = log(x = contagios_novos),
                                               no = 0)),
         obitos_novos_ln = as.double(ifelse(test = obitos_novos > 0,
                                            yes = log(x = obitos_novos),
                                            no = 0)),
         contagios_acumulados_ln = as.double(ifelse(test = contagios_acumulados > 0,
                                                    yes = log(x = contagios_acumulados),
                                                    no = 0)),
         obitos_acumulados_ln = as.double(ifelse(test = obitos_acumulados > 0,
                                                 yes = log(x = obitos_acumulados),
                                                 no = 0))
      ) %>%
      dplyr::select(
         date,
         semana_epidem,
         area_km2,
         pop_2019,
         dplyr::everything()
      ) %>%
      dplyr::arrange(
         date
      )
}

#' Definicoes graficas para graficos do R
#'
#' Definicoes de elementos graficos para construcao de analises graficas.
#'
#' @return Um tema definido para construcao de graficos.
#'
tema_bruno <- function() {
   ggplot2::theme(
      # Bloco sobre a legenda...
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(fill = "black", color = "black"),
      legend.box.background = ggplot2::element_rect(fill = "black", color = "black"),
      legend.key = ggplot2::element_rect(fill = "black"),
      # Bloco sobre o fundo...
      panel.background = ggplot2::element_rect(fill = "black"),
      plot.background = ggplot2::element_rect(fill = "black", color = "cyan"),
      # Bloco sobre as linhas de grade...
      panel.grid.major.x = ggplot2::element_line(color = "#324C63", size = 0.5, linetype = "dotted"),
      panel.grid.major.y = ggplot2::element_line(color = "#324C63", size = 0.5),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_line(color = "#324C63", size = 0.2),
      # Bloco sobre os textos...
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 1, face = "italic", size = 12),
      text = ggplot2::element_text(colour = "#11a2c6"),
      # Bloco sobre os eixos...
      axis.text = ggplot2::element_text(color = "#5889c8", size = 12),
      axis.title = ggplot2::element_text(color = "#11a2c6", size = 12),
      axis.ticks.x = ggplot2::element_line(color = "#324C63"),
      axis.ticks.y = ggplot2::element_line(color = "#324C63"),
      axis.line.x = ggplot2::element_blank()
   )
}
