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
