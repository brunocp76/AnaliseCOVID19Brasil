# Não entram no código principal... ---------------------------------------
library(dplyr)

covid <- readRDS("data-raw/covid.rds")

cls()

covid %>%
   glimpse()

covid %>%
   summary()


# Criando as Sumarizações de Área e População -----------------------------
covid %>%
   group_by(cod_ibge) %>%
   filter(date == max(date)) %>%
   ungroup() %>%
   select(
      area_temp_km2 = area_km2,
      pop_temp_2019 = pop_2019,
      uf_temp = uf,
      regiao_temp = regiao,
      everything()
   ) %>%
   group_by(cod_regiao_saude) %>%
   summarise(
      area_km2 = sum(area_temp_km2, na.rm = TRUE),
      pop_2019 = sum(pop_temp_2019, na.rm = TRUE),
      uf = min(uf_temp, na.rm = TRUE),
      regiao = min(regiao_temp, na.rm = TRUE)
   ) %>%
   ungroup() %>%
   arrange(cod_regiao_saude) -> sumario_regioes_saude

covid %>%
   group_by(cod_ibge) %>%
   filter(date == max(date)) %>%
   ungroup() %>%
   select(
      area_temp_km2 = area_km2,
      pop_temp_2019 = pop_2019,
      regiao_temp = regiao,
      everything()
   ) %>%
   group_by(uf) %>%
   summarise(
      area_km2 = sum(area_temp_km2, na.rm = TRUE),
      pop_2019 = sum(pop_temp_2019, na.rm = TRUE),
      regiao = min(regiao_temp, na.rm = TRUE)
   ) %>%
   ungroup() %>%
   arrange(uf) -> sumario_estados

covid %>%
   group_by(cod_ibge) %>%
   filter(date == max(date)) %>%
   ungroup() %>%
   select(
      area_temp_km2 = area_km2,
      pop_temp_2019 = pop_2019,
      everything()
   ) %>%
   group_by(regiao) %>%
   summarise(
      area_km2 = sum(area_temp_km2, na.rm = TRUE),
      pop_2019 = sum(pop_temp_2019, na.rm = TRUE)
   ) %>%
   ungroup() %>%
   arrange(regiao) -> sumario_regioes_brasil

covid %>%
   group_by(cod_ibge) %>%
   filter(date == max(date)) %>%
   ungroup() %>%
   select(
      area_temp_km2 = area_km2,
      pop_temp_2019 = pop_2019,
      everything()
   ) %>%
   summarise(
      area_km2 = sum(area_temp_km2, na.rm = TRUE),
      pop_2019 = sum(pop_temp_2019, na.rm = TRUE)
   ) -> sumario_brasil

## Rápida Conferência...
cls()

sumario_regioes_saude %>%
   glimpse()

sumario_estados %>%
   glimpse()

sumario_regioes_brasil %>%
   glimpse()

sumario_brasil %>%
   glimpse()

sumario_regioes_saude %>%
   summarise(
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE)
   )

sumario_estados %>%
   summarise(
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE)
   )

sumario_regioes_brasil %>%
   summarise(
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE)
   )


# Bases Derivadas - Cidades -----------------------------------------------
covid %>%
   # select(-c(cod_regiao_saude:regiao)) %>%
   arrange(cod_ibge, date) %>%
   ## Média Móvel (7 dias) - Cidades
   group_by(cod_ibge) %>%
   mutate(
      contagios_novos_mm7 = as.double(
         coalesce(
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
         coalesce(
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
         case_when(
            dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
               contagios_novos_mm7 > 1.15 ~ 1L,
            dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
               contagios_novos_mm7 < 0.85 ~ -1L,
            TRUE ~ 0L
         )
      ),
      evolucao_obitos_mm7 = as.integer(
         case_when(
            dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
               obitos_novos_mm7 > 1.15 ~ 1L,
            dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
               obitos_novos_mm7 < 0.85 ~ -1L,
            TRUE ~ 0L
         )
      )
   ) %>%
   ungroup() %>%
   ## Taxa de Mortalidade dos Casos Detectados - Cidades
   mutate(
      taxa_mortalidade = as.double(ifelse(test = contagios_acumulados > 0,
                                          yes = obitos_acumulados / contagios_acumulados,
                                          no = 0))
   ) %>%
   ## Normalização a cada 100.000 habitantes - Cidades
   mutate(
      contagios_novos_100k = as.double(contagios_novos / (pop_2019 / 100000)),
      obitos_novos_100k = as.double(obitos_novos / (pop_2019 / 100000)),
      contagios_acumulados_100k = as.double(contagios_acumulados / (pop_2019 / 100000)),
      obitos_acumulados_100k = as.double(obitos_acumulados / (pop_2019 / 100000))
   ) %>%
   ## Normalização de População (2019) - Cidades
   mutate(
      contagios_novos_pop = as.double(contagios_novos / pop_2019),
      obitos_novos_pop = as.double(obitos_novos / pop_2019),
      contagios_acumulados_pop = as.double(contagios_acumulados / pop_2019),
      obitos_acumulados_pop = as.double(obitos_acumulados / pop_2019)
   ) %>%
   ## Normalização de Área (Km2) - Cidades
   mutate(
      contagios_novos_area = as.double(contagios_novos / area_km2),
      obitos_novos_area = as.double(obitos_novos / area_km2),
      contagios_acumulados_area = as.double(contagios_acumulados / area_km2),
      obitos_acumulados_area = as.double(obitos_acumulados / area_km2)
   ) %>%
   ## Normalização de Densidade Populacional [População (2019) / Área (Km2)] - Cidades
   mutate(
      contagios_novos_area = as.double(contagios_novos / (pop_2019 / area_km2)),
      obitos_novos_area = as.double(obitos_novos / (pop_2019 / area_km2)),
      contagios_acumulados_area = as.double(contagios_acumulados / (pop_2019 / area_km2)),
      obitos_acumulados_area = as.double(obitos_acumulados / (pop_2019 / area_km2))
   ) %>%
   ## Normalização Logarítmica (Base e) - Cidades
   mutate(
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
   arrange(cod_ibge, date) -> covid_cidades

## Rápida Conferência...
cls()

covid_cidades %>%
   glimpse()

covid_cidades %>%
   group_by(cod_ibge) %>%
   filter(date == max(date)) %>%
   ungroup() %>%
   summarise(
      date_max = max(date, na.rm = TRUE),
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE),
      cont_sum = sum(contagios_acumulados, na.rm = TRUE),
      obit_sum = sum(obitos_acumulados, na.rm = TRUE)
   )


# Bases Derivadas - Regiões de Saúde --------------------------------------
covid %>%
   select(
      -c(cod_ibge:municipio),
      # uf:regiao,
      contagios_novos_regiao_saude = contagios_novos,
      obitos_novos_regiao_saude = obitos_novos,
      contagios_acumulados_regiao_saude = contagios_acumulados,
      obitos_acumulados_regiao_saude = obitos_acumulados
   ) %>%
   arrange(
      cod_regiao_saude,
      nome_regiao_saude,
      date,
      semana_epidem
   ) %>%
   group_by(
      cod_regiao_saude,
      nome_regiao_saude,
      date,
      semana_epidem
   ) %>%
   summarise(
      contagios_novos = sum(contagios_novos_regiao_saude, na.rm = TRUE),
      obitos_novos = sum(obitos_novos_regiao_saude, na.rm = TRUE)
   ) %>%
   ungroup() %>%
   arrange(
      cod_regiao_saude,
      date
   ) %>%
   group_by(cod_regiao_saude) %>%
   mutate(
      contagios_acumulados = cumsum(contagios_novos),
      obitos_acumulados = cumsum(obitos_novos)
   ) %>%
   ungroup() %>%
   arrange(
      cod_regiao_saude,
      nome_regiao_saude,
      date,
      semana_epidem
   ) %>%
   left_join(
      y = sumario_regioes_saude,
      by = "cod_regiao_saude"
   ) %>%
   arrange(
      cod_regiao_saude,
      nome_regiao_saude,
      date,
      semana_epidem
   ) %>%
   ## Média Móvel (7 dias) - Regiões de Saúde
   group_by(cod_regiao_saude) %>%
   mutate(
      contagios_novos_mm7 = as.double(
         coalesce(
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
         coalesce(
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
         case_when(
            dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
               contagios_novos_mm7 > 1.15 ~ 1L,
            dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
               contagios_novos_mm7 < 0.85 ~ -1L,
            TRUE ~ 0L
         )
      ),
      evolucao_obitos_mm7 = as.integer(
         case_when(
            dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
               obitos_novos_mm7 > 1.15 ~ 1L,
            dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
               obitos_novos_mm7 < 0.85 ~ -1L,
            TRUE ~ 0L
         )
      )
   ) %>%
   ungroup() %>%
   ## Taxa de Mortalidade dos Casos Detectados - Regiões de Saúde
   mutate(
      taxa_mortalidade = as.double(ifelse(test = contagios_acumulados > 0,
                                          yes = obitos_acumulados / contagios_acumulados,
                                          no = 0))
   ) %>%
   ## Normalização a cada 100.000 habitantes - Regiões de Saúde
   mutate(
      contagios_novos_100k = as.double(contagios_novos / (pop_2019 / 100000)),
      obitos_novos_100k = as.double(obitos_novos / (pop_2019 / 100000)),
      contagios_acumulados_100k = as.double(contagios_acumulados / (pop_2019 / 100000)),
      obitos_acumulados_100k = as.double(obitos_acumulados / (pop_2019 / 100000))
   ) %>%
   ## Normalização de População (2019) - Regiões de Saúde
   mutate(
      contagios_novos_pop = as.double(contagios_novos / pop_2019),
      obitos_novos_pop = as.double(obitos_novos / pop_2019),
      contagios_acumulados_pop = as.double(contagios_acumulados / pop_2019),
      obitos_acumulados_pop = as.double(obitos_acumulados / pop_2019)
   ) %>%
   ## Normalização de Área (Km2) - Regiões de Saúde
   mutate(
      contagios_novos_area = as.double(contagios_novos / area_km2),
      obitos_novos_area = as.double(obitos_novos / area_km2),
      contagios_acumulados_area = as.double(contagios_acumulados / area_km2),
      obitos_acumulados_area = as.double(obitos_acumulados / area_km2)
   ) %>%
   ## Normalização de Densidade Populacional [População (2019) / Área (Km2)] - Regiões de Saúde
   mutate(
      contagios_novos_area = as.double(contagios_novos / (pop_2019 / area_km2)),
      obitos_novos_area = as.double(obitos_novos / (pop_2019 / area_km2)),
      contagios_acumulados_area = as.double(contagios_acumulados / (pop_2019 / area_km2)),
      obitos_acumulados_area = as.double(obitos_acumulados / (pop_2019 / area_km2))
   ) %>%
   ## Normalização Logarítmica (Base e) - Regiões de Saúde
   mutate(
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
   select(
      date,
      semana_epidem,
      cod_regiao_saude,
      nome_regiao_saude,
      area_km2,
      pop_2019,
      uf,
      regiao,
      everything()
   ) %>%
   arrange(
      cod_regiao_saude,
      date
   ) -> covid_regioes_saude

## Rápida Conferência...
cls()

covid_regioes_saude %>%
   glimpse()

covid_regioes_saude %>%
   group_by(cod_regiao_saude) %>%
   filter(date == max(date)) %>%
   ungroup() %>%
   summarise(
      date_max = max(date, na.rm = TRUE),
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE),
      cont_sum = sum(contagios_acumulados, na.rm = TRUE),
      obit_sum = sum(obitos_acumulados, na.rm = TRUE)
   )


# Bases Derivadas - Estados -----------------------------------------------
covid %>%
   select(
      -c(cod_ibge:nome_regiao_saude),
      # -regiao,
      contagios_novos_estado = contagios_novos,
      obitos_novos_estado = obitos_novos,
      contagios_acumulados_estado = contagios_acumulados,
      obitos_acumulados_estado = obitos_acumulados
   ) %>%
   arrange(
      uf,
      date,
      semana_epidem
   ) %>%
   group_by(
      uf,
      date,
      semana_epidem
   ) %>%
   summarise(
      contagios_novos = sum(contagios_novos_estado, na.rm = TRUE),
      obitos_novos = sum(obitos_novos_estado, na.rm = TRUE)
   ) %>%
   ungroup() %>%
   arrange(
      uf,
      date
   ) %>%
   group_by(uf) %>%
   mutate(
      contagios_acumulados = cumsum(contagios_novos),
      obitos_acumulados = cumsum(obitos_novos)
   ) %>%
   ungroup() %>%
   arrange(
      uf,
      date,
      semana_epidem
   ) %>%
   left_join(
      y = sumario_estados,
      by = "uf"
   ) %>%
   arrange(
      uf,
      date,
      semana_epidem
   ) %>%
   ## Média Móvel (7 dias) - Estados
   group_by(uf) %>%
   mutate(
      contagios_novos_mm7 = as.double(
         coalesce(
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
         coalesce(
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
         case_when(
            dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
               contagios_novos_mm7 > 1.15 ~ 1L,
            dplyr::lag(x = contagios_novos_mm7, n = 6L, na.rm = TRUE) /
               contagios_novos_mm7 < 0.85 ~ -1L,
            TRUE ~ 0L
         )
      ),
      evolucao_obitos_mm7 = as.integer(
         case_when(
            dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
               obitos_novos_mm7 > 1.15 ~ 1L,
            dplyr::lag(x = obitos_novos_mm7, n = 6L, na.rm = TRUE) /
               obitos_novos_mm7 < 0.85 ~ -1L,
            TRUE ~ 0L
         )
      )
   ) %>%
   ungroup() %>%
   ## Taxa de Mortalidade dos Casos Detectados - Estados
   mutate(
      taxa_mortalidade = as.double(ifelse(test = contagios_acumulados > 0,
                                          yes = obitos_acumulados / contagios_acumulados,
                                          no = 0))
   ) %>%
   ## Normalização a cada 100.000 habitantes - Estados
   mutate(
      contagios_novos_100k = as.double(contagios_novos / (pop_2019 / 100000)),
      obitos_novos_100k = as.double(obitos_novos / (pop_2019 / 100000)),
      contagios_acumulados_100k = as.double(contagios_acumulados / (pop_2019 / 100000)),
      obitos_acumulados_100k = as.double(obitos_acumulados / (pop_2019 / 100000))
   ) %>%
   ## Normalização de População (2019) - Estados
   mutate(
      contagios_novos_pop = as.double(contagios_novos / pop_2019),
      obitos_novos_pop = as.double(obitos_novos / pop_2019),
      contagios_acumulados_pop = as.double(contagios_acumulados / pop_2019),
      obitos_acumulados_pop = as.double(obitos_acumulados / pop_2019)
   ) %>%
   ## Normalização de Área (Km2) - Estados
   mutate(
      contagios_novos_area = as.double(contagios_novos / area_km2),
      obitos_novos_area = as.double(obitos_novos / area_km2),
      contagios_acumulados_area = as.double(contagios_acumulados / area_km2),
      obitos_acumulados_area = as.double(obitos_acumulados / area_km2)
   ) %>%
   ## Normalização de Densidade Populacional [População (2019) / Área (Km2)] - Estados
   mutate(
      contagios_novos_area = as.double(contagios_novos / (pop_2019 / area_km2)),
      obitos_novos_area = as.double(obitos_novos / (pop_2019 / area_km2)),
      contagios_acumulados_area = as.double(contagios_acumulados / (pop_2019 / area_km2)),
      obitos_acumulados_area = as.double(obitos_acumulados / (pop_2019 / area_km2))
   ) %>%
   ## Normalização Logarítmica (Base e) - Estados
   mutate(
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
   select(
      date,
      semana_epidem,
      uf,
      area_km2,
      pop_2019,
      regiao,
      everything()
   ) %>%
   arrange(
      uf,
      date
   ) -> covid_estados

## Rápida Conferência...
cls()

covid_estados %>%
   glimpse()

covid_estados %>%
   group_by(uf) %>%
   filter(date == max(date)) %>%
   ungroup() %>%
   summarise(
      date_max = max(date, na.rm = TRUE),
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE),
      cont_sum = sum(contagios_acumulados, na.rm = TRUE),
      obit_sum = sum(obitos_acumulados, na.rm = TRUE)
   )


# Bases Derivadas - Regiões do Brasil -------------------------------------
covid %>%
   select(
      -c(cod_ibge:uf),
      contagios_novos_regiao = contagios_novos,
      obitos_novos_regiao = obitos_novos,
      contagios_acumulados_regiao = contagios_acumulados,
      obitos_acumulados_regiao = obitos_acumulados
   ) %>%
   arrange(
      regiao,
      date,
      semana_epidem
   ) %>%
   group_by(
      regiao,
      date,
      semana_epidem
   ) %>%
   summarise(
      contagios_novos = sum(contagios_novos_regiao, na.rm = TRUE),
      obitos_novos = sum(obitos_novos_regiao, na.rm = TRUE)
   ) %>%
   ungroup() %>%
   arrange(
      regiao,
      date
   ) %>%
   group_by(regiao) %>%
   mutate(
      contagios_acumulados = cumsum(contagios_novos),
      obitos_acumulados = cumsum(obitos_novos)
   ) %>%
   ungroup() %>%
   arrange(
      regiao,
      date,
      semana_epidem
   ) %>%
   left_join(
      y = sumario_regioes_brasil,
      by = "regiao"
   ) %>%
   arrange(
      regiao,
      date,
      semana_epidem
   ) %>%
   ## Média Móvel (7 dias) - Regiões do Brasil
   group_by(regiao) %>%
   mutate(
      contagios_novos_mm7 = as.double(
         coalesce(
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
         coalesce(
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
      )
   ) %>%
   ungroup() %>%
   ## Taxa de Mortalidade dos Casos Detectados - Regiões do Brasil
   mutate(
      taxa_mortalidade = as.double(ifelse(test = contagios_acumulados > 0,
                                          yes = obitos_acumulados / contagios_acumulados,
                                          no = 0))
   ) %>%
   ## Normalização a cada 100.000 habitantes - Regiões do Brasil
   mutate(
      contagios_novos_100k = as.double(contagios_novos / (pop_2019 / 100000)),
      obitos_novos_100k = as.double(obitos_novos / (pop_2019 / 100000)),
      contagios_acumulados_100k = as.double(contagios_acumulados / (pop_2019 / 100000)),
      obitos_acumulados_100k = as.double(obitos_acumulados / (pop_2019 / 100000))
   ) %>%
   ## Normalização de População (2019) - Regiões do Brasil
   mutate(
      contagios_novos_pop = as.double(contagios_novos / pop_2019),
      obitos_novos_pop = as.double(obitos_novos / pop_2019),
      contagios_acumulados_pop = as.double(contagios_acumulados / pop_2019),
      obitos_acumulados_pop = as.double(obitos_acumulados / pop_2019)
   ) %>%
   ## Normalização de Área (Km2) - Regiões do Brasil
   mutate(
      contagios_novos_area = as.double(contagios_novos / area_km2),
      obitos_novos_area = as.double(obitos_novos / area_km2),
      contagios_acumulados_area = as.double(contagios_acumulados / area_km2),
      obitos_acumulados_area = as.double(obitos_acumulados / area_km2)
   ) %>%
   ## Normalização de Densidade Populacional [População (2019) / Área (Km2)] - Regiões do Brasil
   mutate(
      contagios_novos_area = as.double(contagios_novos / (pop_2019 / area_km2)),
      obitos_novos_area = as.double(obitos_novos / (pop_2019 / area_km2)),
      contagios_acumulados_area = as.double(contagios_acumulados / (pop_2019 / area_km2)),
      obitos_acumulados_area = as.double(obitos_acumulados / (pop_2019 / area_km2))
   ) %>%
   ## Normalização Logarítmica (Base e) - Regiões do Brasil
   mutate(
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
   select(
      date,
      semana_epidem,
      regiao,
      area_km2,
      pop_2019,
      everything()
   ) %>%
   arrange(
      regiao,
      date
   ) -> covid_regioes_brasil

## Rápida Conferência...
cls()

covid_regioes_brasil %>%
   glimpse()

covid_regioes_brasil %>%
   group_by(regiao) %>%
   filter(date == max(date)) %>%
   ungroup() %>%
   summarise(
      date_max = max(date, na.rm = TRUE),
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE),
      cont_sum = sum(contagios_acumulados, na.rm = TRUE),
      obit_sum = sum(obitos_acumulados, na.rm = TRUE)
   )


# Bases Derivadas - Brasil ------------------------------------------------
covid %>%
   select(
      -c(cod_ibge:regiao),
      contagios_novos_brasil = contagios_novos,
      obitos_novos_brasil = obitos_novos,
      contagios_acumulados_brasil = contagios_acumulados,
      obitos_acumulados_brasil = obitos_acumulados
   ) %>%
   arrange(
      date,
      semana_epidem
   ) %>%
   group_by(
      date,
      semana_epidem
   ) %>%
   summarise(
      contagios_novos = sum(contagios_novos_brasil, na.rm = TRUE),
      obitos_novos = sum(obitos_novos_brasil, na.rm = TRUE)
   ) %>%
   ungroup() %>%
   arrange(
      date
   ) %>%
   mutate(
      contagios_acumulados = cumsum(contagios_novos),
      obitos_acumulados = cumsum(obitos_novos)
   ) %>%
   arrange(
      date,
      semana_epidem
   ) %>%
   left_join(
      y = sumario_brasil,
      by = character()
   ) %>%
   arrange(
      date,
      semana_epidem
   ) %>%
   ## Média Móvel (7 dias) - Brasil
   mutate(
      contagios_novos_mm7 = as.double(
         coalesce(
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
         coalesce(
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
      )
   ) %>%
   ## Taxa de Mortalidade dos Casos Detectados - Brasil
   mutate(
      taxa_mortalidade = as.double(ifelse(test = contagios_acumulados > 0,
                                          yes = obitos_acumulados / contagios_acumulados,
                                          no = 0))
   ) %>%
   ## Normalização a cada 100.000 habitantes - Brasil
   mutate(
      contagios_novos_100k = as.double(contagios_novos / (pop_2019 / 100000)),
      obitos_novos_100k = as.double(obitos_novos / (pop_2019 / 100000)),
      contagios_acumulados_100k = as.double(contagios_acumulados / (pop_2019 / 100000)),
      obitos_acumulados_100k = as.double(obitos_acumulados / (pop_2019 / 100000))
   ) %>%
   ## Normalização de População (2019) - Brasil
   mutate(
      contagios_novos_pop = as.double(contagios_novos / pop_2019),
      obitos_novos_pop = as.double(obitos_novos / pop_2019),
      contagios_acumulados_pop = as.double(contagios_acumulados / pop_2019),
      obitos_acumulados_pop = as.double(obitos_acumulados / pop_2019)
   ) %>%
   ## Normalização de Área (Km2) - Brasil
   mutate(
      contagios_novos_area = as.double(contagios_novos / area_km2),
      obitos_novos_area = as.double(obitos_novos / area_km2),
      contagios_acumulados_area = as.double(contagios_acumulados / area_km2),
      obitos_acumulados_area = as.double(obitos_acumulados / area_km2)
   ) %>%
   ## Normalização de Densidade Populacional [População (2019) / Área (Km2)] - Brasil
   mutate(
      contagios_novos_area = as.double(contagios_novos / (pop_2019 / area_km2)),
      obitos_novos_area = as.double(obitos_novos / (pop_2019 / area_km2)),
      contagios_acumulados_area = as.double(contagios_acumulados / (pop_2019 / area_km2)),
      obitos_acumulados_area = as.double(obitos_acumulados / (pop_2019 / area_km2))
   ) %>%
   ## Normalização Logarítmica (Base e) - Brasil
   mutate(
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
   select(
      date,
      semana_epidem,
      area_km2,
      pop_2019,
      everything()
   ) %>%
   arrange(
      date
   ) -> covid_brasil

## Rápida Conferência...
cls()

covid_brasil %>%
   glimpse()

covid_brasil %>%
   filter(date == max(date)) %>%
   summarise(
      date_max = max(date, na.rm = TRUE, 0.7),
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE),
      cont_sum = sum(contagios_acumulados, na.rm = TRUE),
      obit_sum = sum(obitos_acumulados, na.rm = TRUE)
   )


# Conferindo as Versões Sumarizadas ---------------------------------------
cls()

covid %>% group_by(cod_ibge) %>% filter(date == max(date)) %>% ungroup() %>%
   summarise(
      data_max = max(date, na.rm = TRUE),
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE),
      cont_sum = sum(contagios_acumulados, na.rm = TRUE),
      obit_sum = sum(obitos_acumulados, na.rm = TRUE)
   )

covid_cidades %>% group_by(cod_ibge) %>% filter(date == max(date)) %>% ungroup() %>%
   summarise(
      data_max = max(date, na.rm = TRUE),
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE),
      cont_sum = sum(contagios_acumulados, na.rm = TRUE),
      obit_sum = sum(obitos_acumulados, na.rm = TRUE)
   )

covid_regioes_saude %>% group_by(cod_regiao_saude) %>% filter(date == max(date)) %>% ungroup() %>%
   summarise(
      data_max = max(date, na.rm = TRUE),
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE),
      cont_sum = sum(contagios_acumulados, na.rm = TRUE),
      obit_sum = sum(obitos_acumulados, na.rm = TRUE)
   )

covid_estados %>% group_by(uf) %>% filter(date == max(date)) %>% ungroup() %>%
   summarise(
      data_max = max(date, na.rm = TRUE),
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE),
      cont_sum = sum(contagios_acumulados, na.rm = TRUE),
      obit_sum = sum(obitos_acumulados, na.rm = TRUE)
   )

covid_regioes_brasil %>% group_by(regiao) %>% filter(date == max(date)) %>% ungroup() %>%
   summarise(
      data_max = max(date, na.rm = TRUE),
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE),
      cont_sum = sum(contagios_acumulados, na.rm = TRUE),
      obit_sum = sum(obitos_acumulados, na.rm = TRUE)
   )

covid_brasil %>% filter(date == max(date)) %>%
   summarise(
      data_max = max(date, na.rm = TRUE),
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE),
      cont_sum = sum(contagios_acumulados, na.rm = TRUE),
      obit_sum = sum(obitos_acumulados, na.rm = TRUE)
   )
