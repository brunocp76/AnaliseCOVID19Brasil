# Não entram no código principal... ---------------------------------------
library(zoo)
library(dplyr)
library(ggdark)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(gganimate)

covid <- readRDS("data-raw/covid.rds")

cls()

covid_R4DS2 %>%
   glimpse()

covid %>%
   glimpse()

covid %>%
   summary()

covid %>%
   distinct(uf, regiao) %>%
   group_by(regiao) %>%
   count() %>%
   ungroup() %>%
   arrange(desc(n)) %>%
   as.data.frame()


# Criando as Sumarizações de Área e População -----------------------------

covid %>%
   group_by(cod_ibge) %>%
   filter(date == max(date)) %>%
   ungroup() %>%
   select(
      area_temp_km2 = area_km2,
      pop_temp_2019 = pop_2019,
      everything()
   ) %>%
   group_by(cod_regiao_saude) %>%
   summarise(
      area_km2 = sum(area_temp_km2, na.rm = TRUE),
      pop_2019 = sum(pop_temp_2019, na.rm = TRUE)
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
      everything()
   ) %>%
   group_by(uf) %>%
   summarise(
      area_km2 = sum(area_temp_km2, na.rm = TRUE),
      pop_2019 = sum(pop_temp_2019, na.rm = TRUE)
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


# Bases Derivadas - Cidades -----------------------------------------------
covid %>%
   select(-c(cod_regiao_saude:regiao)) %>%
   arrange(cod_ibge, date) %>%
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
      contagios_novos_ln = as.double(log(x = contagios_novos)),
      obitos_novos_ln = as.double(log(x = obitos_novos)),
      contagios_acumulados_ln = as.double(log(x = contagios_acumulados)),
      obitos_acumulados_ln = as.double(log(x = obitos_acumulados))
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
      -uf,
      -regiao,
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
      contagios_novos_ln = as.double(log(x = contagios_novos)),
      obitos_novos_ln = as.double(log(x = obitos_novos)),
      contagios_acumulados_ln = as.double(log(x = contagios_acumulados)),
      obitos_acumulados_ln = as.double(log(x = obitos_acumulados))
   ) %>%
   select(
      date,
      semana_epidem,
      cod_regiao_saude,
      nome_regiao_saude,
      area_km2,
      pop_2019,
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
      -regiao,
      contagios_novos_regiao_saude = contagios_novos,
      obitos_novos_regiao_saude = obitos_novos,
      contagios_acumulados_regiao_saude = contagios_acumulados,
      obitos_acumulados_regiao_saude = obitos_acumulados
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
      contagios_novos = sum(contagios_novos_regiao_saude, na.rm = TRUE),
      obitos_novos = sum(obitos_novos_regiao_saude, na.rm = TRUE)
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
      contagios_novos_ln = as.double(log(x = contagios_novos)),
      obitos_novos_ln = as.double(log(x = obitos_novos)),
      contagios_acumulados_ln = as.double(log(x = contagios_acumulados)),
      obitos_acumulados_ln = as.double(log(x = obitos_acumulados))
   ) %>%
   select(
      date,
      semana_epidem,
      uf,
      area_km2,
      pop_2019,
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
      contagios_novos_regiao_saude = contagios_novos,
      obitos_novos_regiao_saude = obitos_novos,
      contagios_acumulados_regiao_saude = contagios_acumulados,
      obitos_acumulados_regiao_saude = obitos_acumulados
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
      contagios_novos = sum(contagios_novos_regiao_saude, na.rm = TRUE),
      obitos_novos = sum(obitos_novos_regiao_saude, na.rm = TRUE)
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
      contagios_novos_ln = as.double(log(x = contagios_novos)),
      obitos_novos_ln = as.double(log(x = obitos_novos)),
      contagios_acumulados_ln = as.double(log(x = contagios_acumulados)),
      obitos_acumulados_ln = as.double(log(x = obitos_acumulados))
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
      contagios_novos_regiao_saude = contagios_novos,
      obitos_novos_regiao_saude = obitos_novos,
      contagios_acumulados_regiao_saude = contagios_acumulados,
      obitos_acumulados_regiao_saude = obitos_acumulados
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
      contagios_novos = sum(contagios_novos_regiao_saude, na.rm = TRUE),
      obitos_novos = sum(obitos_novos_regiao_saude, na.rm = TRUE)
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
      contagios_novos_ln = as.double(log(x = contagios_novos)),
      obitos_novos_ln = as.double(log(x = obitos_novos)),
      contagios_acumulados_ln = as.double(log(x = contagios_acumulados)),
      obitos_acumulados_ln = as.double(log(x = obitos_acumulados))
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
      date_max = max(date, na.rm = TRUE),
      area_sum = sum(area_km2, na.rm = TRUE),
      pop_sum = sum(pop_2019, na.rm = TRUE),
      cont_sum = sum(contagios_acumulados, na.rm = TRUE),
      obit_sum = sum(obitos_acumulados, na.rm = TRUE)
   )


















   select(
      date,
      cod_ibge,
      pop_2019,
      contagios_novos,
      obitos_novos
   ) %>%

   tidyr::pivot_wider(
      # id_cols = c("contagios_novos", "obitos_novos"),
      names_from = "date",
      values_from = c("contagios_novos", "obitos_novos")
   ) %>% head(5)


   ## Normalização por Média Móvel (de 7 dias) - Cidades
   group_by(cod_ibge) %>%
   mutate(
      contagios_novos_mm7 = as.double(rollmean(x = contagios_novos, k = 7, align = "right", na.rm = TRUE)),
      obitos_novos_mm7 = as.double(rollmean(x = obitos_novos, k = 7, align = "right", na.rm = TRUE)),
      contagios_acumulados_mm7 = as.double(rollmean(x = contagios_acumulados, k = 7, align = "right", na.rm = TRUE)),
      obitos_acumulados_mm7 = as.double(rollmean(x = obitos_acumulados, k = 7, align = "right", na.rm = TRUE))
   ) %>%
   ungroup()
   # group_by(cod_ibge) %>%
   # mutate(
   #    across(
   #       .cols = c(
   #          starts_with("contagios"),
   #          starts_with("obitos")
   #       ),
   #       .fns = ~rollmean(x = .x, k = 7, align = "right", na.rm = TRUE),
   #       .names = "{.col}_mm7"
   #    )
   # ) %>%
   # ungroup() %>%
   glimpse()




covid_cidades %>%
   select(
      starts_with("contagios"),
      starts_with("obitos")
   ) %>%
   names()


# Tentando alguns gráficos ------------------------------------------------
## Gráficos de Linha - Escala Normal
covid_cidades %>%
   filter(municipio %in% c("Sao Paulo", "Campinas", "Indaiatuba", "Atibaia")) %>%
   ggplot() +
   geom_line(aes(x = date, y = contagios_novos, color = municipio)) +
   scale_x_date(date_breaks = "1 month") +
   labs(x = "Data")

## Gráficos de Linha - Escala Normalizada
covid_cidades %>%
   filter(municipio %in% c("Sao Paulo", "Campinas", "Indaiatuba", "Atibaia")) %>%
   ggplot() +
   geom_line(aes(x = date, y = contagios_novos_100k, color = municipio)) +
   scale_x_date(date_breaks = "1 month") +
   labs(x = "Data")

## Gráficos de Linha - Escala Normalizada
covid_cidades %>%
   filter(municipio %in% c("Sao Paulo", "Campinas", "Indaiatuba", "Atibaia")) %>%
   ggplot() +
   geom_line(aes(x = date, y = contagios_novos_ln, color = municipio)) +
   scale_x_date(date_breaks = "1 month") +
   labs(x = "Data")

## Gráfico mesclando linhas e colunas
covid_cidades %>%
   filter(municipio == "Sao Paulo") %>%
   ggplot() +
   geom_col(aes(x = date, y = contagios_novos), color = "blue") +
   geom_line(aes(x = date, y = obitos_novos), color = "red") +
   scale_x_date(date_breaks = "2 weeks") +
   dark_mode() +
   labs(x = "Data")

## Boxplots
covid_cidades %>%
   # filter(lubridate::month(date) == 5, uf == "AM") %>%
   mutate(
      semana_epidemiologica = as.factor(semana_epidem)
   ) %>%
   ggplot() +
   geom_boxplot(aes(x = semana_epidemiologica, y = obitos_novos), fill = "white", color = "black")

covid_cidades %>%
   ggplot() +
   geom_boxplot(aes(x = uf, y = contagios_novos), fill = "white", color = "black")

covid_cidades %>%
   group_by(uf) %>%
   summarize(
      obit_ac_q0 = min(obitos_novos, na.rm = TRUE),
      obit_ac_q1 = quantile(obitos_acumulados, probs = 0.25, na.rm = TRUE),
      obit_ac_q2 = quantile(obitos_acumulados, probs = 0.50, na.rm = TRUE),
      obit_ac_q3 = quantile(obitos_acumulados, probs = 0.75, na.rm = TRUE),
      obit_ac_q4 = quantile(obitos_acumulados, probs = 1, na.rm = TRUE)
   )
