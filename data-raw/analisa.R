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


# Bases Derivadas -------------------------------------------------------

## Base ao nível de Cidades
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
      contagios_novos_area = as.double(contagios_novos / area_mun_km2),
      obitos_novos_area = as.double(obitos_novos / area_mun_km2),
      contagios_acumulados_area = as.double(contagios_acumulados / area_mun_km2),
      obitos_acumulados_area = as.double(obitos_acumulados / area_mun_km2)
   ) %>%
   ## Normalização de Densidade Populacional [População (2019) / Área (Km2)] - Cidades
   mutate(
      contagios_novos_area = as.double(contagios_novos / (pop_2019 / area_mun_km2)),
      obitos_novos_area = as.double(obitos_novos / (pop_2019 / area_mun_km2)),
      contagios_acumulados_area = as.double(contagios_acumulados / (pop_2019 / area_mun_km2)),
      obitos_acumulados_area = as.double(obitos_acumulados / (pop_2019 / area_mun_km2))
   ) %>%
   ## Normalização Logarítmica (Base 2) - Cidades
   mutate(
      contagios_novos_log2 = as.double(log(x = contagios_novos, base = 2)),
      obitos_novos_log2 = as.double(log(x = obitos_novos, base = 2)),
      contagios_acumulados_log2 = as.double(log(x = contagios_acumulados, base = 2)),
      obitos_acumulados_log2 = as.double(log(x = obitos_acumulados, base = 2))
   ) %>%
   ## Normalização Logarítmica (Base e) - Cidades
   mutate(
      contagios_novos_ln = as.double(log(x = contagios_novos)),
      obitos_novos_ln = as.double(log(x = obitos_novos)),
      contagios_acumulados_ln = as.double(log(x = contagios_acumulados)),
      obitos_acumulados_ln = as.double(log(x = obitos_acumulados))
   ) -> covid_cidades

covid_cidades %>%
   glimpse()




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
