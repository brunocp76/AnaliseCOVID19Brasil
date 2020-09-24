# Não entram no código principal... ---------------------------------------
library(sf)
library(geobr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(gganimate)

# Tentando definir um tema padrão para meus gráficos... -------------------
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


# Graficos de Linha - Escala Normal - Novos Contágios ---------------------
covid_cidades %>%
   filter(municipio %in% c("Sao Paulo", "Campinas", "Indaiatuba", "Atibaia")) %>%
   ggplot() +
   geom_line(aes(x = date, y = contagios_novos, color = municipio)) +
   scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y") +
   labs(x = "Data") +
   tema_bruno()

# Graficos de Linha - Escala Normalizada - Novos Contágios por 100 mil ----
covid_cidades %>%
   filter(municipio %in% c("Sao Paulo", "Campinas", "Indaiatuba", "Atibaia")) %>%
   ggplot() +
   geom_line(aes(x = date, y = contagios_novos_100k, color = municipio)) +
   scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y") +
   labs(x = "Data") +
   tema_bruno()

# Gráficos de Linha - Escala Normalizada - Log dos Novos Contágios --------
covid_cidades %>%
   filter(municipio %in% c("Sao Paulo", "Campinas", "Indaiatuba", "Atibaia")) %>%
   ggplot() +
   geom_line(aes(x = date, y = contagios_novos_ln, color = municipio)) +
   scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y") +
   labs(x = "Data") +
   tema_bruno() +
   scale_linetype_manual(values = c("white", "orange", "olive", "yellow"))


# Boxplots - Semana Epidemiológica x Novos Óbitos -------------------------
covid_cidades %>%
   mutate(
      semana_epidemiologica = as.factor(semana_epidem)
   ) %>%
   ggplot() +
   geom_boxplot(aes(x = semana_epidemiologica, y = obitos_novos), fill = "white", color = "black") +
   ylim(c(0, 300))

covid_estados %>%
   mutate(
      estado = forcats::fct_reorder(.f = uf, .x = contagios_novos, .desc = TRUE)
   ) %>%
   ggplot() +
   geom_boxplot(aes(x = estado, y = contagios_novos, fill = estado), color = "white", show.legend = FALSE) +
   labs(
      x = "Estados",
      y = "Novos Contágios",
      title = "Novos Contágios"
   ) +
   tema_bruno()

covid_estados %>%
   mutate(
      estado = forcats::fct_reorder(.f = uf, .x = obitos_novos, .desc = TRUE)
   ) %>%
   ggplot() +
   geom_boxplot(aes(x = estado, y = obitos_novos, fill = estado), color = "white", show.legend = FALSE) +
   labs(
      x = "Estados",
      y = "Novos Óbitos",
      title = "Novos Óbitos"
   ) +
   tema_bruno()


# Para a animação dos dados... --------------------------------------------
animacao_obitos_ufs <- covid_estados %>%
   group_by(uf, date) %>%
   summarise(obitos_acumulados = sum(obitos_acumulados)) %>%
   mutate(num_obitos = max(obitos_acumulados)) %>%
   ungroup() %>%
   mutate(
      limite = num_obitos %>%
         unique() %>%
         sort(decreasing = TRUE) %>%
         nth(10)
   ) %>% glimpse()
filter(num_obitos >= limite) %>%
   ggplot(aes(y = obitos_acumulados, x = date, color = uf)) +
   labs(
      x = "Data",
      y = "Óbitos Acumulados",
      title = "Óbitos Acumulados",
      subtitle = "Nos 10 maiores estados"
   ) +
   scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y") +
   geom_line(show.legend = FALSE) +
   geom_label(aes(label = uf), show.legend = FALSE) +
   tema_bruno() +
   gganimate::transition_reveal(date)

animacao_contagios_ufs <- covid_estados %>%
   group_by(uf, date) %>%
   summarise(contagios_acumulados = sum(contagios_acumulados)) %>%
   mutate(num_contagios = max(contagios_acumulados)) %>%
   ungroup() %>%
   mutate(
      limite = num_contagios %>%
         unique() %>%
         sort(decreasing = TRUE) %>%
         nth(10)
   ) %>%
   filter(num_contagios >= limite) %>%
   ggplot(aes(y = contagios_acumulados, x = date, color = uf)) +
   labs(
      x = "Data",
      y = "Contágios Acumulados",
      title = "Contágios Acumulados",
      subtitle = "Nos 10 maiores estados"
   ) +
   scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y") +
   geom_line(show.legend = FALSE) +
   geom_label(aes(label = uf), show.legend = FALSE) +
   tema_bruno() +
   gganimate::transition_reveal(date)


# Gráfico mesclando linhas e colunas - Como os do jornal... ---------------
covid_brasil %>%
   mutate(
      contag_novos_mm7 = last(contagios_novos_mm7)
   ) %>%
   ggplot() +
   geom_col(aes(x = date, y = contagios_novos, fill = contagios_novos), color = "cyan", show.legend = FALSE) +
   geom_line(aes(x = date, y = contagios_novos_mm7), color = "yellow", size = 1L) +
   scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y") +
   tema_bruno() +
   labs(
      x = "Data",
      y = "Novos Contágios",
      title = "Volumes Diários de Novos Contágios",
      subtitle = "Em todo o Brasil"
   )


covid_estados %>%
   mutate(
      uf = forcats::fct_reorder(.f = uf, .x = taxa_mortalidade, .desc = TRUE)
   ) %>%
   ggplot() +
   geom_line(aes(x = date, y = taxa_mortalidade), color = "red") +
   scale_x_date(date_breaks = "1 month", date_labels = "%m") +
   tema_bruno() +
   labs(
      x = "Mês",
      y = "Taxa de Mortalidade",
      title = "Taxas de Mortalidade",
      subtitle = "Estados Ordenados por Taxa de Mortalidade"
   ) +
   facet_wrap(vars(uf), scales = "free")


ggplot(covid_estados) +
   aes(x = date, y = contagios_novos_mm7) +
   scale_x_date(date_breaks = "1 month", date_labels = "%m") +
   geom_line(colour = "cyan") +
   labs(
      x = "Mês",
      y = "Média Móvel de Novos Contágios",
      title = "Média Móvel de Novos Contágios"
   ) +
   tema_bruno() +
   facet_wrap(vars(uf), scales = "free")


temporal_facetada <- function(arquivo, quebra, variavel) {
   arquivo %>%
      ggplot() +
      geom_line(aes_string(x = "date", y = variavel), size = 1L, colour = "cyan") +
      labs(x = "Data") +
      tema_bruno() +
      facet_wrap(vars(uf))
}

temporal_facetada(
   arquivo = "covid_estados",
   quebra = "uf",
   variavel = "contagios_novos_mm7")


# Função de Correlação Cruzada --------------------------------------------
teste <- covid %>%
   select(
      date,
      contagios_novos,
      obitos_novos
      )

correlacoes <- c()
lags <- c(0:50)

for (k in lags) {

   tmp <- teste %>%
      mutate(lagk = lag(contagios_novos, k)) %>%
      select(obitos_novos, lagk) %>%
      na.omit()

   correlacoes <- c(correlacoes, cor(tmp$obitos_novos, tmp$lagk))
}

tibble(lags, correlacoes) %>%
   ggplot(aes(x = lags, y = correlacoes)) +
   geom_point(color = "yellow") +
   geom_line(color = "green") +
   labs(
      x = "Número de Lags",
      y = "Correlações",
      title = "Correlações Cruzadas entre Contágios e Óbitos"
   ) +
   tema_bruno()


# Mapas -------------------------------------------------------------------
sum(sf::st_area(x = tabela_ufs))

covid_estados %>%
   arrange(uf, date) %>%
   group_by(uf) %>%
   filter(date == max(date, na.rm = TRUE)) %>%
   ungroup() %>%
   select(-c(contagios_novos_100k:obitos_acumulados_ln)) %>%
   arrange(uf, date) %>%
   left_join(
      y = tabela_ufs,
      by = c("uf" = "abbrev_state")
   ) %>%
   ggplot() +
   geom_sf(aes(geometry = geom, fill = taxa_mortalidade), color = "darkcyan") +
   geom_sf_text(aes(geometry = geom, label = uf), size = 3.5) +
   scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red"
   ) +
   scale_x_continuous(breaks = seq(-75, -30, 5)) +
   scale_y_continuous(breaks = seq(-35, 5, 5)) +
   labs(
      x = "Longitude",
      y = "Latitude",
      title = "Taxa de Mortalidade da COVID-19",
      subtitle = "Óbitos / Contágios"
   ) +
   tema_bruno() +
   theme(
      legend.key.size = unit(1, "cm"),
      legend.title = element_blank()
   )


sum(sf::st_area(x = tabela_mun))


covid_cidades %>%
   filter(regiao == "Sudeste") %>%
   arrange(cod_ibge, date) %>%
   group_by(cod_ibge) %>%
   filter(date == max(date, na.rm = TRUE)) %>%
   ungroup() %>%
   arrange(uf, date) %>%
   left_join(
      y = tabela_ufs,
      by = c("uf" = "abbrev_state")
   ) %>%
   select(geom_uf = geom, everything()) %>%
   arrange(cod_ibge, date) %>%
   left_join(
      y = tabela_mun,
      by = c("cod_ibge" = "code_muni")
   ) %>%
   select(geom_mun = geom, everything()) %>%
   ggplot() +
   geom_sf(aes(geometry = geom_mun, fill = contagios_acumulados_100k), color = "darkcyan") +
   geom_sf(aes(geometry = geom_uf), alpha = 0, color = "blue", size = 1L) +
   scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red"
   ) +
   scale_x_continuous(limits = c(-54, -38), breaks = seq(-54, -38, 2)) +
   scale_y_continuous(limits = c(-26, -14), breaks = seq(-26, -14, 2)) +
   labs(
      x = "Longitude",
      y = "Latitude",
      title = "Contágios Acumulados na Região Sudeste",
      subtitle = "Por grupo de 100 mil habitantes"
   ) +
   tema_bruno() +
   theme(
      legend.key.size = unit(1, "cm"),
      legend.title = element_blank()
   )

covid_cidades %>%
   filter(regiao == "Nordeste") %>%
   arrange(cod_ibge, date) %>%
   group_by(cod_ibge) %>%
   filter(date == max(date, na.rm = TRUE)) %>%
   ungroup() %>%
   arrange(uf, date) %>%
   left_join(
      y = tabela_ufs,
      by = c("uf" = "abbrev_state")
   ) %>%
   select(geom_uf = geom, everything()) %>%
   arrange(cod_ibge, date) %>%
   left_join(
      y = tabela_mun,
      by = c("cod_ibge" = "code_muni")
   ) %>%
   select(geom_mun = geom, everything()) %>%
   ggplot() +
   geom_sf(aes(geometry = geom_mun, fill = contagios_acumulados_100k), color = "darkcyan") +
   geom_sf(aes(geometry = geom_uf), alpha = 0, color = "blue", size = 1L) +
   scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red"
   ) +
   # scale_x_continuous(limits = c(-54, -38), breaks = seq(-54, -38, 2)) +
   # scale_y_continuous(limits = c(-26, -14), breaks = seq(-26, -14, 2)) +
   labs(
      x = "Longitude",
      y = "Latitude",
      title = "Contágios Acumulados na Região Nordeste",
      subtitle = "Por grupo de 100 mil habitantes"
   ) +
   tema_bruno() +
   theme(
      legend.key.size = unit(1, "cm"),
      legend.title = element_blank()
   )


# Analisando as bases de dados de mapas -----------------------------------
cls()

states_small <- geobr::read_state(
   code_state = "all",
   year = 2019,
   simplified = TRUE,
   showProgress = TRUE
)

states_big <- geobr::read_state(
   code_state = "all",
   year = 2019,
   simplified = FALSE,
   showProgress = TRUE
)

cities_small <- geobr::read_municipality(
   code_muni = "all",
   year = 2019,
   simplified = TRUE,
   showProgress = TRUE
)

cities_big <- geobr::read_municipality(
   code_muni = "all",
   year = 2019,
   simplified = FALSE,
   showProgress = TRUE
)

object.size(states_small)
object.size(states_big)
object.size(cities_small)
object.size(cities_big)

sum(sf::st_area(x = states_small))
sum(sf::st_area(x = states_big))
sum(sf::st_area(x = cities_small))
sum(sf::st_area(x = cities_big))

rm(states_small, states_big, cities_small, cities_big)
