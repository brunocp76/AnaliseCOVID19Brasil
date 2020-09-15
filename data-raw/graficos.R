# Não entram no código principal... ---------------------------------------
library(dplyr)
library(ggplot2)
library(patchwork)
library(gganimate)

# Tentando definir um tema padrão para meus gráficos... -------------------
tema_bruno <- function() {
   ggplot2::theme(
      # Bloco sobre a legenda...
      legend.position = "bottom",
      legend.background = element_rect(fill = "black", color = "black"),
      legend.box.background = element_rect(fill = "black", color = "black"),
      legend.key = element_rect(fill = "black"),
      # Bloco sobre o fundo...
      panel.background = element_rect(fill = "black"),
      plot.background = element_rect(fill = "black", color = "cyan"),
      # Bloco sobre as linhas de grade...
      panel.grid.major.x = element_line(color = "#324C63", size = 0.5, linetype = "dotted"),
      panel.grid.major.y = element_line(color = "#324C63", size = 0.5),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(color = "#324C63", size = 0.2, linetype = "dotted"),
      # Bloco sobre os textos...
      plot.title = element_text(hjust = 0.5),
      text = element_text(colour = "#11a2c6"),
      # Bloco sobre os eixos...
      axis.text = element_text(color = "#3465A4"),
      axis.ticks.x = element_line(color = "#324C63"),
      axis.ticks.y = element_line(color = "#324C63"),
      axis.line.x = element_blank()
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
   ) %>%
   filter(num_obitos >= limite) %>%
   ggplot(aes(y = obitos_acumulados, x = date, color = uf)) +
   labs(
      x = "Data",
      y = "Óbitos Acumulados",
      title = "Óbitos Acumulados",
      subtitle = "Nos 10 maiores estados"
   ) +
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
   geom_line(show.legend = FALSE) +
   geom_label(aes(label = uf), show.legend = FALSE) +
   tema_bruno() +
   gganimate::transition_reveal(date)

animacao_contagios_ufs + animacao_obitos_ufs

# Gráfico mesclando linhas e colunas - Como os do jornal... ---------------
covid_estados %>%
   group_by(uf) %>%
   summarise(
      evolucao_contagios_mm7 = last(evolucao_contagios_mm7),
      evolucao_obitos_mm7 = last(evolucao_obitos_mm7)
   ) %>%
   ungroup() %>%
   arrange(uf) -> evolucao_estados_contagios



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


define_sucesso <- function(arquivo, variavel) {
   arquivo %>%
      mutate(
         resultado = case_when(
            dplyr::lag(x = variavel, n = 6L) / variavel
         )
      )
}

define_sucesso(arquivo = covid_estados, variavel = "contagios_novos_mm7")

ggplot(covid_estados) +
   aes(x = date, y = contagios_novos_mm7) +
   geom_line(size = 1L, colour = "cyan") +
   labs(x = "Data") +
   tema_bruno() +
   facet_wrap(vars(uf))


temporal_facetada <- function(arquivo, quebra, variavel) {
   # rlang::parse_expr(arquivo)
   # rlang::parse_expr(quebra)
   # print(variavel)

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
#


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
