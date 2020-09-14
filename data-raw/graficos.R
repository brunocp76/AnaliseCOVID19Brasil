# Não entram no código principal... ---------------------------------------
library(dplyr)
# library(ggdark)
library(ggplot2)
# library(ggthemes)
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
      # panel.grid.minor = element_line(color = "white"),
      # Bloco sobre as linhas de grade...
      panel.grid.major.x = element_line(color = "#324C63", size = 0.5, linetype = "dotted"),
      panel.grid.major.y = element_line(color = "#324C63", size = 0.5),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(color = "#324C63", size = 0.2, linetype = "dotted"),
      # Bloco sobre os textos...
      plot.title = element_text(
         # family = "Get Schwifty",
         hjust = 0.5
         # size = 30
      ),
      text = element_text(
         colour = "#11a2c6"
         # family = "Get Schwifty",
         # size = 16
      ),
      # Bloco sobre os eixos...
      axis.text = element_text(color = "#3465A4"),
      axis.ticks.x = element_line(color = "#324C63"),
      axis.ticks.y = element_line(color = "#324C63"),
      axis.line.x = element_blank()
   )
}



# Tentando alguns gráficos ------------------------------------------------
## Gráficos de Linha - Escala Normal
covid_cidades %>%
   filter(municipio %in% c("Sao Paulo", "Campinas", "Indaiatuba", "Atibaia")) %>%
   ggplot() +
   geom_line(aes(x = date, y = contagios_novos, color = municipio)) +
   scale_x_date(date_breaks = "1 month") +
   labs(x = "Data") +
   tema_bruno()

## Gráficos de Linha - Escala Normalizada
covid_cidades %>%
   filter(municipio %in% c("Sao Paulo", "Campinas", "Indaiatuba", "Atibaia")) %>%
   ggplot() +
   geom_line(aes(x = date, y = contagios_novos_100k, color = municipio)) +
   scale_x_date(date_breaks = "1 month") +
   labs(x = "Data") +
   tema_bruno()

## Gráficos de Linha - Escala Normalizada
covid_cidades %>%
   filter(municipio %in% c("Sao Paulo", "Campinas", "Indaiatuba", "Atibaia")) %>%
   ggplot() +
   geom_line(aes(x = date, y = contagios_novos_ln, color = municipio)) +
   scale_x_date(date_breaks = "1 month") +
   labs(x = "Data") +
   tema_bruno() +
   scale_linetype_manual(values = c("white", "orange", "olive", "yellow"))


## Boxplots
covid_cidades %>%
   mutate(
      semana_epidemiologica = as.factor(semana_epidem)
   ) %>%
   ggplot() +
   geom_boxplot(aes(x = semana_epidemiologica, y = obitos_novos), fill = "white", color = "black")

covid_estados %>%
   mutate(
      estado = as.factor(uf)
   ) %>%
   ggplot() +
   geom_boxplot(aes(x = estado, y = contagios_novos), fill = "white", color = "black") +
   ylim(c(0, 20000))



## Gráfico mesclando linhas e colunas
covid_estados %>%
   group_by(uf) %>%
   summarise(
      evolucao_contagios_mm7 = last(evolucao_contagios_mm7),
      evolucao_obitos_mm7 = last(evolucao_obitos_mm7)
   ) %>%
   ungroup() %>%
   arrange(uf) -> evolucao_estados_contagios



covid_estados %>%
   mutate(
      uf = forcats::fct_reorder(.f = uf, .x = contagios_acumulados, .desc = TRUE)
   ) %>%
   ggplot() +
   geom_col(aes_string(x = "date", y = "contagios_novos"), fill = "#34EEA4", show.legend = FALSE) +
   geom_line(aes(x = date, y = contagios_novos_mm7), color = "red", size = 1L) +
   # scale_x_date(date_breaks = "2 weeks") +
   tema_bruno() +
   labs(x = "Data") +
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
      # if (!is.na(filtro)) {filter(!! rlang::parse_expr()) %>%}
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
