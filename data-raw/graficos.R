# Não entram no código principal... ---------------------------------------
library(ggdark)
library(ggplot2)
library(ggthemes)
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
      panel.grid.major.x = element_line(color = "#324C63", size = 0.5),
      panel.grid.major.y = element_line(color = "#324C63", size = 0.5),
      panel.grid.minor.x = element_line(color = "#324C63", size = 0.2),
      panel.grid.minor.y = element_line(color = "#324C63", size = 0.2),
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
      axis.text = element_text(color = "#244167"),
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


library(ggplot2)

ggplot(covid_estados) +
 aes(x = date, y = taxa_mortalidade) +
 geom_line(size = 1L, colour = "#0c4c8a") +
 theme_minimal() +
 facet_wrap(vars(uf))
## Gráfico mesclando linhas e colunas
covid_cidades %>%
   filter(municipio == "Sao Paulo") %>%
   ggplot() +
   geom_col(aes(x = date, y = contagios_novos), color = "blue") +
   geom_line(aes(x = date, y = contagios_novos_mma7), color = "red") +
   scale_x_date(date_breaks = "2 weeks") +
   dark_mode() +
   labs(x = "Data")

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

covid_estados %>%
   filter(contagios_novos > 0) %>%
   group_by(uf) %>%
   summarize(
      obit_ac_q0 = min(obitos_novos, na.rm = TRUE),
      obit_ac_q1 = quantile(obitos_acumulados, probs = 0.25, na.rm = TRUE),
      obit_ac_q2 = quantile(obitos_acumulados, probs = 0.50, na.rm = TRUE),
      obit_ac_q3 = quantile(obitos_acumulados, probs = 0.75, na.rm = TRUE),
      obit_ac_q4 = quantile(obitos_acumulados, probs = 1, na.rm = TRUE)
   )
