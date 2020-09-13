#' Atualiza os dados da COVID no Brasil.
#'
#' Realiza a atualizacao de dados da pandemia de COVID-19 com dados do Portal Brasil.io e do Ministerio da Saude.
#'
#' ATENCAO: Esta funcao consome algum tempo e uma quantidade razoavel de memoria RAM.
#'
#' @return Base atualizada ate o momento da COVID-19 no Brasil.
#'
#' @export
atualiza_dados <- function() {
   cls()

   cat("\n", "Etapa 1: Carregando dados do Portal Brasil.io.",
       "\n\n", "Por favor aguarde...", "\n\n", sep = "")
   tempo_covid_brasilio <- as.double(system.time(covid_brasilio <- le_brasil_io())[3])
   cat("\n", "Concluida a importacao de dados do Portal Brasil.io em ",
       tempo_covid_brasilio, " segundos.", "\n\n", sep = "")

   cat("\n\n", "Etapa 2: Carregando dados do Ministerio da Saude.",
       "\n\n", "Por favor aguarde mais um pouco...", "\n\n", sep = "")
   tempo_covid_minister <- as.double(system.time(covid_ministerio <- le_ministerio())[3])
   cat("\n", "Concluida a importacao de dados do Ministerio da Saude em ",
       tempo_covid_minister, " segundos.", "\n\n", sep = "")

   cat("\n\n", "Etapa 3: Carregando Informacoes Auxiliares.", "\n\n", sep = "")
   tempo_infos_geo <- as.double(system.time(load(file = "data/infos_geograficas.rda",
                                                        envir = globalenv()))[3])
   tempo_semana_epid <- as.double(system.time(load(file = "data/semana_epid.rda",
                                                   envir = globalenv()))[3])
   tempo_infos_municip <- as.double(system.time(deriva_codigo_municipio())[3])
   cat("\n", "Concluida a carga de Informacoes Auxiliares em ",
       sum(tempo_infos_geo, tempo_semana_epid, tempo_infos_municip),
       " segundos.", "\n\n", sep = "")

   cat("\n\n", "Etapa 4: Organizando todas as informacoes levantadas.", "\n",
       "Por favor aguarde...", "\n", sep = "")
   tempo_base_covid <- as.double(system.time(processa_final())[3])
   cat("\n", "Concluida a organizacao de todas as informacoes levantadas em ",
       tempo_base_covid, " segundos.", "\n\n", sep = "")


   cat("\n\n", "Parabens! Agora voce esta com a base atualizada!",
       "\n\n", "O processamento foi concluido em ",
       sum(tempo_covid_brasilio, tempo_covid_minister, tempo_infos_geo,
           tempo_semana_epid, tempo_infos_municip, tempo_base_covid),
       " segundos.", "\n\n", "Segue um resumo da base:", "\n\n", sep = "")

   rm(list = ls(pattern = "_"))

   covid %>%
      dplyr::glimpse()
}
