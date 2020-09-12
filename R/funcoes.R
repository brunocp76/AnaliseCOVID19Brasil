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

   cat("\n", "Carregando dados do Portal Brasil.io.", "\n", "Por favor aguarde...", "\n", sep = "")
   tempo_covid_brasilio <- as.double(system.time(covid_brasilio <- le_brasil_io())[3])
   cat("\n", "Concluida a importacao de dados do Portal Brasil.io em ", tempo_covid_brasilio, "segundos.", "\n", sep = "")
   rm(tempo_covid_brasilio)

   cat("\n", "Carregando dados do Ministerio da Saude.", "\n", "Por favor aguarde...", "\n", sep = "")
   tempo_covid_minister <- as.double(system.time(covid_ministerio <- le_ministerio())[3])
   cat("\n", "Concluida a importacao de dados do Ministerio da Saude em ", tempo_covid_minister, "segundos.", "\n", sep = "")
   rm(tempo_covid_minister)

   cat("\n", "Carregando Informacoes Geograficas Auxiliares.", "\n", sep = "")
   tempo_infos_geo <- as.double(system.time(infos_geograficas <- carrega_auxiliares(infos_geograficas))[3])
   cat("\n", "Concluida a carga de Informacoes Geograficas Auxiliares em ", tempo_infos_geo, "segundos.", "\n", sep = "")
   rm(tempo_infos_geo)

   cat("\n", "Carregando Informacoes Auxiliares de Semanas Epidemiologicas.", "\n", sep = "")
   tempo_semana_epidem <- as.double(system.time(semana_epid <- carrega_auxiliares(semanas_epidemiologicas))[3])
   cat("\n", "Concluida a carga de Informacoes Auxiliares de Semanas Epidemiologicas em ", tempo_semana_epidem, "segundos.", "\n", sep = "")
   rm(tempo_semana_epidem)

   cat("\n", "Carregando Informacoes Auxiliares dos Muncipios.", "\n", sep = "")
   tempo_infos_municip <- as.double(system.time(infos_chaves <- deriva_codigo_municipio())[3])
   cat("\n", "Concluida a carga de Informacoes Auxiliares dos Muncipios em ", tempo_infos_municip, "segundos.", "\n", sep = "")
   rm(tempo_infos_municip)

   cat("\n", "Organizando todas as informacoes levantadas.", "\n", "Por favor aguarde...", "\n", sep = "")
   tempo_base_covid <- as.double(system.time(covid <- processa_final())[3])
   cat("\n", "Concluida a organizacao de todas as informacoes levantadas em ", tempo_base_covid, "segundos.", "\n", sep = "")
   rm(tempo_base_covid)
}





