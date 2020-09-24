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

   cat("\n", "Atualizacao de dados iniciada em ", as.character(Sys.time()), sep = "")

   cat("\n\n", "Etapa 1: Carregando dados do Portal Brasil.io.",
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
   tempo_mapa_estados <- as.double(system.time(load(file = "data/tabela_ufs.rda",
                                                    envir = globalenv()))[3])
   tempo_mapa_municip <- as.double(system.time(load(file = "data/tabela_mun.rda",
                                                   envir = globalenv()))[3])
   cat("\n", "Concluida a carga de Informacoes Auxiliares em ",
       sum(tempo_infos_geo, tempo_semana_epid, tempo_infos_municip,
           tempo_mapa_estados, tempo_mapa_municip),
       " segundos.", "\n\n", sep = "")

   cat("\n\n", "Etapa 4: Organizando todas as informacoes levantadas.",
       "\n\n", sep = "")
   tempo_base_covid <- as.double(system.time(processa_final())[3])
   cat("\n", "Concluida a organizacao de todas as informacoes levantadas em ",
       tempo_base_covid, " segundos.", "\n\n", sep = "")


   cat("\n\n", "Parabens! Agora voce esta com a base atualizada!",
       "\n\n", "O processamento foi concluido em ",
       sum(tempo_covid_brasilio, tempo_covid_minister, tempo_infos_geo,
           tempo_semana_epid, tempo_infos_municip,
           tempo_mapa_estados, tempo_mapa_municip, tempo_base_covid),
       " segundos.", "\n\n", "Segue um meta-resumo da base:", "\n\n", sep = "")

   rm(semana_epid, envir = globalenv())
   rm(infos_chaves, envir = globalenv())
   rm(covid_brasilio, envir = globalenv())
   rm(covid_ministerio, envir = globalenv())
   rm(infos_geograficas, envir = globalenv())

   covid %>%
      dplyr::glimpse()
}

#' Gera as Bases de Dados Derivadas para a geracao de graficos.
#'
#' A partir da para de dados atualizada com dados da pandemia de COVID-19 (com dados do Portal Brasil.io e do Ministerio da Saude), gera as versoes derivadas aos niveis de Cidades, Regioes de Saude, Estados, Regioes Politicas e Brasil inteiro. Indicadores normalizados tambem sao acrescentados para um leque de analises.
#'
#' @return Bases derivadas com sumarizacoes e indicadores normalizados nos niveis de Cidades, Regioes de Saude, Estados, Regioes Politicas e Brasil inteiro.
#'
#' @export
bases_derivadas <- function() {
   cls()

   cat("\n", "Etapa 1: Gerando as sumarizacoes de area e populacao.",
       "\n\n", sep = "")
   tempo_bases_sumario <- as.double(system.time(sumarios_derivacoes())[3])
   cat("\n", "Concluida a geracao das sumarizacoes de area e populacao em ",
       tempo_bases_sumario, " segundos.", "\n\n", sep = "")

   cat("\n\n", "Etapa 2: Gerando a Base Derivada ao Nivel de Cidades.",
       "\n\n", sep = "")
   tempo_base_cidades <- as.double(system.time(base_cidades())[3])
   cat("\n", "Concluida a geracao da Base Derivada ao Nivel de Cidades em ",
       tempo_base_cidades, " segundos.", "\n\n", sep = "")

   cat("\n\n", "Etapa 3: Gerando a Base Derivada ao Nivel de Regioes de Saude.",
       "\n\n", sep = "")
   tempo_base_reg_saude <- as.double(system.time(base_regioes_saude())[3])
   cat("\n", "Concluida a geracao da Base Derivada ao Nivel de Regioes de Saude em ",
       tempo_base_reg_saude, " segundos.", "\n\n", sep = "")

   cat("\n\n", "Etapa 4: Gerando a Base Derivada ao Nivel de Estados.",
       "\n\n", sep = "")
   tempo_base_estados <- as.double(system.time(base_estados())[3])
   cat("\n", "Concluida a geracao da Base Derivada ao Nivel de Estados em ",
       tempo_base_estados, " segundos.", "\n\n", sep = "")

   cat("\n\n", "Etapa 5: Gerando a Base Derivada ao Nivel de Regioes do Brasil.",
       "\n\n", sep = "")
   tempo_base_reg_brasil <- as.double(system.time(base_regioes_brasil())[3])
   cat("\n", "Concluida a geracao da Base Derivada ao Nivel de Regioes do Brasil em ",
       tempo_base_reg_brasil, " segundos.", "\n\n", sep = "")

   cat("\n\n", "Etapa 6: Gerando a Base Derivada ao Nivel de Brasil.",
       "\n\n", sep = "")
   tempo_base_brasil <- as.double(system.time(base_brasil())[3])
   cat("\n", "Concluida a geracao da Base Derivada ao Nivel de Brasil em ",
       tempo_base_brasil, " segundos.", "\n\n", sep = "")


   cat("\n\n", "Parabens! Agora voce esta com todas as bases derivadas!",
       "\n\n", "O processamento foi concluido em ",
       sum(tempo_bases_sumario, tempo_base_cidades, tempo_base_reg_saude,
           tempo_base_estados, tempo_base_reg_brasil, tempo_base_brasil),
       " segundos.", "\n\n", "Segue a relacao de bases disponiveis:",
       "\n\n", sep = "")

   rm(sumario_brasil, envir = globalenv())
   rm(sumario_estados, envir = globalenv())
   rm(sumario_regioes_saude, envir = globalenv())
   rm(sumario_regioes_brasil, envir = globalenv())

   ls(envir = globalenv())
}
