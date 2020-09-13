#' Informacoes Geograficas Auxiliares.
#'
#' Um Banco de Dados Geograficos auxiliares contendo informacoes adicionais para uso nas analises.
#'
#' @format Uma tibble com 5.572 linhas e 5 colunas:
#' \describe{
#'   \item{cod_ibge}{Codigo de Municipios do IBGE (Instituto Brasileiro de Geografia e Estatistica)}
#'   \item{lat}{Latitude do Centroide do Municipio}
#'   \item{lon}{Longitude do Centroide do Municipio}
#'   \item{area_mun_km2}{Area do Municipio em Km2}
#'   \item{capital}{Indicador binario do Municipio ser capital do estado}
#' }
#' @source \url{Bruno Cesar Pasquini}
"infos_geograficas"

#' Informacoes Auxiliares sobre as Semanas Epidemiologicas.
#'
#' Um Banco de Dados auxiliares sobre as primeiras Semanas Epidemiologicas para uso nas analises. Este banco de dados cobre as primeiras 37 semanas epidemiologicas, uma vez que as bases de dados atualizadas contem informacao somente a partir da semana epidemiologica 9.
#'
#' @format Uma tibble com 274 linhas e 2 colunas:
#' \describe{
#'   \item{date}{Data}
#'   \item{semana_epidem}{Semana Epidemiologica correspondente a data}
#' }
#' @source \url{https://opendatasus.saude.gov.br/dataset/bd-srag-2020}
"semana_epid"

