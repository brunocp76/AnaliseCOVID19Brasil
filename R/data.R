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
#' Um Banco de Dados auxiliares sobre as primeiras Semanas Epidemiologicas para uso nas analises. Este banco de dados cobre as primeiras 69 semanas epidemiologicas, uma vez que as bases de dados atualizadas contem informacao somente a partir da semana epidemiologica 9.
#'
#' @format Uma tibble com 365 linhas e 2 colunas:
#' \describe{
#'   \item{date}{Data}
#'   \item{semana_epidem}{Semana Epidemiologica correspondente a data}
#' }
#' @source \url{https://opendatasus.saude.gov.br/dataset/bd-srag-2020}
"semana_epid"


#' Tabela com Mapa dos Estados do Brasil.
#'
#' Um Banco de Dados auxiliares com informacoes geograficas e cartograficas dos estados do Brasil. Os dados aparentemente sao oficiais, vindos do IBGE (Instituto Brasileiro de Geografia e Estatistica).
#'
#' @format Uma tibble com 27 linhas e 6 colunas:
#' \describe{
#'   \item{code_state}{Codigo do Estado (de acordo com o IBGE)}
#'   \item{abbrev_state}{Sigla do Estado (Unidade Federativa)}
#'   \item{name_state}{Nome do Estado}
#'   \item{code_region}{Codigo da Regiao (de acordo com o IBGE)}
#'   \item{name_region}{Nome da Regiao}
#'   \item{geom}{Poligono com a Geometria do Estado}
#' }
#' @source \url{https://github.com/ipeaGIT/geobr}
"tabela_ufs"


#' Tabela com Mapa dos Municipios do Brasil.
#'
#' Um Banco de Dados auxiliares com informacoes geograficas e cartograficas dos municipios do Brasil. Os dados aparentemente sao oficiais, vindos do IBGE (Instituto Brasileiro de Geografia e Estatistica).
#'
#' @format Uma tibble com 5.572 linhas e 8 colunas:
#' \describe{
#'   \item{code_muni}{Codigo do Municipio (de acordo com o IBGE)}
#'   \item{name_muni}{Nome do Municipio}
#'   \item{code_state}{Codigo do Estado (de acordo com o IBGE)}
#'   \item{abbrev_state}{Sigla do Estado (Unidade Federativa)}
#'   \item{name_state}{Nome do Estado}
#'   \item{code_region}{Codigo da Regiao}
#'   \item{name_region}{Nome da Regiao}
#'   \item{geom}{Poligono com a Geometria do Estado}
#' }
#' @source \url{https://github.com/ipeaGIT/geobr}
"tabela_mun"
