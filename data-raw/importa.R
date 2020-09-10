# Bibliotecas - Tentar manter isso o mínimo possível... -------------------
library(tidyr)
library(dplyr)

cls <- function() cat("\f")


# Dados do Curso R --------------------------------------------------------
covid_R4DS2 <- readRDS("../CursoR4DS2/data/covid.rds") %>%
   # Filtrando só as linhas necessárias...
   filter(
      regiao != "Brasil",
      !is.na(municipio),
      !is.na(codmun)
   ) %>%
   # Calculando os dados faltantes...
   arrange(codmun, data) %>%
   mutate(
      em_acompanh_acumulados = ifelse(
         test = codmun != lag(codmun),
         yes = ifelse(
            test = is.na(emAcompanhamentoNovos),
            yes = 0,
            no = emAcompanhamentoNovos
         ),
         no = emAcompanhamentoNovos + lag(emAcompanhamentoNovos)
      ),
      recuperados_acumulados = ifelse(
         test = codmun != lag(codmun),
         yes = ifelse(
            test = is.na(Recuperadosnovos),
            yes = 0,
            no = Recuperadosnovos
         ),
         no = Recuperadosnovos + lag(Recuperadosnovos)
      )
   ) %>%
   # Organizando a bagunça dos nomes e reposicionando as colunas...
   select(
      date = data,
      semana_epidem = semanaEpi,
      regiao,
      estado,
      municipio,
      lat,
      lon,
      cod_uf = coduf,
      cod_mun = codmun,
      cod_regiao_saude = codRegiaoSaude,
      nome_regiao_saude = nomeRegiaoSaude,
      pop_tcu_2019 = populacaoTCU2019,
      eh_capital,
      em_acompanh_novos = emAcompanhamentoNovos,
      contagios_novos = casosNovos,
      obitos_novos = obitosNovos,
      recuperados_novos = Recuperadosnovos,
      em_acompanh_acumulados,
      contagios_acumulados = casosAcumulado,
      obitos_acumulados = obitosAcumulado,
      recuperados_acumulados,
      log2_obitos_novos = obitosNovos_log2,
      log2_obitos_acumulados = obitosAcumulado_log2
   ) %>%
   # Deixando a base (bem) mais leve...
   mutate(
      across(
         .cols = c(
            semana_epidem,
            cod_uf,
            cod_mun,
            cod_regiao_saude,
            pop_tcu_2019,
            em_acompanh_novos,
            contagios_novos,
            obitos_novos,
            recuperados_novos,
            em_acompanh_acumulados,
            contagios_acumulados,
            obitos_acumulados,
            recuperados_acumulados
         ),
         .fns = ~ as.integer(.x)
      )
   ) %>%
   # Deixando a base (um pouco) mais leve...
   mutate(
      across(
         .cols = c(
            regiao,
            estado,
            municipio,
            nome_regiao_saude
         ),
         .fns = ~ stringi::stri_trans_general(
            str = stringr::str_trim(.x),
            id = "Latin-ASCII"
         )
      )
   ) %>%
   # Deixando a base (um pouco) mais ajeitada...
   mutate(
      across(
         .cols = c(
            nome_regiao_saude
         ),
         .fns = ~ stringr::str_replace_all(
            string =
               stringr::str_replace_all(
                  string =
                     stringr::str_replace_all(
                        string =
                           stringr::str_to_title(
                              string = .x,
                              locale = "pt_BR"
                           ),
                        pattern = " Da ",
                        replacement = " da "
                     ),
                  pattern = " Do ",
                  replacement = " do "
               )
            ,
            pattern = " De ",
            replacement = " de "
         )
      )
   ) %>%
   # Ordenando para auxiliar no cruzamento dos dados...
   arrange(
      cod_mun,
      date,
      estado,
      municipio
   ) %>%
   # Garantindo que todos os nomes estejam arrumados...
   janitor::clean_names()

# Conferência rápida...
cls()

covid_R4DS2 %>%
   glimpse()

covid_R4DS2 %>%
   object.size()

covid_R4DS2 %>%
   select(
      date,
      em_acompanh_novos,
      contagios_novos,
      obitos_novos,
      recuperados_novos
   ) %>%
   summarise(
      qtd_data = n_distinct(date),
      data_min = min(date, na.rm = TRUE),
      data_max = max(date, na.rm = TRUE),
      soma_aco = sum(em_acompanh_novos, na.rm = TRUE),
      soma_con = sum(contagios_novos, na.rm = TRUE),
      soma_obi = sum(obitos_novos, na.rm = TRUE),
      soma_rec = sum(recuperados_novos, na.rm = TRUE)
   )


# Dados do Brasil.io ------------------------------------------------------
covid_brasilio <- datacovidbr::brasilio() %>%
   # Filtrando só as linhas necessárias...
   filter(
      place_type == "city",
      !is.na(city),
      !is.na(city_ibge_code)
   ) %>%
   # Calculando os dados faltantes...
   arrange(city_ibge_code, date) %>%
   mutate(
      contagios_novos = ifelse(
         test = city_ibge_code == lag(city_ibge_code),
         yes = confirmed - lag(confirmed),
         no = ifelse(
            test = is.na(confirmed),
            yes = 0,
            no = confirmed
         )
      ),
      obitos_novos = ifelse(
         test = city_ibge_code == lag(city_ibge_code),
         yes = deaths - lag(deaths),
         no = ifelse(
            test = is.na(deaths),
            yes = 0,
            no = deaths
         )
      )
   ) %>%
   # Organizando a bagunça dos nomes e reposicionando as colunas...
   select(
      date,
      estado = state,
      municipio = city,
      cod_ibge = city_ibge_code,
      pop_est_2019 = estimated_population_2019,
      contagios_novos,
      obitos_novos,
      contagios_acumulados = confirmed,
      obitos_acumulados = deaths
      # tipo_local = place_type,
      # ultimo_dado = is_last,
      # taxa_obitos = death_rate,
      # conf_por_100k_hab = confirmed_per_100k_inhabitants,
      # ordem_para_lugar = order_for_place
   ) %>%
   # Deixando a base (bem) mais leve...
   mutate(
      across(
         .cols = c(
            cod_ibge,
            pop_est_2019,
            contagios_novos,
            obitos_novos,
            contagios_acumulados,
            obitos_acumulados
         ),
         .fns = ~ as.integer(.x)
      )
   ) %>%
   # Deixando a base (um pouco) mais leve...
   mutate(
      across(
         .cols = c(
            estado,
            municipio
         ),
         .fns = ~ stringi::stri_trans_general(
            str = stringr::str_trim(.x),
            id = "Latin-ASCII"
         )
      )
   ) %>%
   # Ordenando para auxiliar no cruzamento dos dados...
   arrange(
      cod_ibge,
      date,
      estado,
      municipio
   ) %>%
   # Garantindo que todos os nomes estejam arrumados...
   janitor::clean_names()

# Conferência rápida...
cls()

covid_brasilio %>%
   glimpse()

covid_brasilio %>%
   object.size()

covid_brasilio %>%
   select(
      date,
      contagios_novos,
      obitos_novos
   ) %>%
   summarise(
      qtd_data = n_distinct(date),
      data_min = min(date, na.rm = TRUE),
      data_max = max(date, na.rm = TRUE),
      soma_con = sum(contagios_novos, na.rm = TRUE),
      soma_obi = sum(obitos_novos, na.rm = TRUE)
   )

covid_brasilio %>%
   select(
      date,
      contagios_novos,
      obitos_novos,
      contagios_acumulados,
      obitos_acumulados
   ) %>%
   summary()


# Dados do Ministério da Saúde --------------------------------------------
covid_ministerio <- datacovidbr::brMinisterioSaude() %>%
   # Filtrando só as linhas necessárias...
   filter(
      regiao != "Brasil",
      !is.na(municipio),
      !is.na(codmun)
   ) %>%
   # Organizando a bagunça dos nomes e reposicionando as colunas...
   select(
      date,
      semana_epidem = semanaEpi,
      regiao,
      estado,
      municipio,
      cod_uf = coduf,
      cod_mun = codmun,
      cod_regiao_saude = codRegiaoSaude,
      nome_regiao_saude = nomeRegiaoSaude,
      pop_tcu_2019 = populacaoTCU2019,
      # em_acompanh_novos = emAcompanhamentoNovos,
      contagios_novos = casosNovos,
      obitos_novos = obitosNovos,
      # recuperados_novos = Recuperadosnovos,
      contagios_acumulados = casosAcumulado,
      obitos_acumulados = obitosAcumulado,
      interior_metropol = `interior/metropolitana`
   ) %>%
   # Deixando a base (bem) mais leve...
   mutate(
      across(
         .cols = c(
            semana_epidem,
            cod_uf,
            cod_mun,
            cod_regiao_saude,
            pop_tcu_2019,
            contagios_novos,
            obitos_novos,
            contagios_acumulados,
            obitos_acumulados,
            interior_metropol
         ),
         .fns = ~ as.integer(.x)
      )
   ) %>%
   # Deixando a base (um pouco) mais leve...
   mutate(
      across(
         .cols = c(
            regiao,
            estado,
            municipio,
            nome_regiao_saude
         ),
         .fns = ~ stringi::stri_trans_general(
            str = stringr::str_trim(.x),
            id = "Latin-ASCII"
         )
      )
   ) %>%
   # Deixando a base (um pouco) mais ajeitada...
   mutate(
      across(
         .cols = c(
            nome_regiao_saude
         ),
         .fns = ~ stringr::str_replace_all(
            string =
               stringr::str_replace_all(
                  string =
                     stringr::str_replace_all(
                        string =
                           stringr::str_to_title(
                              string = .x,
                              locale = "pt_BR"
                           ),
                        pattern = " Da ",
                        replacement = " da "
                     ),
                  pattern = " Do ",
                  replacement = " do "
               )
            ,
            pattern = " De ",
            replacement = " de "
         )
      )
   ) %>%
   # Ordenando para auxiliar no cruzamento dos dados...
   arrange(
      cod_mun,
      date,
      estado,
      municipio
   ) %>%
   # Garantindo que todos os nomes estejam arrumados...
   janitor::clean_names()

# Rápida conferência...
cls()

covid_ministerio %>%
   glimpse()

covid_ministerio %>%
   object.size()

covid_ministerio %>%
   select(
      date,
      contagios_novos,
      obitos_novos
   ) %>%
   summarise(
      qtd_data = n_distinct(date),
      data_min = min(date, na.rm = TRUE),
      data_max = max(date, na.rm = TRUE),
      soma_con = sum(contagios_novos, na.rm = TRUE),
      soma_obi = sum(obitos_novos, na.rm = TRUE)
   )

covid_ministerio %>%
   select(
      date,
      contagios_novos,
      obitos_novos,
      contagios_acumulados,
      obitos_acumulados
   ) %>%
   summary()


# Conseguindo as Coordenadas dos Municípios Brasileiros -------------------

coords_munic <- readr::read_csv(
   file = "https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/main/csv/municipios.csv"
) %>%
   # Filtrando só as linhas necessárias...
   filter(
      !is.na(latitude),
      !is.na(longitude)
   ) %>%
   # Organizando a bagunça dos nomes e reposicionando as colunas...
   select(
      cod_ibge = codigo_ibge,
      # municipio = nome,
      lat = latitude,
      lon = longitude,
      # cod_uf = codigo_uf,
      capital
   ) %>%
   # Deixando a base (bem) mais leve...
   mutate(
      across(
         .cols = c(
            cod_ibge,
            capital
         ),
         .fns = ~ as.integer(.x)
      )
   ) %>%
   # Ordenando para auxiliar no cruzamento dos dados...
   arrange(
      cod_ibge
   ) %>%
   # Garantindo que todos os nomes estejam arrumados...
   janitor::clean_names()


# Rápida conferência...
cls()

coords_munic %>%
   glimpse()

coords_munic %>%
   object.size()

coords_munic %>%
   select(capital) %>%
   sum()


# Conseguindo as Áreas dos Municípios Brasileiros -------------------------

area_munic <- readr::read_delim(
   file = "data-raw/Dados_Area_Municipios.txt",
   delim = "\t",
   col_names = TRUE,
   trim_ws = TRUE,
   guess_max = 6000,
   skip_empty_rows = TRUE,
) %>%
   # Filtrando só as linhas necessárias...
   filter(
      !is.na(AR_MUN_2019)
   ) %>%
   # Organizando a bagunça dos nomes e reposicionando as colunas...
   select(
      cod_ibge = CD_GCMUN,
      area_mun_km2 = AR_MUN_2019
   ) %>%
   # Deixando a base (bem) mais leve...
   mutate(
      across(
         .cols = c(
            cod_ibge
         ),
         .fns = ~ as.integer(.x)
      )
   ) %>%
   # Ordenando para auxiliar no cruzamento dos dados...
   arrange(
      cod_ibge
   ) %>%
   # Garantindo que todos os nomes estejam arrumados...
   janitor::clean_names()


# Rápida conferência...
cls()

area_munic %>%
   glimpse()

area_munic %>%
   object.size()

area_munic %>%
   select(area_mun_km2) %>%
   sum() %>%
   scales::comma(
      accuracy = 0.01,
      big.mark = ".",
      decimal.mark = ","
   )


# Organizando as Informações que temos... ---------------------------------

cls()

# covid_R4DS2 %>% glimpse()

covid_ministerio %>% glimpse()

covid_brasilio %>% glimpse()

# coords_munic %>% glimpse()
#
# area_munic %>% glimpse()


covid_brasilio %>%
   select(
      date,
      contagios_novos,
      obitos_novos,
   ) %>%
   arrange(date) %>%
   summarise(
      data_min = min(date, na.rm = TRUE),
      data_max = max(date, na.rm = TRUE),
      data_dist = n_distinct(date),
      soma_cont = sum(contagios_novos, na.rm = TRUE),
      soma_obit = sum(obitos_novos, na.rm = TRUE)
   )

covid_ministerio %>%
   select(
      date,
      contagios_novos,
      obitos_novos,
   ) %>%
   arrange(date) %>%
   summarise(
      data_min = min(date, na.rm = TRUE),
      data_max = max(date, na.rm = TRUE),
      data_dist = n_distinct(date),
      soma_cont = sum(contagios_novos, na.rm = TRUE),
      soma_obit = sum(obitos_novos, na.rm = TRUE)
   )

covid_R4DS2 %>%
   select(date) %>%
   distinct(date) %>%
   arrange(date) %>%
   summarise(
      data_min = min(date, na.rm = TRUE),
      data_max = max(date, na.rm = TRUE),
      data_dist = n_distinct(date))



cls()

temp_brasilio <- coords_munic %>%
   full_join(
      y = area_munic,
      by = "cod_ibge"
   ) %>%
   right_join(
      y = covid_brasilio,
      by = "cod_ibge"
   ) %>%
   mutate(cod_mun = as.integer(trunc(cod_ibge / 10))) %>%
   select(
      cod_ibge,
      cod_mun,
      date,
      lat,
      lon,
      capital,
      area_mun_km2,
      estado,
      municipio,
      pop_est_2019,
      contagios_novos,
      obitos_novos,
      contagios_acumulados,
      obitos_acumulados,
      everything()
   ) %>%
   arrange(cod_ibge, date, cod_mun)

temp_brasilio %>%
   glimpse()


temp_ministerio <- coords_munic %>%
   full_join(
      y = area_munic,
      by = "cod_ibge"
   ) %>%
   mutate(cod_mun = as.integer(trunc(cod_ibge / 10))) %>%
   arrange(cod_mun, cod_ibge) %>%
   right_join(
      y = covid_ministerio,
      by = "cod_mun"
   ) %>%
   select(
      cod_ibge,
      cod_mun,
      date,
      lat,
      lon,
      capital,
      area_mun_km2,
      estado,
      municipio,
      pop_tcu_2019,
      contagios_novos,
      obitos_novos,
      contagios_acumulados,
      obitos_acumulados,
      everything(),
      -cod_uf
   ) %>%
   arrange(cod_ibge, date, cod_mun)

temp_ministerio %>%
   glimpse()


cls()

temp_ministerio %>% glimpse()

temp_brasilio %>% glimpse()



big_temp <- temp_brasilio %>%
   full_join(
      y = temp_ministerio,
      by = c("cod_ibge", "date"),
      suffix = c("_brasilio", "_ministerio")
   ) %>%
   select(
      cod_ibge,
      cod_mun_brasilio,
      cod_mun_ministerio,
      date,
      lat_brasilio,
      lat_ministerio,
      lon_brasilio,
      lon_ministerio,
      capital_brasilio,
      capital_ministerio,
      area_mun_km2_brasilio,
      area_mun_km2_ministerio,
      estado_brasilio,
      estado_ministerio,
      municipio_brasilio,
      municipio_ministerio,
      pop_est_2019,
      pop_tcu_2019,
      contagios_novos_brasilio,
      contagios_novos_ministerio,
      obitos_novos_brasilio,
      obitos_novos_ministerio,
      contagios_acumulados_brasilio,
      contagios_acumulados_ministerio,
      obitos_acumulados_brasilio,
      obitos_acumulados_ministerio,
      everything()
   ) %>%
   arrange(
      cod_ibge,
      date
   )

big_temp %>%
   glimpse()

big_temp %>%
   summarise(
      cont_brasil = sum(contagios_novos_brasilio, na.rm = TRUE),
      cont_minist = sum(contagios_novos_ministerio, na.rm = TRUE),
      obit_brasil = sum(obitos_novos_brasilio, na.rm = TRUE),
      obit_minist = sum(obitos_novos_ministerio, na.rm = TRUE)
   )

testa <- function(var){
   cat(var,"\n")

   filter <- paste0(var,"_brasilio != ",var,"_ministerio", sep = "")


   big_temp %>%
      filter(!! rlang::parse_expr(filter)) %>%
      count() %>%
      as.integer()
}

testa2 <- function(var1, var2){
   cat(var1," x ", var2,"\n")

   filter <- paste0(var1," != ",var2, sep = "")

   big_temp %>%
      filter(!! rlang::parse_expr(filter)) %>%
      count() %>%
      as.integer()
}

cls()
big_temp %>%
   glimpse()

testa("cod_mun")
testa("lat")
testa("lon")
testa("capital")
testa("area_mun_km2")
testa("estado")
testa("municipio")
testa("contagios_novos")
testa("obitos_novos")
testa("contagios_acumulados")
testa("obitos_acumulados")
testa2("pop_est_2019", "pop_tcu_2019")

big_temp %>%
   mutate(
      cod_mun = coalesce(cod_mun_ministerio, cod_mun_brasilio),
      lat = coalesce(lat_ministerio, lat_brasilio),
      lon = coalesce(lon_ministerio, lon_brasilio),
      capital = coalesce(capital_ministerio, capital_brasilio),
      area_mun_km2 = coalesce(area_mun_km2_ministerio, area_mun_km2_brasilio),
      estado = coalesce(estado_brasilio, estado_ministerio),
      municipio = coalesce(municipio_brasilio, municipio_ministerio),
      contagios_novos = coalesce(contagios_novos_ministerio, contagios_novos_brasilio),
      obitos_novos = coalesce(obitos_novos_ministerio, obitos_novos_brasilio),
      contagios_acumulados = coalesce(contagios_acumulados_ministerio, contagios_acumulados_brasilio),
      obitos_acumulados = coalesce(obitos_acumulados_ministerio, obitos_acumulados_brasilio),
      pop_2019 = coalesce(pop_est_2019, pop_tcu_2019)
   ) %>%
   select(
      cod_ibge,
      lat,
      lon,
      municipio,
      capital,
      interior_metropol,
      area_mun_km2,
      pop_2019,
      cod_regiao_saude,
      nome_regiao_saude,
      estado,
      regiao,
      date,
      semana_epidem,
      contagios_novos,
      obitos_novos,
      contagios_acumulados,
      obitos_acumulados
   ) %>%
   arrange(cod_ibge, date) -> covid

covid %>%
   select(date, where(is.numeric)) %>%
   summary()

covid %>%
   summarise(
      data_min = min(date, na.rm = TRUE),
      data_max = max(date, na.rm = TRUE),
      data_qtd = n_distinct(date),
      contagios = sum(contagios_novos, na.rm = TRUE),
      obitos = sum(obitos_novos, na.rm = TRUE)
   )

covid %>%
   group_by(semana_epidem) %>%
   summarise(
      data_min = min(date, na.rm = TRUE),
      data_max = max(date, na.rm = TRUE),
      data_qtd = n_distinct(date)
   )

#####
### Falta corrigir a semana epidemiológica...
#####


# Salvando arquivos temporários... ----------------------------------------

saveRDS(
   object = covid,
   file = "data-raw/covid.rds",
   ascii = FALSE,
   version = 3,
   compress = "xz"
)


## code to prepare `importa` dataset goes here

usethis::use_data(
   importa,
   internal = FALSE,
   overwrite = TRUE,
   compress = "xz",
   version = 3
)
