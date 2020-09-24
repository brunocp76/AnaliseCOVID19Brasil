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
      recuperados_novos,
      em_acompanh_acumulados,
      contagios_acumulados,
      obitos_acumulados,
      recuperados_acumulados,
      log2_obitos_novos,
      log2_obitos_acumulados
   ) %>%
   summary()

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
      # soma_aco = sum(em_acompanh_novos, na.rm = TRUE),
      soma_con = sum(contagios_novos, na.rm = TRUE),
      soma_obi = sum(obitos_novos, na.rm = TRUE)
      # soma_rec = sum(recuperados_novos, na.rm = TRUE)
   )


# Dados do Brasil.io ------------------------------------------------------
datacovidbr::brasilio(silent = TRUE) %>%
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
   janitor::clean_names() -> covid_brasilio

# Conferência rápida...
cls()

covid_brasilio %>%
   glimpse()

covid_brasilio %>%
   object.size()

covid_brasilio %>%
   select(
      date,
      pop_est_2019,
      contagios_novos,
      obitos_novos,
      contagios_acumulados,
      obitos_acumulados
   ) %>%
   summary()

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


# Dados do Ministério da Saúde --------------------------------------------
datacovidbr::brMinisterioSaude(silent = TRUE) %>%
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
   janitor::clean_names() -> covid_ministerio

# Rápida conferência...
cls()

covid_ministerio %>%
   glimpse()

covid_ministerio %>%
   object.size()

covid_ministerio %>%
   select(
      date,
      pop_tcu_2019,
      contagios_novos,
      obitos_novos,
      contagios_acumulados,
      obitos_acumulados
   ) %>%
   summary()

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


# Conseguindo as Informações Geográficas dos Municípios Brasileiros -------

## Latitude, Longitude e Indicador de Capital...
readr::read_csv(
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
   distinct(
      cod_ibge,
      lat,
      lon,
      capital
   ) %>%
   arrange(
      cod_ibge
   ) %>%
   # Garantindo que todos os nomes estejam arrumados...
   janitor::clean_names() -> coords_munic

# Rápida conferência...
cls()

coords_munic %>%
   glimpse()

coords_munic %>%
   object.size()

coords_munic %>%
   select(capital) %>%
   sum()


## Área dos Municípios...
readr::read_delim(
   file = "data-raw/Dados_Area_Municipios.txt",
   delim = "\t",
   col_names = TRUE,
   trim_ws = TRUE,
   guess_max = 5700,
   skip_empty_rows = TRUE
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
   distinct(
      cod_ibge,
      area_mun_km2
   ) %>%
   arrange(
      cod_ibge
   ) %>%
   # Garantindo que todos os nomes estejam arrumados...
   janitor::clean_names() -> area_munic

# Rápida conferência...
cls()

area_munic %>%
   glimpse()

area_munic %>%
   object.size()

area_munic %>%
   summary()

area_munic %>%
   select(area_mun_km2) %>%
   sum() %>%
   scales::comma(
      accuracy = 0.01,
      big.mark = ".",
      decimal.mark = ","
   )


## Combinando todas as Informações Geográficas...
coords_munic %>%
   full_join(
      y = area_munic,
      by = "cod_ibge"
   ) %>%
   mutate(
      capital = as.integer(
         ifelse(test = !is.na(capital),
                yes = capital,
                no = 0)
      )
   ) %>%
   select(
      cod_ibge,
      lat,
      lon,
      area_mun_km2,
      capital
   ) %>%
   arrange(cod_ibge) %>%
   distinct(
      cod_ibge,
      lat,
      lon,
      area_mun_km2,
      capital
   ) %>%
   arrange(cod_ibge) -> infos_geograficas

# Rápida conferência...
cls()

infos_geograficas %>%
   glimpse()

infos_geograficas %>%
   object.size()

infos_geograficas %>%
   summary()

infos_geograficas %>%
   select(capital) %>%
   sum()

infos_geograficas %>%
   select(area_mun_km2) %>%
   sum() %>%
   scales::comma(
      accuracy = 0.01,
      big.mark = ".",
      decimal.mark = ","
   )

rm(coords_munic, area_munic)


# Conseguindo a Semana Epidemiológica... ----
cls()

vroom::vroom(
   file = "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2020/INFLUD-21-09-2020.csv"
   , delim = ";"
   , col_names = TRUE
   , col_select = c("DT_NOTIFIC", "SEM_NOT", "DT_SIN_PRI", "SEM_PRI")
   # , n_max = 5
   , trim_ws = TRUE
   # , num_threads = 4
) %>%
   mutate(
      data_pri_sin = lubridate::dmy(DT_SIN_PRI),
      sem_pri_sint = as.integer(SEM_PRI),
      data_notif = lubridate::dmy(DT_NOTIFIC),
      sem_notif = as.integer(SEM_NOT)
   ) %>%
   select(
      data_pri_sin,
      sem_pri_sint,
      data_notif,
      sem_notif
   ) %>%
   arrange(
      data_pri_sin,
      sem_pri_sint,
      data_notif,
      sem_notif
   ) %>%
   distinct(
      data_pri_sin,
      sem_pri_sint,
      data_notif,
      sem_notif
   ) -> temp_sem_epid

# Rápida conferência...
cls()

temp_sem_epid %>%
   glimpse()

temp_sem_epid %>%
   object.size()

temp_sem_epid %>%
   select(
      date = data_pri_sin,
      sem_pri_sint
   ) %>%
   arrange(
      date,
      sem_pri_sint
   ) %>%
   distinct(
      date,
      sem_pri_sint
   ) -> sem_pri_sint

temp_sem_epid %>%
   select(
      date = data_notif,
      sem_notif
   ) %>%
   arrange(
      date,
      sem_notif
   ) %>%
   distinct(
      date,
      sem_notif
   ) -> sem_notif


sem_pri_sint %>%
   full_join(
      y = sem_notif,
      by = "date"
      ) %>%
   as.data.frame()

sem_pri_sint %>%
   full_join(
      y = sem_notif,
      by = "date"
   ) %>%
   filter(sem_pri_sint != sem_notif)

sem_pri_sint %>%
   full_join(
      y = sem_notif,
      by = "date"
   ) %>%
   select(
      date,
      semana_epidem = sem_notif
   ) %>%
   arrange(
      date,
      semana_epidem
   ) %>%
   distinct(
      date,
      semana_epidem
   ) -> semana_epid

rm(temp_sem_epid, sem_pri_sint, sem_notif)

# Rápida conferência...
cls()

semana_epid %>%
   glimpse()

semana_epid %>%
   object.size()

semana_epid %>%
   summary()

semana_epid %>%
   group_by(semana_epidem) %>%
   summarise(
      data_min = min(date, na.rm = TRUE),
      data_max = max(date, na.rm = TRUE),
      qtd_data = n_distinct(date)
   ) %>%
   as.data.frame()


# Conseguindo as Chaves de Códigos de Municípios --------------------------
covid_brasilio %>%
   mutate(cod_mun = as.integer(trunc(cod_ibge / 10))) %>%
   arrange(
      cod_mun,
      date
   ) %>%
   # Juntando as 2 bases grandes...
   full_join(
      y = covid_ministerio,
      by = c("cod_mun", "date"),
      suffix = c("_brasilio", "_ministerio")
   ) %>%
   select(
      cod_ibge,
      cod_mun,
      cod_regiao_saude,
      nome_regiao_saude,
      interior_metropol
   ) %>%
   filter(
      !is.na(cod_ibge),
      !is.na(cod_mun)
   ) %>%
   arrange(
      cod_mun,
      cod_ibge,
      cod_regiao_saude,
      nome_regiao_saude,
      interior_metropol
   ) %>%
   group_by(
      cod_mun,
      cod_ibge
   ) %>%
   summarise(
      cod_regiao_saude_budega = min(cod_regiao_saude, na.rm = TRUE),
      nome_regiao_saude_budega = min(nome_regiao_saude, na.rm = TRUE),
      interior_metropol_budega = min(interior_metropol, na.rm = TRUE)
   ) %>%
   ungroup() %>%
   select(
      cod_mun,
      cod_ibge,
      cod_regiao_saude = cod_regiao_saude_budega,
      nome_regiao_saude = nome_regiao_saude_budega,
      interior_metropol = interior_metropol_budega
   ) %>%
   distinct(
      cod_mun,
      cod_ibge,
      cod_regiao_saude,
      nome_regiao_saude,
      interior_metropol
   ) %>%
   arrange(
      cod_mun
      , cod_ibge
      , cod_regiao_saude
      , nome_regiao_saude
      , interior_metropol
   ) -> infos_chaves

# Rápida conferência...
cls()

infos_chaves %>%
   glimpse()

infos_chaves %>%
   object.size()

infos_chaves %>%
   summary()



# Conseguindo os Dados de Mapas -------------------------------------------
cls()

tabela_ufs <- geobr::read_state(
   code_state = "all",
   year = 2019,
   showProgress = TRUE
)

tabela_mun <- geobr::read_municipality(
   code_muni = "all",
   year = 2019,
   showProgress = TRUE
)

# Rápida Conferência...
tabela_ufs %>% glimpse()

tabela_mun %>% glimpse()

sum(sf::st_area(x = tabela_ufs))

sum(sf::st_area(x = tabela_mun))



# Organizando as Informações que temos... ---------------------------------
cls()

covid_R4DS2 %>% glimpse()

covid_ministerio %>% glimpse()

covid_brasilio %>% glimpse()

infos_chaves %>% glimpse()

infos_geograficas %>% glimpse()

semana_epid %>% glimpse()


covid_brasilio %>%
   mutate(cod_mun = as.integer(trunc(cod_ibge / 10))) %>%
   arrange(
      cod_mun,
      date
   ) %>%
   # Juntando as 2 bases grandes...
   full_join(
      y = covid_ministerio,
      by = c("cod_mun", "date"),
      suffix = c("_brasilio", "_ministerio")
   ) %>%
   select(
      cod_ibge,
      cod_mun,
      date,
      everything(),
      -semana_epidem,
      -interior_metropol
   ) %>%
   mutate(
      cont_nov_brasilio_ministerio = coalesce(
         contagios_novos_brasilio,
         contagios_novos_ministerio
      ),
      cont_nov_ministerio_brasilio = coalesce(
         contagios_novos_ministerio,
         contagios_novos_brasilio
      ),
      obit_nov_brasilio_ministerio = coalesce(
         obitos_novos_brasilio,
         obitos_novos_ministerio
      ),
      obit_nov_ministerio_brasilio = coalesce(
         obitos_novos_ministerio,
         obitos_novos_brasilio
      )
   ) %>%
   # Agregando a chave do Código de Município do IBGE...
   arrange(
      cod_mun
   ) %>%
   left_join(
      y = infos_chaves,
      by = "cod_mun",
      suffix = (c("_big", "_key"))
   ) %>%
   mutate(
      cod_ibge = coalesce(cod_ibge_key, cod_ibge_big),
      cod_regiao_saude = coalesce(cod_regiao_saude_key, cod_regiao_saude_big),
      nome_regiao_saude = coalesce(nome_regiao_saude_key, nome_regiao_saude_big)
   ) %>%
   select(
      date,
      cod_ibge,
      cod_mun,
      everything(),
      -ends_with("_big"),
      -ends_with("_key")
   ) %>%
   # Agregando as Informações Geográficas do Município...
   arrange(
      cod_ibge,
   ) %>%
   left_join(
      y = infos_geograficas,
      by = "cod_ibge"
   ) %>%
   # Agregando a Semana Epidemiológica...
   arrange(
      date
   ) %>%
   left_join(
      y = semana_epid,
      by = "date"
   ) %>%
   # Enfim, finalizando a base...
   select(
      date,
      semana_epidem,
      cod_ibge,
      cod_mun,
      lat,
      lon,
      area_km2 = area_mun_km2,
      capital,
      interior_metropol,
      pop_est_2019,
      pop_tcu_2019,
      municipio_brasilio,
      municipio_ministerio,
      cod_regiao_saude,
      nome_regiao_saude,
      estado_brasilio,
      estado_ministerio,
      regiao,
      contagios_novos_brasilio,
      contagios_novos_ministerio,
      obitos_novos_brasilio,
      obitos_novos_ministerio,
      contagios_acumulados_brasilio,
      contagios_acumulados_ministerio,
      obitos_acumulados_brasilio,
      obitos_acumulados_ministerio,
      everything(),
      -cod_uf
   ) %>%
   arrange(
      cod_ibge,
      date
   ) -> big_temp

# Rápida Conferência...
cls()

big_temp %>%
   glimpse()

big_temp %>%
   object.size()

big_temp %>%
   summarise(
      date_max = max(date, na.rm = TRUE),
      cont_nov_bras_minist = sum(cont_nov_brasilio_ministerio, na.rm = TRUE),
      cont_nov_minist_bras = sum(cont_nov_ministerio_brasilio, na.rm = TRUE),
      obit_nov_bras_minist = sum(obit_nov_brasilio_ministerio, na.rm = TRUE),
      obit_nov_minist_bras = sum(obit_nov_ministerio_brasilio, na.rm = TRUE)
   )


# Testando a semelhança de informações similares... -----------------------
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

testa2("pop_est_2019", "pop_tcu_2019")
testa("municipio")
testa("estado")
testa("contagios_novos")
testa("obitos_novos")
testa("contagios_acumulados")
testa("obitos_acumulados")

big_temp %>%
   mutate(
      pop_2019 = coalesce(pop_est_2019, pop_tcu_2019),
      municipio = coalesce(municipio_brasilio, municipio_ministerio),
      uf = coalesce(estado_brasilio, estado_ministerio),
      regiao = case_when(
         uf %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
         uf %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
         uf %in% c("PR", "RS", "SC") ~ "Sul",
         uf %in% c("DF", "GO", "MS", "MT") ~ "Centro-Oeste",
         uf %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
         TRUE ~ NA_character_
      ),
      contagios_novos = coalesce(contagios_novos_ministerio, contagios_novos_brasilio),
      obitos_novos = coalesce(obitos_novos_ministerio, obitos_novos_brasilio),
      contagios_acumulados = coalesce(contagios_acumulados_ministerio, contagios_acumulados_brasilio),
      obitos_acumulados = coalesce(obitos_acumulados_ministerio, obitos_acumulados_brasilio)
   ) %>%
   select(
      date,
      semana_epidem,
      cod_ibge,
      # cod_mun,
      lat,
      lon,
      area_km2,
      capital,
      interior_metropol,
      pop_2019,
      municipio,
      cod_regiao_saude,
      nome_regiao_saude,
      uf,
      regiao,
      contagios_novos,
      obitos_novos,
      contagios_acumulados,
      obitos_acumulados
   ) %>%
   arrange(cod_ibge, date) -> covid

# Rápida Conferência...
cls()

covid %>%
   object.size()

covid %>%
   glimpse()

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

semana_epid %>%
   filter(semana_epidem >= 9) %>%
   group_by(semana_epidem) %>%
   summarise(
      data_min = min(date, na.rm = TRUE),
      data_max = max(date, na.rm = TRUE),
      data_qtd = n_distinct(date)
   )


# Salvando arquivos temporários... ----------------------------------------

## Base Principal - COVID
cls()

covid %>%
   glimpse()

covid %>%
   summary()

covid %>%
   object.size()

saveRDS(
   object = covid,
   file = "data-raw/covid.rds",
   ascii = FALSE,
   version = 3,
   compress = "xz"
)


## Base Auxiliar - Informações Geográficas
cls()

infos_geograficas %>%
   glimpse()

infos_geograficas %>%
   object.size()

saveRDS(
   object = infos_geograficas,
   file = "data-raw/infos_geograficas.rds",
   ascii = FALSE,
   version = 3,
   compress = "xz"
)


## Base Auxiliar - Primeiras Semanas Epidemiológicas
cls()

semana_epid %>%
   glimpse()

semana_epid %>%
   object.size()

saveRDS(
   object = semana_epid,
   file = "data-raw/semanas_epidemiologicas.rds",
   ascii = FALSE,
   version = 3,
   compress = "xz"
)


## Base Auxiliar - Dados de Mapas dos Estados
cls()

tabela_ufs %>%
   glimpse()

tabela_ufs %>%
   object.size()

saveRDS(
   object = tabela_ufs,
   file = "data-raw/tabela_ufs.rds",
   ascii = FALSE,
   version = 3,
   compress = "xz"
)


## Base Auxiliar - Dados de Mapas dos Municípios
cls()

tabela_mun %>%
   glimpse()

tabela_mun %>%
   object.size()

saveRDS(
   object = tabela_mun,
   file = "data-raw/tabela_mun.rds",
   ascii = FALSE,
   version = 3,
   compress = "xz"
)


## code to prepare `infos_geograficas` dataset goes here

usethis::use_data(
   infos_geograficas,
   internal = FALSE,
   overwrite = TRUE,
   compress = "xz",
   version = 3
)


## code to prepare `semana_epid` dataset goes here

usethis::use_data(
   semana_epid,
   internal = FALSE,
   overwrite = TRUE,
   compress = "xz",
   version = 3
)


## code to prepare `tabela_ufs` dataset goes here

usethis::use_data(
   tabela_ufs,
   internal = FALSE,
   overwrite = TRUE,
   compress = "xz",
   version = 3
)


## code to prepare `tabela_mun` dataset goes here

usethis::use_data(
   tabela_mun,
   internal = FALSE,
   overwrite = TRUE,
   compress = "xz",
   version = 3
)


# Removendo arquivos temporários ------------------------------------------
rm(list = ls(pattern = "_"))
