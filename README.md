Analise da COVID-19 no Brasil
================
Bruno César Pasquini

<!-- README.md is generated from README.Rmd. Please edit that file -->

# AnaliseCOVID19Brasil

<!-- badges: start -->
<!-- badges: end -->

O objetivo do pacote AnaliseCOVID19Brasil é o de prover análises
práticas e atualizadas, incluindo alguns gráficos, além de ser o meu
trabalho de conclusão do curso R para Ciência de Dados 2 da
[Curso-R](https://www.curso-r.com/).

Aí você pode se perguntar…

Se tantos pacotes já fazem análises de dados da COVID-19, por que usar
este aqui ~~e não algum outro~~?

Meu primeiro enfoque foi o de fazer um pacote que permita, *de maneira
muito simples*, **atualizar** os dados da pandemia no Brasil **antes**
de proceder às análises. Em outras palavras, toda a triste dinâmica da
pandemia no Brasil vai se revelando conforme este pacote é utilizado ao
longo do tempo.

------------------------------------------------------------------------

## Instalação

Você pode instalar o AnaliseCOVID19Brasil do
[GitHub](https://github.com/) com:

``` r
# install.packages("devtools")
devtools::install_github("brunocp76/AnaliseCOVID19Brasil")
```

## Utilização - Atualização de Dados

Como disse, os primeiros propósitos deste pacotes são ser simples do
usuário utilizar e poder atualizar os dados antes de proceder à qualquer
análise. Então começamos assim:

``` r
library(AnaliseCOVID19Brasil)

atualiza_dados()
#> 
#> Atualizacao de dados iniciada em 2021-04-22 21:17:21
#> 
#> Etapa 1: Carregando dados do Portal Brasil.io.
#> 
#> Por favor aguarde...
#> 
#> -- Column specification --------------------------------------------------------
#> cols(
#>   date = col_date(format = ""),
#>   state = col_character(),
#>   city = col_character(),
#>   place_type = col_character(),
#>   confirmed = col_double(),
#>   deaths = col_double(),
#>   order_for_place = col_double(),
#>   is_last = col_logical(),
#>   estimated_population_2019 = col_double(),
#>   estimated_population = col_double(),
#>   city_ibge_code = col_double(),
#>   confirmed_per_100k_inhabitants = col_double(),
#>   death_rate = col_double()
#> )
#> 
#> Concluida a importacao de dados do Portal Brasil.io em 57.16 segundos.
#> 
#> 
#> 
#> Etapa 2: Carregando dados do Ministerio da Saude.
#> 
#> Por favor aguarde mais um pouco...
#> 
#> Pronto!
#> 
#> 
#> Concluida a importacao de dados do Ministerio da Saude em 538.37 segundos.
#> 
#> 
#> 
#> Etapa 3: Carregando Informacoes Auxiliares.
#> 
#> 
#> Concluida a carga de Informacoes Auxiliares em 4.45 segundos.
#> 
#> 
#> 
#> Etapa 4: Organizando todas as informacoes levantadas.
#> 
#> 
#> Concluida a organizacao de todas as informacoes levantadas em 7.11 segundos.
#> 
#> 
#> 
#> Parabens! Agora voce esta com a base atualizada!
#> 
#> O processamento foi concluido em 607.09 segundos.
#> 
#> Segue um meta-resumo da base:
#> 
#> Rows: 2,184,592
#> Columns: 18
#> $ date                 <date> 2020-03-27, 2020-03-28, 2020-03-29, 2020-03-30, ~
#> $ semana_epidem        <int> 13, 13, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 1~
#> $ cod_ibge             <int> 1100015, 1100015, 1100015, 1100015, 1100015, 1100~
#> $ lat                  <dbl> -11.9283, -11.9283, -11.9283, -11.9283, -11.9283,~
#> $ lon                  <dbl> -61.9953, -61.9953, -61.9953, -61.9953, -61.9953,~
#> $ area_km2             <dbl> 7067.025, 7067.025, 7067.025, 7067.025, 7067.025,~
#> $ capital              <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
#> $ interior_metropol    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
#> $ pop_2019             <int> 22945, 22945, 22945, 22945, 22945, 22945, 22945, ~
#> $ municipio            <chr> "Alta Floresta D'Oeste", "Alta Floresta D'Oeste",~
#> $ cod_regiao_saude     <dbl> 11005, 11005, 11005, 11005, 11005, 11005, 11005, ~
#> $ nome_regiao_saude    <chr> "Zona da Mata", "Zona da Mata", "Zona da Mata", "~
#> $ uf                   <chr> "RO", "RO", "RO", "RO", "RO", "RO", "RO", "RO", "~
#> $ regiao               <chr> "Norte", "Norte", "Norte", "Norte", "Norte", "Nor~
#> $ contagios_novos      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
#> $ obitos_novos         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
#> $ contagios_acumulados <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
#> $ obitos_acumulados    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
```

Talvez você repare que os totais consolidados da data mais recente
disponível (no caso, 2021-04-22) não batem exatamente com os números
oficiais ou o dos telejornais. Se esta discrepância existe entre o
governo e a imprensa por motivos políticos que eu não discutirei aqui,
eu entendo que aqui basta que os números estejam bem próximos aos
números veiculados pela grande imprensa.

    #>   ultima_data total_contagios total_obitos taxa_mortalidade
    #> 1  2021-04-22      14.134.604      382.787            2,71%

Mais uma vez, o meu enfoque aqui é ter estes dados sendo facilmente
atualizados e viabilizando as análises que apresentarei mais para
frente.

Para isso, a função `atualiza_dados()` é uma das funções principais,
pois aciona uma série de funções menores, cada uma fazendo uma parte do
processo de obter pela internet os dados mais atualizados possíveis de
fontes diferentes e de combinar todos eles em uma única base.

Desta forma, depois de executada esta função você terá a base `covid`
construída com dados disponíveis tanto no portal
[Brasil.io](https://brasil.io/) quanto no [Ministério da
Saúde](https://covid.saude.gov.br/)

------------------------------------------------------------------------

## Utilização - Agregações de Dados

``` r
bases_derivadas()
#> 
#> Etapa 1: Gerando as sumarizacoes de area e populacao.
#> 
#> 
#> Concluida a geracao das sumarizacoes de area e populacao em 2.27 segundos.
#> 
#> 
#> 
#> Etapa 2: Gerando a Base Derivada ao Nivel de Cidades.
#> 
#> 
#> Concluida a geracao da Base Derivada ao Nivel de Cidades em 13.04 segundos.
#> 
#> 
#> 
#> Etapa 3: Gerando a Base Derivada ao Nivel de Regioes de Saude.
#> 
#> 
#> Concluida a geracao da Base Derivada ao Nivel de Regioes de Saude em 5.16 segundos.
#> 
#> 
#> 
#> Etapa 4: Gerando a Base Derivada ao Nivel de Estados.
#> 
#> 
#> Concluida a geracao da Base Derivada ao Nivel de Estados em 7.27 segundos.
#> 
#> 
#> 
#> Etapa 5: Gerando a Base Derivada ao Nivel de Regioes do Brasil.
#> 
#> 
#> Concluida a geracao da Base Derivada ao Nivel de Regioes do Brasil em 5.12 segundos.
#> 
#> 
#> 
#> Etapa 6: Gerando a Base Derivada ao Nivel de Brasil.
#> 
#> 
#> Concluida a geracao da Base Derivada ao Nivel de Brasil em 0.39 segundos.
#> 
#> 
#> 
#> Parabens! Agora voce esta com todas as bases derivadas!
#> 
#> O processamento foi concluido em 33.25 segundos.
#> 
#> Segue a relacao de bases disponiveis:
#> [1] "covid"                "covid_brasil"         "covid_cidades"       
#> [4] "covid_estados"        "covid_regioes_brasil" "covid_regioes_saude" 
#> [7] "tabela_mun"           "tabela_ufs"
```

A função `bases_derivadas()` é outra das funções principais, pois parte
da base `covid` e gera funções com os dados para análise (os dados
originais e mais alguns dados derivados) em 5 níveis distintos de
agregação (de município a Brasil inteiro), para viabilizar a análise
nestes níveis.

Então após executar esta função você terá as bases derivadas com os
dados sumarizados em 5 níveis crescentes de agregação:

Segue a relação de bases disponíveis:

-   `covid`
    -   A base atualizada “original”.
-   `covid_cidades`
    -   Dados agregados ao nível de municípios. Embora seja
        estruturalmente similar à base `covid`, esta base e as seguintes
        possuem mais alguns indicadores derivados para análise.
-   `covid_regioes_saude`
    -   Dados agregados ao nível de regiões de saúde (regiões
        intraestaduais de gestão de saúde).
-   `covid_estados`
    -   Dados agregados ao nível de estados.
-   `covid_regioes_brasil`
    -   Dados agregados ao nível de regiões políticas do Brasil.
-   `covid_brasil`
    -   Dados agregados ao nível do Brasil.

------------------------------------------------------------------------

## Análises Gráficas

Com os dados atualizados e organizados, podemos fazer as primeiras
observações:

### Contágios

#### Novos Contágios por Estado/Região

<div class="figure" style="text-align: center">

<img src="docs/README-Volumes_de_Novos_Contagios_por_Estado-1.png" alt="Gráfico 01 - Volumes de Novos Contágios por Estado" width="100%" />
<p class="caption">
Gráfico 01 - Volumes de Novos Contágios por Estado
</p>

</div>

#### Olhando de perto os 5 estados com o maior número de contágios

<div class="figure" style="text-align: center">

<img src="docs/README-Maiores_Contagios_nos_Estados-1.gif" alt="Gráfico 02 - Animação dos 5 estados com maiores volumes de Contágio pela Pandemia" width="100%" />
<p class="caption">
Gráfico 02 - Animação dos 5 estados com maiores volumes de Contágio pela
Pandemia
</p>

</div>

#### Volumes Diários de Contágios Brasil

<div class="figure" style="text-align: center">

<img src="docs/README-Volumes_Diarios_de_Contagios-1.png" alt="Gráfico 03 - Volumes Diários de Novos Contágios no Brasil" width="100%" />
<p class="caption">
Gráfico 03 - Volumes Diários de Novos Contágios no Brasil
</p>

</div>

#### Volumes Diários de Contágios por Região/Estado (com ajuste de Média Móvel de 7 dias)

##### Sudeste

<div class="figure" style="text-align: center">

<img src="docs/README-Volumes_Estaduais_de_Contagios_Sudeste-1.png" alt="Gráfico 04 - Volumes Diários de Novos Contágios por Estado da Região Sudeste" width="100%" />
<p class="caption">
Gráfico 04 - Volumes Diários de Novos Contágios por Estado da Região
Sudeste
</p>

</div>

##### Nordeste

<div class="figure" style="text-align: center">

<img src="docs/README-Volumes_Estaduais_de_Contagios_Nordeste-1.png" alt="Gráfico 05 - Volumes Diários de Novos Contágios por Estado da Região Nordeste" width="100%" />
<p class="caption">
Gráfico 05 - Volumes Diários de Novos Contágios por Estado da Região
Nordeste
</p>

</div>

##### Sul

<div class="figure" style="text-align: center">

<img src="docs/README-Volumes_Estaduais_de_Contagios_Sul-1.png" alt="Gráfico 06 - Volumes Diários de Novos Contágios por Estado da Região Sul" width="100%" />
<p class="caption">
Gráfico 06 - Volumes Diários de Novos Contágios por Estado da Região Sul
</p>

</div>

##### Centro-Oeste

<div class="figure" style="text-align: center">

<img src="docs/README-Volumes_Estaduais_de_Contagios_Centro_Oeste-1.png" alt="Gráfico 07 - Volumes Diários de Novos Contágios por Estado da Região Centro-Oeste" width="100%" />
<p class="caption">
Gráfico 07 - Volumes Diários de Novos Contágios por Estado da Região
Centro-Oeste
</p>

</div>

##### Norte

<div class="figure" style="text-align: center">

<img src="docs/README-Volumes_Estaduais_de_Contagios_Norte-1.png" alt="Gráfico 08 - Volumes Diários de Novos Contágios por Estado da Região Norte" width="100%" />
<p class="caption">
Gráfico 08 - Volumes Diários de Novos Contágios por Estado da Região
Norte
</p>

</div>

### Óbitos

#### Novos Óbitos por Estado/Região

<div class="figure" style="text-align: center">

<img src="docs/README-Volumes_de_Novos_Obitos_por_Estado-1.png" alt="Gráfico 09 - Volumes de Novos Óbitos por Estado" width="100%" />
<p class="caption">
Gráfico 09 - Volumes de Novos Óbitos por Estado
</p>

</div>

#### Olhando de perto os 5 estados com o maior número de óbitos

<div class="figure" style="text-align: center">

<img src="docs/README-Maiores_Obitos_nos_Estados-1.gif" alt="Gráfico 10 - Animação dos 5 estados com maiores volumes de Óbitos pela Pandemia" width="100%" />
<p class="caption">
Gráfico 10 - Animação dos 5 estados com maiores volumes de Óbitos pela
Pandemia
</p>

</div>

#### Volumes Diários de Óbitos Brasil

<div class="figure" style="text-align: center">

<img src="docs/README-Volumes_Diarios_de_Obitos-1.png" alt="Gráfico 11 - Volumes Diários de Novos Óbitos no Brasil" width="100%" />
<p class="caption">
Gráfico 11 - Volumes Diários de Novos Óbitos no Brasil
</p>

</div>

#### Volumes Diários de Óbitos por Região/Estado (com ajuste de Média Móvel de 7 dias)

##### Sudeste

<div class="figure" style="text-align: center">

<img src="docs/README-Volumes_Estaduais_de_Obitos_Sudeste-1.png" alt="Gráfico 12 - Volumes Diários de Novos Óbitos por Estado da Região Sudeste" width="100%" />
<p class="caption">
Gráfico 12 - Volumes Diários de Novos Óbitos por Estado da Região
Sudeste
</p>

</div>

##### Nordeste

<div class="figure" style="text-align: center">

<img src="docs/README-Volumes_Estaduais_de_Obitos_Nordeste-1.png" alt="Gráfico 13 - Volumes Diários de Novos Óbitos por Estado da Região Nordeste" width="100%" />
<p class="caption">
Gráfico 13 - Volumes Diários de Novos Óbitos por Estado da Região
Nordeste
</p>

</div>

##### Sul

<div class="figure" style="text-align: center">

<img src="docs/README-Volumes_Estaduais_de_Obitos_Sul-1.png" alt="Gráfico 14 - Volumes Diários de Novos Óbitos por Estado da Região Sul" width="100%" />
<p class="caption">
Gráfico 14 - Volumes Diários de Novos Óbitos por Estado da Região Sul
</p>

</div>

##### Centro-Oeste

<div class="figure" style="text-align: center">

<img src="docs/README-Volumes_Estaduais_de_Obitos_Centro_Oeste-1.png" alt="Gráfico 15 - Volumes Diários de Novos Óbitos por Estado da Região Centro-Oeste" width="100%" />
<p class="caption">
Gráfico 15 - Volumes Diários de Novos Óbitos por Estado da Região
Centro-Oeste
</p>

</div>

##### Norte

<div class="figure" style="text-align: center">

<img src="docs/README-Volumes_Estaduais_de_Obitos_Norte-1.png" alt="Gráfico 16 - Volumes Diários de Novos Óbitos por Estado da Região Norte" width="100%" />
<p class="caption">
Gráfico 16 - Volumes Diários de Novos Óbitos por Estado da Região Norte
</p>

</div>

Algumas coisas também nos levam a outra questão por conta da
possibilidade de transmissão assintomática, o tempo entre a detecção do
contágio e a ocorrência do óbito.

### Correlação entre Contágios e Óbitos

<div class="figure" style="text-align: center">

<img src="docs/README-Correlacao_Cruzada_entre_Contagios_e_Obitos-1.png" alt="Gráfico 17 - Correlação Cruzada entre Contágios e Óbitos" width="100%" />
<p class="caption">
Gráfico 17 - Correlação Cruzada entre Contágios e Óbitos
</p>

</div>

Podemos ver, até da maneira como (não) são feitas as notificações no
Brasil, que as maiores correlações são múltiplos de semanas inteiras…

### Taxa de Mortalidade

Uma vez que temos as quantidades de contágios e de óbitos, podemos
calcular a Taxa de Mortalidade (Número de Mortos / Número de Contágios)

É sabido que em países que conseguem adotar a testagem em massa, numa
média de toda a população, a taxa de mortalidade fica **em torno de
1%**.

#### Mortalidade no Mapa

<div class="figure" style="text-align: center">

<img src="docs/README-Taxas_de_Mortalidade_no_Brasil-1.png" alt="Gráfico 18 - Taxas de Mortalidade por Estado" width="100%" />
<p class="caption">
Gráfico 18 - Taxas de Mortalidade por Estado
</p>

</div>

#### Animação da Evolução Histórica das Taxas de Mortalidade no Brasil

<div class="figure" style="text-align: center">

<img src="docs/README-Taxa_de_Mortalidade_no_Brasil-1.gif" alt="Gráfico 19 - Animação da Evolução da Taxa de Mortalidade no Brasil" width="100%" />
<p class="caption">
Gráfico 19 - Animação da Evolução da Taxa de Mortalidade no Brasil
</p>

</div>

#### Evolução Histórica das Taxas de Mortalidade no Brasil

##### Sudeste

<div class="figure" style="text-align: center">

<img src="docs/README-Taxas_de_Mortalidade_por_Estados_do_Sudeste-1.png" alt="Gráfico 20 - Evolução das Taxas de Mortalidade por Estado na Região Sudeste" width="100%" />
<p class="caption">
Gráfico 20 - Evolução das Taxas de Mortalidade por Estado na Região
Sudeste
</p>

</div>

##### Nordeste

<div class="figure" style="text-align: center">

<img src="docs/README-Taxas_de_Mortalidade_por_Estado_do_Nordeste-1.png" alt="Gráfico 21 - Evolução das Taxas de Mortalidade por Estado na Região Nordeste" width="100%" />
<p class="caption">
Gráfico 21 - Evolução das Taxas de Mortalidade por Estado na Região
Nordeste
</p>

</div>

##### Sul

<div class="figure" style="text-align: center">

<img src="docs/README-Taxas_de_Mortalidade_por_Estado_do_Sul-1.png" alt="Gráfico 22 - Evolução das Taxas de Mortalidade por Estado na Região Sul" width="100%" />
<p class="caption">
Gráfico 22 - Evolução das Taxas de Mortalidade por Estado na Região Sul
</p>

</div>

##### Centro-Oeste

<div class="figure" style="text-align: center">

<img src="docs/README-Taxas_de_Mortalidade_por_Estado_do_Centro_Oeste-1.png" alt="Gráfico 23 - Evolução das Taxas de Mortalidade por Estado da Região Centro-Oeste" width="100%" />
<p class="caption">
Gráfico 23 - Evolução das Taxas de Mortalidade por Estado da Região
Centro-Oeste
</p>

</div>

##### Norte

<div class="figure" style="text-align: center">

<img src="docs/README-Taxas_de_Mortalidade_por_Estado_do_Norte-1.png" alt="Gráfico 24 - Evolução das Taxas de Mortalidade por Estado na Região Norte" width="100%" />
<p class="caption">
Gráfico 24 - Evolução das Taxas de Mortalidade por Estado na Região
Norte
</p>

</div>

### Contágios por 100 mil habitantes

Segue o contágio acumulado por grupo de 100 mil habitantes.

#### Sudeste

<div class="figure" style="text-align: center">

<img src="docs/README-Contagios_Acumulados_por_100_mil_hab_Sudeste-1.png" alt="Gráfico 25 - Contágios Acumulados por 100 mil habitantes nos Municípios da Região Sudeste" width="100%" />
<p class="caption">
Gráfico 25 - Contágios Acumulados por 100 mil habitantes nos Municípios
da Região Sudeste
</p>

</div>

#### Nordeste

<div class="figure" style="text-align: center">

<img src="docs/README-Contagios_Acumulados_por_100_mil_hab_Nordeste-1.png" alt="Gráfico 26 - Contágios Acumulados por 100 mil habitantes nos Municípios da Região Nordeste" width="100%" />
<p class="caption">
Gráfico 26 - Contágios Acumulados por 100 mil habitantes nos Municípios
da Região Nordeste
</p>

</div>

#### Sul

<div class="figure" style="text-align: center">

<img src="docs/README-Contagios_Acumulados_por_100_mil_hab_Sul-1.png" alt="Gráfico 27 - Contágios Acumulados por 100 mil habitantes nos Municípios da Região Sul" width="100%" />
<p class="caption">
Gráfico 27 - Contágios Acumulados por 100 mil habitantes nos Municípios
da Região Sul
</p>

</div>

#### Centro-Oeste

<div class="figure" style="text-align: center">

<img src="docs/README-Contagios_Acumulados_por_100_mil_hab_Centro_Oeste-1.png" alt="Gráfico 28 - Contágios Acumulados por 100 mil habitantes nos Municípios da Região Centro-Oeste" width="100%" />
<p class="caption">
Gráfico 28 - Contágios Acumulados por 100 mil habitantes nos Municípios
da Região Centro-Oeste
</p>

</div>

#### Norte

<div class="figure" style="text-align: center">

<img src="docs/README-Contagios_Acumulados_por_100_mil_hab_Norte-1.png" alt="Gráfico 29 - Contágios Acumulados por 100 mil habitantes nos Municípios da Região Norte" width="100%" />
<p class="caption">
Gráfico 29 - Contágios Acumulados por 100 mil habitantes nos Municípios
da Região Norte
</p>

</div>

------------------------------------------------------------------------

## O que falta fazer…

Com o processo construído para ser de fácil atualização e com os dados
estruturados, as possibilidades de análise são muitas…

Ainda gostaria de poder explorar algumas questões de séries temporais…
Com a sazonalidade visível entre novos contágios e novos óbitos, talvez
fosse possível *estimar* uma projeção de óbitos com base nos contágios.
Ainda que este processo seja muito pouco estocástico e dependa muito do
(praticamente imprevisível) comportamento humano…

Meu desejo é o de continuar incluindo novas análises e funcionalidades
neste pacote. Espero que tenha o tempo necessário para isso.

Atualização dos Dados no GitHub - 22/04/2021.
