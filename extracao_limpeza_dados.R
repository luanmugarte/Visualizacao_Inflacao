setwd("/home/luanmugarte/base_de_dados/Visualizacao_Inflacao")

# Carregando pacotes
#### Carregando pacotes ####
library(flexdashboard)
library(shiny)
library(basedosdados)
library(xts) # Objeto xts
library(zoo)
library(cowplot)
library(scales)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(knitr)
library(stargazer)
library(broom)
library(RcppRoll)
library(plotly)
library(here)
library(stringr)
library(readxl)
library(purrr)
library(data.table)

#---------------------------------------------------------------------#
#                                                                     #
####      EXTRAÇÃO DOS DADOS                                       ####
#                                                                     #
#---------------------------------------------------------------------#


# Configurações do pacote basedosdados
set_billing_id("visualizacaobasedosdados")

# Índice cheio do IPCA
query <- bdplyr("br_ibge_ipca.mes_brasil")
df_ipca_cheio <- bd_collect(query) 

# Categorias do IPCA
query <- bdplyr("br_ibge_ipca.mes_categoria_brasil")
df_ipca_categorias <- bd_collect(query)

# Municípios e Regioes Metropolitanas
query <- bdplyr("br_ibge_ipca.mes_categoria_municipio")
df_ipca_municipio <- bd_collect(query)
glimpse(df_ipca_municipio)

query <- bdplyr("br_ibge_ipca.mes_categoria_rm")
df_ipca_rm <- bd_collect(query) 
glimpse(df_ipca_rm)

# Complementação do IPCA de categorias
ipca_categorias_complemento <- read_excel('data/IPCA_categorias.xlsx',col_names = T)

# Reajuste dos nomes das colunas para combinar com o id_categoria
colnames(ipca_categorias_complemento) <- c('data',gsub("[^0-9]", "", colnames(ipca_categorias_complemento)[2:81]))
ipca_categorias_complemento

#---------------------------------------------------------------------#
#                                                                     #
####      LIMPEZA E MANIPULAÇÃO DOS DADOS                          ####
#                                                                     #
#---------------------------------------------------------------------#

# Primeiro projeto: análise de dados da inflação via IPCA
# IPCA cheio, IPCA das categorias e IPCA por RM/Município

# Para o IPCA cheio, analisar variação mensal e acumulada em 12 meses

# Para categorias e geografia que vai de jan/2020 até agora
# Analisar pesos, variação mensal, variação em 12 meses e variação 
# acumulado até o dados mais recente

# IPCA cheio
ipca_cheio <- df_ipca_cheio %>%
  select(!c(indice,variacao_trimestral,variacao_semestral,variacao_anual)) %>%
  filter(ano >= 2000)
ipca_cheio  


# - - - - - - - - - - - - - - - #  

# IPCA de categorias
df_ipca_categorias 
  
ipca_categorias <- df_ipca_categorias %>%
  select(!c(id_categoria_bd,variacao_anual)) %>%
  mutate(id_categoria = as.numeric(id_categoria)) %>%
  # Filtrando somente até ítens do IPCA
  filter(id_categoria < 10000) %>%
  group_by(id_categoria) %>%
  mutate(variacao_acumulada = (cumprod(1+(variacao_mensal/100))-1)*100)
ipca_categorias

# Complementando com os dados de antes de 2020 para obter os dados da variação em 12 meses

# Criando um dataframe vazio e combinando ele com o complemento
empty_df <- data.frame(matrix(data = 0.5, 
                              nrow = nrow(ipca_categorias_complemento),
                              ncol = ncol(ipca_categorias_complemento)))

colnames(empty_df) <- colnames(ipca_categorias_complemento)
df_ipca_categorias_complemento <- rbind(ipca_categorias_complemento,
                                        empty_df)
df_ipca_categorias_complemento

# Criando um tibble que possui os valores falantes do IPCA acumulado em 12 meses
df_ipca_categorias_complemento <- df_ipca_categorias_complemento %>%
  mutate(across(!data, ~(as.numeric(.)/100)+1)) %>%
  mutate(across(!data, ~ round((RcppRoll::roll_prodr(., n = 12, na.rm = T, fill = 1.04)-1)*100,2))) %>%
  select(!data) %>%
  slice(13:(n()-1)) %>%
  mutate(ano = 2020) %>%
  mutate(mes = 1:11) %>%
  dplyr::select(ano,mes,everything())
df_ipca_categorias_complemento


ipca_categorias_melted1 <- reshape2::melt(df_ipca_categorias_complemento,
                                          id.vars=(c('ano','mes')))

colnames(ipca_categorias_melted1) <- c('ano','mes','id_categoria','value')

ipca_categorias_melted1 <- ipca_categorias_melted1   %>%
   mutate(id_categoria = as.factor(id_categoria)) %>%
   arrange(id_categoria)
  
# Pegando os valores acumulados em 12 meses do IPCA de dezembro/20 até 22
ipca_categorias_melted2 <- ipca_categorias %>%
  select(ano,mes,id_categoria,variacao_doze_meses) %>%
  # mutate(id_categoria = as.factor(id_categoria)) %>%
  # mutate(id_categoria = as.numeric(id_categoria)) %>%
  # arrange(id_categoria) %>%
  dplyr::rename(value = variacao_doze_meses) %>%
  drop_na()


# Há um número de categorias menor na IPCA a partir de 2020
length(unique(ipca_categorias_melted1$id_categoria))
length(unique(ipca_categorias_melted2$id_categoria))

# Identificando qual grupo
setdiff(unique(ipca_categorias_melted1$id_categoria),
        unique(ipca_categorias_melted2$id_categoria))

ipca_categorias_melted1 <- ipca_categorias_melted1 %>%
  filter(id_categoria != 7203)

# Conferindo se persiste a diferença
setdiff(unique(ipca_categorias_melted1$id_categoria),
        unique(ipca_categorias_melted2$id_categoria))

# Combinando dados
ipca_categorias_melted <- rbind(ipca_categorias_melted1,
                                ipca_categorias_melted2)

ipca_categorias_melted <- ipca_categorias_melted %>%
  mutate(id_categoria = as.character.numeric_version(id_categoria)) %>%
  mutate(id_categoria = as.numeric(id_categoria)) %>%
  arrange(id_categoria) %>%
  rename(variacao_doze_meses = value)

ipca_categorias <- ipca_categorias %>%
  arrange(id_categoria)

# Criando nova coluna no tibble original
ipca_categorias$variacao_doze_meses <- ipca_categorias_melted$variacao_doze_meses

# Tibble final com todos os valores
ipca_categorias <- ipca_categorias %>%
  mutate(variacao_acumulada = round(variacao_acumulada, 2)) %>%
  mutate(variacao_doze_meses = round(variacao_doze_meses, 2)) %>%
  mutate(variacao_mensal = round(variacao_mensal, 2)) %>%
  mutate(peso_mensal = round(peso_mensal, 2))
  
ipca_categorias  

# - - - - - - - - - - - - - - - #

# Ajustando dados do IPCA por RM/município
  
## Ids dos municípios
query <- "SELECT
id_municipio, 
nome AS nome_municipio,
FROM `basedosdados.br_bd_diretorios_brasil.municipio`"

df_ids <- read_sql(query)

# Tibble com correspondencia de municipios e id
nomes_municipios <- df_ids %>%
  filter(id_municipio %in% unique(df_ipca_municipio$id_municipio)) %>%
  select(id_municipio,nome_municipio)
nomes_municipios

ipca_municipio <- df_ipca_municipio %>%
  filter((as.numeric(id_categoria) < 10) & mes == 12 & (ano == 2020 | ano == 2021) ) %>%
  left_join(nomes_municipios, by = c("id_municipio" = "id_municipio")) %>%
  select(ano,mes,peso_mensal,nome_municipio,categoria,variacao_doze_meses) %>%
  group_by(nome_municipio,ano) %>%
  rename(nome = nome_municipio)

ipca_municipio

## Ids das regioes metropolitanas
nomes_rm_df <- read_excel('data/Composicao_RMs_RIDEs_AglomUrbanas_2020_06_30.xlsx',col_names = T)
nomes_rm_df

unique(df_ipca_rm$id_regiao_metropolitana)
nomes_rm <- nomes_rm_df %>%
  filter(COD %in% unique(df_ipca_rm$id_regiao_metropolitana)) %>%
  rename(id_rm = COD, nome_rm = NOME) %>%
  distinct(id_rm,nome_rm)
nomes_rm

df_ipca_rm
ipca_rm <- df_ipca_rm %>%
  filter((as.numeric(id_categoria) < 10) & mes == 12 & (ano == 2020 | ano == 2021) ) %>%
  rename(id_rm = id_regiao_metropolitana) %>%
  mutate(id_rm = as.numeric(id_rm)) %>%
  left_join(nomes_rm, by = c("id_rm" = "id_rm")) %>%
  select(ano,mes,nome_rm,peso_mensal, variacao_doze_meses,categoria) %>%
  group_by(nome_rm,ano)  %>%
  rename(nome = nome_rm)

ipca_rm_municipio <- bind_rows(ipca_rm,ipca_municipio)
ipca_rm_municipio 


ipca_rm_mun_complemento <- read_excel('data/ipca_cheio_municipio_rm.xlsx',
                                      .name_repair = ~c('nome','ano','indice_cheio'))

ipca_rm_mun_complemento <- ipca_rm_mun_complemento %>%
  mutate(categoria = 'Índice cheio') %>%
  mutate(variacao_doze_meses = indice_cheio) %>%
  select(!indice_cheio)

ipca_rm_municipio <- ipca_rm_municipio %>%
  select(!mes) %>%
  full_join(ipca_rm_mun_complemento, by = c("nome","ano","categoria","variacao_doze_meses")) %>%
  arrange(nome)

ipca_rm_municipio
