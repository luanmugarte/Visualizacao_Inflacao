setwd("/home/luanmugarte/base_de_dados/Visualizacao_Inflacao")

# Carregando pacotes
#### Carregando pacotes ####
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

unique(df_ipca_categorias$categoria)

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


#---------------------------------------------------------------------#
#                                                                     #
####      LIMPEZA E MANIPULAÇÃO DOS DADOS                          ####
#                                                                     #
#---------------------------------------------------------------------#

# Primeiro projeto: análise de dados da inflação via IPCA

# Para o IPCA cheio, analisar variação mensal e acumulada em 12 meses

# Para categorias e geografia que vai de jan/2020 até agora
# Analisar pesos, variação mensal, variação em 12 meses e variação 
# acumulado até o dados mais recente

# IPCA cheio
ipca_cheio <- df_ipca_cheio %>%
  select(!c(indice,variacao_trimestral,variacao_semestral,variacao_anual)) %>%
  filter(ano >= 2000)
ipca_cheio  

# IPCA de categorias
ipca_categorias <- df_ipca_categorias %>%
  select(!c(id_categoria_bd,variacao_anual)) %>%
  mutate(id_categoria = as.numeric(id_categoria)) %>%
  # Filtrando somente até ítens do IPCA
  filter(id_categoria < 10000) %>%
  group_by(id_categoria) %>%
  mutate(variacao_acumulada = (cumprod(1+(variacao_mensal/100))-1)*100,2) %>%
  arrange(id_categoria)
ipca_categorias

# Complementando com os dados de antes de 2020 para obter os dados da variação em 12 meses

# Criando um dataframe vazio e combinando ele com o complemento
empty_df <- data.frame(matrix(data = 0.5, nrow = nrow(ipca_categorias_complemento), ncol = ncol(ipca_categorias_complemento)))
colnames(empty_df) <- colnames(ipca_categorias_complemento)
df_ipca_categorias_complemento <-rbind(ipca_categorias_complemento,empty_df)

# Criando um tibble que possui os valores falantes do IPCA acumulado em 12 meses
ipca_categorias_complemento <- df_ipca_categorias_complemento %>%
  mutate(across(!data, ~(as.numeric(.)/100)+1)) %>%
  mutate(across(!data, ~ round((RcppRoll::roll_prodr(., n = 12, na.rm = T, fill = 1.04)-1)*100,2))) %>%
  select(!data) %>%
  slice(13:(n()-1)) %>%
  mutate(ano = 2020) %>%
  mutate(mes = 1:11) %>%
  dplyr::select(ano,mes,everything())

setDT(ipca_categorias_complemento)
ipca_categorias_melted <- melt(ipca_categorias_complemento,id.vars=(c('ano','mes')))

# 
# colnames(ipca_categorias_melted) <- c('ano','mes','id_categoria','value')
# ipca_categorias_melted
# 
# setDT(ipca_categorias)
# 
# ipca_categorias[.(variacao_doze_meses = ipca_categorias[]
# 
# ipca_categorias[.(variacao_doze_meses = ipca_categorias_melted[value %in% id_categoria, unique(id_categoria)]), on = 'id_categoria']

# df_ipca_categorias %>%
#   mutate(id_categoria = as.numeric(id_categoria)) %>%
#   filter(id_categoria < 10000) %>%
#   distinct(id_categoria) %>%
#   count()
# 
# unique(ipca_grupos$categoria)
# 
# unique(df_ipca_categorias$id_categoria)
# 
# ipca_categorias  <- df_ipca_categorias %>%
#   select(!c(id_categoria_bd,variacao_anual)) %>%
#   order_by(as.numeric(id_categoria))
# 
# data("ToothGrowth")
# ToothGrowth
# ToothGrowth %>% 
#   group_by(supp, dose) %>%
#   mutate(lenmean = mean(len),
#          submean2 = len - lenmean/dose)
# 
# df_ipca_categorias$id_categoria


  # mutate(variacao_acumulada = case_when( ano = 2020 & mes = 1 ~ variacao_mensal,
  #                                        TRUE ~ dplyr::lag(categoria)
  #                                        )
  #        )


ipca_categorias %>% 
  filter(categoria == 'Despesas pessoais')
  
## Ids dos municípios
query <- "SELECT
id_municipio, 
nome AS nome_municipio,
id_regiao_imediata,
nome_regiao_imediata,
id_mesorregiao,
nome_mesorregiao,
id_microrregiao,
nome_microrregiao
FROM `basedosdados.br_bd_diretorios_brasil.municipio`"

df_ids <- read_sql(query)


nomes_municipios <- df_ids %>%
  filter(id_municipio %in% unique(df_ipca_municipio$id_municipio)) %>%
  distinct(nome_municipio)
nomes_municipios

## Ids das regioes metropolitanas
nomes_rm_df <- read_excel('data/Composicao_RMs_RIDEs_AglomUrbanas_2020_06_30.xlsx',col_names = T)

nomes_rm <- nomes_rm_df %>%
  filter(COD %in% unique(df_ipca_rm$id_regiao_metropolitana)) %>%
  distinct(NOME)
nomes_rm  




# Segundo projeto: desigualdade de inflação no Brasil ####
# Dados serão selecionados de julho de 2006 até o ano mais recente,
# em virtude da disponibilidade de dados sobre inflação por classe de
# renda do IPEA.

# Dados de inflação serão utilizados em variação acumulada em doze meses.


# Começar os dados em 1999/08 e terminar em 2021/10, se possível

# # IPCA - Cheio 
# ipca_cheio <- read.csv("/home/luanmugarte/Tese/Dados/IPCA_cheio.csv", sep = ';', dec = ',', header = F)
# ipca_cheio <- as.xts(ts((ipca_cheio[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))

# IPCA - Alimentos e Bebidas 

# IPCA_AlimBeb <- read.csv("/home/luanmugarte/Tese/Dados/alimbebs.csv", sep = ';', dec = ',', header = T)
# 
# IPCA_AlimBeb <- tibble(IPCA_AlimBeb) %>%
# mutate(across(!Data, ~(as.numeric(.)/100)+1)) %>%
# mutate(across(!Data, ~ round((RcppRoll::roll_prodr(., n = 12, na.rm = F, fill = NA)-1)*100,2)))
# glimpse(IPCA_AlimBeb)
# 
# IPCA_AlimBeb <- as.xts(ts((IPCA_AlimBeb[103:nrow(IPCA_AlimBeb),2]), start = c(1999,8), end = c(2021,10), frequency = 12))
# plot(IPCA_AlimBeb)
# 
# # IPCA - Habitação 
# 
# IPCA_Habit <- read.csv("/home/luanmugarte/Tese/Dados/IPCA_Habit.csv", sep = ';', dec = ',', header = T)
# IPCA_Habit
# 
# IPCA_Habit <- tibble(IPCA_Habit) %>%
# mutate(across(!Data, ~(as.numeric(.)/100)+1)) %>%
# mutate(across(!Data, ~ round((RcppRoll::roll_prodr(., n = 12, na.rm = F, fill = NA)-1)*100,2)))
# glimpse(IPCA_Habit)
# 
# IPCA_Habit <- as.xts(ts((IPCA_Habit[103:nrow(IPCA_Habit),2]), start = c(1999,8), end = c(2021,10), frequency = 12))
# plot(IPCA_Habit)
# 
# # Cambio 
# cambio <- read.csv("/home/luanmugarte/Tese/Dados/cambio.csv", sep = ';', dec = ',', header = T)
# cambio <- as.xts(ts((cambio[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))
# 
# # Commodities 
# fmi_comm <- read_excel("/home/luanmugarte/Tese/Dados/FMI_comm.xlsx", col_names = c('date','FMI Index','FMI Petróleo'))
# fmi_comm
# 
# # PIB 
# 
# pib <- read.csv("/home/luanmugarte/Tese/Dados/PIB.csv")
# pib
# pib <- as.xts(ts((pib[11:nrow(pib),2]), start = c(1999,3), end = c(2021,2), frequency = 4))
# plot(pib)
# 
# # Transportes
# ipca_cheio <- read.csv("/home/luanmugarte/Tese/Dados/IPCA_cheio.csv", sep = ';', dec = ',', header = F)
# ipca_cheio <- as.xts(ts((ipca_cheio[,2]), start = c(1999,8), end = c(2019,12), frequency = 12))
# IPCA_transp <- read_excel("/home/luanmugarte/Tese/Dados/transportes_selecionados.xlsx", .name_repair = ~c('Data','Transporte Público','Veículos','Combustíveis'))
# IPCA_transp <- IPCA_transp %>%
#   mutate(across(!Data, ~(as.numeric(.)/100)+1)) %>%
#   mutate(across(!Data, ~ round((RcppRoll::roll_prodr(., n = 12, na.rm = F, fill = 1.04)-1)*100,2))) %>%
#   mutate(across(!Data, ~round(.,2) ))
# date <- seq(as.Date("1999-08-1"), as.Date("2019-12-1"), by = "month")
# df_acum <- data.frame(date,IPCA_transp[,c(2,3,4)],ipca_cheio)
# 
# Inflação por renda
inflacao_dist <- read_excel("/home/luanmugarte/Tese/Dados/inflacao_por_renda.xlsx", .name_repair = ~c('Data','Muito_Baixa','Baixa','MedBaixa','Media','MedAlta','Alta'))
inflacao_dist

# IPCA - Alimentos e Bebidas 
# 
# IPCA_AlimBeb <- read.csv("/home/luanmugarte/Tese/Dados/alimbebs.csv", sep = ';', dec = ',', header = T)
# glimpse(IPCA_AlimBeb)
# 
# IPCA_AlimBeb <- tibble(IPCA_AlimBeb) %>%
#   mutate(across(!Data, ~(./100)+1)) %>%
#   mutate(across(!Data, ~round((RcppRoll::roll_prodr(., n = 12, na.rm = F, fill = NA)-1)*100,2)))
# 
# glimpse(IPCA_AlimBeb)
# 
# IPCA_AlimBeb <- as.xts(ts((IPCA_AlimBeb[186:nrow(IPCA_AlimBeb),2]), start = c(2006,7), end = c(2021,3), frequency = 12))
# plot(IPCA_AlimBeb)

