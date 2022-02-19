setwd("/home/luanmugarte/base_de_dados")

# Carregando pacotes
#### Carregando pacotes ####
library(basedosdados)
library(xts) # Objeto xts
library(zoo)
library(cowplot)
library(scales)
library(RColorBrewer)
library(dplyr)
library(tibble)
library(ggplot2)
library(knitr)
library(stargazer)
library(broom)
library(RcppRoll)
library(plotly)
library(here)

#---------------------------------------------------------------------#
#                                                                     #
####                  EXTRAÇÃO DOS DADOS                           ####
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
df_ipca_categorias
unique(df_ipca_categorias$categoria)

# Cambio
cambio <- read.csv("data/cambio.csv", sep = ';', dec = ',', header = T)
cambio <- as.xts(ts((cambio[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))

# Commodities
fmi_comm <- read_excel("data/FMI_comm.xlsx", col_names = c('date','FMI Index','FMI Petróleo'))
fmi_comm

# Inflação por renda 
inflacao_dist <- read_excel("data/inflacao_por_renda.xlsx", .name_repair = ~c('Data','Muito Baixa','Baixa','MedBaixa','Media','MedAlta','Alta'))
inflacao_dist

# PIB mensal
pib_mensal <- read.csv("data/pib_mensal.csv", sep = ';')

#---------------------------------------------------------------------#
#                                                                     #
####                  MANIPULAÇÃO E LIMPEZA DOS DADOS              ####
#                                                                     #
#---------------------------------------------------------------------#

# Dados serão selecionados de julho de 2006 até o ano mais recente,
# em virtude da disponibilidade de dados sobre inflação por classe de
# renda do IPEA.

# Dados de inflação serão utilizados em variação acumulada em doze meses.


# Começar os dados em 1999/08 e terminar em 2021/10, se possível

# IPCA - Cheio ####
ipca_cheio <- read.csv("/home/luanmugarte/Tese/Dados/IPCA_cheio.csv", sep = ';', dec = ',', header = F)
ipca_cheio <- as.xts(ts((ipca_cheio[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))

# IPCA - Alimentos e Bebidas ####

IPCA_AlimBeb <- read.csv("/home/luanmugarte/Tese/Dados/alimbebs.csv", sep = ';', dec = ',', header = T)

IPCA_AlimBeb <- tibble(IPCA_AlimBeb) %>%
mutate(across(!Data, ~(as.numeric(.)/100)+1)) %>%
mutate(across(!Data, ~ round((RcppRoll::roll_prodr(., n = 12, na.rm = F, fill = NA)-1)*100,2)))
glimpse(IPCA_AlimBeb)

IPCA_AlimBeb <- as.xts(ts((IPCA_AlimBeb[103:nrow(IPCA_AlimBeb),2]), start = c(1999,8), end = c(2021,10), frequency = 12))
plot(IPCA_AlimBeb)

# IPCA - Habitação ####

IPCA_Habit <- read.csv("/home/luanmugarte/Tese/Dados/IPCA_Habit.csv", sep = ';', dec = ',', header = T)
IPCA_Habit

IPCA_Habit <- tibble(IPCA_Habit) %>%
mutate(across(!Data, ~(as.numeric(.)/100)+1)) %>%
mutate(across(!Data, ~ round((RcppRoll::roll_prodr(., n = 12, na.rm = F, fill = NA)-1)*100,2)))
glimpse(IPCA_Habit)

IPCA_Habit <- as.xts(ts((IPCA_Habit[103:nrow(IPCA_Habit),2]), start = c(1999,8), end = c(2021,10), frequency = 12))
plot(IPCA_Habit)

# Cambio ####
cambio <- read.csv("/home/luanmugarte/Tese/Dados/cambio.csv", sep = ';', dec = ',', header = T)
cambio <- as.xts(ts((cambio[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))

# Commodities ####
fmi_comm <- read_excel("/home/luanmugarte/Tese/Dados/FMI_comm.xlsx", col_names = c('date','FMI Index','FMI Petróleo'))
fmi_comm

# PIB ####

pib <- read.csv("/home/luanmugarte/Tese/Dados/PIB.csv")
pib
pib <- as.xts(ts((pib[11:nrow(pib),2]), start = c(1999,3), end = c(2021,2), frequency = 4))
plot(pib)

# Transportes
ipca_cheio <- read.csv("/home/luanmugarte/Tese/Dados/IPCA_cheio.csv", sep = ';', dec = ',', header = F)
ipca_cheio <- as.xts(ts((ipca_cheio[,2]), start = c(1999,8), end = c(2019,12), frequency = 12))
IPCA_transp <- read_excel("/home/luanmugarte/Tese/Dados/transportes_selecionados.xlsx", .name_repair = ~c('Data','Transporte Público','Veículos','Combustíveis'))
IPCA_transp <- IPCA_transp %>%
  mutate(across(!Data, ~(as.numeric(.)/100)+1)) %>%
  mutate(across(!Data, ~ round((RcppRoll::roll_prodr(., n = 12, na.rm = F, fill = 1.04)-1)*100,2))) %>%
  mutate(across(!Data, ~round(.,2) ))
date <- seq(as.Date("1999-08-1"), as.Date("2019-12-1"), by = "month")
df_acum <- data.frame(date,IPCA_transp[,c(2,3,4)],ipca_cheio)

# Inflação por renda ####
inflacao_dist <- read_excel("/home/luanmugarte/Tese/Dados/inflacao_por_renda.xlsx", .name_repair = ~c('Data','Muito_Baixa','Baixa','MedBaixa','Media','MedAlta','Alta'))
inflacao_dist

# IPCA - Alimentos e Bebidas ####

IPCA_AlimBeb <- read.csv("/home/luanmugarte/Tese/Dados/alimbebs.csv", sep = ';', dec = ',', header = T)
glimpse(IPCA_AlimBeb)

IPCA_AlimBeb <- tibble(IPCA_AlimBeb) %>%
  mutate(across(!Data, ~(./100)+1)) %>%
  mutate(across(!Data, ~round((RcppRoll::roll_prodr(., n = 12, na.rm = F, fill = NA)-1)*100,2)))

glimpse(IPCA_AlimBeb)

IPCA_AlimBeb <- as.xts(ts((IPCA_AlimBeb[186:nrow(IPCA_AlimBeb),2]), start = c(2006,7), end = c(2021,3), frequency = 12))
plot(IPCA_AlimBeb)

#######################################################################
#                                                                     #
#                     GRÁFICOS                                        #
#                                                                     #
#######################################################################

# Comparando com cambio, proporções e inflações ####
cambio <- read.csv("/home/luanmugarte/Tese/Dados/cambio.csv", sep = ';', dec = ',', header = T)
cambio <- diff(log(cambio[,2]))
cambio

cambio <- as.xts(ts((cambio[84:260]), start = c(2006,7), end = c(2021,3), frequency = 12))
length(cambio)

date <- seq(as.Date("2006-07-1"), as.Date("2021-3-1"), by = "month")

inflacao_dist
inflacao_dist_acum <- inflacao_dist %>%
  mutate(across(!Data, ~(as.numeric(.)/100)+1)) %>%
  mutate(across(!Data, ~ round((RcppRoll::roll_prodr(., n = 12, na.rm = F, fill = 1.04)-1)*100,2))) %>%
  mutate(prop_alta_mbaixa = Alta/`Muito Baixa`)
inflacao_dist_acum

# inflacao_dist_semacum <-inflacao_dist %>%
# mutate(across(!Data, ~round(.,2) ))

df_acum <- data.frame(date,inflacao_dist_acum[,c(2,5,7,8)])

df_acum <- reshape2::melt(df_acum, id.vars= 'date')

# df_semacum <- data.frame(date,inflacao_dist_semacum[,c(2,5,7)])
# 
# df_semacum <- reshape2::melt(df_semacum, id.vars= 'date')

df_cambio <- tibble(cambio, .name_repair = ~c('cambio')) %>%
mutate(cambio = as.numeric(cambio)) %>%
drop_na()

df_cambio <- data.frame(date,df_cambio)
df_cambio

rects_mensal <- df_cambio %>%
mutate(cambio_5 = case_when(cambio > 0.05 ~ as.numeric(date), cambio < 0.05 ~ 0)) %>%
mutate(cambio_1 = if_else(((cambio < 0.05) & (cambio > 0.0001)), as.numeric(date),0)) %>%
drop_na() %>%
mutate(xstart_5 = ifelse(cambio_5 > 0, as.Date(date),13389)) %>%
mutate(xend_5 = ifelse(cambio_5 > 0, as.Date(date+31),13389)) %>%
mutate(xstart_1 = ifelse(cambio_1 > 0, as.Date(date),13389)) %>%
mutate(xend_1 = ifelse(cambio_1 > 0, as.Date(date+31),13389)) %>%
mutate(col = ifelse(cambio_5 >0, 'Cambio_5',ifelse(cambio_1 >0, 'Cambio_1','')))
rects_mensal

rects_mensal$xstart_5 <- as.Date.numeric(rects_mensal$xstart_5)
rects_mensal$xend_5 <- as.Date.numeric(rects_mensal$xend_5)
rects_mensal$xstart_1 <- as.Date.numeric(rects_mensal$xstart_1)
rects_mensal$xend_1 <- as.Date.numeric(rects_mensal$xend_1)

# Plotando o gráfico acumulado ####
Infineq_cambio_plot <- ggplot(df_acum)  + 
geom_hline(yintercept = 1, colour= 'darkgrey') +
# geom_hline(yintercept = 0, colour= 'darkgrey') +
# geom_hline(yintercept = -1, colour= 'darkgrey') +
geom_rect(data = rects_mensal, aes(xmin = xstart_5, xmax = xend_5, ymin = min(df_acum$value)-0.5, ymax = max(df_acum$value)+0.5, fill = col), alpha = 0.4) +
geom_rect(data = rects_mensal, aes(xmin = xstart_1, xmax = xend_1, ymin = min(df_acum$value)-0.5, ymax = max(df_acum$value)+0.5, fill = col), alpha = 0.4) +
geom_line(aes(x=date, y=value, color=variable, linetype = variable),stat="identity", size = 1) +
scale_y_continuous(labels = function(x) paste0(x, ""),breaks = scales::pretty_breaks(n = 6), expand = c(0,0)) +
scale_x_date(date_breaks = '1 month', date_minor_breaks = '6 months', date_labels = "%m-%y",expand = c(0, 0.02)) +
labs(title = '') +
scale_fill_brewer(palette="Reds", name = '') +
scale_color_brewer(palette="Set1", name = '') +
guides(linetype = "none", fill = 'none') +
ylab('') +
xlab('') +
theme_classic() +
theme(  panel.grid = element_blank(), 
        panel.border = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        legend.key = element_rect(colour = "black"),
        legend.box.background = element_rect(colour = "black", size = 1),
        plot.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
        axis.text.y = element_text(size=10)) 

Infineq_cambio_plot



p <- ggplotly(Infineq_cambio_plot, tooltip = c("x", "y")) %>%
layout(legend = list(orientation = "h", x = 0, y =-0.1)) %>%
config(modeBarButtons = list(list("resetScale2d"),list('toImage'), list('toggleSpikelines'),list('hoverCompareCartesian'),list('hoverClosestCartesian')), displaylogo = FALSE)
p_ineq <- p
p_ineq
p_ineq_acum <- p
# For loop para remover legendas
for (i in 1:5) {
p$x$data[[i]]$showlegend <- F

}
