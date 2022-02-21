#---------------------------------------------------------------------#
#                                                                     #
####      GRÁFICOS                                                 ####
#                                                                     #
#---------------------------------------------------------------------#

# Gráfico do IPCA cheio ####
ipca_cheio_df <- ipca_cheio %>%
  mutate(date = as.Date.character(paste0(ano,"-",mes,'-1'))) %>%
  select(date,variacao_doze_meses)

ipca_cheio_df <- reshape2::melt(ipca_cheio_df, id.vars = 'date')
ipca_cheio_df

ipca_cheio_plot <- ggplot(ipca_cheio_df)  +
  # geom_hline(yintercept = 0, colour= 'darkgrey') +
  geom_rect(data = ipca_cheio_df, aes(xmin = as.Date('2020-02-01'),
                                     xmax = as.Date('2022-01-01'),
                                     # Usar valores invés Inf evita problemas com o plotly
                                     ymin = min(value)-0.5,
                                     ymax = max(value)+0.5,
                                     fill = 'Pandemia de Covid-19'), alpha = 0.4) +
  geom_line(aes(x=date, y=value, color=variable, linetype = variable),stat="identity", size = 1) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     breaks = scales::pretty_breaks(n = 6),
                     expand = c(0,0)) +
  scale_x_date(date_breaks = '12 months', 
               date_minor_breaks = '6 months',
               date_labels = "%Y",expand = c(0, 0)) +
  labs(title = '') +
  scale_fill_brewer(palette= 'Pastel2', name = '') +
  scale_color_brewer(palette="Set1", name = '') +
  guides(color = "none", linetype = 'none') +
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
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.8,size=11, colour = 'black'),
          axis.text.y = element_text(size=14))

ipca_cheio_plot


p_ipca_cheio <- ggplotly(ipca_cheio_plot, tooltip = c("x", "y")) %>%
  layout(legend = list(orientation = "h", x = 0, y =-0.1)) %>%
  config(modeBarButtons = list(list("resetScale2d"),list('toImage'), list('toggleSpikelines'),list('hoverCompareCartesian'),list('hoverClosestCartesian')), displaylogo = FALSE)
p_ipca_cheio

# For loop para remover legendas
p_ipca_cheio$x$data[[2]]$showlegend <- F
p_ipca_cheio$x$data[[1]]$name <-  'Pandemia de Covid-19'
p_ipca_cheio

# Gráfico dos grupos do IPCA ####

# Variação acumulada até 2022
ipca_categorias_acum_df <- ipca_categorias %>%
  ungroup %>%
  mutate(date = as.Date.character(paste0(ano,"-",mes,'-1'))) %>%
  filter(id_categoria < 10) %>%
  select(date,variacao_doze_meses,categoria)

ipca_categorias_acum_df


ipca_categorias_acum_plot <- ggplot(ipca_categorias_acum_df)  +
  geom_hline(yintercept = 0, colour= 'darkgrey') +
  geom_line(aes(x=date, y=variacao_doze_meses, color=categoria, linetype = categoria),stat="identity", size = 1) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     breaks = scales::pretty_breaks(n = 6),
                     expand = c(0,0)) +
  scale_x_date(date_breaks = '2 months', 
               date_minor_breaks = '6 months',
               date_labels = "%b-%Y",expand = c(0, 0)) +
  labs(title = '') +
  # scale_fill_brewer(palette= 'Pastel2', name = '') +
  # scale_color_brewer(palette="Set1", name = '') +
  guides(title = NULL) +
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
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.8,size=11, colour = 'black'),
          axis.text.y = element_text(size=14))

ipca_categorias_acum_plot


p_ipca_categorias_acum <- ggplotly(ipca_categorias_acum_plot, tooltip = c("x", "y")) %>%
  layout(legend = list(orientation = "h", x = 0, y = -0.3, title = '')) %>%
  config(modeBarButtons = list(list("resetScale2d"),list('toImage'), list('toggleSpikelines'),list('hoverCompareCartesian'),list('hoverClosestCartesian')), displaylogo = FALSE)
p_ipca_categorias_acum

# Variacao em 12 meses

ipca_categorias_doze_df <- ipca_categorias %>%
  ungroup %>%
  mutate(date = as.Date.character(paste0(ano,"-",mes,'-1'))) %>%
  filter(id_categoria < 10) %>%
  select(date,variacao_acumulada,categoria)

ipca_categorias_doze_df


ipca_categorias_doze_plot <- ggplot(ipca_categorias_doze_df)  +
  geom_hline(yintercept = 0, colour= 'darkgrey') +
  geom_line(aes(x=date, y=variacao_acumulada, color=categoria, linetype = categoria),stat="identity", size = 1) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     breaks = scales::pretty_breaks(n = 6),
                     expand = c(0,0)) +
  scale_x_date(date_breaks = '2 months', 
               date_minor_breaks = '6 months',
               date_labels = "%b-%Y",expand = c(0, 0)) +
  labs(title = '') +
  # scale_fill_brewer(palette= 'Pastel2', name = '') +
  # scale_color_brewer(palette="Set1", name = '') +
  guides(title = NULL) +
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
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.8,size=11, colour = 'black'),
          axis.text.y = element_text(size=14))

ipca_categorias_doze_plot


p_ipca_categorias_doze <- ggplotly(ipca_categorias_doze_plot, tooltip = c("x", "y")) %>%
  layout(legend = list(orientation = "h", x = 0, y = -0.3, title = '')) %>%
  config(modeBarButtons = list(list("resetScale2d"),list('toImage'), list('toggleSpikelines'),list('hoverCompareCartesian'),list('hoverClosestCartesian')), displaylogo = FALSE)
p_ipca_categorias_doze


# Variacao em 12 meses

ipca_categorias_mensal_df <- ipca_categorias %>%
  ungroup %>%
  mutate(date = as.Date.character(paste0(ano,"-",mes,'-1'))) %>%
  filter(id_categoria < 10) %>%
  select(date,variacao_mensal,categoria)

ipca_categorias_mensal_df


ipca_categorias_mensal_plot <- ggplot(ipca_categorias_mensal_df)  +
  geom_hline(yintercept = 0, colour= 'darkgrey') +
  geom_line(aes(x=date, y=variacao_mensal, color=categoria, linetype = categoria),stat="identity", size = 1) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     breaks = scales::pretty_breaks(n = 6),
                     expand = c(0,0)) +
  scale_x_date(date_breaks = '2 months', 
               date_minor_breaks = '6 months',
               date_labels = "%b-%Y",expand = c(0, 0)) +
  labs(title = '') +
  # scale_fill_brewer(palette= 'Pastel2', name = '') +
  # scale_color_brewer(palette="Set1", name = '') +
  guides(title = NULL) +
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
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.8,size=11, colour = 'black'),
          axis.text.y = element_text(size=14))

ipca_categorias_mensal_plot

p_ipca_categorias_mensal <- ggplotly(ipca_categorias_mensal_plot, tooltip = c("x", "y")) %>%
  layout(legend = list(orientation = "h", x = 0, y = -0.3, title = '')) %>%
  config(modeBarButtons = list(list("resetScale2d"),list('toImage'), list('toggleSpikelines'),list('hoverCompareCartesian'),list('hoverClosestCartesian')), displaylogo = FALSE)
p_ipca_categorias_mensal

# Esse gráfico parece ser pouco informativo, decidi por não incluir

# Mapa do Brasil ####

library(plotly)
library(rjson)

data <- fromJSON(file="https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json")
data$features[[1]]



#---------------------------------------------------------------------#
#                                                                     #

####     TABELAS                                                   ####
#                                                                     #
#---------------------------------------------------------------------#

# Tabela de categorias

tabela_categorias <- DT::datatable(df_ipca_categorias %>%
                ungroup %>%
                select(!c(variacao_acumulada,id_categoria)) %>%
                mutate(across(c(ano,mes), ~ factor(.,ordered = T))), 
              rownames = FALSE, 
              options = list(pageLength = 20, 
                             scrollX = TRUE,
                             searchHighlight = TRUE,
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
              ), 
              class =  c('compact', 'white-space: nowrap', 'stripe'),
              filter = 'top',
              colnames = c('Ano', 
                           'Mês',
                           'Descrição do Grupo/Subgrupo',
                           'Peso Mensal',
                           'Variação Mensal (%)',
                           'Variação acumulada em 12 meses (%)')
)

# Tabela de municípios e RM
ipca_rm_municipio
tabela_rm_municipio <- DT::datatable(ipca_rm_municipio %>%
                ungroup %>%
                mutate(across(c(ano), ~ factor(.,ordered = T))) %>%
                select(ano,nome,categoria, peso_mensal, variacao_doze_meses), 
              rownames = FALSE, 
              options = list(pageLength = 20, 
                             scrollX = TRUE,
                             searchHighlight = TRUE,
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
              ), 
              class =  c('compact', 'white-space: nowrap', 'stripe'),
              filter = 'top',
              colnames = c('Ano', 
                           'Município ou Região Metropolitana',
                           'Categoria ou índice cheio',
                           'Peso Mensal',
                           'Variação acumulada em 12 meses (%)')
)

tabela_rm_municipio
# rects_mensal <- df_cambio %>%
  #   mutate(cambio_5 = case_when(cambio > 0.05 ~ as.numeric(date), cambio < 0.05 ~ 0)) %>%
  #   mutate(cambio_1 = if_else(((cambio < 0.05) & (cambio > 0.0001)), as.numeric(date),0)) %>%
  #   drop_na() %>%
  #   mutate(xstart_5 = ifelse(cambio_5 > 0, as.Date(date),13389)) %>%
  #   mutate(xend_5 = ifelse(cambio_5 > 0, as.Date(date+31),13389)) %>%
  #   mutate(xstart_1 = ifelse(cambio_1 > 0, as.Date(date),13389)) %>%
  #   mutate(xend_1 = ifelse(cambio_1 > 0, as.Date(date+31),13389)) %>%
  #   mutate(col = ifelse(cambio_5 >0, 'Cambio_5',ifelse(cambio_1 >0, 'Cambio_1','')))
# cambio <- read.csv("/home/luanmugarte/Tese/Dados/cambio.csv", sep = ';', dec = ',', header = T)
# cambio <- diff(log(cambio[,2]))
# cambio
# 
# cambio <- as.xts(ts((cambio[84:260]), start = c(2006,7), end = c(2021,3), frequency = 12))
# length(cambio)
# 
# date <- seq(as.Date("2006-07-1"), as.Date("2021-3-1"), by = "month")
# 
# inflacao_dist
# inflacao_dist_acum_t1 <- inflacao_dist %>%
#   mutate(across(!Data, ~(as.numeric(.)/100)+1)) %>%
#   mutate(across(!Data, ~ round((RcppRoll::roll_prodr(., n = 12, na.rm = F, fill = 2)-1)*100,2))) %>%
#   mutate(prop_alta_mbaixa = Alta/`Muito_Baixa`)
# inflacao_dist_acum
# 
# # inflacao_dist_semacum <-inflacao_dist %>%
# # mutate(across(!Data, ~round(.,2) ))
# 
# df_acum <- data.frame(date,inflacao_dist_acum[,c(2,5,7,8)])
# 
# df_acum <- reshape2::melt(df_acum, id.vars= 'date')
# 
# # df_semacum <- data.frame(date,inflacao_dist_semacum[,c(2,5,7)])
# # 
# # df_semacum <- reshape2::melt(df_semacum, id.vars= 'date')
# 
# df_cambio <- tibble(cambio, .name_repair = ~c('cambio')) %>%
#   mutate(cambio = as.numeric(cambio)) %>%
#   drop_na()
# 
# df_cambio <- data.frame(date,df_cambio)
# df_cambio
# 
# rects_mensal <- df_cambio %>%
#   mutate(cambio_5 = case_when(cambio > 0.05 ~ as.numeric(date), cambio < 0.05 ~ 0)) %>%
#   mutate(cambio_1 = if_else(((cambio < 0.05) & (cambio > 0.0001)), as.numeric(date),0)) %>%
#   drop_na() %>%
#   mutate(xstart_5 = ifelse(cambio_5 > 0, as.Date(date),13389)) %>%
#   mutate(xend_5 = ifelse(cambio_5 > 0, as.Date(date+31),13389)) %>%
#   mutate(xstart_1 = ifelse(cambio_1 > 0, as.Date(date),13389)) %>%
#   mutate(xend_1 = ifelse(cambio_1 > 0, as.Date(date+31),13389)) %>%
#   mutate(col = ifelse(cambio_5 >0, 'Cambio_5',ifelse(cambio_1 >0, 'Cambio_1','')))
# rects_mensal
# 
# rects_mensal$xstart_5 <- as.Date.numeric(rects_mensal$xstart_5)
# rects_mensal$xend_5 <- as.Date.numeric(rects_mensal$xend_5)
# rects_mensal$xstart_1 <- as.Date.numeric(rects_mensal$xstart_1)
# rects_mensal$xend_1 <- as.Date.numeric(rects_mensal$xend_1)
# 
# # Plotando o gráfico acumulado ####
# Infineq_cambio_plot <- ggplot(df_acum)  + 
#   geom_hline(yintercept = 1, colour= 'darkgrey') +
#   # geom_hline(yintercept = 0, colour= 'darkgrey') +
#   # geom_hline(yintercept = -1, colour= 'darkgrey') +
#   geom_rect(data = rects_mensal, aes(xmin = xstart_5, xmax = xend_5, ymin = min(df_acum$value)-0.5, ymax = max(df_acum$value)+0.5, fill = col), alpha = 0.4) +
#   geom_rect(data = rects_mensal, aes(xmin = xstart_1, xmax = xend_1, ymin = min(df_acum$value)-0.5, ymax = max(df_acum$value)+0.5, fill = col), alpha = 0.4) +
#   geom_line(aes(x=date, y=value, color=variable, linetype = variable),stat="identity", size = 1) +
#   scale_y_continuous(labels = function(x) paste0(x, ""),breaks = scales::pretty_breaks(n = 6), expand = c(0,0)) +
#   scale_x_date(date_breaks = '1 month', date_minor_breaks = '6 months', date_labels = "%m-%y",expand = c(0, 0.02)) +
#   labs(title = '') +
#   scale_fill_brewer(palette="Reds", name = '') +
#   scale_color_brewer(palette="Set1", name = '') +
#   guides(linetype = "none", fill = 'none') +
#   ylab('') +
#   xlab('') +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="bottom",
#           legend.title = element_blank(),
#           legend.text = element_text(size=12),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           plot.title = element_blank(),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
#           axis.text.y = element_text(size=10)) 
# 
# Infineq_cambio_plot
# 
# 
# 
# p <- ggplotly(Infineq_cambio_plot, tooltip = c("x", "y")) %>%
#   layout(legend = list(orientation = "h", x = 0, y =-0.1)) %>%
#   config(modeBarButtons = list(list("resetScale2d"),list('toImage'), list('toggleSpikelines'),list('hoverCompareCartesian'),list('hoverClosestCartesian')), displaylogo = FALSE)
# p_ineq <- p
# p_ineq
# p_ineq_acum <- p
# # For loop para remover legendas
# for (i in 1:5) {
#   p$x$data[[i]]$showlegend <- F
#   
# }
