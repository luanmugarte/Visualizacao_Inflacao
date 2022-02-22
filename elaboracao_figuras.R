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
          axis.text.y = element_text(size=12))

ipca_cheio_plot


p_ipca_cheio <- ggplotly(ipca_cheio_plot, tooltip = c("x", "y")) %>%
  layout(legend = list(orientation = "h", x = 0, y =-1)) %>%
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
  layout(legend = list(orientation = "h", x = 0, y = -1, title = '')) %>%
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
  layout(legend = list(orientation = "h", x = 0, y = -1, title = '')) %>%
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
  layout(legend = list(orientation = "h", x = 0, y = -0.5, title = '')) %>%
  config(modeBarButtons = list(list("resetScale2d"),list('toImage'), list('toggleSpikelines'),list('hoverCompareCartesian'),list('hoverClosestCartesian')), displaylogo = FALSE)
p_ipca_categorias_mensal

# Esse gráfico parece ser pouco informativo, decidi por não incluir

# Gráfico por RM e município ####

ipca_rm_mun_df <-ipca_rm_municipio %>%
  ungroup() %>%
  filter(categoria == 'Índice cheio') %>%
  # mutate(ano = case_when(ano == 2020 ~ as.numeric(2020),
  #                        ano == 2021 ~ as.numeric(2021))) %>%
  select(ano,nome,variacao_doze_meses)

ipca_rm_mun_plot <- ggplot(ipca_rm_mun_df, aes(x=nome, y = variacao_doze_meses, fill = as.factor(ano))) +
  geom_bar(stat="identity", position = position_dodge2()) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6), expand = c(0,0)) +
  scale_fill_brewer(palette="Set2") +
  theme_classic() +
    ylab('') +
    xlab('') +
    theme(  panel.grid = element_blank(),
            panel.border = element_blank(),
            legend.position="right",
            legend.title = element_blank(),
            legend.text = element_text(size=12),
            legend.key = element_rect(colour = "black"),
            legend.box.background = element_rect(colour = "black", size = 1),
            plot.title = element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
            axis.text.y = element_text(size=10,colour = 'black'))
  
ipca_rm_mun_plot

p_ipca_rm_mun <- ggplotly(ipca_rm_mun_plot, tooltip = c("x", "y")) %>%
  layout(legend = list(orientation = "h", x = 0, y = -1, title = '')) %>%
  config(modeBarButtons = list(list("resetScale2d"),list('toImage'), list('toggleSpikelines'),list('hoverCompareCartesian'),list('hoverClosestCartesian')), displaylogo = FALSE)
p_ipca_rm_mun

p_ipca_rm_mun <- p_ipca_rm_mun %>% layout(legend = list(x = 100, y = 0.5))

#---------------------------------------------------------------------#
#                                                                     #

####     TABELAS                                                   ####
#                                                                     #
#---------------------------------------e------------------------------#

# Tabela de categorias
ipca_categorias
tabela_categorias <- DT::datatable(ipca_categorias %>%
                ungroup %>%
                select(!c(id_categoria,variacao_acumulada)) %>%
                mutate(across(c(ano,mes), ~ factor(.,ordered = T))), 
              rownames = FALSE, 
              options = list(pageLength = 20, 
                             scrollX = F,
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
tabela_categorias
# Tabela de municípios e RM
ipca_rm_municipio
tabela_rm_municipio <- DT::datatable(ipca_rm_municipio %>%
                ungroup %>%
                mutate(across(c(ano), ~ factor(.,ordered = T))) %>%
                select(ano,nome,categoria, peso_mensal, variacao_doze_meses), 
              rownames = FALSE, 
              options = list(pageLength = 20, 
                             scrollX = F,
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
