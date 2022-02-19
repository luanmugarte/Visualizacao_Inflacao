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
inflacao_dist_acum_t1 <- inflacao_dist %>%
  mutate(across(!Data, ~(as.numeric(.)/100)+1)) %>%
  mutate(across(!Data, ~ round((RcppRoll::roll_prodr(., n = 12, na.rm = F, fill = 2)-1)*100,2))) %>%
  mutate(prop_alta_mbaixa = Alta/`Muito_Baixa`)
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
