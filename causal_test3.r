### automatitzem per a comparar els resultats del paper amb els nostres

for (zone in unique(df$country)) {
  df_real <- dplyr::filter(df, type == 'actual', country == zone) %>% dplyr::select(-c(type, country))
  df_sim <- dplyr::filter(df, type == 'baseline', country == zone) %>% dplyr::select(-c(type, country))
  
  df_real <- pivot_wider(df_real, names_from = 'sector', values_from = 'co2')
  df_sim <- pivot_wider(df_sim, names_from = 'sector', values_from = 'co2')
  
  ### analysis of total emissions 2020
  
  y <- df_real[366:731,'Total'] # real 2020 pre and covid data
  x1 <- df_real[1:366, 'Total'] # data from 2019 pre-covid
  t <- df_real[366:731, 'date']
  data <- cbind(y, x1, t)
  names(data) <- c('y', 'x1', 'date')
  
  # causal analysis
  data2 <- zoo(cbind(data$y, data$x1), data$date)
  
  pre.period <- as.POSIXct(c('2020-01-01', '2020-03-14'))
  post.period <- as.POSIXct(c('2020-03-15', '2020-12-31'))
  
  impact <- CausalImpact(data2, pre.period, post.period)
  
  tot_em_19_sp <- sum(df_real$Total[1:365]) # emissions totals en 2019
  tot_em_20_sp <- sum(df_real$Total[366:731]) # emissions totals en 2020
  rel_dec <- (tot_em_19_sp - tot_em_20_sp)*100/tot_em_19_sp
  # Per tant observem una reducció d'emissions (rel_dec, paper) entre 2019 i 2020
  rel_dec_covid <- (impact$summary$Pred[2] - impact$summary$Actual[2])*100/tot_em_19_sp # causal impact compute of emissions reduction due to covid19

  print(zone)
  print('Paper interannual decay')
  print(rel_dec)
  print('Causal impact covid due decay')
  print(rel_dec_covid)
}