### automatitzem per a comparar els resultats del paper amb els nostres per sectors

# Load and install packages

packages <- c("ggplot2", "readxl", "tidyr", 'dplyr', 'zoo', 'CausalImpact' )

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
search()

# load and preprocess data
df <- read_excel("./data/carbon_monitoring.xlsx", 
                 sheet = "Daily Data")
View(df)


df <- dplyr::filter(df, country == 'WORLD') %>% dplyr::select(-country)

for (sec in unique(df$sector)) {
  df_real <- dplyr::filter(df, type == 'actual', sector == sec) %>% dplyr::select(-c(type, sector))
  df_sim <- dplyr::filter(df, type == 'baseline', sector == sec) %>% dplyr::select(-c(type, sector))
  

  
  ### analysis of total emissions 2020
  
  y <- df_real[366:731,'co2'] # real 2020 pre and covid data
  x1 <- df_real[1:366, 'co2'] # data from 2019 pre-covid
  t <- df_real[366:731, 'date']
  data <- cbind(y, x1, t)
  names(data) <- c('y', 'x1', 'date')
  
  # causal analysis
  data2 <- zoo(cbind(data$y, data$x1), data$date)
  
  pre.period <- as.POSIXct(c('2020-01-01', '2020-03-14'))
  post.period <- as.POSIXct(c('2020-03-15', '2020-12-31'))
  
  impact <- CausalImpact(data2, pre.period, post.period)
  
  tot_em_19_sp <- sum(df_real$co2[1:365]) # emissions totals en 2019
  tot_em_20_sp <- sum(df_real$co2[366:731]) # emissions totals en 2020
  rel_dec <- (tot_em_19_sp - tot_em_20_sp)*100/tot_em_19_sp
  # Per tant observem una reducció d'emissions (rel_dec, paper) entre 2019 i 2020
  
  rel_dec_covid <- (impact$summary$Pred[2] - impact$summary$Actual[2])*100/impact$summary$Pred[2] # causal impact compute of emissions reduction due to covid19
  
  print(sec)
  print('Paper interannual decay')
  print(rel_dec)
  print('Causal impact covid due decay')
  print(rel_dec_covid)
}