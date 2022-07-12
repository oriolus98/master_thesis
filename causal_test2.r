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

# we study data from Spain
df_real <- dplyr::filter(df, type == 'actual', country == 'Spain') %>% dplyr::select(-c(type, country))
df_sim <- dplyr::filter(df, type == 'baseline', country == 'Spain') %>% dplyr::select(-c(type, country))

df_real <- pivot_wider(df_real, names_from = 'sector', values_from = 'co2')
df_sim <- pivot_wider(df_sim, names_from = 'sector', values_from = 'co2')

### analysis of total emissions

y <- df_real[366:578,'Total'] # real 2020 pre and covid data
x1 <- df_sim[1:213, 'Total'] # simulated data for 2020 wtho covid
x2 <- df_real[1:213, 'Total'] # data from 2019 pre-covid
t <- df_sim[1:213, 'date']
data <- cbind(y, x1, x2, t)
names(data) <- c('y', 'x1', 'x2', 'date')

# visualize data
ggplot(data = data) + geom_line(aes(x = date, y = x1), color = 'red') + 
  geom_line(aes(x = date, y = x2)) + geom_line(aes(x = date, y = y), color = 'green')

# causal analysis
data2 <- zoo(cbind(data$y, data$x2), data$date)

pre.period <- as.POSIXct(c('2020-01-01', '2020-03-14'))
post.period <- as.POSIXct(c('2020-03-15', '2020-08-01'))

impact <- CausalImpact(data2, pre.period, post.period)
plot(impact)
summary(impact, 'report')


### analysis of residential emissions

y <- df_real[366:578,'Residential']
x1 <- df_sim[1:213, 'Residential']
x2 <- df_real[1:213, 'Residential']
t <- df_sim[1:213, 'date']
data <- cbind(y, x1, x2, t)
names(data) <- c('y', 'x1', 'x2', 'date')

# visualize data
ggplot(data = data) + geom_line(aes(x = date, y = x1), color = 'red') + 
  geom_line(aes(x = date, y = x2)) + geom_line(aes(x = date, y = y), color = 'green')

# causal analysis

#data2 <- zoo(cbind(data$y, data$x1, data$x2), data$date)
data2 <- zoo(cbind(data$y, data$x2), data$date)

pre.period <- as.POSIXct(c('2020-01-01', '2020-03-14'))
post.period <- as.POSIXct(c('2020-03-15', '2020-08-01'))

impact <- CausalImpact(data2, pre.period, post.period)
plot(impact)
summary(impact, 'report')