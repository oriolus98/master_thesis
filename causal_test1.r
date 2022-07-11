# Load and install packages

packages <- c("ggplot2", "readxl", "knitr", 'zoo', 'CausalImpact' )

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
search()

# load and preprocess data

df <- read_excel("./data/forcings.xlsx")
names(df) <- c('Year', 'CO2', 'Solar_irradiance', 'Land_use', 'Volcanic', 'Temp_anomaly')
df$Year <- as.Date(as.character(df$Year), format = "%Y")
df$Volcanic[is.na(df$Volcanic)] <- mean(df$Volcanic, na.rm = TRUE)

# we can choose one of these 4 possible analysis

#data <- zoo(cbind(df$Temp_anomaly, df$CO2, df$Land_use), df$Year)
data <- zoo(cbind(df$Temp_anomaly, df$Solar_irradiance, df$Volcanic), df$Year)
#data <- zoo(cbind(df$Temp_anomaly, df$Solar_irradiance), df$Year)
#data <- zoo(cbind(df$Temp_anomaly, df$Solar_irradiance, df$Volcanic, df$Land_use), df$Year)

# visualize data

ggplot(data = df) + geom_line(aes(x= Year, y = CO2)) 
ggplot(data = df) + geom_line(aes(x= Year, y = Temp_anomaly))

# causal analysis

pre.period <- as.Date(c("1850-07-06", "1940-07-06"))
post.period <- as.Date(c("1941-07-06", "2005-07-06"))

impact <- CausalImpact(data, pre.period, post.period)
plot(impact,c("original", "pointwise")) # cumulative is not useful in this case

# show repport

summary(impact, 'report')

# covariates used (problematic)

plot(impact$model$bsts.model, "coefficients")