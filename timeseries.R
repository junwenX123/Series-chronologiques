library(tseries)
library(forecast)
df <- read.csv("energydata_complete.csv")
sum(is.na(df))
df$date <- as.POSIXct(df$date)
plot(df$date, df$Appliances, 
     type = "l",               
     col = "steelblue",      
     main = "Consommation  des appareils (Appliances)",
     sub = "Données brutes", 
     xlab = "Date",            
     ylab = "Consommation (Wh)")
grid()

df$date_heure <- cut(df$date, breaks = "hour")

df_clean <- aggregate(Appliances ~ date_heure, data = df, sum)

df_clean$date_heure <- as.POSIXct(df_clean$date_heure)

plot(df_clean$date_heure, df_clean$Appliances, 
     type = "l",               
     col = "steelblue",      
     main = "Consommation horaire des appareils (Appliances)",
     sub = "Données agrégées par heure", 
     xlab = "Date",            
     ylab = "Consommation (Wh)")

grid()
ts_appliances <- ts(df_clean$Appliances, frequency = 24)
fit_stl <- stl(ts_appliances, s.window = "periodic")
plot(fit_stl)
decomp <- decompose(ts_appliances, type = "additive")
plot(decomp)
adf_res <- adf.test(ts_appliances, alternative = "stationary")
print(adf_res)
kpss_res <- kpss.test(ts_appliances, null = "Level")
print(kpss_res)
ts_diff_seasonal <- diff(ts_appliances, lag = 24)
plot(ts_diff_seasonal, main = "Série transformée par Différenciation ", ylab = "Différenciation saisonnière (lag 24)")
Acf(ts_appliances, lag.max = 24*14)  
Pacf(ts_appliances, lag.max = 24*14)
adf.test(ts_diff_seasonal)
