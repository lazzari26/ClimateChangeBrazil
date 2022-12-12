
here = "~/clima/4.1_TEMPERATURAS_ARTIGO/"

setwd(here)
source("../include/analysis_functions.R")


stat.aux <- stations_info_INMET()

info.station <- stat.aux$info.station
station.idx <- stat.aux$station.idx

final.date = ymd("2021-01-01")

count.station = 0
tot.m.avg.anom.tx <- NULL
tot.m.avg.anom.tn <- NULL
days.per.year <- NULL

stat = 30
df <- NULL
for(stat in station.idx) {
  
  print(paste0(round(stat/length(info.station$name)*100),"%  Station: ",
               info.station$name[stat]))
  
  station.path <- info.station$path[stat]
  
  df.tx <- read_INMET_variable(station.path, variable = "tmax")
  df.tn <- read_INMET_variable(station.path, variable = "tmin")
  
  df.tx <- df.tx[df.tx$date < final.date,]
  df.tn <- df.tn[df.tn$date < final.date,]
  
  if( is.null(df.tx) | is.null(df.tn) ) next
  
  idx.anomaly.ref.date.tx <- which(df.tx$date > "1960-01-01" & df.tx$date < "1990-01-01")
  idx.anomaly.ref.date.tn <- which(df.tn$date > "1960-01-01" & df.tn$date < "1990-01-01")
  
  if(length(idx.anomaly.ref.date.tx) < 10 | length(idx.anomaly.ref.date.tn) < 10) next
  
  anomaly.ref.tx = mean(df.tx$tmax[idx.anomaly.ref.date.tx])
  anomaly.ref.tn = mean(df.tn$tmin[idx.anomaly.ref.date.tn])
  
  tmax.anomaly <- df.tx$tmax - anomaly.ref.tx
  date.group.tx <- paste0(format(df.tx$date, "%Y-%m"),"-01")
  
  tmin.anomaly <- df.tn$tmin - anomaly.ref.tn
  date.group.tn <- paste0(format(df.tn$date, "%Y-%m"),"-01")
  
  avg.aux.tx <- aggregate(tmax.anomaly, by = list(ym = date.group.tx), mean)
  avg.aux.tn <- aggregate(tmin.anomaly, by = list(ym = date.group.tn), mean)
  
  tx.anomaly <- data.frame(Year = frac.year(avg.aux.tx[,1]), Anomaly = avg.aux.tx[,2])
  tn.anomaly <- data.frame(Year = frac.year(avg.aux.tn[,1]), Anomaly = avg.aux.tn[,2])
  
  tx.anom.avg <- mean(tx.anomaly[tx.anomaly$Year > 2015,2])
  tn.anom.avg <- mean(tn.anomaly[tn.anomaly$Year > 2015,2])
  
  fit.tx <- lm(Anomaly~Year, data = tx.anomaly)
  fit.tn <- lm(Anomaly~Year, data = tx.anomaly)
  
  c.tx <- coef(fit.tx)
  c.tn <- coef(fit.tn)
  
  # plotting annomalie for an station
  if(stat %in% c(30,60,90,120)) {
    png(paste0("temp_anomaly/tmax_anomaly_",info.station$name[stat],".png"), 1000, 600 )
    plot(tx.anomaly, t = "l", lwd = 1, ylab = "Tmax Anomaly")
    abline(a = coeff, lwd = 4, col = "red", lty = 2)
    legend("bottomright", legend = paste("Slope:",round(coeff[2], 4)), 
           text.font = 2, text.col = "red", bty = "n")
    title(main = paste("Tmax Anomaly - Station:",info.station$name[stat]), line = .5)
    dev.off()
    
    png(paste0("temp_anomaly/tmin_anomaly_",info.station$name[stat],".png"), 1000, 600 )
    plot(tn.anomaly, t = "l", lwd = 1, ylab = "Tmin Anomaly")
    abline(a = coeff, lwd = 4, col = "red", lty = 2)
    legend("bottomright", legend = paste("Slope:",round(coeff[2], 4)), 
           text.font = 2, text.col = "red", bty = "n")
    title(main = paste("Tmin Anomaly - Station:",info.station$name[stat]), line = .5)
    dev.off()
  }
  
  df <- rbind(df, data.frame(longitude = info.station$long[stat],
                             latitude = info.station$lat[stat],
                             tx.anom = tx.anom.avg,
                             tn.anom = tn.anom.avg,
                             slope.tx = c.tx[2], 
                             slope.tn = c.tn[2]) )
  
  
  count.station = count.station+1
}



### Plot Brazil Map ###


png("temp_extremes/proportional_change_tx90p.png", 1000, 800)

df.ratio.tx <- data.frame( longitud, latitud, extreme.ratios$ratio.tx )
title = "Extreme Maximum Temperature Proportional Change in the last 30 years"
plot.brasil.events(df, title, "Proportion")

dev.off()
