
here = "~/clima/4.1_TEMPERATURAS_ARTIGO/"
db = "~/clima/data_base/inmet_bruto/"

setwd(here)
source("../include/analysis_functions.R")


stat.aux <- stations_info_INMET(db, cidade = "", estado = "", regiao = "")

info.station <- stat.aux$info.station
station.idx <- stat.aux$station.idx

final.date = ymd("2021-01-01")

count.station = 0
tot.m.avg.anom.tx <- NULL
tot.m.avg.anom.tn <- NULL
days.per.year <- NULL

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
  
  month.avg.anomaly.tx <- data.frame(year = year(avg.aux.tx$ym), 
                                    month = month(avg.aux.tx$ym),
                                    frac.year = round(frac.year(avg.aux.tx$ym),2), 
                                    tmax = avg.aux.tx$x)
  tot.m.avg.anom.tx <- rbind(tot.m.avg.anom.tx, month.avg.anomaly.tx)
  
  month.avg.anomaly.tn <- data.frame(year = year(avg.aux.tn$ym), 
                                  month = month(avg.aux.tn$ym),
                                  frac.year = round(frac.year(avg.aux.tn$ym),2), 
                                  tmin = avg.aux.tn$x)
  tot.m.avg.anom.tn <- rbind(tot.m.avg.anom.tn, month.avg.anomaly.tn)
  
  days.p.y <- aggregate(df.tx$date, by = list(year = year(df.tx$date)), length)
  days.per.year <- rbind(days.per.year, 
                         data.frame(year = days.p.y$year, days = days.p.y$x,
                                    station = rep(stat,length(days.p.y[,1]))) )
  count.station = count.station+1
}

### working stations ###
{
  func <- aggregate(days.per.year, by = list(years = days.per.year$year), sum)[,c(1,3)]
  
  func$days <- func$days/365
  
  path = "anomaly/working_stations.png"
  png(path, 1000, 600)
    barplot(func$days, names.arg = func$years,
            main = paste("Working Climate Stations over",count.station,"total"),
            ylab = "Years", xlab = "Working Stations")
  dev.off()
  
  path = "anomaly/working_stations_proportion.png"
  png(path, 1000, 600)
    barplot(func$days/count.station, names.arg = func$years,
            main = paste("Proportional Working Climate Stations over",count.station,"total"),
            ylab = "Years", xlab = "Working Stations")
  dev.off()
}

# Tmax make means over 5 years
{
  window.mean = 5 
  
  month.avg <- NULL
  for(month in 1:12) {
    idx <- which(tot.m.avg.anom.tx$month == month)
    
    group <- trunc(tot.m.avg.anom.tx$year[idx]/window.mean)*window.mean
    
    month.avg <- cbind(month.avg,
                       aggregate(tot.m.avg.anom.tx$tmax[idx], by = list(ym = group), mean)$x )
  }
  
  
  year.ref <- unique(group)
  year.ref <- year.ref[order(year.ref)]
  
  n = length(year.ref)
  col <- paletteer_c("grDevices::rainbow", n) 
  
  ylim = c(min(month.avg),max(month.avg))
  
  path = "anomaly/brasil_month_anomaly_tmax.png"
  png(path, 1000, 600)
  
  # idx = which(tx.ym$year == uniq.years[1])
  plot(1:12, month.avg[1,], t = "l", col = col[1], ylim = ylim, lwd = 4,
       main = "Brazil's Maximum Temperature Anomaly (5 year mean)\n(average over all stations)",
       xlab = "Months", ylab = "Anomaly Temperature (ºC)")
  
  for(i in 2:n) {
    lines(1:12, month.avg[i,], col = col[i], lwd = 4)
  }
  idx <- order(year.ref, decreasing = T)
  legend("bottomright", legend = paste(year.ref[idx]), col = col[idx], lwd = 5, bty = "n", 
         cex = 1.4, text.col = col[idx], text.font = 2)
  dev.off()
}

# Tmin make means over 5 years
{
  window.mean = 5 
  
  month.avg <- NULL
  for(month in 1:12) {
    idx <- which(tot.m.avg.anom.tn$month == month)
    
    group <- round(tot.m.avg.anom.tn$year[idx]/window.mean)*window.mean
    
    month.avg <- cbind(month.avg,
                       aggregate(tot.m.avg.anom.tn$tmin[idx], by = list(ym = group), mean)$x )
  }
  
  
  year.ref <- unique(group)
  year.ref <- year.ref[order(year.ref)]
  
  n = length(year.ref)
  col <- paletteer_c("grDevices::rainbow", n) 
  
  ylim = c(min(month.avg),max(month.avg))
  
  path = "anomaly/brasil_month_anomaly_tmin.png"
  png(path, 1000, 600)
  
  # idx = which(tx.ym$year == uniq.years[1])
  plot(1:12, month.avg[1,], t = "l", col = col[1], ylim = ylim, lwd = 4,
       main = "Brazil's Minimum Temperature Anomaly (5 year mean)\n(average over all stations)",
       xlab = "Months", ylab = "Anomaly Temperature (ºC)")
  
  for(i in 2:n) {
    lines(1:12, month.avg[i,], col = col[i], lwd = 4)
  }
  idx <- order(year.ref, decreasing = T)
  legend("bottomright", legend = paste(year.ref[idx]), col = col[idx], lwd = 5, bty = "n", 
         cex = 1.4, text.col = col[idx], text.font = 2)
  dev.off()
}

### tmax year average ###
{
  group = tot.m.avg.anom.tx$year
  
  tmax.anomaly.avg <- aggregate(tot.m.avg.anom.tx, by = list(y = group), mean)
  tmax.anomaly.avg <- tmax.anomaly.avg[order(tmax.anomaly.avg$frac.year),]
  
  
  
  path = "temp_anomaly/tx_year_anomaly.pdf"
  pdf(path, 10, 6)
  
    par(mar = c(1.3,2,1,0.5)*4)
  
    xlim = c(1960,2024)
    plot(tmax.anomaly.avg$year, tmax.anomaly.avg$tmax, t = "l", lwd = 5, xlim = xlim,
         yaxt = "n", xaxt = "n", main = "", xlab = "", ylab = "")
    
    
    mtext(side = 3, "TX Anomaly", font = 2, cex = 3, line = 1, adj = 0)
    mtext(side = 2, "Temperature", line = 6, font = 2, cex = 2)
    mtext(side = 1, "Years", font = 2, cex = 2, line = 3)
    axis(1, font = 2, cex.axis = 1.8)
    axis(2, las = 2, font = 2, cex.axis = 1.8,
         at = c(-0.5,0,0.5,1,1.5), labels = paste(c(-0.5,0,0.5,1,1.5),"ºC"))
    
    grid(nx = NA, ny = NULL, lwd = 2, lty = 2, col = "grey80")
    
    ylim <- c(-1,2)
    rect(frac.year("1961-01-01"), ylim[1], frac.year("1990-01-01"), ylim[2], 
         col = rgb(0,1,0,0.1), lty = 0)
    lines(tmax.anomaly.avg$year, tmax.anomaly.avg$tmax, t = "l", lwd = 5)
    
    smooth_curve <- loess(tmax.anomaly.avg$tmax ~ tmax.anomaly.avg$year)
    
    lines(tmax.anomaly.avg$year, predict(smooth_curve), col = "red", lwd = 5)
    
    T_bigger = predict(smooth_curve)[60]
    
    arrows(2022,0, 2022,T_bigger, angle = 90, code = 3, lwd = 3, col = "black")
    text(2015,0.25, paste(round(T_bigger,2),"ºC"), font = 2, cex = 2.5)
    
    
    arrows(1961,1,1990,1, code = 3, angle = 90, length = 0.12, lwd = 3, col = "darkgreen")
    text(1975,1.1, "Reference Period", font = 2, cex = 1.4, col = "darkgreen")
  
  dev.off()
}

### tmin year average ###
{
  group = tot.m.avg.anom.tn$year
  
  tmin.anomaly.avg <- aggregate(tot.m.avg.anom.tn, by = list(y = group), mean)
  tmin.anomaly.avg <- tmin.anomaly.avg[order(tmin.anomaly.avg$frac.year),]
  
  path = "temp_anomaly/tn_year_anomaly.pdf"
  pdf(path, 10, 6)
  
  par(mar = c(1.3,2,1,0.5)*4)
  
  xlim = c(1960,2024)
  plot(tmin.anomaly.avg$year, tmin.anomaly.avg$tmin, t = "l", lwd = 5, xlim = xlim,
       yaxt = "n", xaxt = "n", main = "", xlab = "", ylab = "")
  
  
  mtext(side = 3, "TN Anomaly", font = 2, cex = 3, line = 1, adj = 0)
  mtext(side = 2, "Temperature", line = 6, font = 2, cex = 2)
  mtext(side = 1, "Years", font = 2, cex = 2, line = 3)
  axis(1, font = 2, cex.axis = 1.8)
  axis(2, las = 2, font = 2, cex.axis = 1.8,
       at = c(-0.5,0,0.5,1,1.5), labels = paste(c(-0.5,0,0.5,1,1.5),"ºC"))
  
  grid(nx = NA, ny = NULL, lwd = 2, lty = 2, col = "grey80")
  
  ylim <- c(-1,2)
  rect(frac.year("1961-01-01"), ylim[1], frac.year("1990-01-01"), ylim[2], 
       col = rgb(0,1,0,0.1), lty = 0)
  lines(tmin.anomaly.avg$year, tmin.anomaly.avg$tmin, t = "l", lwd = 5)
  
  smooth_curve <- loess(tmin.anomaly.avg$tmin ~ tmin.anomaly.avg$year)
  
  lines(tmin.anomaly.avg$year, predict(smooth_curve), col = "red", lwd = 5)
  
  T_bigger = predict(smooth_curve)[60]
  
  arrows(2022,0, 2022,T_bigger, angle = 90, code = 3, lwd = 3, col = "black")
  text(2015,0.25, paste(round(T_bigger,2),"ºC"), font = 2, cex = 2.5)
  
  
  arrows(1961,1,1990,1, code = 3, angle = 90, length = 0.12, lwd = 3, col = "darkgreen")
  text(1975,1.1, "Reference Period", font = 2, cex = 1.4, col = "darkgreen")
  
  dev.off()
}
