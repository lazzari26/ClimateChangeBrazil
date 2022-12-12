
here = "~/clima/4.1_TEMPERATURAS_ARTIGO/"

setwd(here)
source("../include/analysis_functions.R")


stat.aux <- stations_info_INMET()

info.station <- stat.aux$info.station
station.idx <- stat.aux$station.idx

start.date = ymd("1960-01-01")
final.date = ymd("2021-01-01")
total.date <- seq(start.date, final.date, by = "day")

days.per.year <- NULL

days.sequence <- 1:5      ### consecutive days to calculate
cut = c(.1,.9)            ### thresholds for tmin and tmax, respectively

ext.points.tx = vector("list", length(days.sequence))
ext.points.tn = vector("list", length(days.sequence))

extreme.ratios <- NULL
valid.stations <- NULL

### Calculating Extremes and Consecutiveness for all stations
for(stat in station.idx) {
  
  print(paste0(round(stat/length(info.station$name)*100),"%  Station: ",
               info.station$name[stat]))
  
  station.path <- info.station$path[stat]
  
  df.tn <- read_INMET_variable(station.path, variable = "tmin")
  df.tx <- read_INMET_variable(station.path, variable = "tmax")
  
  # df.pr <- read_INMET_variable(station.path, variable = "prec")
  # df.pr <- df.pr[df.pr$date < final.date,]
  
  df.tn <- df.tn[df.tn$date < final.date,]
  df.tx <- df.tx[df.tx$date < final.date,]
  
  if( is.null(df.tn) | is.null(df.tx) ) next
  
  t.tn = length(df.tn[,1]) < 0.1*length(total.date)
  t.tx = length(df.tx[,1]) < 0.1*length(total.date)
  
  if(t.tn | t.tx) next
  
  temp.extremes <- temperature_extremes(df.tx, df.tn, cut = cut)
  
  if(is.null(temp.extremes)) next
  
  ext.idx.tn <- temp.extremes$idx.tn
  ext.idx.tx <- temp.extremes$idx.tx
  
  # calculating the percentage change
  idx.tn.e <- which(df.tn$date[ext.idx.tn] > as.Date("1960-01-01") & 
                    df.tn$date[ext.idx.tn] < as.Date("1990-01-01") )
  idx.tn.t <- which(df.tn$date > as.Date("1960-01-01") & df.tn$date < as.Date("1990-01-01") )
  
  idx.tx.e <- which(df.tx$date[ext.idx.tx] > as.Date("1960-01-01") & 
                      df.tx$date[ext.idx.tx] < as.Date("1990-01-01") )
  idx.tx.t <- which(df.tx$date > as.Date("1960-01-01") & df.tx$date < as.Date("1990-01-01") )
  
  ratio.ref.tn = length(idx.tn.e)/length(idx.tn.t)
  ratio.ref.tx = length(idx.tx.e)/length(idx.tx.t)
  
  ratio.tn = length(ext.idx.tn)/length(df.tn$tmin) - ratio.ref.tn
  ratio.tx = length(ext.idx.tx)/length(df.tx$tmax) - ratio.ref.tx
  
  extreme.ratios <- rbind(extreme.ratios, data.frame(station = stat, ratio.tn, ratio.tx))
  
  valid.stations <- c(valid.stations, stat)
}

### Writing extreme data ###
{
  days.sequence <- 1:5
  
  write.table(days.per.year, "../data_base/inmet_temperature/working_station.dat",
              row.names = F, col.names = T)
  
  write.table(extreme.ratios, paste0("../data_base/inmet_temperature/extreme_ratios_tn",
              round(cut[1]*100),"p_tx",round(cut[2]*100),"p.dat"),
              row.names = F, col.names = T)
  
  for(i in days.sequence) {
    write.table(ext.points.tn[[i]], 
                paste0("../data_base/inmet_temperature/tn",round(cut[1]*100),"p",i,"d.dat"),
                row.names = F, col.names = T)
    write.table(ext.points.tx[[i]], 
                paste0("../data_base/inmet_temperature/tx",round(cut[2]*100),"p",i,"d.dat"),
                row.names = F, col.names = T)
  }
}

### Reading extreme data ###
{
  days.sequence <- 1:5
  
  ext.points.tx = vector("list", length(days.sequence))
  ext.points.tn = vector("list", length(days.sequence))
  
  days.per.year <- read.table("../data_base/inmet_temperature/working_station.dat")
  
  extreme.ratios <- read.table(paste0("../data_base/inmet_temperature/extreme_ratios_tn",
                                      round(cut[1]*100),"p_tx",round(cut[2]*100),"p.dat"))
  
  for(i in days.sequence) {
    ext.points.tn[[i]] <- read.table(paste0("../data_base/inmet_temperature/ext_points_tn_consec",i,".dat"))
    ext.points.tx[[i]] <- read.table(paste0("../data_base/inmet_temperature/ext_points_tx_consec",i,".dat"))
    
    names(ext.points.tn[[i]]) <- paste(ext.points.tn[[i]][1,])
    names(ext.points.tx[[i]]) <- paste(ext.points.tx[[i]][1,])
    
    ext.points.tn[[i]] <- ext.points.tn[[i]][-1,]
    ext.points.tx[[i]] <- ext.points.tx[[i]][-1,]
  }
}

############## START PLOTS AREA ############## 


# normalization = "by.station"
normalization = "cumulative"
### Normalize and take means ###
{
  cumul = F
  if(normalization == "cumulative") cumul = T
  prepare_plot_temp_EE <- function(ext.points, days.per.year, cumul.norm = F) {
    
    ### cumul.norm = T is the division of all EE (all stations) per year by the working days 
    ###                of all stations in that year
    ### cumul.norm = F is the EE division of one station by the working days of the same
    ###                station in that year
    
    names(ext.points)[2] <- "variable"
    
    if(cumul.norm == F) {
      group = list(year = year(ext.points$date), station = ext.points$station)
      ee <- aggregate(ext.points$variable, by = group, length)
      
      norm.idx <- match(paste(ee$year, ee$station),
                        paste(days.per.year$year,days.per.year$station) )
      
      ee$x <- ee$x / ( days.per.year$days[norm.idx] / 365 )
      
      ee <- na.omit(ee)
    }
    
    if(cumul.norm == T) { 
      group <- list(year = year(ext.points$date))
      ee <- aggregate(ext.points$variable, by = group, length)
      
      func <- aggregate(days.per.year$days, by = list(years = days.per.year$year), sum)
      ee$x <- ee$x / (func$x / 365)
      
      ee <- na.omit(ee)
    }
    
    
    norm <- aggregate(ee$x, by = list(year = ee$year), mean)
    
    data.frame(norm, smooth = predict( loess(norm$x ~ norm$year) ) )
    
  }
  
  df.tn <- df.tx <- 1961:2020
  sm.tn <- sm.tx <- 1961:2020
  for(days in days.sequence) {
    tx <- prepare_plot_temp_EE(ext.points.tx[[days]], days.per.year, cumul.norm = cumul)
    tn <- prepare_plot_temp_EE(ext.points.tn[[days]], days.per.year, cumul.norm = cumul)
    
    df.tx <- cbind(df.tx, tx[,2])
    df.tn <- cbind(df.tn, tn[,2])
    sm.tx <- cbind(sm.tx, tx[,3])
    sm.tn <- cbind(sm.tn, tn[,3])
  }
}




### Plot Brazil Map ###


latitude <- info.station$lat[extreme.ratios$station]
longitude <- info.station$long[extreme.ratios$station]

png("temp_extremes/proportional_change_tx90p.png", 1000, 800)

df.ratio.tx <- data.frame(latitude, longitude, extreme.ratios$ratio.tx)
title = "Extreme\nTmax"
plot.brasil.events(df.ratio.tx, title, "Proportion", leg.lim = c(-.05, .13))

dev.off()

png("temp_extremes/proportional_change_tn10p.png", 1000, 800)

df.ratio.tn <- data.frame(latitude, longitude, extreme.ratios$ratio.tn)
title = "Extreme\nTmin"
plot.brasil.events(df.ratio.tn, title, "Proportion", leg.lim = c(-.05,.05))

dev.off()






# plot_temp_EE <- function(ext.points) {
#   
#   ### Tmin EE - p05n3d ###
#   {
#     group = year(out05$date)
#     
#     tmin.ee <- aggregate(out05$tmin, by = list(year = group), length)
#     tmin.ee <- tmin.ee[order(tmin.ee$year),]
#     
#     tmin.ee$x <- tmin.ee$x / func$days
#     
#     path = paste0("temp_extreme-consecutive/brasil_ee_tn05p",seq.days.n,"d.png")
#     png(path, 1000, 600)
#     plot(tmin.ee$year, tmin.ee$x, t = "l", lwd = 5,
#          main = paste0("Brazil's Extreme Events of Minimum Temperature per Year (tn05p",seq.days.n,"d)",
#                        "\n[Tmin < 5% in ",seq.days.n," consecutive days, average over all stations]"),
#          xlab = "Years", ylab = "Frequency of Extreme Events per Station")
#     grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)  
#     
#     y.pos = par("yaxp")
#     dy = y.pos[2]-y.pos[1]
#     y.pos = y.pos[1] + dy*7/8
#     arrows(1961,y.pos,1990,y.pos, code = 3, angle = 90, length = 0.12, lwd = 3, col = "darkgreen")
#     text(1975,y.pos+dy*0.1, "Reference Period", font = 2, cex = 1.4, col = "darkgreen")
#     
#     smooth_curve <- loess(tmin.ee$x ~ tmin.ee$year)
#     lines(tmin.ee$year, predict(smooth_curve), col = "red", lwd = 5)
#     
#     dev.off()
#   }
# }
# 
# plot_temp_EE(out05.3d, out10.3d, out90.3d, out95.3d, seq.days.n = 3)
# plot_temp_EE(out05.5d, out10.5d, out90.5d, out95.5d, seq.days.n = 5)
# plot_temp_EE(out05.7d, out10.7d, out90.7d, out95.7d, seq.days.n = 7)
# plot_temp_EE(out05.10d, out10.10d, out90.10d, out95.10d, seq.days.n = 10)
# 
# ### Rare Tmax > 90p ###
# {
#   group = year(out90.1d$date)
#   
#   tmax.up <- aggregate(out90.1d$tmax, by = list(year = group), length)
#   tmax.up <- tmax.up[order(tmax.up$year),]
#   
#   tmax.up$x <- tmax.up$x / func$days
#   
#   path = "temp_rare-threshold/brasil_rare_tx90p.png"
#   png(path, 1000, 600)
#   plot(tmax.up$year, tmax.up$x, t = "l", lwd = 5,
#        main = "Brazil's Rare Events of Maximum Temperature per Year (tx90p) \n[Tmax > 90%, average over all stations]",
#        xlab = "Years", ylab = "Frequency of Rare Events per Station")
#   grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)  
#   
#   y.pos = par("yaxp")
#   dy = y.pos[2]-y.pos[1]
#   y.pos = y.pos[1] + dy*3/4
#   arrows(1961,y.pos,1990,y.pos, code = 3, angle = 90, length = 0.12, lwd = 3, col = "darkgreen")
#   text(1975,y.pos+dy*0.1, "Reference Period", font = 2, cex = 1.4, col = "darkgreen")
#   
#   smooth_curve <- loess(tmax.up$x ~ tmax.up$year)
#   lines(tmax.up$year, predict(smooth_curve), col = "red", lwd = 5)
#   
#   dev.off()
# }
# 
# ### Rare Tmax > 95p ###
# {
#   group = year(out95.1d$date)
#   
#   tmax.up <- aggregate(out95.1d$tmax, by = list(year = group), length)
#   tmax.up <- tmax.up[order(tmax.up$year),]
#   
#   tmax.up$x <- tmax.up$x / func$days
#   
#   path = "temp_rare-threshold/brasil_rare_tx95p.png"
#   png(path, 1000, 600)
#   plot(tmax.up$year, tmax.up$x, t = "l", lwd = 5,
#        main = "Brazil's Rare Events of Maximum Temperature per Year (tx95p) \n[Tmax > 95%, average over all stations]",
#        xlab = "Years", ylab = "Frequency of Rare Events per Station")
#   grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)  
#   
#   y.pos = par("yaxp")
#   dy = y.pos[2]-y.pos[1]
#   y.pos = y.pos[1] + dy*3/4
#   arrows(1961,y.pos,1990,y.pos, code = 3, angle = 90, length = 0.12, lwd = 3, col = "darkgreen")
#   text(1975,y.pos+dy*0.1, "Reference Period", font = 2, cex = 1.4, col = "darkgreen")
#   
#   smooth_curve <- loess(tmax.up$x ~ tmax.up$year)
#   lines(tmax.up$year, predict(smooth_curve), col = "red", lwd = 5)
#   
#   dev.off()
# }
# 
# ### Rare Tmin < 05p ###
# {
#   group = year(out05.1d$date)
#   
#   tmin.down <- aggregate(out.1d05$tmin, by = list(year = group), length)
#   tmin.down <- tmin.down[order(tmin.down$year),]
#   
#   tmin.down$x <- tmin.down$x / func$days
#   
#   path = "temp_rare-threshold/brasil_rare_tmin_p05n.png"
#   png(path, 1000, 600)
#   plot(tmin.down$year, tmin.down$x, t = "l", lwd = 5,
#        main = "Brazil's Rare Events of Minimum Temperature per Year (tn05p) \n[Tmin < 05%, average over all stations]",
#        xlab = "Years", ylab = "Frequency of Rare Events per Station")
#   grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)  
#   
#   y.pos = par("yaxp")
#   dy = y.pos[2]-y.pos[1]
#   y.pos = y.pos[1] + dy*7/8
#   arrows(1961,y.pos,1990,y.pos, code = 3, angle = 90, length = 0.12, lwd = 3, col = "darkgreen")
#   text(1975,y.pos+dy*0.1, "Reference Period", font = 2, cex = 1.4, col = "darkgreen")
#   
#   smooth_curve <- loess(tmin.down$x ~ tmin.down$year)
#   lines(tmin.down$year, predict(smooth_curve), col = "red", lwd = 5)
#   
#   dev.off()
# }
# 
# ### Rare Tmin < 10p ###
# {
#   group = year(out10.1d$date)
#   
#   tmin.down <- aggregate(out10.1d$tmin, by = list(year = group), length)
#   tmin.down <- tmin.down[order(tmin.down$year),]
#   
#   tmin.down$x <- tmin.down$x / func$days
#   
#   path = "temp_rare-threshold/brasil_rare_tn10p.png"
#   png(path, 1000, 600)
#   plot(tmin.down$year, tmin.down$x, t = "l", lwd = 5,
#        main = "Brazil's Rare Events of Minimum Temperature per Year (tn10p) \n[Tmin < 10%, average over all stations]",
#        xlab = "Years", ylab = "Frequency of Rare Events per Station")
#   grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1) 
#   
#   y.pos = par("yaxp")
#   dy = y.pos[2]-y.pos[1]
#   y.pos = y.pos[1] + dy*7/8
#   arrows(1961,y.pos,1990,y.pos, code = 3, angle = 90, length = 0.12, lwd = 3, col = "darkgreen")
#   text(1975,y.pos+dy*0.1, "Reference Period", font = 2, cex = 1.4, col = "darkgreen")
#   
#   smooth_curve <- loess(tmin.down$x ~ tmin.down$year)
#   lines(tmin.down$year, predict(smooth_curve), col = "red", lwd = 5)
#   
#   dev.off()
# }
