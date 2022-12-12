

## paleta wes_anderson:
col.wes <- as.character(wes_palette("Cavalcanti1", n = 5, type = "discrete"))

## paleta regioes do brasil:
## col.regions <- hcl.colors(5, "Temps", alpha = 1)


## Setting plot title with station identification
station.title <- function(title, info.station, i) { 
  paste("Name:",info.station$name[i], " Station:",info.station$code[i],
      "\n",title)
}

## plot Extreme Precipitation Volume Time Serie
plot.EPV.serie <- function(date, prec, p95, prob.u, time.window, 
                           info.statio, station.i.want) { 
  
    p95.mean <- window.mean(date[p95], prec[p95], window = time.window)
    plot(frac.year(date[p95]), prec[p95], lwd = 2, col = "purple",
         xlab = "Years", ylab = "Precipitation (mm)",
         main = paste("Name:",info.station$name[station.i.want],
                      " Station:",info.station$code[station.i.want],
                      "\nExtreme Precipitation Volume Time Serie"))
    missing.date.interval.plot(date, ylim = par("yaxp"))
    lines(p95.mean$years, p95.mean$mean, lwd = 3, col = "red")
    legend("topright", legend = c(paste0("prec > u(Δt = ",time.window,") = ",prob.u,"%"),
                                  "Extremes Mean (Δt = ",time.window,")"),
           lwd = c(2,4), lty = c(0,1), pch = c(1,-1), 
           col = c("purple","red"), cex = 2, bty = "n", text.font = 2)
}
  
## function to plot lack data window on time serie: 
missing.date.interval.plot <- function(date, ylim) {
  # ymin = par("yaxp") is the standard definition to be used after the main plot
  
  lack.days <- diff(date)
  where.lack <- which(lack.days != 1)
  
  half.dy = (ylim[2]-ylim[1])/2.
  ylim <- c(ylim[1]-half.dy, ylim[2]+half.dy)
  
  out <- NULL
  for(i in where.lack) {
    rect(frac.year(date[i]), ylim[1], frac.year(date[i+1]), ylim[2], col = rgb(1,0,0,0.2), lty = 0)
    # rect(date[i], ylim[1], date[i+1], ylim[2], col = rgb(1,0,0,0.2), lty = 0)
  }
}

reference.period.interval.plot <- function(start = 1961, final = 1990, y.pos, type = "interval", date = T) {
  
  if(date == T) { start = frac.to.date(start); final = frac.to.date(final) }
  
  dy = y.pos[2]-y.pos[1]
  dx = final-start
  # if(type == "arrows") {
  #   arrow.y.pos = y.pos[1] + dy*3/4
  #   text.x.pos = start + dx/2
  #   
  #   arrows(start,arrow.y.pos,final,arrow.y.pos, code = 3, angle = 90, length = 0.12, lwd = 3, col = "darkgreen")
  #   text(text.x.pos,y.pos+dy*0.05, "Reference Period", font = 2, cex = 1.8, col = "darkgreen")
  # }
  if(type == "interval") {
    text.x.pos = start + dx/2
    text.y.pos = y.pos[2]+0.*dy
    
    rect(start,-y.pos[1], final,3*y.pos[2], col = rgb(0,1,0,0.2), lty = 0)
    text(text.x.pos,text.y.pos, "Reference Period", font = 2, cex = 1.8, col = "darkgreen")
  }
  
}
  

barplot.month.s2id <- function(data, variable, year, uf) {
  ## 'data' é um data.frame com conjunto de dados muito específicos
  ## 'variable' busca uma coluna específica (relativa a um custo ou ocorrência de algum evento) e a plota
  
  y.scale = max(data[[variable]])
  
  par(mar = c(8,2.5,2,1.5))
  bp <- barplot(data[[variable]], ylim = c(0,y.scale*1.3), 
                main = paste0(variable," em ",year," no ",uf))
  text(x = bp, y = -y.scale*0.05, labels = data$Desastre, srt = 60, xpd = T, adj = 1, cex = 1, font = 2)
  text(x = bp, y = data[[variable]], 
       label = signif(data[[variable]],2), pos = 3)
}


plot.series.custos.2x2.s2id <- function(serie, uf) {
  par(mfrow = c(2,2))
  
  ylim = c(min(serie[,seq(2,10,3)]), max(serie[,seq(2,10,3)]))
  
  plot(serie$ano, serie[,2], t = "l", lwd = 3, col = "blue", ylim = ylim,
       xlab = "Ano", ylab = "Ocorrências por município", 
       main = paste("Eventos de Chuva e Seca -",uf))
  lines(serie$ano, serie[,5], lwd = 3, col = "red")
  lines(serie$ano, serie[,8], lwd = 3, col = "darkgreen")
  legend("topleft", legend = c("Chuva Forte","Falta de chuva","Consequências da chuva"),
         col = c("blue","red","darkgreen"), lwd = 2, lty = 1, text.font = 2, 
         cex = 1.2, bty = "n")
  
  ylim = c(min(serie[,seq(11,19,3)]), max(serie[,seq(11,19,3)]))
  
  plot(serie$ano, serie[,11], t = "l", lwd = 3, col = "cadetblue", ylim = ylim,
       xlab = "Ano", ylab = "Ocorrências por município", 
       main = paste("Eventos de Temperatura e Incêndios -",uf))
  lines(serie$ano, serie[,14], lwd = 3, col = "darkred")
  lines(serie$ano, serie[,17], lwd = 3, col = "purple")
  legend("topleft", legend = c("Calor","Frio","Incêndios"),
         col = c("cadetblue","darkred","purple"), lwd = 2, lty = 1, text.font = 2, 
         cex = 1.2, bty = "n")
  
  ylim = c(min(serie[,seq(3,10,3)]/1e6), max(serie[,seq(3,10,3)]/1e6))
  
  plot(serie$ano, serie[,3]/1e6, t = "l", lwd = 3, col = "blue", ylim = ylim,
       xlab = "Ano", ylab = "Custo (em Milhões de Reais)", 
       main = paste("Custo total dos Eventos de Chuva e Seca (Mi) -",uf))
  lines(serie$ano, serie[,6]/1e6, lwd = 3, col = "red")
  lines(serie$ano, serie[,9]/1e6, lwd = 3, col = "darkgreen")
  legend("topleft", legend = c("Chuva Forte","Falta de chuva","Consequências da chuva"),
         col = c("blue","red","darkgreen"), lwd = 2, lty = 1, text.font = 2, 
         cex = 1.2, bty = "n")
  
  ylim = c(min(serie[,seq(4,10,3)]), max(serie[,seq(4,10,3)]))
  
  plot(serie$ano, serie[,4], t = "l", lwd = 3, col = "blue", ylim = ylim,
       xlab = "Ano", ylab = "Custo (em Reais)", 
       main = paste("Custo médio per capta dos Eventos de Chuva e Seca (Reais) -",uf))
  lines(serie$ano, serie[,7], lwd = 3, col = "red")
  lines(serie$ano, serie[,10], lwd = 3, col = "darkgreen")
  legend("topleft", legend = c("Chuva Forte","Falta de chuva","Consequências da chuva"),
         col = c("blue","red","darkgreen"), lwd = 2, lty = 1, text.font = 2, 
         cex = 1.2, bty = "n")
}


# geographic map plot
plot.brasil.events <- function(df, title, legend, leg.lim = c(-0.03, 0.03), plot.test = F,
                               colors = c('#7C4D79',"white",'#0F4A1D')) {
  # if your scale include 0 value, it is advisable to use the "white" color together in the scale
  
  if(length(df) != 3) {
    print("Error: enter a df with 3 columns: longitud, latitud and variable to be ploted")
    return()
  }
  if(length(df) == 3) {
    
    
    mapaUF <- st_read("../data_base/mapa_brasil_2010/grandes_regioes_shp.shp", quiet = T)
    map.xlim = c(-70,-25)
    
    df <- na.omit(df)
    
    simm.idx <- which(df[,3] > leg.lim[1] & df[,3] < leg.lim[2])
    
    nslices = 10
    cuts <- cut(df[simm.idx,3], breaks = nslices)
    
    chars <- levels(cuts)
    chars <- gsub("\\(","", chars)
    chars <- gsub("]","", chars)
    chars <- gsub(","," - ", chars)
    
    leg.values <- unique( as.numeric( unlist(strsplit(chars, " - ")) ) )
    leg.values <- leg.values[1:nslices] + diff(leg.values)/2
    
    zero.idx <- order(abs( leg.values ))[1]
    
    col.regions <- hcl.colors(5, "Temps", alpha = 1)
    rbPal <- colorRampPalette( colors )
    
    point.col <- rbPal(nslices)[as.numeric(cuts)]
    
    col <- rep(NA, length(df[,3]))
    col[simm.idx] <- point.col
    col[-simm.idx] <- ifelse(df[-simm.idx,3] > 0, colors[length(colors)], colors[1] )
    
    col <- alpha(col, abs(df[,3])/max(abs(df[,3])))
    
    inverse <- nslices:1
    
    if(plot.test) png("map_test.png", 1000, 800)
    
    plot(st_geometry(mapaUF$geometry), axes = F, xlab = "", ylab = "", main = "",
         yaxt = "n", xaxt = "n",
         col = alpha(col.regions,.0), xlim = map.xlim )
    points(df[,2], df[,1], pch = 16, col = col, cex = 3)
    legend(-32, 1, legend = round(leg.values[inverse], 3), col = rbPal(nslices)[inverse], pch = 16,
           bty = "n", pt.cex = 5, text.font = 2, cex = 3)
    text(-27, 2, legend, font = 2, cex = 3.5)
    text(-66, -23, title, font = 2, cex = 3.5)
    if(plot.test) dev.off()
    
  }
}
