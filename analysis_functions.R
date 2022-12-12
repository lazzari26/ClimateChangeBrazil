require(readxl, quietly = T)
require(dplyr, quietly = T)
require(ggplot2, quietly = T)
require(lubridate, quietly = T)
require(extRemes, quietly = T)
require(DataCombine, quietly = T)
require(forecast, quietly = T)
require(wesanderson, quietly = T)
require(minpack.lm, quietly = T)
require(sf, quietly = T)
require(maptools, quietly = T)
require(plotrix, quietly = T)
require(Kendall, quietly = T)
require(paletteer, quietly = T)
require(naturalsort, quietly = T)

source("~/clima/include/plot_functions.R")
source("~/clima/include/read_functions.R")

## function to fractionate year on data
frac.year <- function(date) { return(year(date)+yday(date)/366) }

## function to fractionate year on data
frac.to.date <- function(date) { as.Date(date_decimal(date)) }

## function to calculate mean over time window: out -> line data frame
window.mean <- function(date, var, window = 1, by = "year") {
  
  if(!is.Date(date)) date <- frac.to.date(date)
  
  if(by == "day") group <- year(date)+yday(date)/366
  if(by == "week") group <- year(date)+week(date)/52
  if(by == "month") group <- year(date)+month(date)/12
  if(by == "year") group <- round(year(date)/window)*window
  if(by == "half.decade") group <- round(year(date)/(5*window))*5*window
  if(by == "decade") group <- round(year(date)/(10*window))*(10*window)
  
  m.mean <- aggregate(var, by = list(year = group), mean)
  
  m.period <- rep(m.mean$year, each = 2)
  m.mean <- rep(m.mean$x, each = 2)
  
  m.period <- m.period[-c(1,length(m.period))]
  m.mean <- m.mean[-c((length(m.mean)-1):length(m.mean))]
  
  data.frame(years = m.period, mean = m.mean)
}

## function to calculate quantile over time window: 
##                      out -> line data frame, c(idx) points > threshold
window.quantile <- function(date, variable, prob, window = 1, by = "year") {
  
  if(!is.Date(date)) date <- frac.to.date(date)
  
  if(by == "day") group <- year(date)+yday(date)/366
  if(by == "week") group <- year(date)+week(date)/52
  if(by == "month") group <- year(date)+month(date)/12
  if(by == "year") group <- round(year(date)/window)*window
  if(by == "half.decade") group <- round(year(date)/(5*window))*5*window
  if(by == "decade") group <- round(year(date)/(10*window))*(10*window)
  
  q <- NULL
  p.idx <- NULL
  u.idx <- NULL
  y <- NULL
  for(i in unique(group)) {
    interval.dt.idx <- i == group
    
    threshold = quantile(variable[interval.dt.idx], prob)
    
    q <- rbind(q, data.frame(x = i, variable = threshold))
    
    instant.idx <- which(interval.dt.idx & variable >= threshold )
    
    p.idx <- c(p.idx, instant.idx)
    u.idx <- c(u.idx, rep(threshold, length(instant.idx)) )
    y <- c(y, variable[instant.idx] - threshold)
  }
  
  q.period <- rep(q$x, each = 2)
  q <- rep(q$variable, each = 2)
  
  q.period <- q.period[-c(1,length(q.period))]
  q <- q[-c((length(q)-1):length(q))]
  
  list( quantile.line = data.frame(years = q.period, quantile = q), 
        points.above.idx = p.idx,
        u.for.each.idx = u.idx,
        threshold.distance = y )
}

## Function to make a complete analysis of precipitation:
ev_prec_sintesis <- function(station.path, 
                             us = c(0.99),     # percentil threshold (can be more than one)
                             time.window = 1,  # time.window in years
                             plot.serie = F, 
                             start.date = ymd("1960-01-01"), 
                             final.date = ymd("2022-01-01")) {
  ## This function makes:
  ##                      1. correct data file
  ##                      2. take extreme points over threshold (can be more than one)
  ##                      3. take extreme value distributions
  ##                      4. fit extremes and give the slope and mean coefficients
  
  ### Technical corrections for INMET basis ###
  file <- tryCatch(read.csv(station.path, skip = 10, sep = ";"), 
           error = function(e) NULL)
  
  prec <- NULL
  
  if(!is.null(file)) {
    ### Verifying if the last column is blank
    last.col = length(file[1,])
    data.size = length(file[,1])
    if( sum(is.na(file[,last.col])) == data.size ) file <- file[,-last.col]
    
    prec <- file$PRECIPITACAO.TOTAL..DIARIO.mm.
    date <- file$Data.Medicao
    
    ### Keeping just valid points
    omit.idx <- which(prec == "null" | 
                      as.Date(date) > final.date | 
                      as.Date(date) < start.date )
    if(length(omit.idx) > 0) {
      prec <- prec[-omit.idx]
      date <- date[-omit.idx]
    }
    
    prec <- as.numeric(prec)
    date <- as.Date(date)
  }
  
  # mean.serie <- window.mean(date, prec, window = 1)
  
  col <- as.character(wes_palette("Cavalcanti1", n = length(us), type = "discrete"))
  
  if(length(prec) == 0) { print("Not analyzed: no precipitation value."); return(NA) }
  else {
    
    # print("Working station.")
  
    extreme.dist <- list()
    extreme.points <- list()
    fit.coef <- NULL
    
    for(i in 1:length(us)) {
      prob.u = us[i]
      aux <- window.quantile(date, prec, prob = prob.u, window = 1, by = "year")
      
      quantile.line <- aux$quantile.line
      points.above.idx <- aux$points.above.idx
      points.threshold <- aux$u.for.each.idx
      
      dist <- hist(prec[points.above.idx], breaks = 40, plot = F)
      
      
      if(plot.serie == T) { plot.EPV.serie(date, prec, points.above.idx, prob.u, time.window,
                                           info.statio, station.i.want) }
      
      prob.name <- paste0("p",round(prob.u*100) )
      
      extreme.points[[prob.name]] <- data.frame(idx = points.above.idx, threshold = points.threshold)
      extreme.dist[[prob.name]] <- data.frame(mids = dist$mids, dens = dist$density)
      
      df.fit <- data.frame(date = frac.year(date[points.above.idx]), prec = prec[points.above.idx])
      
      fit <- nlsLM(prec ~ a*date+b, data = df.fit, start = list(a=1, b=1))
      c <- coef(fit)
      mean <- mean(df.fit$prec)
      mk = mk.test(df.fit$prec) ## Mann-Kendall Trend Fit
      
      # mk.tau = 
      
      fit.coef <- rbind(fit.coef, 
                        data.frame(u = round(prob.u*100), a = c[1], b = c[2], mean, 
                                   mk.tau = mk$estimates[3], mk.pvalue = mk$p.value ))
      
    }
    
    valid.data = length(date)/as.numeric(final.date-start.date)
    
    return(list( df = data.frame(date = frac.year(date), prec = prec),
                 extreme.points = extreme.points, 
                 extreme.dists = extreme.dist, 
                 fit.coef = fit.coef,
                 valid.data = valid.data))
  }
}

precipitation_extremes <- function(df.prec) {
  
  control <- aggregate(year(df.prec$date), list(year = year(df.prec$date)), length)
  
  year.flaws <- control$year[ control$x < 300 ]
  df.prec <- df.prec[!year(df.prec$date) %in% year.flaws,] 
  ## taking off years with less then 300 days of data capture
  
  df.prec <- df.prec[df.prec$prec >= 1,]  ## wet days have precipitation volume > 1mm
  
  ### reference period: 1960-1990
  prec.ref.idx <- which( df.prec$date >= as.Date("1960-01-01") & 
                           df.prec$date < as.Date("1990-01-01") )
  
  if(length(prec.ref.idx) < 10) {
    print("No data in the reference period 1960-1990.")
    return(NULL)
  }
  else {
    prec.quant.year = quantile(df.prec$prec[prec.ref.idx], prob = 0.95)
    prec.extremes <- df.prec[df.prec$prec >= prec.quant.year,]
    group = list(year = year(prec.extremes$date))
    R95P <- aggregate(prec.extremes$prec, by = group, sum)
    occur_R95P <- aggregate(prec.extremes$prec, by = group, length)
    trend_a <- tryCatch(MannKendall(R95P$x), error = function(e) NULL)
    
    prec.quant.year = quantile(df.prec$prec[prec.ref.idx], prob = 0.99)
    prec.extremes <- df.prec[df.prec$prec >= prec.quant.year,]
    group = list(year = year(prec.extremes$date))
    R99P <- aggregate(prec.extremes$prec, by = group, sum)
    occur_R99P <- aggregate(prec.extremes$prec, by = group, length)
    trend_b <- tryCatch(MannKendall(R99P$x), error = function(e) NULL)
    
    group <- list(years = year(df.prec$date))
    PRCPTOT <- aggregate(df.prec$prec, by = group, sum)
    occur_PRCPTOT <- aggregate(group, by = group, length)
    trend_c <- tryCatch(MannKendall(PRCPTOT$x), error = function(e) NULL)
    
    years = unique(year(df.prec$date))
    
    aux.R95P <- aux.R99P <- aux.PRCPTOT <- rep(0, length(years))
    aux.R95P.freq <- aux.R99P.freq <- aux.PRCPTOT.freq <- rep(0, length(years))
    
    aux.R95P[ years %in% R95P$year ] <- R95P$x
    aux.R99P[ years %in% R99P$year ] <- R99P$x
    aux.PRCPTOT[ years %in% PRCPTOT$year ] <- PRCPTOT$x
    
    aux.R95P.freq[ years %in% R95P$year ] <- occur_R95P$x
    aux.R99P.freq[ years %in% R99P$year ] <- occur_R99P$x
    aux.PRCPTOT.freq[ years %in% PRCPTOT$year ] <- occur_PRCPTOT[,2]
    
    
    list = list(
      cumulative = data.frame(year = years, R95P = aux.R95P, R95P.freq = aux.R95P.freq,
                              R99P = aux.R99P, R99P.freq = aux.R99P.freq,
                              PRCPTOT = aux.PRCPTOT, PRCPTOT.freq = aux.PRCPTOT.freq,
                              working.days = control$x[ control$year %in% years ] ),
      trend = list(R95P = trend_a, R99P = trend_b, PRCPTOT = trend_c)
    )
    
    return(list)
  }
}

# here we're trying to reproduce the exact indice that we calculate to temperature i.g. tx90p2d
precipitation_extremes2 <- function(df.prec, cut = 0.9) {
  
  # control <- aggregate(year(df.prec$date), list(year = year(df.prec$date)), length)
  
  # year.flaws <- control$year[ control$x < 300 ]
  # df.prec <- df.prec[!year(df.prec$date) %in% year.flaws,] 
  ## taking off years with less then 300 days of data capture
  
  df.prec <- df.prec[df.prec$prec >= 1,]  ## wet days have precipitation volume > 1mm
  
  ### reference period: 1960-1990
  prec.ref.idx <- which( df.prec$date >= as.Date("1960-01-01") & 
                           df.prec$date < as.Date("1990-01-01") )
  
  # plot(month(df.prec$date[prec.ref.idx]), df.prec$prec[prec.ref.idx])
  
  if(length(prec.ref.idx) < 10) {
    print("No data in the reference period 1960-1990.")
    return(NULL)
  }
  else {
    thresholds <- aggregate( df.prec$prec[prec.ref.idx], 
                             by = list(month = month(df.prec$date[prec.ref.idx])), 
                             function(x){quantile(x, cut)} )
    ext.idx <- NULL
    for(month in thresholds$month) {
      month.idx <- which(month(df.prec$date) == month)
      ext.idx <- c(ext.idx, month.idx[ df.prec$prec[month.idx] > thresholds$x[month] ] )
    }
    
    ext.idx[order(ext.idx)]
  }
}



temperature_extremes <- function(df.tx, df.tn, cut = c(.05,.95),
                               up.down.tx = "+", up.down.tn = "-" ) {
  
  names(df.tx)[2] <- "tmax"
  names(df.tn)[2] <- "tmin"
  
  ### reference period: 1960-1990
  idx.ref.tn <- which(df.tn$date >= as.Date("1960-01-01") & df.tn$date < as.Date("1990-01-01")) 
  idx.ref.tx <- which(df.tx$date >= as.Date("1960-01-01") & df.tx$date < as.Date("1990-01-01")) 
  
  tn.ref.yday <- yday(df.tn$date[idx.ref.tn])
  tx.ref.yday <- yday(df.tx$date[idx.ref.tx])
  
  tn.tot.yday <- yday(df.tn$date)
  tx.tot.yday <- yday(df.tx$date)
  
  if(length(tx.ref.yday) < 10 | length(tn.ref.yday) < 10) {
    print("No data in the reference period 1960-1990.")
    return(NULL)
  }
  else {
    ydays <- 1:366
    aux.yday <- c(ydays,ydays,ydays)
    time.window = 5
    half.window = round(time.window/2)
    
    window.idx <- NULL
    
    p.idx.tx <- p.idx.tn <- NULL
    quant.tx <- quant.tn <- NULL
    
    for(day.of.year in ydays) {
      
      time.window.start = day.of.year-half.window
      time.window.end = day.of.year+half.window
      
      window <- aux.yday[(366+day.of.year-half.window):(366+day.of.year+half.window)]

      window.tn <- which(tn.ref.yday %in% window)
      window.tx <- which(tx.ref.yday %in% window)
      
      idx.tot.tn <- which(tn.tot.yday == day.of.year)
      idx.tot.tx <- which(tx.tot.yday == day.of.year)
      
      quant.tmin = quantile(df.tn$tmin[idx.ref.tn[window.tn]], prob = cut[1], na.rm = T)
      quant.tn <- rbind(quant.tn, cbind(cut[1], day.of.year, quant.tmin))
      
      quant.tmax = quantile(df.tx$tmax[idx.ref.tx[window.tx]], prob = cut[2], na.rm = T)
      quant.tx <- rbind(quant.tx, cbind(cut[2], day.of.year, quant.tmax))
      
      if(up.down.tn == "+") aux <- which( df.tn$tmin[idx.tot.tn] >= quant.tmin )
      if(up.down.tn == "-") aux <- which( df.tn$tmin[idx.tot.tn] <= quant.tmin )
      p.idx.tn <- c(p.idx.tn, idx.tot.tn[ aux ] )
        
      if(up.down.tx == "+") aux <- which( df.tx$tmax[idx.tot.tx] >= quant.tmax )
      if(up.down.tx == "-") aux <- which( df.tx$tmax[idx.tot.tx] <= quant.tmax )
      p.idx.tx <- c(p.idx.tx, idx.tot.tx[ aux ] )
        
      # window.idx <- rbind(window.idx,
                          # data.frame( yday = rep(day.of.year, length(idx)), idx ) )
    }
    
    # for(i in 1:length(p.idx.tx)) p.idx.tx[[i]] <- p.idx.tx[[i]][order(p.idx.tx[[i]])]
    # for(i in 1:length(p.idx.tn)) p.idx.tn[[i]] <- p.idx.tn[[i]][order(p.idx.tn[[i]])]
    
    p.idx.tx <- p.idx.tx[order(p.idx.tx)]
    p.idx.tn <- p.idx.tn[order(p.idx.tn)]
    
    names(p.idx.tx) <- paste0("p")
    names(p.idx.tn) <- paste0("p")
    
    return(list(idx.tx = as.vector(p.idx.tx), idx.tn = as.vector(p.idx.tn), 
                quant.tx = quant.tx, quant.tn = quant.tn, cut = cut))
  }
}

temperature_extremes_consecutive <- function(df, p90, seq.days.n) {
  # df: complete data frame with data.frame(date, temperature)
  # p90 is the index of all temperatures above (or under) some threshold
  
  p90 <- p90[order(p90)]
  
  rle.seq <- rle( as.numeric( diff( df$date[p90] ) ) )
  
  aux.idx <-  which(rle.seq$values == 1 & rle.seq$lengths >= seq.days.n-1)
  
  p90.3d <- NULL
  for(i in aux.idx) {
    event.size = rle.seq$lengths[i]
    
    start.event = sum(rle.seq$lengths[1:i])+1 - event.size
    
    event.times = trunc((event.size+1)/seq.days.n)
    
    for(event in 1:event.times) {
      
      p90.3d <- c(p90.3d, p90[start.event-1+seq.days.n*event] )
    }
  }
  
  p90.3d
}

####### ANALYTICS ####### 
pdf.gev <- function(x, par) { 
  t <- function(x, mu, sigma, xi) { ( 1+ xi*(x-mu)/sigma )^(-1/xi) }
  
  mu = par[1]; sigma = par[2]; xi = par[3]
  1/sigma * t(x,mu,sigma,xi)^(1+xi) * exp(-t(x,mu,sigma,xi)) 
}

cdf.gev <- function(x, par) { 
  mu = par[1]; sigma = par[2]; xi = par[3]
  exp(-(1+xi*(x-mu)/sigma)^(-(1/xi))) 
}

