
###### leitura base S2iD dividida por indices 2013-2022 ######
read_S2id_indices <- function() {
  
  places = dir("../data_base",full.names = T)
  if( length(grep("S2iD_por_indice",places)) == 0 ) { 
    print("Erro, não foi encontrada a base de dados 'S2iD_por_indice'")
    print("Certifique-se de que exista o camiho 'paste de trabalho' > data_base > S2iD_por_indice")
    break 
  }
  if( length(grep("cidade_habitantes_por_estado.csv",places)) == 0 ) { 
    print("Erro, não foi encontrada a base de dados 'cidade_habitantes_por_estado.csv'")
    print("Certifique-se de que exista o camiho 'paste de trabalho' > data_base > cidade_habitantes_por_estado.csv")
    break 
  }
  
  files <- dir("../data_base/S2iD_por_indice", full.names = T)
  info.states <- read.csv("../data_base/cidade_habitantes_por_estado.csv", header = T)
  
  file <- NULL
  events.years <- NULL
  for(path in files) {
    year.file <- read.table(path, header = T, skip = 4, sep = ";", fileEncoding = "latin1")
    
    this.year <- as.numeric( strsplit( strsplit(path, "Informados")[[1]][2], ".csv")[[1]][1] )
    
    events.years <- c(events.years, rep(this.year, nrow(year.file)) )
    
    file <- rbind(file, year.file)
  }
  
  file$COBRADE <- gsub("[0-9][0-9]* - ", "", file$COBRADE)  # tira o número COBRADE, deixa só o Desastre
  
  # file <- file[order(as.Date(file$Registro, format="%d/%m/%Y")),]  # ordena os eventos por data
  # events.years <- as.numeric(format(as.Date(file$Registro, format="%d/%m/%Y"), format="%Y"))
  # extrai apenas o ano da data do evento
  # file[c(4,6,grep("DA",names(file)))] <- NULL                      # retira as colunas de Danos Ambientais
  
  names(file)[names(file) == "COBRADE"] <- "Desastre"     # troca o nome da quarta coluna
  
  return(list(file = file, info.states = info.states, events.years = events.years))
}
##############################################################


###### leitura base S2iD todos os registros 2003-2022 ######
read_S2id_TODOS_OS_REGISTROS <- function() {
  directory = "S2iD_TODOS_OS_REGISTROS"
  
  places = dir("../data_base",full.names = T)
  if( length(grep(directory,places)) == 0 ) { 
    print(paste0("Erro, não foi encontrada a base de dados '",directory,"'"))
    print(paste0("Certifique-se de que exista o caminho 'pasta de trabalho' < .. > data_base > ",directory,""))
    break 
  }
  if( length(grep("cidade_habitantes_por_estado.csv",places)) == 0 ) { 
    print("Erro, não foi encontrada a base de dados 'cidade_habitantes_por_estado.csv'")
    print("Certifique-se de que exista o camiho 'paste de trabalho' > data_base > cidade_habitantes_por_estado.csv")
    break 
  }
  
  files <- dir(paste0("../data_base/",directory), full.names = T, recursive = T)
  info.states <- read.csv("../data_base/cidade_habitantes_por_estado.csv", header = T)
  
  file <- NULL
  for(path in files) {
    aux <- strsplit( strsplit(path,paste0(directory,"/"))[[1]][2], "/")
    year = as.numeric(aux[[1]][1])
    uf = strsplit(aux[[1]][2],".xls" )[[1]][1]
    
    read <- suppressMessages( read_xls(path) )
    
    start = which(read[,1] == "Nº")
    if(length(start) != 0) read <- suppressMessages( read_xls(path, skip = start) )
    
    idx.evento <- grep("Evento", names(read))
    if(length(idx.evento) > 0)  names(read)[idx.evento] <- "Desastre"
    
    # date <- as.Date(na.omit(read$"Data do D.O.U."))
    
    municipio <- na.omit(read$Município)
    idx <- which(grepl("Total",municipio))            # retira as últimas linhas de "nao dados"
    if(length(idx) > 0) municipio <- municipio[-idx]
    
    desastre <- na.omit(read$Desastre)
    idx <- which(grepl("Total",desastre))             # retira as últimas linhas de "nao dados"
    if(length(idx) > 0) desastre <- desastre[-idx]
    
    df <- data.frame(UF = rep(uf, length(desastre)), 
                     municipio = municipio, 
                     desastre = desastre,
                     ano = rep(year, length(desastre)) )
    # data = date,
    # year = format(date, format="%Y") )
    
    file <- rbind(file, df)
  }
  
  return(list(file = file, info.states = info.states))
}
##############################################################


###### infos estações do INMET ######
stations_info_INMET <- function(db.path = "../data_base/inmet_bruto/", cidade = "", estado = "", regiao = "") {
  file.name = "data_table_resume_stations.dat"
  
  places <- dir(db.path)
  if( length(grep(file.name,places)) == 0 ) { 
    print(paste0("Erro, não foi encontrada o arquivo '",file.name,"'"))
    print(paste0("Certifique-se de que exista o camiho 'paste de trabalho' > data_base > inmet >",file.name,""))
    break 
  }
  
  info.station <- read.table(paste0(db.path,file.name), header = T)
  info.station$path <- paste0(db.path,info.station$path)
  
  station.idx <- NULL
  if(cidade != "" & estado == "" & regiao == "") {
    station.idx <- grep(paste(cidade, collapse = "|"), info.station$name, ignore.case = T)
  }
  if(cidade == "" & estado != "" & regiao == "") {
    station.idx <- grep(paste(estado, collapse = "|"), info.station$uf, ignore.case = T)
  }
  if(cidade == "" & estado == "" & regiao != "") {
    station.idx <- grep(paste(regiao, collapse = "|"), info.station$regiao, ignore.case = T)
  }
  if(is.null(station.idx)) station.idx <- 1:length(info.station$name)
  
  if(439 %in% station.idx) station.idx <- station.idx[!(station.idx == 439)]
  
  
  return( list(info.station = info.station, station.idx = station.idx) )
}
##############################################################

###### infos estações do INMET ######
read_INMET_variable <- function(station.path, variable = "prec", 
                                start.date = as.Date("1960-01-01"), final.date = as.Date("2021-01-01")) {
  
  file <- tryCatch(read.csv(station.path, skip = 10, sep = ";"), 
                   error = function(e) NULL)
  var <- NULL
  df = data.frame()
  if(!is.null(file)) {
    ### Verifying if the last column is blank
    last.col = length(file[1,])
    data.size = length(file[,1])
    if( sum(is.na(file[,last.col])) == data.size ) file <- file[,-last.col]
    
    if("prec" %in% variable) var <- file$PRECIPITACAO.TOTAL..DIARIO.mm.
    if("tmax" %in% variable) var <- file$TEMPERATURA.MAXIMA..DIARIA..C.
    if("tmin" %in% variable) var <- file$TEMPERATURA.MINIMA..DIARIA..C.
    if("umid" %in% variable) var <- file$UMIDADE.RELATIVA.DO.AR..MEDIA.DIARIA...
    if("insol" %in% variable) var <- file$INSOLACAO.TOTAL..DIARIO.h.
    if("evap" %in% variable) var <- file$EVAPORACAO.DO.PICHE..DIARIA.mm.
    
    date <- file$Data.Medicao
    
    ### Keeping just valid points
    omit.idx <- which(var == "null" | as.Date(date) > final.date | as.Date(date) < start.date )
    if(length(omit.idx) > 0) {
      var <- var[-omit.idx]
      date <- date[-omit.idx]
    }
    
    var <- as.numeric(var)
    date <- as.Date(date)
    
    df <- data.frame(date, var)
    names(df)[2] <- variable
  }
  
  if(nrow(df) == 0 | is.null(file)) { 
    print(paste("All lines are NULL for",variable))
    return(NULL) 
  }
  else return(df)
  
}
##############################################################


