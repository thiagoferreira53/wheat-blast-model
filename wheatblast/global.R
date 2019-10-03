library(shiny)
library(shinyjs)
library(plyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(lubridate)
library(jsonlite)
library(dplyr)
library(tidyr)
library(jsonlite)
library(scales)
library(gridExtra)
library(leaflet)
library(shinythemes)
library(shinydashboard)
library(leaflet.extras)
library(magrittr)
library(DT)
library(RColorBrewer)
library(darksky)
library(weathermetrics)
library(shinyWidgets)
library(shinyalert)
library(shinycssloaders)

#library(leaflet.minicharts)
#library(manipulateWidget)

jsCode <- "shinyjs.pageCol = function(params){
var defaultParams = {
id : null,
col : 'red'
};
params = shinyjs.getParams(params, defaultParams);
$('body').css('background', params.col);
console.log(params.col); 
Shiny.onInputChange('var', 'teste');
}"

jsSaveCache <-"shinyjs.saveCache = function(params){
  var defaultParams = {
    name : null,
    lat : null,
    long : null,
    color : 'blue'
  }

  params = shinyjs.getParams(params, defaultParams);

  if(localStorage.getItem('cimmytName')){
    localStorage.setItem('cimmytName',localStorage.getItem('cimmytName')+';'+params.name);
  }else{
    localStorage.setItem('cimmytName',params.name);
  }

  if(localStorage.getItem('cimmytlat')){
    localStorage.setItem('cimmytlat',localStorage.getItem('cimmytlat')+';'+params.lat);
  }else{
    localStorage.setItem('cimmytlat',params.lat);
  }
  if(localStorage.getItem('cimmytlong')){
    localStorage.setItem('cimmytlong',localStorage.getItem('cimmytlong')+';'+params.long);
  }else{
    localStorage.setItem('cimmytlong',params.long);
  }  
  if(localStorage.getItem('cimmytcolor')){
    localStorage.setItem('cimmytcolor',localStorage.getItem('cimmytcolor')+';'+params.color);
  }else{
    localStorage.setItem('cimmytcolor',params.color);
  } 

}
"
jsGetCache <-"shinyjs.getCache = function(params){
  Shiny.onInputChange('cimmytName',localStorage.getItem('cimmytName'));
  Shiny.onInputChange('cimmytlat', localStorage.getItem('cimmytlat'));
  Shiny.onInputChange('cimmytlong', localStorage.getItem('cimmytlong'));
  Shiny.onInputChange('cimmytcolor', localStorage.getItem('cimmytcolor'));
}
"

locales <- c(
"Brazil" = "BR",
"Bangladesh" = "BD"
)

startingDateMap <- Sys.Date()
load("/home/csisa_bihar/dados/df.rda")

urlSpore<-"/home/csisa_bihar/sporeCloud"

estado <-"DF"
pais   <-"BR"
inicio <-Sys.Date()
coichesPais<-c("BR","BD")
bdStates<-c("BL","BS","CD","DJ","FR","JS","MD","MH","PT","RS")
names(coichesPais)<-c("Brazil","Bangladesh")
globalCountry <-"BR"
globalState <- "DF"

choicesEstados<-c("DF","GO","MG","MT",
"MS","PR","RS","SC","SP")
names(choicesEstados)<-c("Distrito Federal",
"Goiais","Minas Gerais","Mato Grosso",
"Mato Grosso do Sul","Paraná","Rio Grande do Sul",
"Santa Catarina",
"Sã Paulo")


choicesEstadosMapsHeat<-c("DF","GO","MG","MT",
                          "MS","PR","RS","SC","SP")
names(choicesEstadosMapsHeat)<-c("Distrito Federal",
                                 "Goiais","Minas Gerais","Mato Grosso",
                                 "Mato Grosso do Sul","Paraná","Rio Grande do Sul",
                                 "Santa Catarina",
                                 "São Paulo")


choicesRisk <-c("wheatblast","precipitation","temperature","humidity")
names(choicesRisk)<-c("Wheat blast","Precipitation","Temperature","Humidity")

estacoes<-fromJSON("http://dev.sisalert.com.br/apirest/api/v1/stations/BR/DF")

choicesEstacoes<-estacoes$`_id`

names(choicesEstacoes) <- estacoes$name

choicesEstados<-sort(choicesEstados)

startingDate <- Sys.Date()

calc_dif <- function(temp,rh) {
  ifelse(temp >= 15 && temp < 27 && rh > 93, 14.35-0.25*temp,ifelse(temp >= 27 && temp < 35 && rh > 93,-8.5+0.59*temp,0))
}

loadEstacoes<-function(pais,estado,org){
    if(is.null(estado))
    estado = "DF"
    url<-paste("http://dev.sisalert.com.br/apirest/api/v1/stations/",pais,"/",estado,sep = '')
    estacoes<-fromJSON(url)
    if(org !="BMD"){
      if(estado == "RS"){
        estacoes<-subset(estacoes,estacoes$organization$abbr!="AGAPOMI")
        estacoes<-subset(estacoes,estacoes$organization$abbr!="BASF")
        
      }else{
        estacoes<-subset(estacoes,estacoes$organization$abbr==org)
      }
    }
    estacoes <-estacoes[with(estacoes, order(name)), ]
    est<-estacoes$`_id`
    names(est) <- estacoes$name
    return(est)
}
loadDataBDS<-function(codStation,dateInterface) {
    
    dateinicio<-dateInterface-90
    dataFim <-dateInterface+30
    if(dateinicio < as.Date("2014-01-01")){
        dateinicio<-"2014-01-01"
    }
    if(dataFim > as.Date("2016-12-31"))
    dataFim <- "2016-12-31"
}

loadData<-function(codStation,dateInterface) {
    inicialDate<-format(dateInterface-90, "%m-%d-%Y")
    endDate<-format(dateInterface+30, "%m-%d-%Y")
    url<-paste("http://dev.sisalert.com.br/apirest/api/v1/data/station/",codStation,"/range/",
    as.character(inicialDate),"/",as.character(endDate),sep='')

    dadosJson <- fromJSON(url)
    
    weatherStation <-dadosJson$weatherStation
    dadosJson <- dadosJson$data
    dadosJson<-dadosJson[!rev(duplicated(rev(dadosJson$datetime))),]
    tryCatch({
      dadosJson$date<-as.Date(dadosJson$datetime, format = "%m-%d-%Y")
    },error = function(e) {
      shinyalert("", "Something went wrong. \nNo data available!", type = "error")
    }
    )
    
    dadosJson$date<-format(dadosJson$date, "%Y-%m-%d")
    
    dadosJson$time<-format(as.POSIXct(strptime(dadosJson$datetime,"%m-%d-%Y %H:%M",tz="")) ,
    format = "%H:%M:%S")
    if(is.null(dadosJson$data$windS))
    dadosJson$data$windS<-0
    if(is.null(dadosJson$data$avgDP))
    dadosJson$data$avgDP<-0
    
    dados <-data.frame(date=dadosJson$date,time=dadosJson$time,
    temp =dadosJson$data$avgT, rh = dadosJson$data$avgH,
    wind = dadosJson$data$windS,srad = dadosJson$data$solarR,
    rain = dadosJson$data$totR,porvalho = dadosJson$data$avgDP)
    lista<-list(dados=dados , weatherStation=weatherStation)
    return(lista)
}

loadDataRh90<-function(codStation,dateInterface) {
  id<-format(dateInterface-90, "%m-%d-%Y")
  ed<-format(dateInterface+30, "%m-%d-%Y")
  url<-paste("http://dev.sisalert.com.br/apirest/api/v1/data/station/",codStation,"/range/",
             id,"/",ed,sep='')

  dadosJson <- fromJSON(url)
  
  weatherStation <-dadosJson$weatherStation
  dadosJson <- dadosJson$data
  
  dadosJson<-dadosJson[!rev(duplicated(rev(dadosJson$datetime))),]
  tryCatch({
    dadosJson$date<-as.Date(dadosJson$datetime, format = "%m-%d-%Y")
  },error = function(e) {
    shinyalert("", "Something went wrong. \nNo data available!", type = "error")
  }
  )
  
  dadosJson$date<-format(dadosJson$date, "%Y-%m-%d")
  
  dadosJson$time<-format(as.POSIXct(strptime(dadosJson$datetime,"%m-%d-%Y %H:%M",tz="")) ,
                         format = "%H:%M:%S")
  if(is.null(dadosJson$data$windS))
    dadosJson$data$windS<-0
  if(is.null(dadosJson$data$avgDP))
    dadosJson$data$avgDP<-0
  dados <-data.frame(date=dadosJson$date,time=dadosJson$time,
                     temp =dadosJson$data$avgT, rh = dadosJson$data$avgH,
                     wind = dadosJson$data$windS,srad = dadosJson$data$solarR,
                     rain = dadosJson$data$totR,porvalho = dadosJson$data$avgDP)
  dados$rh90 <- ifelse(dados$rh>=90,1,0)
  dados$srad <-round(dados$srad/277.7777,1)
  dados$temp <-round(dados$temp,1)
  dados$rh<-round(dados$rh,1)
  dados$wind<-round(dados$wind,1)
  dados$rain<-round(dados$rain,1)
  dados$porvalho<-round(dados$porvalho,1)
  #dados$srad <-dados$srad *0.001
  dados$datetime <- paste(dados$date,dados$time)
  wall <<- cbind(datetime2=dados$datetime, dados) %>%
    mutate(date=as.Date(strptime(as.character(datetime2), format="%Y-%m-%d %H:%M:%S"))) %>%
    group_by(date) %>% summarise(srad=sum(srad),
                                 tmax = max(temp),
                                 tmin = min(temp),
                                 rain=sum(rain),
                                 rh90 = sum(rh90)
    )
  
  
  wall$date<-format(wall$date, '%Y%j')
  wall$date<-as.integer(wall$date)
  return(wall)
}

hcp<- function(temp,rh) { # verificar
   # #print(paste0(temp," - ",rh))
    if(temp >= 15 && temp < 27 && rh > 93){
      return(1/(14.35-0.25*temp))
    }else{
      if(temp >= 27 && temp < 35 && rh > 93){
        return(1/(-8.5+0.59*temp))
      }else{
        return(0)
      }
    }
}
diasFavInfe2<-function(tMax,rhMax,rain){
    ifelse(rhMax >=93,ifelse(tMax>23,ifelse(rain<=5,1,0),0),0)
    
}
diasFavInfe<-function(tempMax,tempMin,rh){
    difTemp= tempMax-tempMin
    ifelse(tempMax>=23,ifelse(difTemp >=13,ifelse(rh>=70,1,0),0),0)
}

calcAndGraph <- function(codStation,dateInterface,pais){

    if(pais == "BD")
      chcpp <- 40
    else
      chcpp <-25
    lista <-loadData(codStation,dateInterface)
    dados<-lista$dados
    dados$hcp <- round(mapply('hcp', dados$temp, dados$rh), 3)
    
    dados$datetime <- paste(dados$date,dados$time)
    wall <<- cbind(datetime2=dados$datetime, dados,  dif = mapply('calc_dif', dados$temp, dados$rh)) %>%
    mutate(dd=as.Date(strptime(as.character(datetime2), format="%Y-%m-%d %H:%M:%S")),
    dif = ifelse(dif > 0, 1/dif, dif),
    doy = as.numeric(strftime(dd, format="%j"))) %>%
    group_by(dd) %>% summarise(  maxTemp = max(temp),
    minTemp = min(temp),
    maxUr = max(rh),
    temp=mean(temp),
    chuva=sum(as.double(rain)),
    ur=mean(rh),
    doy = first(doy),
    s_dif = sum(dif),
    hcp = sum(hcp))
    wall$diaFavInf<-diasFavInfe(wall$maxTemp,wall$minTemp,wall$ur)
    
    w1516 <<- wall %>% mutate(yr = year(dd),
    leap = leap_year(dd)) %>%
    group_by(yr) %>%
    ## mudei de 92,91 para pegar os mesmos dias do ano
    #filter(between(doy, ifelse(first(leap), 02,01), ifelse(first(leap), 366, 365))) %>%
    mutate(acc_dif=cumsum(s_dif)) %>%
    ungroup %>% select(dd:acc_dif)
    
    roll_dif = function(dif_vector, dif_new, id, discount_factor = NULL) {
        if(is.null(discount_factor)) discount_factor = seq(1, by = -0.1, length.out = 7)
        nmax = length(discount_factor)
        idx = seq(from=(id-1), by=-1, length.out = nmax-1)
        idx = idx[idx > 0]
        if(id == 1) {
            return(dif_vector[id])
        } else if(id > 1  && id <= nmax) {
            y = matrix(discount_factor[1:id], ncol = 1)
        } else {
            y = matrix(discount_factor, ncol = 1)
        }
        x = c(dif_vector[id],dif_new[idx])
        temp = as.numeric(x %*% y) / nmax
        return(temp)
    }
    
    #
    # Fator de desconto linear 7 dias, step = 0.10
    ddisc7 = seq(1, by = -0.1, length.out = 7)
    
    # Fator de desconto linear 3 dias, step = 0.25
    ddisc3 <- seq(1, by=-0.25,length.out = 3)
    
    
    dif16 = w1516$s_dif
    dif_new16_lag10 = vector(mode = mode(dif16), length = length(dif16))
    dif_new16_lag3 = vector(mode = mode(dif16), length = length(dif16))
    
    sapply(1:length(dif16), function(x) {
        dif_new16_lag10[x] <<- roll_dif(dif16, dif_new16_lag10, x, discount_factor = ddisc7)
        dif_new16_lag3[x] <<- roll_dif(dif16, dif_new16_lag3, x, discount_factor = ddisc3)
    })
    
    w1516[['dif_adj_lag10']] = c(dif_new16_lag10)
    w1516[['dif_adj_lag3']] = c( dif_new16_lag3)
    
    rm(list = ls(pattern = '^dif'))
    
    w1516 = w1516 %>%
    group_by(yr) %>%
    mutate(acc_dif_adj_lag10 = cumsum(dif_adj_lag10),
    acc_dif_adj_lag3 = cumsum(dif_adj_lag3))
    dados$hcp <- round(mapply('hcp', dados$temp, dados$rh), 3)
    
    dados$dd<-as.Date(paste(dados$date,sep=''),"%Y-%m-%d")
    # Calculate daily mean TInsterature and acculuted hcp in a given day
    days<-ddply(dados,.(dd),summarise,tempMax =max(temp),tempMin = min(temp),
    maxUr=max(rh),minUr=min(rh),temp=mean(temp),chuva=sum(as.double(rain)),
    ur=mean(rh),hcp=sum(hcp))
    
    days$dif_adj_lag3 <- w1516$dif_adj_lag3
    
    days$diaFavInf<-diasFavInfe2(days$tempMax,days$maxUr,days$chuva)
    days$chcp<-cumsum(days$hcp)
    days<-days[order(days$dd),]
    days$tempMax<-NULL
    days$tempMin<-NULL
    days$hcp<-NULL
    days$minUr <-NULL
    days$maxUr<-NULL
    #days$dif_lag10 <- w1516$dif_adj_lag10
    #days$acc_dif <- w1516$acc_dif
    
    teste <- melt(days, id.vars=c("dd"))

    annDate = teste %>%
    filter(variable %in% c('diaFavInf', 'chcp','dif_adj_lag3')) %>%
    spread(variable, value) %>%
    filter(chcp >= chcpp, diaFavInf == 1,dif_adj_lag3>=0.2) %>%
    select(dd) %>% as.data.frame
    
    dGraph = teste %>% filter(variable != 'diaFavInf')
    
    varNames = c(
    temp = "Temperature [C]",
    chuva = "Rainfall [mm]",
    ur = "Relative Humidity (%)",
    chcp = "Inoculum Potential",
    dif_adj_lag3 = "Spores"
    )
    
    p<-ggplot(dGraph,
    aes(x = dd, y = value, ymin = 0, ymax = value)) +
    facet_grid(variable~., scales = "free",
    as.table = FALSE,
    labeller = labeller( variable = varNames, .multi_line = TRUE )) +
    geom_step(data = teste %>%
    filter(variable %in% c( 'chcp'))) +
    geom_bar(data = teste %>%
    filter(variable %in% c('chuva')),stat= "identity") +
    geom_line(data = teste %>%
    filter(variable %in% c('ur', 'temp', 'dif_adj_lag3'))) +
    theme_bw() +theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    xlab("Date") + ylab("")+
    scale_x_date(date_breaks = "2 day",labels = date_format("%d-%b"),position="bottom")+
    ggtitle(lista$weatherStation$name)
    aux <- p
    
    p = if(nrow(annDate) > 0) {
        p + annotate("rect", xmin=annDate$dd, xmax = annDate$dd+1,
        ymin=-Inf, ymax=Inf, alpha=0.2, fill="red")
    }
    
    if(is.null(p)){
        p<-aux
    }
    plot(p)
}

sporeCloud <- function(codStation,dateInterface,pais){


  dados <- loadDataRh90(codStation,dateInterface)
  wd <-getwd()
  setwd(urlSpore)
  #setwd("~/Documents/teste")
  #setwd("/opt/projects/development/riskMonitoringData/sporeCloud2")

  size <- length(dados$date)
  json <- toJSON(dados)
  #write(json, "dado.json")
  arf <- 100000
  date <- as.integer(min(dados$date))
  AREALEAF <<-10000

  print(as.integer(date))
  print(as.integer(date))
  print(as.integer(size))
  print(as.integer(arf))
  pst<- paste0('[{"PESTID#":"WH001","PSTNAME":"Wheat Blast","DSPL":100,"SPE":[6,15,22,30],"SCF":[0.98669,10.71894,0.93374],"MSCD":15000,"ASR":0.044,"SPO2P":0.25,"SPP2F":0.45,"CCFPO":[3,6,9],"II":100,"AFII":5,"TFS":[30,15,28],"IE":0.08,"DF":0.15,"IPS":0.001,"LP":7,"IP":21,"WT":6,"HF":0.7,"IGF":"0.01+(x*0.2/9)","VGF":"0.21+((x-9)*0.1/21)"}]')
  write(json,"dataJson.json")
  write(pst,"pst.json")
  #dyn.load("teste2.so")
  cmd <- paste("./teste",as.integer(date),as.integer(date),size)
  print(cmd)
  print(cmd)
  system(cmd, intern = TRUE, show.output.on.console = FALSE)
  
  cloudF <-read.table("spore.out",head= T)
  
  cloudF[cloudF<0 ] <-NA
  cloudF <-na.omit(cloudF)
  cf <-data.frame(sporeCloud = cloudF)
  setwd(wd)
  return(cf)
}

calcAndGraph2 <- function(codStation,dateInterface,pais){

  print("PAIS!!!")
  print(pais)
  if(pais == "BD")
    chcpp <- 40
  else
    chcpp <-25
  lista <-loadData(codStation,dateInterface)
  dados<-lista$dados
  cloudF<-sporeCloud(codStation,dateInterface)
  cloudF <- round(cloudF,3)
  #print(cloudF)
  dados$hcp <- round(mapply('hcp', dados$temp, dados$rh), 3)
  dados$datetime <- paste(dados$date,dados$time)
  wall <<- cbind(datetime2=dados$datetime, dados,  dif = mapply('calc_dif', dados$temp, dados$rh)) %>%
    mutate(dd=as.Date(strptime(as.character(datetime2), format="%Y-%m-%d %H:%M:%S")),
           dif = ifelse(dif > 0, 1/dif, dif),
           doy = as.numeric(strftime(dd, format="%j"))) %>%
    group_by(dd) %>% summarise(  maxTemp = max(temp),
                                 minTemp = min(temp),
                                 maxUr = max(rh),
                                 temp=mean(temp),
                                 chuva=sum(as.double(rain)),
                                 ur=mean(rh),
                                 doy = first(doy),
                                 s_dif = sum(dif),
                                 hcp = sum(hcp))
  wall$diaFavInf<-diasFavInfe(wall$maxTemp,wall$minTemp,wall$ur)
  
  w1516 <<- wall %>% mutate(yr = year(dd),
                            leap = leap_year(dd)) %>%
    group_by(yr) %>%
    ## mudei de 92,91 para pegar os mesmos dias do ano
    #filter(between(doy, ifelse(first(leap), 02,01), ifelse(first(leap), 366, 365))) %>%
    mutate(acc_dif=cumsum(s_dif)) %>%
    ungroup %>% select(dd:acc_dif)
  
  roll_dif = function(dif_vector, dif_new, id, discount_factor = NULL) {
    if(is.null(discount_factor)) discount_factor = seq(1, by = -0.1, length.out = 7)
    nmax = length(discount_factor)
    idx = seq(from=(id-1), by=-1, length.out = nmax-1)
    idx = idx[idx > 0]
    if(id == 1) {
      return(dif_vector[id])
    } else if(id > 1  && id <= nmax) {
      y = matrix(discount_factor[1:id], ncol = 1)
    } else {
      y = matrix(discount_factor, ncol = 1)
    }
    x = c(dif_vector[id],dif_new[idx])
    temp = as.numeric(x %*% y) / nmax
    return(temp)
  }
  
  #
  # Fator de desconto linear 7 dias, step = 0.10
  ddisc7 = seq(1, by = -0.1, length.out = 7)
  
  # Fator de desconto linear 3 dias, step = 0.25
  ddisc3 <- seq(1, by=-0.25,length.out = 3)
  
  
  dif16 = w1516$s_dif
  dif_new16_lag10 = vector(mode = mode(dif16), length = length(dif16))
  dif_new16_lag3 = vector(mode = mode(dif16), length = length(dif16))
  
  sapply(1:length(dif16), function(x) {
    dif_new16_lag10[x] <<- roll_dif(dif16, dif_new16_lag10, x, discount_factor = ddisc7)
    dif_new16_lag3[x] <<- roll_dif(dif16, dif_new16_lag3, x, discount_factor = ddisc3)
  })
  
  w1516[['dif_adj_lag10']] = c(dif_new16_lag10)
  w1516[['dif_adj_lag3']] = c( dif_new16_lag3)
  
  rm(list = ls(pattern = '^dif'))
  
  w1516 = w1516 %>%
    group_by(yr) %>%
    mutate(acc_dif_adj_lag10 = cumsum(dif_adj_lag10),
           acc_dif_adj_lag3 = cumsum(dif_adj_lag3))
  dados$hcp <- round(mapply('hcp', dados$temp, dados$rh), 3)
  
  dados$dd<-as.Date(paste(dados$date,sep=''),"%Y-%m-%d")
  # Calculate daily mean TInsterature and acculuted hcp in a given day
  days<-ddply(dados,.(dd),summarise,tempMax =max(temp),tempMin = min(temp),
              maxUr=max(rh),minUr=min(rh),temp=mean(temp),chuva=sum(as.double(rain)),
              ur=mean(rh),hcp=sum(hcp))

  days$dif_adj_lag3 <- w1516$dif_adj_lag3
  print("TESTE DE TAMANHOA")
  print(days)
  print(length(days$dif_adj_lag3))
  print(length(cloudF$sporeCloud))
  days$cloudF <- cloudF$sporeCloud
  #days$cloudF <- 0
  
  print(days)
  
  days$diaFavInf<-diasFavInfe2(days$tempMax,days$maxUr,days$chuva)
  days$chcp<-cumsum(days$hcp)
  days<-days[order(days$dd),]
  days$tempMax<-NULL
  days$tempMin<-NULL
  days$hcp<-NULL
  days$minUr <-NULL
  days$maxUr<-NULL
  #days$dif_lag10 <- w1516$dif_adj_lag10
  #days$acc_dif <- w1516$acc_dif
  
  teste <- melt(days, id.vars=c("dd"))
  print("TESTE")
  print(teste)
  annDate = teste %>%
    filter(variable %in% c('diaFavInf', 'chcp','dif_adj_lag3')) %>%
    spread(variable, value) %>%
    filter(chcp >= chcpp, diaFavInf == 1,dif_adj_lag3>=0.2) %>%
    select(dd) %>% as.data.frame
  
  dGraph = teste %>% filter(variable != 'diaFavInf')
  
  varNames = c(
    temp = "Temperature [C]",
    chuva = "Rainfall [mm]",
    ur = "Relative Humidity (%)",
    chcp = "Inoculum Potential",
    dif_adj_lag3 = "Spores"
  )
  if(!empty(annDate)){

    aux2 <- annDate$dd
    aux <- teste %>%
      filter(variable %in% c( 'chcp'))
    line_list1 <- list()
    
    k<-1

    for(k in 1:length(aux2)){
     
      line_list1[[k]] <- 
        list(type = "rect",
             fillcolor = "red", line = list(color = "red",opacity = 0.0), opacity = 0.2,
             x0 = aux2[k], x1 = aux2[k]+1, xref = "x",
             y0 = 0, y1 = (max(aux$value)*8), yref = "y")
    }

    aux <- teste %>%
      filter(variable %in% c( 'dif_adj_lag3'))
    line_list2 <- list()
    k<-1
    for(k in 1:length(aux2)){
      #print(k)
      line_list2[[k]] <- 
        list(type = "rect",
             fillcolor = "red", line = list(color = "red",opacity = 0.0), opacity = 0.2,
             x0 = aux2[k], x1 = aux2[k]+1, xref = "x2",
             y0 = -5.3, y1 = (max(aux$value)+2), yref = "y2")
    }
    aux <- teste %>%
      filter(variable %in% c( 'ur'))
    line_list3 <- list()
    k<-1
    for(k in 1:length(aux2)){
     
      line_list3[[k]] <- 
        list(type = "rect",
             fillcolor = "red", line = list(color = "red",opacity = 0.0), opacity = 0.2,
             x0 = aux2[k], x1 = aux2[k]+1, xref = "x4",
             y0 = 0, y1 = (max(aux$value)*7)+100, yref = "y4")
    }
    aux <- teste %>%
      filter(variable %in% c( 'chuva'))
    line_list4 <- list()
    k<-1
    for(k in 1:length(aux2)){
      #print(k)
      line_list4[[k]] <- 
        list(type = "rect",
             fillcolor = "red", line = list(color = "red",opacity = 0.0), opacity = 0.2,
             x0 = aux2[k], x1 = aux2[k]+1, xref = "x5",
             y0 = 0, y1 = (max(aux$value)*17), yref = "y5")
    }
    
    aux <- teste %>%
      filter(variable %in% c( 'temp'))
    line_list5 <- list()
    k<-1
    for(k in 1:length(aux2)){
      ##print(k)
      line_list5[[k]] <- 
        list(type = "rect",
             fillcolor = "red", line = list(color = "red",opacity = 0.0), opacity = 0.2,
             x0 = aux2[k], x1 = aux2[k]+1, xref = "x6",
             y0 = 0, y1 = (max(aux$value)*10), yref = "y6")
    }
    print("AAAAAAAAAAAAAAAAAAAA")
    aux <- teste %>%
      filter(variable %in% c( 'cloudF'))
    line_list6 <- list()
    print("AUXXXX")
    print(aux)
    k<-1
    for(k in 1:length(aux2)){
      ##print(k)
      line_list6[[k]] <- 
        list(type = "rect",
             fillcolor = "red", line = list(color = "red",opacity = 0.0), opacity = 0.2,
             x0 = aux2[k], x1 = aux2[k]+1, xref = "x3",
             y0 = 1, y1 = ((max(aux$value)+1)*9), yref = "y3")
    }
    
    
    
    p1 <- teste %>%
      filter(variable %in% c( 'chcp')) %>%
      plot_ly(x=~dd, y=~value,
              type = 'scatter',
              color = I("black"),
              name = 'Inoculum Potential',
              mode="line",
              hoverinfo = 'text',
              text = ~paste('Date:', dd,
                            '<br>Inoculum Potential:',value))%>%
      layout(
        yaxis = list (title = "Inoculum Potential"), shapes = line_list1,
        xaxis = list(title = ""))
    p1  
    
    p2 <- teste %>%
      filter(variable %in% c( 'dif_adj_lag3')) %>%
      plot_ly(x=~dd, y=~value,
              color = I("green"),
              type="scatter", mode="lines",
              hoverinfo = 'text',
              name = "Spores",
              text = ~paste('Date:', dd,
                            '<br>Spores:',round(value,2)))%>%
      layout(
        yaxis = list (title = "Spores"), shapes = line_list2,
        xaxis = list(title = ""))
    p2
    
    
    
    p3 <- teste %>%
      filter(variable %in% c( 'ur')) %>%
      plot_ly(x=~dd, y=~value, 
              type="scatter", mode="lines",
              hoverinfo = 'text',
              name = "Relative Humidity",
              color = I("purple"),
              text = ~paste('Date:', dd,
                            '<br>Relative Humidity:',round(value,2),'%'))%>%
      layout(
        yaxis = list (title = "Humidity(%)"), shapes = line_list3,
        xaxis = list(title = ""))
    p3
    
    
    p4 <- teste %>%
      filter(variable %in% c( 'chuva')) %>%
      plot_ly(x=~dd, y=~value, 
              type="bar", mode="bar",color = I("blue"),
              name = "Rain",hoverinfo = 'text',
              text = ~paste('Date:', dd,
                            '<br>Rain:',round(value,2),"mm"))%>%
      layout(
        yaxis = list (title = "Rain(mm)"),shapes = line_list4,
        xaxis = list(title = ""))
    p4
    
    
    p5 <- teste %>%
      filter(variable %in% c( 'temp')) %>%
      plot_ly(x=~dd, y=~value, 
              type="scatter", mode="lines",
              color = I("orange"),
              name = "Temperature",
              hoverinfo = 'text',
              text = ~paste('Date:', dd,
                            '<br>Temperature:',round(value,2)," C"))%>%
      layout(
        yaxis = list (title = "Temperature(C)"), shapes = line_list5,  yaxis = list(range = 100),
        xaxis = list(title = "<br>Date"))
    p5
    
    p6 <- teste %>%
      filter(variable %in% c( 'cloudF')) %>%
      plot_ly(x=~dd, y=~value,
              type = 'scatter',
              color = I("pink"),
              name = 'Inoculum Potential',
              mode="line",
              hoverinfo = 'text',
              text = ~paste('Date:', dd,
                            '<br>Spore Cloud:',value))%>%
      layout(
        yaxis = list (title = "Spore Cloud"), shapes = line_list6,
        xaxis = list(title = ""))
    p6  
    
    subplot(p1,
                  p2,
                  p6,
                  p3,
                  p4,
                  p5,
                  nrows = 6,titleX = T,titleY =TRUE,shareX= FALSE,shareY=F, which_layout =T)%>%
      layout(
        showlegend = FALSE
      ) 
  }else{
    p1 <- teste %>%
      filter(variable %in% c( 'chcp')) %>%
      plot_ly(x=~dd, y=~value,
              type = 'scatter',
              color = I("black"),
              name = "Inoculum Potential",
              mode="line",
              hoverinfo = 'text',
              text = ~paste('Date:', dd,
                            '<br>Inoculum Potential:',value))%>%
      layout(
        yaxis = list (title = "Inoculum Potential"),
        xaxis = list(title = ""))
    p1  
    
    p2 <- teste %>%
      filter(variable %in% c( 'dif_adj_lag3')) %>%
      plot_ly(x=~dd, y=~value,
              color = I("green"),
              type="scatter", mode="lines",
              name = "Spores",
              hoverinfo = 'text',
              text = ~paste('Date:', dd,
                            '<br>Spores:',round(value,2)))%>%
      layout(
        yaxis = list (title = "Spores"),
        xaxis = list(title = ""))
    p2
    
    
    
    p3 <- teste %>%
      filter(variable %in% c( 'ur')) %>%
      plot_ly(x=~dd, y=~value, 
              type="scatter", mode="lines",
              hoverinfo = 'text',
              name = "Relative Humidity",
              color = I("purple"),
              text = ~paste('Date:', dd,
                            '<br>Relative Humidity:',round(value,2),'%'))%>%
      layout(
        yaxis = list (title = "Humidity(%)"),
        xaxis = list(title = ""))
    p3
    
    
    p4 <- teste %>%
      filter(variable %in% c( 'chuva')) %>%
      plot_ly(x=~dd, y=~value, 
              type="bar", mode="bar", color = I("blue"),
              hoverinfo = 'text',
              name = "Rain",
              text = ~paste('Date:', dd,
                            '<br>Rain:',round(value,2),"mm"))%>%
      layout(
        yaxis = list (title = "Rain(mm)"),
        xaxis = list(title = ""))
    p4
    
    p5 <- teste %>%
      filter(variable %in% c( 'temp')) %>%
      plot_ly(x=~dd, y=~value, 
              type="scatter", mode="lines",
              color = I("orange"),
              name = "Temperature",
              hoverinfo = 'text',
              text = ~paste('Date:', dd,
                            '<br>Temperature:',round(value,2)," C"))%>%
      layout(
        yaxis = list (title = "Temperature(C)"),
        xaxis = list(title = "<br>Date"))
    p5
    p6 <- teste %>%
      filter(variable %in% c( 'cloudF')) %>%
      plot_ly(x=~dd, y=~value,
              type = 'scatter',
              color = I("pink"),
              name = "Spore Cloud",
              mode="line",
              hoverinfo = 'text',
              text = ~paste('Date:', dd,
                            '<br>Spore Cloud:',value))%>%
      layout(
        yaxis = list (title = "Spore Cloud"),
        xaxis = list(title = ""))
    p6 

    
    subplot(p1,
            p2,
            p6,
            p3,
            p4,
            p5,
            nrows = 6,titleX = FALSE,titleY =TRUE,shareX= TRUE,shareY=F)%>%
      layout(
        xaxis = list(title = "<br>Date"),
        showlegend = FALSE
      )
    
  }
  
  
}



graphClim <-function(codStation,dateInterface,pais){
    lista <-loadData(codStation,dateInterface)
    dados<-lista$dados
    
    daily<-ddply(dados,.(date),summarise,tmax=max(temp),tmin=min(temp),rhmin=min(rh),rhmax=max(rh),rain=sum(as.double(rain)),srad=sum(srad))
    #daily<-graphClim2(codigo_station,dateInterface,pais)
    daily$tmax
    daily$date <-as.Date(daily$date)
    tmax <-daily$tmax
    tmin <-daily$tmin
    rhmax <-daily$rhmax
    rhmin <-daily$rhmin
    rain <- daily$rain
    dd <-daily$date
    
    p <- plot_ly(daily, x = ~date, y = ~tmax, type = 'scatter',color = I("red"),hoverinfo = 'text',
                 mode = 'lines',name ="Max. Temp",text = paste("Date: ",dd,"<br>Max. temperature: ", tmax,"C")) %>%
      add_trace(y = ~tmin, name = 'Min. Temp', color = I("blue"),text = paste("Date: ",dd,"<br>Min. temperature: ", tmin,"C"))%>%
      layout(
        yaxis = list (title = "Temperature(C)"),
        xaxis = list(title = ""))
    
    p2 <- plot_ly(daily, x = ~date, y = ~rhmax, type = 'scatter',color = I("purple"),hoverinfo = 'text',
                  mode = 'lines',name ="Max. RH",text = paste("Date: ",dd,"<br>Max. Relative Humidty: ", rhmax,"%")) %>%
      add_trace(y = ~rhmin, name = 'Min. RH', mode = 'lines', color = I("pink"),text = paste("Date: ",dd,"<br>Min. Relative Humidty: ", rhmin,"%"))%>%
      layout(
        yaxis = list (title = "Humidity(%)"),
        xaxis = list(title = "")
        )
    
    p3 <- plot_ly(daily,
                  x = ~date,
                  y = ~rain,
                  name = "Rain",
                  hoverinfo = 'text',
                  text = paste("Date: ",dd,"<br>Rain: ", rain," mm"),
                  color = I("blue"),
                  type = "bar")%>%
      layout(
        yaxis = list (title = "Rain(mm)"),
        xaxis = list(title = "<br>Date"))
    
    subplot(p, p2,p3,nrows = 3,titleX = T,titleY =TRUE,shareX= F)
}


loadEstacoesMap<-function(locale,estado){
    if(locale == "BR" && !is.element(estado,choicesEstados)){
        estado <-"DF"
    }else{
        if(locale == "BD" && !is.element(estado,bdStates)){
            estado <-"BL"
        }
    }
    url<-paste("http://dev.sisalert.com.br/apirest/api/v1/stations/",locale,"/",estado,sep = '')
    ##print("URLLL ESTACAOOO!!!!")
    ##print(url)
    estacoes<-fromJSON(url)
    if(locale == "BR"){
        estacoes<-subset(estacoes,estacoes$organization$abbr=="INMET")
        
    }
    estacoes <-estacoes[with(estacoes, order(name)), ]
    ##print(estacoes)
    return(estacoes)
}


calculaIp<-function(codStation,dateInterfaceMap) {
    dados<-NULL
    dadosJson<-NULL
    teste<-dateInterfaceMap
    dateInterfaceMap<-format(dateInterfaceMap, "%m-%d-%Y")
    url<-paste("http://dev.sisalert.com.br/apirest/api/v1/data/station/",codStation,"/range/",
    as.character(dateInterfaceMap),"/",as.character(dateInterfaceMap),sep='')
    
    dadosJson <- fromJSON(url)
    weatherStation <-dadosJson$weatherStation
    dadosJson <- dadosJson$data
    
    dadosJson<-dadosJson[!rev(duplicated(rev(dadosJson$datetime))),]
    if(!is.null(dadosJson)){
        
        dadosJson$date<-as.Date(dadosJson$datetime, format = "%m-%d-%Y")
        dadosJson$date<-format(dadosJson$date, "%Y-%m-%d")
        dadosJson$time<-format(as.POSIXct(strptime(dadosJson$datetime,"%m-%d-%Y %H:%M",tz="")) ,
        format = "%H:%M:%S")
        dados <-data.frame(date=dadosJson$date,time=dadosJson$time,
        temp =dadosJson$data$avgT, rh = dadosJson$data$avgH,
        wind = dadosJson$data$windS,srad = dadosJson$data$solarR,
        rain = dadosJson$data$totR,porvalho = dadosJson$data$avgDP)
        d<-subset(dados,as.integer(dados$time)==max(as.integer(dados$time)))
        dados$hcp <- round(mapply('hcp', dados$temp, dados$rh), 3)
        
        calc_dif <- function(temp,rh) {
            ifelse(temp >= 15 && temp < 27 && rh > 93, 14.35-0.25*temp,ifelse(temp >= 27 && temp < 35 && rh > 93,-8.5+0.59*temp,0))
        }
        dados$datetime <- paste(dados$date,dados$time)
        wall <<- cbind(datetime2=dados$datetime, dados,  dif = mapply('calc_dif', dados$temp, dados$rh)) %>%
        mutate(dd=as.Date(strptime(as.character(datetime2), format="%Y-%m-%d %H:%M:%S")),
        dif = ifelse(dif > 0, 1/dif, dif),
        # dif = calc_dif(as.numeric(avgT), as.numeric(avgH)),
        doy = as.numeric(strftime(dd, format="%j"))) %>%
        group_by(dd) %>% summarise(  maxTemp = max(temp),
        minTemp = min(temp),
        maxUr = max(rh),
        temp=mean(temp),
        chuva=sum(rain),
        ur=mean(rh),
        doy = first(doy),
        s_dif = sum(dif),
        hcp = sum(hcp))
        wall$diaFavInf<-diasFavInfe(wall$maxTemp,wall$minTemp,wall$ur)
        
        w1516 <<- wall %>% mutate(yr = year(dd),
        leap = leap_year(dd)) %>%
        group_by(yr) %>%
        ## mudei de 92,91 para pegar os mesmos dias do ano
        #filter(between(doy, ifelse(first(leap), 02,01), ifelse(first(leap), 366, 365))) %>%
        mutate(acc_dif=cumsum(s_dif)) %>%
        ungroup %>% select(dd:acc_dif)
        
        roll_dif = function(dif_vector, dif_new, id, discount_factor = NULL) {
            if(is.null(discount_factor)) discount_factor = seq(1, by = -0.1, length.out = 7)
            nmax = length(discount_factor)
            idx = seq(from=(id-1), by=-1, length.out = nmax-1)
            idx = idx[idx > 0]
            if(id == 1) {
                return(dif_vector[id])
            } else if(id > 1  && id <= nmax) {
                y = matrix(discount_factor[1:id], ncol = 1)
            } else {
                y = matrix(discount_factor, ncol = 1)
            }
            x = c(dif_vector[id],dif_new[idx])
            temp = as.numeric(x %*% y) / nmax
            return(temp)
        }
        
        #
        # Fator de desconto linear 7 dias, step = 0.10
        ddisc7 = seq(1, by = -0.1, length.out = 7)
        
        # Fator de desconto linear 3 dias, step = 0.25
        ddisc3 <- seq(1, by=-0.25,length.out = 3)
        
        
        dif16 = w1516$s_dif
        dif_new16_lag10 = vector(mode = mode(dif16), length = length(dif16))
        dif_new16_lag3 = vector(mode = mode(dif16), length = length(dif16))
        
        sapply(1:length(dif16), function(x) {
            dif_new16_lag10[x] <<- roll_dif(dif16, dif_new16_lag10, x, discount_factor = ddisc7)
            dif_new16_lag3[x] <<- roll_dif(dif16, dif_new16_lag3, x, discount_factor = ddisc3)
        })
        
        w1516[['dif_adj_lag10']] = c(dif_new16_lag10)
        w1516[['dif_adj_lag3']] = c( dif_new16_lag3)
        
        rm(list = ls(pattern = '^dif'))
        
        w1516 = w1516 %>%
        group_by(yr) %>%
        mutate(acc_dif_adj_lag10 = cumsum(dif_adj_lag10),
        acc_dif_adj_lag3 = cumsum(dif_adj_lag3))
        dados$hcp <- round(mapply('hcp', dados$temp, dados$rh), 3)
        
        dados$dd<-as.Date(paste(dados$date,sep=''),"%Y-%m-%d")
        # Calculate daily mean TInsterature and acculuted hcp in a given day
        days<-NULL
        days<-ddply(dados,.(dd),summarise,tempMax =max(temp),tempMin = min(temp),
        maxUr=max(rh),minUr=min(rh),temp=mean(temp),chuva=sum(rain),
        ur=mean(rh),hcp=sum(hcp))
        
        days$dif_adj_lag3 <- w1516$dif_adj_lag3
        
        days$diaFavInf<-diasFavInfe2(days$tempMax,days$maxUr,days$chuva)
        days$chcp<-cumsum(days$hcp)
        days<-days[order(days$dd),]
        #teste<-format(dateInterfaceMap, "%Y-%m-%d")
        days <-subset(days,days$dd == teste)
        
        lista <-NULL
        lista<-list(ip = days$chcp, diaFavInf = days$diaFavInf ,dif_adj_lag3 = days$dif_adj_lag3,
        weatherStation=weatherStation, rain = d$rain,rh=d$rh,temp=d$temp,time=d$time)
        
        return(lista)
    }
    lista<-list(ip = 0, diaFavInf = 0 , dif_adj_lag3 = 0
    ,weatherStation=weatherStation,rain = 0,rh=0,temp=0,time=0)
    return(lista)
}


buscaDados<- function(locale,startingDateMap,estado,estacao){
    if(is.null(estado))
    estado<-"DF"

    tamChoices <-length(choicesEstados)
    est <-NULL
    est <- loadEstacoesMap(locale,estado)
    a<-dim(est)
    i=1
    df<-data.frame()
    if(!is.null(estacao)){
      est<-subset(est,est$`_id` == estacao)
    }
    for(i in 1:a[1]){
        lista<-NULL
        
        lista<-calculaIp(est$`_id`[i],startingDateMap)
        ret<-NULL
        ret<- data.frame(cod_area = est$`_id`[i], nome = est$name[i],
        latitude = est$location$lat[i],
        longitute = est$location$lon[i],
        altitude = est$location$elev[i],
        country = locale,
        state = estado,
        ip = lista$ip,
        rain = lista$rain,
        rh = lista$rh,
        temp = lista$temp,
        time = lista$time,
        diaFav = lista$diaFavInf,
        dif_adj_lag3 = lista$dif_adj_lag3,
        abrr = est$organization$abbr[i]
        )
        df <- rbind(df,ret)
    }
    return(df)
    
}

gCollor<-function(dif_adj_lag3,diaFav,ip) {
    if(dif_adj_lag3 >=0.2 && diaFav == 1 && ip>=0.2) {
        "red"
    } else if(dif_adj_lag3 < 0.2 && diaFav != 1 && ip< 0.2 ) {
        "green"
    } else {
        "orange"
    } }
getColor <- function(dado) {
    try(
    mapply(gCollor,dado$dif_adj_lag3,dado$diaFav,dado$ip)
    )
}

gLabel<-function(dif_adj_lag3,diaFav,ip,name) {
    if(dif_adj_lag3 >=0.2 && diaFav == 1 && ip>=0.2) {
        risco<-"HIGH"
    } else if(dif_adj_lag3 < 0.2 && diaFav != 1 && ip< 0.2 ) {
        risco<-"NO RISK"
    } else {
        risco<-"MODERATE"
    }
    paste(name," - ",risco,sep="")
}
getLabel <-function(dado){
    try(
    mapply(gLabel,dado$dif_adj_lag3,dado$diaFav,dado$ip,dado$nome)
    )
}

gPop<-function(dif_adj_lag3,diaFav,ip,name,id,latitude,longitute,abrr,time,
               rh,rain,temp) {
    name <-paste("<h4>",name,"</h4>",sep="")
    if(dif_adj_lag3 >=0.2 && diaFav == 1 && ip>=0.2) {
        risk<-"<strong>Risk: HIGH</strong><br>"
        
    } else if(dif_adj_lag3 < 0.2 && diaFav != 1 && ip< 0.2 ) {
        risk<-"<strong>Risk: NO RISK</strong><br>"
    } else {
        risk<-"<strong>Risk: MODERATE</strong><br>"
    }
    ids<-paste("ID: ",id,"<br>",sep="")
    org<-paste("Organization: ",abrr,"<br>",sep = "")
    lat<-paste("Latitude: ",latitude,"<br>",sep = "")
    lng <-paste("Longitude: ",longitute,"<br>",sep = "")
    temp<-paste("Temperature: ",round(temp[1],2),"C<br>",sep = "")
    rain<-paste("Rain: ",round(rain[1],2),"<br>",sep = "")
    rh<-paste("Humidity: ",round(rh[1],2),"%<br>",sep = "")
    spor<-paste("SPOR: ",round(dif_adj_lag3[1],2),"<br>",sep = "")
    tim<-paste("Time: ",time,"<br>",sep = "")
    if(diaFav == 1){
        diaf <-"<strong>Favorable to infection</strong><br>"
    }else{
        diaf <-"<strong>Not favorable to infection</strong><br>"
    }
    paste(name,risk,tim,org,lat,lng,temp,rain,rh,spor,diaf,sep = "")
}
getPopup <-function(dado){
    try(
    mapply(gPop,dado$dif_adj_lag3,dado$diaFav,dado$ip,dado$nome,
    dado$cod_area,dado$latitude,dado$longitute,dado$abrr,dado$time,
    dado$rh,dado$rain,dado$temp)
    )
}
exploreData <- function(codStation,dateInterface,pais){
    
  
    lista <-loadData(codStation,dateInterface)
    dados<-lista$dados
    calc_dif <- function(temp,rh) {
      ifelse(temp >= 15 && temp < 27 && rh > 93, 14.35-0.25*temp,ifelse(temp >= 27 && temp < 35 && rh > 93,-8.5+0.59*temp,0))
    }
    dados$hcp <- round(mapply('hcp', dados$temp, dados$rh), 3)
    dados$datetime <- paste(dados$date,dados$time)


    dados$datetime <- paste(dados$date,dados$time)
    wall <<- cbind(datetime2=dados$datetime, dados,  dif = mapply('calc_dif', dados$temp, dados$rh)) %>%
      mutate(dd=as.Date(strptime(as.character(datetime2), format="%Y-%m-%d %H:%M:%S")),
             dif = ifelse(dif > 0, 1/dif, dif),
             # dif = calc_dif(as.numeric(avgT), as.numeric(avgH)),
             doy = as.numeric(strftime(dd, format="%j"))) %>%
      group_by(dd) %>% summarise(  maxTemp = max(temp),
                                   minTemp = min(temp),
                                   maxUr = max(rh),
                                   temp=mean(temp),
                                   chuva=sum(rain),
                                   ur=mean(rh),
                                   doy = first(doy),
                                   s_dif = sum(dif),
                                   hcp = sum(hcp))
    wall$diaFavInf<-diasFavInfe(wall$maxTemp,wall$minTemp,wall$ur)
    
    w1516 <<- wall %>% mutate(yr = year(dd),
                              leap = leap_year(dd)) %>%
      group_by(yr) %>%
      ## mudei de 92,91 para pegar os mesmos dias do ano
      #filter(between(doy, ifelse(first(leap), 02,01), ifelse(first(leap), 366, 365))) %>%
      mutate(acc_dif=cumsum(s_dif)) %>%
      ungroup %>% select(dd:acc_dif)
    
    roll_dif = function(dif_vector, dif_new, id, discount_factor = NULL) {
      if(is.null(discount_factor)) discount_factor = seq(1, by = -0.1, length.out = 7)
      nmax = length(discount_factor)
      idx = seq(from=(id-1), by=-1, length.out = nmax-1)
      idx = idx[idx > 0]
      if(id == 1) {
        return(dif_vector[id])
      } else if(id > 1  && id <= nmax) {
        y = matrix(discount_factor[1:id], ncol = 1)
      } else {
        y = matrix(discount_factor, ncol = 1)
      }
      x = c(dif_vector[id],dif_new[idx])
      temp = as.numeric(x %*% y) / nmax
      return(temp)
    }
    
    #
    # Fator de desconto linear 7 dias, step = 0.10
    ddisc7 = seq(1, by = -0.1, length.out = 7)
    
    # Fator de desconto linear 3 dias, step = 0.25
    ddisc3 <- seq(1, by=-0.25,length.out = 3)
    
    
    dif16 = w1516$s_dif
    dif_new16_lag10 = vector(mode = mode(dif16), length = length(dif16))
    dif_new16_lag3 = vector(mode = mode(dif16), length = length(dif16))
    
    sapply(1:length(dif16), function(x) {
      dif_new16_lag10[x] <<- roll_dif(dif16, dif_new16_lag10, x, discount_factor = ddisc7)
      dif_new16_lag3[x] <<- roll_dif(dif16, dif_new16_lag3, x, discount_factor = ddisc3)
    })
    
    w1516[['dif_adj_lag10']] = c(dif_new16_lag10)
    w1516[['dif_adj_lag3']] = c( dif_new16_lag3)
    
    rm(list = ls(pattern = '^dif'))
    
    w1516 = w1516 %>%
      group_by(yr) %>%
      mutate(acc_dif_adj_lag10 = cumsum(dif_adj_lag10),
             acc_dif_adj_lag3 = cumsum(dif_adj_lag3))
    dados$hcp <- round(mapply('hcp', dados$temp, dados$rh), 3)
    
    dados$dd<-as.Date(paste(dados$date,sep=''),"%Y-%m-%d")
    # Calculate daily mean TInsterature and acculuted hcp in a given day
    days<-NULL
    days<-ddply(dados,.(dd),summarise,tempMax =max(temp),tempMin = min(temp),
                maxUr=max(rh),minUr=min(rh),temp=mean(temp),chuva=sum(rain),
                ur=mean(rh),hcp=sum(hcp),porvalho = mean(porvalho))
    
    days$dif_adj_lag3 <- w1516$dif_adj_lag3
    
    days$diaFavInf<-diasFavInfe2(days$tempMax,days$maxUr,days$chuva)
    days$chcp<-cumsum(days$hcp)
    days<-days[order(days$dd),]
    
    days$diaFavInf <-NULL
    days$hcp <-NULL
    days$temp <-round(days$temp,2)
    days$ur<-round(days$ur,2)
    days$chcp <- round(days$chcp,2)
    days$tempMax<-round(days$tempMax,2)
    days$tempMin<-round(days$tempMin,2)
    days$dif_adj_lag3 <-round(days$dif_adj_lag3,2)
    days$porvalho <-round(days$porvalho,2)
    
    cloudF<-sporeCloud(codStation,dateInterface)
    cloudF <- round(cloudF,3)
    
   # names(days)<-c("Date","Max. Temp.(C)","Min. Temp.(C)","Max. Hum.(%)","Min. Hum.(%)",
  #                 "Temp.(C)","Rain(mm)","Hum.(%)","Dew point(C)", "SPOR", "IP")
    days$CloudF <- cloudF$sporeCloud
    names(days)<-c("Date","Max. Temp.(C)","Min. Temp.(C)","Max. Hum.(%)","Min. Hum.(%)",
                   "Temp.(C)","Rain(mm)","Hum.(%)","Dew point(C)", "SPOR", "IP","Spore Cloud")
    days <- days[rev(order(as.Date(days$Date))),]
    days
}

##############################
gPopm<-function(dd,dif_adj_lag3,diaFav,ip,latitude,longitute,
               rh,minrh,rain,temp,mintemp) {
  if(dif_adj_lag3 >=0.2 && diaFav == 1 && ip>=0.2) {
    risk<-"<strong>Risk: HIGH</strong><br>"
    
  } else if(dif_adj_lag3 < 0.2 && diaFav != 1 && ip< 0.2 ) {
    risk<-"<strong>Risk: NO RISK</strong><br>"
  } else {
    risk<-"<strong>Risk: MODERATE</strong><br>"
  }
  dat<-paste("Date: ",dd,"<br>",sep = "")
  lat<-paste("Latitude: ",latitude,"<br>",sep = "")
  lng <-paste("Longitude: ",longitute,"<br>",sep = "")
  temp<-paste("Max. Temperature: ",round(temp[1],2),"C<br>",sep = "")
  mtemp<-paste("Min Temperature: ",round(mintemp[1],2),"C<br>",sep = "")
  rain<-paste("Rain: ",round(rain[1],2),"<br>",sep = "")
  rh<-paste("Max. Humidity: ",round(rh[1],2),"%<br>",sep = "")
  mrh<-paste("Min. Humidity: ",round(minrh[1],2),"%<br>",sep = "")
  spor<-paste("SPOR: ",round(dif_adj_lag3[1],2),"<br>",sep = "")
  
  if(diaFav == 1){
    diaf <-"<strong>Favorable to infection</strong><br>"
  }else{
    diaf <-"<strong>Not favorable to infection</strong><br>"
  }
  paste(risk,dat,lat,lng,temp,mtemp,rain,rh,mrh,spor,diaf,sep = "")
}
getPopu<-function(dado){

  try(
    mapply(gPopm,dado$dd,dado$dif_adj_lag3,dado$diaFav,dado$chcp,
           dado$lat,dado$lng,
           dado$maxUr,dado$minUr,dado$chuva,dado$tempMax,dado$tempMin)
  )
}
graphClim2 <-function(codStation,dateInterface,pais){
  lista <-loadData(codStation,dateInterface)
  dados<-lista$dados
  
  daily<-ddply(dados,.(date),summarise,tmax=max(temp),tmin=min(temp),rhmin=min(rh),rhmax=max(rh),rain=sum(as.double(rain)),srad=sum(srad))
  return(daily)
}

getDatacache <- function(input,session){
  js$getCache(1)

  if(!is.null(input$cimmytName)){
    cimmytname <-input$cimmytName
    cimmytlat<-input$cimmytlat
    cimmytlong<-input$cimmytlong
    cimmytcol<- input$cimmytcolor
    name <-strsplit(cimmytname,";")
    lat <- strsplit(cimmytlat,";")
    long<-strsplit(cimmytlong,";")
    col <- strsplit(cimmytcol,";")

    df <-data.frame(name = name[[1]],lat =as.numeric( lat[[1]]),long = as.numeric(long[[1]]),color= col[[1]])
    df$lab <- mapply("labpHeatMap",df$lat,df$long,df$name)
    return(df)
  }
  return(NULL)
}

labpHeatMap <- function(lat, long,name){
  date<-Sys.Date()
  dia <-format(date,"%d")
  dia <-as.integer(dia)%%2
  if(dia != 0){
    apiKey<-"e05aea7011ee95597b918eba7ac5ee3f"
  }else{
    apiKey <-"f65ec68580b6d7112187b817d26be957"
  }
  Sys.setenv(DARKSKY_API_KEY = apiKey)
  #print("vou ler darsky")
  df<-get_current_forecast(latitude = lat,longitude = long)
  #print("li darsky")
  options(scipen = 2)
  df <- df$currently
  
  df$date<-as.Date(df$time, format = "%Y-%m-%d")
  df$date<-format(df$date, "%d-%m-%Y")
  
  df$time2<-format(as.POSIXct(strptime(df$time,"%Y-%m-%d %H:%M",tz="")) ,
                 format = "%H:%M:%S")
  df$humidity <-df$humidity *100
  df$precipIntensity <- df$precipIntensity*25.4
  df$precipIntensity <-round(df$precipIntensity,1)
  df$temperature <- fahrenheit.to.celsius(df$temperature, round = 2)
  df$dewPoint <-fahrenheit.to.celsius(df$dewPoint, round = 2)
  df$precipIntensity<-df$precipIntensity
  
  lab <- paste0("<strong>",name,"</strong><br>",
                df$date,"<br>",
                "Latitude: ",lat,"<br>",
                "Longitude: ",long,"<br>",
                "Temperature: ",df$temperature," ??C<br>",
                "Rainfall: ",df$precipIntensity," mm<br>",
                "Humidity: ",df$humidity," %<br>")
  return(lab)
}

