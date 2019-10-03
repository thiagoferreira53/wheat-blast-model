#!/usr/bin/Rscript
library(darksky)
library(rgdal)
library(weathermetrics)
#library(SmarterPoland)
library(leaflet)
library(sp)
library(maps)
library(RColorBrewer)
library(classInt)
library(spatialEco)
library(plyr)
library(lubridate)
library(dplyr)
library(tidyr)
library(magrittr)
library(gstat)
library(maptools)


hcp<- function(temp,rh) { # verificar
  ifelse(temp >= 15 && temp < 27 && rh > 93,
         1/(14.35-0.25*temp),
         ifelse(temp >= 27 && temp < 35 && rh > 93,1/(-8.5+0.59*temp),0))
}
diasFavInfe2<-function(tMax,rhMax,rain){
  ifelse(rhMax >=93,ifelse(tMax>23,ifelse(rain<=5,1,0),0),0)
  
}
diasFavInfe<-function(tempMax,tempMin,rh){
  difTemp= tempMax-tempMin  
  ifelse(tempMax>=23,ifelse(difTemp >=13,ifelse(rh>=70,1,0),0),0)
}
gCollor<-function(dif_adj_lag3,diaFav,ip) {
  if(dif_adj_lag3 >=0.2 && diaFav == 1 && ip>=0.2) {
    "red" 
  } else if(dif_adj_lag3 < 0.2 && diaFav != 1 && ip< 0.2 ) {
    "green"
  } else {
    "orange"
  } } 
getForecastDarkSkyHour <-function(apiKey,lat,long){
  print("lat")
  print(lat)
  print(long)
  Sys.setenv(DARKSKY_API_KEY = apiKey)
  print(apiKey)
  df<-get_current_forecast(latitude = lat,longitude = long)
  options(scipen = 2)
  f<-df$hourly
  f$date<-as.Date(f$time, format = "%Y-%m-%d")
  f$date<-format(f$date, "%Y-%m-%d")
  f$time<-format(as.POSIXct(strptime(f$time,"%Y-%m-%d %H:%M",tz="")) ,
                 format = "%H:%M:%S")
  f$humidity <-f$humidity *100
  f$precipIntensity <- f$precipIntensity*25.4
  f$precipIntensity <-round(f$precipIntensity,2)
  f$temperature <- fahrenheit.to.celsius(f$temperature, round = 2)
  f$dewPoint <-fahrenheit.to.celsius(f$dewPoint, round = 2)
  f$precipIntensity<-f$precipIntensity
  dados <-data.frame(date=f$date,time=f$time,temp = f$temperature,rh=f$humidity,
                     wind = f$windSpeed,srad = 0, rain = f$precipIntensity,
                     porvalho = f$dewPoint)
  
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
    ungroup %>%dplyr::select(dd:acc_dif)
  
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
  days$color<-mapply(gCollor,days$dif_adj_lag3,days$diaFav,days$chcp)
  days$lat<-lat
  days$long<-long
  return(days)
}


apiKey<-"Your API key goes here. You can get on https://darksky.net"

setwd("/home/csisa_bihar/forecastDarkSky/shapesBd")

shapeDistrict<- readOGR(".", "BD_UPZ_GCS", GDAL1_integer64_policy = TRUE)
coordenadas <- coordinates(shapeDistrict)
proj4string(shapeDistrict)
a<-coordenadas
a<-as.data.frame(a)
lon<-a$V1
lat<-a$V2

x.range <- as.numeric(c(min(lon),max(lon)))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(min(lat),max(lat)))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.115), y = seq(from = y.range[1], 
                                                                                    to = y.range[2], by = 0.115)) 
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE
g<-coordinates(grd)
g<-as.data.frame(g)
lon<-g$x
lat<-g$y

coordenadas <- coordinates(shapeDistrict)
colnames(g)<-c("LONGITUDE", "LATITUDE")
pontos <- g[c(1,2)]
coordinates(pontos) <- c("LONGITUDE", "LATITUDE")

tabela <-g

proj4string(pontos) <- proj4string(shapeDistrict)


new_pontos <- tabela[!is.na(over(pontos, as(shapeDistrict, "SpatialPolygons"))),]


lon<-new_pontos$LONGITUDE
lat<-new_pontos$LATITUDE
aux <-getForecastDarkSkyHour(apiKey,lat[1],lon[1])
for(i in 2:length(lat)){
  aux2<-getForecastDarkSkyHour(apiKey,lat[i],lon[i])
  aux<-rbind(aux,aux2)
}
forecast <-aux
#save(forecast,file = "/Users/felipevargas/Google Drive/projetos/CIMMYT/wheatblast/bd_forecast")
#save(forecast,file = "/opt/projects/development/riskMonitoringData/dadoBd/bd_forecast")
save(forecast,file = "/home/csisa_bihar/dados/bd_forecast_darksky")
