#path <-"/opt/projects/development/riskMonitoringData/dados/"
path <-"/home/csisa_bihar/dados/"
#path <-"/Users/felipevargas/Google\ Drive/projetos/CIMMYT/wheatblast/dados/"

#pathHeat <- "/opt/projects/development/riskMonitoringData/dadoBd/"
pathHeat <- "/home/csisa_bihar/dados/"
#pathHeat <-"/Users/felipevargas/Google\ Drive/projetos/CIMMYT/wheatblast/dados/"
source("global.R")

stationsBD<-c("BL","BS","CD","DJ","DK","FR","JS","MD","MH","PT","RS")

shinyServer(function(input, output, session) {
  #gera o mapa!
  output$states = renderUI({
    sel <-NULL
    if(input$pais == "BR"){
      sel <-"DF"
      choicesEstados<-c("DF","GO","MG","MT",
                        "MS","PR","RS","SC","SP")
      names(choicesEstados)<-c("Distrito Federal",
                               "Goiás","Minas Gerais","Mato Grosso",
                               "Mato Grosso do Sul","Paraná","Rio Grande do Sul",
                               "Santa Catarina",
                               "São Paulo")
    }
    if(input$pais == "BD"){
      #input$states <-"BL"
      choicesEstados<-c("BL","BS","CD","DJ","DK","FR","JS","MD","MH","PT","RS")
      names(choicesEstados)<-c("Bhola District","Barisal District","Chuadanga District",
                               "Dinajpur District","Dhaka District",
                               "Faridpur District","Jessore District","Madaripur District",
                               "Meherpur District",
                               "Patuakhali District","Rajshahi District")
    }
    
    choicesEstadosMaps<-sort(choicesEstados)
    selectInput('states', 'State', choicesEstados)
  })
  
  dados <- reactive ({
    estado <- input$statesMaps
#    if(!input$showAll){
     estacao <- input$stationMaps
#    }else{
#      estacao <- NULL
#    }
    if(input$locale =="BD"){
      if(is.null(estado) || !is.element(estado,bdStates)){
        estado <-"BL"
      }
    }
    locale <-input$locale
    
    if(is.null(estado))
      estado="DF"
    d<-data.frame()
    #print("ESTACAOOOOO")
    #print(estacao)
    if(length(estacao)>1){
      if(locale!="BD"){
      d <- buscaDados(locale, input$startingDateMap,estado,estacao[1])
      
      for(i in 2:length(estacao)){
        aux <- buscaDados(locale, input$startingDateMap,estado,estacao[i])
        d <-rbind(d,aux)
      }
      }else{
        d<-buscaDados(locale, input$startingDateMap,stationsBD[1],estacao[1])
        for(i in 2:length(stationsBD)){
          for(j in 2:length(estacao)){
            aux <- buscaDados(locale, input$startingDateMap,stationsBD[i],estacao[j])
            d <-rbind(d,aux)
          }
        }
      }
    }else{
      if(locale!="BD"){
        d<-buscaDados(locale, input$startingDateMap,estado,estacao)
      }else{
        d<-buscaDados(locale, input$startingDateMap,stationsBD[1],estacao)
        for(i in 2:length(stationsBD)){
          aux <- buscaDados(locale, input$startingDateMap,stationsBD[i],estacao)
          d <-rbind(d,aux)
        }
      }
    }
    return(d)
  })
  dadosHeat <- reactive ({
    estado <- input$statesMapsHeat
    if(input$localeHeat =="BD"){
      if(is.null(estado) || !is.element(estado,bdStates)){
        estado <-"BL"
      }
    }
    locale <-input$localeHeat
    
    if(is.null(estado))
      estado="DF"
    d<-data.frame()
    d<-buscaDados(locale, input$startingDateMapHeat,estado,NULL)
    return(d)
  })
  
  output$map <- renderLeaflet({
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
      try(
        dados<- dados()
      )
      icons <- awesomeIcons(
        icon = 'ios-plus',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(dados)
      )
      estado <-input$statesMaps
      if(is.null(estado))
        estado <-"DF"
      if(input$locale == "BR"){
        if(estado =="DF"){
          coord<-c(-15.8451568,-47.3012397)
        }
        else{
          if(estado =="GO"){
            coord<-c(-16.1308511,-47.2365033)
          }else{
            if(estado =="MG"){
              coord<-c(-20.6050441,-42.5973196)
            }else{
              if(estado =="MS"){
                coord<-c(-21.0024969,-53.2239587)
              }else{
                if(estado =="MT"){
                  coord<-c(-14.3985269,-51.7796119)
                }else{
                  if(estado =="PR"){
                    coord<-c(-25.0738359,-49.7255616)
                  }else{
                    if(estado =="RS"){
                      
                      coord<-c(-30.3443834,-50.8981533)
                    }else{
                      if(estado =="SC"){
                        coord<-c(-28.1647,-48.9924751)
                      }else{
                        if(estado =="SP"){
                          coord<-c(-22.8599067,-46.7982757)
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }else{
        if(estado =="BL"){
          coord<-c(22.3337423,90.631635)
        }else{
          if(estado =="BS"){
            coord<-c(22.6954732,90.3180418)
          }else{
            if(estado =="CD"){
              coord<-c(23.6333,88.85)
            }else{
              if(estado =="FR"){
                coord<-c(23.4615591,89.5558264)
              }else{
                if(estado =="JS"){
                  coord<-c(23.0832767,88.9305279)
                }else{
                  if(estado =="MD"){
                    coord<-c(23.2221343,89.8673699)
                  }else{
                    if(estado =="PT"){
                      coord<-c(22.2056903,90.0950082)
                    }else{
                      if(estado =="RS"){
                        coord<-c(24.3664324,88.6012016)
                      }else{
                        if(estado == "DJ"){
                          coord<-c(25.74593,88.67250)
                        }else{
                          if(estado == "MH"){
                            coord<-c(23.72749,88.73213)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      try(
        leaflet(dados) %>%
          addTiles(
            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
          ) %>%
          setView(lat = coord[1], lng = coord[2] , zoom = 6)%>%
          addAwesomeMarkers(lat= ~latitude,lng=~longitute, icon=icons,label=getLabel(dados),
                            popup=getPopup(dados))%>%
          addLegend("bottomright",title = "Outbreak risk factor",colors = c("green","orange","red"),
                    labels =c("No risk","Moderate","High") )
      )
    
  })
  
  observe({
    
      estado<-input$statesMaps
      
      if(is.null(estado) && input$locale=="BR")
        estado <-"DF"
      
      try(
        dados<-dados(),
        icons <- awesomeIcons(
          library = 'ion',
          markerColor = getColor(dados)
        )
      )
      try(
        if(input$locale =="BR"){
          if(input$locale == "BR" && !is.element(estado,choicesEstados)){
            estado <-"DF"
          }
          if(estado =="DF"){
            coord<-c(-15.8451568,-47.3012397)
          }
          else{
            if(estado =="GO"){
              coord<-c(-16.1308511,-47.2365033)
            }else{
              if(estado =="MG"){
                coord<-c(-20.6050441,-42.5973196)
              }else{
                if(estado =="MS"){
                  coord<-c(-21.0024969,-53.2239587)
                }else{
                  if(estado =="MT"){
                    coord<-c(-14.3985269,-51.7796119)
                  }else{
                    if(estado =="PR"){
                      coord<-c(-25.0738359,-49.7255616)
                    }else{
                      if(estado =="RS"){
                        
                        coord<-c(-30.3443834,-50.8981533)
                      }else{
                        if(estado =="SC"){
                          coord<-c(-28.1647,-48.9924751)
                        }else{
                          if(estado =="SP"){
                            coord<-c(-22.8599067,-46.7982757)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }else{
          if(input$locale =="BD"){
            if(input$locale == "BD" && !is.element(estado,bdStates)){
              estado <-"BL"
            }
            if(estado =="BL"){
              coord<-c(22.3337423,90.631635)
            }else{
              if(estado =="BS"){
                coord<-c(22.6954732,90.3180418)
              }else{
                if(estado =="CD"){
                  coord<-c(23.6333,88.85)
                }else{
                  if(estado =="FR"){
                    coord<-c(23.4615591,89.5558264)
                  }else{
                    if(estado =="JS"){
                      coord<-c(23.0832767,88.9305279)
                    }else{
                      if(estado =="MD"){
                        coord<-c(23.2221343,89.8673699)
                      }else{
                        if(estado =="PT"){
                          coord<-c(22.2056903,90.0950082)
                        }else{
                          if(estado =="RS"){
                            coord<-c(24.3664324,88.6012016)
                          }else{
                            if(estado == "DJ"){
                              coord<-c(25.74593,88.67250)
                            }else{
                              if(estado == "MH"){
                                coord<-c(23.72749,88.73213)
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      )
      try(
        leafletProxy("map", data = dados) %>%
          clearPopups() %>%
          setView(lat = coord[1], lng = coord[2] , zoom = 6)
      )
    
  })
  
  output$heatmap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
      estado<-input$statesMaps
      
      dadosCache <- getDatacache(input,session)
      
      icons <- awesomeIcons(
        icon = 'ios-plus',
        iconColor = 'black',
        library = 'ion',
        markerColor = dadosCache$color
      )
      
      if(input$locale == "BR" || input$localeHeat == "BR"){
        if(!is.element(estado,choicesEstados))
          estado<-"DF"
        if(estado == "BL")
          estado <-"DF"
        #url
        #url<-"C:/Users/felly/Google Drive/projetos/ex shape/dados/"
        url<-path
        url<-paste(url,tolower(estado),".rda",sep="")
        load(url)
        if(estado =="DF"){
          coord<-c(-15.8451568,-47.3012397)
        }
        else{
          if(estado =="GO"){
            coord<-c(-16.1308511,-47.2365033)
          }else{
            if(estado =="MG"){
              coord<-c(-20.6050441,-42.5973196)
            }else{
              if(estado =="MS"){
                coord<-c(-21.0024969,-53.2239587)
              }else{
                if(estado =="MT"){
                  coord<-c(-14.3985269,-51.7796119)
                }else{
                  if(estado =="PR"){
                    coord<-c(-25.0738359,-49.7255616)
                  }else{
                    if(estado =="RS"){
                      
                      coord<-c(-30.3443834,-50.8981533)
                    }else{
                      if(estado =="SC"){
                        coord<-c(-28.1647,-48.9924751)
                      }else{
                        if(estado =="SP"){
                          coord<-c(-22.8599067,-46.7982757)
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
        dados<-subset(l,l$dd==min(l$dd))
        color<-dados$color
        shinyjs::hide("startingDateMapHeat")
        color<-dados$color
        colorL<-c("green","orange","red")
        labelL<-c("No risk","Moderate","High")
        
        if(input$riskHeat == "wheatblast"){
          color<-dados$color
          colorL<-c("green","orange","red")
          labelL<-c("No risk","Moderate","High")
          titulo<-"Outbreak risk factor"
        }else{
          if(input$riskHeat == "precipitation"){
            variavel <-dados$chuva
            colors <- brewer.pal(9, "Blues")#set breaks for the 9 colors 
            #brks<-classIntervals(variavel, n=9, style="quantile")
            #brks<- brks$brks #plot the map
            brks<-c(0,5,15,25,35,45,55,100)
            color<-colors[findInterval(variavel, brks,all.inside=TRUE)]
            colorL<-colors[findInterval(brks, brks,all.inside=TRUE)]
            labelL<-c("0-5","5-15","15-25","25-35","35-45","45-55","55-100"," > 100")
            titulo <-"Precipitation (mm)"
          }else{
            if(input$riskHeat == "temperature"){
              variavel <-dados$temp
              colors <- brewer.pal(9, "Reds") #set breaks for the 9 colors 
              #brks<- brks$brks #plot the map
              brks<-c(0,5,10,15,20,25,30,35)
              color<-color[findInterval(variavel, brks,all.inside=TRUE)]
              colorL<-colors[findInterval(brks, brks,all.inside=TRUE)]
              labelL<-c("0-5","5-10","10-15","15-20","20-25","25-30","30-35"," > 35")
              titulo <-"Temperature(C)"
            }else{
              if(input$riskHeat == "humidity"){
                variavel <-dados$ur
                colors <- brewer.pal(9, "Purples") #set breaks for the 9 colors 
                brks<-c(10,20,30,40,50,60,70,80,90,100)
                color<-color[findInterval(variavel, brks,all.inside=TRUE)]
                colorL<-colors[findInterval(brks, brks,all.inside=TRUE)]
                labelL<-c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100")
                titulo <-"Humidity(%)"
              }
            }
          }
        }

        if(!is.null(dadosCache)){
          try(
            map1<-leaflet(dados) %>%
              addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
              )%>%
              clearControls()
            %>%
              setView(lat = coord[1], lng = coord[2] , zoom = 6) %>%
              addRectangles(
                lng1=~lng-0.075, lat1=~lat-0.075,
                lng2=~lng+0.075, lat2=~lat+0.075,
                color=color, 
                weight = 0)%>%

              addAwesomeMarkers(lat=dadosCache$lat,lng=dadosCache$long,
                                icon = icons,label =dadosCache$name,popup =dadosCache$lab  )%>%
              addLegend("bottomright",title = titulo,colors = colorL,
                        labels =labelL )%>%
              syncWith("maps"),
            
            dados<-subset(l,l$dd==min(l$dd)+1),
            color<-dados$color,
            
            map2<-leaflet(dados) %>%
              addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
              )%>%
              clearControls()
            %>%
              setView(lat = coord[1], lng = coord[2] , zoom = 6) %>%
              addRectangles(
                lng1=~lng-0.075, lat1=~lat-0.075,
                lng2=~lng+0.075, lat2=~lat+0.075,
                color=color, 
                weight = 0)%>%
              addAwesomeMarkers(lat=dadosCache$lat,lng=dadosCache$long,
                                icon = icons,label =dadosCache$name,popup =dadosCache$lab  )%>%
              addLegend("bottomright",title = titulo,colors = colorL,
                        labels =labelL )%>%
              syncWith("maps"),
              combineWidgets(map1, map2)
            
          )
        }else{
          try(
            leaflet(dados) %>%
              clearControls()%>%
              clearTiles()%>%
              addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
              )
            %>%
              setView(lat = coord[1], lng = coord[2] , zoom = 6) %>%
              addRectangles(
                lng1=~lng-0.075, lat1=~lat-0.075,
                lng2=~lng+0.075, lat2=~lat+0.075,
                color=color, 
                weight = 0)
            %>%
              addLegend("bottomright",title = titulo,colors = colorL,
                        labels =labelL )
            
          )
        }
      }else{
        url <- pathHeat
        if(input$sourceMapsHeat == "BMD"){
          load(paste0(url,"bd_forecast_bmd"))
        }else{
          load(paste0(url,"bd_forecast_darksky"))
        }
        
        dados<-subset(forecast,forecast$dd==input$slider)
        
        shinyjs::hide("startingDateMapHeat")
        color<-dados$color
        colorL<-c("green","orange","red")
        labelL<-c("No risk","Moderate","High")
        
        if(input$riskHeat == "wheatblast"){
          color<-dados$color
          colorL<-c("green","orange","red")
          labelL<-c("No risk","Moderate","High")
          titulo<-"Outbreak risk factor"
        }else{
          if(input$riskHeat == "precipitation"){
            variavel <-dados$chuva
            colors <- brewer.pal(9, "Blues")#set breaks for the 9 colors 
            #brks<-classIntervals(variavel, n=9, style="quantile")
            #brks<- brks$brks #plot the map
            brks<-c(0,5,15,25,35,45,55,100)
            color<-colors[findInterval(variavel, brks,all.inside=TRUE)]
            colorL<-colors[findInterval(brks, brks,all.inside=TRUE)]
            labelL<-c("0-5","5-15","15-25","25-35","35-45","45-55","55-100"," > 100")
            titulo <-"Precipitation (mm)"
          }else{
            if(input$riskHeat == "temperature"){
              variavel <-dados$temp
              colors <- brewer.pal(9, "Reds") #set breaks for the 9 colors 
              #brks<- brks$brks #plot the map
              brks<-c(0,5,10,15,20,25,30,35)
              color<-color[findInterval(variavel, brks,all.inside=TRUE)]
              colorL<-colors[findInterval(brks, brks,all.inside=TRUE)]
              labelL<-c("0-5","5-10","10-15","15-20","20-25","25-30","30-35"," > 35")
              titulo <-"Temperature(C)"
            }else{
              if(input$riskHeat == "humidity"){
                variavel <-dados$ur
                colors <- brewer.pal(9, "Purples") #set breaks for the 9 colors 
                brks<-c(10,20,30,40,50,60,70,80,90,100)
                color<-color[findInterval(variavel, brks,all.inside=TRUE)]
                colorL<-colors[findInterval(brks, brks,all.inside=TRUE)]
                labelL<-c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100")
                titulo <-"Humidity(%)"
              }
            }
          }
        }
        if(input$sourceMapsHeat =="BMD"){
          mlong <- 0.04364
          mlat <- 0.040
        }else{
          mlong <- 0.0575
          mlat <- 0.0575
        }
        
        if(!is.null(dadosCache)){
          try(
            #https://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png
            leaflet(dados) %>%
              addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
              )%>%
              clearControls()
            %>%
              addRectangles(
                lng1=~long-mlong, lat1=~lat-mlat,
                lng2=~long+mlong, lat2=~lat+mlat,
                color=color, 
                weight = 0)%>%
              addAwesomeMarkers(lat=dadosCache$lat,lng=dadosCache$long,
                                icon = icons,label =dadosCache$name,popup =dadosCache$lab  )%>%
              addLegend("bottomright",title = titulo,colors = colorL,
                        labels =labelL )
            
          )
        }else{
          try(
            leaflet(dados) %>%
              addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
              )%>%
              clearControls()
            %>%
              addRectangles(
                lng1=~long-mlong, lat1=~lat-mlat,
                lng2=~long+mlong, lat2=~lat+mlat,
                color=color, 
                weight = 0)%>%
              addLegend("bottomright",title = titulo,colors = colorL,
                        labels =labelL )
            
          )
        }
        
      }
      
    
  })
  
  observe({
      estado<-input$statesMapsHeat
      data<-input$slider
      
      if(is.null(estado) && input$localeHeat=="BR")
        estado <-"DF"
      try(
        if(input$localeHeat =="BR"){
          if(input$locale == "BR" && !is.element(estado,choicesEstados)){
            estado <-"DF"
          }
          if(estado =="DF"){
            coord<-c(-15.8451568,-47.3012397)
          }
          else{
            if(estado =="GO"){
              coord<-c(-16.1308511,-47.2365033)
            }else{
              if(estado =="MG"){
                coord<-c(-20.6050441,-42.5973196)
              }else{
                if(estado =="MS"){
                  coord<-c(-21.0024969,-53.2239587)
                }else{
                  if(estado =="MT"){
                    coord<-c(-14.3985269,-51.7796119)
                  }else{
                    if(estado =="PR"){
                      coord<-c(-25.0738359,-49.7255616)
                    }else{
                      if(estado =="RS"){
                        
                        coord<-c(-30.3443834,-50.8981533)
                      }else{
                        if(estado =="SC"){
                          coord<-c(-28.1647,-48.9924751)
                        }else{
                          if(estado =="SP"){
                            coord<-c(-22.8599067,-46.7982757)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          estado<-tolower(estado)
          #url<-paste("~/Google Drive/projetos/ex shape/dados/",estado,".rda",sep = "")
          #url<-paste("C:/Users/felly/Google Drive/projetos/ex shape/dados/",estado,".rda",sep = "")
          url<-paste(path,estado,".rda",sep = "")
          load(url)
          dados<-subset(l,l$dd==input$slider)
          color<-dados$color
          colorL<-c("green","orange","red")
          labelL<-c("No risk","Moderate","High")
          
          if(input$riskHeat == "wheatblast"){
            color<-dados$color
            colorL<-c("green","orange","red")
            labelL<-c("No risk","Moderate","High")
          }else{
            if(input$riskHeat == "precipitation"){
              variavel <-dados$chuva
              colors <- brewer.pal(9, "Blues") #set breaks for the 9 colors 
              #brks<-classIntervals(variavel, n=9, style="quantile")
              #brks<- brks$brks #plot the map
              brks<-c(0,5,15,25,35,45,55,100)
              color<-colors[findInterval(variavel, brks,all.inside=TRUE)]
            }else{
              if(input$riskHeat == "temperature"){
                variavel <-dados$temp
                color <- brewer.pal(9, "Reds") #set breaks for the 9 colors 
                brks<-c(0,5,10,15,20,25,30,35)
                color<-color[findInterval(variavel, brks,all.inside=TRUE)]
              }else{
                if(input$riskHeat == "humidity"){
                  variavel <-dados$ur
                  color <- brewer.pal(9, "Purples") #set breaks for the 9 colors 
                  #brks<- brks$brks #plot the map
                  brks<-c(10,20,30,40,50,60,70,80,90,100)
                  color<-color[findInterval(variavel, brks,all.inside=TRUE)]
                }
              }
            }
          }
          
          
          try(
            leafletProxy("heatmap", data = dados)%>%
              clearShapes() %>%
              clearPopups() %>%
              addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
              )%>%
              setView(lat = coord[1], lng = coord[2] , zoom = 6) %>%
              addRectangles(
                lng1=~lng-0.075, lat1=~lat-0.075,
                lng2=~lng+0.075, lat2=~lat+0.075,
                color=color,
                noClip=FALSE,
                weight = 0)
          )
        }else{
          url <- pathHeat
          if(input$sourceMapsHeat == "BMD"){
            load(paste0(url,"bd_forecast_bmd"))
          }else{
            load(paste0(url,"bd_forecast_darksky"))
          }
          
          dados<-subset(forecast,forecast$dd== input$slider)
          
          color<-dados$color
          colorL<-c("green","orange","red")
          labelL<-c("No risk","Moderate","High")
          
          if(input$riskHeat == "wheatblast"){
            color<-dados$color
            colorL<-c("green","orange","red")
            labelL<-c("No risk","Moderate","High")
          }else{
            if(input$riskHeat == "precipitation"){
              variavel <-dados$chuva
              colors <- brewer.pal(9, "Blues") #set breaks for the 9 colors 
              #brks<-classIntervals(variavel, n=9, style="quantile")
              #brks<- brks$brks #plot the map
              brks<-c(0,5,15,25,35,45,55,100)
              color<-colors[findInterval(variavel, brks,all.inside=TRUE)]
            }else{
              if(input$riskHeat == "temperature"){
                variavel <-dados$temp
                color <- brewer.pal(9, "Reds") #set breaks for the 9 colors 
                brks<-c(0,5,10,15,20,25,30,35)
                color<-color[findInterval(variavel, brks,all.inside=TRUE)]
              }else{
                if(input$riskHeat == "humidity"){
                  variavel <-dados$ur
                  color <- brewer.pal(9, "Purples") #set breaks for the 9 colors 
                  #brks<- brks$brks #plot the map
                  brks<-c(10,20,30,40,50,60,70,80,90,100)
                  color<-color[findInterval(variavel, brks,all.inside=TRUE)]
                }
              }
            }
          }
          
          if(input$sourceMapsHeat =="BMD"){
            mlong <- 0.04364
            mlat <- 0.040
          }else{
            mlong <- 0.0575
            mlat <- 0.0575
          }
          try(
            leafletProxy("heatmap", data = dados)%>%
              clearShapes() %>%
              clearPopups() %>%
              # clearMarkers() %>%
              addRectangles(
                lng1=~long-mlong, lat1=~lat-mlat,
                lng2=~long+mlong, lat2=~lat+mlat,
                color=color,
                noClip=FALSE,
                weight = 0)
          )
          
          
        }
      )
      
    
  })
  
  #  observeEvent(input$heatmap_click, {
  #    ## Get the click info like had been doing
  #    click <- input$heatmap_click
  #    clat <- click$lat
  #    clng <- click$lng
  #    address <- revgeocode(c(clng,clat))
  #    leafletProxy("heatmap", data = dados)%>%
  #      addAwesomeMarkers(lat=clat,lng=clng)
  #  })
  
  
  output$statesMaps = renderUI({
    if(input$locale == "BR"){
      choicesEstadosMaps<-c("DF","GO","MG","MT",
                            "MS","PR","RS","SC","SP")
      names(choicesEstadosMaps)<-c("Distrito Federal",
                                   "Goiás","Minas Gerais","Mato Grosso",
                                   "Mato Grosso do Sul","Paraná","Rio Grande do Sul",
                                   "Santa Catarina",
                                   "São Paulo")
      selected = "MG"
    }
    if(input$locale == "BD"){
      choicesEstadosMaps<-c("BL","BS","CD","DJ","DK","FR","JS","MD","MH","PT","RS")
      names(choicesEstadosMaps)<-c("Bhola District","Barisal District","Chuadanga District",
                                   "Dinajpur District","Dhaka District",
                                   "Faridpur District","Jessore District","Madaripur District",
                                   "Meherpur District",
                                   "Patuakhali District","Rajshahi District")
    }
    choicesEstadosMaps<-sort(choicesEstadosMaps)
    
    selectInput('statesMaps', 'State', choicesEstadosMaps)
    
  })
  
  output$statesMapsHeat = renderUI({

    choicesEstadosMapsHeat<-c("DF","GO","MG","MT",
                              "MS","PR","RS","SC","SP")
    names(choicesEstadosMapsHeat)<-c("Distrito Federal",
                                     "Goiás","Minas Gerais","Mato Grosso",
                                     "Mato Grosso do Sul","Paraná","Rio Grande do Sul",
                                     "Santa Catarina",
                                     "São Paulo")
    
    selectInput('statesMapsHeat', 'State', choicesEstadosMapsHeat)

  })
  output$sourceMapsHeat = renderUI({
    
    choicesSourceMapsHeat<-c("BMD","DK")
    names(choicesSourceMapsHeat)<-c("BMD","Darksky")
    
    selectInput('sourceMapsHeat', 'Source', choicesSourceMapsHeat,selected = "BMD")
    
  })
  output$estacoes = renderUI({
    try(
      if(!is.null(input$pais)){
        if(input$pais == "BD" && !is.element(input$states,bdStates))
          estado = "BL"
        else
          if(input$pais == "BR" && !is.element(input$states,choicesEstados))
            estado = "DF"
          else
            estado =input$states
      }
    )
    pais = input$pais
    if(input$pais == "BR"){
      if(!is.null(input$organization)){
        if(input$states == "PR")
          org = input$organization
        else
          org = "INMET"
      }
      else{
        org = "INMET"
      }
    }else{
      org = "BMD"
    }
    choicesEstacoes<-""
    try(
      choicesEstacoes<-loadEstacoes(pais,estado,org)
    )
    selectInput('estacoes', 'Station', choicesEstacoes)
  })
  
  output$stationMaps = renderUI({
    try(
      if(!is.null(input$statesMaps)){
        if(input$statesMaps == "BD" && !is.element(input$statesMaps,bdStates))
          estado = "BL"
        else
          if(input$statesMaps == "BR" && !is.element(input$statesMaps,choicesEstados))
            estado = "DF"
          else
            estado =input$statesMaps
      }
    )
    pais = input$locale
    if(pais == "BR"){
      org = "INMET"
    }else{
      org = "BMD"
    }
    choicesEstacoesMap <-""
    try(
      if(pais != "BD"){
      choicesEstacoesMap<-loadEstacoes(pais,estado,org)
      }else{
        auxs1 <- loadEstacoes(pais,stationsBD[1],org)
        for(i in 2:length(stationsBD)){
          auxs2 <- loadEstacoes(pais,stationsBD[i],org)
          auxs1<-c(auxs1,auxs2)
        }
        choicesEstacoesMap <-auxs1
      }
      
    )
    #selectInput('stationMaps', 'Station', choicesEstacoesMap,multiple = TRUE)
    pickerInput(inputId = "stationMaps", 
                label = "Station", 
                choices = choicesEstacoesMap, options = list(`actions-box` = TRUE,`none-selected-text` = "All stations"), 
                multiple = TRUE)
    
  })
  
  output$slider<-renderUI({
    shinyjs::hide("startingDateMapHeat")
    
    if(input$localeHeat == "BR"){
      maxi <-as.Date(min(l$dd)+11,"%m-%d")
      mini<-as.Date(min(l$dd),"%m-%d")
    }
    if(input$localeHeat == "BD"){
      url <- pathHeat
      if(input$sourceMapsHeat == "BMD"){
        load(paste0(url,"bd_forecast_bmd"))
        maxi <-as.Date(min(forecast$dd)+6,"%m-%d")
        mini<-as.Date(min(forecast$dd)+1,"%m-%d")
      }else{
        load(paste0(url,"bd_forecast_darksky"))
        maxi <-as.Date(min(forecast$dd)+3,"%m-%d")
        mini<-as.Date(min(forecast$dd)+1,"%m-%d")
      }

      
    }
    sliderInput("slider", "Day", min = mini,
                max = maxi,
                value=mini,
                width=320,
                timeFormat="%d-%m")
    
  })
  
  output$organization<-renderUI({
    try(
      if(!is.null(input$pais)){
        if(input$pais == "BD" && !is.element(input$states,bdStates))
          estado = "BL"
        else
          if(input$pais == "BR" && !is.element(input$states,choicesEstados))
            estado = "DF"
          else
            estado =input$states
      }
      
    )
    pais = input$pais
    if(estado == "PR" && pais =="BR"){
      choicesOrg <-c("INMET","SIMEPAR")
      names(choicesOrg)<-c("INMET","SIMEPAR")
      selectInput('organization', 'Organization', choicesOrg)
    }
  })
  
  output$grafico <- renderPlot({ 
    if(!is.null(input$estacoes)){
        ## capturando os dados da tela
        codigo_station<-NULL
        codigo_station <-input$estacoes
        dateInterface<-input$startingDateRS
        pais <- input$pais
        incProgress(1, detail = paste("Plotting..."))
        try(
          calcAndGraph(codigo_station,dateInterface,pais)
        )
      
    }
  })
  output$grafico0 <- renderPlotly({ 
    if(!is.null(input$estacoes)){
      ## capturando os dados da tela
      codigo_station<-NULL
      codigo_station <-input$estacoes
      dateInterface<-input$startingDateRS
      pais <- input$pais
      incProgress(1, detail = paste("Plotting..."))
      try(
        calcAndGraph2(codigo_station,dateInterface,pais)
      )
      
    }
  })
  
  
  output$grafico2 <- renderPlot({ 
    #print("OI DO GRAFICOOO")
    if(!is.null(input$estacoes)){
      withProgress(message = 'Processing', value = 0, {
        ## capturando os dados da tela
        codigo_station <-input$estacoes
        dateInterface<-input$startingDateRS
        pais <- input$pais
     
          graphClim(codigo_station,dateInterface,pais)

      })
    }
  })
  output$grafico3 <- renderPlotly({
    if(!is.null(input$estacoes)){
        ## capturando os dados da tela
        codigo_station <-input$estacoes
        dateInterface<-input$startingDateRS
        pais <- input$pais
        try(
          graphClim(codigo_station,dateInterface,pais)
        )
 
    }
  })
  
  printDT <- reactive ({
    codigo_station <-input$estacoes
    dateInterface<-input$startingDateRS
    pais <- input$pais
    d<-exploreData(codigo_station,dateInterface,pais)
    return(d)
  })
  #  output$datatable <-DT::renderDataTable({
  #    if(!is.null(input$estacoes)){
  #      withProgress(message = 'Processing', value = 0, {
  #        ## capturando os dados da tela
  #        ,options = list(pageLength = 5)
  #      })
  #    }
  #    
  ##  })
  output$datatable = DT::renderDataTable(
    printDT(), options = list(pageLength = 10,scrollX=TRUE,dom = 'tp')
  )
  
  observeEvent(input$insert, {
    if(!is.na(input$longmarker) && !is.na(input$latmarker)){
      lat = as.double(input$latmarker)
      long = as.double(input$longmarker)
      col = input$colormarker
      name = input$namemarker
      js$saveCache(name,lat,long,col)
      
      date<-Sys.Date()
      dia <-format(date,"%d")
      dia <-as.integer(dia)%%2
      if(dia != 0){
        apiKey<-"e05aea7011ee95597b918eba7ac5ee3f"
      }else{
        apiKey <-"f65ec68580b6d7112187b817d26be957"
      }
      Sys.setenv(DARKSKY_API_KEY = apiKey)

      df<-get_current_forecast(latitude = lat,longitude = long)
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
      
      
      icons <- awesomeIcons(
        icon = 'ios-plus',
        iconColor = 'black',
        library = 'ion',
        markerColor = col
      )
      
      lab <- paste0("<strong>",name,"</strong><br>",
                    df$date," - ",df$time2,"<br>",
                    "Latitude: ",lat,"<br>",
                    "Longitude: ",long,"<br>",
                    "Temperature: ",df$temperature," C<br>",
                    "Rainfall: ",df$precipIntensity," mm<br>",
                    "Humidity: ",df$humidity," %<br>")
      #lat = -28.2571616
      #long = -52.417535
      try(
        leafletProxy("heatmap")%>%
          #clearMarkers() %>%
          addAwesomeMarkers(lat=lat,lng=long,icon = icons,label =name,popup =lab  )
      )
    }
  })
  
  
  observe({
    req(input$locale)
    sel <-input$locale
    updateSelectInput(session, 'localeHeat', selected = sel)
    updateSelectInput(session, 'pais', selected = sel)
  })
  
  observe({
    req(input$localeHeat)
    sel <-input$localeHeat
    updateSelectInput(session, 'locale', selected = sel)
    updateSelectInput(session, 'pais', selected = sel)
  })
  
  observe({
    req(input$pais)
    sel <-input$pais
    updateSelectInput(session, 'locale', selected = sel)
    updateSelectInput(session, 'localeHeat', selected = sel)
  })
  
  observe({
    req(input$statesMaps)
    sel <-input$statesMaps
    updateSelectInput(session, 'statesMapsHeat', selected = sel)
    updateSelectInput(session, 'states', selected = sel)
  })
  
  observe({
    req(input$statesMapsHeat)
    sel <-input$statesMapsHeat
    updateSelectInput(session, 'statesMaps', selected = sel)
    updateSelectInput(session, 'states', selected = sel)
  })
  
  observe({
    req(input$states)
    sel <-input$states
    updateSelectInput(session, 'statesMaps', selected = sel)
    updateSelectInput(session, 'statesMapsHeat', selected = sel)
  })
  
  observe({
    req(input$startingDateMap)
    sel<-input$startingDateMap
    updateDateInput(session, "startingDateRS", value = sel)
  })
  
#  observe({
#    if(!input$showAll){
#      shinyjs::show("stationMaps")
#    }else{
#      shinyjs::hide("stationMaps")
#    }
#  })
  observe({
    if(input$showMarker){
      shinyjs::show("namemarker")
      shinyjs::show("latmarker")
      shinyjs::show("longmarker")
      shinyjs::show("colormarker")
      shinyjs::show("insert")
    }else{
      shinyjs::hide("namemarker")
      shinyjs::hide("latmarker")
      shinyjs::hide("longmarker")
      shinyjs::hide("colormarker")
      shinyjs::hide("insert")
    }
  })
  
  observeEvent(input$localeHeat, {
    if(input$localeHeat == "BR"){
      shinyjs::show("statesMapsHeat")
      shinyjs::show("slider")
      shinyjs::hide("sourceMapsHeat")
      #shinyjs::show("riskHeat")
      #shinyjs::show("insert")
      #shinyjs::show("latLong")
      #shinyjs::show("latLong")
    }
    if(input$localeHeat == "BD"){
      
      #shinyjs::hide("riskHeat")
      shinyjs::hide("statesMapsHeat")
      #shinyjs::hide("insert")
      #shinyjs::hide("latLong")
      shinyjs::show("sourceMapsHeat")
    }
    
  })
  
  output$dataComp <- renderText({ 
    if(input$pais == "BR"){
      msg<-"* Data courtesy of the Brazil Meteorological Department (INMET)"
    }else{
      msg<-"* Data courtesy of the Bangladesh Meteorological Department"
    }
    msg
  })
  
  observeEvent(input$todayDate, {
    dd <- Sys.Date()
    updateDateInput(session, "startingDateRS", label = "Starting date:", value = dd)
  })
  observeEvent(input$todayDateM, {
    dd <- Sys.Date()
    updateDateInput(session, "startingDateMap", label = "Day", value = dd)
  })
  
  observeEvent(input$locale, {
    #print("AEAFAEFAFE")
    if(input$locale == "BR"){
      shinyjs::show("statesMaps")
    }else{
      shinyjs::hide("statesMaps")
    }
  })
  
  dataModal <- function(failed = FALSE) {
    modalDialog(
      title = "Spore Cloud model parameters",
      numericInput("DSPL","Daily Spore Production",DSPL),
      numericInput("MSCD","Maximun Spore Clouds Density",MSCD),
      numericInput("II","Initial Inoculum",II),
      numericInput("LP","Latent Period",LP),
      numericInput("IP","Infection Period",IP),
      numericInput("WT","Wetness Threshold",WT),
      numericInput("AREALEAF","Leaf Area",AREALEAF),
      numericInput("SPO2P","Spore Proportion that moves from Organ cloud to Plant cloud",SPO2P),
      numericInput("SPP2F","Spore Proportion that moves from Plant cloud to Field cloud",SPP2F),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  #DSPL <- 10000
  #MSCD <- 10000
  #II <- 100
  #LP <- 6
  #IP <- 20
  #WT <- 1
  observeEvent(input$showP, {
    #print(DSPL)
    showModal(dataModal())
  })
  observeEvent(input$ok, {
    # Check that data object exists and is data frame.
      DSPL<-input$DSPL
      MSCD<-input$MSCD
      II<-input$II
      LP<-input$LP
      IP<-input$IP
      WT<-input$WT
      AREALEAF<-input$AREALEAF
      SPO2P<-input$SPO2P
      SPP2F<-input$SPP2F
      
      assign("DSPL", DSPL, envir = .GlobalEnv)
      assign("MSCD", MSCD, envir = .GlobalEnv)
      assign("II", II, envir = .GlobalEnv)
      assign("LP", LP, envir = .GlobalEnv)
      assign("IP", IP, envir = .GlobalEnv)
      assign("WT", WT, envir = .GlobalEnv)
      assign("AREALEAF", AREALEAF, envir = .GlobalEnv)
      assign("SPO2P", SPO2P, envir = .GlobalEnv)
      assign("SPP2F", SPP2F, envir = .GlobalEnv)
      removeModal()
      output$grafico0 <- renderPlotly({ 
        if(!is.null(input$estacoes)){
          ## capturando os dados da tela
          codigo_station<-NULL
          codigo_station <-input$estacoes
          dateInterface<-input$startingDateRS
          pais <- input$pais
          incProgress(1, detail = paste("Plotting..."))
          try(
            calcAndGraph2(codigo_station,dateInterface,pais)
          )
          
        }
      })
  })
  
  output$image1 <- renderImage({
    return(list(
      src = "logos/American_Red_Cross.png",
      filetype = "image/png",
      alt = "This is a chainring"
    ))
    
  }, deleteFile = FALSE)
})
