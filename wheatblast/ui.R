library(shinyalert)
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Wheat Blast-EWS",titleWidth = 210),
  dashboardSidebar(
    width = 210,
    tags$head(
      HTML(
        "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 150000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
      ),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      )
    ),
    useShinyjs(),
    useShinyalert(),  # Set up shinyalert
    sidebarMenu(id="m",
      menuItem("Home", tabName = "home", icon = icon("home")),
      #menuItem("How to use this EWS", tabName = "useEWS", icon = icon("question-circle")),
      menuItem("About this model", tabName = "aboutModel", icon = icon("info-circle")),
      menuItem("Observed risk map", tabName = "interactiveMap", icon = icon("map")),
      menuItem("Observed risk data", tabName = "monitoring", icon = icon("bar-chart")),
      menuItem("Forecast risk map", tabName = "heatMap", icon = icon("cloud")),
      menuItem("Crop and Disease Model", tabName = "cropmodel", icon = icon("info")),
      #menuItem("Contact", tabName = "contactUs", icon = icon("envelope")),
      conditionalPanel("input.m == 'interactiveMap'",
                       br(),
                       h5("Data Input"),
                       selectInput("locale", "Country", locales),
                       uiOutput('statesMaps'),
                       try(
                       uiOutput('stationMaps')),
                       dateInput("startingDateMap", "Day", value=startingDate,format="yyyy-mm-dd"),
                       br(),
                       actionButton("todayDateM", "Today's Date")
                       #checkboxInput("showAll", "Show all stations", TRUE)
      ),
      conditionalPanel("input.m == 'heatMap'",
                       br(),
                       extendShinyjs(text = jsGetCache),
                       extendShinyjs(text = jsSaveCache),
                       h5("Data Input"),
                       selectInput("localeHeat", "Country", locales),
                       uiOutput('statesMapsHeat'),
                       selectInput("riskHeat", "Layer selection", choicesRisk,selected = "wheatblast"),
                       dateInput("startingDateMapHeat", "Day", value=startingDate,format="yyyy-mm-dd"),
                       uiOutput('sourceMapsHeat'),
                       uiOutput("slider"),
                       textInput("namemarker", "Name", ""),
                       textInput("latmarker", "Latitude", ""),
                       textInput("longmarker", "Longitude", ""),
                       selectInput("colormarker", "Color", c("blue","orange",
                                                              "white","purple",
                                                              "black","brown"),selected = "blue"),
                       actionButton("insert", "Insert marker"),
                       checkboxInput("showMarker", "Show insert marker", FALSE)
                       
      ),
      conditionalPanel("input.m == 'monitoring'",
                       br(),
                       h5("Data Input"),
                       selectInput("pais", "Country", 
                                   choices=coichesPais), 
                       uiOutput('states'),
                       uiOutput('organization'),
                       uiOutput('estacoes'),
                       dateInput("startingDateRS", "Starting date:", value=startingDate,format="yyyy-mm-dd"),
                       br(),
                       div(class="btn-group mr-2",
                           actionButton("todayDate", "Today's Date")#,
                          # actionButton("showP", "Parameters")
                       )
      )
    ),
    #sidebarMenuOutput("menu"),
    tags$head(
      # Include our custom CSS
      includeCSS("styles.css")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(
      '.myClass { 
        font-size: 20px;
        line-height: 45px;
        text-align: center;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
    #tags$script(HTML('
    #  $(document).ready(function() {
    #    $("header").find("nav").append(\'<span class="myClass"> Wheat blast disease risk assessment and early warning system</span>\');
    #  })
    # ')),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                column(width = 12,
                       div(id = "PCA_discussion", class = "card",
                           h2("Welcome"),
                           hr(),
                           div(id = "formula", class = "formula",
                               img(src ="Top2.png",width = "100%", height = "100%" ,class="responsive2")),
                           br(),
                           br(),
                           p(includeText("home.txt")),
                          br(),
                          div(id = "formula", class = "formula",
                              #img(src ="Pic.jpg",class="responsive2")),)
                              img(src ="Pic3.png",class="responsive2")),
                          br(),
                          br(),
                          div(id = "formula", class = "formula",
                              img(src ="Bttom2.png",class="responsive2")),
                          br(),
                          hr()
                        )
                )

              )
      ),
      tabItem(tabName = "useEWS",
              fluidRow(
                column(width = 12,
                       div(id = "PCA_discussion", class = "card",
                           h2("How to use EWS"),
                           hr(),
                           #p(includeText("home.txt")),
                           # p("Wheat blast disease risk assessment and early warning system is a tool developed by CIMMYT in partnership with UPF and Embrapa-Trigo."),
                           #column(width = 4,
                           #tags$img(src='scoll.png'),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           hr()
                       )
                )
                
              )
      ),
      tabItem(tabName = "aboutModel",
              fluidRow(
                column(width = 12,
                       div(id = "PCA_discussion", class = "card",
                           h2("About this model"),
                           hr(),
                           #p(includeText("home.txt")),
                            p("A wheat blast simulation model that accounts for inoculum build-up and infection has been validated in Brazil (Fernandes et al., 2017).  The prediction model was developed and evaluated based on the analysis of historical epidemics and weather series data in the northern Paraná state, Brazil. Local available epidemiological knowledge was also employed in model parameterization. Importantly the model also assumes the spatially uniform presence of MoT inoculum in the environment for which simulations are run."),
                            p("The disease and hourly-scale weather datasets examined by Fernandes et al., (2017) for Brazil encompassed the 2001–2012 period. A specific database management application (Farman et al., 2017) developed using R Shiny was programmed to visualize and identify patterns in weather variables during two major outbreaks (2004 and 2009). Uncommonly humid and warm weather was observed for most locations in this study during a 60-day period preceding wheat heading during years of major outbreaks. These conditions were therefore considered key drivers of inoculum build-up and airborne spores from regional inoculum sources in the surroundings."),
                            p("The prediction blast model has four components. The first component assumes spores are present and estimates the rate of conidiophore development as a function of temperature and relative humidity (Bregaglio and Donatelli, 2015), both of which are integrated to estimate blast inoculum potential by solving equation 1 for the hourly sum of inoculum potential (IP) over the season."),
                           #div(id = "logos", class = "logos", 
                           div(id = "formula", class = "formula",
                           img(src ="modelFormula.png", class="responsive")),
                           p("Where T and RH are air temperature and relative humidity, respectively. Where RH is below the threshold in Equation 1, the model does not accumulate thermal time. The model also calculates the development of a spore cloud subject to assumptions of air current uptake, atmospheric diffusion and wind shear that affect spore longevity. Survival of spores while airborne may also be affected by temperature, solar and ultraviolet radiation, in addition to relative humidity (Deacon, 2005). Spore cohorts were therefore assumed to have a half-life of three days within any seven-day window."),
                           p("The model also determines the number and timing of days with climatic conditions favoring blast infection using a conditional ruleset. Days favoring infection were consequently declared following spore cloud development when the daily maximum temperature exceeded 23C and temperature amplitude (calculated daily minimum temperature subtracted from daily maximum temperature) was > 13C, with mean daily RH above 70%. The model adequately described observed epidemic and non-epidemics years during and beyond the study period in Brazil."),
                           p("Hourly weather data for the state of Paraná, Brazil is collected from the SIMEPAR (Technological Institute Simepar) automated weather station network. While the automated weather station network from INMET (National Institute of Meteorology) provide hourly weather data for the whole country. The short term numerical weather forecast is provided by INPE (National Institute of Spacial Research) in a grid of 15 x 15 km."),
                           p("In Bangladesh, observed hourly weather data is obtained from BMD (Bangladesh Meteorological Department) automated weather station network and from CIMMYT.  The short term numerical weather forecast is provided by BMD in a grid of 17 x 17 km."),
                           br(),
                           br(),
                           h3("References"),
                           br(),
                           p("Bregaglio S, Donatelli M (2015) A set of software components for the simulation of plant airborne diseases. Environ Model Softw 72:426–444"),
                           br(),
                           p("Deacon, J. (2005) Fungal spores, spore dormancy, and spore dispersal, in fungal biology, 4th edition, Blackwell doi:10.1002/9781118685068.Chapter 10."),
                           br(),
                           p("Farman M., Peterson G.L., Chen L., Starnes J.H., Valent B., Bachi M.P., Murdock L.,  Hershman D.E., Pedley K.F., Fernandes J.M.C., Bavaresco J. (2017). The Lolium pathotype of Magnaporthe oryzae recovered from a single blasted wheat plant in the United States. Plant Dis 101:684–692."),
                           br(),
                           #div(class="header", checked=NA,
                           p(),
                           div(p(HTML(paste0('Fernandes, J. M. C., Nicolau, M., Pavan, W., Hölbig, C. A., Karrei, M., de Vargas, F., Tsukahara, R. Y. (2017). A weather-based model for predicting early season inoculum build-up and spike infection by the wheat blast pathogen. Tropical Plant Pathology. ',a(href = "https://doi.org/10.1007/s40858-017-0164-2","https://doi.org/10.1007/s40858-017-0164-2."))))),
                           #),
                           #column(width = 4,
                           #tags$img(src='scoll.png'),
                           br(),
                           br(),
                           br(),
                           hr()
                       )
                )
                
              )
      ),
      tabItem(tabName = "interactiveMap",
              fluidRow(
                column(width = 12,
              box(width = NULL,status = "primary", solidHeader = TRUE,
                  collapsible = FALSE,
              div(class="outer",
                  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                  try(
                  leafletOutput("map", width = "100%", height = "100%"))%>% withSpinner(color="#0dc5c1")
              )
      )
      )
      )
      ),
      tabItem(tabName = "heatMap",
              fluidRow(
                column(width = 12,
                       box(width = NULL,status = "primary", solidHeader = TRUE,
                           collapsible = FALSE,
                       div(class="outer",
                           tags$style(type = "text/css", "#heatmap {height: calc(100vh - 80px) !important;}"),
                           #useShinyjs(),
                           leafletOutput("heatmap", width = "100%", height = "100%")%>% withSpinner(color="#0dc5c1")
                           #combineWidgetsOutput("heatmap")
                       )
                )
                )
              )
      
      ),
      tabItem(tabName = "monitoring",
              fluidRow(
              column(width = 12,
                     box(width = NULL,status = "primary", solidHeader = TRUE,
                         collapsible = FALSE,
  
                         tabsetPanel(
                           #tabPanel("Risk factor",br(),plotOutput("grafico",height = "600px" , width = "100%")%>% withSpinner(color="#0dc5c1")),
                           tabPanel("Risk factor",br(),plotlyOutput("grafico0",height = "700px" , width = "100%")%>% withSpinner(color="#0dc5c1")),
                           #tabPanel("Weather data1",br(),plotOutput("grafico2",height = "500px" ,width = "100%")),
                           tabPanel("Weather data graphs",br(),plotlyOutput("grafico3",height = "600px" ,width = "100%")%>% withSpinner(color="#0dc5c1")),
                           tabPanel("Weather data explorer",DT::dataTableOutput("datatable")%>% withSpinner(color="#0dc5c1"))
                         ),
                         h6(textOutput("dataComp"))
                     )
              )
              )

      ),
      tabItem(tabName = "cropmodel",
              fluidRow(
                column(width = 12,
                       div(id = "PCA_discussion", class = "card",
                           h2("Crop and Disease Model background"),
                           hr(),
                           p("Crop models can be useful tools to study crop and environmental interactions in a virtual setting. Crop models applications include study of the impacts of environmental factors such as climate change, climate variability, drought, and heat stress on crop production at different spatial and temporal scales. Alongside traditional field research, an improved understanding of the effects of these factors can be achieved using crop models capable of simulating different scenarios either for a single environmental factor (Asseng, 2015) or a combination of factors (Asseng et al., 2004). Models also permit researchers to simulate environmental and crop management effects at several locations or different crop developmental phases, or both. This part of the wheat blast early warning system integrates the disease environmental suitability assessments described by Fernandes et al. (2017) with a dynamic crop simulation model to predict specific periods of crop development when wheat may be most susceptible to disease infection."),
                           p("Nwheat is a process-based simulation model that simulates wheat crop growth and development as a function of weather, soil characteristics, and crop management. Typically, Nwheat model estimates times at which specific growth stages are attained (e.g., flowering and physiological maturity) and biomass weight of the wheat plant components (e.g., leaves, stems, roots, and grains) as they change over time during the course of the cropping season. Nwheat also simulates the effects of changes in soil moisture content and nutrient status on these and other measurements. NWheat was recently embedded in the widely used simulation model ‘Decision Support System for Agrotechnology Transfer’, or  DSSAT, Version 4.7 (Hoogenboom, et al., 2017). Nwheat also been has been tested under a number of crop production environments with different in temperature regimes, carbon dioxide levels, and variable soil nitrogen and moisture conditions (Kassie et al., 2016)."),
                           p("Wheat blast, caused by Magnaporthe oryzaepathotype Triticum, is a potentially severe disease that is established in South America and in Bangladesh (Cruz and Valent, 2017). Infections vary as a function of differing weather conditions, crop cultivars, location and time. The DSSAT-Nwheat model is currently under developer improvement to enable simulations that account for biotic stresses to crop growth, most notably for the damage imposed by diseases and pests. By coupling the Nwheat crop growth model with environmental disease suitability assessments, model users will eventually be able to explore the effect of a range of sowing dates, weather conditions, and cultivar effects on wheat blast infection. This will ultimately permit researchers to develop improved agronomic management recommendations to mitigate the effects of wheat blast in farmers’ fields. Here, we introduce a novel work to integrate the DSSAT-Nwheat simulation model with the weather-data driven wheat blast simulation model described by Fernandes et al. (2017) in both Brazil and Bangladesh."),
                           p("The dynamic linkage between wheat blast infection and resulting crop injuries and the wheat crop is by coupling points between the wheat blast model and the wheat model. Coupling points are, places where the values of state variables – the variables that represent the state of the physical quantities being modeled – can be exchanged with other models. Examples of coupling point variables in this study include leaf mass or area and seed mass or number, both of which might be negatively impacted by wheat blast.  By identifying specific mechanical and infection damage pathways, and rates of damage using these variables, growth models can be tuned to quantify how crop development and yield might be affected."),
                           div(id = "formula", class = "formula",
                               img(src ="modelCoupling.png",width = "100%", height = "100%" ,class="responsive2")),
                           h3("References"),
                           br(),
                           p("Asseng, S., Jamieson, P.D., Kimball, B., Pinter, P. and Sayre, K. (2004). Simulated wheat growth affected by rising temperature, increased water deficit and elevated atmospheric CO 2. 85: 85–102. doi: 10.1016/S0378-4290(03)00154-0."),
                           br(),
                           p("Asseng, S. (2015). Rising temperatures reduce global wheat production. 5(December 2014): 143–147. doi: 10.1038/NCLIMATE2470."),
                           br(),
                           p("Cruz, C. D. and Valent, B. (2017). Wheat blast disease: danger on the move. Trop Plant Pathol. doi:10.1007/s40858-017-0159-z."),
                           br(),
                           div(p(HTML(paste0('Fernandes, J. M. C., Nicolau, M., Pavan, W., Hölbig, C. A., Karrei, M., de Vargas, F., Tsukahara, R. Y. (2017). A weather-based model for predicting early season inoculum build-up and spike infection by the wheat blast pathogen. Tropical Plant Pathology. ',a(href = "https://doi.org/10.1007/s40858-017-0164-2","https://doi.org/10.1007/s40858-017-0164-2."))))),
                          br(),
                          p("Hoogenboom, G., C.H. Porter, V. Shelia, K.J. Boote, U. Singh, J.W. White, L.A. Hunt, R. Ogoshi, J.I. Lizaso, J. Koo, S. Asseng, A. Singels, L.P. Moreno, and J.W. Jones. (2017). Decision Support System for Agrotechnology Transfer (DSSAT) Version 4.7 (https://DSSAT.net). DSSAT Foundation, Gainesville, Florida, USA."),
                          br(), 
                          div(p(HTML(paste0('Kassie, B. T., Asseng, S., Porter, C. H., & Royce, F. S. (2016). Performance of DSSAT-Nwheat across a wide range of current and future growing conditions. Eur. J Agron. ',a(href = "https://doi.org/10.1016/j.eja.2016.08.012 ","https://doi.org/10.1016/j.eja.2016.08.012."))))),
                          br(),br(), hr(),br(),br(),br(),br(),
                          actionLink(inputId='ab1', label="Go to Crop model", 
                                      #icon = icon("th"), 
                                      onclick ="window.open('http://dev.sisalert.com.br/shiny/cropwheat/', '_blank')")
                       )
              
                )
              )
      ),
      tabItem(tabName = "contactUs",
              fluidRow(column(width = 12,
                              box(width = NULL, solidHeader = TRUE,
                                  collapsible = FALSE,
                                  div(id = "PCA_discussion", class = "card",
                                      h2("Contact us!"),
                                      hr(),
                                  textInput("textFirstNameContact", label = "First name:", value = ""),
                                  textInput("textLastNameContact", label = "Last name:", value = ""),
                                  textInput("textEmailContact", label = "Email:", value = ""),
                                  textAreaInput("textAreaContact",height = "100px",label = "Message", value = ""),
                                  actionButton("buttonBack", " Cancel"),
                                  actionButton("buttonSend", " Send")
                                  )
                                  )
                          ))
      )
    )
  )
)
