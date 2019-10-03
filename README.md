# wheat-blast-model

To run the shiny APP:
1) Go to the sporeCloud folder and run compile.sh to create the executable for C++ spore cloud generic model we using gcc to compile.
  To compile the model use the terminal and run this command:
   ./compile.sh

2) Go to the wheatblast folder and change de paths on server.R to access the RDAs files that contains forecast datas...all RDA files are in "dados" folder. We using RDA's files because R can read more quickly.
Next go to the global.R and change de sporecloud path (where did you put the model executable. If you used the compile file the executable name will be teste2).

On the global.R you will find all the model subroutines.

loadEstacoes = get the stations from API.

loadData = get weather data form API

loadDataRh90 = get weather data from API and calculates the amount of hours with humidity above 90 for spore cloud model

OBS: to use api http://dev.sisalert.com.br/apirest/api/v1/data/station/codStation/range/inicialDate(MM-DD-YYYY)/endDate(MM-DD-YYYY)

sporeCloud = run spore cloud generic model

diasFavInfe = test if the day is favorable to infection

calcAndGraph2 = run wheat blast model and generates interactive graphs.

graphClim = get the historical weather data and generates interactive graphs

hcp = build up the Inoculum


To run forecast script:
1) you need to create a api key from darksky.net and the darksky package
2) you need to set the path to shapefile directory (shapefile includes) 
3) We using around 900 points

For spore cloud model
There is 2 json file: one for weather data and one for disease data
For output the model generates a file named spore.out
