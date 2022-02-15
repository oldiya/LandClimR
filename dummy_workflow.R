library(devtools)
load_all(".")


configDF <-  LandClimR::default_config
configDF$InputConfiguration_TreeInitStateFile <- ""
configDF$InputConfiguration_InputPath <- "" 
configDF$InputConfiguration_HarvestStandsFile<- ""
configDF$InputConfiguration_HarvestManagementAreasFile<- ""
configDF$ClimateConfiguration_ClimateFile<- "climate.txt"

configDF$OutputConfiguration_Light_Coordinates_Coordinate_Rows <- "0"
configDF$OutputConfiguration_Light_Coordinates_Coordinate_Cols <- "0"

configDF$OutputConfiguration_Climate_Coordinates_Coordinate_Rows <- "0"
configDF$OutputConfiguration_Climate_Coordinates_Coordinate_Cols <- "0"






configLandClim(overwrite=TRUE, decadal=FALSE, config = configDF, 
               barkbeetle= LandClimR::default_barkbeetle,
               landtypeparameters= LandClimR::default_landtypeparameters,
               plantingparameters= LandClimR::default_plantingparameters,
               randomstate= LandClimR::default_randomstate,
               species= LandClimR::default_species)

landClimpath <- "C:/Users/abbash/Desktop/landclim/build/Release/netcoreapp3.1/publish"

climateTable <- matrix(data=1:25, nrow=1, ncol= 25)

LandClimR::write.climate(filename = "workspace/climate.txt", latitude = 12, 
                         elevation = 15, climate = climateTable)
LandClimR::writeLandscape(ySize=100, xSize=100, cellSize=25, 
                          coor = "+init=epsg:4326", altitude = rep(10, 16), slop = rep(10, 16), 
                          so = rep(10, 16), landt=rep(10, 16), asp=rep(10, 16), fullpath = "C:/Users/abbash/Desktop/landclimr/workspace")

LandClimR::runLandClim("workspace/config.xml",
                       binPath = landClimpath)
