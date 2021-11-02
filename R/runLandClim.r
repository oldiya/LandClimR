################################################################################
# Title: Core functions from LandClimR
# Aim: Run the model LandClim with the user defined setup
#
################################################################################


default_barkbeetle <- read.csv("R/default-data/barkbeetle.csv",
                               fileEncoding = "UTF-8-BOM", header = T)
default_config <- read.csv("R/default-data/config.csv",
                           fileEncoding = "UTF-8-BOM", header = T)
default_landtypeparameters <- read.csv("R/default-data/landtypeparameters.csv",
    fileEncoding = "UTF-8-BOM", header = T)
default_plantingparameters <-read.csv("R/default-data/plantingparameters.csv",
    fileEncoding = "UTF-8-BOM", header = T)
default_randomstate <- read.csv("R/default-data/randomstate.csv",
                                fileEncoding = "UTF-8-BOM", header = T)
default_species <- read.csv("R/default-data/species.csv",
                            fileEncoding = "UTF-8-BOM", header = T)

# Important: if the XML path file already exists, the provided configuration
# will not be processed. Please ensure that the XML file path does not exist
# if you want to change anything in the configuration of a particular XML
# LandClimR does not overwrite any XML file. It creates new files only.
# The is needed to ensure that existing XMLs are not overwritten

runLandClim <- function(config = data.frame(),
                        configXMLpath = "",
                        barkbeetle = data.frame(),
                        barkbeetleXMLpath = "", # path with name of the file and .xml
                        landtypeparameters = data.frame(),
                        landtypeparametersXMLpath = "",
                        plantingparameters = data.frame(),
                        plantingparametersXMLpath = "",
                        randomstate = data.frame(),
                        randomstateXMLpath = "",
                        species = data.frame(),
                        speciesXMLpath = "",
                        workspacePath = "workspace",
                        consoleOutputPath = "",
                        binPath = "bin") {
  #print("asd")
  if (length(config) == 0){config <- default_config}
  if (length(barkbeetle) == 0){barkbeetle <- default_barkbeetle}
  if (length(landtypeparameters) == 0){landtypeparameters <- default_landtypeparameters}
  if (length(plantingparameters) == 0){plantingparameters <- default_plantingparameters}
  if (length(randomstate) == 0){randomstate <- default_randomstate}
  if (length(species) == 0){species <- default_species}

  if (configXMLpath == "")
    {configXMLpath <- paste0(workspacePath, "config.xml")}
  if (barkbeetleXMLpath == "")
    {barkbeetleXMLpath <- paste0(workspacePath, "barkbeetle.xml")}
  if (landtypeparametersXMLpath == "")
    landtypeparametersXMLpath <- paste0(workspacePath, "landtype.xml")
  if (plantingparametersXMLpath == "")
    plantingparametersXMLpath <- paste0(workspacePath, "planting.xml")
  if (randomstateXMLpath == "")
    randomstateXMLpath <- paste0(workspacePath, "randomstate.xml")
  if (speciesXMLpath == "")
    speciesXMLpath <- paste0(workspacePath, "species.xml")

  if (!is.null(consoleOutputPath)) {
      if (consoleOutputPath == "") {
        tempStr <- strsplit(tempdir(), "\\\\")
        consoleOutputPath <- paste0(configXMLpath, "_consoleOutput_",
                                    tempStr[[1]][[length(tempStr[[1]])]], ".txt")
      }
  }


  if (!dir.exists(workspacePath))
    dir.create(workspacePath, recursive = TRUE)

  writeXML(data = config, type = "config", filePathXML = configXMLpath)
  writeXML(barkbeetle, "barkbeetle", barkbeetleXMLpath)
  writeXML(landtypeparameters, "landtypeparameters", landtypeparametersXMLpath)
  writeXML(plantingparameters,"plantingparameters",plantingparametersXMLpath)
  writeXML(randomstate, "randomstate", randomstateXMLpath)
  writeXML(species, "species", speciesXMLpath)

  if (tolower(.Platform$OS.type) == "unix")
    system2(paste0( binPath, '/LandClim'),
            args = paste0(getwd(), "/", configXMLpath))
           # , stdout = consoleOutputPath)
  else
    system2(paste0(binPath, '/LandClim.exe'),
            args = paste0(getwd(), "/", configXMLpath), stdout=consoleOutputPath)
}


