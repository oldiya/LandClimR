#' Create theoretical landscapes to be used in LandClim
#'
#' @param overwrite defult is FALSE, indicating that if the xml files already exist they will not been overwritten. A warning appears as a function return.
#' @param config the path to the configuration file as a character or a data.frame where each of the variables/columns is a tag in the model configuration file in LandClim. If not provided the default values will be used.
#' @param barkbeetle the path to the barkbeetle file as a character or a data.frame where each of the variables/columns is a tag in the bark beetle configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath.
#' @param landtypeparameters the path to the landtypeparameters file as a character or a data.frame where each of the variables/columns is a tag in the land type parameters configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath.
#' @param plantingparameters the path to the plantingparametersfile as a character or a data.frame where each of the variables/columns is a tag in the planting parameters configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath.
#' @param randomstate the path to the randomstate file as a character or a data.frame where each of the variables/columns is a tag in the random state configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath.
#' @param species the path to the species file as a character or a data.frame where each of the variables/columns is a tag in the species parameters configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath.
#' @param workspacePath path where the the input xml files and output will be created. The name of the files will be the default names (config.xml, barkbeetle.xml, landtype.xml, planting.xml, randomstate.xml, species.xml) unless they have previously created.
#' @param consoleOutputPath path to the folder and name ( e.g. nameofthefile.txt) of the file where you want to storage the console outputs shown during the simulation. If not provided the console outcomes will be seen in the console and there will not be storaged.
#' @param binPath path to the folder where LandClim model binary files needed for your operative system are storaged.
#' @return Run LandClim with the desired input files and saved in the \code{workspacePath} provided.
#' @examples
#' runLandClim(overwrite = FALSE,
#'             config = NA,
#'             barkbeetle = NA,
#'             landtypeparameters = NA,
#'             plantingparameters = NA,
#'             randomstate =  NA,
#'             species = NA,
#'             workspacePath = "workspace",
#'             consoleOutputPath = "",
#'             binPath = "bin")

# Load default values

default_config <- read.csv("R/default-data/config.csv",
                           fileEncoding = "UTF-8-BOM", header = T)
default_barkbeetle <- read.csv("R/default-data/barkbeetle.csv",
                               fileEncoding = "UTF-8-BOM", header = T)
default_landtypeparameters <- read.csv("R/default-data/landtypeparameters.csv",
                                       fileEncoding = "UTF-8-BOM", header = T)
default_plantingparameters <-read.csv("R/default-data/plantingparameters.csv",
                                      fileEncoding = "UTF-8-BOM", header = T)
default_randomstate <- read.csv("R/default-data/randomstate.csv",
                                fileEncoding = "UTF-8-BOM", header = T)
default_species <- read.csv("R/default-data/species.csv",
                            fileEncoding = "UTF-8-BOM", header = T)



runLandClim <- function(overwrite = FALSE,
                        config = NA,
                        barkbeetle = NA,
                        landtypeparameters = NA,
                        plantingparameters = NA,
                        randomstate =  NA,
                        species = NA,
                        workspacePath = "workspace",
                        consoleOutputPath = "",
                        binPath = "bin") {

  # Check if the folder where you want to storage the parameter files and
  # simulation outputs exists if it does not exist the folder is created
  if (!dir.exists(workspacePath)){
    dir.create(workspacePath, recursive = TRUE)
  }



  # Save the console outputs from the simulation?
  if (!is.null(consoleOutputPath)) {
    if (consoleOutputPath == "") {
      tempStr <- strsplit(tempdir(), "\\\\")
      consoleOutputPath <- paste0(configXMLpath, "_consoleOutput_",
                                  tempStr[[1]][[length(tempStr[[1]])]], ".txt")
    }
  }



  # OPTION A- No parameters are provided, use the default values and create the xml file in workspacePath
  if (!(class(config) == "data.frame") && is.na(config)){config <- default_config}
  if (!(class(barkbeetle) == "data.frame") && is.na(barkbeetle)){barkbeetle <- default_barkbeetle}
  if (!(class(landtypeparameters) == "data.frame") && is.na(landtypeparameters)){landtypeparameters <- default_landtypeparameters}
  if (!(class(plantingparameters) == "data.frame") && is.na(plantingparameters)){plantingparameters <- default_plantingparameters}
  if (!(class(randomstate) == "data.frame") && is.na(randomstate)){randomstate <- default_randomstate}
  if (!(class(species) == "data.frame") && is.na(species)){species <- default_species}

  #OPTION B - provide your own parameter values, create the xml file in workspacePath
  # or has been assigned to the default values in the step above
  if (class(config) == "data.frame"){
    configXMLpath <- paste0(workspacePath, "config.xml")
    writeXML(data = config, type = "config", filePathXML = configXMLpath, overwrite)}
  if (class(barkbeetle) == "data.frame"){
    barkbeetleXMLpath <- paste0(workspacePath, "barkbeetle.xml")
    writeXML(barkbeetle, "barkbeetle", barkbeetleXMLpath, overwrite)}
  if (class(landtypeparameters) == "data.frame"){
    landtypeparametersXMLpath <- paste0(workspacePath, "landtype.xml")
    writeXML(landtypeparameters, "landtypeparameters", landtypeparametersXMLpath, overwrite)}
  if (class(plantingparameters) == "data.frame"){
    plantingparametersXMLpath <- paste0(workspacePath, "planting.xml")
    writeXML(plantingparameters,"plantingparameters",plantingparametersXMLpath, overwrite)}
  if (class(randomstate) == "data.frame"){
    randomstateXMLpath <- paste0(workspacePath, "randomstate.xml")
    writeXML(randomstate, "randomstate", randomstateXMLpath, overwrite)}
  if (class(species) == "data.frame"){
    speciesXMLpath <- paste0(workspacePath, "species.xml")
    writeXML(species, "species", speciesXMLpath, overwrite)}


  # OPTION C - provide a path where the parameter file is located
  if (class(config) == "character"){
    configXMLpath <- paste0(workspacePath, "config.xml")
  }
  #if (class(barkbeetle) == "character")
  #   barkbeetleXMLpath <- paste0(workspacePath, "barkbeetle.xml")
  #if (class(landtypeparameters) == "character")
  #  landtypeparametersXMLpath <- paste0(workspacePath, "landtype.xml")
  #if (class(plantingparameters) == "character")
  #  plantingparametersXMLpath <- paste0(workspacePath, "planting.xml")
  #if (class(randomstate) == "character")
  #  randomstateXMLpath <- paste0(workspacePath, "randomstate.xml")
  #if (class(species) == "character")
  #  speciesXMLpath <- paste0(workspacePath, "species.xml")

  # Run the model with the desired set-up
  if (tolower(.Platform$OS.type) == "unix"){
    system2(paste0( binPath, '/LandClim'),
            args = paste0(getwd(), "/", configXMLpath))#, stdout = consoleOutputPath)
  } else {
    system2(paste0(binPath, '/LandClim.exe'),
            args = paste0(getwd(), "/", configXMLpath), stdout=consoleOutputPath)}
}



