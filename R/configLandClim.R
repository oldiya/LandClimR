#' Create configuration files to be used in LandClim
#'
#' @param overwrite default is FALSE, indicating that if the xml files already exist they will not been overwritten. A warning appears as a function return.
#' @param model the path to the model configuration file as a character or a data.frame where each of the variables/columns is a tag in the model configuration file in LandClim. If not provided the default values will be used.
#' @param barkbeetle the path to the barkbeetle configuration file as a character or a data.frame where each of the variables/columns is a tag in the bark beetle configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath.
#' @param landtype  the path to the landtype configuration file as a character or a data.frame where each of the variables/columns is a tag in the land type parameters configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath.
#' @param planting  the path to the planting configuration file as a character or a data.frame where each of the variables/columns is a tag in the planting parameters configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath.
#' @param randomstate the path to the randomstate configuration file  as a character or a data.frame where each of the variables/columns is a tag in the random state configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath.
#' @param species the path to the species configuration file  as a character or a data.frame where each of the variables/columns is a tag in the species parameters configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath.
#' @param workspacePath path where the the input xml configuration files  and output will be created. The name of the files will be the default names (config.xml, barkbeetle.xml, landtype.xml, planting.xml, randomstate.xml, species.xml) unless they have previously created.
#' @param binPath path to the folder where LandClim model binary files needed for your operative system are storage.
#' @return Run LandClim with the desired input files and saved in the \code{workspacePath} provided.
#' @examples
#' runLandClim(overwrite = FALSE,
#'             model = NA,
#'             barkbeetle = NA,
#'             landtype = NA,
#'             planting = NA,
#'             randomstate =  NA,
#'             species = NA,
#'             workspacePath = "workspace",
#'             consoleOutputPath = "",
#'             binPath = "bin")


configLandClim <- function(overwrite = FALSE,
                           decadal,
                           model = NA,
                           barkbeetle = NA,
                           landtype = NA,
                           planting = NA,
                           randomstate =  NA,
                           species = NA,
                           workspacePath = "workspace/") {

  # Load default values
  load("data/default_model.RData")
  load("data/default_barkbeetle.RData")
  load("data/default_landtype.RData")
  load("data/default_planting.RData")
  load("data/default_randomstate.RData")
  load("data/default_species.RData")

  # Check if the folder where you want to storage the parameter files and
  # simulation outputs exists if it does not exist the folder is created
  if (!dir.exists(workspacePath)) {
    dir.create(workspacePath, recursive = TRUE)
  }


  # OPTION A- No parameters are provided, use the default values and create the xml file in workspacePath
  if (!(class(model) == "data.frame") && is.na(model)) {model <- default_model}
  if (!(class(barkbeetle) == "data.frame") && is.na(barkbeetle)) {barkbeetle <- default_barkbeetle}
  if (!(class(landtype) == "data.frame") && is.na(landtype )) {landtype <- default_landtype}
  if (!(class(planting) == "data.frame") && is.na(planting )) {planting <- default_planting}
  if (!(class(randomstate) == "data.frame") && is.na(randomstate)) {randomstate <- default_randomstate}
  if (!(class(species) == "data.frame") && is.na(species)) {species <- default_species}

  #OPTION B - provide your own parameter values, create the xml file in workspacePath
  # or has been assigned to the default values in the step above
  if (class(model) == "data.frame"){
    modelXMLpath <- paste0(workspacePath, "model.xml")
     writeConfigFile(data = model, type = "model", filePathXML = modelXMLpath,
                     overwrite, decadal)}
  if (class(barkbeetle) == "data.frame"){
    barkbeetleXMLpath <- paste0(workspacePath, "barkbeetle.xml")
     writeConfigFile(barkbeetle, "barkbeetle", barkbeetleXMLpath, overwrite, decadal)}
  if (class(landtype ) == "data.frame"){
    landtypeXMLpath <- paste0(workspacePath, "landtype.xml")
     writeConfigFile(landtype , "landtype", landtypeXMLpath, overwrite, decadal)}
  if (class(planting ) == "data.frame"){
    plantingXMLpath <- paste0(workspacePath, "planting.xml")
     writeConfigFile(planting ,"planting",plantingXMLpath, overwrite, decadal)}
  if (class(randomstate) == "data.frame"){
    randomstateXMLpath <- paste0(workspacePath, "randomstate.xml")
     writeConfigFile(randomstate, "randomstate", randomstateXMLpath, overwrite, decadal)}
  if (class(species) == "data.frame"){
    speciesXMLpath <- paste0(workspacePath, "species.xml")
     writeConfigFile(species, "species", speciesXMLpath, overwrite, decadal)}


  # OPTION C - provide a path where the parameter file is located
  if (class(model) == "character"){
    modelXMLpath <- paste0(workspacePath, "model.xml")
  }



}



