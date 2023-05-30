#' Create configuration files to be used in LandClim
#'
#' @param overwrite default is FALSE, indicating that if the xml files already exist they will not been overwritten. A warning appears as a function return.
#' @param model the path to the model configuration file as a character or a data.frame where each of the variables/columns is a tag in the model configuration file in LandClim. If not provided the default values will be used.
#' @param barkbeetle the path to the barkbeetle configuration file as a character or a data.frame where each of the variables/columns is a tag in the bark beetle configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath. If NULL is assigned the parameter file wont be created.
#' @param landtype  the path to the landtype configuration file as a character or a data.frame where each of the variables/columns is a tag in the land type parameters configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath.If NULL is assigned the parameter file wont be created.
#' @param planting  the path to the planting configuration file as a character or a data.frame where each of the variables/columns is a tag in the planting parameters configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath.If NULL is assigned the parameter file wont be created.
#' @param randomstate the path to the randomstate configuration file  as a character or a data.frame where each of the variables/columns is a tag in the random state configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath.If NULL is assigned the parameter file wont be created.
#' @param species the path to the species configuration file  as a character or a data.frame where each of the variables/columns is a tag in the species parameters configuration file in LandClim. If not provided the default values will be used. The created file will be saved in the indicated workspacepath.If NULL is assigned the parameter file wont be created.
#' @param workspacePath path where the the input xml configuration files  and output will be created. The name of the files will be the default names (config.xml, barkbeetle.xml, landtype.xml, planting.xml, randomstate.xml, species.xml) unless they have previously created.
#' @return The desired input parameter files and saved them in the \code{workspacePath} provided.
#' @examples
#' runLandClim(overwrite = FALSE,
#'             model = NA,
#'             barkbeetle = NA,
#'             landtype = NA,
#'             planting = NA,
#'             randomstate =  NA,
#'             species = NA,
#'             workspacePath = "workspace",
#'             consoleOutputPath = "")


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
  data("default_model", envir = environment())
  data("default_barkbeetle", envir = environment())
  data("default_landtype", envir = environment())
  data("default_planting", envir = environment())
  data("default_randomstate", envir = environment())
  data("default_species", envir = environment())


  # Check if the folder where you want to storage the parameter files and
  # simulation outputs exists if it does not exist the folder is created
  if (!dir.exists(workspacePath)) {
    dir.create(workspacePath, recursive = TRUE)
  }

  #OPTION
  #A - user parameter file is not created--> NULL
  #B - provide your own parameter values, create the xml file in workspacePath
  #C -  been assigned to the default values if NA was assigned
  if (is.null(model)) {print("Model user parameter file is not created")
    } else if (TRUE %in% (class(model) %in% "data.frame")){
     modelXMLpath <- paste0(workspacePath, "model.xml")
     writeConfigFile(data = model, type = "model", filePath = modelXMLpath,
                     overwrite = overwrite, decadal = decadal)
    }else if (is.na(model)) {
       model <- default_model
       modelXMLpath <- paste0(workspacePath, "model.xml")
       writeConfigFile(data = model, type = "model", filePath = modelXMLpath,
                       overwrite = overwrite, decadal = decadal)}


  if (is.null(barkbeetle)) {print("Barkbeetle user parameter file is not created")
    } else if (TRUE %in% (class(barkbeetle) %in% "data.frame")){
     barkbeetleXMLpath <- paste0(workspacePath, "barkbeetle.xml")
     writeConfigFile(data = barkbeetle, type = "barkbeetle",
                     filePath = barkbeetleXMLpath,
                     overwrite = overwrite, decadal = decadal)
    } else  if (is.na(barkbeetle)) {
       barkbeetle <- default_barkbeetle
       barkbeetleXMLpath <- paste0(workspacePath, "barkbeetle.xml")
       writeConfigFile(data = barkbeetle, type = "barkbeetle",
                       filePath = barkbeetleXMLpath,
                       overwrite = overwrite, decadal = decadal)}


  if (is.null(landtype)) {print("Landtype user parameter file is not created")
    } else if (TRUE %in% (class(landtype) %in% "data.frame")){
     landtypeXMLpath <- paste0(workspacePath, "landtype.xml")
     writeConfigFile(data = landtype, type = "landtype",
                     filePath = landtypeXMLpath,
                     overwrite = overwrite, decadal = decadal)
    } else if (is.na(landtype)) {
     landtype <- default_landtype
     landtypeXMLpath <- paste0(workspacePath, "landtype.xml")
     writeConfigFile(data = landtype, type = "landtype",
                     filePath = landtypeXMLpath,
                     overwrite = overwrite, decadal = decadal)}

  if (is.null(planting)) {print("Planting user parameter file is not created")
    } else if (TRUE %in% (class(planting) %in% "data.frame")){
    plantingXMLpath <- paste0(workspacePath, "planting.xml")
     writeConfigFile(data = planting ,type = "planting",
                     filePath = plantingXMLpath,
                     overwrite = overwrite, decadal = decadal)
    } else  if (is.na(planting)) {
    planting <- default_planting
    plantingXMLpath <- paste0(workspacePath, "planting.xml")
    writeConfigFile(data = planting ,type = "planting",
                    filePath = plantingXMLpath,
                    overwrite = overwrite, decadal = decadal)}

  if (is.null(randomstate)) {print("Randomstate user parameter file is not created")
    } else if (TRUE %in% (class(randomstate)  %in% "data.frame")){
    randomstateXMLpath <- paste0(workspacePath, "randomstate.xml")
     writeConfigFile(data = randomstate, type = "randomstate",
                     filePath = randomstateXMLpath,
                     overwrite = overwrite, decadal = decadal)
    } else  if (is.na(randomstate)) {
    randomstate <- default_randomstate
    randomstateXMLpath <- paste0(workspacePath, "randomstate.xml")
    writeConfigFile(data = randomstate, type = "randomstate",
                    filePath = randomstateXMLpath,
                    overwrite = overwrite, decadal = decadal)}

  if (is.null(species)) {print("Species user parameter file is not created")
    } else if (TRUE %in% (class(species)  %in% "data.frame")){
    speciesXMLpath <- paste0(workspacePath, "species.xml")
     writeConfigFile(data = species, type = "species",
                     filePath = speciesXMLpath,
                     overwrite = overwrite, decadal = decadal)
    } else  if (is.na(species)) {
       species <- default_species
       speciesXMLpath <- paste0(workspacePath, "species.xml")
       writeConfigFile(data = species, type = "species",
                       filePath = speciesXMLpath,
                       overwrite = overwrite, decadal = decadal)}

  # OPTION C - provide a path where the model configuration file is located
  # this file contain the path to the other parameter and configuration files
  if (class(model) == "character") {
    modelXMLpath <- paste0(workspacePath, "model.xml")
  }

  # OR where the relative path to the parameter files


}

