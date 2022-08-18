#' Modify default values and structure of the configuration files via data.frame
#'
#' @param type  this loads the default table c("model","barkbeetle","landtype","planting","randomstate","species"),
#' @param VarToChange change a selection of variables from their default value
#' @return a new table to be used to create the configuration file with the function writeConfigFile
#' @examples
#'modifyDefaultXML(type= "model",
#' VarToChange = data.frame (DispersalConfiguration_KernelProbabilityThreshold = 0.001))

  modifyDefaultXML <- function(type = c("model","barkbeetle","landtype",
                                        "planting","randomstate", "species"),
                               VarToChange){

    if (type == "model") {
      data("default_model", envir = environment())
      tableToChange <- default_model}

    if (type == "barkbeetle") {
      data("default_barkbeetle", envir = environment())
      tableToChange <- default_barkbeetle}

    if (type == "landtype") {
      data("default_landtype", envir = environment())
      tableToChange <- default_landtype
      }

    if (type == "planting") {
      data("default_planting", envir = environment())
      tableToChange <- default_planting
      }
    if (type == "randomstate") {
      data("default_randomstate", envir = environment())
      tableToChange <- default_randomstate
      }
    if (type == "species") {
     data("default_species", envir = environment())
      tableToChange <- default_species
      }


    for (i in 1:length(VarToChange)) {
      ONEVarToChange <- VarToChange[i]

      # check if that new tag is part of the default structure
      if (colnames(ONEVarToChange) %in% colnames(tableToChange)) {
        tableToChange[, colnames(ONEVarToChange)] <- ONEVarToChange
      } else { # if it is not add it as a new column
        tableToChange <- cbind(tableToChange, ONEVarToChange)
      }

    }

    return(tableToChange)
  }

