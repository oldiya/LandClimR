################################################################################
# Functions needed to run a LandClim simulation
# Author: Olalla Díaz Yáñez (olalladiaz.net)
# Aim: change the default table to configureate the XML parameters files
################################################################################


# This function takes the default csv files from the package LandClimR and
# modify the selected variables that the user wants to be different from the default

   # for example:
      #configTable <- LandClimR::default_config
      #tableType = "config"
      #listVarToChange <- rep(NA, length(names(LandClimR::default_config)))
      #names(listVarToChange) <- colnames (LandClimR::default_config)

      #VarToChange = data.frame (InputConfiguration_InputPath = "Data/theoretical/squared",
      ##                         RuntimeConfiguration_StartYear = 1,
      #                          RuntimeConfiguration_EndYear =  20,
      #                          InputConfiguration_TreeInitStateFile = "treeInitPabies.csv"
      #)

  modifyDefaultXML <- function (tableType = c("config", "barkbeetle", "landtypeparameters",
                                                "plantingparameters", "randomstate", "species"),
                                VarToChange){
    if (tableType == "config"){tableToChange <- LandClimR::default_config}
    if (tableType == "barkbeetle"){tableToChange <- LandClimR::default_barkbeetle}
    if (tableType == "landtypeparameters"){tableToChange <- LandClimR::default_landtypeparameters}
    if (tableType == "plantingparameters"){tableToChange <- LandClimR::default_plantingparameters}
    if (tableType == "randomstate"){tableToChange <- LandClimR::default_randomstate}
    if (tableType == "species"){tableToChange <- LandClimR::default_species}


    for (i in 1:length (VarToChange)){
      ONEVarToChange <- VarToChange[i]
      if (colnames(ONEVarToChange) %in% colnames(tableToChange)){
        tableToChange [, colnames(ONEVarToChange)] <- ONEVarToChange
      } else {
        tableToChange <- cbind (tableToChange, ONEVarToChange)
      }

    }

    return(tableToChange)
  }

