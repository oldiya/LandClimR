################################################################################
# Functions needed to run a LandClim simulation
# Author: Olalla Díaz Yáñez (olalladiaz.net)
# Aim: Set of functions to run a LandClim simulation
################################################################################


# This function updates the paths in the control file 

  cfPaths <- function (controlfilename, controlfilepath, 
                                 inputpath, outputpath){
    
    #load the xml file where you want to make changes
    controlxmlfile <- xml2::read_xml (paste0 (controlfilepath, controlfilename))
    
    # Modified the control file to set the path to the input folder in the node 'InputPath'
     # Select the node where you want the text changed
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//InputPath/text()")

    # Change the node text 
    xml2::xml_text (nodetochange) <- c(paste0 (getwd (), "/", inputpath))

    # Modified the control file to set the path to the output folder in the node 'OutputDirectory' 
    # Select the node where you want the text changed
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//OutputDirectory/text()")
    # Change the node text 
    xml2::xml_text (nodetochange) <- c(paste0 (getwd (), "/", outputpath))
    
    xml2::write_xml (controlxmlfile, paste0 (controlfilepath, controlfilename)) 
  }

 # Function that modifies the control file parameter by
  # activating the modules of interest and setting its parameter values  
  # direct to the correct input files
  # set the simulation time

  
  LCconfiguration <- function (controlfilename, controlfilepath, 
                               
                               fulloutputyears,
                               aggregateoutput1,
                               aggregateoutput2,
                        
                         
                               climateFile,
                               climateFormat = c("Yearly"),
                               climateRandomization = c("Restart", "Sequence", "Random"),
                               climateSequence =  NULL,
                               waterBalance = "ORIGINAL",
                               
                               plantingEnable = c("false", "true"),
                               
                               fireEnable = c("false", "true"),
                               fireProbExp = 2.5,
                               fireProbExpBeta = 1,
                               fireProbPMax = 1.0,
                               fireProbPMin = 0,
                               fireSizeMax = 20, 
                               fireSpreadFunction = "IncompleteBetaDistribution",
                               fireDroughtIndex = "Default",
                               
                               windEnable = c("false", "true"),
                               windModel = "DEFAULT",
                               windBreakoutCoef = 0.002,
                               windMeanWindthrowSize = 1250, 
                               windProbabilityCoef = 200, 
                               windSizeCoef = 0.2,
                               windMeanReturnInterval = 800,
                               windMinWindthrowSize = 620,
                               windMaxWindthrowSize = 2500,
                               
                               harvestEnable = c("false", "true"),
                               
                               beetleEnable = c("false", "true"),
                               beetleHostName = "piceabie",
                               beetleBetaScale = 10000,
                               beetleIniBdi = 0.01,
                               beetleBgBpi = 0.01,
                               beetleSiWMaxCoeff = 1,
                               beetleBdiMaxCoeff = 1,
                               beetleCellCoI = 1,
                               beetleSiCellNi = 0,
                               beetleSiWni = 1,
                               beetleWISiCell = 0.5, 
                               beetleBpCoeff = 14,
                               beetleMaxInfBm = 150,
                               beetleWIDist = 500,
                               beetleDTBark = 2,
                               beetleLowerAsymDrS = 0,
                               beetleParamCDrS = 0, 
                               beetleDrSFunction = "default",
                               
                               runtimeStartYear = 1,
                               runtimeEndYear = 110,
                               runtimeRandomInitX = 1,
                               runtimeRandomInitY = 10000,
                               runtimeRandomInitZ = 3000,
                               
                               dispersalMode = "UNIFORM",
                               dispersalBaseSeedProb = 0.001,
                               dispersalEffectiveDm1Prob = 0.1,
                               dispersalMaxDm1Prob = 0.01,
                               
                               ElevationFile = "Dem.txt",
                               SlopeFile = "Slope.txt",
                               LandMaskFile = "Mask.txt",
                               LandTypeFile = "LandType.txt",
                               AspectFile = "Aspect.txt",
                               SoilDepthFile = "Soil.txt",
                               treeInitStateFile,
                               HarvestStandsFile = "HarvestStands.txt",
                               HarvestManagementAreasFile = "HarvestManagementAreas.txt",
                               
                               BeetleParameterFile = "BarkbeetleParameters.xml",
                               LandTypeParameterFile = "LandTypeParameters.xml",
                               SpeciesConfigurationFile = "SpeciesConfiguration.xml",
                               RandomStateFile = "RandomState.xml",
                               harvestFile = "HarvestParameters.xml",
                               plantingFile = "PlantingParameters.xml") {

    #load the xml file where you want to make changes
    controlxmlfile <- xml2::read_xml (paste0 (controlfilepath, controlfilename))
    
    
    # OUTPUT CONFIGURATION
    
    # Modified the control file to set the number of years when you want the full out file 
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//OutputConfiguration/Cohort/Full/Years/text()")
    xml2::xml_text (nodetochange) <- paste(sub (',', '', fulloutputyears ), 
                                           sep = " ", collapse = " ") 
    
    # Modified control file to select the aggregation mode in the output files
    nodetochange <- xml2:: xml_find_all (controlxmlfile, "//OutputConfiguration/Aggregations/OutputAggregation/Name/text()")[1]
    xml2::xml_text (nodetochange) <- aggregateoutput1[1]
    nodetochange <- xml2:: xml_find_all (controlxmlfile, "//OutputConfiguration/Aggregations/OutputAggregation/Map/text()")[1]
    xml2::xml_text (nodetochange) <- aggregateoutput1[2]
    
    nodetochange <- xml2:: xml_find_all (controlxmlfile, "//OutputConfiguration/Aggregations/OutputAggregation/Name/text()")[2]
    xml2::xml_text (nodetochange) <- aggregateoutput2[1]
    nodetochange <- xml2:: xml_find_all (controlxmlfile, "//OutputConfiguration/Aggregations/OutputAggregation/Map/text()")[2]
    xml2::xml_text (nodetochange) <- aggregateoutput2[2]
    
    
    # Input files names:
    
    # Modified the control file to set the path to the input treeinit file 
    # this files contains information on the forest status at the beginning of the simulation
    # if it is not provided the simulation starts on bare ground
    # If the tag is not defined in the XML it is created:
    
    if(is_empty (xml2::xml_find_all (controlxmlfile, "//InputConfiguration/TreeInitStateFile/text()"))){
      
      xml2::xml_add_child (xml2::xml_children(controlxmlfile)[[1]],
                           xml2::read_xml(paste0 ("<TreeInitStateFile>", 
                                                  treeInitStateFile,
                                                  "</TreeInitStateFile>")))
      
    } else {
      nodetochange <- xml2::xml_find_all (controlxmlfile, "//InputConfiguration/TreeInitStateFile/text()")
      xml2::xml_text (nodetochange) <- treeInitStateFile
    }
    
    # Modified the control file to set the path to the input file with the climate data 
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//ClimateConfiguration/ClimateFile/text()")
    xml2::xml_text (nodetochange) <- climateFile
    
    # Modified the control file to set the path to the input file with the planting information
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//PlantingDisturbanceConfiguration/ParameterFile/text()")
    xml2::xml_text (nodetochange) <- plantingFile
    
    # harvest ParameterFile
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//HarvestDisturbanceConfiguration/ParameterFile/text()")
    xml2::xml_text (nodetochange) <- harvestFile
    
    # Modified the control file to set the path to the input file with Elevation information
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//InputConfiguration/ElevationFile/text()")
    xml2::xml_text (nodetochange) <- ElevationFile
    
    
    # Modified the control file to set the path to the input file with Slope information
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//InputConfiguration/SlopeFile/text()")
    xml2::xml_text (nodetochange) <- SlopeFile
    
    # Modified the control file to set the path to the input file with Mask information
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//InputConfiguration/LandMaskFile/text()")
    xml2::xml_text (nodetochange) <- LandMaskFile
    
    # Modified the control file to set the path to the input file with Land type information
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//InputConfiguration/LandTypeFile/text()")
    xml2::xml_text (nodetochange) <- LandTypeFile
    
    # Modified the control file to set the path to the input file with land type parameters information
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//InputConfiguration/LandTypeParameterFile/text()")
    xml2::xml_text (nodetochange) <- LandTypeParameterFile
    
    # Modified the control file to set the path to the input file with aspect information
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//InputConfiguration/AspectFile/text()")
    xml2::xml_text (nodetochange) <- AspectFile
    
    # Modified the control file to set the path to the input file with soil information
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//InputConfiguration/SoilDepthFile/text()")
    xml2::xml_text (nodetochange) <- SoilDepthFile
 
    
    # Modified the control file to set the path to the input file with harvest stands information
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//InputConfiguration/HarvestStandsFile/text()")
    xml2::xml_text (nodetochange) <- HarvestStandsFile
    
    # Modified the control file to set the path to the input file with harvest management information
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//InputConfiguration/HarvestManagementAreasFile/text()")
    xml2::xml_text (nodetochange) <- HarvestManagementAreasFile
    
    # Modified the control file to set the path to the input file with species configuration information
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//InputConfiguration/SpeciesConfigurationFile/text()")
    xml2::xml_text (nodetochange) <- SpeciesConfigurationFile

    # Modified the control file to set the path to the input file with random initiation information
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//InputConfiguration/RandomStateFile/text()")
    xml2::xml_text (nodetochange) <- RandomStateFile
    
    

    
    # CLIMATE CONFIGURATION ----
    # Modified the control file to set the path to the input file with the climate data 
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//ClimateConfiguration/ClimateFile/text()")
    xml2::xml_text (nodetochange) <- climateFile
   
    # Set the climate configuration to Yearly
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//ClimateConfiguration/Format/text()")
    xml2::xml_text (nodetochange) <- climateFormat

    
    # RANDOMIZATION ---- #ToDo write correctly the alternative options!!
       # <!-- 
       #   <Randomization>Sequence</Randomization>
       #   <Sequence>1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41</Sequence>
       ##   --> 
       #   <!-- <Randomization>Random</Randomization> -->	
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//ClimateConfiguration/Randomization/text()")
    xml2::xml_text (nodetochange) <- climateRandomization
    
    if (climateRandomization == "Sequence"){
      if (is.null (climateSequence)) {
        "ERROR: Please provide the climate randomization sequence"
      } else {
        nodetochange <- xml2::xml_find_all (controlxmlfile, "//ClimateConfiguration/Randomization/text()")
        xml2::xml_text (nodetochange) <- climateSequence}}

    # Set the waterbalance model 
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//ClimateConfiguration/WaterBalanceModel/text()")
    xml2::xml_text (nodetochange) <- waterBalance
    
   
    # PLANTING DISTURBANCE CONFIGURATION ----
    # Enable 
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//PlantingDisturbanceConfiguration/Enabled/text()")
    xml2::xml_text (nodetochange) <- plantingEnable
    
    # Modified the control file to set the path to the input file with the climate data 
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//PlantingDisturbanceConfiguration/ParameterFile/text()")
    xml2::xml_text (nodetochange) <- plantingFile
    
    # FIRE DISTURBANCE CONFIGURATION----
    # Enable
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//FireDisturbanceConfiguration/Enabled/text()")
    xml2::xml_text (nodetochange) <- fireEnable

    # Set probability of extension # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//FireDisturbanceConfiguration/ProbExp/text()")
    xml2::xml_text (nodetochange) <- as.character (fireProbExp)
    
    # Set ProbExpBeta # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//FireDisturbanceConfiguration/ProbExpBeta/text()")
    xml2::xml_text (nodetochange) <- as.character (fireProbExpBeta)
    
    # Set ProbPMax # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//FireDisturbanceConfiguration/ProbPMax/text()")
    xml2::xml_text (nodetochange) <- as.character (fireProbPMax)
    
    # Set ProbPMin # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//FireDisturbanceConfiguration/ProbPMin/text()")
    xml2::xml_text (nodetochange) <- as.character (fireProbPMin)
    
    # Set SizeMax # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//FireDisturbanceConfiguration/SizeMax/text()")
    xml2::xml_text (nodetochange) <- as.character (fireSizeMax)
    
    # Set SpreadFunction # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//FireDisturbanceConfiguration/SpreadFunction/text()")
    xml2::xml_text (nodetochange) <- fireSpreadFunction
    
    # Set SpreadFunction # ToDo define what is this # ToDo check that this parameter is not define elsewhere!!!!
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//FireDisturbanceConfiguration/DroughtIndex/text()")
    xml2::xml_text (nodetochange) <- fireDroughtIndex
    

    # WIND DISTURBANCE CONFIGURATION----
    # Enable
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//WindDisturbanceConfiguration/Enabled/text()")
    xml2::xml_text (nodetochange) <- windEnable
    
    # Set the Wind model # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//WindDisturbanceConfiguration/WindthrowModel/text()")
    xml2::xml_text (nodetochange) <- windModel
    
    # Set the BreakoutCoefficient # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//WindDisturbanceConfiguration/BreakoutCoefficient/text()")
    xml2::xml_text (nodetochange) <- as.character (windBreakoutCoef)
    
    # Set the MeanWindthrowSize # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//WindDisturbanceConfiguration/MeanWindthrowSize/text()")
    xml2::xml_text (nodetochange) <- as.character (windMeanWindthrowSize)
    
    # Set the ProbabilityCoefficient # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//WindDisturbanceConfiguration/ProbabilityCoefficient/text()")
    xml2::xml_text (nodetochange) <- as.character (windProbabilityCoef)
    
    
    # Set the ProbabilityCoefficient # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//WindDisturbanceConfiguration/SizeCoefficient/text()")
    xml2::xml_text (nodetochange) <- as.character (windSizeCoef)
    
    # Set the MeanReturnInterval # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//WindDisturbanceConfiguration/MeanReturnInterval/text()")
    xml2::xml_text (nodetochange) <- as.character (windMeanReturnInterval)

    # Set the MinWindthrowSize # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//WindDisturbanceConfiguration/MinWindthrowSize/text()")
    xml2::xml_text (nodetochange) <- as.character (windMinWindthrowSize)
    
    # Set the MinWindthrowSize # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//WindDisturbanceConfiguration/MaxWindthrowSize/text()")
    xml2::xml_text (nodetochange) <- as.character (windMaxWindthrowSize)
    
    #HARVEST DISTURBANCE CONFIGURATION----
    # Enable
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//HarvestDisturbanceConfiguration/Enabled/text()")
    xml2::xml_text (nodetochange) <- harvestEnable
    
    # harvest ParameterFile
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//HarvestDisturbanceConfiguration/ParameterFile/text()")
    xml2::xml_text (nodetochange) <- harvestFile

   
    # BEETLE  DISTURBANCE CONFIGURATION ----
    # Enable
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//BeetleDisturbanceConfiguration/Enabled/text()")
    xml2::xml_text (nodetochange) <- beetleEnable
    
    # Modified the control file to set the path to the input file with Beetle information
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//BeetleDisturbanceConfiguration/ParameterFile/text()")
    xml2::xml_text (nodetochange) <- BeetleParameterFile
    

    
    # RUN TIME CONFIGURATION----
    
    # Start year 
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//RuntimeConfiguration/StartYear/text()")
    xml2::xml_text (nodetochange) <- as.character (runtimeStartYear)
    
    # EndYear
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//RuntimeConfiguration/EndYear/text()")
    xml2::xml_text (nodetochange) <- as.character (runtimeEndYear)
    
    # RandomInit   # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//RuntimeConfiguration/RandomInit/X/text()")
    xml2::xml_text (nodetochange) <- as.character (runtimeRandomInitX)
    
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//RuntimeConfiguration/RandomInit/Y/text()")
    xml2::xml_text (nodetochange) <- as.character (runtimeRandomInitY)
    
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//RuntimeConfiguration/RandomInit/Z/text()")
    xml2::xml_text (nodetochange) <- as.character (runtimeRandomInitZ)
    
  
    # DISPERSAL CONFIGURATION----
    
    # Dispersal Mode  # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//DispersalConfiguration/DispersalMode/text()")
    xml2::xml_text (nodetochange) <- dispersalMode
    
    # Base Seed Probability  # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//DispersalConfiguration/BaseSeedProbability/text()")
    xml2::xml_text (nodetochange) <- as.character (dispersalBaseSeedProb)
    
    # Effective Dm1 Probability  # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//DispersalConfiguration/EffectiveDm1Probability/text()")
    xml2::xml_text (nodetochange) <- as.character (dispersalEffectiveDm1Prob)
    
    # Effective Dm1 Probability  # ToDo define what is this
    nodetochange <- xml2::xml_find_all (controlxmlfile, "//DispersalConfiguration/MaxDm1Probability/text()")
    xml2::xml_text (nodetochange) <- as.character (dispersalMaxDm1Prob)
    
    xml2::write_xml (controlxmlfile, paste0 (controlfilepath, controlfilename))}
    
  

    
  

# Find the system you are working in
  
  get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }
  
  
  is_empty <- function(x) if(length(x) == 0) TRUE else FALSE
  
  
  
  # Function to add or remove nodes in the xml files 
  #xmlfileBB <- xml2::read_xml (paste0 (path, filename))
  #variableName = NA
  #nodeName = "DRSiCell"
 # nodePath = "//BeetleConfiguration/BeetleParameters/"
 # underNode =NULL
 # xmlfile = xmlfileBB
  
  #xml2::write_xml (xmlfileBB, paste0 (path, filename))
  
 
  
  addRemoveNode <- function (variableName, nodeName, nodePath, underNode = NULL, 
                             xmlfile) {
    # If the parameter  has value NULL or NA and the parameter is in the XML file, removed the node  parameter from the XML file
    if (is.null (variableName)){
      if (!is_empty (xml2::xml_find_all(xmlfile, paste0 (".//*[name()='", nodeName, "']")))){
        xml2:: xml_remove (xml2::xml_find_all (xmlfile, paste0 (nodePath, nodeName, "/text()")))
        
      }} else if (is.na (variableName)){
        
      if (!is_empty (xml2::xml_find_all(xmlfile, paste0 (".//*[name()='", nodeName, "']")))){
        xml2:: xml_remove (xml2::xml_find_all (xmlfile, paste0 (nodePath, nodeName, "/text()")))
        
      }} else {
        
        # If the parameter has a value and the parameter is not in the XML file, add the parameter from the XML file
        # if the Node parameter is already in the XML file, change its value
        if (is_empty (xml2::xml_find_all(xmlfile, paste0 (".//*[name()='", nodeName, "']")))){
          if (!is.null (underNode)){ # if the node has to be under another parameter node, add thsat node first
            if (is_empty (xml2::xml_find_all(xmlfile, paste0 (".//*[name()='", underNode, "']")))){
              xml2::xml_add_child (xml2::xml_children(xmlfile)[[1]],
                                   xml2::read_xml (paste0 ("<", underNode, ">", "</", underNode, ">")))
              xml2::xml_add_child (xml2::xml_child (xml2::xml_children (xmlfile)[[1]], underNode),
                                   xml2::read_xml (paste0 ("<", nodeName, ">", variableName, "</", nodeName, ">")))
            } else {
              xml2::xml_add_child (xml2::xml_child (xml2::xml_children (xmlfile)[[1]], underNode),
                                   xml2::read_xml (paste0 ("<", nodeName, ">", variableName, "</", nodeName, ">")))
            }
            
          } else {
            xml2::xml_add_child (xml2::xml_children(xmlfile)[[1]],
                                 xml2::read_xml(paste0 ("<", nodeName, ">", variableName, "</", nodeName, ">")))
          }
          
        } else {
          nodetochange <- xml2::xml_find_all(xmlfile, paste0 (".//*[name()='", nodeName, "']"))
          xml2::xml_text (nodetochange) <- paste (sub (',', '', variableName), 
                                                  sep = " ", collapse = " ") 
        }}
    
    
    return (xmlfile)
  }
  
  
  
# Configure the bark beetle module

  configurationBB <- function (filename, path,
                               beetleHostName = "piceabie",
                               
                               # Susceptibility weights 
                               wWind = 0.6,
                               wDrought =  NULL,
                               wBeetleHostShare =  NULL,
                               wAge =  NULL,
                               
                               # Drought susceptibility parameters 
                               beetleLowerAsymDrS = 0, # <DrSFunction>default</DrSFunction> lower Asymptote of logistic function that relates droughtIndex to drought-induced beetle susceptibility (S_d; Eq. A1a-c in Appendix A)
                               beetleParamCDrS = 0, #  <ParamCDrS>0</ParamCDrS> <LowerAsymDrS>0</LowerAsymDrS> parameter used in polynomial drought-susceptibility function to specify minimum susceptibility
                               beetleDrSFunction = "default", #String that defines shape of relationship between droughtIndex and susceptibilitIndexDrought: Can be "default","negLAsym" or "polyn"
         
                        
                               beetleBetaScale = 10000, #<BetaScale>10000</BetaScale>
                               beetleIniBdi = 0.01,  #  <IniBdi>0.01</IniBdi>
                               beetleBgBpi = 0.01, # #  <BgBpi>0.01</BgBpi> background beetle pressure if beetle pressure is lower than background beetle pressure
                               beetleSiWMaxCoeff = 1, # <SiWMaxCoeff>1</SiWMaxCoeff> Coefficient that controls at which fraction of maximal biomass density (default 300 t/ha) wind susceptibility, a result of the windthrown, biomass is maximal (default: 1)
                               beetleBdiMaxCoeff = 1, # <SiWMaxCoeff>1</SiWMaxCoeff> Coefficient that controls at which fraction of maximal biomass density (default 300 t/ha) BDI is maximal (default: 1)
                               beetleCellCoI = 1, #  <CellCoI>1</CellCoI>  Importance of cell level beetle mortality risk components on cohort mortality risk (default: 1)
                               beetleSiCellNi = 0, #Importance of cell susceptibility on susceptibility of neighboring cell (default: 0)
                               beetleSiWni = 1, # <SiWni>1</SiWni> Importance of neighborhood on wind susceptibility (Eq. 1; default: 1)
                               beetleWISiCell = 0.5,# <SiCellNi>0</SiCellNi> Importance of cell susceptibility on susceptibility of neighboring cell (default: 0)
                               
                               beetleBpCoeff = 14, # <BpCoeff>14</BpCoeff> Coefficient to control beetle pressure (C_bp, Eq. 3; default: 14)
                               beetleMaxInfBm = 150, #Maximum Beetle infested beetle host (e.g. spruce) biomass (max B_inf, Eq. 5). This is the beetle host biomass that would be infested if susceptibility and beetle pressure is maximal, i.e 1. Default: 150 t/ha
                               beetleWIDist = 500, #  <WIDist>500</WIDist> Distance to which windthrown beetle host biomass influences wind-induced bark beetle susceptibility (D_w, Eq. 1; default: 500 m).
                               beetleDTBark = 2, #  <DTBark>2</DTBark> Difference in monthly mean Temp. between air and sub-bark (phloem) (Appendinx A; default: 2°)
            
                             # Spruce share parameters
                              beetleSsA = 0.08,
                              beetleSsK = 1,
                              beetleSsQ = 0.2674846,
                              beetleSsr = 6.8455438,
                              beetleSsm = 0.375,
                              beetleSsv = 0.3002734,

                              # DBH / Age susceptibility parameters
                                 
                               beetleSdbhAgetype = "AGE", #"DBH"
                               beetleSdbhAgeA = 0.2,
                               beetleSdbhAger = 0.1094542,
                               beetleSdbhAgem = 70,
                               beetleSdbhmeanDBHfunction = NULL # Only used if  beetleSdbhAgetype = "DBH", e,g.: "max",
                               
                              
                                          
  ) {
    #load the xml parameter file where you want to make changes
    xmlfileBB <- xml2::read_xml (paste0 (path, filename))
    
    # Modified the parameter file to set the beetle species host 
    addRemoveNode (variableName = beetleHostName, nodeName = "BeetleHostName", 
                   nodePath = "//BeetleConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB) 
    
    # Beetle BetaScale  # ToDo define what is this
    addRemoveNode (variableName = as.character (beetleBetaScale), nodeName = "BetaScale", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB) 
    
    # Beetle IniBdi  # ToDo define what is this
    addRemoveNode (variableName = as.character (beetleIniBdi), nodeName = "IniBdi", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB) 
    
    # Beetle BgBpi  # ToDo define what is this
    addRemoveNode (variableName = as.character (beetleBgBpi), nodeName = "BgBpi", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB) 
    
    # Beetle SiWMaxCoeff  # ToDo define what is this
    addRemoveNode (variableName = as.character (beetleSiWMaxCoeff), nodeName = "SiWMaxCoeff", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB) 
    
    # Beetle BdiMaxCoeff  # ToDo define what is this
    addRemoveNode (variableName = as.character (beetleBdiMaxCoeff), nodeName = "BdiMaxCoeff", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB)
    
    # Beetle CellCoI  # ToDo define what is this
    addRemoveNode (variableName = as.character (beetleCellCoI), nodeName = "CellCoI", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB)
    
    # Beetle SiCellNi  # ToDo define what is this
    addRemoveNode (variableName = as.character (beetleSiCellNi), nodeName = "SiCellNi", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB)
  
    # Beetle SiWni  # ToDo define what is this
    addRemoveNode (variableName = as.character (beetleSiWni), nodeName = "SiWni", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB)
    
    # Beetle WISiCell  # ToDo define what is this
    addRemoveNode (variableName = as.character (beetleWISiCell), nodeName = "WISiCell", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB)
    
    # Beetle BpCoeff  # ToDo define what is this
    addRemoveNode (variableName = as.character (beetleBpCoeff), nodeName = "BpCoeff", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB)
    
    # Beetle MaxInfBm  # ToDo define what is this
    addRemoveNode (variableName = as.character (beetleMaxInfBm), nodeName = "MaxInfBm", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB)
    
    # Beetle WIDist # ToDo define what is this
    addRemoveNode (variableName = as.character (beetleWIDist), nodeName = "WIDist", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB)
    
    # Beetle DTBark # ToDo define what is this
    addRemoveNode (variableName = as.character (beetleDTBark), nodeName = "DTBark", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB)
    
    # Drought----
    
    # Beetle LowerAsymDrS 
    #Set the lower Asymptote of logistic function that relates droughtIndex to 
    #drought-induced beetle susceptibility (S_d; Eq. A1a-c in Appendix A)
    addRemoveNode (variableName = as.character (beetleLowerAsymDrS), nodeName = "LowerAsymDrS", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB)
    
    # Beetle ParamCDrS 
    # Set the parameter used in polynomial drought-susceptibility function 
    # to specify minimum susceptibility
    addRemoveNode (variableName = as.character (beetleParamCDrS), nodeName = "ParamCDrS", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB)
    
    # Beetle DrSFunction 
    # Set the string that defines shape of relationship between droughtIndex and 
    #susceptibilitIndexDrought: Can be "default","negLAsym" or "polyn"
    addRemoveNode (variableName = beetleDrSFunction, nodeName = "DrSFunction", 
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/", 
                   xmlfile = xmlfileBB)
    
    
    # Susceptibility weights ----
    
    # Modified the parameter file to set the weight for the wind susceptibility on the cell susceptibility value
    nodetochange <- xml2::xml_find_all (xmlfileBB, "//BeetleConfiguration/BeetleParameters/WISiCell/text()")
    xml2::xml_text (nodetochange) <- paste(sub (',', '', wWind), 
                                           sep = " ", collapse = " ") 
    
    # Add the weights for drought susceptibility, age and spruce share
    
    # If the weights are NA, then transformed to NULL 
    wDrought <- if(is.na (wDrought)) NULL else {wDrought}
    wBeetleHostShare <- if(is.na (wBeetleHostShare )) NULL else {wBeetleHostShare }
    wAge <- if(is.na (wAge)) NULL else {wAge}
    
    # If the weights for drought, share of spruce and age are NULL and the nodes are in the XML they are removed
    if (is.null (wDrought) && is.null (wBeetleHostShare) && is.null (wAge)){
      if(!is_empty (xml2::xml_find_all (xmlfileBB, "//BeetleConfiguration/BeetleParameters/DRSiCell"))){
        xml2:: xml_remove (xml2::xml_find_all (xmlfileBB, "//BeetleConfiguration/BeetleParameters/DRSiCell"))
      }
      if(!is_empty (xml2::xml_find_all (xmlfileBB, "//BeetleConfiguration/BeetleParameters/SSSiCell"))){
          xml2:: xml_remove (xml2::xml_find_all (xmlfileBB, "//BeetleConfiguration/BeetleParameters/SSSiCell"))
      }   
      if(!is_empty (xml2::xml_find_all (xmlfileBB, "//BeetleConfiguration/BeetleParameters/ADSiCell"))){
          xml2:: xml_remove (xml2::xml_find_all (xmlfileBB, "//BeetleConfiguration/BeetleParameters/ADSiCell"))
    }}
    
    # Il the values for the weights of drought, share of spruce and age are not null
    # they are added and if the node does not exist in the XML file
    # it is included
    if (!is.null (wDrought) && !is.null (wBeetleHostShare) && !is.null (wAge)){
      if(is_empty (xml2::xml_find_all (xmlfileBB, "//BeetleConfiguration/BeetleParameters/DRSiCell"))){
        
        xml2::xml_add_child (xml2::xml_children(xmlfileBB)[[1]],
                             xml2::read_xml(paste0 ("<DRSiCell>", wDrought, "</DRSiCell>")))
      } else {
        nodetochange <- xml2::xml_find_all (xmlfileBB, "//BeetleConfiguration/BeetleParameters/DRSiCell/text()")
        xml2::xml_text (nodetochange) <- paste (sub (',', '', wDrought), 
                                                sep = " ", collapse = " ") 
      }
      
      if(is_empty (xml2::xml_find_all (xmlfileBB, "//BeetleConfiguration/BeetleParameters/SSSiCell"))){
        
        xml2::xml_add_child (xml2::xml_children (xmlfileBB)[[1]],
                             xml2::read_xml (paste0 ("<SSSiCell>", wBeetleHostShare, "</SSSiCell>")))
      } else {
        nodetochange <- xml2::xml_find_all (xmlfileBB, "//BeetleConfiguration/BeetleParameters/SSSiCell/text()")
        xml2::xml_text (nodetochange) <- paste(sub (',', '', wBeetleHostShare), 
                                               sep = " ", collapse = " ") 
      }
      
      if(is_empty (xml2::xml_find_all (xmlfileBB, "//BeetleConfiguration/BeetleParameters/ADSiCell"))){
        
        xml2::xml_add_child (xml2::xml_children (xmlfileBB)[[1]],
                             xml2::read_xml (paste0 ("<ADSiCell>", wAge, "</ADSiCell>")))
      } else {
        nodetochange <- xml2::xml_find_all (xmlfileBB, "//BeetleConfiguration/BeetleParameters/ADSiCell/text()")
        xml2::xml_text (nodetochange) <- paste(sub (',', '', wAge), 
                                               sep = " ", collapse = " ") 
      }
    }
    
    # Share parameters susceptibility----
    
    # SsA parameter equation A 
    addRemoveNode (variableName = as.character (beetleSsA), nodeName = "SsA", underNode = "SpruceShareParameters",
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/SpruceShareParameters/", 
                   xmlfile = xmlfileBB)
    
    # SsK parameter equation K 
    addRemoveNode (variableName = as.character (beetleSsK), nodeName = "SsK", underNode = "SpruceShareParameters",
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/SpruceShareParameters/", 
                   xmlfile = xmlfileBB)
    
    # SsQ parameter equation Q 
    addRemoveNode (variableName = as.character (beetleSsQ), nodeName = "SsQ", underNode = "SpruceShareParameters",
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/SpruceShareParameters/", 
                   xmlfile = xmlfileBB)
    
    # Ssr parameter equation r 
    addRemoveNode (variableName = as.character (beetleSsr), nodeName = "Ssr", underNode = "SpruceShareParameters",
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/SpruceShareParameters/", 
                   xmlfile = xmlfileBB)
    
    # Ssm parameter equation m 
    addRemoveNode (variableName = as.character (beetleSsm), nodeName = "Ssm", underNode = "SpruceShareParameters",
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/SpruceShareParameters/", 
                   xmlfile = xmlfileBB)
    
    # Ssv parameter equation m 
    addRemoveNode (variableName = as.character (beetleSsm), nodeName = "Ssv", underNode = "SpruceShareParameters",
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/SpruceShareParameters/", 
                   xmlfile = xmlfileBB)
    

    # DBH or Age susceptibility parameters ----
    
    # Type parameter equation m 
    addRemoveNode (variableName = beetleSdbhAgetype, nodeName = "type", underNode = "SdbhAgeParameters",
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/SdbhAgeParameters/", 
                   xmlfile = xmlfileBB)
    
    # A parameter equation 
    addRemoveNode (variableName = beetleSdbhAgeA, nodeName = "A", underNode = "SdbhAgeParameters",
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/SdbhAgeParameters/", 
                   xmlfile = xmlfileBB)
    
    # r parameter equation 
    addRemoveNode (variableName = beetleSdbhAger, nodeName = "r", underNode = "SdbhAgeParameters",
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/SdbhAgeParameters/", 
                   xmlfile = xmlfileBB)
    
    # m parameter equation 
    addRemoveNode (variableName = beetleSdbhAgem, nodeName = "m", underNode = "SdbhAgeParameters",
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/SdbhAgeParameters/", 
                   xmlfile = xmlfileBB)
    
    # meanDBHfunction parameter equation 
    addRemoveNode (variableName = beetleSdbhmeanDBHfunction, nodeName = "meanDBHfunction", underNode = "SdbhAgeParameters",
                   nodePath = "//BeetleDisturbanceConfiguration/BeetleParameters/SdbhAgeParameters/", 
                   xmlfile = xmlfileBB)


    xml2::write_xml (xmlfileBB, paste0 (path, filename))
  }
    
    
    

  

  
 
        
     
    
 
      


    
    
  
  
  
# This function runs landclim with the settings done

# Example  
#controlfilename = "model-configuration_tutorial.xml"
#controlfilepath = "/Simulation/1_Input/"
#executablefilepath ="/Simulation/0_LandClim_Model/landclim/build/Release/netcoreapp3.1/"

#runlandclim (controlfilename = controlfilename,
#             controlfilepath = controlfilepath,
#             executablefilepath =executablefilepath,
#             system = "i")
  

  runlandclim <- function (executablefilepath, controlfilepath, controlfilename) {
    controlfile <- paste0 (getwd(), "/", controlfilepath, controlfilename)
    
    Osystem <- get_os()
    if (Osystem == "osx") {executablefilename <- "osx-x64/LandClim"
    } else if (Osystem == "windows") {executablefilename <- "win-x64/LandClim.exe"
    } else if (Osystem == "linux"){executablefilename <- "linux-x64/LandClim"}
    
    executablefile <- paste0 (getwd(), "/", executablefilepath, executablefilename)
    system2 (executablefile, controlfile)
  } 
    

# Run simulation 
  
  # Define the simulation parameters
  #scenarioname = "test" # Give a name for the scenario you are simulating 
  #climatefileinuse ="Climate1001_2000Dischma.dat"  # Climate file that you are going to use
  
 # windactivated = "true"    # Activate / Deactivate the bark beetle disturbance
  #beetleactivated = "true"
  
 # simulationlength = 100 # Length of the simulation in years
  #fulloutputtime = c(1, 10, 50, 100)
  
 # controlfilename = "model-configuration.xml" 
  #controlfilepath = "Inputfiles/Dischma/" # location of the control file 
  
  # Path to the input and output folders
 # inputpath = "Inputfiles/Dischma/" # Folder where the input files are
 # outputpath = paste0 ("Outputfiles/", scenarioname, "/") # Folder where the output files are going to be saved
  
  # model in use 
 # executablefilepath = "Model/landclim2DEV/"  #executablefilepath = "Model/landclim2.0.0/" # folder where the model LandClim is
  
  # bark beetle parameters 
 # beetlefilename  = "BarkbeetleParameters.xml"
 # beetleHostName = "piceabie"
 # weightWind = 0.7
 # weightDrought = NULL
 # weightBeetleHostShare = NULL
 # weightAge = NULL
  
  
  
  runsimulation <- function (
    
    scenarioname,      # Give a name for the scenario you are simulating 
    climatefileinuse,  # Climate file name that you are going to use
    windactivated,     # Activate / Deactivate the wind disturbance
    beetleactivated,   # Activate / Deactivate the bark beetle disturbance
    simulationlength,  # Length of the simulation in years
    fulloutputtime,    # Years from which you want to get the output
    controlfilename,   # name of the model control file
    controlfilepath,   # Path to the control file 
    inputpath,         # Path to the folder where the input files are
    outputfolder,      # Path to the folder where the you want to keep the output files
    executablefilepath,# Folder where the model LandClim is
    treeInitStateFile,
    # Beetle module parameters
    
    beetlefilename,         # Name of the XML beetle parameter file
    beetleHostName,         # Name of the tree host spp "piceabie" for Ips typographus
    weightWind,             # Weight assigned to the susceptibility parameter caused by wind 
    weightDrought,          # Weight assigned to the susceptibility parameter caused by drought
    weightBeetleHostShare,  # Weight assigned to the susceptibility parameter caused by share of tree host spp 
    weightAge,              # Weight assigned to the susceptibility parameter caused by host tree age
    beetleLowerAsymDrS, #lower Asymptote of logistic function that relates drought Index to drought-induced beetle susceptibility (S_d; Eq. A1a-c in Appendix A)
    beetleParamCDrS,    #parameter used in polynomial drought-susceptibility function to specify minimum susceptibility
    beetleDrSFunction,   #String that defines shape of relationship between droughtIndex and susceptibilitIndexDrought: Can be "default","negLAsym" or "polyn"
    ...
    ){
    
    # Creates the output path to the scenario folder inside the output folder  
    outputpath = paste0 (outputfolder, scenarioname, "/")
    
    # Creates a folder to save the output data with the scenario name
    
    dir.create (paste0 (outputfolder, scenarioname)) 
    
    # This function is going to set up the model control file  paths for input and output folders
    
    cfPaths (controlfilename = controlfilename,
             controlfilepath = controlfilepath,
             inputpath = inputpath,
             outputpath = outputpath)
    
    
    # bark beetle module set up
    
    configurationBB (filename = beetlefilename,
                     path = inputpath,
                     beetleHostName = "piceabie",
                     wWind = weightWind,
                     wDrought = weightDrought,
                     wBeetleHostShare = weightBeetleHostShare,
                     wAge = weightAge,
                     
                     beetleLowerAsymDrS = beetleLowerAsymDrS , #lower Asymptote of logistic function that relates drought Index to drought-induced beetle susceptibility (S_d; Eq. A1a-c in Appendix A)
                     beetleParamCDrS = beetleParamCDrS, #parameter used in polynomial drought-susceptibility function to specify minimum susceptibility
                     beetleDrSFunction = beetleDrSFunction #String that defines shape of relationship between droughtIndex and susceptibilitIndexDrought: Can be "default","negLAsym" or "polyn"
    )
    
    # add module configurations
    
    LCconfiguration (controlfilename = controlfilename,
                     controlfilepath = controlfilepath,
                     climateFile =  climatefileinuse,
                     climateFormat = "Yearly",  
                     climateRandomization =  "Restart",
                     
                     plantingEnable = "false",
                     fireEnable = "false",
                     harvestEnable = "false",
                     
                     runtimeStartYear = 1,
                     runtimeEndYear = simulationlength,
                     
                     windEnable =  windactivated,
                     beetleEnable = beetleactivated,
                     
                     
                     # Output specifications
                     fulloutputyears =  fulloutputtime, # years from where you want to obtain the full output files 
                     aggregateoutput1 = c("elevation", "Dem.txt"),
                     aggregateoutput2 = c("landtype", "LandType.txt"),
                     
                     # Names of the map files
                     ElevationFile = "Dem.txt",
                     SlopeFile = "Slope.txt",
                     LandMaskFile = "Mask.txt",
                     LandTypeFile = "LandType.txt",
                     AspectFile = "Aspect.txt",
                     SoilDepthFile = "Soil.txt",
                     
                     # Name of the configuration files 
                     BeetleParameterFile = "BarkBeetleParameters.xml",
                     SpeciesConfigurationFile = "SpeciesConfiguration.xml",#"SpeciesCentralEuropeInclHerb.xml",
                     LandTypeParameterFile = "LandTypeParameters.xml",
                     treeInitStateFile = treeInitStateFile,
                     HarvestStandsFile = "",# This file is empty because for this simulation we are not using this input
                     HarvestManagementAreasFile = "") # This file is empty because for this simulation we are not using this input
    
    
    # Run landclim 
    
    controlfilename = controlfilename
    executablefilepath = "Model/landclim2DEV/"
    
    runlandclim (controlfilename = controlfilename,
                 controlfilepath = controlfilepath,
                 executablefilepath = executablefilepath)
    
  }
  
  
  t_col <- function(color, percent = 50, name = NULL) {
    #      color = color name
    #    percent = % transparency
    #       name = an optional name for the color
    
    ## Get RGB values for named color
    rgb.val <- col2rgb(color)
    
    ## Make new color using input color as base and alpha set by transparency
    t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                 max = 255,
                 alpha = (100 - percent) * 255 / 100,
                 names = name)
    
    ## Save the color
    invisible(t.col)
  }

 # Function to extract the proportion of Spruce across several scenarios and 
  
  sprProp <- function (scenarioPath){

    fulloutfiles <- list.files (path = scenarioPath,
                                pattern = "fullOut_year_", full.names = T)
    dt  <- NULL
    
    for (n in 1:length (fulloutfiles)){
      FullOutfile <- read.csv (fulloutfiles[n], header = T)
      
      dominantSpecies <- FullOutfile  %>% 
        mutate (cohortBiomass = stems * biomass) %>% 
        group_by (cell) %>% 
        mutate (biomassTotalCell = sum (cohortBiomass)) %>% 
        ungroup () %>% 
        mutate (biomassProp =  (cohortBiomass / biomassTotalCell) * 100) %>% 
        group_by (row, col, cell) %>% 
        slice (which.max (biomassProp)) %>% 
        select (row, col, cell, species, biomassProp)
      
      SppProp <- dominantSpecies %>%
        mutate (domSpp = ifelse (biomassProp > 70, species, "mixed"))  %>% 
        group_by (domSpp) %>% 
        dplyr::summarise (cellCount = n()) %>% 
        mutate (PropForestType = cellCount / sum (cellCount) * 100 ) %>% 
        mutate (Time = paste0 (stringr::str_match (fulloutfiles[n], "fullOut_year_\\s*(.*?)\\s*.csv")[,2],
                               " years")) %>% 
        mutate (Scenario = scenarioPath) 
      
      SprcData  <- SppProp %>% 
        filter(domSpp == "piceabie") %>% 
        mutate (time = sub(" year.*", "", Time))
      
      dt  <- rbind (dt, SprcData)
      
    }
    return (dt)}
    

    

  
