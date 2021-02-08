################################################################################
# Run a LandClim simulation
# Author: Olalla Díaz Yáñez (olalladiaz.net)
# Aim: Run a Landclim simulation with the desired set-up
################################################################################

# 0) Install packages, if needed

    # To run simulations you are going to need a set of packages installed in  R
    options(repos = structure (c(CRAN = "https://stat.ethz.ch/CRAN/")))

    # packages to install
    list.of.packages <- c("xml2", "dplyr", "ggplot2", "RColorBrewer", "gplots", "plotrix")

    # check if the packages are installed already
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

    # install missing packages
    if (length (new.packages)) install.packages (new.packages)


    # 1) Upload functions

    source ("Code/Functions.R")


# SIMULATION LOOP

    # SIMULATION INPUT

    simulationLengthNumber <- 1
    simulationInitNumber <- 1
    simulationEndNumber <- simulationInitNumber + (simulationLengthNumber - 1)
    simulationlength = 600
    rootNameScenarios <- "testTime"

    # table of input parameters
    listscenarionames <- list()
    for (i in simulationInitNumber:simulationEndNumber){
      listscenarionames[i] <-  paste0 (rootNameScenarios, i)
    }

    inputvariables <- data.frame(
      scenarioname = unlist (listscenarionames),
      climatefileinuse = "Climate1001_2000Dischma.dat", #"Climate1001_2000Dischma.dat",  # Climate file that you are going to use

      windactivated = "true" ,# Activate / Deactivate the bark beetle disturbance
      beetleactivated = "true",

      simulationlength = simulationlength, # Length of the simulation in years
      fulloutputtime = NA, #list(c(1, 10, 50, 100)),

      controlfilename = "model-configurationTime.xml",
      controlfilepath = "Inputfiles/Dischma/", # location of the control file

      # Path to the input and output folders
      inputpath = "Inputfiles/Dischma/" ,# Folder where the input files are
      outputfolder = "Outputfiles/",     # General folder where the output files are going to be saved, inside this folder the scenario folders will be

      # Use initial set of trees from Equilibrium simulation

      treeInitStateFile = "TreeInit2000EqEst.csv", #"TreeInit2000RCP26.csv",

      # model in use
      executablefilepath = "Model/landclim2DEV/",  #executablefilepath = "Model/landclim2.0.0/" # folder where the model LandClim is

      # bark beetle parameters
      beetlefilename  = "BarkbeetleParameters.xml",
      beetleHostName = "piceabie",

      # Importance of susceptibility values
      weightWind =  0.6, #unlist(lapply (listx, function(x) x[1])), #NA
      weightDrought = NA, # lapply (listx, function(x) x[2]),#NA
      weightBeetleHostShare = NA, # lapply (listx, function(x) x[3]),#NA,
      weightAge =  NA, # lapply (listx, function(x) x[4]),#NA,

      # drought
      beetleDrSFunction = "default", #"negLAsym" #"polyn"
      beetleLowerAsymDrS = 0, #lower Asymptote of logistic function that relates drought Index to drought-induced beetle susceptibility (S_d; Eq. A1a-c in Appendix A)
      beetleParamCDrS = 0    #parameter used in polynomial drought-susceptibility function to specify minimum susceptibility

      # spruce share

      #<SsA>0.08</SsA>

      #  <SsK>1<SsK>

      #  <SsQ>0.2674846</SsQ>

      #  <Ssr>6.8455438</Ssr>

      #  <Ssm>0.375</Ssm>

      #   <Ssv>0.3002734</Ssv>



    )

    inputvariables$fulloutputtime <- list(c(1,  seq (100, 600, 100)))
    #inputvariables$fulloutputtime <-  list(c(1, seq(from = 0, to = simulationlength, by = 10)[-1]))

    # Random weights:

    # Random weights to susceptibility parameters
    listx <- list ()

    for (i in 1:simulationLengthNumber){
      x <- runif(4)
      weigths <- x/sum(x)
      listx[[i]] <- weigths
    }

    inputvariables$weightWind =  unlist(lapply (listx, function(x) x[1]))
    inputvariables$weightDrought = unlist(lapply (listx, function(x) x[2]))
    inputvariables$weightBeetleHostShare = unlist(lapply (listx, function(x) x[3]))
    inputvariables$weightAge = unlist(lapply (listx, function(x) x[4]))


    # Save as a txt
    inputvariablesSave <- inputvariables
    inputvariablesSave$fulloutputtime <- vapply (inputvariablesSave$fulloutputtime, paste, collapse = ", ", character(1L))
    write.table (inputvariablesSave,
                 paste0 ("Outputfiles/", rootNameScenarios, "L",  simulationlength,
                         "N", simulationInitNumber, "_", simulationEndNumber, ".txt"),
                 append = FALSE, sep = "\t", dec = ".")


    ## SIMULATION

    # Load parameter table

    inputvariables2 <- read.table (paste0 ("Outputfiles/", rootNameScenarios,
                                           "L",  simulationlength,
                                           "N",  simulationInitNumber, "_", simulationEndNumber, ".txt"),
                                   header = T, sep = "\t", dec = ".")

    a <- strsplit (as.character (inputvariables2$fulloutputtime),',')
    inputvariables2$fulloutputtime <- lapply (a, FUN = as.numeric)

    inputvariables <- inputvariables2

    for (i in 1:nrow (inputvariables)){

      runsimulation (

        # Define the simulation parameters
        scenarioname = inputvariables$scenarioname[i], #"test" ,# Give a name for the scenario you are simulating
        climatefileinuse =  inputvariables$climatefileinuse[i], #"Climate1001_2000Dischma.dat",  # Climate file that you are going to use

        windactivated =  inputvariables$windactivated [i], #"true" ,# Activate / Deactivate the bark beetle disturbance
        beetleactivated =  inputvariables$beetleactivated[i], #"true",

        simulationlength =  inputvariables$simulationlength[i], #100, # Length of the simulation in years
        fulloutputtime =  inputvariables$fulloutputtime [[i]], #c(1, 10, 50, 100),

        controlfilename =  inputvariables$controlfilename[i], #"model-configuration.xml",
        controlfilepath =  inputvariables$controlfilepath[i], #"Inputfiles/Dischma/", # location of the control file

        # Initial tree status
        treeInitStateFile = inputvariables$treeInitStateFile[i],

        # Path to the input and output folders
        inputpath =  inputvariables$inputpath[i], #"Inputfiles/Dischma/" ,# Folder where the input files are
        outputfolder =  inputvariables$outputfolder[i], #"Outputfiles/",     # General folder where the output files are going to be saved, inside this folder the scenario folders will be

        # model in use
        executablefilepath = inputvariables$executablefilepath[i], #"Model/landclim2DEV/",  #executablefilepath = "Model/landclim2.0.0/" # folder where the model LandClim is

        # bark beetle parameters
        beetlefilename  =  inputvariables$beetlefilename[i], #"BarkbeetleParameters.xml",
        beetleHostName = inputvariables$beetleHostName[i], #"piceabie",
        weightWind =  inputvariables$weightWind[i], #0.7,
        weightDrought =  inputvariables$weightDrought[i], #NULL,
        weightBeetleHostShare =  inputvariables$weightBeetleHostShare[i], #NULL,
        weightAge =  inputvariables$weightAge[i], #NULL,

        beetleLowerAsymDrS = inputvariables$beetleLowerAsymDrS[i], #0, #lower Asymptote of logistic function that relates drought Index to drought-induced beetle susceptibility (S_d; Eq. A1a-c in Appendix A)
        beetleParamCDrS =  inputvariables$beetleParamCDrS[i], #0,    #parameter used in polynomial drought-susceptibility function to specify minimum susceptibility
        beetleDrSFunction =  inputvariables$beetleDrSFunction[i], #"default" #String that defines shape of relationship between droughtIndex and susceptibilitIndexDrought: Can be "default","negLAsym" or "polyn"
      )

    }

















    ### Plot figures ----

    scenario <- scenarionameLIST[i]


    if (simulationlength > 100){
      fulloutoptionsLIST <- c(100, simulationlength)
    } else {fulloutoptionsLIST <- 100}


    for (n in 1:length(fulloutoptionsLIST)){

    # Load full output for specified year

    fulloutfileinuse <- fulloutoptionsLIST [n]

    FullOutfile <- read.csv (paste0 ("Outputfiles/", scenario, "/fullOut_year_",
                                     fulloutfileinuse, ".csv"), header = T)


    #For displaying the landscape maps, we first need to aggregate the data:

    #(1) biomass per cell,
    #(2) number of stems per cell,
    #(3) dominant species per cell.

    library (dplyr) # Required library activated

    #(1) Calculate biomass sum per grid-cell

    full_aggregate_biomass <- FullOutfile  %>%
      group_by (row, col) %>%
      summarise (CellBiomass = sum (biomass))

    #(2) Calculate sum of stems per grid-cell

    full_aggregate_stems <- FullOutfile  %>%
      group_by (row, col) %>%
      summarise(CellStems = sum (stems))

    #(3) Calculate dominant species based on tree height

    dominantSpecies <- FullOutfile  %>%
      mutate (cohortBiomass = stems * biomass) %>%
      group_by (row, col, cell, species) %>%
      summarise (speciesStems = sum (stems),
                 speciesBiomass = sum (cohortBiomass),
                 height = max (height)) %>%
      group_by (row, col, cell) %>%
      filter (speciesStems == max (speciesStems)) %>%
      filter(speciesBiomass == max (speciesBiomass)) %>%
      filter (height == max(height))

    # Plot the distribution of tree biomass over the landscape

    library (ggplot2)
    TreeBiomassPlot <- ggplot(full_aggregate_biomass) +
      ggtitle (paste0 ("Distribution of tree biomass", " on the scenario ", scenario,
                       "(yr ", fulloutfileinuse, ")")) +
      geom_raster (aes (x = col, y = row, fill = CellBiomass)) +
      scale_fill_gradient(low = "lightgreen", high = "darkgreen", space = "Lab")

    ggsave(paste0 ("TreeBiomassPlot", scenario, "_", fulloutfileinuse, ".png"),
           plot = TreeBiomassPlot, path = "Figures/")


    # Plot number of stems per cell

    StemsCellPlot <- ggplot (full_aggregate_stems) +
      ggtitle (paste0 ("Number of stems per cell", " on the scenario ", scenario,
                       "(yr ", fulloutfileinuse, ")")) +
      geom_raster (aes (x = col, y = row, fill = CellStems)) +
      scale_fill_gradient (low = "lightgreen", high = "darkgreen", space = "Lab")

    ggsave(paste0 ("StemsCellPlot", scenario, "_", fulloutfileinuse, ".png"),
           plot = StemsCellPlot, path = "Figures/")

    # Plot distribution of dominant species

    library (RColorBrewer)
    DomSpeciesPlot <- ggplot (dominantSpecies) +
      ggtitle (paste0 ("Distribution of dominant species", " on the scenario ",
                       scenario,  "(yr ", fulloutfileinuse, ")")) +
      geom_raster (aes (x = col, y = row, fill = species)) +
      scale_fill_brewer (palette = "Spectral")

    ggsave(paste0 ("DomSpeciesPlot", scenario, "_", fulloutfileinuse, ".png"),
           plot =  DomSpeciesPlot, path = "Figures/")

  }




    # load elevation biomass (in tons per ha)
    elevBiomass <- read.csv (paste0 ("Outputfiles/", scenario,
                                     "/BiomassOut_elevation.csv"), header = T)

    # abbreviated species names
    SpeciesNames <- names (elevBiomass)[2 : (ncol (elevBiomass) - 2)]

    # load library required for colour palette
    library(gplots)
    myCols <- rev (rich.colors (length (SpeciesNames))) # create colour palette

    for (n in 1:length(ListOfElevations )){

      SelectElevation <- ListOfElevations [n]
      ElevBiomassSubset <- elevBiomass [elevBiomass$elevation == SelectElevation, ]

      png(paste0 ("Figures/BioSppAltPlot", scenario, SelectElevation, ".png"),
          width = 900, height = 600, units = 'px', res= 100)

      # Plot succession figure
      plotrix::stackpoly (ElevBiomassSubset [SpeciesNames], stack = T, col = myCols,
                 main = paste ("Species succession (elevation ", SelectElevation, ")",
                               " on the scenario ", scenario, sep = ""),
                 ylab = "Biomass (tons per ha)", xlab = "Time (decades)",
                 axis4 = F, cex.lab = 1, las = 2, cex.axis = 2, ylim = c(0, 400))

      # Add legend
      legend ("topright", legend = SpeciesNames, cex = 0.6, fill = myCols, ncol = 6)
      dev.off()


      elevBiomass <- read.csv (paste0 ("Outputfiles/", scenario, "/BiomassOut_elevation.csv"),
                               header = T) # load elevation biomass #biomass (in tons per ha)

      # abbreviated species names
      SpeciesNames <- names (elevBiomass)[2 : (ncol (elevBiomass) - 2)]

    }


      for (n in 1: length(ListOfSelectYear)){
        SelectYear <- ListOfSelectYear[n]

        # Select one year in the simulation
        ElevBiomassSubsetYr <- elevBiomass [elevBiomass$year == SelectYear , ]

        png(paste0 ("Figures/BioSppYrPlot", scenario, SelectYear, ".png"),
            width = 900, height = 600, units = 'px', res= 100)

        # Plot succession figure
        plotrix::stackpoly (x = ElevBiomassSubsetYr$elevation,
                   y =  ElevBiomassSubsetYr [SpeciesNames], stack = T, col = myCols,
                   main = paste ("Species elevation gradient (year ", SelectYear, ")",
                                 " on the scenario ", scenario, sep = ""),
                   ylab = "Biomass (tons per ha)", xlab = "Elevation (m)",
                   axis4 = F, cex.lab = 1, las = 2, cex.axis = 2, ylim = c(0, 400))

        # Add legend
        legend ("topright", legend = SpeciesNames, cex = 0.6, fill = myCols, ncol = 6)
        dev.off()
      }
  }



