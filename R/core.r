default_barkbeetle <- read.csv("default-data/barkbeetle.csv", fileEncoding = "UTF-8-BOM", header = T)
default_config <- read.csv("default-data/config.csv", fileEncoding = "UTF-8-BOM", header = T)
default_landtypeparameters <- read.csv("default-data/landtypeparameters.csv", fileEncoding = "UTF-8-BOM", header = T)
default_plantingparameters <- read.csv("default-data/plantingparameters.csv", fileEncoding = "UTF-8-BOM", header = T)
default_randomstate <- read.csv("default-data/randomstate.csv", fileEncoding = "UTF-8-BOM", header = T)
default_species <- read.csv("default-data/species.csv", fileEncoding = "UTF-8-BOM", header = T)



runLandClim <- function(config = data.frame(), configXMLpath = "", barkbeetle = data.frame(), barkbeetleXMLpath = "", landtypeparameters = data.frame(), landtypeparametersXMLpath = "", plantingparameters = data.frame(), plantingparametersXMLpath = "", randomstate = data.frame(), randomstateXMLpath = "", species = data.frame(), speciesXMLpath = "", workspacePath = "workspace", binPath = "bin") {
    if (configXMLpath == "")
        configXMLpath <- paste0(workspacePath, "/modelConfiguration.xml")
    if (barkbeetleXMLpath == "")
        barkbeetleXMLpath <- paste0(workspacePath, "/barkbeetle-parameters.xml")
    if (landtypeparametersXMLpath == "")
        landtypeparametersXMLpath <- paste0(workspacePath, "/Landtype_parameters.xml")
    if (plantingparametersXMLpath == "")
        plantingparametersXMLpath <- paste0(workspacePath, "/planting-parameters.xml")
    if (randomstateXMLpath == "")
        randomstateXMLpath <- paste0(workspacePath, "/randomstate.xml")
    if (speciesXMLpath == "")
        speciesXMLpath <- paste0(workspacePath, "/species-configuration.xml")

    writeXML(config, "config", configXMLpath)
    writeXML(barkbeetle, "barkbeetle", barkbeetleXMLpath)
    writeXML(landtypeparameters, "landtypeparameters", landtypeparametersXMLpath)
    writeXML(plantingparameters, "plantingparameters", plantingparametersXMLpath)
    writeXML(randomstate, "randomstate", randomstateXMLpath)
    writeXML(species, "species", speciesXMLpath)

    if(.Platform$OS.type == "Unix")
        system2(paste0(binPath, '/LandClim'), args=paste0(getwd(), "/", configXMLpath))
    else
        system2(paste0(binPath, '/LandClim.exe'), args=paste0(getwd(), "/", configXMLpath))
}



writeXML <- function(data = data.frame(), type = "none", filePathXML = "temp.xml") {
    if (file.exists(filePathXML)){
        print(paste0("XML file already exists: ", filePathXML))
        return(-1)
    }
    if (tolower(type) == 'config') {
        if (length (data) == 0) { 
            data <- default_config
        } else {
            data <- cbind(data, default_config[!names(default_config) %in% names(data)])
        }
        data_columns = names(data)

        root <- xml2::xml_new_root(.value="ControlFile")

        # write InputConfiguration
        InputConfiguration <- xml2::xml_add_child(root, .value="InputConfiguration")
        OutputConfiguration <- xml2::xml_add_child(root, .value="OutputConfiguration")
        


        # populate InputConfiguration
        for (col in data_columns) {
            if (startsWith(col, "InputConfiguration_")) {
                if (!is.na(data[col][[1]])) {
                    xml2::xml_set_text(xml2::xml_add_child(InputConfiguration, .value=strsplit(col, '_')[[1]][2]), toString(data[col][[1]]))
                }
            }
        }

        if (length(xml2::xml_find_all(InputConfiguration, ".//InputPath")) == 0)
            xml2::xml_add_child(InputConfiguration, .value="InputPath")
        

        # populate OutputConfiguration
        if (!is.na(data["OutputConfiguration_OutputDirectory"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration, .value="OutputDirectory"), toString(data["OutputConfiguration_OutputDirectory"][[1]]))
        else
            xml2::xml_add_child(OutputConfiguration, .value="OutputDirectory")

        if ("OutputConfiguration_CsvFieldDelimiter" %in% data_columns) {
            if (!is.na(data["OutputConfiguration_CsvFieldDelimiter"][[1]]))
                xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration, .value="CsvFieldDelimiter"), toString(data["OutputConfiguration_CsvFieldDelimiter"][[1]]))
            else
                xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration, .value="CsvFieldDelimiter"), ",")

        } else
            xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration, .value="CsvFieldDelimiter"), ",")
        
        
        if (!is.na(data["OutputConfiguration_Aggregations_OutputAggregation_Names"][[1]]) && !is.na(data["OutputConfiguration_Aggregations_OutputAggregation_Maps"][[1]])) {
            OutputConfiguration_Aggregations <- xml2::xml_add_child(OutputConfiguration, .value="Aggregations")
            name_list <- strsplit(toString(data["OutputConfiguration_Aggregations_OutputAggregation_Names"][[1]]), "#")[[1]]
            map_list <- strsplit(toString(data["OutputConfiguration_Aggregations_OutputAggregation_Maps"][[1]]), "#")[[1]]

            if (length(name_list) != length(map_list)) {
                print("Contradicting data: OutputConfiguration_Aggregations_OutputAggregation_Names & OutputConfiguration_Aggregations_OutputAggregation_Maps")
                return(-1)
            }
            for (i in 1:length(name_list)) {
                OutputConfiguration_Aggregations_OutputAggregation <- xml2::xml_add_child(OutputConfiguration_Aggregations, .value="OutputAggregation")
                xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Aggregations_OutputAggregation, .value="Name"), toString(name_list[i]))
                xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Aggregations_OutputAggregation, .value="Map"), toString(map_list[i]))
            }

        }

        OutputConfiguration_Climate <- xml2::xml_add_child(OutputConfiguration, .value="Climate")
        
        if (!is.na(data["OutputConfiguration_Climate_Coordinates_Coordinate_Rows"][[1]]) && !is.na(data["OutputConfiguration_Climate_Coordinates_Coordinate_Cols"][[1]])) {
            OutputConfiguration_Climate_Coordinates <- xml2::xml_add_child(OutputConfiguration_Climate, .value="Coordinates")
            rows <- strsplit(toString(data["OutputConfiguration_Climate_Coordinates_Coordinate_Rows"][[1]]), "#")[[1]]
            cols <- strsplit(toString(data["OutputConfiguration_Climate_Coordinates_Coordinate_Cols"][[1]]), "#")[[1]]
            
            if (length(rows) != length(cols)) {
                print("Contradicting data: OutputConfiguration_Climate_Coordinates_Coordinate_Rows & OutputConfiguration_Climate_Coordinates_Coordinate_Cols")
                return(-1)
            }

            for (i in 1:length(rows)) {
                OutputConfiguration_Climate_Coordinates_Coordinate <- xml2::xml_add_child(OutputConfiguration_Climate_Coordinates, .value="Coordinate")
                xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Climate_Coordinates_Coordinate, .value="Row"), toString(rows[i]))
                xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Climate_Coordinates_Coordinate, .value="Col"), toString(cols[i]))
            }

        }

        xml2::xml_set_text(xml2::xml_add_child(xml2::xml_add_child(OutputConfiguration_Climate, .value="Climate"), .value="Enabled"), toString(data["OutputConfiguration_Climate_Climate_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(xml2::xml_add_child(OutputConfiguration_Climate, .value="SoilMoisture"), .value="Enabled"), toString(data["OutputConfiguration_Climate_SoilMoisture_Enabled"][[1]]))

        OutputConfiguration_Cohort <- xml2::xml_add_child(OutputConfiguration, .value="Cohort")
        OutputConfiguration_Cohort_Full <- xml2::xml_add_child(OutputConfiguration_Cohort, .value="Full")
        xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Cohort_Full, .value="Enabled"), toString(data["OutputConfiguration_Cohort_Full_Enabled"][[1]]))
        
        if("OutputConfiguration_Cohort_Full_Years" %in% data_columns)
            xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Cohort_Full, .value="Years"), gsub("#", " ", data["OutputConfiguration_Cohort_Full_Years"][[1]]))

        xml2::xml_set_text(xml2::xml_add_child(xml2::xml_add_child(OutputConfiguration_Cohort, .value="Biomass"), .value="Enabled"), toString(data["OutputConfiguration_Cohort_Biomass_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(xml2::xml_add_child(OutputConfiguration_Cohort, .value="StemCount"), .value="Enabled"), toString(data["OutputConfiguration_Cohort_StemCount_Enabled"][[1]]))
        
        
        if (!is.na(data["OutputConfiguration_Cohort_Aggregations"][[1]])) {
            OutputConfiguration_Cohort_Aggregations <- xml2::xml_add_child(OutputConfiguration_Cohort, .value="Aggregations")
            aggregations <- strsplit(toString(data["OutputConfiguration_Cohort_Aggregations"][[1]]), "#")[[1]]
            for (i in 1:length(aggregations))
                xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Cohort_Aggregations, .value="Aggregation"), toString(aggregations[i]))
        }
        
        OutputConfiguration_Grass <- xml2::xml_add_child(OutputConfiguration, .value="Grass")
        xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Grass, .value="Enabled"), toString(data["OutputConfiguration_Grass_Enabled"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Grass, .value="BiomassYear")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Grass_BiomassYear_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Grass_BiomassYear_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Grass, .value="Growth")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Grass_Growth_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Grass_Growth_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Grass, .value="Yield")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Grass_Yield_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Grass_Yield_Start"][[1]]))


        OutputConfiguration_Fuel <- xml2::xml_add_child(OutputConfiguration, .value="Fuel")
        xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Fuel, .value="Enabled"), toString(data["OutputConfiguration_Fuel_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Fuel, .value="Start"), toString(data["OutputConfiguration_Fuel_Start"][[1]]))

        OutputConfiguration_Mortality <- xml2::xml_add_child(OutputConfiguration, .value="Mortality")
        OutputConfiguration_Mortality_Detail <- xml2::xml_add_child(OutputConfiguration_Mortality, .value="Detail")
        OutputConfiguration_Mortality_Summary <- xml2::xml_add_child(OutputConfiguration_Mortality, .value="Summary")
        xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Mortality_Detail, .value="Enabled"), toString(data["OutputConfiguration_Mortality_Detail_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Mortality_Detail, .value="Start"), toString(data["OutputConfiguration_Mortality_Detail_Start"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Mortality_Summary, .value="Enabled"), toString(data["OutputConfiguration_Mortality_Summary_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Mortality_Summary, .value="Start"), toString(data["OutputConfiguration_Mortality_Summary_Start"][[1]]))


        OutputConfiguration_Planting <- xml2::xml_add_child(OutputConfiguration, .value="Planting")
        child <- xml2::xml_add_child(OutputConfiguration_Planting, .value="Detail")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Planting_Detail_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Planting_Detail_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Planting, .value="DetailBiomass")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Planting_DetailBiomass_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Planting_DetailBiomass_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Planting, .value="DetailStemcount")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Planting_DetailStemcount_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Planting_DetailStemcount_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Planting, .value="Summary")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Planting_Summary_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Planting_Summary_Start"][[1]]))



        OutputConfiguration_Harvest <- xml2::xml_add_child(OutputConfiguration, .value="Harvest")
        child <- xml2::xml_add_child(OutputConfiguration_Harvest, .value="Detail")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Harvest_Detail_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Harvest_Detail_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Harvest, .value="DetailBiomass")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Harvest_DetailBiomass_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Harvest_DetailBiomass_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Harvest, .value="DetailStemcount")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Harvest_DetailStemcount_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Harvest_DetailStemcount_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Harvest, .value="Summary")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Harvest_Summary_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Harvest_Summary_Start"][[1]]))


        OutputConfiguration_Light <- xml2::xml_add_child(OutputConfiguration, .value="Light")
        if (!is.na(data["OutputConfiguration_Light_Coordinates_Coordinate_Rows"][[1]]) && !is.na(data["OutputConfiguration_Light_Coordinates_Coordinate_Cols"][[1]])) {
            OutputConfiguration_Light_Coordinates <- xml2::xml_add_child(OutputConfiguration_Light, .value="Coordinates")
            rows <- strsplit(toString(data["OutputConfiguration_Light_Coordinates_Coordinate_Rows"][[1]]), "#")[[1]]
            cols <- strsplit(toString(data["OutputConfiguration_Light_Coordinates_Coordinate_Cols"][[1]]), "#")[[1]]
            
            if (length(rows) != length(cols)) {
                print("Contradicting data: OutputConfiguration_Light_Coordinates_Coordinate_Rows & OutputConfiguration_Light_Coordinates_Coordinate_Cols")
                return(-1)
            }

            for (i in 1:length(rows)) {
                OutputConfiguration_Light_Coordinates_Coordinate <- xml2::xml_add_child(OutputConfiguration_Light_Coordinates, .value="Coordinate")
                xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Light_Coordinates_Coordinate, .value="Row"), toString(rows[i]))
                xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Light_Coordinates_Coordinate, .value="Col"), toString(cols[i]))
            }

        }

        child <- xml2::xml_add_child(OutputConfiguration_Light, .value="Detail")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Light_Detail_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Light_Detail_Start"][[1]]))


        child <- xml2::xml_add_child(OutputConfiguration_Light, .value="LAI")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Light_LAI_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Light_LAI_Start"][[1]]))



        OutputConfiguration_Drought <- xml2::xml_add_child(OutputConfiguration, .value="Drought")

        child <- xml2::xml_add_child(OutputConfiguration_Drought, .value="Tree")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Drought_Tree_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Drought_Tree_Start"][[1]]))


        child <- xml2::xml_add_child(OutputConfiguration_Drought, .value="Herb")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Drought_Herb_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Drought_Herb_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Drought, .value="Sapling")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Drought_Sapling_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Drought_Sapling_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Drought, .value="Stand")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Drought_Stand_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Drought_Stand_Start"][[1]]))

        OutputConfiguration_Stand <- xml2::xml_add_child(OutputConfiguration, .value="Stand")
        xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_Stand, .value="DbhClassSize"), toString(data["OutputConfiguration_Stand_DbhClassSize"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Stand, .value="Detail")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Stand_Detail_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Stand_Detail_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Stand, .value="Species")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Stand_Species_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Stand_Species_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Stand, .value="DbhClasses")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Stand_DbhClasses_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Stand_DbhClasses_Start"][[1]]))

        OutputConfiguration_ManagementArea <- xml2::xml_add_child(OutputConfiguration, .value="ManagementArea")
        xml2::xml_set_text(xml2::xml_add_child(OutputConfiguration_ManagementArea, .value="DbhClassSize"), toString(data["OutputConfiguration_ManagementArea_DbhClassSize"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_ManagementArea, .value="Detail")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_ManagementArea_Detail_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_ManagementArea_Detail_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_ManagementArea, .value="Species")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_ManagementArea_Species_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_ManagementArea_Species_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_ManagementArea, .value="DbhClasses")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_ManagementArea_DbhClasses_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_ManagementArea_DbhClasses_Start"][[1]]))


        OutputConfiguration_Fire <- xml2::xml_add_child(OutputConfiguration, .value="Fire")
        child <- xml2::xml_add_child(OutputConfiguration_Fire, .value="Detail")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Fire_Detail_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Fire_Detail_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Fire, .value="Summary")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Fire_Summary_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Fire_Summary_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Fire, .value="History")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Fire_History_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Fire_History_Start"][[1]]))




        OutputConfiguration_Wind <- xml2::xml_add_child(OutputConfiguration, .value="Wind")
        child <- xml2::xml_add_child(OutputConfiguration_Wind, .value="Detail")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Wind_Detail_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Wind_Detail_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Wind, .value="Summary")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Wind_Summary_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Wind_Summary_Start"][[1]]))




        OutputConfiguration_Pasture <- xml2::xml_add_child(OutputConfiguration, .value="Pasture")
        child <- xml2::xml_add_child(OutputConfiguration_Pasture, .value="Detail")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Pasture_Detail_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Pasture_Detail_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Pasture, .value="Year")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Pasture_Year_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Pasture_Year_Start"][[1]]))


        OutputConfiguration_Beetle <- xml2::xml_add_child(OutputConfiguration, .value="Beetle")
        child <- xml2::xml_add_child(OutputConfiguration_Beetle, .value="Detail")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Beetle_Detail_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Beetle_Detail_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Beetle, .value="Summary")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Beetle_Summary_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Beetle_Summary_Start"][[1]]))

        child <- xml2::xml_add_child(OutputConfiguration_Beetle, .value="Cell")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Enabled"), toString(data["OutputConfiguration_Beetle_Cell_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Start"), toString(data["OutputConfiguration_Beetle_Cell_Start"][[1]]))





        # write ClimateConfiguration
        ClimateConfiguration <- xml2::xml_add_child(root, .value="ClimateConfiguration")
        xml2::xml_set_text(xml2::xml_add_child(ClimateConfiguration, .value="ClimateFile"), toString(data["ClimateConfiguration_ClimateFile"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(ClimateConfiguration, .value="Format"), toString(data["ClimateConfiguration_Format"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(ClimateConfiguration, .value="Randomization"), toString(data["ClimateConfiguration_Randomization"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(ClimateConfiguration, .value="WaterBalanceModel"), toString(data["ClimateConfiguration_WaterBalanceModel"][[1]]))
        if("ClimateConfiguration_Sequence" %in% data_columns)
            xml2::xml_set_text(xml2::xml_add_child(ClimateConfiguration, .value="Sequence"), toString(gsub("#", " ", data["ClimateConfiguration_Sequence"][[1]])))




        # write PlantingDisturbanceConfiguration
        PlantingDisturbanceConfiguration <- xml2::xml_add_child(root, .value="PlantingDisturbanceConfiguration")
        xml2::xml_set_text(xml2::xml_add_child(PlantingDisturbanceConfiguration, .value="Enabled"), toString(data["PlantingDisturbanceConfiguration_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(PlantingDisturbanceConfiguration, .value="ParameterFile"), toString(data["PlantingDisturbanceConfiguration_ParameterFile"][[1]]))


        # write FireDisturbanceConfiguration
        FireDisturbanceConfiguration <- xml2::xml_add_child(root, .value="FireDisturbanceConfiguration")
        xml2::xml_set_text(xml2::xml_add_child(FireDisturbanceConfiguration, .value="Enabled"), toString(data["FireDisturbanceConfiguration_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(FireDisturbanceConfiguration, .value="ProbExp"), toString(data["FireDisturbanceConfiguration_ProbExp"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(FireDisturbanceConfiguration, .value="ProbExpBeta"), toString(data["FireDisturbanceConfiguration_ProbExpBeta"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(FireDisturbanceConfiguration, .value="ProbPMax"), toString(data["FireDisturbanceConfiguration_ProbPMax"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(FireDisturbanceConfiguration, .value="ProbPMin"), toString(data["FireDisturbanceConfiguration_ProbPMin"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(FireDisturbanceConfiguration, .value="SizeMax"), toString(data["FireDisturbanceConfiguration_SizeMax"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(FireDisturbanceConfiguration, .value="SpreadFunction"), toString(data["FireDisturbanceConfiguration_SpreadFunction"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(FireDisturbanceConfiguration, .value="DroughtIndex"), toString(data["FireDisturbanceConfiguration_DroughtIndex"][[1]]))




        # write WindDisturbanceConfiguration
        WindDisturbanceConfiguration <- xml2::xml_add_child(root, .value="WindDisturbanceConfiguration")
        xml2::xml_set_text(xml2::xml_add_child(WindDisturbanceConfiguration, .value="Enabled"), toString(data["WindDisturbanceConfiguration_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(WindDisturbanceConfiguration, .value="WindthrowModel"), toString(data["WindDisturbanceConfiguration_WindthrowModel"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(WindDisturbanceConfiguration, .value="BreakoutCoefficient"), toString(data["WindDisturbanceConfiguration_BreakoutCoefficient"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(WindDisturbanceConfiguration, .value="MeanWindthrowSize"), toString(data["WindDisturbanceConfiguration_MeanWindthrowSize"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(WindDisturbanceConfiguration, .value="ProbabilityCoefficient"), toString(data["WindDisturbanceConfiguration_ProbabilityCoefficient"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(WindDisturbanceConfiguration, .value="SizeCoefficient"), toString(data["WindDisturbanceConfiguration_SizeCoefficient"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(WindDisturbanceConfiguration, .value="MeanReturnInterval"), toString(data["WindDisturbanceConfiguration_MeanReturnInterval"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(WindDisturbanceConfiguration, .value="MinWindthrowSize"), toString(data["WindDisturbanceConfiguration_MinWindthrowSize"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(WindDisturbanceConfiguration, .value="MaxWindthrowSize"), toString(data["WindDisturbanceConfiguration_MaxWindthrowSize"][[1]]))



        # write HarvestDisturbanceConfiguration
        HarvestDisturbanceConfiguration <- xml2::xml_add_child(root, .value="HarvestDisturbanceConfiguration")
        xml2::xml_set_text(xml2::xml_add_child(HarvestDisturbanceConfiguration, .value="Enabled"), toString(data["HarvestDisturbanceConfiguration_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(HarvestDisturbanceConfiguration, .value="ParameterFile"), toString(data["HarvestDisturbanceConfiguration_ParameterFile"][[1]]))

        # write BeetleDisturbanceConfiguration
        BeetleDisturbanceConfiguration <- xml2::xml_add_child(root, .value="BeetleDisturbanceConfiguration")
        xml2::xml_set_text(xml2::xml_add_child(BeetleDisturbanceConfiguration, .value="Enabled"), toString(data["BeetleDisturbanceConfiguration_Enabled"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleDisturbanceConfiguration, .value="ParameterFile"), toString(data["BeetleDisturbanceConfiguration_ParameterFile"][[1]]))
        
        # write RuntimeConfiguration
        RuntimeConfiguration <- xml2::xml_add_child(root, .value="RuntimeConfiguration")
        xml2::xml_set_text(xml2::xml_add_child(RuntimeConfiguration, .value="StartYear"), toString(data["RuntimeConfiguration_StartYear"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(RuntimeConfiguration, .value="EndYear"), toString(data["RuntimeConfiguration_EndYear"][[1]]))
        child <- xml2::xml_add_child(RuntimeConfiguration, .value="RandomInit")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="X"), toString(data["RuntimeConfiguration_RandomInit_X"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Y"), toString(data["RuntimeConfiguration_RandomInit_Y"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Z"), toString(data["RuntimeConfiguration_RandomInit_Z"][[1]]))



        # write DispersalConfiguration
        DispersalConfiguration <- xml2::xml_add_child(root, .value="DispersalConfiguration")
        xml2::xml_set_text(xml2::xml_add_child(DispersalConfiguration, .value="DispersalMode"), toString(data["DispersalConfiguration_DispersalMode"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(DispersalConfiguration, .value="BaseSeedProbability"), toString(data["DispersalConfiguration_BaseSeedProbability"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(DispersalConfiguration, .value="EffectiveDm1Probability"), toString(data["DispersalConfiguration_EffectiveDm1Probability"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(DispersalConfiguration, .value="MaxDm1Probability"), toString(data["DispersalConfiguration_MaxDm1Probability"][[1]]))

        # write DebugConfiguration
        DebugConfiguration <- xml2::xml_add_child(root, .value="DebugConfiguration")    
        xml2::xml_set_text(xml2::xml_add_child(DebugConfiguration, .value="WaitForExit"), toString(data["DebugConfiguration_WaitForExit"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(DebugConfiguration, .value="DebugOutput"), toString(data["DebugConfiguration_DebugOutput"][[1]]))
    


    } else if (tolower(type) == 'barkbeetle') {
        if (length (data) == 0) { 
            data <- default_barkbeetle
        } else {
            data <- cbind(data, default_barkbeetle[!names(default_barkbeetle) %in% names(data)])
        }
        data_columns = names(data)

        root <- xml2::xml_new_root(.value="BeetleConfiguration")
        BeetleParameters <- xml2::xml_add_child(root, .value="BeetleParameters")
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="BeetleHostName"), toString(data["BeetleHostName"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="BetaScale"), toString(data["BetaScale"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="IniBdi"), toString(data["IniBdi"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="BgBpi"), toString(data["BgBpi"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="SiWMaxCoeff"), toString(data["SiWMaxCoeff"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="BdiMaxCoeff"), toString(data["BdiMaxCoeff"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="CellCoI"), toString(data["CellCoI"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="SiCellNi"), toString(data["SiCellNi"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="SiWni"), toString(data["SiWni"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="WISiCell"), toString(data["WISiCell"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="DRSiCell"), toString(data["DRSiCell"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="SSSiCell"), toString(data["SSSiCell"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="ADSiCell"), toString(data["ADSiCell"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="BpCoeff"), toString(data["BpCoeff"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="MaxInfBm"), toString(data["MaxInfBm"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="WIDist"), toString(data["WIDist"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="DTBark"), toString(data["DTBark"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="LowerAsymDrS"), toString(data["LowerAsymDrS"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="ParamCDrS"), toString(data["ParamCDrS"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="DrStrRf"), toString(data["DrStrRf"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(BeetleParameters, .value="DrSFunction"), toString(data["DrSFunction"][[1]]))
        child <- xml2::xml_add_child(BeetleParameters, .value="SpruceShareParameters")
        xml2::xml_set_text(xml2::xml_add_child(child, .value="SsA"), toString(data["SpruceShareParameters_SsA"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="SsK"), toString(data["SpruceShareParameters_SsK"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="SsQ"), toString(data["SpruceShareParameters_SsQ"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Ssr"), toString(data["SpruceShareParameters_Ssr"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Ssm"), toString(data["SpruceShareParameters_Ssm"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(child, .value="Ssv"), toString(data["SpruceShareParameters_Ssv"][[1]]))







    } else if (tolower(type) == 'landtypeparameters') {
        if (length (data) == 0) { 
            data <- default_landtypeparameters
        }
        
        root <- xml2::xml_new_root(.value="LandTypeParametersFile")
        
        for(r in 1:nrow(data)) {
            row <- data[r,]
            LandTypeParameter <- xml2::xml_add_child(root, .value="LandTypeParameter")
            xml2::xml_set_text(xml2::xml_add_child(LandTypeParameter, .value="Id"), toString(row["Id"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(LandTypeParameter, .value="Name"), toString(row["Name"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(LandTypeParameter, .value="MinShadeYears"), toString(row["MinShadeYears"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(LandTypeParameter, .value="MeanFireReturnInterval"), toString(row["MeanFireReturnInterval"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(LandTypeParameter, .value="FireIgnitionCoefficient"), toString(row["FireIgnitionCoefficient"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(LandTypeParameter, .value="FireProbabilityCoefficient"), toString(row["FireProbabilityCoefficient"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(LandTypeParameter, .value="LastWindthrowDisturbance"), toString(row["LastWindthrowDisturbance"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(LandTypeParameter, .value="LastFireDisturbance"), toString(row["LastFireDisturbance"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(LandTypeParameter, .value="BrowsingIntensity"), toString(row["BrowsingIntensity"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(LandTypeParameter, .value="PotentialSaplingDensity"), toString(row["PotentialSaplingDensity"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(LandTypeParameter, .value="DefaultEstablishmentCoefficient"), toString(row["DefaultEstablishmentCoefficient"][[1]]))
            if ("SpeciesEstablishmentCoefficient_Name" %in% names(data) && "SpeciesEstablishmentCoefficient_EstablishmentCoefficient" %in% names(data) && "SpeciesEstablishmentCoefficient_VegetativePropagules" %in% names(data)) {
                child <- xml2::xml_add_child(LandTypeParameter, .value="SpeciesEstablishmentCoefficients")
                name_list <- strsplit(toString(row["SpeciesEstablishmentCoefficient_Name"][[1]]), "#")[[1]]
                ec_list <- strsplit(toString(row["SpeciesEstablishmentCoefficient_EstablishmentCoefficient"][[1]]), "#")[[1]]
                vp_list <- strsplit(toString(row["SpeciesEstablishmentCoefficient_VegetativePropagules"][[1]]), "#")[[1]]

                if (length(name_list) != length(ec_list) || length(name_list) != length(vp_list) || length(ec_list) != length(vp_list)) {
                    print("Contradicting data: SpeciesEstablishmentCoefficient_Name, SpeciesEstablishmentCoefficient_EstablishmentCoefficient & SpeciesEstablishmentCoefficient_VegetativePropagules")
                    return(-1)
                }

                for (i in 1:length(name_list)) {
                    grandchild <- xml2::xml_add_child(child, .value="SpeciesEstablishmentCoefficient")
                    xml2::xml_set_text(xml2::xml_add_child(grandchild, .value="Name"), toString(name_list[i]))
                    xml2::xml_set_text(xml2::xml_add_child(grandchild, .value="EstablishmentCoefficient"), toString(ec_list[i]))
                    xml2::xml_set_text(xml2::xml_add_child(grandchild, .value="VegetativePropagules"), toString(vp_list[i]))
                }

            }
            xml2::xml_set_text(xml2::xml_add_child(LandTypeParameter, .value="DefaultVegetativePropagules"), toString(row["DefaultVegetativePropagules"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(LandTypeParameter, .value="MaxBiomassDensity"), toString(row["MaxBiomassDensity"][[1]]))



        }

    } else if (tolower(type) == 'plantingparameters') {
        if (length (data) == 0) { 
            data <- default_plantingparameters
        }
        
        root <- xml2::xml_new_root(.value="PlantingConfiguration")
        
        for(r in 1:nrow(data)) {
            row <- data[r,]
            PlantingParameters <- xml2::xml_add_child(root, .value="PlantingParameters")
            xml2::xml_set_text(xml2::xml_add_child(PlantingParameters, .value="ManagementAreaId"), toString(row["ManagementAreaId"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(PlantingParameters, .value="Begin"), toString(row["Begin"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(PlantingParameters, .value="End"), toString(row["End"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(PlantingParameters, .value="Interval"), toString(row["Interval"][[1]]))
            if ("Years" %in% names(row)) {
                if (!is.na(row["Years"][[1]]) && row["Years"][[1]] != "") {
                    xml2::xml_set_text(xml2::xml_add_child(PlantingParameters, .value="Years"), toString(gsub("#", " ", row["Years"][[1]])))
                }
            }
            xml2::xml_set_text(xml2::xml_add_child(PlantingParameters, .value="Target"), toString(row["Target"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(PlantingParameters, .value="Rank"), toString(row["Rank"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(PlantingParameters, .value="HarvestedAreaOnly"), toString(row["HarvestedAreaOnly"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(PlantingParameters, .value="MinBiomass"), toString(row["MinBiomass"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(PlantingParameters, .value="MinDensity"), toString(row["MinDensity"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(PlantingParameters, .value="Browse"), toString(row["Browse"][[1]]))
            xml2::xml_set_text(xml2::xml_add_child(PlantingParameters, .value="LastPlanting"), toString(row["LastPlanting"][[1]]))
            if ("PlantingMask_SpeciesPlanting_Name" %in% names(row) && "PlantingMask_SpeciesPlanting_BiomassPlant" %in% names(row) && "PlantingMask_SpeciesPlanting_DensityPlant" %in% names(row)) {
                if (!is.na(row["PlantingMask_SpeciesPlanting_Name"][[1]]) && row["PlantingMask_SpeciesPlanting_Name"][[1]] != "" && !is.na(row["PlantingMask_SpeciesPlanting_BiomassPlant"][[1]]) && row["PlantingMask_SpeciesPlanting_BiomassPlant"][[1]] != "" && !is.na(row["PlantingMask_SpeciesPlanting_DensityPlant"][[1]]) && row["PlantingMask_SpeciesPlanting_DensityPlant"][[1]] != "") {
                    # PlantingMask_SpeciesPlanting_Name
                    # PlantingMask_SpeciesPlanting_BiomassPlant
                    # PlantingMask_SpeciesPlanting_DensityPlant
                    child <- xml2::xml_add_child(PlantingParameters, .value="PlantingMask")
                    name_list <- strsplit(toString(row["PlantingMask_SpeciesPlanting_Name"][[1]]), "#")[[1]]
                    bp_list <- strsplit(toString(row["PlantingMask_SpeciesPlanting_BiomassPlant"][[1]]), "#")[[1]]
                    dp_list <-  strsplit(toString(row["PlantingMask_SpeciesPlanting_DensityPlant"][[1]]), "#")[[1]]

                    if(length(name_list) != length(bp_list) || length(name_list) != length(dp_list) || length(bp_list) != length(dp_list)) {
                        print("Contradicting data: PlantingMask_SpeciesPlanting_Name, PlantingMask_SpeciesPlanting_BiomassPlant & PlantingMask_SpeciesPlanting_DensityPlant")
                        return(-1)
                    }

                    for (i in 1:length(name_list)) {
                        grandchild <- xml2::xml_add_child(child, .value="SpeciesPlanting")
                        xml2::xml_set_text(xml2::xml_add_child(grandchild, .value="Name"), toString(name_list[i]))
                        xml2::xml_set_text(xml2::xml_add_child(grandchild, .value="BiomassPlant"), toString(bp_list[i]))
                        xml2::xml_set_text(xml2::xml_add_child(grandchild, .value="DensityPlant"), toString(dp_list[i]))
                    }

                }

            }


        }

    } else if (tolower(type) == 'randomstate') {
        if (length (data) == 0) { 
            data <- default_randomstate
        } else {
            data <- cbind(data, default_randomstate[!names(default_randomstate) %in% names(data)])
        }
        
        root <- xml2::xml_new_root(.value="RandomState")
        xml2::xml_set_text(xml2::xml_add_child(root, .value="X"), toString(data["X"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(root, .value="Y"), toString(data["Y"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(root, .value="Z"), toString(data["Z"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(root, .value="XSeed"), toString(data["XSeed"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(root, .value="YSeed"), toString(data["YSeed"][[1]]))
        xml2::xml_set_text(xml2::xml_add_child(root, .value="ZSeed"), toString(data["ZSeed"][[1]]))



    } else if (tolower(type) == 'species') {
        if (length (data) == 0) { 
            data <- default_species
        }
        root <- xml2::xml_new_root(.value="SpeciesFile")


        for(r in 1:nrow(data)) {
            row <- data[r,]
            child <- xml2::xml_add_child(root, .value="SpeciesParameters") 
            for (colname in names(data)) 
                if (!is.na(row[colname][[1]]) && row[colname][[1]] != "")
                    xml2::xml_set_text(xml2::xml_add_child(child, .value=colname), toString(row[colname][[1]]))
            
        }
        
    } else {
        print(paste0("Error in XML generator. Unknown type: ", type))
        return(-1)
    }
      xml2::write_xml(root, filePathXML)
}
