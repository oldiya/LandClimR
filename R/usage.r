source('core.r')
library(doParallel)

# The example below shows how 4 landclim runs can be executed
# with 2 cores availability. It runs Landclim in parallel (at most 2 
# processes at a time) until all landclim configurations are executed
# num_cores can be changed as needed (pls don't exceed core availabilty)


num_cores <- 2
cl <- makeCluster(num_cores)
registerDoParallel(cl)

config <- list(data.frame(OutputConfiguration_OutputDirectory = "1"), 
                data.frame(OutputConfiguration_OutputDirectory = "2"),
                data.frame(OutputConfiguration_OutputDirectory = "3"),
                data.frame(OutputConfiguration_OutputDirectory = "4"))
configXMLpath <- list("workspace/1.xml", "workspace/2.xml",
                        "workspace/3.xml", "workspace/4.xml")
consoleOutputPath <- list("workspace/consoleOutput1.txt",
                            "workspace/consoleOutput2.txt",
                            "workspace/consoleOutput3.txt",
                            "workspace/consoleOutput4.txt")

foreach(i=1:length(config)) %dopar% runLandClim(config=config[[i]], 
                                    configXMLpath=configXMLpath[[i]], 
                                    consoleOutputPath=consoleOutputPath[[i]])