source('core.r')
library(doMPI)

# Note: this example is only valid for MPI job submissions via Euler
# Please do not run this script on your personal computer!


cl <- startMPIcluster(verbose=TRUE, logdir="log")
registerDoMPI(cl)


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

closeCluster(cl)
mpi.quit()