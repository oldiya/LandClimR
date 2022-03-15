#' runs LandClim with the desired set up
#'
#' @param configXMLpath path where the the configuration xml file is located.
#' @param consoleOutputPath path to the folder and name (e.g. nameofthefile.txt) of the file where you want to storage the console outputs shown during the simulation. If not provided the console outcomes will be seen in the console and there will not be storage.
#' @param binPath path to the folder where LandClim model binary files needed for your operative system are storaged.
#' @return Run LandClim with the desired input files and saved in the \code{workspacePath} provided.
#' @examples
#' runLandClim(configXMLpath,
            #' consoleOutputPath = "",
            #' binPath = "bin")


runLandClim <- function(configXMLpath,
                         consoleOutputPath = "",
                         binPath = "bin",
                         batch = FALSE, runtime = 4, memory = 1024){

  # Save the console outputs from the simulation?
  if (!is.null(consoleOutputPath)) {
    if (consoleOutputPath == "") {
      tempStr <- basename(tempdir())
      consoleOutputPath <- paste0(configXMLpath, "_consoleOutput_",
                                  tempStr, ".txt")

    }
  }

  print(consoleOutputPath)

  if (batch == TRUE) {
    cmd <- paste0(binPath, '/LandClim ', getwd(), "/", configXMLpath)
    system2(cat(paste0('bsub -n 1 -R "rusage[mem=', memory, ']" -W ', runtime, ':00 "', cmd,'"')))

  } else if (batch == FALSE) {
    # Run the model with the desired set-up
    start_time <- Sys.time()
    if (tolower(.Platform$OS.type) == "unix") {
      system2(paste0(binPath, '/LandClim'),
              args = paste0(getwd(), "/", configXMLpath), stdout = consoleOutputPath)

    } else {
      system2(paste0(binPath, '/LandClim.exe'),
              args = paste0(getwd(), "/", configXMLpath), stdout = consoleOutputPath)
    }
  }







}
