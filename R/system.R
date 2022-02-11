################################################################################
# Functions needed to run a LandClim simulation
# Author: Olalla Díaz Yáñez (olalladiaz.net)
# Aim:
################################################################################


# Find the system you are working in

  get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)) {
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


