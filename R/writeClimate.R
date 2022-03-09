# Write the climate file in the format required for input file in LandClim
#
#
#' Write the climate file in the format required for input file in LandClim
#'
#' @param filename Name that you want for your climate file.
#' @param latitude Wheather station latitude.
#' @param elevation weather station elevation.
#' @param climate table where the column variables are year, mean temperature Jan to Temperature Dec (12) and precipitation sumJan to December (12)
#' @param Tlapse is the value at which  temperature falls with altitude.
#' @param Plapse is the value at which  precipitation falls with altitude.
#' @param delta : altitude rate corrections
#' @return Creates the climate.dat file required to run LandClim
#' @examples
#' write.climate(filename,
#'               latitude,
#'               elevation,
#'               climate,
#'               Tlapse = rep(0, 12),
#'               Plapse = rep(0,12),
#'               delta = 100)



write.climate <- function(filename, latitude, elevation, climate,
                          Tlapse = rep(0, 12), Plapse = rep(0,12), delta = 100){

  con <- file(filename, open="w")
  writeLines("#header info#", con)
  writeLines(paste(latitude, "#latitude; used for calculating drought index in model bugmann#", sep="\t"), con)
  writeLines(paste(elevation, "#meter a.s.l. of climate station#", sep="\t"), con)
  writeLines("", con)
  writeLines("#temperature lapse rate with increasing altitude: #", con)
  writeLines(paste(round(Tlapse, 2), collapse="\t"), con)
  writeLines("", con)
  writeLines("#precipitation lapse rate with increasing altitude: #", con)
  writeLines(paste(round(Plapse, 2), collapse="\t"), con)
  writeLines("", con)
  writeLines(paste(delta, "#meter: corrections (see above) every x meter. #", sep="\t"), con)
  writeLines("", con)
  writeLines("#	mean monthly temperature (?C) total monthly precipitation (mm) #", con)
  writeLines("#year\tjan.T\tfeb.T\tmar.T\tapr.T\tmay.T\tjun.T\tjul.T\taug.T\tsep.T\toct.T\tnov.T\tdec.T\tjan.P\tfeb.P\tmar.P\tapr.P\tmay.P\tjun.P\tjul.P\taug.P\tsep.P\toct.P\tnov.P\tdec.P#", con)
  write(t(climate), file=con, ncolumns=25, sep="\t")
  close(con)

}



