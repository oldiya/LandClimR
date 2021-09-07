################################################################################
# Functions needed to run a LandClim simulation
# Author: Olalla Díaz Yáñez (olalladiaz.net)
# Aim:
################################################################################


# Write the climate file in the format required for input file in LandClim

#filename,
#latitude: Wheather station latitude
#elevation: weather station elevation
#climate: table where the column variables are year, mean temperature Jan to Temperature Dec [12] and precipitation sumJan to December [12]
#Tlapse: is the value at which  temperature falls with altitude
#Plapse: is the value at which  precipitation falls with altitude
#delta : altitude rate corrections

write.climate <- function (filename, latitude, elevation, climate,
                           Tlapse= rep(0, 12), Plapse = rep(0,12), delta = 100){

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

