################################################################################
# Functions needed to run a LandClim simulation
# Author: Olalla Díaz Yáñez (olalladiaz.net)
# Modified from LandclimTools package function write_landclim_maps
# Aim:
################################################################################


# Write maps required as input files

writeMaps <- function(landClimRasterStack,
                      nodata_value = "-9999",
                      lcResolution = 25,
                      folder = getwd()) {
  ex <- (raster::extent(landClimRasterStack))
  landClimRasterStack_list <- lapply(raster::unstack(landClimRasterStack),
                                     function(x) raster::crop(x, ex))
  rs <- raster::stack(landClimRasterStack_list)
  names(rs) <- names(landClimRasterStack)
  #raster::writeRaster(rs, filename=paste0(folder, "/landclim_maps.tif"), overwrite = TRUE)
  rm(rs)
  foo <- function(x){
    sink(paste(folder, "/", names(x), ".asc", sep = ""))
    writeLines(c(paste("ncols", ncol(x)),
                 paste("nrows", nrow(x)),
                 paste("xllcorner", raster::xmin(x)),
                 paste("yllcorner", raster::ymin(x)),
                 paste("cellsize", lcResolution),
                 paste("NODATA_value", nodata_value)))
    sink()
    write.table(matrix(round(x[]), nrow = nrow(x), ncol = ncol(x), byrow = TRUE),
                file = paste(folder, "/", names(x), ".asc", sep = ""), append = TRUE,
                quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
  lapply(landClimRasterStack_list, function(x) foo(x))
}

