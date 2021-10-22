################################################################################
# Functions needed to run a LandClim simulation
# Author: Olalla Díaz Yáñez (olalladiaz.net)
# Aim: Save theoretical maps with the user defined input values
################################################################################


# Write the required map input files with a simplified theoretical landscape

#Example of input data
#ySize <<-  1500
#xSize <- 1500
#cellSize <- 30
#coor <- 5514
#altitude <- 1800
#slop <- 0
#so <- 20
#landt <- 11
#asp <- 0
#plot <- TRUE
#fullpath <- "/Users/olalladiaz/MyRepos/beetleunc/Data/theoretical/squared"

theorLandscape <- function (ySize, # y size of the lanscape in m
                            xSize, # x size of the lanscape in m
                            cellSize, # in m
                            coor, # assign crs coordinate system #EPSG
                            altitude, # assig the dem values, EXPLANATION: if it is a singled value all the map will have the same altitude, if you want to provide diferent values per cell you have to provide a vector of length equal to the number of cells and consider that the values will be assigned from top to bottom and from left to right
                            slop, # assing slope value, EXPLANATION same as is altitude
                            so, # bucket size of the soil , EXPLANATION same as is altitude
                            landt, #land type , EXPLANATION same as is altitude
                            asp, # aspect , EXPLANATION same as is altitude
                            plot, # if you want to plot the maps created
                            fullpath# fullpath to the desired location
                            ){
  nr <- ceiling (ySize / cellSize) # number of rows
  nc <- ceiling (xSize / cellSize) # number of columns

  # Create raster with the desired size
  x <- raster::raster()
  extension <- raster::extent (0, nc * cellSize, 0, nr * cellSize)
  dem <- raster::raster (nrows = nr, ncols = nc, extension)

  # Assign values to the raster
  dem[] <- altitud

  # Assign a coordinate reference system (CRS)
  raster::crs(dem) <- coor #EPSG

  # Create LandClim map "slope".
  slope <- dem
  slope[]<- slop

  #  LandClim map "soil".
  soil <- dem
  soil[] <- so

  #  LandClim map "landtype".
  landtype <- slope
  landtype[] <- landt

  # Aspect
  aspect <- slope
  aspect[] <- asp

  # Mask
  mask <- dem # use same setting as for dem file
  mask[] <- 1 # set all cells as 'active'
  mask[dem[] == -9999] <- 0 # set boundary cells as 'inactive'

  if (plot == TRUE){
    # Plot maps
    raster::plot(dem, col = terrain.colors(100), main = "DEM")
    raster::plot(slope, col = terrain.colors(100), main = "Slope")
    raster::plot(aspect, col = terrain.colors(100), main = "Aspect")
    raster::plot(mask, col = terrain.colors(100), main = "Mask")
    raster::plot(soil , col = terrain.colors(100), main = "Soil")
    raster::plot(landtype, col = terrain.colors(100), main = "Landtype")
  }

  # Create raster-stack
  MapStack <- raster::stack (dem, slope, aspect, mask, soil, landtype)
  names(MapStack) <- c("DEM", "Slope", "Aspect", "Mask", "Soil", "Landtype")

  # Save the maps in the desired location
  writeMaps (landClimRasterStack = MapStack, nodata_value = "-9999",
             lcResolution = cellSize,
             folder = fullpath)

  return (print (paste0("The theoretical landscapes created have been succesfully saved in the path: ", fullpath)))
}



















