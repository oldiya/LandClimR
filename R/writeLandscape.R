#' Create theoretical landscapes to be used in LandClim
#'
#' @param ySize y landscape length in m.
#' @param xSize x landscape length in m.
#' @param cellSize cell size in m.
#' @param altitude Altitude in m, if it is a single value the whole map will have the same value, if you want to provide different values per cell you have to provide a vector of length equal to the number of cells and consider that the values will be assigned from top to bottom and from left to right.
#' @param slop Slope in degrees, if it is a single value the whole map will have the same value, if you want to provide different values per cell you have to provide a vector of length equal to the number of cells and consider that the values will be assigned from top to bottom and from left to right.
#' @param so bucket size of the soil, if it is a single value the whole map will have the same value, if you want to provide different values per cell you have to provide a vector of length equal to the number of cells and consider that the values will be assigned from top to bottom and from left to right.
#' @param asp aspect in degrees, if it is a single value the whole map will have the same value, if you want to provide different values per cell you have to provide a vector of length equal to the number of cells and consider that the values will be assigned from top to bottom and from left to right.
#' @param coor CRS, coordinate system in EPSG.
#' @param landt Land type,if it is a single value the whole map will have the same value, if you want to provide different values per cell you have to provide a vector of length equal to the number of cells and consider that the values will be assigned from top to bottom and from left to right.
#' @param plot If TRUE the created landscapes will be plotted as an extra outcome.
#' @param fullpath The full path where the landscapes will be saved.
#' @return Save the created maps into the \code{fullpath} provided.
#' @examples
#' theorLandscape (ySize = 1500, xSize = 1500, cellSize = 30, coor = 5514,
#'                 altitude = 1800, slop = 0, so = 20, landt = 11, asp = 0,
#'                 plot = TRUE, fullpath = "/Users/olalladiaz/MyRepos/beetleunc/Data/theoretical/squared"=



writeLandscape <- function(ySize, # y size of the landscape in m
                           xSize, # x size of the landscape in m
                           cellSize, # in m
                           coor, # assign crs coordinate system #EPSG
                           altitude, # the dem values, EXPLANATION: if it is a singled value all the map will have the same altitude, if you want to provide diferent values per cell you have to provide a vector of length equal to the number of cells and consider that the values will be assigned from top to bottom and from left to right
                           slop, #  slope value, EXPLANATION same as is altitude
                           so, # bucket size of the soil , EXPLANATION same as is altitude
                           landt, #land type , EXPLANATION same as is altitude
                           asp, # aspect , EXPLANATION same as is altitude
                           plot = FALSE, # if you want to plot the maps created
                           fullpath# full path to the desired location
                           ){

  nr <- ceiling(ySize / cellSize) # number of rows
  nc <- ceiling(xSize / cellSize) # number of columns

  # Create raster with the desired size
  x <- raster::raster()
  extension <- raster::extent(0, nc * cellSize, 0, nr * cellSize)
  dem <- raster::raster(nrows = nr, ncols = nc, extension)

  # Assign values to the raster
  dem[] <- altitude

  # Assign a coordinate reference system (CRS)
  raster::crs(dem) <- coor #EPSG

  # Create LandClim map "slope".
  slope <- dem
  slope[] <- slop

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

  if (plot == TRUE) {
    # Plot maps
    raster::plot(dem, col = Palettes::terrain.colors(100), main = "DEM")
    raster::plot(slope, col = Palettes::terrain.colors(100), main = "Slope")
    raster::plot(aspect, col = Palettes::terrain.colors(100), main = "Aspect")
    raster::plot(mask, col = Palettes::terrain.colors(100), main = "Mask")
    raster::plot(soil , col = Palettes::terrain.colors(100), main = "Soil")
    raster::plot(landtype, col = Palettes::terrain.colors(100), main = "Landtype")
  }

  # Create raster-stack
  MapStack <- raster::stack(dem, slope, aspect, mask, soil, landtype)
  names(MapStack) <- c("DEM", "Slope", "Aspect", "Mask", "Soil", "Landtype")

  # Save the maps in the desired location
  writeMaps(landClimRasterStack = MapStack, nodata_value = "-9999",
            lcResolution = cellSize,
            folder = fullpath)

  return(print(paste0("The theoretical landscapes created have been succesfully saved in the path: ", fullpath)))
}



















