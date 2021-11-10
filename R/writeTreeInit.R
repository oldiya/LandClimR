

# Tree initial tree spices for strip ####
# NOTE: We create a variety of tree sizes in the same cell for represent better a variety of young trees in the inital conditions
writeTreeInit <- function (row, col, species, age, stems, #in a cell of 625m
                           biomass, slowGrowth, fullpath){
  
  treeIniT <- data.frame (row = row,
                          col = col,
                          species = species,
                          age = age,
                          stems = stems, #in a cell of 625m
                          biomass = biomass,
                          slowGrowth = slowGrowth)
  
  write.csv (treeIniT, file = paste0 (fullpath, "treeini", species,
                                      ".csv"),
             row.names = FALSE)
  
}