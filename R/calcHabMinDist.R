#' Calculate minimum distances to habitat cells
#'
#' @param hab_cc A matrix with two columns containing the coordinates (lon, lat) of a set of locations. Potentially a lattice of habitat cells.
#' @param tmerproj The Mercator projected coordinate system used for distance calculations.
#' @param features A dataframe with at least two columns containing the coordinates (lon, lat) of the locations we want to calculate minimum distances to.
#' @param buffer A number indicating a distance from origin. If set, features within this distance to origin will be ignored.
#'
#' @return A vector with the minimum distance from each of the hab locations to any of the feature locations.
#' @export
#'
#' @examples
#' hab <- as.matrix(expand.grid(-10:10, -10:10))
#' colnames(hab) <- c("lon", "lat")
#' origin <- c(2, 1)
#' tmerproj <- paste0("+proj=tmerc +lat_0=", origin[2], " +lon_0=", origin[1],
#' " +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#' features <- data.frame(lon = c(-5, 5),
#'                        lat = c(-5, 5))
#' calcHabMinDist(hab, tmerproj, features)
calcHabMinDist <- function(hab_cc, tmerproj, features, buffer = NULL){

  features <- sp::SpatialPointsDataFrame(coords = features[,c("lon", "lat")], data = features,
                                         proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  features <- sp::spTransform(features, tmerproj)

  features_cc <- sp::coordinates(features)

  # Then, we need to remove any features that is less than "buffer" units away from the origin
  if(!is.null(buffer)){
    dist_all <- vultRmap::calcDist(c(0,0), features_cc[,1], features_cc[,2])
    features_cc <- features_cc[dist_all > buffer,]
  }


  # Calculate minimum distance between hab and features
  out <- vultRmap::calcMinDist_cpp(hab_cc, features_cc)

  return(out)

}
