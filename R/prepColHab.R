#' Prepare habitat around a colony
#'
#' @param col_cc A numeric vector with the coordinates of the colony to simulate around (lon, lat)
#' @param max_range The maximum distance from the colony the vultures will be allowed to travel in kilometers.
#'     Note that increasing the range will increase computation time and memory use
#' @param col_all A data frame containing at least two columns (lon, lat) with the coordinates of all colonies
#' @param sfs A data frame containing at least two columns (lon, lat) with the coordinates of all supplementary feeding sites
#'
#' @return A data frame with variables needed for activity simulation such as: distance to colony, distance to other colonies
#'     or distance to supplementary feeding sites.
#' @export
#'
#' @examples
#' prepColHab(c(27.7, -31.2), 100)
prepColHab <- function(col_cc, max_range, col_all = NULL, sfs = NULL){

  if(is.null(col_all) || is.null(sfs)){
    stop("Need colony and supplementary feeding sites data. Contact package maintainer.")
  }

  # Change range from kilometers to degrees
  max_range <- max_range/111

  # Prepare whole range data ------------------------------------------------

  # Load habitat data
  hab <- vultRmap::range_covts

  # Fix variable names for later
  sfs$lon <- sfs$longitude
  sfs$lat <- sfs$latitude

  # Calculate approximate distances to colony
  hab <- hab %>%
    dplyr::mutate(distp = vultRmap::calcDist(c(col_cc[1], col_cc[2]), lon, lat))

  # Keep only cells within max range
  hab <- hab %>%
    dplyr::filter(distp < max_range) %>%
    dplyr::select(-distp)

  if(nrow(hab) == 0){
    stop("No cells within max range")
  }

  # Take colony to the centre of the grid
  dists <- vultRmap::calcDist(col_cc, hab$lon, hab$lat)
  col_cc <- c(hab$lon[which.min(dists)], hab$lat[which.min(dists)])


  # Calculate distances for this colony -------------------------------------

  hab <- hab %>%
    dplyr::mutate(dist_col = vultRmap::calcHabMinDist(., col_cc, data.frame(lon = col_cc[1], lat = col_cc[2])),
                  dist_col_any = vultRmap::calcHabMinDist(., col_cc, col_all, buffer = 5000),
                  dist_sfs = vultRmap::calcHabMinDist(., col_cc, sfs))


  # Prepare other variables -------------------------------------------------

  hab <- hab %>%
    as.data.frame() %>%
    dplyr::mutate(cell_id = 1:nrow(.),
                  dist_col_sc = dist_col, # Store distance in original scale
                  dist_col = dist_col / attr(hab, "mod_scale")["dist_col"],
                  dist_col_any = dist_col_any / attr(hab, "mod_scale")["dist_col_any"],
                  dist_col = ifelse(dist_col < 0.015, 0.015, dist_col), # This is the minimum distance greater than zero. Otherwise log is not finite
                  log_dist_col = log(dist_col_sc),
                  dist_sfs = dist_sfs / attr(hab, "mod_scale")["dist_sfs"])


  # Add projected coordinates -----------------------------------------------

  # Define projection
  tmerproj <- paste0("+proj=tmerc +lat_0=", col_cc[2], " +lon_0=", col_cc[1],
                     " +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # Create spatial objects and transform
  hab_sp <- sp::SpatialPointsDataFrame(coords = hab[,c("lon", "lat")], data = hab,
                                    proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  hab_sp <- sp::spTransform(hab_sp, tmerproj)

  hab_cc <- sp::coordinates(hab_sp)

  hab <- hab %>%
    dplyr::mutate(x = hab_cc[,1],
                  y = hab_cc[,2])

  return(hab)

}
