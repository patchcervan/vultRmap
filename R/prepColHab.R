#' Prepare habitat around a colony
#'
#' @param col_cc A numeric vector with the coordinates of the colony to simulate
#' around (lon, lat).
#' @param max_range The maximum distance from the colony the vultures will be
#' allowed to travel in kilometers. Note that increasing the range will increase
#' computation time and memory use.
#' @param col_all A data frame containing at least two columns (lon, lat) with
#' the coordinates of all colonies.
#' @param sfs A data frame containing at least two columns (lon, lat) with the
#' coordinates of all supplementary feeding sites.
#' @param scale A character string with one of three options: "ssf" to scale
#' variables for the SSF model, "hgt" to scale for the height model or "none" for
#' no scaling.
#'
#' @return A data frame with variables needed for activity simulation such as:
#' distance to colony, distance to other colonies or distance to supplementary
#' feeding sites.
#' @export
#'
#' @examples
#' # This will fail because you need aux data:
#' # prepColHab(c(27.7, -31.2), 100)
prepColHab <- function(col_cc, max_range, col_all = NULL, sfs = NULL,
                       scale = "ssf"){

  if(is.null(col_all) || is.null(sfs)){
    stop("Need colony and supplementary feeding sites data. Contact package maintainer.")
  }

  # Change range from kilometers to degrees for a pre-subset (more later)
  max_range_p <- max_range/111 + 2


  # Prepare whole range data ------------------------------------------------

  # Load habitat data
  hab <- vultRmap::range_covts

  # Prepare scaling factors
  if(!scale %in% c("ssf", "hgt", "none")){
    stop("scale must be either ssf, hgt or none")
  }
  sds <- NULL
  if(scale == "ssf"){
    sds <- attr(hab, "ssf_mod_scale")
  } else if(scale == "hgt"){
    sds <- attr(hab, "ssf_mod_scale")
  }


  # Fix variable names for later
  sfs$lon <- sfs$longitude
  sfs$lat <- sfs$latitude

  # Calculate approximate distances to colony
  hab <- hab %>%
    dplyr::mutate(distp = vultRmap::calcDist(c(col_cc[1], col_cc[2]), lon, lat))

  # Keep only cells within max range
  hab <- hab %>%
    dplyr::filter(distp < max_range_p) %>%
    dplyr::select(-distp)

  if(nrow(hab) == 0){
    stop("No cells within max range")
  }

  # Take colony to the centre of the grid
  dists <- vultRmap::calcDist(col_cc, hab$lon, hab$lat)
  col_cc <- c(hab$lon[which.min(dists)], hab$lat[which.min(dists)])


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


  # Calculate distances for this colony -------------------------------------

  # Calculate distance to colony
  hab <- hab %>%
    dplyr::mutate(dist_col = vultRmap::calcDist(c(0,0), x, y))

  # Subset distance in project coordinates kilometers
  max_range <- max_range*1000

  hab <- hab %>%
    dplyr::filter(dist_col < max_range)

  hab_cc <- as.matrix(hab[, c("x", "y")])

  # Calculate distance to other colonies and supplementary feeding sites
  hab <- hab %>%
    dplyr::mutate(dist_col_any = vultRmap::calcHabMinDist(hab_cc, tmerproj, col_all, buffer = 10000),
                  dist_sfs = vultRmap::calcHabMinDist(hab_cc, tmerproj, sfs))


  # Prepare other variables -------------------------------------------------

  # We define min distance to colony as being 5km which is the buffer we defined
  # around colonies for removing "other" colonies.
  mindist <- 5000

  if(scale != "none"){
    hab <- hab %>%
      as.data.frame() %>%
      dplyr::arrange(lon, lat) %>%
      dplyr::mutate(cell_id = dplyr::row_number(),
                    elev = elev / sds["elev"],
                    slope = slope / sds["slope"],
                    rugg = rugg / sds["rugg"],
                    dist_col_any = dist_col_any / sds["dist_col_any"],
                    dist_col = ifelse(dist_col < mindist, mindist, dist_col), # This is the minimum distance greater than zero. Otherwise log is not finite
                    log_dist_col = log(dist_col),
                    dist_col = dist_col / sds["dist_col"],
                    dist_sfs = dist_sfs / sds["dist_sfs"])
  } else {
    hab <- hab %>%
      as.data.frame() %>%
      dplyr::arrange(lon, lat) %>%
      dplyr::mutate(cell_id = dplyr::row_number(),
                    dist_col = ifelse(dist_col < mindist, mindist, dist_col), # This is the minimum distance greater than zero. Otherwise log is not finite
                    log_dist_col = log(dist_col))
  }

  # Restore attributes
  attr(hab, "mod_scale") <- sds

  return(hab)

}
