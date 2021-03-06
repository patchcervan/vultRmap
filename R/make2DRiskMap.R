#' Make 2D Risk Map
#'
#' @description Takes all files with smoothed counts present in a directory
#' and creates a risk map.
#' @param col_to_pred A dataframe with the coordinates and sizes of all colonies
#' we want to include in the map.
#' @param age A character string indicating the age of the vultures we want to
#' calculate the risk map for: either "ad" or "juv.
#' @param map_type A character string indicating the type of map that should be
#' calculated: either "risk" in which the utilization distribution is multiplied
#' by the size of the colony or "hazard" in which the UD is not scaled by the
#' size of the colony.
#' @param countsdir A character string indicating the path to the directory
#' where the smoothed counts are found.
#' @param .suffix A character string containing the part the name of the file
#' that appears after the colony code (e.g. "_ad_gam.rds)
#' @param outdir A character string. If not NULL (default) the output map is
#' saved to disk in the desired location. The map type and the age will be
#' recorded in the name of the output file.
#'
#' @return
#' @export
#'
#' @examples
make2DRiskMap <- function(col_to_pred, age, map_type = "risk", countsdir,
                          suffix, outdir = NULL){

  if(!dir.exists(dirname(outdir))){
    stop("outdir doesn't exist")
  }

  if(map_type == "hazard"){
    scale <- FALSE
  } else if(map_type == "risk"){
    scale <- TRUE
  } else {
    stop("map_type must either be 'hazard' or 'risk'")
  }

  hab <- vultRmap::range_covts %>%
    dplyr::select(lon, lat)

  attr(hab, "mod_scale") <- NULL

  ud <- calcUdColony(.col_sel = col_to_pred[1,], .hab = hab, .age = age,
                     .scale = scale, .countsdir = countsdir,
                     .suffix = suffix, .outputdir = NULL)

  for(i in seq_len(nrow(col_to_pred[-1,]))){

    newud <- calcUdColony(.col_sel = col_to_pred[i+1,], .hab = hab, .age = age,
                          .scale = scale, .countsdir = countsdir,
                          .suffix = suffix, .outputdir = NULL)

    ud$count <- ud$count + newud$count
    ud$gamfit <- ud$gamfit + newud$gamfit

  }

  # If calculating a hazard map, we want the mean hazard across colonies
  if(map_type == "hazard"){

    ncols <- nrow(col_to_pred)

    ud$count <- (ud$count + newud$count) / ncols
    ud$gamfit <- (ud$gamfit + newud$gamfit) / ncols

  }

  r_gamfit <- ud %>%
    dplyr::select(x = lon, y = lat, z = gamfit) %>%
    raster::rasterFromXYZ(crs = sp::CRS("+init=epsg:4326"))

  # Save raster file
  if(!is.null(outdir)){

    outfile <- paste0(outdir, map_type, "_", age, ".tif")
    raster::writeRaster(r_gamfit, outfile, overwrite = TRUE)

  } else {

    return(r_gamfit)

  }

}
