#' Make 3D UD Map
#'
#' @description Takes all files with smoothed counts present in a directory
#' and creates a UD map.
#' @param col_to_pred A dataframe with the coordinates and sizes of all colonies
#' we want to include in the map.
#' @param age A character string indicating the age of the vultures we want to
#' calculate the UD map for: either "ad" or "juv.
#' @param map_type A character string indicating the type of map that should be
#' calculated: either "pud" in which the utilization distribution is multiplied
#' by the size of the colony or "ud" in which the UD is not scaled by the
#' size of the colony.
#' @param countsdir A character string indicating the path to the directory
#' where the smoothed counts are found.
#' @param .suffix A character string containing the part the name of the file
#' that appears after the colony code (e.g. "_ad_hgt_gam.rds)
#' @param outdir A character string. If not NULL (default) the output map is
#' saved to disk in the desired location. The map type and the age will be
#' recorded in the name of the output file.
#'
#' @return
#' @export
#'
#' @examples
make3DRiskMap <- function(col_to_pred, age, map_type = "pud", countsdir,
                          suffix, outdir = NULL){

  if(!dir.exists(dirname(outdir))){
    stop("outdir doesn't exist")
  }

  if(map_type == "ud"){
    scale <- FALSE
  } else if(map_type == "pud"){
    scale <- TRUE
  } else {
    stop("map_type must either be 'ud' or 'pud'")
  }

  hab <- vultRmap::range_covts %>%
    dplyr::select(lon, lat)

  attr(hab, "mod_scale") <- NULL

  ud <- calcUdHgtColony(.col_sel = col_to_pred[1,], .hab = hab, .age = age,
                        .scale = scale, .countsdir = countsdir,
                        .suffix = suffix, .outputdir = NULL)

  for(i in seq_len(nrow(col_to_pred[-1,]))){

    newud <- calcUdHgtColony(.col_sel = col_to_pred[i+1,], .hab = hab, .age = age,
                             .scale = scale, .countsdir = countsdir,
                             .suffix = suffix, .outputdir = NULL)

    ud$count <- ud$count + newud$count
    ud$gamfit <- ud$gamfit + newud$gamfit

  }

  # If calculating a ud map, we want the mean ud across colonies
  if(map_type == "ud"){

    ncols <- nrow(col_to_pred)

    ud$count <- (ud$count + newud$count) / ncols
    ud$gamfit <- (ud$gamfit + newud$gamfit) / ncols

  }

  r_gamfit <- ud %>%
    dplyr::select(x = lon, y = lat, z = gamfit) %>%
    raster::rasterFromXYZ(crs = sp::CRS("+init=epsg:4326"))

  # Crop map
  frame <- raster::extent(c(12, 40, -37, -17))
  r_gamfit <- raster::crop(r_gamfit, frame)

  # Save raster file
  if(!is.null(outdir)){

    outfile <- paste0(outdir, map_type, "_hgt_", age, ".tif")
    raster::writeRaster(r_gamfit, outfile, overwrite = TRUE)

  } else {

    return(r_gamfit)

  }

}
