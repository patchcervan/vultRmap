#' Make 2D standard error map
#'
#' @description Takes all files with step-selection standard error present in a
#'directory and creates a SE map.
#' @param col_to_pred A dataframe with the coordinates of all colonies we want
#' to include in the map.
#' @param age A character string indicating the age of the vultures we want to
#' calculate the risk map for: either "ad" or "juv.
#' @param sedir A character string indicating the path to the directory
#' where the SE files are.
#' @param suffix A character string containing the part the name of the file
#' that appears after the colony code (e.g. "_ssf_se.rds)
#' @param outdir A character string. If not NULL (default) the output map is
#' saved to disk in the desired location. The map type and the age will be
#' recorded in the name of the output file.
#'
#' @return
#' @export
#'
#' @examples
make2DSEmap <- function(col_to_pred, age, sedir, suffix, outputdir = NULL){

  se_total <- rep(0, length = nrow(vultRmap::range_covts))
  ncols <- rep(0, length = nrow(vultRmap::range_covts))

  for(i in seq_len(nrow(col_to_pred))){
    # i = 1
    col_sel <- col_to_pred[i,]

    se <- readRDS(paste0(sedir, col_sel$id, "_", age, suffix))

    hab <- vultRmap::prepColHab(col_cc = unlist(col_sel[, c("lon","lat")]), max_range = 1200,
                                col_all = col_all, sfs = sfs, scale = "ssf")

    se_df <- data.frame(lon = hab$lon,
                        lat = hab$lat,
                        se = se)

    se_df <- se_df %>%
      dplyr::mutate(lon = round(lon, 3),
                    lat = round(lat, 3))

    # Load habitat grid
    hab <- vultRmap::range_covts
    attr(hab, "mod_scale") <- NULL

    hab <- hab %>%
      dplyr::mutate(lon = round(lon, 3),
                    lat = round(lat, 3))

    # Join counts and grid cells
    hab <- hab %>%
      dplyr::select(lon, lat) %>%
      dplyr::left_join(se_df, by = c("lon", "lat"))

    hab <- hab %>%
      mutate(ncol = if_else(is.na(se), 0L, 1L),
             se = if_else(is.na(se), 0, se))

    se_total <- se_total + col_se$se^2
    ncols <- ncols + col_se$ncol

  }

  # save?
  if(!is.null(outputdir)){
    saveRDS(hab, udfile)
  }

  # Create temporary name for output file
  udfile <- paste0(outputdir, .age, "_ssf_se.rds")

  return(hab)

}
