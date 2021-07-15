#' Update colony information
#'
#' @description This function will update the colony database and potentially
#' overwrite the existing one. Make sure to make a backup copy before
#' proceeding with any updates!
#' @param id_colony A character string corresponding to the colony that needs to
#' be updated.
#' @param ad_new The updated number of adults the site.
#' @param prop_juv Proportion of juveniles to adults. By default this is 0.43
#' which is a rough estimate based on the literature.
#' @param remove_col If TRUE the colony with id_colony will be completely removed.
#' If a colony should act as an attractor of vulture activity or might host
#' vultures in the future it should NOT be removed, but allocated ad_new = NA.
#' Unless we know that a colony doesn't exist, we don't want to remove them.
#' @param add_col If TRUE a new record will be added to the database instead of
#' updated. Additional fields must be passed on to info_col.
#' @param info_col A named list with the following fields: "name", "lon", "lat",
#' "avg_ad", "avg_juv", "counted" (1 if it has been counted, 0 if not), "type"
#' (either "breed" or "roost"), "id" new id (if NULL an id will be assigned
#' automatically).
#' @param dir_colony The directory where the colony data is stored. The name and
#' extention of the file must also be supplied.
#' @param overwrite If TRUE the original file will be overwriten! Be sure to
#' make a copy just in case. If FALSE the new dataframe will not be saved to
#' disk.
#'
#' @return A colony dataframe with the updated colony count.
#' @export
#'
#' @examples
updateColonyCount <- function(id_colony = NULL, ad_new = NULL, prop_juv = 0.43,
                              remove_col = FALSE, add_col = FALSE,
                              info_col = NULL, dir_colony,
                              overwrite){

  col_old <- utils::read.csv(dir_colony)

  if(add_col){

    if(is.null(info_col)){
      stop("Additional information needs to be passed on to info_col")
    }

    if(is.null(info_col$id)){
      old_ids <- col_old$id
      old_ids <- old_ids[grep("cvcol", old_ids)]
      old_ids <- as.numeric(gsub("cvcol", "", old_ids))
      info_col$id <- paste0("cvcol", max(old_ids) + 1)
    }

    new_record <- data.frame(name = info_col["name"],
                             lon = info_col["lon"],
                             lat = info_col["lat"],
                             avg_ad = ad_new,
                             avg_juv = ad_new*prop_juv,
                             names_old = NA,
                             counted = info_col["counted"],
                             type = info_col["type"],
                             id = info_col$id)
    col_new <- col_old %>%
      cbind(new_record)

  } else if(remove_col){

    col_new <- col_old %>%
      dplyr::filter(id != id_colony)

  } else {

    col_new <- col_old %>%
      dplyr::mutate(avg_ad = ifelse(id == id_colony, round(ad_new), avg_ad),
                    avg_juv = ifelse(id == id_colony, round(prop_juv*avg_ad), avg_juv),
                    counted = ifelse(is.na(ad_new), 0, 1))
  }

  if(overwrite){
    print("Records updated and overwriten!")
    utils::write.csv(col_new, dir_colony, row.names = FALSE)
  } else {
    return(col_new)
  }

}
