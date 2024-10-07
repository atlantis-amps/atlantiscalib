#' Pull the fill value
#'
#' @param varid variable names
#'
#' @return fillval_df
#' @export
#'
#' @examples
get_nc_fillval <- function(varid, init_nc) {
  # function that pulls the fillvalue

  fillval <- ncdf4::ncatt_get(nc = init_nc, varid = varid, attname = "fill.value")
  fillval_df <- data.frame(varid, fillval[2])
  fillval_df$species <- gsub("[0-9]", "",gsub("_StructN","",gsub("_ResN","",fillval_df$varid)))
  fillval_df$age <- as.numeric(gsub("[^0-9]", "",fillval_df$varid))
  #fillval_df$var <- sub(".*[0-9]_(.*)", "\\1", fillval_df$varid)
  fillval_df <- fillval_df %>% dplyr::select(species, age, value)
  return(fillval_df)
}
