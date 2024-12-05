#' extract species names and param value
#'
#' @param data
#'
#' @return ex.values extracted values
#' @export
#'
#' @examples
extract_values <- function(data) {
  species <- sapply(data, function(x) strsplit(x, "   ")[[1]][1])
  values <- sapply(data, function(x) as.numeric(strsplit(x, "   ")[[1]][2]))
  species <- sapply(species, function(x) strsplit(x, "_")[[1]][2])

  ex.values <- data.frame(species = species, values = values)

  return(ex.values)
}
