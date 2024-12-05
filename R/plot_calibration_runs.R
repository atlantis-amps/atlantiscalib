#' Plot calibration runs
#'
#' @param fungrouplist functional group list
#' @param prm.modify list of runs to plot from Google sheet
#' @param run.dir directory where runs are stored
#'
#' @return
#' @export
#'
#' @examples
plot_calibration_runs <- function(fungrouplist, prm.modify, run.dir){

  #loads functional group file
  fg.list <- fungrouplist %>%
    dplyr::select(Code, IsTurnedOn, GroupType, NumCohorts, name, longname) %>%
    dplyr::filter(!Code %in% c("DIN","DL"))

  these.runs <- prm.modify[prm.modify$run_no %in% runs.modify,]$run_name

  folder.paths <- paste0(run.dir,"/",these.runs,"/outputFolder")

  folder.num <- 1:length(folder.paths)

  varlist <- c("Biomass","ResN","Nums","StructN","Wage")


  lapply(folder.num, plot_param, these.runs, folder.paths, varlist)
   #end loop over runs
} # end function
