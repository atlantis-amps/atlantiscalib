#' Read biomass
#'
#' @param eachfolder folder number
#' @param fg.list list of functional groups
#' @param folder.paths list of folder paths
#' @param these.runs list of runs
#' @param maxtimestep maximum desired timestep
#'
#' @return
#' @export
#'
#' @examples
read_biomass <- function(eachfolder, fg.list, folder.paths, these.runs, biom.output.file){

  this.folder <- folder.paths[eachfolder]
  this.run <- these.runs[eachfolder]

  maxtimestep <- prm.modify[prm.modify$run_name==this.run,][1,]$max_timestep_plot

  print(this.folder)

  this.output.biomass <- readr::read_delim(paste0(this.folder,"/","AMPS_OUTBiomIndx.txt")) %>%
    dplyr::select(Time:DIN) %>%
    tidyr::pivot_longer(cols =BB:DIN, names_to= "Code",values_to="biomass") %>%
    dplyr::left_join(fg.list, by="Code") %>%
    dplyr::filter(Time <= maxtimestep) %>%
    dplyr::mutate(Year = Time/365) %>%
    dplyr::select(-IsTurnedOn, -GroupType, -NumCohorts, -Time)  %>%
    dplyr::mutate(run_name = this.run)

  print(head(this.output.biomass))

  if(eachfolder==1) readr::write_csv(this.output.biomass, paste0(run.dir,"/", biom.output.file,".csv"), append = FALSE)
  if(eachfolder!=1) readr::write_csv(this.output.biomass, paste0(run.dir,"/", biom.output.file,".csv"), append = TRUE)


}
