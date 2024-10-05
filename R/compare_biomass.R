#' Compare biomass across runs
#'
#' @param fungrouplist list of functional groups
#' @param prm.modify list of runs to compare
#' @param run.dir directory where runs are stored
#' @param maxtimestep maximum desired timestep
#' @param runs.modify runs to compare
#'
#' @return
#' @export
#'
#' @examples
compare_biomass <- function(fungrouplist, prm.modify, run.dir, runs.modify){

  these.runs <- prm.modify[prm.modify$run_no %in% runs.modify,]$run_name

  folder.paths <- paste0(run.dir,"/",these.runs,"/outputFolder")

  folder.num <- 1:length(folder.paths)

  biom.output.file <- paste0("biomass_compare_runs", paste0(as.character(runs.modify),collapse="-"))
  print(biom.output.file)
 #run.names <- c("v6704", "v6708","v6716","v6716fp","v6717")

  run.colors <- c("#386cb0","#f0027f","#2cf3b8","#b01e28","#063970","#f38e2c","#5f0670","#fdc086")[1:length(these.runs)]

#specify output frequency and years run

# You can now use the colors_vector in your code

names(run.colors) <- these.runs


# if(file.exists(paste0(run.dir,"/", biom.output.file,".csv"))==TRUE){
#
#   file.remove(paste0(run.dir,"/", biom.output.file,".csv"))
# }

fg.list <- fungrouplist %>%
  dplyr::select(Code, IsTurnedOn, GroupType, NumCohorts, name, longname)

print("Reading biomass data")
lapply(folder.num, read_biomass, fg.list, folder.paths, these.runs, biom.output.file)

print("Plotting biomass data")
plot_biomass(biom.output.file, run.colors, run.dir, runs.modify)


print("Combining pdf comparison plots")
pdf.list <- list.files(path=run.dir, pattern="compare_runs.*\\.pdf$", full.names = TRUE)
qpdf::pdf_combine(pdf.list, output = paste0(run.dir,"/biomass_compare_plots_runs_",paste0(as.character(min(runs.modify)), "-",as.character(max(runs.modify))),".pdf"))
file.remove(pdf.list)


}
