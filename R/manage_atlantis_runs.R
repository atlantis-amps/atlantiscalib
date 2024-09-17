#' Manage and run Atlantis runs
#'
#' @param sh.file Atlantis run
#' @param runs.modify Entries from Google sheet that will be run
#' @param prm.modify Google sheet
#' @param run.dir Directory where runs are stored
#'
#' @return
#' @export
#'
#' @examples
manage_atlantis_runs <- function(sh.file, runs.modify, prm.modify, run.dir, flagnoclusters){

these.runs <- prm.modify[prm.modify$run_no %in% runs.modify,]$run_name

folder.paths <- paste0(run.dir, "/", these.runs)

folder.length <- 1:length(folder.paths)


#run multiple Atlantis simulations on local machine cores

if(flagnoclusters==1){

  NumberOfCluster <- parallel::detectCores() - 1

} else if(flagnoclusters==0){

  NumberOfCluster <- parallel::detectCores()

}

# Initiate cluster
cl <- parallel::makeCluster(NumberOfCluster)
doSNOW::registerDoSNOW(cl)

# Run this for loop for one call of model from each cluster, assuming cluster is already initiated.
atlantis.scenarios <- foreach::foreach(this.index=folder.length, .verbose = TRUE) %dopar% {

  this.folder <- folder.paths[this.index]

  system(paste0("sudo chmod -R a+rwx ", this.folder), wait = TRUE)
  setwd(this.folder)
  # run Atlantis scenario
  system(paste("cd ",this.folder," sudo flip -uv *; sudo chmod +x ", sh.file,"; sudo sh ./", sh.file, sep=""), wait = TRUE)

  done <- as.data.frame("done")

  system(paste0("sudo chmod -R a+rwx ", this.folder), wait = TRUE)

}


stopCluster(cl)


}
