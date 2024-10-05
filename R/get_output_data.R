#' Get output data from runs
#'
#' @param prm.modify
#' @param runs.modify
#' @param run.dir
#' @param run.time
#' @param fungrouplist
#' @param this.output.nc
#'
#'
#' @return
#' @export
#'
#' @examples
get_output_data <- function(prm.modify, runs.modify, run.dir, run.time, fungrouplist, this.output.nc){

  data(fungrouplist)
  scenario.names <- unique(prm.modify[prm.modify$run_no%in%runs.modify,]$run_name)

  folder.paths <- paste0(run.dir,"/",scenario.names,"/outputFolder")

  folder.num <- 1:length(folder.paths)

  NumberOfCluster <- parallel::detectCores()

  # Initiate cluster
  cl <- parallel::makeCluster(NumberOfCluster)
  doSNOW::registerDoSNOW(cl)

  # Run this for loop for one call of model from each cluster, assuming cluster is already initiated.
  atlantis.scenarios <- foreach::foreach(eachnum=folder.num, .verbose = TRUE) %dopar% {

    .packages = c("dplyr","readr","RNetCDF")


    # Install CRAN packages (if not already installed)
   # .inst <- .packages %in% installed.packages()

   # if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], )

    # Load packages into session
    lapply(.packages, require, character.only=TRUE)

    source("~/atlantiscalib/R/get_nc_data.R")

    this.run <- scenario.names[eachnum]
    this.path <- folder.paths[eachnum]

    this.maxtimestep <- prm.modify[prm.modify$run_name==this.run,][1,]$max_timestep_plot
    outputfrequency <- prm.modify[prm.modify$run_name==this.run,][1,]$output_frequency_days

    #needed in case the file is going to be overwritten
    system(paste0("sudo chmod -R a+rwx ", this.path), wait = TRUE)

    this.output.biomass <- readr::read_delim(paste0(this.path,"/AMPS_OUTBiomIndx.txt")) %>%
      dplyr::select(Time:DIN) %>%
      tidyr::gather(Code,biomass, -Time) %>%
      dplyr::left_join(fungrouplist, by="Code") %>%
      dplyr::filter(Time <= this.maxtimestep) %>%
      dplyr::mutate(Year = Time/365) %>%
      dplyr::select(Code, biomass, Index, name, longname, Year)

    readr::write_csv(this.output.biomass, paste0(this.path,"/biomass_",this.run,".csv"))

    nc <- RNetCDF::open.nc(paste0(this.path,"/",this.output.nc))
    nc.data <- RNetCDF::read.nc(nc)

    used.groups <- fungrouplist[fungrouplist$IsTurnedOn==1,]
    vert.groups <- used.groups[used.groups$GroupType %in% c("FISH","SHARK","BIRD","MAMMAL"),]$name

    group.atlantis.data <- lapply(vert.groups, get_nc_data, thisncfile = nc, fungrouplist, prm.modify, maxtimestep=this.maxtimestep, outputfrequency) %>%
      dplyr::bind_rows()

    readr::write_csv(group.atlantis.data, paste0(this.path,"/Nums_ResN_W_",this.run,".csv"))

  }



}

#Still need to add biomass by box plots
#  this.output.box.biomass <-  read_delim(here("outputFolder","/AMPS_OUTBoxBiomass.txt")) %>%
#    gather(Code,biomass, -Time, -Box) %>%
#    left_join(fg.list, by="Code") %>%
#    mutate(Year = Time/365) %>%
#    filter(Time <= maxtimestep)
#
#  max.year <- this.output.box.biomass %>% pull(Year) %>% max
#
#  this.output.box.biomass.df <-  this.output.box.biomass %>%
#    filter(Year == max.year) %>%
#    dplyr::select(-Code, -Time, -GroupType, -IsTurnedOn, -NumCohorts, -name) %>%
#    dplyr::rename(id=Box) %>%
#    left_join(shape.file.df, by="id")
#
# ggplot() +
#   geom_polygon(data = this.output.box.biomass.df, aes( x = long, y = lat, group = group, fill=biomass, color=biomass)) +
#   theme_void() +
# facet_wrap(~ longname)

