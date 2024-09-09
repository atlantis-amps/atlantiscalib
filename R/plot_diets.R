#' Plot diets
#'
#' @param dietsAll
#' @param FG_to_plot
#' @param threshold
#' @param yearsselected
#' @param startyear
#'
#' @return dietplot
#' @export
#'
#' @examples
plot_Diets<-function(fungrouplist, prm.modify, threshold, timeselected, starttime, outdietfile){

  #loads functional group file
  fg.list <- fungrouplist %>%
    dplyr::select(Code, IsTurnedOn, GroupType, NumCohorts, name, longname) %>%
    filter(!Code %in% c("DIN","DL"))

  these.runs <- prm.modify[prm.modify$run_no %in% runs.modify,]$run_name

  folder.paths <- paste0(run.dir,"/",these.runs,"/outputFolder")

  folder.num <- 1:length(folder.paths)

  for(eachnum in folder.num){

    this.run <- these.runs[eachnum]
    this.path <- folder.paths[eachnum]

    print(this.path)
    #needed in case the file is going to be overwritten
    system(paste0("sudo chmod -R a+rwx ", this.path), wait = TRUE)


  diet_check <- read.table(paste0(this.path, "/",outdietfile), as.is = TRUE,header=TRUE,sep=" ")
  tictoc::toc()

  #get group information
  grp_list <-get_groups(groups_csv, thisfolder)

  #load atlantis files
  atlantis_outputs <- load_output(thisfolder, thisoutncfile, timeperiod)


  pred_groups <- grp_list$pred_groups
  tyrs <- atlantis_outputs$tyrs

  if(timeselected=="all"){theseyears<-starttime+unique(dietsAll$Time)/timeperiod}

  FG_code<-pred_groups$Code[pred_groups$Name==FG_to_plot]

  #print(FG_code)
  subDiet<- dietsAll %>%
    dplyr::filter(Predator==FG_code)

  #print(nrow(subDiet))

  if(!sum(subDiet[,6:ncol(subDiet)])%in%c(0,NA)){

    selec_prey<-names(which(colSums(subDiet[6:ncol(subDiet)])>threshold))

    colourCount = length(selec_prey)
    getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

    thisdietdata <- subDiet %>%
      reshape2::melt(id.vars = c("Time", "Predator", "Cohort"), measure.vars=selec_prey)   %>%
      dplyr::mutate(variable = as.factor(variable)) %>%
      dplyr::left_join(grp_list$fgrps, by = c("variable" = "Code"))


    dietplot <- thisdietdata %>%
      dplyr::filter(Time%in%as.character(c((theseyears-starttime)*timeperiod))) %>%
      ggplot2::ggplot(ggplot2::aes(x=starttime+(Time/timeperiod),y=value*100,fill=`Long Name`, color=`Long Name`))+
      ggplot2::geom_area(stat="identity")+
      ggplot2::scale_fill_manual(values=getPalette(colourCount), name = "Prey")+
      ggplot2::scale_colour_manual(values=getPalette(colourCount),name = "Prey")+
      ggplot2::facet_wrap(~paste("Age",Cohort))+
      ggplot2::labs(title= paste("Diet of ",pred_groups$`Long Name`[pred_groups$Name==FG_to_plot]),
                    y="Diet proportions (%)", x = "Years",fill = "Prey",
                    color="Prey")+
      ggplot2::theme(legend.position='bottom')

    #return(dietplot)
  }else{
    dietplot <- ggplot2::ggplot() + ggplot2::annotate(geom="text", x = 4, y = 25, label = "plot could not be produced - check the diet output files") + theme_void()
  }
  return(dietplot)
  #return("")

}
