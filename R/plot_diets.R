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
plot_Diets<-function(fungrouplist, prm.modify, runs.modify, threshold, outdietfile, starttimediet, endtimediet, run.dir){

  #loads functional group file
  fg.list <- fungrouplist %>%
    dplyr::select(Code, IsTurnedOn, GroupType, NumCohorts, name, longname) %>%
    dplyr::filter(!Code %in% c("DIN","DL"))

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

  #get group information
  grp_list <-get_groups(fungrouplist, this.path)

  pred_groups <- grp_list$pred_groups
  pred.nums <- 1:length(pred_groups$name)

  for(eachgroup in pred.nums){

    this.pred <- pred_groups$name[eachgroup]
    FG_code<-pred_groups$Code[pred_groups$name==this.pred]
    this.longname<-pred_groups$longname[pred_groups$name==this.pred]

    print(FG_code)
    subDiet<- diet_check %>%
      dplyr::filter(Predator==FG_code)


  #print(nrow(subDiet))

  if(!sum(subDiet[,6:ncol(subDiet)])%in%c(0,NA)){

    selec_prey<-names(which(colSums(subDiet[6:ncol(subDiet)])>threshold))

    colourCount = length(selec_prey)
    getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

    thisdietdata <- subDiet %>%
      reshape2::melt(id.vars = c("Time", "Predator", "Cohort"), measure.vars=selec_prey)   %>%
      dplyr::mutate(variable = as.factor(variable)) %>%
      dplyr::left_join(grp_list$fgrps, by = c("variable" = "Code")) %>%
      dplyr::select(Time, Predator, Cohort, variable, value, longname) %>%
      dplyr::filter(Time>starttimediet & Time <endtimediet)


    dietplot <- thisdietdata %>%
      #dplyr::mutate(time = as.factor(Time)) %>%
      ggplot2::ggplot(ggplot2::aes(x=Time,y=value*100,fill=`longname`, color=`longname`))+
      ggplot2::geom_area(stat="identity")+
      ggplot2::scale_fill_manual(values=getPalette(colourCount), name = "Prey")+
      ggplot2::scale_colour_manual(values=getPalette(colourCount),name = "Prey")+
      ggplot2::facet_wrap(~paste("Age",Cohort))+
      ggplot2::labs(title= paste("Diet of ",this.longname),
                    y="Diet proportions (%)", x = "Years",fill = "Prey",
                    color="Prey")+
      ggplot2::theme(legend.position='bottom')

    #return(dietplot)

    thisplotname <- paste(this.run,this.longname,"dietplot.pdf",sep="_")

    # ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
    ggplot2::ggsave(thisplotname, plot = dietplot, path=run.dir, width = 21, height = 29, units = "cm")

  }

  }
  #return("")

  print("Combining pdf diet plots")
  pdf.list <- list.files(path=run.dir, pattern="dietplot.*\\.pdf$", full.names = TRUE)
  qpdf::pdf_combine(pdf.list, output = paste0(run.dir,"/",this.run,"_diet_plots_", ".pdf"))
  file.remove(pdf.list)

}
}
