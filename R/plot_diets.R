#' Plot diets
#'
#' @param fungrouplist list of functional groups
#' @param prm.modify file of runs to modify
#' @param runs.modify list of run numbers to modify
#' @param outdietfile diet file name
#' @param run.dir run dir
#'
#' @return
#' @export
#'
#' @examples
plot_diets<-function(fungrouplist, prm.modify, runs.modify, outdietfile, run.dir){

  #loads functional group file
  fg.list <- fungrouplist %>%
    dplyr::select(Code, IsTurnedOn, GroupType, NumCohorts, name, longname) %>%
    dplyr::filter(!Code %in% c("DIN","DL"))

  these.runs <- unique(prm.modify[prm.modify$run_no %in% runs.modify,]$run_name)


  folder.paths <- paste0(run.dir,"/",these.runs,"/outputFolder")
  print(paste("Analyze these runs"))
  print(these.runs)
  folder.num <- 1:length(folder.paths)

  for(eachnum in folder.num){

    this.run <- these.runs[eachnum]
    this.path <- folder.paths[eachnum]

    print(this.path)
    #needed in case the file is going to be overwritten
    system(paste0("sudo chmod -R a+rwx ", this.path), wait = TRUE)

    print("read diet check")
    diet_check <- read.table(paste0(this.path, "/",outdietfile), as.is = TRUE,header=TRUE,sep=" ")

    #get group information
    grp_list <-get_groups(fungrouplist, this.path)

    pred_groups <- grp_list$pred_groups
    pred.nums <- 1:length(pred_groups$name)

    NumberOfCluster <- parallel::detectCores()

    # Initiate cluster
    cl <- parallel::makeCluster(NumberOfCluster)
    doSNOW::registerDoSNOW(cl)

    # Run this for loop for one call of model from each cluster, assuming cluster is already initiated.
    atlantis.scenarios <- foreach::foreach(eachgroup=pred.nums, .verbose = TRUE) %dopar% {

      .packages = c("dplyr", "ggplot2",  "grDevices", "tidyr", "RColorBrewer")


      # Install CRAN packages (if not already installed)
      # .inst <- .packages %in% installed.packages()

      # if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], )

      # Load packages into session
      lapply(.packages, require, character.only=TRUE)

      this.pred <- pred_groups$name[eachgroup]
      FG_code<-pred_groups$Code[pred_groups$name==this.pred]
      this.longname<-pred_groups$longname[pred_groups$name==this.pred]

      print(FG_code)

      subDiet<- diet_check %>%
        dplyr::filter(Predator==FG_code)

      #print(nrow(subDiet))

      if(!sum(subDiet[,6:ncol(subDiet)])%in%c(0,NA)){

        threshold <- prm.modify %>%
          dplyr::filter(run_name==this.run) %>%
          dplyr::distinct(diet_threshold) %>%
          dplyr::pull(diet_threshold)

        starttimediet <- unique(prm.modify[prm.modify$run_name ==this.run,]$starttimediet)

        endtimediet <- unique(prm.modify[prm.modify$run_name ==this.run,]$endtimediet)

        thisdietdata <- subDiet %>%
          tidyr::pivot_longer(cols=BB:DC, names_to = "prey", values_to = "value") %>%
          dplyr::left_join(grp_list$fgrps, by = c("prey" = "Code")) %>%
          dplyr::mutate(prey = as.factor(prey)) %>%
          dplyr::select(Time, Predator, Cohort, prey, value, longname) %>%
          dplyr::filter(Time>starttimediet & Time <endtimediet) %>%
          dplyr::mutate(year = Time / 365) %>%
          dplyr::filter(value>threshold)

        colourCount = length(unique(thisdietdata$prey))
        getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(colourCount, "Paired"))

        legend.rows <- ceiling(max(colourCount)/3)

        dietplot <- thisdietdata %>%
          #dplyr::mutate(time = as.factor(Time)) %>%
          ggplot2::ggplot(ggplot2::aes(x=year,y=value*100, fill=`longname`, color=`longname`))+
          ggplot2::geom_area(stat="identity")+
          ggplot2::scale_fill_manual(labels = ~ stringr::str_wrap(.x, width = 20), values=getPalette(colourCount), name = "Prey") +
          ggplot2::scale_color_manual(labels = ~ stringr::str_wrap(.x, width = 20), values=getPalette(colourCount), name = "Prey") +
          ggplot2::facet_wrap(~paste("Age",Cohort)) +
          ggplot2::labs(title= paste("Diet of ",this.longname),
                        y="Diet proportions (%)", x = "Year",fill = "Prey",
                        color="Prey")+
          ggplot2::theme(legend.position = "bottom") +
          ggplot2::guides(fill = ggplot2::guide_legend(nrow = legend.rows)) +
          ggplot2::guides(color = "none")

        #return(dietplot)

        thisplotname <- paste(this.run,this.longname,"dietplot.pdf",sep="_")

        # ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
        ggplot2::ggsave(thisplotname, plot = dietplot, path=this.path, width = 21, height = 29, units = "cm")

      }

    }
    #return("")

    # do not combine files because it results in a very large pdf that is hard to open
    # print("Combining pdf diet plots")
    # pdf.list <- list.files(path=run.dir, pattern="dietplot.*\\.pdf$", full.names = TRUE)
    # qpdf::pdf_combine(pdf.list, output = paste0(run.dir,"/",this.run,"_diet_plots_", ".pdf"))
    # file.remove(pdf.list)

  }
}
