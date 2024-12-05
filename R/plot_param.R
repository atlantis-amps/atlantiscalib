
#' Plot output variables
#'
#' @param eachnum run number
#' @param these.runs Runs to plot
#' @param folder.paths Paths to runs
#' @param varlist List of variables to plot
#'
#' @return
#' @export
#'
#' @examples
plot_param <- function(eachnum, these.runs, folder.paths, varlist){

  this.run <- these.runs[eachnum]
  this.path <- folder.paths[eachnum]

  print(this.path)
  #needed in case the file is going to be overwritten
  system(paste0("sudo chmod -R a+rwx ", this.path), wait = TRUE)

  this.output.biomass <- data.table::fread(paste0(this.path,"/biomass_",this.run,".csv"))

  group.atlantis.data <- data.table::fread(paste0(this.path,"/Nums_ResN_W_",this.run,".csv"))


  NumberOfCluster <- parallel::detectCores()

  # Initiate cluster
  cl <- parallel::makeCluster(NumberOfCluster)
  doSNOW::registerDoSNOW(cl)

  # Run this for loop for one call of model from each cluster, assuming cluster is already initiated.
  atlantis.scenarios <- foreach::foreach(thisvariabletype=varlist, .verbose = TRUE) %dopar% {

    .packages = c("dplyr","ggplot2")


    # Install CRAN packages (if not already installed)
    # .inst <- .packages %in% installed.packages()

    # if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], )

    # Load packages into session
    lapply(.packages, require, character.only=TRUE)

#    print(paste("Creating plots for", thisvariabletype))

    if(thisvariabletype=="Biomass"){

      thisdataset <- this.output.biomass[!this.output.biomass$Code%in%c("DIN","DL"),]

      thisdataset$longname <- as.factor(thisdataset$longname)

      # Calculate the number of pages with 12 panels per page
      n_pages <- ceiling(
        length(levels(as.factor(thisdataset$longname)))/ 9
      )

 #     print(n_pages)
      for (i in seq_len(n_pages)) {

  #      print(i)

        pplot <-  ggplot2::ggplot(thisdataset, ggplot2::aes(x=Year,y=biomass, group = longname))+
          ggplot2::geom_line(colour="darkblue")+
          ggplot2::labs(y= thisvariabletype, x = "Year") +
          ggplot2::scale_y_continuous(limits = c(0,NA))+
          ggforce::facet_wrap_paginate(~longname, ncol = 3, nrow = 3, page = i, shrink = FALSE, scales = "free_y")+
          ggplot2::theme(legend.position="none")+
          ggplot2::theme_minimal()

        thisplotname <- paste(thisvariabletype,i,"plot.pdf",sep="_")

        #ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
        ggplot2::ggsave(thisplotname, plot = pplot, path = this.path, width = 21, height = 29, units = "cm")
      }
    } else if (thisvariabletype=="StructN" | thisvariabletype=="ResN") {


      thisdataset <- group.atlantis.data[!group.atlantis.data$code%in%c("DIN","DL"),]

      thisdataset$age <- as.factor(thisdataset$age)

      thisdataset <- thisdataset[thisdataset$variable_type==thisvariabletype,]

      # Calculate the number of pages with 12 panels per page
      n_pages <- ceiling(
        length(levels(as.factor(thisdataset$longname)))/ 12
      )

      print(n_pages)

      for (i in seq_len(n_pages)) {
        print(i)
        try(pplot <-   ggplot2::ggplot(thisdataset, ggplot2::aes(x=Year,y=variable, group = age))+
              ggplot2::geom_line(ggplot2::aes(colour= age))+
              ggplot2::labs(y= thisvariabletype, x = "Year") +
              # facet_wrap(~longname, scales = "free")
              ggforce::facet_wrap_paginate(~longname, ncol = 3, nrow = 4, page = i, scales = "free")+
              ggplot2::theme(legend.position="none")+
              ggplot2::geom_hline(yintercept=1, linetype="solid", color = "black")+
              ggplot2::geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
              ggplot2::geom_hline(yintercept=1.5, linetype="dashed", color = "red"))

        thisplotname <- paste(thisvariabletype,i,"plot.pdf",sep="_")

        # ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
        ggplot2::ggsave(thisplotname, plot = pplot, path = this.path, width = 21, height = 29, units = "cm")
      }


    }  else if (thisvariabletype=="Nums" | thisvariabletype=="Wage"){

      thisdataset <- group.atlantis.data %>%
        dplyr::filter(!code %in% c("DIN","DL")) %>%
        dplyr::filter(variable_type==thisvariabletype) %>%
        dplyr::mutate(age = as.factor(age))

      thisdataset <- group.atlantis.data[!group.atlantis.data$code%in%c("DIN","DL"),]

      thisdataset$age <- as.factor(thisdataset$age)

      thisdataset <- thisdataset[thisdataset$variable_type==thisvariabletype,]


      # Calculate the number of pages with 12 panels per page
      n_pages <- ceiling(
        length(levels(as.factor(thisdataset$longname)))/ 12
      )

      print(n_pages)

      for (i in seq_len(n_pages)) {
        print(i)
        pplot <-   ggplot2::ggplot(thisdataset, ggplot2::aes(x=Year,y=variable, group = age))+
          ggplot2::geom_line(ggplot2::aes(colour= age))+
          ggplot2::labs(y= thisvariabletype, x = "Year") +
          # facet_wrap(~longname, scales = "free")
          ggforce::facet_wrap_paginate(~longname, ncol = 3, nrow = 4, page = i, scales = "free")+
          ggplot2::theme(legend.position="none")

        thisplotname <- paste(thisvariabletype,i,"plot.pdf",sep="_")

        # ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
        ggplot2::ggsave(thisplotname, plot = pplot, path=this.path, width = 21, height = 29, units = "cm")
      }
    }
  } #end loop over variables

  stopCluster(cl)

  print("Combining pdf plots")
  pdf.list <- list.files(path=this.path, pattern="*.*pdf", full.names = TRUE)
  qpdf::pdf_combine(pdf.list, output = paste0(run.dir,"/",this.run,"_run_output.pdf"))
  file.remove(pdf.list)

}
