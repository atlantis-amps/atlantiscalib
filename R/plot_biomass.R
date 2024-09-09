#' Plot biomass comparison
#'
#' @param biom.output.file biomass for all runs
#' @param run.colors colors for each run
#' @param run.directory directory where runs are stored

#'
#' @return
#' @export
#'
#' @examples
plot_biomass <- function(biom.output.file, run.colors, run.dir){

  thisdataset <- data.table::fread(paste0(run.dir,"/", biom.output.file,".csv"))

  # Calculate the number of pages with 12 panels per page
  n_pages <- ceiling(
    length(levels(as.factor(thisdataset$longname)))/ 9
  )

  print(n_pages)
  for (i in seq_len(n_pages)) {

    print(i)

    pplot <-   thisdataset %>%
      dplyr::mutate(run_name = as.factor(run_name)) %>%
      ggplot2::ggplot(aes(x=Year,y=biomass, group = run_name, colour = run_name, linetype=run_name))+
      ggplot2::geom_line(alpha = 0.8)+
      ggplot2::labs(y= "Biomass", x = "Year") +
      ggplot2::scale_color_manual(values=run.colors) +
      ggforce::facet_wrap_paginate(~longname, ncol = 3, nrow = 3, page = i, shrink = FALSE, scales = "free_y") +
      ggplot2::theme(legend.position="none") +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_continuous(limits = c(0,NA)) +
      ggplot2::theme(legend.position = "top")


    thisplotname <- paste(biom.output.file, i,"plot.pdf",sep="_")

    #ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
    ggplot2::ggsave(thisplotname, plot = pplot, path = run.dir, width = 21, height = 29, units = "cm")

  }

}
