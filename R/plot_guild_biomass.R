#' Plot biomass by guild
#'
#' @param fungrouplist
#' @param prm.modify
#' @param group_guilds
#'
#' @return
#' @export
#'
#' @examples
plot_guild_biomass<- function(fungrouplist, prm.modify, group_guilds){

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

    this.output.biomass <- readr::read_delim(paste0(this.path,"/AMPS_OUTBiomIndx.txt")) %>%
      dplyr::select(Time:DC) %>%
      tidyr::pivot_longer(-Time, names_to = "Code", values_to = "mt") %>%
      dplyr::mutate(year = Time / 365) %>%
      dplyr::left_join(fg.list %>%
                         dplyr::select(Code, longname), by = "Code")

    this.output.biomass.guild <- this.output.biomass %>%
      dplyr::rename(code=Code) %>%
      dplyr::left_join(group_guilds, by = "code")


    # calc
    biom_prop <- this.output.biomass.guild %>%
      dplyr::group_by(year, guild) %>%
      dplyr::summarise(mt_guild = sum(mt)) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(mt_tot = sum(mt_guild)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(prop = mt_guild / mt_tot)


    biom_prop.bac <- this.output.biomass.guild %>%
      dplyr::filter(!guild %in% c("Bacteria", "Detritus")) %>%
      dplyr::group_by(year, guild) %>%
      dplyr::summarise(mt_guild = sum(mt)) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(mt_tot = sum(mt_guild)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(prop = mt_guild / mt_tot)

    guild.list <- unique(group_guilds$guild)
    guild.list.nobac <- guild.list[!guild.list %in% c("Bacteria", "Detritus")]


    # make palette
    colourCount = length(unique(biom_prop$guild))
    getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(colourCount, "Paired"))

    legend.rows <- ceiling(max(colourCount)/3)


    # plot all guilds including bacteria and detritus
    guild.plot1 <-  biom_prop %>%
      #dplyr::mutate(time = as.factor(Time)) %>%
      ggplot2::ggplot(ggplot2::aes(x=year,y=prop*100, fill=`guild`, color=`guild`))+
      ggplot2::geom_area(stat="identity")+
      ggplot2::scale_fill_manual(labels = ~ stringr::str_wrap(.x, width = 20), values=getPalette(colourCount), name = "Guild") +
      ggplot2::scale_color_manual(labels = ~ stringr::str_wrap(.x, width = 20), values=getPalette(colourCount), name = "Guild") +
      ggplot2::theme_bw()+
      ggplot2::labs(title = "Guild Biomass", x = "Year", y = "Percent of total system biomass") +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = legend.rows)) +
      ggplot2::guides(color = "none")

    thisplotname <- paste(this.run, "Allguild_plot1.pdf",sep="_")

    #ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
    ggplot2::ggsave(thisplotname, plot = guild.plot1, path = run.dir, width = 19, height = 25, units = "cm")

    # make palette
    colourCount = length(unique(biom_prop.bac$guild))
    getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(colourCount, "Paired"))
    legend.rows <- ceiling(max(colourCount)/3)

    #plot without bacteria and detritus
    guild.plot2 <-  biom_prop.bac %>%
      ggplot2::ggplot(ggplot2::aes(x=year,y=prop*100, fill=`guild`, color=`guild`))+
      ggplot2::geom_area(stat="identity")+
      ggplot2::scale_fill_manual(labels = ~ stringr::str_wrap(.x, width = 20), values=getPalette(colourCount), name = "Guild") +
      ggplot2::scale_color_manual(labels = ~ stringr::str_wrap(.x, width = 20), values=getPalette(colourCount), name = "Guild") +
      ggplot2::theme_bw()+
      ggplot2::labs(title = "Guild Biomass no detritus or bacteria", x = "Year", y = "Percent of total system biomass") +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = legend.rows)) +
      ggplot2::guides(color = "none")


    thisplotname2 <- paste(this.run, "Allguild_plot2.pdf",sep="_")

    #ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
    ggplot2::ggsave(thisplotname2, plot = guild.plot2, path = run.dir, width = 19, height = 25, units = "cm")


    guild.num <- 1:length(guild.list.nobac)

    this.guild.nobac <- this.output.biomass.guild[!this.output.biomass.guild$guild%in% c("Bacteria", "Detritus"),]


    for (eachguildnum in guild.num) {

      eachguild <- guild.list.nobac[eachguildnum]

      print(paste("Plotting guild",eachguild))

      this.guild <- this.guild.nobac[this.guild.nobac$guild == eachguild,]

      this.guild.prop <- this.guild %>%
        dplyr::group_by(year, guild) %>%
        dplyr::summarise(mt_guild = sum(mt)) %>%
        dplyr::left_join(this.guild, by = c("year","guild")) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(prop = mt / mt_guild)

      # make palette
      colourCount = length(unique(this.guild.prop$longname))
      getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(colourCount, "Paired"))
      legend.rows <- ceiling(max(length(unique(this.guild.prop$longname)))/3)

      guild.comp.plot <- this.guild.prop %>%
        ggplot2::ggplot(ggplot2::aes(x=year,y=prop*100, fill=longname, color=longname))+
        ggplot2::geom_area(stat="identity")+
        ggplot2::scale_fill_manual(labels = ~ stringr::str_wrap(.x, width = 20), values=getPalette(colourCount), name = "Guild") +
        ggplot2::scale_color_manual(labels = ~ stringr::str_wrap(.x, width = 20), values=getPalette(colourCount), name = "Guild") +
        ggplot2::theme_bw()+
        ggplot2::labs(title = paste0("Guild Biomass for ", eachguild), x = "Year", y = "Percent of total system biomass") +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = legend.rows)) +
        ggplot2::guides(color = "none")

      thisplotname3 <- paste0(this.run, "_",eachguild, "_guild_plot.pdf")


      ggplot2::ggsave(thisplotname3, plot = guild.comp.plot, path = run.dir, width = 21, height = 29, units = "cm")

    } #end guild list loop

    print("Combining pdf guild plots")
    pdf.list <- list.files(path=run.dir, pattern="guild_plot.*\\.pdf$", full.names = TRUE)
    qpdf::pdf_combine(pdf.list, output = paste0(run.dir,"/",this.run,"_guild.pdf"))
    file.remove(pdf.list)


  } #end folder.num loop
} #end function
