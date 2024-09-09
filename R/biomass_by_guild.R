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
                         select(Code, longname), by = "Code")

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
    this_pal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(length(guild.list), "Paired"))

    # plot all guilds including bacteria and detritus
    guild.plot <- biom_prop %>%
      ggplot2::ggplot()+
      ggplot2::geom_bar(aes(x=year, y=prop, fill=guild), position='stack', stat='identity')+
      ggplot2::scale_fill_manual(values = this_pal(length(unique(biom_prop$guild)))) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Guild Biomass", x = "Year", y = "Proportion") +
      ggplot2::theme(legend.position = "bottom")

    thisplotname <- paste(this.run, "Allguild_plot1.pdf",sep="_")

    #ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
    ggplot2::ggsave(thisplotname, plot = guild.plot, path = run.dir, width = 19, height = 25, units = "cm")


    this_pal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(length(guild.list.nobac), "Paired"))

    #plot without bacteria and detritus
    guild.plot2 <- biom_prop.bac %>%
      ggplot2::ggplot()+
      ggplot2::geom_bar(aes(x=year, y=prop, fill=guild), position='stack', stat='identity')+
      ggplot2::scale_fill_manual(values = this_pal(length(unique(biom_prop$guild)))) +
      ggplot2::theme_bw()+
      ggplot2::labs(title = "Guild Biomass no detritus or bacteria", x = "Year", y = "Proportion") +
      ggplot2::theme(legend.position = "bottom")


    thisplotname2 <- paste(this.run, "Allguild_plot2.pdf",sep="_")

    #ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
    ggplot2::ggsave(thisplotname2, plot = guild.plot2, path = run.dir, width = 19, height = 25, units = "cm")


    guild.num <- 1:length(guild.list.nobac)

    this.guild.nobac <- this.output.biomass.guild[!this.output.biomass.guild$guild%in% c("Bacteria", "Detritus"),]


    for (eachguildnum in guild.num) {

      eachguild <- guild.list.nobac[eachguildnum]

      print(paste("Plotting guild",eachguild))

      this.guild <- this.guild.nobac[this.guild.nobac$guild == eachguild,]

      guild.comp.plot <- this.guild %>%
        ggplot2::ggplot()+
        ggplot2::geom_bar(aes(x=year, y=mt, fill=long_name), position='stack', stat='identity')+
        #   ggplot2::scale_fill_manual(values = this_pal(length(unique(biom_prop$guild)))) +
        ggplot2::theme_bw()+
        ggplot2::labs(title = paste("Guild Biomass",eachguild), x = "Year", y = "Proportion")+
        ggplot2::theme(legend.position = "bottom",
                       plot.margin = margin(t = 1,  # Top margin
                                            r = 1,  # Right margin
                                            b = 3,  # Bottom margin
                                            l = 2,  # Left margin
                                            unit = "cm")) #end ggplot


      if(eachguildnum==3 | eachguildnum==5 | eachguildnum==6){

        guild.comp.plot <- guild.comp.plot  +
          ggplot2::theme(legend.position = "right") #end ggplot
      }

      #add 2 to guild counter to avoid overwriting other combined guild plots
      guild.counter <- eachguildnum + 2

      thisplotname3 <- paste0(this.run, "_",eachguild, "_guild_plot",guild.counter,".pdf")


      ggplot2::ggsave(thisplotname3, plot = guild.comp.plot, path = run.dir, width = 21, height = 29, units = "cm")

    } #end guild list loop

    print("Combining pdf guild plots")
    pdf.list <- list.files(path=run.dir, pattern="guild_plot.*\\.pdf$", full.names = TRUE)
    qpdf::pdf_combine(pdf.list, output = paste0(run.dir,"/",this.run,"_guild.pdf"))
    file.remove(pdf.list)


  } #end folder.num loop
} #end function
